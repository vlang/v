// The DEFLATE encoder (RFC 1951).
//
// The compressor finds repeated strings with a hash chain over a 32 KiB
// window, buffers the resulting symbols, and closes a block every 16 384 of
// them. For each block it then costs the three encodings the format allows -
// stored, fixed Huffman, dynamic Huffman - and writes the cheapest.
//
// Costing all three is what makes the output competitive and what keeps it
// honest: data that does not compress is stored rather than inflated, which
// matters to every container that puts already-compressed members through a
// deflate encoder.
module deflate

// Window and match geometry, as fixed by RFC 1951 §3.2.5.
const window_size = 32768
const window_mask = window_size - 1
const min_match = 3
const max_match = 258
// max_distance is the furthest back a match may reach. It is short of the
// full window by one maximum match, which is what zlib does, so that the
// output of the two can be compared byte for byte.
const max_distance = window_size - (max_match + min_match + 1)

// Hash chain geometry. The table is indexed by a hash of three bytes and
// holds absolute positions, so nothing has to be rebased as the window
// advances - the chain walk simply stops once it reaches out of range.
const hash_bits = 15
const hash_size = 1 << hash_bits
const nil_pos = u32(0xffff_ffff)

// Alphabet sizes (RFC 1951 §3.2.5 and §3.2.7).
const num_litlen_codes = 286
const num_dist_codes = 30
const num_precode_codes = 19
const end_of_block = 256
// A literal/length or distance code is bounded by `max_code_bits`, shared with
// the decoder; the code lengths of the precode are bounded separately and much
// lower.
const max_precode_bits = 7

// symbols_per_block is how many literals and matches are buffered before a
// block is closed. It is zlib's default, and measurably close to optimal:
// larger blocks amortise the Huffman tables further but track the data's
// statistics less well.
const symbols_per_block = 16384

// LevelConfig is the search effort of one compression level, with the same
// meaning and the same values as zlib's, so that a level number means the
// same thing here as everywhere else.
struct LevelConfig {
	// good_length: once a match this long has been found, the chain walk is
	// cut to a quarter of its length.
	good_length int
	// max_lazy: on the lazy levels, matches at least this long are taken
	// without looking at the next position; on the others, matches longer
	// than this only have their first position inserted in the hash chain.
	max_lazy int
	// nice_length: stop the walk as soon as a match this long is found.
	nice_length int
	// max_chain: how many chain entries to visit at most.
	max_chain int
	// lazy: whether to try the next position before emitting a match.
	lazy bool
}

const level_configs = [
	LevelConfig{0, 0, 0, 0, false}, // 0: stored only
	LevelConfig{4, 4, 8, 4, false},
	LevelConfig{4, 5, 16, 8, false},
	LevelConfig{4, 6, 32, 32, false},
	LevelConfig{4, 4, 16, 16, true},
	LevelConfig{8, 16, 32, 32, true},
	LevelConfig{8, 16, 128, 128, true},
	LevelConfig{8, 32, 128, 256, true},
	LevelConfig{32, 128, 258, 1024, true},
	LevelConfig{32, 258, 258, 4096, true},
]!

// too_far is the distance beyond which a three byte match costs more than
// the three literals it replaces.
const too_far = 4096

// BitWriter accumulates bits least significant first, which is the order
// RFC 1951 §3.1.1 fixes for the stream.
//
// It writes through a raw pointer into a buffer it grows itself rather than
// appending to an array: the encoder emits a few bits per symbol and appends
// millions of times, and each append through the array machinery is a call
// with a bounds check and a memcpy of one byte.
struct BitWriter {
mut:
	buf   []u8
	p     &u8 = unsafe { nil }
	n     int
	cap   int
	bits  u64
	nbits int
}

// init sizes the output buffer for an input of `input_len` bytes.
fn (mut w BitWriter) init(input_len int) {
	w.cap = input_len / 2 + 64
	w.buf = []u8{len: w.cap}
	w.n = 0
	w.bits = 0
	w.nbits = 0
	unsafe {
		w.p = &u8(w.buf.data)
	}
}

// grow makes room for `extra` more bytes.
fn (mut w BitWriter) grow(extra int) {
	mut want := w.cap * 2
	if want < w.n + extra + 64 {
		want = w.n + extra + 64
	}
	mut next := []u8{len: want}
	unsafe {
		vmemcpy(next.data, w.buf.data, w.n)
		w.buf = next
		w.cap = want
		w.p = &u8(next.data)
	}
}

// write_bits appends the low `nbits` bits of `value`.
//
// The accumulator is drained four bytes at a time. A single call adds at
// most 16 bits and the accumulator is drained below 32, so it never holds
// more than 48 of its 64 bits.
@[inline]
fn (mut w BitWriter) write_bits(value u32, nbits int) {
	w.bits |= u64(value) << w.nbits
	w.nbits += nbits
	if w.nbits >= 32 {
		if w.n + 4 > w.cap {
			w.grow(4)
		}
		unsafe {
			w.p[w.n] = u8(w.bits)
			w.p[w.n + 1] = u8(w.bits >> 8)
			w.p[w.n + 2] = u8(w.bits >> 16)
			w.p[w.n + 3] = u8(w.bits >> 24)
		}
		w.n += 4
		w.bits >>= 32
		w.nbits -= 32
	}
}

// put appends one byte.
@[inline]
fn (mut w BitWriter) put(b u8) {
	if w.n + 1 > w.cap {
		w.grow(1)
	}
	unsafe {
		w.p[w.n] = b
	}
	w.n++
}

// put_bytes appends a slice verbatim.
fn (mut w BitWriter) put_bytes(data []u8) {
	if data.len == 0 {
		return
	}
	if w.n + data.len > w.cap {
		w.grow(data.len)
	}
	unsafe {
		vmemcpy(w.p + w.n, data.data, data.len)
	}
	w.n += data.len
}

// align_to_byte discards the bits that pad the current byte, which is what a
// stored block must do before its length fields.
fn (mut w BitWriter) align_to_byte() {
	for w.nbits > 0 {
		w.put(u8(w.bits))
		w.bits >>= 8
		w.nbits -= 8
	}
	w.bits = 0
	w.nbits = 0
}

// flush writes out any partial byte, ending the stream.
fn (mut w BitWriter) flush() {
	w.align_to_byte()
}

// output returns the bytes written, and hands the buffer over.
fn (mut w BitWriter) output() []u8 {
	out := unsafe { w.buf[..w.n] }
	w.buf = []u8{}
	w.p = unsafe { nil }
	w.n = 0
	w.cap = 0
	return out
}

// Compressor holds the working state of the encoder: the match finder, the
// symbols of the block being built, and the scratch tables both need.
//
// It exists so that the tables can be reused. Compressing one member of an
// archive allocates a few kilobytes of them, which is nothing once and a few
// hundred megabytes over a hundred thousand members, so a caller with many
// small buffers to compress should make one of these and keep it.
pub struct Compressor {
mut:
	cfg   LevelConfig
	level int
	input []u8
	w     BitWriter
	// head maps a three byte hash to the most recent position with that
	// hash; prev chains each position to the previous one, indexed modulo
	// the window. Both are sized to the input rather than to the window, so
	// that compressing many small members - which is what an archive writer
	// does - does not allocate a quarter of a megabyte per member.
	head      []u32
	prev      []u32
	head_mask u32
	prev_mask int
	// Raw views on the tables above, taken once per reset. V compiles an
	// indexed read directly but routes every indexed *write* through
	// `array__set`, a call with a memcpy inside - and the match finder does
	// two of those per input byte, which is the single hottest thing the
	// encoder does.
	head_p &u32 = unsafe { nil }
	prev_p &u32 = unsafe { nil }
	lits_p &u8  = unsafe { nil }
	dist_p &u16 = unsafe { nil }
	// pos is the next position to be processed. `covered` is how far the
	// buffered symbols reach, which lags `pos` by one byte whenever a lazy
	// match is still pending, and block_start is where the current block
	// begins. A stored block copies exactly `block_start .. covered`.
	pos         int
	covered     int
	block_start int
	// The symbols of the current block: a literal byte, or a length and a
	// distance. `dists[i] == 0` marks a literal.
	lits    []u8
	dists   []u16
	nsym    int
	freq_ll []u32
	freq_d  []u32
	// Scratch for the per-block Huffman work. Held here for the same reason
	// as the match finder tables: a block builds three code trees, and an
	// archive writer closes one block per member.
	ll_lens  []int
	d_lens   []int
	ll_codes []u32
	d_codes  []u32
	pre_freq []u32
	pre_lens []int
	pre_code []u32
	seq      []int
	items    []PrecodeItem
	// Scratch for `huffman_lengths`, which runs three times per block.
	hl_keys   []u64
	hl_weight []u64
	hl_parent []int
	hl_count  []int
	// Lazy match state: the match found at the previous position, held back
	// in case the next one is better.
	prev_length int
	prev_match  int
	match_avail bool
}

// new_compressor returns a compressor for `level`, which runs from 0 (store,
// no compression) to 9 (most effort).
pub fn new_compressor(level int) !&Compressor {
	if level < 0 || level > 9 {
		return error('deflate: compression level ${level} is out of the range 0..9')
	}
	return &Compressor{
		level:     level
		cfg:       level_configs[level]
		freq_ll:   []u32{len: num_litlen_codes}
		freq_d:    []u32{len: num_dist_codes}
		ll_lens:   []int{len: num_litlen_codes}
		d_lens:    []int{len: num_dist_codes}
		// Sized for the fixed alphabets, which run two codes past the
		// dynamic ones: 288 literal/length codes and 32 distance codes.
		ll_codes:  []u32{len: 288}
		d_codes:   []u32{len: 32}
		pre_freq:  []u32{len: num_precode_codes}
		pre_lens:  []int{len: num_precode_codes}
		pre_code:  []u32{len: num_precode_codes}
		seq:       []int{cap: num_litlen_codes + num_dist_codes}
		items:     []PrecodeItem{cap: 64}
		hl_keys:   []u64{cap: num_litlen_codes}
		hl_weight: []u64{cap: 2 * num_litlen_codes}
		hl_parent: []int{len: 2 * num_litlen_codes}
		hl_count:  []int{len: max_code_bits + 2}
	}
}

// compress_raw compresses `data` to a raw RFC 1951 DEFLATE stream.
// The compressor keeps its scratch tables afterwards, so compressing many
// buffers through the same one allocates them once.
pub fn (mut c Compressor) compress_raw(data []u8) ![]u8 {
	if c.level == 0 {
		return store_only(data)
	}
	c.reset(data)
	c.run()!
	c.w.flush()
	return c.w.output()
}

// reset prepares the compressor for `data`, growing the match finder tables
// when they are too small and clearing only the part that will be used.
fn (mut c Compressor) reset(data []u8) {
	head_len := table_size(data.len, hash_size)
	prev_len := table_size(data.len, window_size)
	syms := if data.len < symbols_per_block { data.len + 1 } else { symbols_per_block }
	if c.head.len < head_len {
		c.head = []u32{len: head_len}
	}
	if c.prev.len < prev_len {
		c.prev = []u32{len: prev_len}
	}
	if c.lits.len < syms {
		c.lits = []u8{len: syms}
		c.dists = []u16{len: syms}
	}
	for i in 0 .. head_len {
		c.head[i] = nil_pos
	}
	c.head_mask = u32(head_len - 1)
	c.prev_mask = prev_len - 1
	unsafe {
		c.head_p = &u32(c.head.data)
		c.prev_p = &u32(c.prev.data)
		c.lits_p = &u8(c.lits.data)
		c.dist_p = &u16(c.dists.data)
	}
	c.input = data
	c.w = BitWriter{}
	c.w.init(data.len)
	c.pos = 0
	c.covered = 0
	c.block_start = 0
	c.nsym = 0
	c.prev_length = 0
	c.prev_match = 0
	c.match_avail = false
	for i in 0 .. c.freq_ll.len {
		c.freq_ll[i] = 0
	}
	for i in 0 .. c.freq_d.len {
		c.freq_d[i] = 0
	}
}

// compress_level compresses `data` at `level` through a compressor used
// once, which is what the one-shot entry points of this module do.
fn compress_level(data []u8, level int) ![]u8 {
	mut c := new_compressor(level)!
	return c.compress_raw(data)!
}

// store_only writes the input as stored blocks, which is what level 0 asks
// for and the shortest possible encoding of incompressible data.
fn store_only(data []u8) ![]u8 {
	mut w := BitWriter{}
	w.init(data.len * 2 + 16)
	if data.len == 0 {
		w.write_bits(1, 1)
		w.write_bits(0, 2)
		w.align_to_byte()
		w.put_bytes([u8(0), 0, 0xff, 0xff])
		return w.output()
	}
	mut off := 0
	for off < data.len {
		mut n := data.len - off
		if n > 65535 {
			n = 65535
		}
		last := if off + n >= data.len { 1 } else { 0 }
		w.write_bits(u32(last), 1)
		w.write_bits(0, 2)
		w.align_to_byte()
		w.put(u8(n & 0xff))
		w.put(u8((n >> 8) & 0xff))
		w.put(u8(~n & 0xff))
		w.put(u8((~n >> 8) & 0xff))
		w.put_bytes(data[off..off + n])
		off += n
	}
	return w.output()
}

// run walks the input, emitting literals and matches into blocks.
@[direct_array_access]
fn (mut c Compressor) run() ! {
	if c.input.len == 0 {
		c.flush_block(true)!
		return
	}
	for c.pos < c.input.len {
		// A match already as long as `max_lazy` is taken without looking for a
		// better one at the next position, as zlib does.
		match_len, match_dist := if c.cfg.lazy && c.prev_length >= c.cfg.max_lazy {
			0, 0
		} else {
			c.find_match(c.pos)
		}
		if !c.cfg.lazy {
			if match_len >= min_match {
				c.add_match(match_len, match_dist)
				// The fast levels use `max_lazy` as zlib's max_insert_length:
				// only short matches have every position inserted, a long one
				// only its start, which keeps the chains short.
				if match_len <= c.cfg.max_lazy {
					c.insert_range(c.pos, match_len)
				} else {
					c.insert_at(c.pos)
				}
				c.pos += match_len
			} else {
				c.add_literal(c.input[c.pos])
				c.insert_at(c.pos)
				c.pos++
			}
		} else {
			c.step_lazy(match_len, match_dist)
		}
		if c.nsym >= symbols_per_block {
			c.flush_block(false)!
		}
	}
	// A match held back by the lazy search still has to be emitted.
	if c.match_avail {
		c.add_literal(c.input[c.pos - 1])
		c.match_avail = false
	}
	c.flush_block(true)!
}

// step_lazy takes the match at the current position only if the one held
// back from the previous position is not at least as good. Deferring the
// decision by one byte is what separates the higher levels from the lower
// ones, and costs one comparison per position.
@[direct_array_access]
fn (mut c Compressor) step_lazy(match_len int, match_dist int) {
	mut len := match_len
	mut dist := match_dist
	// A three byte match from far away costs more than the literals it
	// replaces, so it is not worth interrupting the literal run for.
	if len == min_match && dist > too_far {
		len = min_match - 1
	}
	if c.prev_length >= min_match && len <= c.prev_length {
		// The previous match wins: emit it, starting one byte back.
		c.add_match(c.prev_length, c.prev_match)
		// The two positions the match starts at are already in the chain,
		// and re-inserting them would put it out of order - which silently
		// costs matches later, since the walk stops at the first entry that
		// is out of window range.
		if c.prev_length > 2 {
			c.insert_range(c.pos + 1, c.prev_length - 2)
		}
		c.pos += c.prev_length - 1
		c.prev_length = 0
		c.match_avail = false
		return
	}
	if c.match_avail {
		c.add_literal(c.input[c.pos - 1])
	}
	c.prev_length = len
	c.prev_match = dist
	c.match_avail = true
	c.insert_at(c.pos)
	c.pos++
	if len < min_match {
		c.prev_length = 0
	}
}

// table_size rounds `n` up to a power of two, bounded by `limit`. Sizing the
// match finder to the data is what keeps a small member cheap.
fn table_size(n int, limit int) int {
	mut size := 256
	for size < limit && size < n {
		size *= 2
	}
	return if size > limit { limit } else { size }
}

// hash3 hashes the three bytes at `pos`.
@[direct_array_access; inline]
fn (c &Compressor) hash3(pos int) int {
	v := u32(c.input[pos]) | (u32(c.input[pos + 1]) << 8) | (u32(c.input[pos + 2]) << 16)
	return int(((v * u32(2654435761)) >> u32(32 - hash_bits)) & c.head_mask)
}

// insert_at records `pos` in the hash chain.
@[direct_array_access; inline]
fn (mut c Compressor) insert_at(pos int) {
	if pos + min_match > c.input.len {
		return
	}
	h := c.hash3(pos)
	unsafe {
		c.prev_p[pos & c.prev_mask] = c.head_p[h]
		c.head_p[h] = u32(pos)
	}
}

// insert_range records every position covered by a match, so that later
// matches can start inside it.
//
// The loop is written out rather than calling `insert_at` per position: this
// runs once per byte of every match, and hoisting the tables and masks into
// locals is worth more here than anywhere else in the encoder.
@[direct_array_access]
fn (mut c Compressor) insert_range(pos int, length int) {
	mut end := pos + length
	if end + min_match > c.input.len {
		end = c.input.len - min_match + 1
	}
	if end <= pos {
		return
	}
	unsafe {
		data := &u8(c.input.data)
		mut head := c.head_p
		mut prev := c.prev_p
		hmask := c.head_mask
		pmask := c.prev_mask
		for i in pos .. end {
			v := u32(data[i]) | (u32(data[i + 1]) << 8) | (u32(data[i + 2]) << 16)
			h := int(((v * u32(2654435761)) >> u32(32 - hash_bits)) & hmask)
			prev[i & pmask] = head[h]
			head[h] = u32(i)
		}
	}
}

// find_match returns the longest match at `pos` and its distance, or a
// length below `min_match` when there is none worth taking.
@[direct_array_access]
fn (mut c Compressor) find_match(pos int) (int, int) {
	if pos + min_match > c.input.len {
		return 0, 0
	}
	mut max_len := c.input.len - pos
	if max_len > max_match {
		max_len = max_match
	}
	if max_len < min_match {
		return 0, 0
	}
	mut best_len := min_match - 1
	mut best_dist := 0
	mut chain := c.cfg.max_chain
	// A match already long enough does not deserve a full walk.
	if c.prev_length >= c.cfg.good_length {
		chain >>= 2
	}
	mut nice := c.cfg.nice_length
	if nice > max_len {
		nice = max_len
	}
	mut cur := unsafe { c.head_p[c.hash3(pos)] }
	for chain > 0 && cur != nil_pos {
		node := int(cur)
		dist := pos - node
		if dist <= 0 || dist > max_distance || dist > c.prev_mask {
			break
		}
		// Comparing the byte past the current best first rejects most
		// candidates in one load.
		if c.input[node + best_len] == c.input[pos + best_len] {
			// The match is extended eight bytes at a time. Both reads stay
			// inside the input: the loop only runs while eight more bytes
			// fit within `max_len`, which is itself bounded by what is left.
			mut l := 0
			unsafe {
				a := &u8(c.input.data) + node
				b := &u8(c.input.data) + pos
				for l + 8 <= max_len {
					// The input has no alignment guarantee, so the eight bytes
					// are copied out rather than loaded through a u64 pointer;
					// C compilers turn the copy into a single unaligned load.
					mut wa := u64(0)
					mut wb := u64(0)
					vmemcpy(&wa, a + l, 8)
					vmemcpy(&wb, b + l, 8)
					if wa != wb {
						break
					}
					l += 8
				}
				for l < max_len && a[l] == b[l] {
					l++
				}
			}
			if l > best_len {
				best_len = l
				best_dist = dist
				if l >= nice {
					break
				}
			}
		}
		cur = unsafe { c.prev_p[node & c.prev_mask] }
		chain--
	}
	if best_len < min_match {
		return 0, 0
	}
	return best_len, best_dist
}

// add_literal buffers one literal byte.
@[direct_array_access; inline]
fn (mut c Compressor) add_literal(b u8) {
	unsafe {
		c.lits_p[c.nsym] = b
		c.dist_p[c.nsym] = 0
	}
	c.nsym++
	c.covered++
	c.freq_ll[b]++
}

// add_match buffers one length and distance pair.
@[direct_array_access]
fn (mut c Compressor) add_match(length int, dist int) {
	unsafe {
		c.lits_p[c.nsym] = u8(length - min_match)
		c.dist_p[c.nsym] = u16(dist)
	}
	c.nsym++
	c.covered += length
	c.freq_ll[257 + length_code(length)]++
	c.freq_d[dist_code(dist)]++
}

// length_codes maps a match length, 3 to 258, to its index in the length
// alphabet; dist_codes_low does the same for distances 1 to 256, and
// dist_codes_high for larger ones, indexed by the distance shifted right by
// seven. Both replace a scan of the base tables that ran twice per match.
const length_codes = build_length_codes()
const dist_codes_low = build_dist_codes_low()
const dist_codes_high = build_dist_codes_high()

fn build_length_codes() []u8 {
	mut out := []u8{len: max_match + 1}
	for length in min_match .. max_match + 1 {
		for i := length_bases.len - 1; i >= 0; i-- {
			if length >= length_bases[i] {
				out[length] = u8(i)
				break
			}
		}
	}
	return out
}

// Both distance tables are indexed by `distance - 1`, which is what makes
// the high one work: the code boundaries of the distance alphabet all fall
// on 128k + 1, so every bucket of 128 consecutive values maps to one code.
fn build_dist_codes_low() []u8 {
	mut out := []u8{len: 256}
	for d in 0 .. 256 {
		out[d] = u8(dist_code_of(d + 1))
	}
	return out
}

fn build_dist_codes_high() []u8 {
	mut out := []u8{len: (window_size >> 7) + 1}
	for high in 0 .. out.len {
		out[high] = u8(dist_code_of((high << 7) + 1))
	}
	return out
}

// dist_code_of scans the base table, which is only used to build the two
// lookup tables above.
fn dist_code_of(dist int) int {
	for i := dist_bases.len - 1; i >= 0; i-- {
		if dist >= dist_bases[i] {
			return i
		}
	}
	return 0
}

// length_code returns the index into the length alphabet of `length`.
// Length 258 has its own code with no extra bits, which the table carries.
@[direct_array_access; inline]
fn length_code(length int) int {
	return int(length_codes[length])
}

// dist_code returns the index into the distance alphabet of `dist`.
@[direct_array_access; inline]
fn dist_code(dist int) int {
	d := dist - 1
	if d < 256 {
		return int(dist_codes_low[d])
	}
	return int(dist_codes_high[d >> 7])
}

// PrecodeItem is one symbol of the code length alphabet, with the extra bits
// that follow it (RFC 1951 §3.2.7).
struct PrecodeItem {
	sym   int
	extra u32
	nbits int
}

// flush_block closes the block being built: the three encodings the format
// allows are costed in bits, and the cheapest is written.
fn (mut c Compressor) flush_block(last bool) ! {
	// The literal/length alphabet always ends with the end-of-block symbol.
	c.freq_ll[end_of_block]++
	c.huffman_lengths(c.freq_ll, max_code_bits, mut c.ll_lens)
	c.huffman_lengths(c.freq_d, max_code_bits, mut c.d_lens)
	// A Huffman code needs two codewords even when the data used fewer.
	// The specification allows an empty distance code, but zlib and several
	// widely deployed readers - Windows Explorer among them - reject one, so
	// two one-bit codewords are always emitted instead.
	force_two_codes(mut c.ll_lens)
	force_two_codes(mut c.d_lens)
	ll_lens := c.ll_lens
	d_lens := c.d_lens
	hlit := used_codes(ll_lens, 257)
	hdist := used_codes(d_lens, 1)

	c.seq.clear()
	c.seq << ll_lens[..hlit]
	c.seq << d_lens[..hdist]
	c.items.clear()
	rle_code_lengths(c.seq, mut c.items)
	for i in 0 .. c.pre_freq.len {
		c.pre_freq[i] = 0
	}
	for item in c.items {
		c.pre_freq[item.sym]++
	}
	c.huffman_lengths(c.pre_freq, max_precode_bits, mut c.pre_lens)
	items := c.items
	pre_lens := c.pre_lens
	// At least four code lengths are sent, whatever the specification says
	// about three: that is what every implementation writes and reads.
	mut hclen := num_precode_codes
	for hclen > 4 && pre_lens[cl_order[hclen - 1]] == 0 {
		hclen--
	}

	dynamic_bits := 3 + 14 + 3 * hclen + precode_bits(items, pre_lens) +
		symbol_bits(c.freq_ll, ll_lens, c.freq_d, d_lens)
	static_bits := 3 + symbol_bits(c.freq_ll, fixed_litlen_lens, c.freq_d, fixed_dist_lens)
	block_len := c.covered - c.block_start
	stored_bits := stored_block_bits(block_len, c.w.nbits)

	if stored_bits <= dynamic_bits && stored_bits <= static_bits {
		c.emit_stored(block_len, last)
	} else if static_bits <= dynamic_bits {
		c.w.write_bits(u32(if last { 1 } else { 0 }), 1)
		c.w.write_bits(1, 2)
		c.emit_symbols(fixed_litlen_lens, fixed_dist_lens)!
	} else {
		c.w.write_bits(u32(if last { 1 } else { 0 }), 1)
		c.w.write_bits(2, 2)
		c.emit_dynamic_header(hlit, hdist, hclen, items, pre_lens)!
		c.emit_symbols(ll_lens, d_lens)!
	}

	c.block_start = c.covered
	c.nsym = 0
	for i in 0 .. c.freq_ll.len {
		c.freq_ll[i] = 0
	}
	for i in 0 .. c.freq_d.len {
		c.freq_d[i] = 0
	}
}

// emit_stored writes the block verbatim, byte aligned, in chunks of at most
// 65535 bytes because that is what the length field can express.
fn (mut c Compressor) emit_stored(block_len int, last bool) {
	mut off := c.block_start
	mut left := block_len
	for {
		mut n := left
		if n > 65535 {
			n = 65535
		}
		final := last && n == left
		c.w.write_bits(u32(if final { 1 } else { 0 }), 1)
		c.w.write_bits(0, 2)
		c.w.align_to_byte()
		c.w.put(u8(n & 0xff))
		c.w.put(u8((n >> 8) & 0xff))
		c.w.put(u8(~n & 0xff))
		c.w.put(u8((~n >> 8) & 0xff))
		c.w.put_bytes(c.input[off..off + n])
		off += n
		left -= n
		if left == 0 {
			break
		}
	}
}

// emit_dynamic_header writes the two code trees of a dynamic block.
fn (mut c Compressor) emit_dynamic_header(hlit int, hdist int, hclen int, items []PrecodeItem, pre_lens []int) ! {
	c.w.write_bits(u32(hlit - 257), 5)
	c.w.write_bits(u32(hdist - 1), 5)
	c.w.write_bits(u32(hclen - 4), 4)
	for i in 0 .. hclen {
		c.w.write_bits(u32(pre_lens[cl_order[i]]), 3)
	}
	assign_codes(pre_lens, mut c.pre_code)
	for item in items {
		c.w.write_bits(c.pre_code[item.sym], pre_lens[item.sym])
		c.w.write_bits(item.extra, item.nbits)
	}
}

// emit_symbols writes the buffered literals and matches with the given code
// lengths, then the end-of-block symbol.
@[direct_array_access]
fn (mut c Compressor) emit_symbols(ll_lens []int, d_lens []int) ! {
	assign_codes(ll_lens, mut c.ll_codes)
	assign_codes(d_lens, mut c.d_codes)
	ll_codes := c.ll_codes
	d_codes := c.d_codes
	for i in 0 .. c.nsym {
		dist := int(unsafe { c.dist_p[i] })
		if dist == 0 {
			lit := int(unsafe { c.lits_p[i] })
			c.w.write_bits(ll_codes[lit], ll_lens[lit])
			continue
		}
		length := int(unsafe { c.lits_p[i] }) + min_match
		lc := length_code(length)
		sym := 257 + lc
		c.w.write_bits(ll_codes[sym], ll_lens[sym])
		c.w.write_bits(u32(length - length_bases[lc]), length_extra_bits[lc])
		dc := dist_code(dist)
		c.w.write_bits(d_codes[dc], d_lens[dc])
		c.w.write_bits(u32(dist - dist_bases[dc]), dist_extra_bits[dc])
	}
	c.w.write_bits(ll_codes[end_of_block], ll_lens[end_of_block])
}

// symbol_bits is what the buffered symbols cost with the given code lengths,
// extra bits included.
fn symbol_bits(freq_ll []u32, ll_lens []int, freq_d []u32, d_lens []int) int {
	mut bits := 0
	for i in 0 .. freq_ll.len {
		if freq_ll[i] == 0 {
			continue
		}
		mut n := ll_lens[i]
		if i >= 257 {
			n += length_extra_bits[i - 257]
		}
		bits += int(freq_ll[i]) * n
	}
	for i in 0 .. freq_d.len {
		if freq_d[i] == 0 {
			continue
		}
		bits += int(freq_d[i]) * (d_lens[i] + dist_extra_bits[i])
	}
	return bits
}

// precode_bits is what the code length sequence costs to transmit.
fn precode_bits(items []PrecodeItem, pre_lens []int) int {
	mut bits := 0
	for item in items {
		bits += pre_lens[item.sym] + item.nbits
	}
	return bits
}

// stored_block_bits is what the block costs stored: the header, the padding
// to the next byte boundary, then four bytes of length per chunk and the
// data itself.
fn stored_block_bits(block_len int, pending_bits int) int {
	chunks := if block_len == 0 { 1 } else { (block_len + 65534) / 65535 }
	mut bits := 0
	mut nbits := pending_bits
	for _ in 0 .. chunks {
		bits += 3
		nbits = (nbits + 3) % 8
		if nbits != 0 {
			bits += 8 - nbits
			nbits = 0
		}
		bits += 32
	}
	return bits + 8 * block_len
}

// used_codes returns how many codes of an alphabet have to be sent: the
// index past the last one used, never fewer than `minimum`.
fn used_codes(lens []int, minimum int) int {
	mut n := lens.len
	for n > minimum && lens[n - 1] == 0 {
		n--
	}
	return n
}

// force_two_codes gives an alphabet two one-bit codewords when the data used
// fewer than two symbols, which is what makes the block readable by
// implementations that reject a code with a single codeword.
fn force_two_codes(mut lens []int) {
	mut used := 0
	for l in lens {
		if l != 0 {
			used++
			if used >= 2 {
				return
			}
		}
	}
	mut assigned := 0
	for i in 0 .. lens.len {
		if lens[i] != 0 {
			assigned++
		}
	}
	for i in 0 .. lens.len {
		if assigned >= 2 {
			return
		}
		if lens[i] == 0 {
			lens[i] = 1
			assigned++
		}
	}
}

// rle_code_lengths encodes a sequence of code lengths with the run length
// codes 16, 17 and 18 of RFC 1951 §3.2.7. A run of the same non-zero length
// repeats the previous one; runs of zeros have their own two codes, for
// short and long runs.
fn rle_code_lengths(lens []int, mut items []PrecodeItem) {
	mut i := 0
	for i < lens.len {
		cur := lens[i]
		mut run := 1
		for i + run < lens.len && lens[i + run] == cur {
			run++
		}
		if cur == 0 {
			for run >= 11 {
				mut n := run
				if n > 138 {
					n = 138
				}
				items << PrecodeItem{18, u32(n - 11), 7}
				run -= n
				i += n
			}
			for run >= 3 {
				mut n := run
				if n > 10 {
					n = 10
				}
				items << PrecodeItem{17, u32(n - 3), 3}
				run -= n
				i += n
			}
			for run > 0 {
				items << PrecodeItem{0, 0, 0}
				run--
				i++
			}
			continue
		}
		// A non-zero length is written once, then repeated by code 16.
		items << PrecodeItem{cur, 0, 0}
		run--
		i++
		for run >= 3 {
			mut n := run
			if n > 6 {
				n = 6
			}
			items << PrecodeItem{16, u32(n - 3), 2}
			run -= n
			i += n
		}
		for run > 0 {
			items << PrecodeItem{cur, 0, 0}
			run--
			i++
		}
	}
}

// assign_codes fills `codes` with the canonical Huffman codes of `lens`
// (RFC 1951 §3.2.2), each reversed so that it can be written least
// significant bit first like everything else in the stream.
//
// Codes are assigned in order of increasing length, and within a length in
// order of increasing symbol, which is what makes the code recoverable from
// the lengths alone - and therefore what lets a block transmit the lengths
// and nothing else.
fn assign_codes(lens []int, mut codes []u32) {
	mut count := [16]int{}
	for l in lens {
		if l > 0 {
			count[l]++
		}
	}
	mut next := [16]u32{}
	mut code := u32(0)
	for bits in 1 .. 16 {
		code = (code + u32(count[bits - 1])) << 1
		next[bits] = code
	}
	for i, l in lens {
		if l == 0 {
			codes[i] = 0
			continue
		}
		codes[i] = bit_reverse(next[l], l)
		next[l]++
	}
}

// huffman_lengths assigns code lengths to symbols in proportion to their
// frequency, with no length exceeding `max_len`.
//
// The tree is built by the two queue method - leaves sorted by frequency in
// one queue, the nodes they produce in the other - which needs no heap
// because both queues come out in order. Lengths that overrun the limit are
// then folded back until the Kraft sum is exactly one again, so the code
// stays complete and prefix-free.
//
// Symbols are sorted by packing frequency and symbol into one integer and
// sorting those, rather than by handing a comparison function to `sort`: a
// block builds three of these trees, and an indirect call per comparison is
// what the encoder spent most of its time on before.
fn (mut c Compressor) huffman_lengths(freq []u32, max_len int, mut lens []int) {
	for i in 0 .. lens.len {
		lens[i] = 0
	}
	c.hl_keys.clear()
	for i in 0 .. freq.len {
		if freq[i] > 0 {
			c.hl_keys << (u64(freq[i]) << 16) | u64(i)
		}
	}
	m := c.hl_keys.len
	if m == 0 {
		return
	}
	if m == 1 {
		lens[int(c.hl_keys[0] & 0xffff)] = 1
		return
	}
	c.hl_keys.sort()
	c.hl_weight.clear()
	for i in 0 .. m {
		c.hl_weight << c.hl_keys[i] >> 16
	}
	for i in 0 .. 2 * m - 1 {
		c.hl_parent[i] = -1
	}
	mut leaf := 0
	mut inode := m
	for next := m; next < 2 * m - 1; next++ {
		mut a := 0
		if leaf < m && (inode >= next || c.hl_weight[leaf] <= c.hl_weight[inode]) {
			a = leaf
			leaf++
		} else {
			a = inode
			inode++
		}
		mut b := 0
		if leaf < m && (inode >= next || c.hl_weight[leaf] <= c.hl_weight[inode]) {
			b = leaf
			leaf++
		} else {
			b = inode
			inode++
		}
		c.hl_weight << c.hl_weight[a] + c.hl_weight[b]
		c.hl_parent[a] = next
		c.hl_parent[b] = next
	}
	for i in 0 .. c.hl_count.len {
		c.hl_count[i] = 0
	}
	mut clamped := false
	for i in 0 .. m {
		mut depth := 0
		mut p := c.hl_parent[i]
		for p != -1 {
			depth++
			p = c.hl_parent[p]
		}
		if depth > max_len {
			clamped = true
			depth = max_len
		}
		lens[int(c.hl_keys[i] & 0xffff)] = depth
		c.hl_count[depth]++
	}
	if !clamped {
		return
	}
	// Clamping the deep leaves to `max_len` over-subscribes the code: the
	// Kraft sum, counted in units of 2^-max_len, exceeds 2^max_len. Each step
	// below removes one leaf from the deepest level (-1) and splits a leaf of
	// the deepest non-empty shorter level into two one level down (+0), so
	// the sum drops by exactly one until the code is complete again. This is
	// miniz's tdefl_huffman_enforce_max_code_size.
	mut total := u64(0)
	for bits in 1 .. max_len + 1 {
		total += u64(c.hl_count[bits]) << u64(max_len - bits)
	}
	for total > u64(1) << u64(max_len) {
		c.hl_count[max_len]--
		for bits := max_len - 1; bits > 0; bits-- {
			if c.hl_count[bits] > 0 {
				c.hl_count[bits]--
				c.hl_count[bits + 1] += 2
				break
			}
		}
		total--
	}
	// Reassign the lengths: the longest codes go to the least frequent
	// symbols, which is the order the keys are already in.
	mut idx := 0
	for bits := max_len; bits >= 1; bits-- {
		mut n := c.hl_count[bits]
		for n > 0 && idx < m {
			lens[int(c.hl_keys[idx] & 0xffff)] = bits
			idx++
			n--
		}
	}
}
