// Implementation of Snappy compression fully in V.
//
// Compatible with the standard Snappy binary format. Implements level-1
// (fast) compression using a hash-table approach over 64 KiB blocks.
//
// Public API:
//   compress(input []u8) []u8
//   decompress(input []u8) ![]u8

module snappy

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

const block_size = 1 << 16 // 65 536 — max block processed at once
const min_table_size = 1 << 8 // 256
const max_table_size = 1 << 15 // 32 768  (fits in u16 alongside block_size-1)
const input_margin = 15 // bytes of look-ahead the compressor needs
const hash_magic = u32(0x1e35a7bd)

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

// max_compressed_length returns an upper bound on the compressed size for
// `n` bytes of uncompressed input.
fn max_compressed_length(n int) int {
	return 32 + n + n / 6
}

// compress compresses `input` and returns the compressed bytes.
pub fn compress(input []u8) []u8 {
	n := input.len
	mut out := []u8{cap: max_compressed_length(n)}

	// Write the uncompressed length as a varint header.
	write_uvarint(mut out, u32(n))

	if n == 0 {
		return out
	}

	// Allocate a single hash table reused across every block.
	mut table := []u16{len: max_table_size}

	mut pos := 0
	for pos < n {
		end := if pos + block_size < n { pos + block_size } else { n }
		block := input[pos..end]

		// Zero only the slice of the table we will actually use.
		tsize := table_size_for(block.len)
		for i in 0 .. tsize {
			table[i] = 0
		}

		compress_fragment(block, mut out, mut table, tsize)
		pos = end
	}

	return out
}

// decompress decompresses a Snappy-compressed slice and returns the
// original bytes, or an error if the data is malformed.
pub fn decompress(input []u8) ![]u8 {
	// Read the varint-encoded uncompressed length from the header.
	ulen, hdr_end, ok := read_uvarint(input, 0)
	if !ok {
		return error('snappy: invalid header varint')
	}

	// Pre-allocate the exact output size; reject obviously bogus lengths.
	ilen := int(ulen)
	if ilen < 0 {
		return error('snappy: decoded length overflow')
	}

	// Use a pre-sized buffer with a write cursor instead of dynamic appends.
	// This eliminates per-byte reallocation checks on every literal and copy.
	mut out := []u8{len: ilen}
	mut op := 0
	mut pos := hdr_end

	for pos < input.len {
		tag := input[pos]
		pos++

		tag_type := tag & 0x03

		if tag_type == 0 {
			// LITERAL
			lit_len, new_pos, lit_ok := decode_literal_len(input, tag, pos)
			if !lit_ok {
				return error('snappy: truncated literal length')
			}
			pos = new_pos
			if pos + lit_len > input.len {
				return error('snappy: literal extends past input')
			}
			if op + lit_len > ilen {
				return error('snappy: output exceeds declared length')
			}
			for i in 0 .. lit_len {
				out[op + i] = input[pos + i]
			}
			op += lit_len
			pos += lit_len
		} else if tag_type == 1 {
			// COPY_1 (2-byte offset)
			if pos >= input.len {
				return error('snappy: truncated copy-1 tag')
			}
			length := int((tag >> 2) & 0x07) + 4
			offset := int(u32(tag & 0xe0) << 3 | u32(input[pos]))
			pos++
			if offset == 0 {
				return error('snappy: zero offset in copy-1')
			}
			op = expand_copy_into(mut out, op, ilen, offset, length)!
		} else if tag_type == 2 {
			// COPY_2 (3-byte tag)
			if pos + 1 >= input.len {
				return error('snappy: truncated copy-2 tag')
			}
			length := int((tag >> 2) & 0x3f) + 1
			offset := int(u32(input[pos]) | u32(input[pos + 1]) << 8)
			pos += 2
			if offset == 0 {
				return error('snappy: zero offset in copy-2')
			}
			op = expand_copy_into(mut out, op, ilen, offset, length)!
		} else {
			// COPY_4 (5-byte tag)
			if pos + 3 >= input.len {
				return error('snappy: truncated copy-4 tag')
			}
			length := int((tag >> 2) & 0x3f) + 1
			offset := int(u32(input[pos]) | u32(input[pos + 1]) << 8 | u32(input[pos + 2]) << 16 | u32(input[
				pos + 3]) << 24)
			pos += 4
			if offset == 0 {
				return error('snappy: zero offset in copy-4')
			}
			op = expand_copy_into(mut out, op, ilen, offset, length)!
		}
	}

	if op != ilen {
		return error('snappy: decompressed length mismatch: got ${op}, want ${ilen}')
	}
	return out
}

// ---------------------------------------------------------------------------
// Compression internals
// ---------------------------------------------------------------------------

// compress_fragment compresses a single block (≤ 64 KiB) into `out`.
// `table` must have at least `tsize` zeroed entries (tsize is a power of 2).
fn compress_fragment(input []u8, mut out []u8, mut table []u16, tsize int) {
	n := input.len
	if n == 0 {
		return
	}

	// tbits is the number of bits needed to index `tsize` entries; used
	// to right-shift a 32-bit hash down to the table index.
	tbits := ilog2(tsize)

	// ip  — current input position (index into `input`)
	// lit — start of current literal run
	mut ip := 0
	mut lit := 0

	// We need at least `input_margin` bytes of look-ahead for the main loop.
	// Very short blocks go to the slow literal-only path.
	if n >= input_margin {
		// Emit one forced literal so we have a non-empty literal buffer,
		// then begin the hash loop one byte in.
		ip = 1

		mut next_hash := mix32(load32le(input, ip), tbits)

		outer: for {
			// skip grows when matches are not found — we scan with
			// increasing stride to amortize the cost.
			mut skip := 32
			mut candidate := 0
			mut next_ip := ip

			// Inner probe loop: walk forward until we find a 4-byte match.
			for {
				ip = next_ip
				hash := next_hash
				bytes_until_skip := skip >> 5
				skip++
				next_ip = ip + bytes_until_skip
				if next_ip > n - input_margin {
					break outer
				}
				next_hash = mix32(load32le(input, next_ip), tbits)
				candidate = int(table[hash])
				table[hash] = u16(ip)
				if load32le(input, ip) == load32le(input, candidate) {
					break
				}
			}

			// We have a 4-byte match at [ip] == [candidate].
			// Emit the literal bytes accumulated since `lit`.
			write_literal(mut out, input[lit..ip])

			// Extend the match as far as possible.
			for {
				matched := 4 + find_match_length(input, candidate + 4, ip + 4, n)
				write_copy(mut out, ip - candidate, matched)
				ip += matched
				lit = ip

				if ip >= n - input_margin {
					break outer
				}

				// Look for a consecutive match immediately after the copy.
				// Note: mix32 already returns a value in [0, tsize), so the
				// redundant `& tmask` from the original code is removed.
				prev_hash := mix32(load32le(input, ip - 1), tbits)
				table[prev_hash] = u16(ip - 1)
				cur_hash := mix32(load32le(input, ip), tbits)
				candidate = int(table[cur_hash])
				table[cur_hash] = u16(ip)
				if load32le(input, ip) != load32le(input, candidate) {
					// seed next_hash for the outer loop's next iteration
					next_hash = mix32(load32le(input, ip + 1), tbits)
					ip++
					break
				}
			}
		}
	}

	// Emit whatever literal bytes remain.
	write_literal(mut out, input[lit..])
}

// write_literal emits a LITERAL tag followed by the literal bytes.
@[inline]
fn write_literal(mut out []u8, lit []u8) {
	n := lit.len
	if n == 0 {
		return
	}
	n1 := n - 1
	if n1 < 60 {
		out << u8(u32(n1) << 2) // tag byte encodes length-1 directly
	} else {
		w := byte_width(n1)
		out << u8(u32(59 + w) << 2) // tag signals 1..4 extra length bytes
		mut v := n1
		for _ in 0 .. w {
			out << u8(v & 0xff)
			v >>= 8
		}
	}
	out << lit
}

// write_copy chooses and emits the most compact COPY tag for the given
// (offset, length) back-reference.
@[inline]
fn write_copy(mut out []u8, offset int, length int) {
	mut remaining := length
	for remaining > 0 {
		// COPY_2 handles lengths 1..64; use it as the general case.
		// COPY_1 saves a byte when offset < 2048 and length is 4..11.
		chunk := if remaining > 64 { 64 } else { remaining }
		if offset < 2048 && chunk >= 4 && chunk <= 11 {
			// COPY_1: 2 bytes total
			out << u8(u32(1) | (u32(chunk - 4) << 2) | ((u32(offset) >> 3) & u32(0xe0)))
			out << u8(offset & 0xff)
		} else if offset < 65536 {
			// COPY_2: 3 bytes total
			out << u8(2 | (u32(chunk - 1) << 2))
			out << u8(offset & 0xff)
			out << u8((offset >> 8) & 0xff)
		} else {
			// COPY_4: 5 bytes total (never reached for 64 KiB blocks, left for completeness)
			out << u8(3 | (u32(chunk - 1) << 2))
			out << u8(offset & 0xff)
			out << u8((offset >> 8) & 0xff)
			out << u8((offset >> 16) & 0xff)
			out << u8((offset >> 24) & 0xff)
		}
		remaining -= chunk
	}
}

// find_match_length returns how many bytes starting at s1 and s2 are equal,
// stopping before `limit`. Uses 4-byte word comparisons when possible.
@[inline]
fn find_match_length(input []u8, s1 int, s2 int, limit int) int {
	mut i := 0
	// Compare 4 bytes at a time while there is room for a full word.
	for s2 + i + 4 <= limit && s1 + i + 4 <= limit {
		if load32le(input, s1 + i) != load32le(input, s2 + i) {
			// Mismatch somewhere in this word — find the exact byte.
			for s2 + i < limit && input[s1 + i] == input[s2 + i] {
				i++
			}
			return i
		}
		i += 4
	}
	// Handle the remaining 0..3 tail bytes.
	for s2 + i < limit && input[s1 + i] == input[s2 + i] {
		i++
	}
	return i
}

// ---------------------------------------------------------------------------
// Decompression internals
// ---------------------------------------------------------------------------

// expand_copy_into copies `length` bytes from `offset` bytes before the
// write cursor `op` in the pre-allocated `out` buffer. Returns the new
// write cursor. Handles the overlapping (run-length) case correctly.
@[inline]
fn expand_copy_into(mut out []u8, op int, cap_ int, offset int, length int) !int {
	src := op - offset
	if src < 0 {
		return error('snappy: copy offset past beginning of output')
	}
	if op + length > cap_ {
		return error('snappy: output exceeds declared length')
	}
	// Both overlapping and non-overlapping copies read from indices
	// that were written in earlier iterations, so a forward byte loop
	// is correct in all cases (and required for the RLE / overlap case).
	for i in 0 .. length {
		out[op + i] = out[src + i]
	}
	return op + length
}

// decode_literal_len decodes the length (in bytes) of a literal run from the
// tag byte and returns (length, new_pos, ok). `pos` should point to the
// byte immediately after the tag.
@[inline]
fn decode_literal_len(data []u8, tag u8, pos int) (int, int, bool) {
	n1 := int(tag >> 2) // length - 1, or 60..63 for extended form
	if n1 < 60 {
		return n1 + 1, pos, true
	}
	extra := n1 - 59 // number of extra bytes (1..4)
	if pos + extra > data.len {
		return 0, pos, false
	}
	mut val := 0
	for i in 0 .. extra {
		val |= int(u32(data[pos + i]) << u32(8 * i))
	}
	return val + 1, pos + extra, true
}

// ---------------------------------------------------------------------------
// Varint (unsigned LEB-128, protobuf-style)
// ---------------------------------------------------------------------------

// write_uvarint appends `val` as a variable-length unsigned integer.
@[inline]
fn write_uvarint(mut out []u8, val u32) {
	mut v := val
	for v >= 0x80 {
		out << u8(v & 0x7f | 0x80)
		v >>= 7
	}
	out << u8(v)
}

// read_uvarint decodes a varint from `data` starting at `pos`.
// Returns (value, new_pos, ok).
@[inline]
fn read_uvarint(data []u8, pos int) (u32, int, bool) {
	mut val := u32(0)
	mut shift := u32(0)
	mut i := pos
	for i < data.len && i - pos < 5 {
		b := data[i]
		i++
		val |= u32(b & 0x7f) << shift
		if b & 0x80 == 0 {
			return val, i, true
		}
		shift += 7
	}
	return 0, pos, false
}

// ---------------------------------------------------------------------------
// Utilities
// ---------------------------------------------------------------------------

// load32le reads four bytes from `data[pos..]` as a little-endian u32.
@[inline]
fn load32le(data []u8, pos int) u32 {
	return u32(data[pos]) | u32(data[pos + 1]) << 8 | u32(data[pos + 2]) << 16 | u32(data[pos + 3]) << 24
}

// mix32 hashes a 32-bit word down to a `tbits`-bit index using a
// multiplicative hash (same constant as the reference implementation).
@[inline]
fn mix32(v u32, tbits int) int {
	return int((v * hash_magic) >> u32(32 - tbits))
}

// table_size_for returns the smallest power-of-two hash table size that
// comfortably covers `n` bytes, clamped to [min_table_size, max_table_size].
fn table_size_for(n int) int {
	mut size := min_table_size
	for size < n && size < max_table_size {
		size <<= 1
	}
	return size
}

// ilog2 returns floor(log2(n)) for n ≥ 1.
@[inline]
fn ilog2(n int) int {
	mut v := n
	mut r := 0
	for v > 1 {
		v >>= 1
		r++
	}
	return r
}

// byte_width returns the number of bytes needed to represent `n` (1..4).
@[inline]
fn byte_width(n int) int {
	if n < (1 << 8) {
		return 1
	}
	if n < (1 << 16) {
		return 2
	}
	if n < (1 << 24) {
		return 3
	}
	return 4
}
