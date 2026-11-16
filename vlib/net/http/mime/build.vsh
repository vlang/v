import net.http
import json2 as json
import strings
import os

// Regenerates db.v: the full MIME type database (`const db`), plus a
// case-insensitive extension -> MIME type lookup built as a CHD perfect hash
// (compress, hash, displace; Belazzougui, Boldi, Pagh, Vigna). The generator
// is fully deterministic: given the same db.json input, it always produces
// byte-identical output.
//
// The hash and the key comparison both work on 8-byte windows instead of
// single bytes: the extension is read as its first `min(8, len)` bytes plus
// (for longer keys) its bytes 8..16 and its last 8 bytes, each window
// ASCII-case-folded branchlessly and mixed with a multiply, so the lookup
// cost is independent of the extension length. Unknown extensions are the
// common case for a file server, and they pay the same fixed cost as hits.
//
// When a command line argument is given, it is used as the mime-db source
// (the format of the jshttp/mime-db `db.json`) instead of downloading it;
// this allows regenerating the exact same tables from a saved copy.

struct MimeType {
	source       string
	extensions   []string
	compressible bool
	charset      string
}

// Candidate multipliers for the word hash mix. The generator picks the first
// one whose 20-bit hash is injective over all known extensions.
const ph_mix_multipliers = [
	u64(0x2D358DCCAA6C78A5),
	u64(0xBF58476D1CE4E5B9),
	u64(0x94D049BB133111EB),
	u64(0xC2B2AE3D27D4EB4F),
	u64(0x165667B19E3779F9),
	u64(0x27D4EB2F165667C5),
	u64(0x853C49E6748FEA9B),
	u64(0x53D3C1AE0BE2E1F5),
	u64(0x2545F4914F6CDD1D),
	u64(0x9FB42AB9E0E0F6A1),
	u64(0x5E4FDBBA6A5C6A37),
	u64(0x7BD8F89031AF0A6D),
]

const ph_injectivity_bits = 20

// fixed mix constants for the last window, the middle window and the length
const ph_m2 = u64(0x8BB84B93962EACC9)
const ph_m3 = u64(0x4B33A62ED433D4A3)
const ph_m4 = u64(0x9E3779B97F4A7C15)

// ph_fold lowercases the ASCII letters 'A'..'Z' in every byte of `w`
// branchlessly: bytes in the range get bit 0x20 set, every other byte
// (digits, '-', '_', '.', all bytes above 'Z') is left untouched.
@[inline]
fn ph_fold(w u64) u64 {
	a := w & 0x7F7F7F7F7F7F7F7F
	ge_a := (a + 0x3F3F3F3F3F3F3F3F) & 0x8080808080808080
	gt_z := (a + 0x2525252525252525) & 0x8080808080808080
	return w | ((ge_a & ~gt_z) >> 2)
}

// ph_w8 returns the 8 bytes at `p` as a little-endian word. It is assembled
// from single bytes, which is defined for any alignment, and an optimizing
// compiler folds it into one load.
@[direct_array_access; inline; unsafe]
fn ph_w8(p &u8) u64 {
	return unsafe {
		u64(p[0]) | u64(p[1]) << 8 | u64(p[2]) << 16 | u64(p[3]) << 24 |
			u64(p[4]) << 32 | u64(p[5]) << 40 | u64(p[6]) << 48 | u64(p[7]) << 56
	}
}

// ph_windows returns the extension `ext[i..]` as up to three folded
// 8-byte windows: the first min(8, len) bytes, the last 8 bytes and the
// bytes 8..16, zero when the extension is not long enough to have them.
// Short extensions are gathered one byte at a time (only in-bounds reads,
// no serial dependency chain), long ones with whole word reads.
@[direct_array_access; inline]
fn ph_windows(ext string, i int, l int) (u64, u64, u64) {
	mut w1 := u64(0)
	mut wl := u64(0)
	mut wm := u64(0)
	if l >= 8 {
		p := unsafe { ext.str + i }
		w1 = ph_fold(unsafe { ph_w8(p) })
		wl = if l > 8 { ph_fold(unsafe { ph_w8(p + l - 8) }) } else { u64(0) }
		wm = if l > 16 { ph_fold(unsafe { ph_w8(p + 8) }) } else { u64(0) }
	} else {
		mut w := u64(0)
		for j in 0 .. l {
			w |= u64(ext[i + j]) << (8 * j)
		}
		w1 = ph_fold(w)
	}
	return w1, wl, wm
}

// ph_hash mixes the windows into a u32 hash:
// u32(x ^ (x >> 32)) where
// x = (w1 * m1) ^ (wl * ph_m2) ^ (wm * ph_m3) ^ (u64(l) * ph_m4)
// (wl and wm are zero for extensions shorter than 8 bytes)
@[direct_array_access; inline]
fn ph_hash(ext string, i int, l int, m1 u64) u32 {
	w1, wl, wm := ph_windows(ext, i, l)
	x := (w1 * m1) ^ (wl * ph_m2) ^ (wm * ph_m3) ^ (u64(l) * ph_m4)
	return u32(x ^ (x >> 32))
}

// place all extensions of one hash bucket into free slots, by searching for a
// bucket-wide displacement d such that slot = ((h >> slot_shift) + d) & tmask
// is free (and unique within the bucket) for every extension in it
fn displace_bucket(b int, ext_idx []int, hashes []u32, mut slots []int, mut disp []u16, tmask u32, slot_shift int) {
	mut d := 0
	for {
		mut target := []int{cap: ext_idx.len}
		mut ok := true
		for i in ext_idx {
			s := int(((hashes[i] >> slot_shift) + u32(d)) & tmask)
			if slots[s] != -1 || s in target {
				ok = false
				break
			}
			target << s
		}
		if ok {
			for k, i in ext_idx {
				slots[target[k]] = i
			}
			disp[b] = u16(d)
			return
		}
		d++
		if d > 200_000 {
			eprintln('CHD displacement search failed for bucket ${b}')
			exit(1)
		}
	}
}

// the generated perfect hash tables, shared by the self-check below
struct PhTables {
	m1          u64
	slot_shift  int
	mbits       int
	max_ext_len int
	bmask       u32
	tmask       u32
	mime_mask   u32
	disp        []u16
	slots       []u32 // packed slot values
	blob        []u8
	mimes       []string
}

// mirrors the lookup_generated function emitted into db.v, so that the tables
// are verified against every known extension before anything is written
fn ph_check(t PhTables, ext string) string {
	mut i := 0
	if ext.len > 0 && ext[0] == `.` {
		i = 1
	}
	l := ext.len - i
	if l == 0 || l > t.max_ext_len {
		return ''
	}
	w1, wl, wm := ph_windows(ext, i, l)
	x := (w1 * t.m1) ^ (wl * ph_m2) ^ (wm * ph_m3) ^ (u64(l) * ph_m4)
	h := u32(x ^ (x >> 32))
	slot := t.slots[((h >> t.slot_shift) + u32(t.disp[h & t.bmask])) & t.tmask]
	// branchless finish, mirroring the emitted lookup_generated
	el := int((slot >> t.mbits) & 31)
	off := int(slot >> (t.mbits + 5)) - 1
	p := unsafe { &u8(t.blob.data) + off + 9 }
	m := if l >= 8 { u64(0xFFFFFFFFFFFFFFFF) } else { (u64(1) << (8 * l)) - 1 }
	st1 := unsafe { ph_w8(p) }
	stl := unsafe { ph_w8(p + el - 8) }
	stm := unsafe { ph_w8(p + 8) }
	mut eq := u64(el == l)
	eq &= u64((w1 & m) == (st1 & m))
	eq &= if l > 8 { u64(wl == stl) } else { u64(1) }
	eq &= if l > 16 { u64(wm == stm) } else { u64(1) }
	idx := if eq != 0 { slot & t.mime_mask } else { u32(0) }
	return t.mimes[idx]
}

fn ph_hex_byte(b u8) string {
	digits := '0123456789abcdef'
	return '\\x' + digits[b >> 4].ascii_str() + digits[b & 15].ascii_str()
}

// escape arbitrary bytes into a valid V single-quoted string literal
fn ph_v_literal(bytes []u8) string {
	mut sb := strings.new_builder(bytes.len + 16)
	for b in bytes {
		if b == `\\` {
			sb.write_string('\\\\')
		} else if b == `'` {
			sb.write_string("\\'")
		} else if b >= 32 && b < 127 {
			sb.write_u8(b)
		} else {
			sb.write_string(ph_hex_byte(b))
		}
	}
	return sb.str()
}

fn main() {
	mime_folder := dir(executable())
	chdir(mime_folder)!
	//
	db_text := if os.args.len > 1 {
		read_file(os.args[1])!
	} else {
		http.get('https://raw.githubusercontent.com/jshttp/mime-db/master/db.json')!.body
	}
	mt_map := json.decode[map[string]MimeType](db_text)!

	// extension -> media type (lowercase keys: lookups are case-insensitive)
	mut ext_to_mt := map[string]string{}
	for mt_str, mt in mt_map {
		for ext in mt.extensions {
			ext_to_mt[ext.to_lower()] = mt_str
		}
	}
	ext_to_mt['v'] = 'text/x-vlang'
	ext_to_mt['vsh'] = 'text/x-vlang'

	// deterministic, sorted key list
	mut exts := []string{cap: ext_to_mt.len}
	for ext in ext_to_mt.keys() {
		exts << ext
	}
	exts.sort()
	n := exts.len

	// --- pick a hash multiplier with an injective 20-bit hash ----------------
	inj_mask := (u32(1) << ph_injectivity_bits) - 1
	mut m1 := u64(0)
	for cand in ph_mix_multipliers {
		mut seen := map[u32]bool{}
		mut injective := true
		for ext in exts {
			h := ph_hash(ext, 0, ext.len, cand) & inj_mask
			if h in seen {
				injective = false
				break
			}
			seen[h] = true
		}
		if injective {
			m1 = cand
			break
		}
	}
	if m1 == 0 {
		eprintln('no hash multiplier from ph_mix_multipliers is injective over ${n} extensions')
		exit(1)
	}
	mut hashes := []u32{len: n}
	for i, ext in exts {
		hashes[i] = ph_hash(ext, 0, ext.len, m1)
	}

	// --- CHD: size the slot table, bucket by hash, then displace -------------
	mut tslots := 2048 // keep the load factor <= 0.75
	for n * 100 > tslots * 75 {
		tslots *= 2
	}
	nbuckets := tslots >> 2
	tmask := u32(tslots - 1)
	bmask := u32(nbuckets - 1)
	slot_shift := 9 // bucket and slot use disjoint bit ranges of the hash

	mut buckets := [][]int{len: nbuckets, init: []int{cap: 4}}
	for i in 0 .. n {
		buckets[hashes[i] & bmask] << i
	}
	// displace the largest buckets first (cheaper searches); ties by bucket id,
	// so the result is fully deterministic
	mut max_bucket := 0
	for bucket in buckets {
		if bucket.len > max_bucket {
			max_bucket = bucket.len
		}
	}
	mut slots := []int{len: tslots, init: -1}
	mut disp := []u16{len: nbuckets}
	for size := max_bucket; size >= 1; size-- {
		for b in 0 .. nbuckets {
			if buckets[b].len != size {
				continue
			}
			displace_bucket(b, buckets[b], hashes, mut slots, mut disp, tmask, slot_shift)
		}
	}

	// --- pack the tables ------------------------------------------------------
	// extensions blob: 8 padding bytes, then [u8 ext_len][ext bytes] ... per
	// extension, then 8 more padding bytes. The padding lets the emitted
	// lookup read every window of a stored key with one word load, whatever
	// its position: unknown extensions pay no mispredicted early exits, so
	// the whole comparison after the slot probe is branchless.
	mut blob := []u8{cap: n * 8 + 16}
	for _ in 0 .. 8 {
		blob << 0
	}
	mut ext_off := map[string]int{}
	mut max_ext_len := 0
	for ext in exts {
		if ext.len > 24 {
			eprintln('extension longer than 24 bytes cannot be covered by the hash windows: ${ext}')
			exit(1)
		}
		if ext.len > max_ext_len {
			max_ext_len = ext.len
		}
		if ext !in ext_off {
			// the offset of the entry's length byte, ignoring the leading
			// padding (the emitted lookup adds it back as a constant)
			ext_off[ext] = blob.len - 8
			blob << u8(ext.len)
			blob << ext.bytes()
		}
	}
	for _ in 0 .. 8 {
		blob << 0
	}
	// unique media types; index 0 is reserved for the empty string, so a
	// miss can select it with a single conditional move
	mut mimes := []string{cap: n + 1}
	mut mime_idx := map[string]int{}
	mimes << ''
	mime_idx[''] = 0
	for ext in exts {
		mt := ext_to_mt[ext]
		if mt !in mime_idx {
			mime_idx[mt] = mimes.len
			mimes << mt
		}
	}
	// mime index bits packed into each slot next to the extension length and
	// the blob offset
	mut mbits := 12
	for mimes.len > (1 << mbits) {
		mbits++
	}
	if blob.len >= (1 << (32 - mbits - 5)) {
		eprintln('extension blob too large for ${mbits} mime index bits')
		exit(1)
	}
	// slot: 0 = empty, otherwise
	//   ((ext_offset + 1) << (mbits + 5)) | (ext_len << mbits) | mime_index
	mut packed := []u32{len: tslots}
	for s in 0 .. tslots {
		if slots[s] != -1 {
			i := slots[s]
			ext := exts[i]
			packed[s] = (u32(ext_off[ext] + 1) << (mbits + 5)) | (u32(ext.len) << mbits) |
				u32(mime_idx[ext_to_mt[ext]])
		}
	}

	// --- self-check before emitting anything ----------------------------------
	// ph_check mirrors the lookup_generated function emitted into db.v
	tables := PhTables{
		m1:          m1
		slot_shift:  slot_shift
		mbits:       mbits
		max_ext_len: max_ext_len
		bmask:       bmask
		tmask:       tmask
		mime_mask:   u32((1 << mbits) - 1)
		disp:        disp
		slots:       packed
		blob:        blob
		mimes:       mimes
	}
	for ext in exts {
		if ph_check(tables, ext) != ext_to_mt[ext] {
			eprintln('self-check failed for extension: ${ext}')
			exit(1)
		}
		if ph_check(tables, '.' + ext) != ext_to_mt[ext] {
			eprintln('self-check failed for dot-prefixed extension: ${ext}')
			exit(1)
		}
		if ph_check(tables, ext.to_upper()) != ext_to_mt[ext] {
			eprintln('self-check failed for uppercased extension: ${ext}')
			exit(1)
		}
		if ph_check(tables, '.' + ext.to_upper()) != ext_to_mt[ext] {
			eprintln('self-check failed for dot-prefixed uppercased extension: ${ext}')
			exit(1)
		}
	}
	for miss in ['', '.', 'no_such_extension', 'not-an-extension', 'index.html', 'htmX', 'W0FF',
		'pngx', '..html', 'html ', 'zzzzzzzzzzzzzzzzzzzzzzzzzzz'] {
		if ph_check(tables, miss) != '' {
			eprintln('self-check failed for unknown extension: ${miss}')
			exit(1)
		}
	}

	// --- emit db.v -------------------------------------------------------------
	mut sb := strings.new_builder(1024 * 1024)
	sb.writeln('module mime')
	sb.writeln('')
	sb.writeln('// FILE AUTOGENERATED BY `build.vsh` - DO NOT MANUALLY EDIT')
	sb.writeln('//')
	sb.writeln('// Contains the full MIME type database, plus the extension -> MIME type')
	sb.writeln('// lookup used by `lookup`/`get_mime_type`: a CHD perfect hash')
	sb.writeln('// (compress, hash, displace) over ${n} known file extensions, stored as')
	sb.writeln('// constant data. A lookup hashes and compares the extension as up to')
	sb.writeln('// three 8-byte windows, ASCII-case-folded branchlessly, so its cost does')
	sb.writeln('// not grow with the extension length.')
	sb.writeln('')
	sb.writeln('const db = ${mt_map}')
	sb.writeln('')
	sb.writeln('// hash mix constants: ph_m1 is the first candidate multiplier whose')
	sb.writeln('// 20-bit hash is injective over all known extensions')
	sb.writeln('const ph_m1 = u64(0x${m1.hex()})')
	sb.writeln('const ph_m2 = u64(0x8BB84B93962EACC9)')
	sb.writeln('const ph_m3 = u64(0x4B33A62ED433D4A3)')
	sb.writeln('const ph_m4 = u64(0x9E3779B97F4A7C15)')
	sb.writeln('')
	sb.writeln("// ph_fold lowercases the ASCII letters 'A'..'Z' in every byte of `w`")
	sb.writeln('// branchlessly: bytes in the range get bit 0x20 set, every other byte')
	sb.writeln("// (digits, '-', '_', '.', all bytes above 'Z') is left untouched.")
	sb.writeln('@[inline]')
	sb.writeln('fn ph_fold(w u64) u64 {')
	sb.writeln('\ta := w & 0x7F7F7F7F7F7F7F7F')
	sb.writeln('\tge_a := (a + 0x3F3F3F3F3F3F3F3F) & 0x8080808080808080')
	sb.writeln('\tgt_z := (a + 0x2525252525252525) & 0x8080808080808080')
	sb.writeln('\treturn w | ((ge_a & ~gt_z) >> 2)')
	sb.writeln('}')
	sb.writeln('')
	sb.writeln('// ph_w8 returns the 8 bytes at `p` as a little-endian word. It is')
	sb.writeln('// assembled from single bytes, which is defined for any alignment, and')
	sb.writeln('// an optimizing compiler folds it into one load.')
	sb.writeln('@[direct_array_access; inline; unsafe]')
	sb.writeln('fn ph_w8(p &u8) u64 {')
	sb.writeln('\treturn unsafe {')
	sb.writeln('\t\tu64(p[0]) | u64(p[1]) << 8 | u64(p[2]) << 16 | u64(p[3]) << 24')
	sb.writeln('\t\t\t| u64(p[4]) << 32 | u64(p[5]) << 40 | u64(p[6]) << 48 | u64(p[7]) << 56')
	sb.writeln('\t}')
	sb.writeln('}')
	sb.writeln('')
	sb.writeln('// bucket displacement for the perfect hash:')
	sb.writeln('// slot = ((h >> ${slot_shift}) + ph_disp[h & ${nbuckets - 1}]) & ${tslots - 1}')
	sb.writeln('const ph_disp = [')
	for v in disp {
		sb.writeln('\tu16(${v}),')
	}
	sb.writeln(']')
	sb.writeln('')
	sb.writeln('// ${tslots} hash slots: 0 = empty, otherwise')
	sb.writeln('//   ((ext_offset + 1) << ${mbits + 5}) | (ext_len << ${mbits}) | mime_index')
	sb.writeln('const ph_slots = [')
	for v in packed {
		sb.writeln('\tu32(${v}),')
	}
	sb.writeln(']')
	sb.writeln('')
	sb.writeln('// packed extensions: 8 padding bytes, then [u8 ext_len][ext bytes] per')
	sb.writeln('// extension, then 8 more padding bytes. Every window of a stored key')
	sb.writeln('// is readable with one word load, whatever its position.')
	sb.writeln("const ph_blob = '${ph_v_literal(blob)}'")
	sb.writeln('')
	sb.writeln('// unique MIME types, indexed by the low ${mbits} bits of a ph_slots value;')
	sb.writeln('// index 0 is the empty string, returned for unknown extensions')
	sb.writeln('const ph_mimes = [')
	for v in mimes {
		sb.writeln("\t'${ph_v_literal(v.bytes())}',")
	}
	sb.writeln(']')
	sb.writeln('')
	// ph_masks[l] covers the low 8*min(l, 8) bits of a folded window word
	mut masks := []u64{len: max_ext_len + 1}
	for li in 0 .. max_ext_len + 1 {
		masks[li] = if li >= 8 { u64(0xFFFFFFFFFFFFFFFF) } else { (u64(1) << (8 * li)) - 1 }
	}
	sb.writeln('// ph_masks[l] covers the low 8*min(l, 8) bits of a folded window word')
	sb.writeln('const ph_masks = [')
	for li in 0 .. max_ext_len + 1 {
		sb.writeln('\tu64(${masks[li]}),')
	}
	sb.writeln(']')
	sb.writeln('')
	sb.writeln('const ph_max_ext_len = ${max_ext_len}')
	sb.writeln('')
	sb.writeln("// lookup returns the MIME type for the given file extension, or '' when")
	sb.writeln('// unknown. The extension may be given with or without a leading dot,')
	sb.writeln('// and is compared ASCII case-insensitively without allocating.')
	sb.writeln('//')
	sb.writeln('// The extension is hashed and compared as up to three 8-byte windows,')
	sb.writeln('// ASCII-case-folded branchlessly, so the cost does not grow with its')
	sb.writeln('// length: short extensions are gathered one byte at a time, long ones')
	sb.writeln('// are read with whole word loads. After the single table probe the')
	sb.writeln('// finish is branchless - unknown extensions take no mispredicted')
	sb.writeln('// early exits, which is what dominated their cost.')
	sb.writeln('@[direct_array_access; inline]')
	sb.writeln('pub fn lookup(ext string) string {')
	sb.writeln('\tmut i := 0')
	sb.writeln('\tif ext.len > 0 && ext[0] == `.` {')
	sb.writeln('\t\ti = 1')
	sb.writeln('\t}')
	sb.writeln('\tl := ext.len - i')
	sb.writeln('\tif l == 0 || l > ph_max_ext_len {')
	sb.writeln("\t\treturn ''")
	sb.writeln('\t}')
	sb.writeln('\tmut w1 := u64(0)')
	sb.writeln('\tmut wl := u64(0)')
	sb.writeln('\tmut wm := u64(0)')
	sb.writeln('\tif l >= 8 {')
	sb.writeln('\t\tp := unsafe { ext.str + i }')
	sb.writeln('\t\tw1 = ph_fold(unsafe { ph_w8(p) })')
	sb.writeln('\t\tif l > 8 {')
	sb.writeln('\t\t\twl = ph_fold(unsafe { ph_w8(p + l - 8) })')
	sb.writeln('\t\t}')
	sb.writeln('\t\tif l > 16 {')
	sb.writeln('\t\t\twm = ph_fold(unsafe { ph_w8(p + 8) })')
	sb.writeln('\t\t}')
	sb.writeln('\t} else {')
	sb.writeln('\t\t// gather the bytes into one little-endian word with a rolling')
	sb.writeln('\t\t// place multiplier, so no runtime shift (and its overflow')
	sb.writeln('\t\t// check) is needed')
	sb.writeln('\t\tmut w := u64(0)')
	sb.writeln('\t\tmut pow := u64(1)')
	sb.writeln('\t\tfor j in 0 .. l {')
	sb.writeln('\t\t\tw += u64(ext[i + j]) * pow')
	sb.writeln('\t\t\tpow <<= 8')
	sb.writeln('\t\t}')
	sb.writeln('\t\tw1 = ph_fold(w)')
	sb.writeln('\t}')
	sb.writeln('\tx := if l >= 8 {')
	sb.writeln('\t\t(w1 * ph_m1) ^ (wl * ph_m2) ^ (wm * ph_m3) ^ (u64(l) * ph_m4)')
	sb.writeln('\t} else {')
	sb.writeln('\t\t(w1 * ph_m1) ^ (u64(l) * ph_m4)')
	sb.writeln('\t}')
	sb.writeln('\th := u32(x ^ (x >> 32))')
	sb.writeln('\tslot := ph_slots[((h >> ${slot_shift}) + ph_disp[h & ${nbuckets - 1}]) & ${tslots - 1}]')
	sb.writeln('\t// branchless finish: read every window of the stored key, compare, and')
	sb.writeln('\t// select the result; ph_mimes[0] is the empty string for a miss')
	sb.writeln('\tel := int((slot >> ${mbits}) & 31)')
	sb.writeln('\toff := int(slot >> ${mbits + 5}) - 1')
	sb.writeln('\tp := unsafe { ph_blob.str + off + 9 }')
	sb.writeln('\tm := ph_masks[l]')
	sb.writeln('\tst1 := unsafe { ph_w8(p) }')
	sb.writeln('\t// slot == 0 needs no test of its own: it stores el = 0, and l >= 1')
	sb.writeln('\tmut eq := u64(el == l)')
	sb.writeln('\teq &= u64((w1 & m) == (st1 & m))')
	sb.writeln('\tif l > 8 {')
	sb.writeln('\t\tstl := unsafe { ph_w8(p + el - 8) }')
	sb.writeln('\t\teq &= u64(wl == stl)')
	sb.writeln('\t}')
	sb.writeln('\tif l > 16 {')
	sb.writeln('\t\tstm := unsafe { ph_w8(p + 8) }')
	sb.writeln('\t\teq &= u64(wm == stm)')
	sb.writeln('\t}')
	sb.writeln('\tidx := if eq != 0 { slot & ${(1 << mbits) - 1} } else { u32(0) }')
	sb.writeln('\treturn ph_mimes[idx]')
	sb.writeln('}')
	sb.writeln('')

	write_file('db.v', sb.str())!
	execute('${@VEXE} fmt -w db.v')
	println('db.v was regenerated. New file size: ${file_size('db.v')} bytes .')
	load := f64(n) / f64(tslots)
	mut max_disp := 0
	for v in disp {
		if int(v) > max_disp {
			max_disp = int(v)
		}
	}
	println('perfect hash: multiplier=0x${m1.hex()}, slots=${tslots}, load=${load:.3f}, unique mimes=${mimes.len}, max displacement=${max_disp}')
}
