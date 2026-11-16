import net.http
import json2 as json
import strings

// Regenerates db.v: the full MIME type database (`const db`), plus a
// case-insensitive extension -> MIME type lookup built as a CHD perfect hash
// (compress, hash, displace; Belazzougui, Boldi, Pagh, Vigna). The generator
// is fully deterministic: given the same db.json input, it always produces
// byte-identical output.

struct MimeType {
	source       string
	extensions   []string
	compressible bool
	charset      string
}

// Candidate multipliers for the rolling polynomial hash. The generator picks
// the first one whose 20-bit hash is injective over all known extensions.
const ph_multipliers = [u32(33), 31, 37, 41, 65599, 131, 17, 39, 43, 53, 97, 193, 5381, 65587]

const ph_injectivity_bits = 20

fn lower_byte(c u8) u8 {
	return if c >= `A` && c <= `Z` { c + 32 } else { c }
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
	m           u32
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
	mut h := u32(l)
	for j in 0 .. l {
		h = h * t.m + lower_byte(ext[i + j])
	}
	slot := t.slots[((h >> t.slot_shift) + t.disp[h & t.bmask]) & t.tmask]
	if slot == 0 {
		return ''
	}
	off := int(slot >> t.mbits) - 1
	el := int(t.blob[off])
	if el != l {
		return ''
	}
	for j in 0 .. el {
		if lower_byte(ext[i + j]) != t.blob[off + 1 + j] {
			return ''
		}
	}
	return t.mimes[slot & t.mime_mask]
}

// rolling polynomial hash over all bytes, seeded with the length
fn ph_hash(ext string, m u32) u32 {
	mut h := u32(ext.len)
	for i in 0 .. ext.len {
		h = h * m + ext[i]
	}
	return h
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
	mt_json := http.get('https://raw.githubusercontent.com/jshttp/mime-db/master/db.json')!
	mt_map := json.decode[map[string]MimeType](mt_json.body)!

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
	mut m := u32(0)
	for cand in ph_multipliers {
		mut seen := map[u32]bool{}
		mut injective := true
		for ext in exts {
			h := ph_hash(ext, cand) & inj_mask
			if h in seen {
				injective = false
				break
			}
			seen[h] = true
		}
		if injective {
			m = cand
			break
		}
	}
	if m == 0 {
		eprintln('no hash multiplier from ph_multipliers is injective over ${n} extensions')
		exit(1)
	}
	mut hashes := []u32{len: n}
	for i, ext in exts {
		hashes[i] = ph_hash(ext, m)
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
	// extensions blob: [u8 ext_len][ext bytes] ...
	mut blob := []u8{cap: n * 8}
	mut ext_off := map[string]int{}
	mut max_ext_len := 0
	for ext in exts {
		if ext.len > 255 {
			eprintln('extension too long: ${ext}')
			exit(1)
		}
		if ext.len > max_ext_len {
			max_ext_len = ext.len
		}
		if ext !in ext_off {
			ext_off[ext] = blob.len
			blob << u8(ext.len)
			blob << ext.bytes()
		}
	}
	// unique media types
	mut mimes := []string{cap: n}
	mut mime_idx := map[string]int{}
	for ext in exts {
		mt := ext_to_mt[ext]
		if mt !in mime_idx {
			mime_idx[mt] = mimes.len
			mimes << mt
		}
	}
	// mime index bits packed into each slot next to the blob offset
	mut mbits := 12
	for mimes.len > (1 << mbits) {
		mbits++
	}
	if blob.len + 1 >= (1 << (32 - mbits)) {
		eprintln('extension blob too large for ${mbits} mime index bits')
		exit(1)
	}
	// slot: 0 = empty, otherwise ((ext_offset + 1) << mbits) | mime_index
	mut packed := []u32{len: tslots}
	for s in 0 .. tslots {
		if slots[s] != -1 {
			i := slots[s]
			ext := exts[i]
			packed[s] = (u32(ext_off[ext] + 1) << mbits) | u32(mime_idx[ext_to_mt[ext]])
		}
	}

	// --- self-check before emitting anything ----------------------------------
	// ph_check mirrors the lookup_generated function emitted into db.v
	tables := PhTables{
		m:           m
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
	}
	if ph_check(tables, '') != '' || ph_check(tables, '.') != ''
		|| ph_check(tables, 'no_such_extension') != '' {
		eprintln('self-check failed for unknown extensions')
		exit(1)
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
	sb.writeln('// constant data. A lookup is a single table probe plus an inline')
	sb.writeln('// case-insensitive byte comparison of the extension.')
	sb.writeln('')
	sb.writeln('const db = ${mt_map}')
	sb.writeln('')
	sb.writeln('// bucket displacement for the perfect hash:')
	sb.writeln('// slot = ((h >> ${slot_shift}) + ph_disp[h & ${nbuckets - 1}]) & ${tslots - 1}')
	sb.writeln('const ph_disp = [')
	for v in disp {
		sb.writeln('\tu16(${v}),')
	}
	sb.writeln(']')
	sb.writeln('')
	sb.writeln('// ${tslots} hash slots: 0 = empty, otherwise ((ext_offset + 1) << ${mbits}) | mime_index')
	sb.writeln('const ph_slots = [')
	for v in packed {
		sb.writeln('\tu32(${v}),')
	}
	sb.writeln(']')
	sb.writeln('')
	sb.writeln('// packed extensions: [u8 ext_len][ext bytes] ...')
	sb.writeln("const ph_blob = '${ph_v_literal(blob)}'")
	sb.writeln('')
	sb.writeln('// unique MIME types, indexed by the low ${mbits} bits of a ph_slots value')
	sb.writeln('const ph_mimes = [')
	for v in mimes {
		sb.writeln("\t'${ph_v_literal(v.bytes())}',")
	}
	sb.writeln(']')
	sb.writeln('')
	sb.writeln('const ph_max_ext_len = ${max_ext_len}')
	sb.writeln('')
	sb.writeln('// lookup_generated returns the MIME type for ext (with or without a')
	sb.writeln('// leading dot), compared ASCII case-insensitively, without allocating.')
	sb.writeln('@[direct_array_access; inline]')
	sb.writeln('fn lookup_generated(ext string) string {')
	sb.writeln('\tmut i := 0')
	sb.writeln('\tif ext.len > 0 && ext[0] == `.` {')
	sb.writeln('\t\ti = 1')
	sb.writeln('\t}')
	sb.writeln('\tl := ext.len - i')
	sb.writeln('\tif l == 0 || l > ph_max_ext_len {')
	sb.writeln("\t\treturn ''")
	sb.writeln('\t}')
	sb.writeln('\tmut h := u32(l)')
	sb.writeln('\tfor j in 0 .. l {')
	sb.writeln('\t\th = h * ${m} + lower_byte(ext[i + j])')
	sb.writeln('\t}')
	sb.writeln('\tslot := ph_slots[((h >> ${slot_shift}) + ph_disp[h & ${nbuckets - 1}]) & ${tslots - 1}]')
	sb.writeln('\tif slot == 0 {')
	sb.writeln("\t\treturn ''")
	sb.writeln('\t}')
	sb.writeln('\toff := int(slot >> ${mbits}) - 1')
	sb.writeln('\tel := int(ph_blob[off])')
	sb.writeln('\tif el != l {')
	sb.writeln("\t\treturn ''")
	sb.writeln('\t}')
	sb.writeln('\tfor j in 0 .. el {')
	sb.writeln('\t\tif lower_byte(ext[i + j]) != ph_blob[off + 1 + j] {')
	sb.writeln("\t\t\treturn ''")
	sb.writeln('\t\t}')
	sb.writeln('\t}')
	sb.writeln('\treturn ph_mimes[slot & ${(1 << mbits) - 1}]')
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
	println('perfect hash: multiplier=${m}, slots=${tslots}, load=${load:.3f}, unique mimes=${mimes.len}, max displacement=${max_disp}')
}
