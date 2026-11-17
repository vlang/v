import net.http
import json2 as json
import strings
import os

// Regenerates db.v: the full MIME type database as static tables (sorted
// names plus packed per-type info, found by `db_index` in lookup.v through
// a CHD perfect hash over the names), and a case-insensitive extension ->
// MIME type lookup built as another CHD perfect hash
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
// All tables are plain constant arrays, which V emits as static data: no
// map is built at program startup. The lookup code itself lives in
// lookup.v; ph_fold, ph_w8 and the mix constants below must match it.
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
// (digits, '-', '_', '.', and all non-ASCII bytes) is left untouched.
@[inline]
fn ph_fold(w u64) u64 {
	a := w & 0x7F7F7F7F7F7F7F7F
	ge_a := (a + 0x3F3F3F3F3F3F3F3F) & 0x8080808080808080
	gt_z := (a + 0x2525252525252525) & 0x8080808080808080
	return w | ((ge_a & ~gt_z & ~w) >> 2)
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

// Chd is a CHD perfect hash over a set of u32 key hashes:
// slot = ((h >> shift) + disp[h & bmask]) & tmask, slots[slot] = key index
struct Chd {
	disp  []u16
	slots []int // -1 = empty
	bmask u32
	tmask u32
	shift int
}

// chd sizes the slot table, buckets the keys by hash, then displaces the
// buckets, largest first (cheaper searches); ties by bucket id, so the
// result is fully deterministic
fn chd(hashes []u32) Chd {
	n := hashes.len
	mut tslots := 2048 // keep the load factor <= 0.75
	for n * 100 > tslots * 75 {
		tslots *= 2
	}
	nbuckets := tslots >> 2
	tmask := u32(tslots - 1)
	bmask := u32(nbuckets - 1)
	// bucket and slot use disjoint bit ranges of the hash
	mut shift := 0
	for (1 << shift) < nbuckets {
		shift++
	}
	mut buckets := [][]int{len: nbuckets, init: []int{cap: 4}}
	for i in 0 .. n {
		buckets[hashes[i] & bmask] << i
	}
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
			if buckets[b].len == size {
				displace_bucket(b, buckets[b], hashes, mut slots, mut disp, tmask, shift)
			}
		}
	}
	return Chd{
		disp:  disp
		slots: slots
		bmask: bmask
		tmask: tmask
		shift: shift
	}
}

// name_hash hashes a whole MIME type name as 8-byte little-endian words;
// it must match `name_hash` in lookup.v
fn name_hash(s string) u32 {
	mut h := u64(s.len) * ph_m4
	mut i := 0
	for i + 8 <= s.len {
		h = (h ^ unsafe { ph_w8(s.str + i) }) * ph_m3
		h ^= h >> 29
		i += 8
	}
	if i < s.len {
		mut w := u64(0)
		if s.len >= 8 {
			w = unsafe { ph_w8(s.str + s.len - 8) }
		} else {
			for j in 0 .. s.len {
				w |= u64(s[j]) << (8 * j)
			}
		}
		h = (h ^ w) * ph_m3
		h ^= h >> 29
	}
	h *= ph_m2
	return u32(h ^ (h >> 32))
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
	names       []string
}

// mirrors `lookup` in lookup.v, so that the tables
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
	// branchless finish, mirroring `lookup`
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
	return t.names[idx]
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

// write `const name = [items...]` on one line; vfmt wraps it densely
fn emit_list(mut sb strings.Builder, name string, items []string) {
	sb.writeln('const ${name} = [${items.join(', ')}]')
	sb.writeln('')
}

// typed wraps the first element in a `typ(...)` cast, which types the
// whole array literal
fn typed(typ string, vals []string) []string {
	mut res := vals.clone()
	if res.len > 0 {
		res[0] = '${typ}(${res[0]})'
	}
	return res
}

fn quoted(s []string) []string {
	return s.map("'${ph_v_literal(it.bytes())}'")
}

// index_table returns the sorted distinct values of `vals`, with '' first
fn index_table(vals []string, limit int, what string) []string {
	mut seen := map[string]bool{}
	seen[''] = true
	for v in vals {
		seen[v] = true
	}
	mut res := seen.keys()
	res.sort()
	if res.len > limit {
		eprintln('too many distinct ${what} values: ${res.len} > ${limit}')
		exit(1)
	}
	return res
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
	mut mt_map := json.decode[map[string]MimeType](db_text)!
	// the V source extensions, see the overrides in ext_to_mt below
	if 'text/x-vlang' !in mt_map {
		mt_map['text/x-vlang'] = MimeType{
			extensions:   ['v', 'vsh']
			compressible: true
		}
	}

	// --- the MIME type database ------------------------------------------------
	// db_names is sorted for a deterministic output; index 0 is the
	// empty string, used both for unknown types and for unknown extensions
	mut names := mt_map.keys()
	names.sort()
	names.insert(0, '')
	mut name_idx := map[string]int{}
	for i, name in names {
		name_idx[name] = i
	}
	mut all_sources := []string{}
	mut all_charsets := []string{}
	for _, mt in mt_map {
		all_sources << mt.source
		all_charsets << mt.charset
	}
	sources := index_table(all_sources, 8, 'source')
	charsets := index_table(all_charsets, 16, 'charset')
	// db_info[i] = ext_start << 16 | ext_count << 8 | charset << 4 |
	// source << 1 | compressible, with the extensions in db_exts
	mut db_exts := []string{}
	mut info := []u32{len: names.len}
	for i, name in names {
		if i == 0 {
			continue
		}
		mt := mt_map[name]
		if mt.extensions.len > 255 || db_exts.len + mt.extensions.len > 0xFFFF {
			eprintln('too many extensions for db_info at ${name}')
			exit(1)
		}
		info[i] = (u32(db_exts.len) << 16) | (u32(mt.extensions.len) << 8) | (u32(charsets.index(mt.charset)) << 4) | (u32(sources.index(mt.source)) << 1) | u32(mt.compressible)
		db_exts << mt.extensions
	}

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
	mut exts := ext_to_mt.keys()
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

	ph := chd(hashes)
	tslots := ph.slots.len
	slots := ph.slots
	disp := ph.disp
	tmask := ph.tmask
	bmask := ph.bmask
	slot_shift := ph.shift

	// --- pack the tables ------------------------------------------------------
	// extensions blob: 8 padding bytes, then [u8 ext_len][ext bytes] ... per
	// extension, then 16 more padding bytes. The padding lets `lookup` read
	// every window of any stored key with one word load, whatever its
	// position and length (the middle window reads key bytes 8..16, even
	// for a 1 byte key at the very end), so the whole comparison after the
	// slot probe is branchless and in bounds.
	mut blob := []u8{len: 8, cap: n * 8 + 24}
	mut ext_off := map[string]int{}
	for ext in exts {
		if ext.len > 24 {
			eprintln('extension longer than 24 bytes cannot be covered by the hash windows: ${ext}')
			exit(1)
		}
		// the offset of the entry's length byte, ignoring the leading
		// padding (`lookup` adds it back as a constant)
		ext_off[ext] = blob.len - 8
		blob << u8(ext.len)
		blob << ext.bytes()
	}
	blob << []u8{len: 16}
	// the slots point into db_names, whose index 0 is the empty string, so
	// a miss can select it with a single conditional move
	mut mbits := 12
	for names.len > (1 << mbits) {
		mbits++
	}
	if blob.len >= (1 << (32 - mbits - 5)) {
		eprintln('extension blob too large for ${mbits} mime index bits')
		exit(1)
	}
	// slot: 0 = empty, otherwise
	//   ((ext_offset + 1) << (mbits + 5)) | (ext_len << mbits) | name_index
	mut packed := []u32{len: tslots}
	for s in 0 .. tslots {
		if slots[s] != -1 {
			ext := exts[slots[s]]
			packed[s] = (u32(ext_off[ext] + 1) << (mbits + 5)) | (u32(ext.len) << mbits) | u32(name_idx[ext_to_mt[ext]])
		}
	}

	// --- self-check before emitting anything ----------------------------------
	tables := PhTables{
		m1:          m1
		slot_shift:  slot_shift
		mbits:       mbits
		max_ext_len: 24
		bmask:       bmask
		tmask:       tmask
		mime_mask:   u32((1 << mbits) - 1)
		disp:        disp
		slots:       packed
		blob:        blob
		names:       names
	}
	for ext in exts {
		want := ext_to_mt[ext]
		for variant in [ext, '.' + ext, ext.to_upper(), '.' + ext.to_upper()] {
			if ph_check(tables, variant) != want {
				eprintln('self-check failed for extension: ${variant}')
				exit(1)
			}
		}
	}
	for miss in ['', '.', 'no_such_extension', 'not-an-extension', 'index.html', 'htmX', 'W0FF',
		'pngx', '..html', 'html ', 'zzzzzzzzzzzzzzzzzzzzzzzzzzz', 'j\xcdon'] {
		if ph_check(tables, miss) != '' {
			eprintln('self-check failed for unknown extension: ${miss}')
			exit(1)
		}
	}

	// --- MIME type name -> db_names index, another CHD ------------------------
	nh := chd(names[1..].map(name_hash(it)))
	if names.len > 0xFFFF {
		eprintln('too many MIME types for u16 name slots: ${names.len}')
		exit(1)
	}
	// 0 = empty slot, which selects the '' sentinel name
	nh_slots := nh.slots.map(if it == -1 { u16(0) } else { u16(it + 1) })
	for i, name in names {
		for probe in [name, name + 'x', name#[..-1]] {
			h := name_hash(probe)
			idx := int(nh_slots[((h >> nh.shift) + nh.disp[h & nh.bmask]) & nh.tmask])
			got := if names[idx] == probe { idx } else { 0 }
			want := if probe == name { i } else { name_idx[probe] or { 0 } }
			if got != want {
				eprintln('self-check failed for MIME type: ${probe}')
				exit(1)
			}
		}
	}

	// --- emit db.v -------------------------------------------------------------
	mut sb := strings.new_builder(512 * 1024)
	sb.writeln('module mime')
	sb.writeln('')
	sb.writeln('// FILE AUTOGENERATED BY `build.vsh` - DO NOT MANUALLY EDIT')
	sb.writeln('//')
	sb.writeln('// The MIME type database and a CHD perfect hash over ${n} file extensions,')
	sb.writeln('// as constant data; the code using it is in lookup.v and mime.v.')
	sb.writeln('')
	sb.writeln('const ph_m1 = u64(0x${m1.hex()})')
	sb.writeln('const ph_slot_shift = ${slot_shift}')
	sb.writeln('const ph_bmask = u32(${bmask})')
	sb.writeln('const ph_tmask = u32(${tmask})')
	sb.writeln('const ph_mbits = ${mbits}')
	sb.writeln('')
	sb.writeln('// bucket displacements: slot = ((h >> ph_slot_shift) + ph_disp[h & ph_bmask]) & ph_tmask')
	emit_list(mut sb, 'ph_disp', typed('u16', disp.map(it.str())))
	sb.writeln('// hash slots: 0 = empty, otherwise')
	sb.writeln('//   ((ext_offset + 1) << (ph_mbits + 5)) | (ext_len << ph_mbits) | db_names index')
	emit_list(mut sb, 'ph_slots', typed('u32', packed.map(it.str())))
	sb.writeln('// packed extensions: 8 padding bytes, [u8 ext_len][ext bytes] per extension,')
	sb.writeln('// then 16 padding bytes, so every key window is readable with one word load')
	sb.writeln("const ph_blob = '${ph_v_literal(blob)}'")
	sb.writeln('')
	sb.writeln('// sorted MIME type names; index 0 is the empty string for unknown types')
	emit_list(mut sb, 'db_names', quoted(names))
	sb.writeln('// name hash: db_names index = nh_slots[((h >> nh_shift) + nh_disp[h & nh_bmask]) & nh_tmask]')
	sb.writeln('const nh_shift = ${nh.shift}')
	sb.writeln('const nh_bmask = u32(${nh.bmask})')
	sb.writeln('const nh_tmask = u32(${nh.tmask})')
	sb.writeln('')
	emit_list(mut sb, 'nh_disp', typed('u16', nh.disp.map(it.str())))
	emit_list(mut sb, 'nh_slots', typed('u16', nh_slots.map(it.str())))
	emit_list(mut sb, 'db_sources', quoted(sources))
	emit_list(mut sb, 'db_charsets', quoted(charsets))
	sb.writeln('// the extensions of every MIME type, in db_names order')
	emit_list(mut sb, 'db_exts', quoted(db_exts))
	sb.writeln('// per db_names entry: ext_start << 16 | ext_count << 8 | db_charsets index << 4 |')
	sb.writeln('//   db_sources index << 1 | compressible')
	emit_list(mut sb, 'db_info', typed('u32', info.map(it.str())))

	write_file('db.v', sb.str())!
	res := os.exec([@VEXE, 'fmt', '-w', 'db.v'])
	if res.exit_code != 0 {
		eprintln(res.output)
		exit(1)
	}
	println('db.v was regenerated. New file size: ${file_size('db.v')} bytes .')
	load := f64(n) / f64(tslots)
	mut max_disp := 0
	for v in disp {
		if int(v) > max_disp {
			max_disp = int(v)
		}
	}
	println('perfect hash: multiplier=0x${m1.hex()}, slots=${tslots}, load=${load:.3f}, mime types=${names.len - 1}, max displacement=${max_disp}')
}
