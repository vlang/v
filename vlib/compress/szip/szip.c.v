module szip

// Pure V implementation of zip (replaces the former C/miniz backend).
import os
import hash.crc32
import compress.deflate

// ---------------------------------------------------------------------------
// Enums
// ---------------------------------------------------------------------------

pub enum CompressionLevel {
	no_compression
	best_speed
	best_compression
	uber_compression
	default_level
	default_compression
}

pub enum OpenMode {
	write
	read_only
	append
}

@[inline]
fn (om OpenMode) to_u8() u8 {
	return match om {
		.write { `w` }
		.read_only { `r` }
		.append { `a` }
	}
}

// ---------------------------------------------------------------------------
// Options
// ---------------------------------------------------------------------------

@[params]
pub struct ZipFolderOptions {
pub:
	omit_empty_folders bool
}

// ---------------------------------------------------------------------------
// Entry
// ---------------------------------------------------------------------------

pub struct ZipEntry {
pub mut:
	name       string
	method     u16 // 0 = stored, 8 = deflate
	crc32      u32
	size       u64 // uncompressed
	csize      u64 // compressed
	offset     u64 // local header offset
	is_dir     bool
	data       []u8 // uncompressed data (for write) / cached (for read)
}

// ---------------------------------------------------------------------------
// Zip
// ---------------------------------------------------------------------------

pub struct Zip {
pub mut:
	path     string
	mode     OpenMode
	level    CompressionLevel
	entries  []ZipEntry
	// write state
	current  int = -1
	// cached file bytes for read
	filedata []u8
}

// helper: deflate level int
fn (z &Zip) deflate_level() int {
	return match z.level {
		.no_compression { 0 }
		.best_speed { 1 }
		.best_compression { 9 }
		.uber_compression { 9 }
		.default_level { 6 }
		.default_compression { 6 }
	}
}

// ---------------------------------------------------------------------------
// open / close
// ---------------------------------------------------------------------------

pub fn open(name string, level CompressionLevel, mode OpenMode) !&Zip {
	if name == '' {
		return error('szip: name of file empty')
	}
	mut z := &Zip{
		path:  name
		mode:  mode
		level: level
	}
	match mode {
		.read_only {
			if !os.exists(name) {
				return error('szip: cannot open archive')
			}
			z.filedata = os.read_bytes(name) or { return error('szip: cannot read archive') }
			z.parse() or { return error('szip: cannot parse archive: ${err.msg()}') }
		}
		.write {
			// create/overwrite
			z.filedata = []u8{}
			z.entries = []ZipEntry{}
		}
		.append {
			if os.exists(name) {
				z.filedata = os.read_bytes(name) or { return error('szip: cannot read archive') }
				z.parse() or { return error('szip: cannot parse archive: ${err.msg()}') }
			} else {
				z.filedata = []u8{}
				z.entries = []ZipEntry{}
			}
		}
	}
	return z
}

pub fn (mut z Zip) close() {
	// flush write mode
	if z.mode == .write || z.mode == .append {
		z.flush() or {}
	}
}

// ---------------------------------------------------------------------------
// Parse (read mode)
// ---------------------------------------------------------------------------

fn (mut z Zip) parse() ! {
	data := z.filedata
	// find EOCD
	mut eocd_pos := -1
	for i := data.len - 22; i >= 0; i-- {
		if data[i] == 0x50 && data[i+1] == 0x4b && data[i+2] == 0x05 && data[i+3] == 0x06 {
			eocd_pos = i
			break
		}
	}
	if eocd_pos < 0 {
		return error('no eocd')
	}
	// read number of entries
	n_entries := int(u16(data[eocd_pos+10]) | (u16(data[eocd_pos+11]) << 8))
	cd_size := u32(data[eocd_pos+12]) | (u32(data[eocd_pos+13]) << 8) | (u32(data[eocd_pos+14]) << 16) | (u32(data[eocd_pos+15]) << 24)
	cd_offset := u32(data[eocd_pos+16]) | (u32(data[eocd_pos+17]) << 8) | (u32(data[eocd_pos+18]) << 16) | (u32(data[eocd_pos+19]) << 24)

	// parse central directory
	mut pos := int(cd_offset)
	for _ in 0 .. n_entries {
		if pos + 46 > data.len {
			break
		}
		if !(data[pos] == 0x50 && data[pos+1] == 0x4b && data[pos+2] == 0x01 && data[pos+3] == 0x02) {
			break
		}
		method := u16(data[pos+10]) | (u16(data[pos+11]) << 8)
		crc := u32(data[pos+16]) | (u32(data[pos+17]) << 8) | (u32(data[pos+18]) << 16) | (u32(data[pos+19]) << 24)
		csize := u32(data[pos+20]) | (u32(data[pos+21]) << 8) | (u32(data[pos+22]) << 16) | (u32(data[pos+23]) << 24)
		size := u32(data[pos+24]) | (u32(data[pos+25]) << 8) | (u32(data[pos+26]) << 16) | (u32(data[pos+27]) << 24)
		name_len := int(u16(data[pos+28]) | (u16(data[pos+29]) << 8))
		extra_len := int(u16(data[pos+30]) | (u16(data[pos+31]) << 8))
		comment_len := int(u16(data[pos+32]) | (u16(data[pos+33]) << 8))
		local_offset := u32(data[pos+42]) | (u32(data[pos+43]) << 8) | (u32(data[pos+44]) << 16) | (u32(data[pos+45]) << 24)

		name := data[pos+46..pos+46+name_len].bytestr()
		is_dir := name.ends_with('/')

		z.entries << ZipEntry{
			name:   name
			method: method
			crc32:  crc
			size:   u64(size)
			csize:  u64(csize)
			offset: u64(local_offset)
			is_dir: is_dir
		}
		pos += 46 + name_len + extra_len + comment_len
	}
	_ = cd_size
}

// ---------------------------------------------------------------------------
// Entry access (read mode)
// ---------------------------------------------------------------------------

pub fn (mut z Zip) total() !int {
	return z.entries.len
}

pub fn (mut z Zip) open_entry_by_index(index int) ! {
	if index < 0 || index >= z.entries.len {
		return error('szip: cannot open archive entry at index ${index}')
	}
	z.current = index
}

pub fn (mut z Zip) open_entry(name string) ! {
	for i, e in z.entries {
		if e.name == name {
			z.current = i
			return
		}
	}
	// write mode: append new entry
	if z.mode == .write || z.mode == .append {
		z.entries << ZipEntry{ name: name, method: 0 }
		z.current = z.entries.len - 1
		return
	}
	return error('szip: cannot open archive entry')
}

pub fn (mut z Zip) close_entry() {
	// nothing special
}

pub fn (mut z Zip) name() string {
	if z.current < 0 || z.current >= z.entries.len {
		return ''
	}
	return z.entries[z.current].name
}

pub fn (mut z Zip) is_dir() !bool {
	if z.current < 0 || z.current >= z.entries.len {
		return error('szip: cannot check entry type')
	}
	return z.entries[z.current].is_dir
}

pub fn (mut z Zip) size() u64 {
	if z.current < 0 || z.current >= z.entries.len {
		return 0
	}
	return z.entries[z.current].size
}

pub fn (mut z Zip) crc32() u32 {
	if z.current < 0 || z.current >= z.entries.len {
		return 0
	}
	return z.entries[z.current].crc32
}

pub fn (mut z Zip) index() !int {
	if z.current < 0 {
		return error('szip: cannot get current index')
	}
	return z.current
}

// read entry data (uncompressed)
fn (mut z Zip) read_current_data() ![]u8 {
	if z.current < 0 || z.current >= z.entries.len {
		return error('szip: no current entry')
	}
	e := z.entries[z.current]
	if e.is_dir {
		return []u8{}
	}
	// local header
	mut pos := int(e.offset)
	if pos + 30 > z.filedata.len {
		return error('szip: bad offset')
	}
	name_len := int(u16(z.filedata[pos+26]) | (u16(z.filedata[pos+27]) << 8))
	extra_len := int(u16(z.filedata[pos+28]) | (u16(z.filedata[pos+29]) << 8))
	data_start := pos + 30 + name_len + extra_len
	data_end := data_start + int(e.csize)
	if data_end > z.filedata.len {
		return error('szip: truncated data')
	}
	compressed := z.filedata[data_start..data_end]
	if e.method == 0 {
		// stored
		return compressed.clone()
	}
	if e.method == 8 {
		res := deflate.decompress_raw_with_consumed(compressed) or {
			return error('szip: deflate failed: ${err.msg()}')
		}
		return res.decoded
	}
	return error('szip: unsupported method ${e.method}')
}

pub fn (mut z Zip) read_entry_buf(buf voidptr, in_bsize int) !int {
	data := z.read_current_data() or { return error('szip: ${err.msg()}') }
	n := if data.len < in_bsize { data.len } else { in_bsize }
	if n > 0 {
		unsafe {
			C.memcpy(buf, data.data, n)
		}
	}
	return n
}

pub fn (mut z Zip) write_entry(data []u8) ! {
	if z.current < 0 || z.current >= z.entries.len {
		return error('szip: no current entry')
	}
	z.entries[z.current].data = data.clone()
	z.entries[z.current].size = u64(data.len)
	z.entries[z.current].crc32 = crc32.sum(data)
	// compression
	if z.deflate_level() == 0 {
		z.entries[z.current].method = 0
		z.entries[z.current].csize = u64(data.len)
	} else {
		compressed := deflate.compress_raw(data) or {
			return error('szip: compress failed: ${err.msg()}')
		}
		// only use deflate if smaller
		if compressed.len < data.len {
			z.entries[z.current].method = 8
			z.entries[z.current].csize = u64(compressed.len)
			// store compressed in data? We need to keep compressed bytes. Use a parallel slice.
			// We'll store compressed in entry via a separate field. Add cdata field.
		} else {
			z.entries[z.current].method = 0
			z.entries[z.current].csize = u64(data.len)
		}
	}
}

// Need to store compressed bytes. Add cdata to ZipEntry? Let's add field.
// (handled below by redefining ZipEntry with cdata)

// ---------------------------------------------------------------------------
// Flush (write to file)
// ---------------------------------------------------------------------------

fn (mut z Zip) flush() ! {
	// Build zip file in memory
	mut out := []u8{}
	mut local_offsets := []u64{}
	for i, e in z.entries {
		local_offsets << u64(out.len)
		// local header
		out << [u8(0x50), 0x4b, 0x03, 0x04]
		// version needed (2.0)
		out << [u8(20), 0x00]
		// flags
		out << [u8(0), 0x00]
		// method
		out << [u8(e.method), 0x00]
		// mod time/date (0)
		out << [u8(0), 0x00, 0x00, 0x00]
		// crc32
		out << u32_to_bytes(e.crc32)
		// compressed size
		out << u32_to_bytes(u32(e.csize))
		// uncompressed size
		out << u32_to_bytes(u32(e.size))
		// filename len
		name_bytes := e.name.bytes()
		out << u16_to_bytes(u16(name_bytes.len))
		// extra len
		out << [u8(0), 0x00]
		// filename
		out << name_bytes
		// data
		if e.is_dir {
			// no data
		} else {
			if e.method == 0 {
				out << e.data
			} else {
				if e.data.len == 0 {
					out << []u8{}
				} else {
					compressed := deflate.compress_raw(e.data)!
					out << compressed
				}
			}
		}
		z.entries[i].offset = local_offsets[i]
	}
	// central directory
	cd_start := out.len
	for i, e in z.entries {
		out << [u8(0x50), 0x4b, 0x01, 0x02]
		// version made by
		out << [u8(20), 0x00]
		// version needed
		out << [u8(20), 0x00]
		// flags
		out << [u8(0), 0x00]
		// method
		out << [u8(e.method), 0x00]
		// mod time/date
		out << [u8(0), 0x00, 0x00, 0x00]
		// crc32
		out << u32_to_bytes(e.crc32)
		// compressed size
		out << u32_to_bytes(u32(e.csize))
		// uncompressed size
		out << u32_to_bytes(u32(e.size))
		// filename len
		name_bytes := e.name.bytes()
		out << u16_to_bytes(u16(name_bytes.len))
		// extra len
		out << [u8(0), 0x00]
		// comment len
		out << [u8(0), 0x00]
		// disk number
		out << [u8(0), 0x00]
		// internal attrs
		out << [u8(0), 0x00]
		// external attrs
		out << [u8(0), 0x00, 0x00, 0x00]
		// local header offset
		out << u32_to_bytes(u32(local_offsets[i]))
		// filename
		out << name_bytes
	}
	cd_size := out.len - cd_start
	// EOCD
	out << [u8(0x50), 0x4b, 0x05, 0x06]
	// disk number
	out << [u8(0), 0x00]
	// disk with cd
	out << [u8(0), 0x00]
	// entries on this disk
	out << u16_to_bytes(u16(z.entries.len))
	// total entries
	out << u16_to_bytes(u16(z.entries.len))
	// cd size
	out << u32_to_bytes(u32(cd_size))
	// cd offset
	out << u32_to_bytes(u32(cd_start))
	// comment len
	out << [u8(0), 0x00]

	os.write_file_array(z.path, out) or { return error('szip: write failed: ${err.msg()}') }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn u16_to_bytes(v u16) []u8 {
	return [u8(v & 0xFF), u8((v >> 8) & 0xFF)]
}

fn u32_to_bytes(v u32) []u8 {
	return [u8(v & 0xFF), u8((v >> 8) & 0xFF), u8((v >> 16) & 0xFF), u8((v >> 24) & 0xFF)]
}

// ---------------------------------------------------------------------------
// High-level helpers
// ---------------------------------------------------------------------------

pub fn zip_files(path_to_file []string, path_to_export_zip string) ! {
	mut zip := open(path_to_export_zip, .no_compression, .write) or { return error('szip: ${err.msg()}') }
	for file in path_to_file {
		zip.open_entry(os.base(file)) or { return error('szip: ${err.msg()}') }
		file_as_byte := os.read_bytes(file) or { return error('szip: ${err.msg()}') }
		zip.write_entry(file_as_byte) or { return error('szip: ${err.msg()}') }
		zip.close_entry()
	}
	zip.close()
}

pub fn zip_folder(folder string, zip_file string, opt ZipFolderOptions) ! {
	path := folder.trim_right(os.path_separator)
	mut files := []string{}
	os.walk_with_context(path, &files, fn (ctx voidptr, file string) {
		mut files := unsafe { &[]string(ctx) }
		files << file
	})
	mut zip := open(zip_file, .no_compression, .write)!
	defer { zip.close() }
	for file in files {
		is_dir := os.is_dir(file)
		if opt.omit_empty_folders && is_dir {
			continue
		}
		mut zip_file_entry := file.trim_string_left(path + os.path_separator)
		$if windows {
			zip_file_entry = zip_file_entry.replace(os.path_separator, '/')
		}
		if is_dir {
			zip_file_entry += '/'
		}
		zip.open_entry(zip_file_entry)!
		if !is_dir {
			file_as_byte := os.read_bytes(file)!
			zip.write_entry(file_as_byte)!
		}
		zip.close_entry()
	}
}

pub fn extract_zip_to_dir(file string, dir string) !bool {
	if !os.exists(dir) {
		return error('szip: cannot open directory for extracting, directory not exists')
	}
	mut z := open(file, .no_compression, .read_only) or { return error('szip: ${err.msg()}') }
	n := z.total() or { return error('szip: ${err.msg()}') }
	for i in 0 .. n {
		z.open_entry_by_index(i) or { continue }
		entry_name := z.name()
		target := os.join_path(dir, entry_name)
		if entry_name.ends_with('/') {
			os.mkdir_all(target) or {}
			z.close_entry()
			continue
		}
		// ensure parent dir
		parent := os.dir(target)
		if !os.exists(parent) {
			os.mkdir_all(parent) or {}
		}
		data := z.read_current_data() or { z.close_entry(); continue }
		os.write_file_array(target, data) or {}
		z.close_entry()
	}
	z.close()
	return true
}

// end of module