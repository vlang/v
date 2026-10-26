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
	name   string
	method u16 // 0 = stored, 8 = deflate
	flags  u16
	crc32  u32
	size   u64 // uncompressed
	csize  u64 // compressed
	offset u64 // local header offset
	is_dir bool
	data   []u8 // uncompressed data (for write) / cached (for read)
}

// ---------------------------------------------------------------------------
// Zip
// ---------------------------------------------------------------------------

pub struct Zip {
pub mut:
	path    string
	mode    OpenMode
	level   CompressionLevel
	entries []ZipEntry
	// write state
	current int = -1
	// cached file bytes for read
	filedata []u8
mut:
	directory_offset int
	validated        bool
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

// close flushes pending entries; use zip_files or zip_folder when write errors must be returned.
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
	mut eocd_pos := -1
	// The end record may be followed only by its declared archive comment.
	for i := data.len - 22; i >= 0 && i >= data.len - 65557; i-- {
		if data[i..i + 4] == [u8(0x50), 0x4b, 0x05, 0x06] {
			comment_len := int(u16(data[i + 20]) | (u16(data[i + 21]) << 8))
			if i + 22 + comment_len == data.len {
				eocd_pos = i
				break
			}
		}
	}
	if eocd_pos < 0 {
		return error('missing end record')
	}
	if data[eocd_pos + 4..eocd_pos + 8] != [u8(0), 0, 0, 0] {
		return error('multi-disk archives are unsupported')
	}
	n_entries := int(u16(data[eocd_pos + 10]) | (u16(data[eocd_pos + 11]) << 8))
	n_disk_entries := int(u16(data[eocd_pos + 8]) | (u16(data[eocd_pos + 9]) << 8))
	cd_size := u32(data[eocd_pos + 12]) | (u32(data[eocd_pos + 13]) << 8) | (u32(data[eocd_pos + 14]) << 16) | (u32(data[eocd_pos + 15]) << 24)
	cd_offset := u32(data[eocd_pos + 16]) | (u32(data[eocd_pos + 17]) << 8) | (u32(data[eocd_pos + 18]) << 16) | (u32(data[eocd_pos + 19]) << 24)
	if n_entries == 0xffff || cd_size == 0xffffffff || cd_offset == 0xffffffff {
		return error('ZIP64 archives are unsupported')
	}
	if n_disk_entries != n_entries || u64(cd_offset) + u64(cd_size) > u64(eocd_pos) {
		return error('invalid central directory bounds')
	}
	z.directory_offset = int(cd_offset)
	cd_end := int(cd_offset) + int(cd_size)
	mut pos := int(cd_offset)
	for _ in 0 .. n_entries {
		if pos > cd_end - 46 || data[pos..pos + 4] != [u8(0x50), 0x4b, 0x01, 0x02] {
			return error('invalid central directory entry')
		}
		flags := u16(data[pos + 8]) | (u16(data[pos + 9]) << 8)
		if flags & 1 != 0 {
			return error('encrypted entries are unsupported')
		}
		method := u16(data[pos + 10]) | (u16(data[pos + 11]) << 8)
		if method !in [u16(0), 8] {
			return error('unsupported compression method')
		}
		crc := u32(data[pos + 16]) | (u32(data[pos + 17]) << 8) | (u32(data[pos + 18]) << 16) | (u32(data[pos + 19]) << 24)
		csize := u32(data[pos + 20]) | (u32(data[pos + 21]) << 8) | (u32(data[pos + 22]) << 16) | (u32(data[pos + 23]) << 24)
		size := u32(data[pos + 24]) | (u32(data[pos + 25]) << 8) | (u32(data[pos + 26]) << 16) | (u32(data[pos + 27]) << 24)
		name_len := int(u16(data[pos + 28]) | (u16(data[pos + 29]) << 8))
		extra_len := int(u16(data[pos + 30]) | (u16(data[pos + 31]) << 8))
		comment_len := int(u16(data[pos + 32]) | (u16(data[pos + 33]) << 8))
		local_offset := u32(data[pos + 42]) | (u32(data[pos + 43]) << 8) | (u32(data[pos + 44]) << 16) | (u32(data[pos + 45]) << 24)
		entry_end := pos + 46 + name_len + extra_len + comment_len
		if name_len == 0 || entry_end > cd_end || csize == 0xffffffff || size == 0xffffffff || local_offset == 0xffffffff {
			return error('invalid central directory metadata')
		}
		name := data[pos + 46..pos + 46 + name_len].bytestr()
		z.entries << ZipEntry{
			name:   name
			method: method
			flags:  flags
			crc32:  crc
			size:   u64(size)
			csize:  u64(csize)
			offset: u64(local_offset)
			is_dir: name.ends_with('/')
		}
		pos = entry_end
	}
	if pos != cd_end {
		return error('central directory size mismatch')
	}
	// Cache validated payloads so reads and append mode do not decompress entries twice.
	for i in 0 .. z.entries.len {
		z.current = i
		z.entries[i].data = z.read_current_data()!
	}
	z.current = -1
	z.validated = true
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

// open_entry finds a name case-insensitively when reading, or appends a new writable entry.
pub fn (mut z Zip) open_entry(name string) ! {
	z.current = -1
	normalized := name.replace('\\', '/')
	if normalized.len == 0 {
		return error('szip: entry name is empty')
	}
	if z.mode == .write || z.mode == .append {
		z.entries << ZipEntry{
			name:   normalized
			method: 0
			is_dir: normalized.ends_with('/')
		}
		z.current = z.entries.len - 1
		return
	}
	for i, e in z.entries {
		if e.name.to_lower() == normalized.to_lower() {
			z.current = i
			return
		}
	}
	return error('szip: cannot open archive entry')
}

pub fn (mut z Zip) close_entry() {
	z.current = -1
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
	if z.current < 0 || z.current >= z.entries.len {
		return error('szip: cannot get current index of zip entry')
	}
	return z.current
}

// read entry data (uncompressed)
fn (mut z Zip) read_current_data() ![]u8 {
	if z.current < 0 || z.current >= z.entries.len {
		return error('szip: no current entry')
	}
	e := z.entries[z.current]
	if z.validated {
		return e.data.clone()
	}
	if e.offset > u64(z.directory_offset) || e.offset + 30 > u64(z.directory_offset) {
		return error('szip: bad local header offset')
	}
	pos := int(e.offset)
	data := z.filedata
	if data[pos..pos + 4] != [u8(0x50), 0x4b, 0x03, 0x04] {
		return error('szip: invalid local header')
	}
	flags := read_u16(data, pos + 6)
	if flags != e.flags || flags & 1 != 0 || read_u16(data, pos + 8) != e.method {
		return error('szip: inconsistent local header')
	}
	name_len := int(read_u16(data, pos + 26))
	extra_len := int(read_u16(data, pos + 28))
	data_start := pos + 30 + name_len + extra_len
	if data_start > z.directory_offset || e.csize > u64(z.directory_offset - data_start) {
		return error('szip: truncated entry data')
	}
	if data[pos + 30..pos + 30 + name_len].bytestr() != e.name {
		return error('szip: inconsistent entry name')
	}
	// With a data descriptor, the central directory carries the final sizes and checksum.
	if flags & 8 == 0 && (read_u32(data, pos + 14) != e.crc32
		|| u64(read_u32(data, pos + 18)) != e.csize
		|| u64(read_u32(data, pos + 22)) != e.size) {
		return error('szip: inconsistent entry metadata')
	}
	data_end := data_start + int(e.csize)
	if flags & 8 != 0 {
		mut descriptor := data_end
		if descriptor <= z.directory_offset - 4 && read_u32(data, descriptor) == 0x08074b50 {
			descriptor += 4
		}
		if descriptor > z.directory_offset - 12 || read_u32(data, descriptor) != e.crc32
			|| u64(read_u32(data, descriptor + 4)) != e.csize
			|| u64(read_u32(data, descriptor + 8)) != e.size {
			return error('szip: invalid data descriptor')
		}
	}
	compressed := data[data_start..data_end]
	mut decoded := []u8{}
	if e.method == 0 {
		decoded = compressed.clone()
	} else if e.method == 8 {
		res := deflate.decompress_raw_with_consumed(compressed) or {
			return error('szip: deflate failed: ${err.msg()}')
		}
		if res.consumed != compressed.len {
			return error('szip: compressed size mismatch')
		}
		decoded = res.decoded
	} else {
		return error('szip: unsupported method ${e.method}')
	}
	if u64(decoded.len) != e.size || crc32.sum(decoded) != e.crc32 {
		return error('szip: entry size or checksum mismatch')
	}
	if e.is_dir && decoded.len != 0 {
		return error('szip: directory entry contains data')
	}
	return decoded
}

pub fn (mut z Zip) read_entry_buf(buf voidptr, in_bsize int) !int {
	data := z.read_current_data() or { return error('szip: ${err.msg()}') }
	if in_bsize < data.len || (data.len > 0 && buf == unsafe { nil }) {
		return error('szip: cannot read properly data from entry')
	}
	n := data.len
	if n > 0 {
		unsafe {
			mut dst := &u8(buf)
			for i in 0 .. n {
				dst[i] = data[i]
			}
		}
	}
	return n
}

// read_entry allocates the current entry's bytes; the caller must free the returned buffer.
pub fn (mut z Zip) read_entry() !voidptr {
	data := z.read_current_data()!
	// Allocate one byte for empty entries too, preserving the owning-buffer API.
	// The entry API transfers ownership of this allocation to its caller.
	buf := unsafe { malloc(if data.len == 0 { 1 } else { data.len }) }
	if data.len > 0 {
		unsafe { vmemcpy(buf, data.data, data.len) }
	}
	return buf
}

// create_entry writes the file at name to the current archive entry.
pub fn (mut z Zip) create_entry(name string) ! {
	data := os.read_bytes(name) or { return error('szip: cannot read file: ${err.msg()}') }
	z.write_entry(data)!
}

// extract_entry writes the current archive entry to path.
pub fn (mut z Zip) extract_entry(path string) ! {
	data := z.read_current_data()!
	os.write_file_array(path, data) or { return error('szip: cannot extract entry: ${err.msg()}') }
}

// write_entry appends bytes to the currently open writable entry.
pub fn (mut z Zip) write_entry(data []u8) ! {
	if z.mode == .read_only {
		return error('szip: archive is read-only')
	}
	if z.current < 0 || z.current >= z.entries.len {
		return error('szip: no current entry')
	}
	z.entries[z.current].data << data
	contents := z.entries[z.current].data
	z.entries[z.current].size = u64(contents.len)
	z.entries[z.current].crc32 = crc32.sum(contents)
	// compression
	if z.deflate_level() == 0 {
		z.entries[z.current].method = 0
		z.entries[z.current].csize = u64(contents.len)
	} else {
		compressed := deflate.compress_raw(contents) or {
			return error('szip: compress failed: ${err.msg()}')
		}
		// only use deflate if smaller
		if compressed.len < contents.len {
			z.entries[z.current].method = 8
			z.entries[z.current].csize = u64(compressed.len)
		} else {
			z.entries[z.current].method = 0
			z.entries[z.current].csize = u64(contents.len)
		}
	}
}

// ---------------------------------------------------------------------------
// Flush (write to file)
// ---------------------------------------------------------------------------

fn (mut z Zip) flush() ! {
	if z.entries.len >= 0xffff {
		return error('szip: ZIP64 archives are unsupported')
	}
	mut payloads := [][]u8{cap: z.entries.len}
	for i, e in z.entries {
		payload := if e.method == 8 { deflate.compress_raw(e.data)! } else { e.data.clone() }
		if e.name.len == 0 || e.name.len > 0xffff || payload.len >= i64(0xffffffff) || e.data.len >= i64(0xffffffff) {
			return error('szip: entry exceeds ZIP32 limits')
		}
		z.entries[i].size = u64(e.data.len)
		z.entries[i].csize = u64(payload.len)
		z.entries[i].crc32 = crc32.sum(e.data)
		payloads << payload
	}
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
		// Re-encoded payload lengths above also cover entries retained by append mode.
		out << payloads[i]
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

fn read_u16(data []u8, pos int) u16 {
	return u16(data[pos]) | (u16(data[pos + 1]) << 8)
}

fn read_u32(data []u8, pos int) u32 {
	return u32(data[pos]) | (u32(data[pos + 1]) << 8) | (u32(data[pos + 2]) << 16) | (u32(data[pos + 3]) << 24)
}

fn u16_to_bytes(v u16) []u8 {
	return [u8(v & 0xFF), u8((v >> 8) & 0xFF)]
}

fn u32_to_bytes(v u32) []u8 {
	return [u8(v & 0xFF), u8((v >> 8) & 0xFF), u8((v >> 16) & 0xFF), u8((v >> 24) & 0xFF)]
}

// ---------------------------------------------------------------------------
// High-level helpers
// ---------------------------------------------------------------------------

// zip_files writes the supplied files to a ZIP archive and reports write failures.
pub fn zip_files(path_to_file []string, path_to_export_zip string) ! {
	mut zip := open(path_to_export_zip, .no_compression, .write) or { return error('szip: ${err.msg()}') }
	for file in path_to_file {
		zip.open_entry(os.base(file)) or { return error('szip: ${err.msg()}') }
		file_as_byte := os.read_bytes(file) or { return error('szip: ${err.msg()}') }
		zip.write_entry(file_as_byte) or { return error('szip: ${err.msg()}') }
		zip.close_entry()
	}
	zip.flush()!
}

// zip_folder archives a directory, optionally omitting empty folders.
pub fn zip_folder(folder string, zip_file string, opt ZipFolderOptions) ! {
	path := folder.trim_right(os.path_separator)
	if !os.is_dir(path) {
		return error('szip: source is not a directory')
	}
	mut files := []string{}
	mut pending := [path]
	for pending.len > 0 {
		current := pending.pop()
		for name in os.ls(current)! {
			entry := os.join_path(current, name)
			files << entry
			if os.is_dir(entry) && !os.is_link(entry) {
				pending << entry
			}
		}
	}
	mut zip := open(zip_file, .no_compression, .write)!
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
	zip.flush()!
}

// extract_zip_to_dir extracts entries beneath dir and reports invalid paths or I/O failures.
pub fn extract_zip_to_dir(file string, dir string) !bool {
	if !os.is_dir(dir) {
		return error('szip: cannot open directory for extracting, directory not exists')
	}
	root := os.real_path(dir)
	mut z := open(file, .no_compression, .read_only)!
	defer { z.close() }
	for i in 0 .. z.entries.len {
		z.open_entry_by_index(i)!
		target := extraction_path(root, z.name())!
		if z.is_dir()! {
			os.mkdir_all(target)!
		} else {
			os.mkdir_all(os.dir(target))!
			z.extract_entry(target)!
		}
		z.close_entry()
	}
	return true
}

fn extraction_path(root string, name string) !string {
	// ZIP names use '/', but also reject Windows separators and drive-qualified names.
	normalized := name.replace('\\', '/')
	if normalized.starts_with('/') || normalized.contains(':') || normalized.contains_u8(0) {
		return error('szip: invalid extraction path')
	}
	mut target := root
	for part in normalized.trim_right('/').split('/') {
		if part in ['', '.', '..'] {
			return error('szip: invalid extraction path')
		}
		target = os.join_path(target, part)
		if os.is_link(target) {
			return error('szip: extraction path contains a symbolic link')
		}
	}
	return target
}
