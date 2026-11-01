module szip

// Pure V implementation of zip (replaces the former C/miniz backend).
import os
import rand
import time
import hash.crc32
import compress.deflate

const crc_table = crc32.new(crc32.ieee)

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
	data   []u8 // uncompressed bytes of an entry being written
	// modified is the modification time: the extended timestamp when the
	// archive has one, else the MS-DOS time, which has a two second resolution.
	modified time.Time
	// mode holds the Unix file type and permission bits, or 0 when the archive
	// was not made on Unix.
	mode u32
mut:
	raw_name   []u8 // the name as stored, written back unchanged in append mode
	keep_extra []u8 // extra field blocks of an existing entry that are written back
	sealed     bool // written to the output, or retained from the existing archive
	retained   bool // read from the existing archive, at `offset` in it
	out_flags  u16  // general purpose flags in the archive being written
	out_offset u64  // offset of the local header in the archive being written
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
mut:
	directory_offset u64
	limits           Limits
	// The archive being read is accessed by offset, so opening it only reads
	// its central directory and an entry is decompressed when it is read.
	file       os.File
	file_open  bool
	file_size  u64
	total_read u64
	// An archive being written is streamed to a temporary file next to it,
	// which replaces the archive when it is closed.
	out      os.File
	out_open bool
	out_dir  string
	out_path string
	out_pos  u64
	finished bool
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

// open opens an archive for reading, writing or appending, applying the default
// `Limits` to what it reads.
pub fn open(name string, level CompressionLevel, mode OpenMode) !&Zip {
	return open_with_limits(name, level, mode, Limits{})
}

// open_with_limits opens an archive like `open`, applying `limits` to what it
// reads. An archive of unknown origin should be read with limits that match what
// the program is prepared to handle.
pub fn open_with_limits(name string, level CompressionLevel, mode OpenMode, limits Limits) !&Zip {
	if name == '' {
		return error('szip: name of file empty')
	}
	mut z := &Zip{
		path:   name
		mode:   mode
		level:  level
		limits: limits
	}
	if mode == .read_only || (mode == .append && os.exists(name)) {
		if !os.exists(name) {
			return error('szip: cannot open archive')
		}
		z.open_file() or { return error('szip: cannot read archive') }
		z.parse() or {
			z.close_file()
			return parse_error(err)
		}
	}
	if mode != .read_only {
		z.open_output() or {
			z.close_file()
			return error('szip: cannot create archive: ${err.msg()}')
		}
		// The existing entries are validated and copied to the new archive now;
		// the original stays readable, and in place, until the archive is closed.
		z.retain_existing() or {
			z.discard_output()
			z.close_file()
			return parse_error(err)
		}
	}
	return z
}

fn parse_error(err IError) IError {
	if err is LimitExceeded {
		return err
	}
	return error('szip: cannot parse archive: ${err.msg()}')
}

fn (mut z Zip) open_file() ! {
	z.file = os.open_file(z.path, 'rb')!
	z.file_open = true
	z.file_size = os.file_size(z.path)
}

fn (mut z Zip) close_file() {
	if z.file_open {
		z.file.close()
		z.file_open = false
	}
}

// open_output creates the file the archive is written to. It lives in a private
// directory created next to the archive: creating a directory fails when
// anything, a symbolic link included, already has its name, and nobody else can
// create files in it, so the file cannot be redirected elsewhere.
fn (mut z Zip) open_output() ! {
	z.out_dir = os.join_path(os.dir(z.path), '.${os.file_name(z.path)}.${rand.ulid()}.tmp')
	os.mkdir(z.out_dir, mode: 0o700)!
	z.out_path = os.join_path(z.out_dir, 'archive.zip')
	z.out = os.open_file(z.out_path, 'wb') or {
		os.rmdir(z.out_dir) or {}
		return err
	}
	z.out_open = true
	z.out_pos = 0
}

fn (mut z Zip) discard_output() {
	if z.out_open {
		z.out.close()
		z.out_open = false
	}
	if z.out_path != '' {
		os.rm(z.out_path) or {}
		os.rmdir(z.out_dir) or {}
		z.out_path = ''
	}
	z.finished = true
}

fn (mut z Zip) write_out(data []u8) ! {
	if data.len == 0 {
		return
	}
	n := z.out.write(data)!
	if n != data.len {
		return error('short write to the archive')
	}
	z.out_pos += u64(data.len)
}

// read_at reads exactly `n` bytes at `pos`. `os.File.read_from` may return fewer
// bytes than asked for, so it is called until the buffer is full.
fn (mut z Zip) read_at(pos u64, n int) ![]u8 {
	if n < 0 || pos > z.file_size || u64(n) > z.file_size - pos {
		return error('read past the end of the archive')
	}
	mut buf := []u8{len: n}
	mut done := 0
	for done < n {
		mut chunk := unsafe { buf[done..] }
		got := z.file.read_from(pos + u64(done), mut chunk) or {
			return error('cannot read the archive: ${err.msg()}')
		}
		if got <= 0 {
			return error('unexpected end of the archive')
		}
		done += got
	}
	return buf
}

// DirectoryReader walks the central directory through a chunk of it held in memory.
struct DirectoryReader {
mut:
	buf       []u8
	buf_start u64
	pos       u64
	end       u64
}

const directory_chunk_size = 1 << 20

// directory_bytes returns the next `n` bytes of the central directory, reading
// another chunk of at most 1 MiB, or of the record when it is longer, when needed.
fn (mut z Zip) directory_bytes(mut cd DirectoryReader, n int) ![]u8 {
	if u64(n) > cd.end - cd.pos {
		return error('central directory record runs past its end')
	}
	if cd.pos + u64(n) > cd.buf_start + u64(cd.buf.len) {
		remaining := cd.end - cd.pos
		mut want := u64(if n > directory_chunk_size { n } else { directory_chunk_size })
		if want > remaining {
			want = remaining
		}
		cd.buf = z.read_at(cd.pos, int(want))!
		cd.buf_start = cd.pos
	}
	start := int(cd.pos - cd.buf_start)
	cd.pos += u64(n)
	return cd.buf[start..start + n]
}

// close flushes pending entries; use zip_files or zip_folder when write errors must be returned.
pub fn (mut z Zip) close() {
	if z.mode == .write || z.mode == .append {
		z.flush() or {}
	}
	z.close_file()
}

// ---------------------------------------------------------------------------
// Parse (read mode)
// ---------------------------------------------------------------------------

fn (mut z Zip) parse() ! {
	// The end record sits in the last 22 bytes plus at most a 64 KiB comment.
	tail_len := if z.file_size < 65557 { int(z.file_size) } else { 65557 }
	tail_start := z.file_size - u64(tail_len)
	tail := z.read_at(tail_start, tail_len)!
	mut eocd_pos := -1
	// The end record may be followed only by its declared archive comment.
	for i := tail.len - 22; i >= 0; i-- {
		if read_u32(tail, i) == sig_eocd {
			comment_len := int(read_u16(tail, i + 20))
			if i + 22 + comment_len == tail.len {
				eocd_pos = i
				break
			}
		}
	}
	if eocd_pos < 0 {
		return error('missing end record')
	}
	if read_u16(tail, eocd_pos + 4) != 0 || read_u16(tail, eocd_pos + 6) != 0 {
		return error('multi-disk archives are unsupported')
	}
	z.limits.check_header_sizes(0, 0, int(read_u16(tail, eocd_pos + 20)))!
	mut n_entries := u64(read_u16(tail, eocd_pos + 10))
	mut n_disk_entries := u64(read_u16(tail, eocd_pos + 8))
	mut cd_size := u64(read_u32(tail, eocd_pos + 12))
	mut cd_offset := u64(read_u32(tail, eocd_pos + 16))
	// The central directory must end where the next record starts: the end
	// record, or the ZIP64 end record when there is one.
	mut cd_limit := tail_start + u64(eocd_pos)
	if cd_limit >= u64(zip64_locator_size) {
		locator := z.read_at(cd_limit - u64(zip64_locator_size), zip64_locator_size)!
		if read_u32(locator, 0) == sig_zip64_locator {
			if read_u32(locator, 4) != 0 || read_u32(locator, 16) > 1 {
				return error('multi-disk archives are unsupported')
			}
			record_offset := read_u64(locator, 8)
			if record_offset > cd_limit - u64(zip64_locator_size)
				|| cd_limit - u64(zip64_locator_size) - record_offset < u64(zip64_eocd_size) {
				return error('ZIP64 locator points outside the archive')
			}
			record := z.read_at(record_offset, zip64_eocd_size)!
			if read_u32(record, 0) != sig_zip64_eocd {
				return error('missing ZIP64 end record')
			}
			// The record, extensible data included, must end where the locator starts.
			record_size := read_u64(record, 4)
			if record_size < u64(zip64_eocd_size - 12)
				|| record_size != cd_limit - u64(zip64_locator_size) - record_offset - 12 {
				return error('invalid ZIP64 end record size')
			}
			if read_u32(record, 16) != 0 || read_u32(record, 20) != 0 {
				return error('multi-disk archives are unsupported')
			}
			n_disk_entries = read_u64(record, 24)
			n_entries = read_u64(record, 32)
			cd_size = read_u64(record, 40)
			cd_offset = read_u64(record, 48)
			cd_limit = record_offset
		}
	}
	z.limits.check_entry_count(n_entries)!
	if n_disk_entries != n_entries || cd_offset > cd_limit || cd_size > cd_limit - cd_offset {
		return error('invalid central directory bounds')
	}
	if cd_size < u64(46) * n_entries {
		return error('invalid central directory bounds')
	}
	z.directory_offset = cd_offset
	// The directory is read in chunks, record by record, so what is allocated
	// follows the records actually parsed and the limits checked on each,
	// rather than the size the end record claims.
	mut cd := DirectoryReader{
		pos: cd_offset
		end: cd_offset + cd_size
	}
	for _ in 0 .. n_entries {
		data := z.directory_bytes(mut cd, 46) or { return error('invalid central directory entry') }
		if read_u32(data, 0) != sig_central_header {
			return error('invalid central directory entry')
		}
		made_by := read_u16(data, 4)
		flags := read_u16(data, 8)
		if flags & 1 != 0 {
			return error('encrypted entries are unsupported')
		}
		method := read_u16(data, 10)
		if method !in [u16(0), 8] {
			return error('unsupported compression method')
		}
		name_len := int(read_u16(data, 28))
		extra_len := int(read_u16(data, 30))
		comment_len := int(read_u16(data, 32))
		z.limits.check_header_sizes(name_len, extra_len, comment_len)!
		if name_len == 0 {
			return error('invalid central directory metadata')
		}
		disk := read_u16(data, 34)
		if disk != 0 && disk != sentinel_u16 {
			return error('multi-disk archives are unsupported')
		}
		rest := z.directory_bytes(mut cd, name_len + extra_len + comment_len) or {
			return error('invalid central directory metadata')
		}
		raw_name := rest[..name_len].clone()
		extra := rest[name_len..name_len + extra_len]
		mut size := u64(read_u32(data, 24))
		mut csize := u64(read_u32(data, 20))
		mut offset := u64(read_u32(data, 42))
		z64 := parse_zip64_extra(extra, size == u64(sentinel_u32), csize == u64(sentinel_u32),
			offset == u64(sentinel_u32), disk == sentinel_u16)!
		if disk == sentinel_u16 && z64.disk != 0 {
			return error('multi-disk archives are unsupported')
		}
		if size == u64(sentinel_u32) {
			size = z64.size
		}
		if csize == u64(sentinel_u32) {
			csize = z64.csize
		}
		if offset == u64(sentinel_u32) {
			offset = z64.offset
		}
		name := unicode_path(extra, raw_name) or { decode_name(raw_name, flags) }
		// A Unicode Path block is written back with the raw name it describes, so
		// the entry keeps its name when the archive is rewritten.
		mut keep_extra := []u8{}
		if block := find_extra_block(extra, extra_unicode_path) {
			put_u16(mut keep_extra, extra_unicode_path)
			put_u16(mut keep_extra, u16(block.len))
			keep_extra << block
		}
		external := read_u32(data, 38)
		mode := if made_by >> 8 == host_unix { external >> 16 } else { u32(0) }
		modified := extended_mtime(extra) or {
			dos_to_time(read_u16(data, 14), read_u16(data, 12))
		}
		z.entries << ZipEntry{
			name:       name
			raw_name:   raw_name
			keep_extra: keep_extra
			method:     method
			flags:      flags
			crc32:      read_u32(data, 16)
			size:       size
			csize:      csize
			offset:     offset
			is_dir:     name.ends_with('/')
			modified:   modified
			mode:       mode
			sealed:     true
			retained:   true
		}
	}
	if cd.pos != cd.end {
		return error('central directory size mismatch')
	}
}

// retain_existing validates every entry of an archive opened for appending and
// copies its compressed bytes, unchanged, to the archive being written.
fn (mut z Zip) retain_existing() ! {
	for i in 0 .. z.entries.len {
		e := z.entries[i]
		z.limits.check_entry(e.name, e.csize, e.size, z.total_read)!
		raw := z.read_raw_entry(i)!
		z.decode_entry(i, raw)!
		z.total_read += e.size
		// The rewritten local header carries the sizes and checksum, so a data
		// descriptor is no longer needed; the name encoding flag is kept. The
		// source flags stay as they are, so the entry remains readable.
		z.entries[i].out_flags = e.flags & 0x0800
		z.write_local(i, raw)!
	}
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
		// Opening an entry closes the previous one, so only the last entry can
		// still be open.
		if z.entries.len > 0 {
			z.seal(z.entries.len - 1)!
		}
		is_dir := normalized.ends_with('/')
		z.entries << ZipEntry{
			name:      normalized
			raw_name:  normalized.bytes()
			method:    0
			flags:     0x0800
			out_flags: 0x0800
			is_dir:    is_dir
			modified:  time.now()
			mode:      if is_dir { unix_type_dir | 0o755 } else { unix_type_file | 0o644 }
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

// close_entry finishes the current entry; a written entry is compressed now.
pub fn (mut z Zip) close_entry() {
	if z.current >= 0 && z.current < z.entries.len && z.mode != .read_only {
		z.seal(z.current) or {}
	}
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

// modified returns the modification time of the current entry.
pub fn (mut z Zip) modified() time.Time {
	if z.current < 0 || z.current >= z.entries.len {
		return time.Time{}
	}
	return z.entries[z.current].modified
}

// unix_mode returns the Unix file type and permission bits of the current
// entry, or 0 when the archive did not record them.
pub fn (mut z Zip) unix_mode() u32 {
	if z.current < 0 || z.current >= z.entries.len {
		return 0
	}
	return z.entries[z.current].mode
}

pub fn (mut z Zip) index() !int {
	if z.current < 0 || z.current >= z.entries.len {
		return error('szip: cannot get current index of zip entry')
	}
	return z.current
}

// read_current_data reads and decompresses the current entry.
fn (mut z Zip) read_current_data() ![]u8 {
	if z.current < 0 || z.current >= z.entries.len {
		return error('szip: no current entry')
	}
	e := z.entries[z.current]
	if !e.sealed {
		return e.data.clone()
	}
	if !e.retained || !z.file_open {
		return error('szip: entry has already been written to the archive')
	}
	z.limits.check_entry(e.name, e.csize, e.size, z.total_read)!
	raw := z.read_raw_entry(z.current)!
	decoded := z.decode_entry(z.current, raw)!
	z.total_read += u64(decoded.len)
	return decoded
}

// read_raw_entry checks the local header of entry `i` against the central
// directory and returns its compressed bytes.
fn (mut z Zip) read_raw_entry(i int) ![]u8 {
	e := z.entries[i]
	// The compressed bytes are read into memory before they are decompressed,
	// so their size is checked first: against `max_compressed_size`, and for a
	// stored entry against its uncompressed size, which it must equal.
	z.limits.check_entry(e.name, e.csize, 0, 0)!
	if e.method == 0 && e.csize != e.size {
		return error('szip: stored entry sizes differ')
	}
	if e.offset > z.directory_offset || z.directory_offset - e.offset < 30 {
		return error('szip: bad local header offset')
	}
	header := z.read_at(e.offset, 30)!
	if read_u32(header, 0) != sig_local_header {
		return error('szip: invalid local header')
	}
	flags := read_u16(header, 6)
	if flags != e.flags || flags & 1 != 0 || read_u16(header, 8) != e.method {
		return error('szip: inconsistent local header')
	}
	name_len := int(read_u16(header, 26))
	extra_len := int(read_u16(header, 28))
	z.limits.check_header_sizes(name_len, extra_len, 0)!
	data_start := e.offset + 30 + u64(name_len) + u64(extra_len)
	if data_start > z.directory_offset || e.csize > z.directory_offset - data_start {
		return error('szip: truncated entry data')
	}
	if e.csize > u64(max_int) {
		return error('szip: entry too large')
	}
	name_extra := z.read_at(e.offset + 30, name_len + extra_len)!
	if name_extra[..name_len] != e.raw_name {
		return error('szip: inconsistent entry name')
	}
	local_extra := name_extra[name_len..]
	mut local_size := u64(read_u32(header, 22))
	mut local_csize := u64(read_u32(header, 18))
	zip64_sizes := local_size == u64(sentinel_u32) || local_csize == u64(sentinel_u32)
	if zip64_sizes {
		z64 := parse_zip64_extra(local_extra, local_size == u64(sentinel_u32), local_csize == u64(sentinel_u32),
			false, false)!
		if local_size == u64(sentinel_u32) {
			local_size = z64.size
		}
		if local_csize == u64(sentinel_u32) {
			local_csize = z64.csize
		}
	}
	// With a data descriptor, the central directory carries the final sizes and checksum.
	if flags & 8 == 0 && (read_u32(header, 14) != e.crc32 || local_csize != e.csize
		|| local_size != e.size) {
		return error('szip: inconsistent entry metadata')
	}
	data_end := data_start + e.csize
	if flags & 8 != 0 {
		// A ZIP64 entry has 8-byte sizes in its data descriptor.
		desc_len := if zip64_sizes { 20 } else { 12 }
		mut descriptor := data_end
		if descriptor + 4 <= z.directory_offset
			&& read_u32(z.read_at(descriptor, 4)!, 0) == sig_data_descriptor {
			descriptor += 4
		}
		if descriptor + u64(desc_len) > z.directory_offset {
			return error('szip: invalid data descriptor')
		}
		d := z.read_at(descriptor, desc_len)!
		d_csize := if zip64_sizes { read_u64(d, 4) } else { u64(read_u32(d, 4)) }
		d_size := if zip64_sizes { read_u64(d, 12) } else { u64(read_u32(d, 8)) }
		if read_u32(d, 0) != e.crc32 || d_csize != e.csize || d_size != e.size {
			return error('szip: invalid data descriptor')
		}
	}
	return z.read_at(data_start, int(e.csize))!
}

// decode_entry decompresses the compressed bytes of entry `i` and checks them
// against its declared size and checksum. Decompression stops as soon as the
// output passes the declared size, so a lying header cannot make it allocate more.
fn (mut z Zip) decode_entry(i int, compressed []u8) ![]u8 {
	e := z.entries[i]
	z.limits.check_entry(e.name, e.csize, e.size, 0)!
	if e.size > u64(max_int) {
		return error('szip: entry too large')
	}
	mut decoded := []u8{}
	if e.method == 0 {
		decoded = unsafe { compressed }
	} else if e.method == 8 {
		res := deflate.decompress_raw_bounded(compressed, int(e.size)) or {
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
	data := z.read_current_data() or {
		if err is LimitExceeded {
			return err
		}
		return error('szip: ${err.msg()}')
	}
	if in_bsize < data.len || (data.len > 0 && buf == unsafe { nil }) {
		return error('szip: cannot read properly data from entry')
	}
	n := data.len
	if n > 0 {
		unsafe { vmemcpy(buf, data.data, n) }
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

// create_entry writes the file at name to the current archive entry, keeping
// its modification time and permissions.
pub fn (mut z Zip) create_entry(name string) ! {
	data := os.read_bytes(name) or { return error('szip: cannot read file: ${err.msg()}') }
	z.write_entry(data)!
	z.take_file_metadata(name)
}

// take_file_metadata gives the current written entry the modification time and
// permissions of the file at `path`.
fn (mut z Zip) take_file_metadata(path string) {
	if z.current < 0 || z.current >= z.entries.len {
		return
	}
	st := os.stat(path) or { return }
	z.entries[z.current].modified = time.unix(st.mtime)
	kind := if z.entries[z.current].is_dir { unix_type_dir } else { unix_type_file }
	z.entries[z.current].mode = kind | (st.mode & 0o777)
}

// extract_entry writes the current archive entry to path.
pub fn (mut z Zip) extract_entry(path string) ! {
	data := z.read_current_data()!
	os.write_file_array(path, data) or { return error('szip: cannot extract entry: ${err.msg()}') }
}

// write_entry appends bytes to the currently open writable entry. The entry is
// compressed once, when it is closed.
pub fn (mut z Zip) write_entry(data []u8) ! {
	if z.mode == .read_only {
		return error('szip: archive is read-only')
	}
	if z.current < 0 || z.current >= z.entries.len {
		return error('szip: no current entry')
	}
	if z.entries[z.current].sealed {
		return error('szip: entry is already closed')
	}
	z.entries[z.current].data << data
	// size() and crc32() report what has been written so far.
	z.entries[z.current].size += u64(data.len)
	z.entries[z.current].crc32 = crc_table.update(z.entries[z.current].crc32, data)
}

// seal compresses a written entry at the archive's level, keeping it stored
// when deflate does not make it smaller, and writes it to the archive.
fn (mut z Zip) seal(i int) ! {
	if z.entries[i].sealed {
		return
	}
	contents := z.entries[i].data
	level := z.deflate_level()
	mut method := u16(0)
	// The uncompressed bytes are released below, so the payload takes over
	// whichever buffer is kept instead of copying it.
	mut payload := unsafe { contents }
	if level > 0 && contents.len > 0 {
		compressed := deflate.compress(contents, format: .raw_deflate, level: level) or {
			return error('szip: compress failed: ${err.msg()}')
		}
		if compressed.len < contents.len {
			method = 8
			payload = unsafe { compressed }
		}
	}
	z.entries[i].method = method
	z.entries[i].csize = u64(payload.len)
	z.write_local(i, payload)!
	z.entries[i].data = []u8{}
	z.entries[i].sealed = true
}

// write_local writes the local header and the compressed bytes of entry `i`
// to the archive being written. The sizes are known, so no data descriptor is
// used; sizes that do not fit 32 bits go in a ZIP64 extra field.
fn (mut z Zip) write_local(i int, payload []u8) ! {
	e := z.entries[i]
	if e.raw_name.len == 0 || e.raw_name.len > 0xffff {
		return error('szip: invalid entry name length')
	}
	zip64 := e.size >= u64(sentinel_u32) || e.csize >= u64(sentinel_u32)
	mut extra := extended_timestamp_block(e.modified)
	if zip64 {
		put_u16(mut extra, extra_zip64)
		put_u16(mut extra, 16)
		put_u64(mut extra, e.size)
		put_u64(mut extra, e.csize)
	}
	extra << e.keep_extra
	if extra.len > 0xffff {
		return error('szip: extra field too long')
	}
	dos_date, dos_time := time_to_dos(e.modified.local())
	mut h := []u8{cap: 30 + e.raw_name.len + extra.len}
	put_u32(mut h, sig_local_header)
	put_u16(mut h, if zip64 { version_zip64 } else { version_default })
	put_u16(mut h, e.out_flags)
	put_u16(mut h, e.method)
	put_u16(mut h, dos_time)
	put_u16(mut h, dos_date)
	put_u32(mut h, e.crc32)
	put_u32(mut h, if zip64 { sentinel_u32 } else { u32(e.csize) })
	put_u32(mut h, if zip64 { sentinel_u32 } else { u32(e.size) })
	put_u16(mut h, u16(e.raw_name.len))
	put_u16(mut h, u16(extra.len))
	h << e.raw_name
	h << extra
	z.entries[i].out_offset = z.out_pos
	if !e.retained {
		z.entries[i].offset = z.out_pos
	}
	z.write_out(h) or { return error('szip: write failed: ${err.msg()}') }
	z.write_out(payload) or { return error('szip: write failed: ${err.msg()}') }
}

// ---------------------------------------------------------------------------
// Flush (write to file)
// ---------------------------------------------------------------------------

fn (mut z Zip) flush() ! {
	if z.finished {
		return
	}
	z.finished = true
	z.write_directory() or {
		z.discard_output()
		return err
	}
	z.out.flush()
	z.out.close()
	z.out_open = false
	z.close_file()
	// A write error can stay buffered until the file is closed, which does not
	// report it; a file shorter than what was written must not replace the archive.
	if os.file_size(z.out_path) != z.out_pos {
		z.discard_output()
		return error('szip: write failed: the archive was not completely written')
	}
	z.replace_archive() or {
		z.discard_output()
		return error('szip: write failed: ${err.msg()}')
	}
	os.rmdir(z.out_dir) or {}
}

// replace_archive moves the written file over the archive. The new archive keeps
// the permissions of the one it replaces.
fn (mut z Zip) replace_archive() ! {
	$if windows {
		// Windows does not rename over an existing file: the previous archive is
		// moved aside first, and moved back if the new one cannot take its place.
		if os.exists(z.path) {
			previous := os.join_path(z.out_dir, 'previous.zip')
			os.rename(z.path, previous)!
			os.rename(z.out_path, z.path) or {
				os.rename(previous, z.path) or {}
				return err
			}
			os.rm(previous) or {}
			return
		}
	} $else {
		if st := os.stat(z.path) {
			os.chmod(z.out_path, int(st.mode & 0o7777)) or {}
		}
	}
	os.rename(z.out_path, z.path)!
}

// write_directory seals the open entries, then writes the central directory
// and the end records, with ZIP64 records when a count, size or offset does
// not fit the 32-bit format.
fn (mut z Zip) write_directory() ! {
	for i in 0 .. z.entries.len {
		z.seal(i)!
	}
	cd_start := z.out_pos
	for e in z.entries {
		need_size := e.size >= u64(sentinel_u32)
		need_csize := e.csize >= u64(sentinel_u32)
		need_offset := e.out_offset >= u64(sentinel_u32)
		zip64 := need_size || need_csize || need_offset
		mut extra := extended_timestamp_block(e.modified)
		if zip64 {
			mut z64 := []u8{}
			if need_size {
				put_u64(mut z64, e.size)
			}
			if need_csize {
				put_u64(mut z64, e.csize)
			}
			if need_offset {
				put_u64(mut z64, e.out_offset)
			}
			put_u16(mut extra, extra_zip64)
			put_u16(mut extra, u16(z64.len))
			extra << z64
		}
		extra << e.keep_extra
		version := if zip64 { version_zip64 } else { version_default }
		mut external := e.mode << 16
		if e.is_dir {
			external |= dos_attr_directory
		}
		dos_date, dos_time := time_to_dos(e.modified.local())
		mut h := []u8{cap: 46 + e.raw_name.len + extra.len}
		put_u32(mut h, sig_central_header)
		put_u16(mut h, host_unix << 8 | version)
		put_u16(mut h, version)
		put_u16(mut h, e.out_flags)
		put_u16(mut h, e.method)
		put_u16(mut h, dos_time)
		put_u16(mut h, dos_date)
		put_u32(mut h, e.crc32)
		put_u32(mut h, saturate(e.csize))
		put_u32(mut h, saturate(e.size))
		put_u16(mut h, u16(e.raw_name.len))
		put_u16(mut h, u16(extra.len))
		put_u16(mut h, 0) // comment length
		put_u16(mut h, 0) // disk number
		put_u16(mut h, 0) // internal attributes
		put_u32(mut h, external)
		put_u32(mut h, saturate(e.out_offset))
		h << e.raw_name
		h << extra
		z.write_out(h) or { return error('szip: write failed: ${err.msg()}') }
	}
	cd_size := z.out_pos - cd_start
	count := u64(z.entries.len)
	mut end := []u8{}
	if count >= u64(sentinel_u16) || cd_size >= u64(sentinel_u32) || cd_start >= u64(sentinel_u32) {
		record_offset := z.out_pos
		put_u32(mut end, sig_zip64_eocd)
		put_u64(mut end, u64(zip64_eocd_size - 12))
		put_u16(mut end, host_unix << 8 | version_zip64)
		put_u16(mut end, version_zip64)
		put_u32(mut end, 0) // this disk
		put_u32(mut end, 0) // disk with the central directory
		put_u64(mut end, count)
		put_u64(mut end, count)
		put_u64(mut end, cd_size)
		put_u64(mut end, cd_start)
		put_u32(mut end, sig_zip64_locator)
		put_u32(mut end, 0) // disk with the ZIP64 end record
		put_u64(mut end, record_offset)
		put_u32(mut end, 1) // total disks
	}
	put_u32(mut end, sig_eocd)
	put_u16(mut end, 0) // disk number
	put_u16(mut end, 0) // disk with the central directory
	short_count := if count >= u64(sentinel_u16) { sentinel_u16 } else { u16(count) }
	put_u16(mut end, short_count)
	put_u16(mut end, short_count)
	put_u32(mut end, saturate(cd_size))
	put_u32(mut end, saturate(cd_start))
	put_u16(mut end, 0) // comment length
	z.write_out(end) or { return error('szip: write failed: ${err.msg()}') }
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

fn read_u64(data []u8, pos int) u64 {
	return u64(read_u32(data, pos)) | (u64(read_u32(data, pos + 4)) << 32)
}

// ---------------------------------------------------------------------------
// High-level helpers
// ---------------------------------------------------------------------------

// zip_files writes the supplied files to a ZIP archive, keeping their
// modification times and permissions, and reports write failures.
pub fn zip_files(path_to_file []string, path_to_export_zip string) ! {
	mut zip := open(path_to_export_zip, .no_compression, .write) or { return error('szip: ${err.msg()}') }
	for file in path_to_file {
		zip.open_entry(os.base(file)) or {
			zip.discard_output()
			return error('szip: ${err.msg()}')
		}
		zip.create_entry(file) or {
			zip.discard_output()
			return error('szip: ${err.msg()}')
		}
		zip.close_entry()
	}
	zip.flush()!
}

// zip_folder archives a directory, keeping modification times and
// permissions, optionally omitting empty folders.
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
		zip.open_entry(zip_file_entry) or {
			zip.discard_output()
			return err
		}
		if is_dir {
			zip.take_file_metadata(file)
		} else {
			zip.create_entry(file) or {
				zip.discard_output()
				return err
			}
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
	mut dirs := []int{}
	for i in 0 .. z.entries.len {
		z.open_entry_by_index(i)!
		target := extraction_path(root, z.name())!
		if z.is_dir()! {
			os.mkdir_all(target)!
			dirs << i
		} else {
			os.mkdir_all(os.dir(target))!
			z.extract_entry(target)!
			restore_metadata(target, z.entries[i])!
		}
		z.close_entry()
	}
	// Directories get their metadata last, deepest first whatever their order in
	// the archive: creating their contents changes their modification time, and
	// a directory made read-only could not receive them.
	dirs.sort_with_compare(fn [z] (a &int, b &int) int {
		da := z.entries[*a].name.trim_right('/').count('/')
		db := z.entries[*b].name.trim_right('/').count('/')
		return db - da
	})
	for i in dirs {
		restore_metadata(extraction_path(root, z.entries[i].name)!, z.entries[i])!
	}
	return true
}

// restore_metadata applies the permission bits and the modification time of an
// entry to its extracted file or directory. Only the permission bits are kept:
// set-user-ID, set-group-ID and sticky bits from an archive are dropped.
fn restore_metadata(path string, e ZipEntry) ! {
	if e.modified.unix() > 0 {
		os.utime(path, e.modified.unix(), e.modified.unix())!
	}
	$if !windows {
		// Python's zipfile writes the permission bits without a file type, so
		// they apply to whatever was extracted when the type is missing. A mode
		// that is entirely zero says nothing, and is left alone.
		kind := e.mode & unix_type_mask
		if kind == unix_type_file || kind == unix_type_dir || (kind == 0 && e.mode & 0o777 != 0) {
			os.chmod(path, int(e.mode & 0o777))!
		}
	}
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
