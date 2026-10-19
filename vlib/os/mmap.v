module os

// MmapMode selects the protection of a memory mapping.
pub enum MmapMode {
	read_only
	read_write
}

// Mmap is a memory-mapped file region: the file (or its prefix) is visible
// as bytes, backed by the OS page cache. Writes through a read_write mapping
// reach the file after sync (or OS page-out). The bytes() view must not be
// used after close.
@[heap]
pub struct Mmap {
mut:
	data        []u8
	mode        MmapMode
	mapping     voidptr // Windows file-mapping handle; nil elsewhere
	file_handle voidptr // Windows file handle retained for durable sync
}

// mmap_open maps the whole file at path. Missing and empty files are errors
// (use mmap_create for those).
pub fn mmap_open(path string, mode MmapMode) !&Mmap {
	if !exists(path) {
		return error('os.mmap_open: no such file `${path}`')
	}
	size := file_size(path)
	if size == 0 {
		return error('os.mmap_open: cannot map empty file `${path}`')
	}
	mmap_check_length(size)!
	write := mode == .read_write
	$if windows {
		addr, mapping, file_handle := mmap_map_windows(path, size, write, false)!
		return &Mmap{
			data:        unsafe { addr.vbytes(int(size)) }
			mode:        mode
			mapping:     mapping
			file_handle: file_handle
		}
	} $else {
		addr := mmap_map_nix(path, size, write, false)!
		return &Mmap{
			data: unsafe { addr.vbytes(int(size)) }
			mode: mode
		}
	}
}

// mmap_create creates (or truncates) path to len bytes and maps it read-write,
// zero-filled. len must be positive.
pub fn mmap_create(path string, len u64) !&Mmap {
	mmap_check_length(len)!
	$if windows {
		addr, mapping, file_handle := mmap_map_windows(path, len, true, true)!
		return &Mmap{
			data:        unsafe { addr.vbytes(int(len)) }
			mode:        .read_write
			mapping:     mapping
			file_handle: file_handle
		}
	} $else {
		addr := mmap_map_nix(path, len, true, true)!
		return &Mmap{
			data: unsafe { addr.vbytes(int(len)) }
			mode: .read_write
		}
	}
}

// bytes returns the mapped region as a zero-copy byte view.
// The caller must keep the mapping open while using the view, must not resize or
// free the view, and must not mutate a read_only view. The file must not be truncated
// while the mapping is in use.
@[unsafe]
pub fn (m &Mmap) bytes() []u8 {
	return m.data
}

// len returns the mapped length in bytes.
pub fn (m &Mmap) len() u64 {
	return u64(m.data.len)
}

// sync flushes writes through a read_write mapping to the file.
// It returns an error for read-only or closed mappings.
pub fn (mut m Mmap) sync() ! {
	if m.data.len == 0 { return error('os.Mmap.sync: mapping is closed') }
	if m.mode != .read_write { return error('os.Mmap.sync: mapping is read-only') }
	mmap_sync_platform(voidptr(m.data.data), u64(m.data.len), m.file_handle)!
}

// close unmaps the region and releases all handles. The bytes() view must
// not be used afterwards. Closing twice is a no-op.
pub fn (mut m Mmap) close() {
	if m.data.len == 0 {
		return
	}
	mmap_unmap_platform(voidptr(m.data.data), u64(m.data.len), m.mapping, m.file_handle)
	m.data = []
	m.mapping = nil
	m.file_handle = nil
}

// mmap_check_length rejects lengths that cannot be represented as V array lengths.
fn mmap_check_length(len u64) ! {
	if len == 0 { return error('os.mmap_create: length must be positive') }
	if len > u64(max_int) || len > u64(max_i64) || u64(usize(len)) != len {
		return error('os.mmap: length exceeds the supported array size')
	}
}
