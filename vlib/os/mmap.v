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
	data    []u8
	mapping voidptr // Windows file-mapping handle; nil elsewhere
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
	write := mode == .read_write
	$if windows {
		addr, mapping := mmap_map_windows(path, size, write, false)!
		return &Mmap{
			data:    unsafe { addr.vbytes(int(size)) }
			mapping: mapping
		}
	} $else {
		addr := mmap_map_nix(path, size, write, false)!
		return &Mmap{
			data: unsafe { addr.vbytes(int(size)) }
		}
	}
}

// mmap_create creates (or truncates) path to len bytes and maps it read-write,
// zero-filled. len must be positive.
pub fn mmap_create(path string, len u64) !&Mmap {
	if len == 0 {
		return error('os.mmap_create: length must be positive')
	}
	$if windows {
		addr, mapping := mmap_map_windows(path, len, true, true)!
		return &Mmap{
			data:    unsafe { addr.vbytes(int(len)) }
			mapping: mapping
		}
	} $else {
		addr := mmap_map_nix(path, len, true, true)!
		return &Mmap{
			data: unsafe { addr.vbytes(int(len)) }
		}
	}
}

// bytes returns the mapped region as a byte view.
pub fn (m &Mmap) bytes() []u8 {
	return m.data
}

// len returns the mapped length in bytes.
pub fn (m &Mmap) len() u64 {
	return u64(m.data.len)
}

// sync flushes writes through a read_write mapping to the file.
pub fn (mut m Mmap) sync() ! {
	mmap_sync_platform(voidptr(m.data.data), u64(m.data.len))!
}

// close unmaps the region and releases all handles. The bytes() view must
// not be used afterwards. Closing twice is a no-op.
pub fn (mut m Mmap) close() {
	if m.data.len == 0 {
		return
	}
	mmap_unmap_platform(voidptr(m.data.data), u64(m.data.len), m.mapping)
	m.data = []
	m.mapping = nil
}
