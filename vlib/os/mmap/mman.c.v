// This module provides memory mapping related functionalities.
// The memory mapping functions are OS specific. The V-API is
// consistent across the OSes and has been choosen to be Linux-like.
module mmap

import os

// These are the mmap() function arguments
pub struct MmapOptions {
pub:
	addr   voidptr = unsafe { nil } // No preferred virtual address
	len    usize // use file size if fd != 0, else error
	prot   int = prot_read
	flags  int = map_shared
	fd     int
	offset usize
	// beginning of file
}

// vbytes cast the memory mapped region to a byte array
pub fn (minfo MmapInfo) vbytes() []u8 {
	if minfo.fsize == 0 {
		return []
	}
	unsafe {
		return minfo.addr.vbytes(int(minfo.fsize))
	}
}

// bytestr cast the memory mapped region to a string
pub fn (minfo MmapInfo) bytestr() string {
	if minfo.fsize == 0 {
		return ''
	}
	return minfo.vbytes().bytestr()
}

// The struct returned from mmap_file()
pub struct MmapInfo {
pub mut:
	fd os.File
pub:
	fsize usize
	addr  voidptr
	data  []u8
}

// close unmap the memory mapped region and close the underlying file
pub fn (mut minfo MmapInfo) close() ! {
	if minfo.addr != unsafe { nil } && minfo.fsize > 0 {
		munmap(minfo.addr, minfo.fsize)!
	}
	minfo.fd.close()
}

// mmap_file memory map a file's content for read-only access
pub fn mmap_file(file string) !MmapInfo {
	fsize := os.file_size(file)
	if fsize == 0 {
		return error('File is empty')
	}
	mut f := os.open(file)!
	addr := mmap(len: fsize, prot: prot_read, flags: map_shared, fd: f.fd, offset: 0)!
	return MmapInfo{
		fd:    f
		fsize: usize(fsize)
		addr:  addr
		data:  unsafe { addr.vbytes(int(fsize)) }
	}
}
