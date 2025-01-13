import os

struct MemoryMappedFile {
	size u64
mut:
	data &u8
	file os.File
}

fn mmap_file(path string) MemoryMappedFile {
	mut mf := MemoryMappedFile{
		file: os.open_file(path, 'r', 0) or { panic('fail') }
		size: os.file_size(path)
		data: C.NULL
	}

	mf.data = &u8(C.mmap(C.NULL, mf.size, C.PROT_READ, C.MAP_SHARED, mf.file.fd, 0))
	return mf
}

fn (mut mf MemoryMappedFile) unmap() {
	if C.munmap(mf.data, mf.size) != 0 {
		panic('(${C.errno}) munmap() failed')
	}
	mf.file.close()
}
