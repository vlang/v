module main

import arena

fn main() {
	// Request a real backing mapping larger than the Linux userspace address
	// space, without overflowing either the chunk header or dlmalloc metadata.
	storage := arena.new(chunk_size: max_int / 4)
	storage.push()
	_ = unsafe { malloc(1) }
	println('unexpected arena allocation success')
}
