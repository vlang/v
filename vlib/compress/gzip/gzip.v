module gzip

import compress

// compresses an array of bytes using gzip and returns the compressed bytes in a new array
// Example: compressed := gzip.compress(b)?
[manualfree]
pub fn compress(data []u8) ?[]u8 {
	if u64(data.len) > compress.max_size {
		return error('data too large ($data.len > $compress.max_size)')
	}
	mut out_len := usize(0)

	address := C.tdefl_compress_mem_to_heap(data.data, data.len, &out_len, 0)
	if address == 0 {
		return error('compression failed')
	}
	if u64(out_len) > compress.max_size {
		return error('compressed data is too large ($out_len > $compress.max_size)')
	}
	compressed := unsafe {
		address.vbytes(int(out_len))
	}
	copy := compressed.clone()
	unsafe {
		free(address)
	}
	return copy
}

// decompresses an array of bytes using zlib and returns the decompressed bytes in a new array
// Example: decompressed := zlib.decompress(b)?
[manualfree]
pub fn decompress(data []u8) ?[]u8 {
	mut out_len := usize(0)

	address := C.tinfl_decompress_mem_to_heap(data.data, data.len, &out_len, 0)
	if address == 0 {
		return error('decompression failed')
	}
	if u64(out_len) > compress.max_size {
		return error('decompressed data is too large ($out_len > $compress.max_size)')
	}
	decompressed := unsafe {
		address.vbytes(int(out_len))
	}
	copy := decompressed.clone()
	unsafe {
		free(address)
	}
	return copy
}