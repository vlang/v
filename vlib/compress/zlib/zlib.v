module zlib

#flag -I @VEXEROOT/thirdparty/zip
#include "miniz.h"

pub const max_size = u64(1 << 31)

fn C.tdefl_compress_mem_to_heap(source_buf voidptr, source_buf_len usize, out_len &usize, flags int) voidptr
fn C.tinfl_decompress_mem_to_heap(source_buf voidptr, source_buf_len usize, out_len &usize, flags int) voidptr

// compresses an array of bytes using zlib and returns the compressed bytes in a new array
// Example: compressed := zlib.compress(b) ?
[manualfree]
pub fn compress(data []u8) ?[]u8 {
	if u64(data.len) > zlib.max_size {
		return error('data too large ($data.len > $zlib.max_size)')
	}
	mut out_len := usize(0)

	// flags = TDEFL_WRITE_ZLIB_HEADER (0x01000)
	address := C.tdefl_compress_mem_to_heap(data.data, data.len, &out_len, 0x01000)
	if address == 0 {
		return error('compression failed')
	}
	if u64(out_len) > zlib.max_size {
		return error('compressed data is too large ($out_len > $zlib.max_size)')
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
// Example: decompressed := zlib.decompress(b) ?
[manualfree]
pub fn decompress(data []u8) ?[]u8 {
	mut out_len := usize(0)

	// flags = TINFL_FLAG_PARSE_ZLIB_HEADER (0x1)
	address := C.tinfl_decompress_mem_to_heap(data.data, data.len, &out_len, 0x1)
	if address == 0 {
		return error('decompression failed')
	}
	if u64(out_len) > zlib.max_size {
		return error('decompressed data is too large ($out_len > $zlib.max_size)')
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
