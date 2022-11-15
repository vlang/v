module compress

#flag -I @VEXEROOT/thirdparty/zip
#include "miniz.h"

pub const max_size = u64(1 << 31)

fn C.tdefl_compress_mem_to_heap(source_buf voidptr, source_buf_len usize, out_len &usize, flags int) voidptr
fn C.tinfl_decompress_mem_to_heap(source_buf voidptr, source_buf_len usize, out_len &usize, flags int) voidptr

// compresses an array of bytes based on providing flags and returns the compressed bytes in a new array
// NB: this is a low level api, a high level implementation like zlib/gzip should be preferred
[manualfree]
pub fn compress(data []u8, flags int) ![]u8 {
	if u64(data.len) > compress.max_size {
		return error('data too large (${data.len} > ${compress.max_size})')
	}
	mut out_len := usize(0)

	address := C.tdefl_compress_mem_to_heap(data.data, data.len, &out_len, flags)
	if address == 0 {
		return error('compression failed')
	}
	if u64(out_len) > compress.max_size {
		return error('compressed data is too large (${out_len} > ${compress.max_size})')
	}
	return unsafe { address.vbytes(int(out_len)) }
}

// decompresses an array of bytes based on providing flags and returns the decompressed bytes in a new array
// NB: this is a low level api, a high level implementation like zlib/gzip should be preferred
[manualfree]
pub fn decompress(data []u8, flags int) ![]u8 {
	mut out_len := usize(0)

	address := C.tinfl_decompress_mem_to_heap(data.data, data.len, &out_len, flags)
	if address == 0 {
		return error('decompression failed')
	}
	if u64(out_len) > compress.max_size {
		return error('decompressed data is too large (${out_len} > ${compress.max_size})')
	}
	return unsafe { address.vbytes(int(out_len)) }
}
