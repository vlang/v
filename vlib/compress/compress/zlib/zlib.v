module zlib

#flag -I @VEXEROOT/thirdparty/zip
#include "miniz.h"


fn C.tdefl_compress_mem_to_heap(source_buf voidptr, source_buf_len usize, out_len &usize, flags int) voidptr
fn C.tinfl_decompress_mem_to_heap(source_buf voidptr, source_buf_len usize, out_len &usize, flags int) voidptr


pub fn compress(data []byte) ?[]byte {
	mut out_len := usize(0)

	// flags = TDEFL_WRITE_ZLIB_HEADER (0x01000)
	address := C.tdefl_compress_mem_to_heap(data.data, data.len, &out_len, 0x01000)
	if address == 0 {
		return error("compression failed")
	}
	if out_len > 1 << 31 {
		return error("compression result is too large")
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


pub fn decompress(data []byte) ?[]byte {
	mut out_len := usize(0)

	// flags = TINFL_FLAG_PARSE_ZLIB_HEADER (0x1)
	address := C.tinfl_decompress_mem_to_heap(data.data, data.len, &out_len, 0x1)
	if address == 0 {
		return error("decompression failed")
	}
	if out_len > 1 << 31 {
		return error("decompression result is too large")
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
