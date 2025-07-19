module compress

#flag -I @VEXEROOT/thirdparty/zip
#include "miniz.h"

pub const max_size = u64(1 << 31)

fn C.tdefl_compress_mem_to_heap(source_buf voidptr, source_buf_len usize, out_len &usize, flags int) voidptr
fn C.tinfl_decompress_mem_to_heap(source_buf voidptr, source_buf_len usize, out_len &usize, flags int) voidptr

// compresses an array of bytes based on providing flags and returns the compressed bytes in a new array
// NB: this is a low level api, a high level implementation like zlib/gzip should be preferred
@[manualfree]
pub fn compress(data []u8, flags int) ![]u8 {
	if u64(data.len) > max_size {
		return error('data too large (${data.len} > ${max_size})')
	}
	mut out_len := usize(0)

	address := C.tdefl_compress_mem_to_heap(data.data, data.len, &out_len, flags)
	if address == 0 {
		return error('compression failed')
	}
	if u64(out_len) > max_size {
		return error('compressed data is too large (${out_len} > ${max_size})')
	}
	unsafe {
		ret := address.vbytes(int(out_len)).clone()
		C.free(address)
		return ret
	}
}

// decompresses an array of bytes based on providing flags and returns the decompressed bytes in a new array
// NB: this is a low level api, a high level implementation like zlib/gzip should be preferred
@[manualfree]
pub fn decompress(data []u8, flags int) ![]u8 {
	mut out_len := usize(0)

	address := C.tinfl_decompress_mem_to_heap(data.data, data.len, &out_len, flags)
	if address == 0 {
		return error('decompression failed')
	}
	if u64(out_len) > max_size {
		return error('decompressed data is too large (${out_len} > ${max_size})')
	}

	unsafe {
		ret := address.vbytes(int(out_len)).clone()
		C.free(address)
		return ret
	}
}

// ChunkCallback is used to receive decompressed chunks of maximum 32768 bytes.
// After processing the chunk this function should return the chunk's length to indicate
// the decompressor to send more chunks, otherwise the decompression stops.
// The userdata parameter comes from the call to decompress_with_callback/4, and can be used
// to pass arbitrary data, without having to create a closure.
pub type ChunkCallback = fn (chunk []u8, userdata voidptr) int

// decompress_with_callback decompresses an array of bytes, based on the provided flags, and a V fn callback to receive decompressed chunks, of at most 32 kilobytes each.
// It returns the total decompressed length, or a decompression error.
// NB: this is a low level api, a high level implementation like zlib/gzip should be preferred.
pub fn decompress_with_callback(data []u8, cb ChunkCallback, userdata voidptr, flags int) !u64 {
	cbdata := DecompressionCallBackData{
		data:     data.data
		size:     usize(data.len)
		cb:       cb
		userdata: userdata
	}
	status := C.tinfl_decompress_mem_to_callback(cbdata.data, &cbdata.size, c_cb_for_decompress_mem,
		&cbdata, flags)
	if status == 0 {
		return error('decompression error')
	}
	return cbdata.decompressed_size
}

struct DecompressionCallBackData {
mut:
	data              voidptr
	size              usize
	decompressed_size u64
	userdata          voidptr
	cb                ChunkCallback = unsafe { nil }
}

fn c_cb_for_decompress_mem(buf &char, len int, pdcbd voidptr) int {
	mut cbdata := unsafe { &DecompressionCallBackData(pdcbd) }
	if cbdata.cb(unsafe { voidptr(buf).vbytes(len) }, cbdata.userdata) == len {
		cbdata.decompressed_size += u64(len)
		return 1 // continue decompressing
	}
	return 0 // stop decompressing
}

type DecompressCallback = fn (const_buffer voidptr, len int, userdata voidptr) int

fn C.tinfl_decompress_mem_to_callback(const_input_buffer voidptr, psize &usize, put_buf_cb DecompressCallback, userdata voidptr, flags int) int
