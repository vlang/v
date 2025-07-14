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

// decompress_callback decompresses an array of bytes based on providing flags and a callback to
// receive chunks at most of 32 kilobytes. Returns the number of chunks decompressed or a decompression error.
// NB: this is a low level api, a high level implementation like zlib/gzip should be preferred
pub fn decompress_callback(data []u8, flags int, cb ChunkCallback) !int {
	mut chunks := new_chunks(cb)
	mut size := usize(data.len)
	status := C.tinfl_decompress_mem_to_callback(data.data, &size, chunks.c_callback,
		unsafe { nil }, flags)
	if status == 0 {
		return error('decompression error')
	}
	return chunks.len
}

// ChunkCallback is used to receive decompressed chunks of maximum 32768 bytes.
// After processing the chunk this function should return the chunk's length to indicate
// the decompressor to send more chunks, otherwise the decompression stops.
pub type ChunkCallback = fn (chunk []u8) int

type DecompressCallback = fn (buf voidptr, len int, user voidptr) int

fn C.tinfl_decompress_mem_to_callback(buf voidptr, size &usize, cb DecompressCallback, user voidptr, flags int) voidptr

// Chunks is a helper to connect decompress' C and V callbacks
@[heap]
struct Chunks {
	v_callback ChunkCallback = unsafe { nil }
mut:
	chunk []u8
	len   int
}

fn new_chunks(cb ChunkCallback) &Chunks {
	return &Chunks{
		v_callback: cb
		chunk:      []u8{len: 33_000} // 33_000 > 32768 just in case
	}
}

fn (mut c Chunks) c_callback(buf &char, len int, user voidptr) int {
	unsafe {
		for i in 0 .. len {
			c.chunk[i] = u8(buf[i])
		}
	}
	if c.v_callback(c.chunk[0..len]) == len {
		c.len += len
		return 1
	}
	// indicates caller compr_decompress_cb should stop
	return 0
}
