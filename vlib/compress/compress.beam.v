// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// BEAM backend compress module
// On BEAM, V compression operations map to Erlang's zlib module
module compress

pub const max_size = u64(1 << 31)

// compress compresses an array of bytes using raw deflate and returns the compressed bytes.
// On BEAM: Uses zlib:compress/1 for compression
// Note: This is a low level API - prefer compress.gzip or compress.zlib for higher level usage.
pub fn compress(data []u8, flags int) ![]u8 {
	if u64(data.len) > max_size {
		return error('data too large (${data.len} > ${max_size})')
	}
	// BEAM codegen translates to:
	//   Z = zlib:open(),
	//   zlib:deflateInit(Z),
	//   Compressed = zlib:deflate(Z, Data, finish),
	//   zlib:deflateEnd(Z),
	//   zlib:close(Z),
	//   Compressed
	// For now, return stub - actual implementation is in codegen
	return []
}

// decompress decompresses an array of bytes using raw inflate and returns the decompressed bytes.
// On BEAM: Uses zlib:uncompress/1 for decompression
// Note: This is a low level API - prefer compress.gzip or compress.zlib for higher level usage.
pub fn decompress(data []u8, flags int) ![]u8 {
	// BEAM codegen translates to:
	//   Z = zlib:open(),
	//   zlib:inflateInit(Z),
	//   Decompressed = zlib:inflate(Z, Data),
	//   zlib:inflateEnd(Z),
	//   zlib:close(Z),
	//   Decompressed
	// For now, return stub - actual implementation is in codegen
	return []
}

// ChunkCallback is used to receive decompressed chunks of maximum 32768 bytes.
// After processing the chunk this function should return the chunk's length to indicate
// the decompressor to send more chunks, otherwise the decompression stops.
// The userdata parameter comes from the call to decompress_with_callback/4, and can be used
// to pass arbitrary data, without having to create a closure.
pub type ChunkCallback = fn (chunk []u8, userdata voidptr) int

// decompress_with_callback decompresses an array of bytes, based on the provided flags,
// and a V fn callback to receive decompressed chunks, of at most 32 kilobytes each.
// It returns the total decompressed length, or a decompression error.
// On BEAM: Uses chunked zlib decompression with callback
// Note: This is a low level API - prefer compress.gzip for higher level usage.
pub fn decompress_with_callback(data []u8, cb ChunkCallback, userdata voidptr, flags int) !u64 {
	// BEAM codegen would use zlib streaming API:
	//   Z = zlib:open(),
	//   zlib:inflateInit(Z),
	//   loop: zlib:safeInflate(Z, Data) -> {continue, Chunk} | {finished, Chunk}
	//         call callback with chunk
	//   zlib:inflateEnd(Z),
	//   zlib:close(Z)
	// For now, return stub - actual implementation is in codegen
	return u64(0)
}

// Note: The following C-specific types and callbacks are not needed on BEAM
// but are defined here for type compatibility with the main module:

struct DecompressionCallBackData {
mut:
	data              voidptr
	size              usize
	decompressed_size u64
	userdata          voidptr
	cb                ChunkCallback = unsafe { nil }
}
