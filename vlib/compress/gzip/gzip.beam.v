// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// BEAM backend gzip module
// [rfc1952](https://datatracker.ietf.org/doc/html/rfc1952) compliant
// gzip compression/decompression using Erlang's zlib module
module gzip

import compress as compr

// CompressFlags - compression behavior flags
// Note: On BEAM, some flags may not be applicable
@[flag]
pub enum CompressFlags {
	// The low 12 bits will be overwritten by `compression_level`
	compression_level_overwrite_flag01
	compression_level_overwrite_flag02
	compression_level_overwrite_flag03
	compression_level_overwrite_flag04
	compression_level_overwrite_flag05
	compression_level_overwrite_flag06
	compression_level_overwrite_flag07
	compression_level_overwrite_flag08
	compression_level_overwrite_flag09
	compression_level_overwrite_flag10
	compression_level_overwrite_flag11
	compression_level_overwrite_flag12
	// If set, the compressor outputs a zlib header before the deflate data
	write_zlib_header
	// Always compute the adler-32 of the input data
	compute_adler32
	// Set to use faster greedy parsing
	greedy_parsing_flag
	// Enable to decrease the compressor's initialization time
	nondeterministic_parsing_flag
	// Only look for RLE matches
	rle_matches
	// Discards matches <= 5 chars if enabled
	filter_matches
	// Disable usage of optimized Huffman tables
	force_all_static_blocks
	// Only use raw (uncompressed) deflate blocks
	force_all_raw_blocks
}

// CompressParams set compression_level for compression:
// 0-9 on BEAM (standard zlib compression levels)
@[params]
pub struct CompressParams {
pub:
	compression_level int = 6 // 0~9, default 6 for zlib
	flags             CompressFlags
}

// compress compresses an array of bytes using gzip format and returns the compressed bytes.
// On BEAM: Uses zlib:gzip/1 for RFC 1952 compliant gzip compression
// Example: b := 'abcde'.repeat(1000).bytes(); cmprsd := gzip.compress(b)!
//
// BEAM codegen interception required. Translates to:
//   case CompressionLevel of
//       6 ->
//           %% Default level - use simple API
//           zlib:gzip(list_to_binary(Data));
//       Level ->
//           %% Custom level - use streaming API
//           Z = zlib:open(),
//           ok = zlib:deflateInit(Z, Level, deflated, 31, 8, default),
//           Compressed = zlib:deflate(Z, Data, finish),
//           ok = zlib:deflateEnd(Z),
//           ok = zlib:close(Z),
//           iolist_to_binary(Compressed)
//   end
pub fn compress(data []u8, params CompressParams) ![]u8 {
	if data.len == 0 {
		return []
	}
	if params.compression_level !in 0..10 {
		return error('compression level should be in [0,9] on BEAM')
	}
	// Actual compression requires Erlang's zlib module via codegen interception.
	// Without codegen, we cannot perform gzip compression in pure V.
	return error('gzip.compress requires codegen interception (zlib:gzip/1)')
}

// DecompressFlags - decompression behavior flags
// Note: On BEAM, some flags may not be applicable
@[flag]
pub enum DecompressFlags {
	// If set, the input has a valid zlib header
	parse_zlib_header
	// If set, there are more input bytes available
	has_more_input
	// If set, the output buffer is large enough for entire stream
	using_non_wrapping_output_buf
	// Force adler-32 checksum computation
	compute_adler32
}

// DecompressParams set flags for decompression
@[params]
pub struct DecompressParams {
pub:
	verify_header_checksum bool = true
	verify_length          bool = true
	verify_checksum        bool = true
	flags                  DecompressFlags
}

// Gzip header constants (RFC 1952)
pub const reserved_bits = 0b1110_0000
pub const ftext = 0b0000_0001
pub const fextra = 0b0000_0100
pub const fname = 0b0000_1000
pub const fcomment = 0b0001_0000
pub const fhcrc = 0b0000_0010

const min_header_length = 18

@[noinit]
pub struct GzipHeader {
pub mut:
	length            int = 10
	extra             []u8
	filename          []u8
	comment           []u8
	modification_time u32
	operating_system  u8
}

// validate validates the gzip header and returns its details if valid
@[direct_array_access]
pub fn validate(data []u8, params DecompressParams) !GzipHeader {
	if data.len < min_header_length {
		return error('data is too short, not gzip compressed?')
	} else if data[0] != 0x1f || data[1] != 0x8b {
		return error('wrong magic numbers, not gzip compressed?')
	} else if data[2] != 0x08 {
		return error('gzip data is not compressed with DEFLATE')
	}
	mut header := GzipHeader{}

	if data[3] & reserved_bits > 0 {
		return error('reserved flags are set, unsupported field detected')
	}

	if data[3] & fextra > 0 {
		xlen := data[header.length]
		header.extra = data[header.length + 1..header.length + 1 + xlen]
		header.length += xlen + 1
	}
	if data[3] & fname > 0 {
		for header.length < data.len && data[header.length] != 0x00 {
			header.filename << data[header.length]
			header.length++
		}
		header.length++
	}
	if data[3] & fcomment > 0 {
		for header.length < data.len && data[header.length] != 0x00 {
			header.comment << data[header.length]
			header.length++
		}
		header.length++
	}
	if data[3] & fhcrc > 0 {
		if header.length + 12 > data.len {
			return error('data too short')
		}
		// Note: header CRC verification would happen here
		// On BEAM, zlib:gunzip handles this automatically
		header.length += 4
	}
	if header.length + 8 > data.len {
		return error('data too short')
	}
	header.operating_system = data[9]
	return header
}

// decompress decompresses gzip data and returns the decompressed bytes.
// On BEAM: Uses zlib:gunzip/1 for RFC 1952 compliant gzip decompression
// Example: b := gzip.compress(data)!; original := gzip.decompress(b)!
//
// BEAM codegen interception required. Translates to:
//   zlib:gunzip(list_to_binary(Data))
// zlib:gunzip handles header parsing, decompression, CRC32, and length verification.
pub fn decompress(data []u8, params DecompressParams) ![]u8 {
	if data.len == 0 {
		return error('gzip.decompress: empty input')
	}
	// Validate header first (for detailed error messages before calling zlib)
	_ := validate(data, params)!

	// Actual decompression requires Erlang's zlib module via codegen interception.
	return error('gzip.decompress requires codegen interception (zlib:gunzip/1)')
}

// decompress_with_callback decompresses gzip data using chunked decompression.
// The callback receives chunks of at most 32KB each.
//
// BEAM codegen interception required. Translates to:
//   Z = zlib:open(),
//   zlib:inflateInit(Z, 31),      % 31 = 15+16 for gzip window
//   loop:
//     case zlib:safeInflate(Z, Data) of
//         {continue, Chunk} -> call callback with Chunk, continue;
//         {finished, Chunk} -> call callback with Chunk, done
//     end,
//   zlib:inflateEnd(Z),
//   zlib:close(Z)
pub fn decompress_with_callback(data []u8, cb compr.ChunkCallback, userdata voidptr, params DecompressParams) !int {
	if data.len == 0 {
		return 0
	}
	_ := validate(data, params)!

	// Actual chunked decompression requires Erlang's zlib streaming API via codegen.
	return error('gzip.decompress_with_callback requires codegen interception (zlib streaming API)')
}
