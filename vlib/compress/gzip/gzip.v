// [rfc1952](https://datatracker.ietf.org/doc/html/rfc1952) compliant
// gzip compression/decompression

module gzip

import compress as compr
import hash.crc32

// CompressFlags
// TODO: These flags have no use now
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

	// If set, the compressor outputs a zlib header before the deflate data, and the Adler-32 of the source data at the end. Otherwise, you'll get raw deflate data.
	write_zlib_header //= 0x01000
	// Always compute the adler-32 of the input data (even when not writing zlib headers).
	compute_adler32 //= 0x02000
	// Set to use faster greedy parsing, instead of more efficient lazy parsing.
	greedy_parsing_flag //= 0x04000
	// Enable to decrease the compressor's initialization time to the minimum, but the output may vary from run to run given the same input (depending on the contents of memory).
	nondeterministic_parsing_flag //= 0x08000
	// Only look for RLE matches (matches with a distance of 1)
	rle_matches //= 0x10000
	// Discards matches <= 5 chars if enabled.
	filter_matches //= 0x20000
	// Disable usage of optimized Huffman tables.
	force_all_static_blocks //= 0x40000
	// Only use raw (uncompressed) deflate blocks.
	force_all_raw_blocks //= 0x80000
}

// CompressParams set compression_level for compression:
// 0: Huffman only;
// 1: Huffman+LZ (fastest/crap compression);
// 128: default_max_probes;
// 4095: Huffman+LZ (slowest/best compression)
@[params]
pub struct CompressParams {
pub:
	compression_level int = 128 // 0~4095
	flags             CompressFlags
}

// compresses an array of bytes using gzip and returns the compressed bytes in a new array
// Example: compressed := gzip.compress(b, compression_level:4095)!
// Note: compression_level 0~4095
pub fn compress(data []u8, params CompressParams) ![]u8 {
	if params.compression_level !in 0..4096 {
		return error('compression level should in [0,4095]')
	}
	// The low 12 bits are reserved to control the max # of hash probes per dictionary lookup.
	flags := params.compression_level | (int(params.flags) & ~int(4095))
	compressed := compr.compress(data, flags)!
	// header
	mut result := [
		u8(0x1f), // magic numbers (1F 8B)
		0x8b,
		0x08, // deflate
		0x00, // header flags
		0x00, // 4-byte timestamp, 0 = no timestamp (00 00 00 00)
		0x00,
		0x00,
		0x00,
		0x00, // extra flags
		0xff, // operating system id (0xff = unknown)
	] // 10 bytes
	result << compressed
	// trailer
	checksum := crc32.sum(data)
	length := data.len
	result << [
		u8(checksum),
		u8(checksum >> 8),
		u8(checksum >> 16),
		u8(checksum >> 24),
		u8(length),
		u8(length >> 8),
		u8(length >> 16),
		u8(length >> 24),
	] // 8 bytes
	return result
}

// DecompressFlags
// TODO: These flags have no use now
@[flag]
pub enum DecompressFlags {
	// If set, the input has a valid zlib header and ends with an adler32 checksum (it's a valid zlib stream). Otherwise, the input is a raw deflate stream.
	parse_zlib_header
	// If set, there are more input bytes available beyond the end of the supplied input buffer. If clear, the input buffer contains all remaining input.
	has_more_input
	// If set, the output buffer is large enough to hold the entire decompressed stream. If clear, the output buffer is at least the size of the dictionary (typically 32KB).
	using_non_wrapping_output_buf
	// Force adler-32 checksum computation of the decompressed bytes.
	compute_adler32
}

// DecompressParams set flags for decompression:
@[params]
pub struct DecompressParams {
pub:
	verify_header_checksum bool = true
	verify_length          bool = true
	verify_checksum        bool = true
	flags                  DecompressFlags
}

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

// validate validates the header and returns its details if valid
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

	// parse flags, we ignore most of them, but we still need to parse them
	// correctly, so we dont accidently decompress something that belongs
	// to the header

	if data[3] & reserved_bits > 0 {
		// rfc 1952 2.3.1.2 Compliance
		// A compliant decompressor must give an error indication if any
		// reserved bit is non-zero, since such a bit could indicate the
		// presence of a new field that would cause subsequent data to be
		// interpreted incorrectly.
		return error('reserved flags are set, unsupported field detected')
	}

	if data[3] & fextra > 0 {
		xlen := data[header.length]
		header.extra = data[header.length + 1..header.length + 1 + xlen]
		header.length += xlen + 1
	}
	if data[3] & fname > 0 {
		// filename is zero-terminated, so skip until we hit a zero byte
		for header.length < data.len && data[header.length] != 0x00 {
			header.filename << data[header.length]
			header.length++
		}
		header.length++
	}
	if data[3] & fcomment > 0 {
		// comment is zero-terminated, so skip until we hit a zero byte
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
		checksum_header := crc32.sum(data[..header.length])
		checksum_header_expected := (u32(data[header.length]) << 24) | (u32(data[header.length + 1]) << 16) | (u32(data[
			header.length + 2]) << 8) | data[header.length + 3]
		if params.verify_header_checksum && checksum_header != checksum_header_expected {
			return error('header checksum verification failed')
		}
		header.length += 4
	}
	if header.length + 8 > data.len {
		return error('data too short')
	}
	header.operating_system = data[9]
	return header
}

// decompress an array of bytes using zlib and returns the decompressed bytes in a new array.
// Example: decompressed := gzip.decompress(b)!
pub fn decompress(data []u8, params DecompressParams) ![]u8 {
	gzip_header := validate(data, params)!
	header_length := gzip_header.length

	decompressed := compr.decompress(data[header_length..data.len - 8], 0)!
	length_expected := (u32(data[data.len - 1]) << 24) | (u32(data[data.len - 2]) << 16) | (u32(data[data.len - 3]) << 8) | data[data.len - 4]
	if params.verify_length && decompressed.len != length_expected {
		return error('length verification failed, got ${decompressed.len}, expected ${length_expected}')
	}
	checksum := crc32.sum(decompressed)
	checksum_expected := (u32(data[data.len - 5]) << 24) | (u32(data[data.len - 6]) << 16) | (u32(data[data.len - 7]) << 8) | data[data.len - 8]
	if params.verify_checksum && checksum != checksum_expected {
		return error('checksum verification failed')
	}
	return decompressed
}

// decompress_with_callback decompresses the given `data`, using zlib. It calls `cb` with each chunk of decompressed bytes.
// A chunk is usually 32 KB or less. Note: the chunk data received by `cb` should be cloned, if you need to store it for later,
// and not process it right away.
// The callback function should return the chunk length, if it wants to continue decompressing, or 0, if it wants to abort the decompression early.
// See also compress.ChunkCallback for more details.
pub fn decompress_with_callback(data []u8, cb compr.ChunkCallback, userdata voidptr, params DecompressParams) !int {
	gzip_header := validate(data, params)!
	header_len := gzip_header.length
	expected_len := int((u32(data[data.len - 1]) << 24) | (u32(data[data.len - 2]) << 16) | (u32(data[data.len - 3]) << 8) | data[data.len - 4])
	body := data[header_len..data.len - 8]
	chunks_len := int(compr.decompress_with_callback(body, cb, userdata, 0)!)
	if params.verify_length && expected_len != chunks_len {
		return error('Decompress error: expected length:${expected_len}, got:${chunks_len}')
	}
	return chunks_len
}
