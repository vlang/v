// [rfc1952](https://datatracker.ietf.org/doc/html/rfc1952) compliant
// gzip compression/decompression

module gzip

import compress.deflate
import encoding.binary
import hash.crc32

// CompressParams set compression_level for compression:
// 0: Huffman only;
// 1: Huffman+LZ (fastest/crap compression);
// 128: default_max_probes;
// 4095: Huffman+LZ (slowest/best compression)
@[params]
pub struct CompressParams {
pub:
	compression_level int = 128 // 0~4095
}

// compress compresses an array of bytes using gzip and returns the compressed bytes in a new array
// Note: compression_level 0~4095 (currently unused, reserved for future optimization)
pub fn compress(data []u8, params CompressParams) ![]u8 {
	if params.compression_level !in 0..4096 {
		return error('compression level should in [0,4095]')
	}
	// Delegate to deflate.compress_gzip() which implements RFC 1952
	return deflate.compress_gzip(data)
}

// DecompressParams controls gzip decompression behaviour.
// All verification is now performed by the deflate backend; this struct
// is kept for API compatibility.
@[params]
pub struct DecompressParams {}

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

// validate validates the gzip header of data and returns its parsed details if valid.
// Note: only retained for API compatibility, all validation is now performed by the deflate backend.
// The returned header details are not used by the decompressor.
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
	header.modification_time = binary.little_endian_u32_at(data, 4)

	if data[3] & reserved_bits > 0 {
		// rfc 1952 2.3.1.2 Compliance
		// A compliant decompressor must give an error indication if any
		// reserved bit is non-zero, since such a bit could indicate the
		// presence of a new field that would cause subsequent data to be
		// interpreted incorrectly.
		return error('reserved flags are set, unsupported field detected')
	}

	if data[3] & fextra > 0 {
		if header.length + 2 > data.len {
			return error('data too short')
		}
		xlen := int(u16(data[header.length]) | (u16(data[header.length + 1]) << 8))
		header.length += 2
		if header.length + xlen > data.len {
			return error('data too short')
		}
		header.extra = data[header.length..header.length + xlen]
		header.length += xlen
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
		if header.length + 2 > data.len {
			return error('data too short')
		}
		checksum_header := crc32.sum(data[..header.length])
		checksum_header_expected := u16(data[header.length]) | (u16(data[header.length + 1]) << 8)
		if u16(checksum_header & 0xffff) != checksum_header_expected {
			return error('header checksum verification failed')
		}
		header.length += 2
	}
	if header.length + 8 > data.len {
		return error('data too short')
	}
	header.operating_system = data[9]
	return header
}

// decompress decompresses a gzip stream and returns the decompressed bytes in a new array.
// Example: b := 'abcdef'.repeat(1000).bytes(); cmpr := gzip.compress(b)!; decmpr := gzip.decompress(cmpr)!; assert cmpr.len < b.len; assert b == decmpr
pub fn decompress(data []u8, params DecompressParams) ![]u8 {
	return deflate.decompress_gzip(data)
}

// decompress_with_callback decompresses the given gzip `data` and calls `cb` with each chunk of
// decompressed bytes. A chunk is usually 32 KB or less. The chunk data received by `cb` should be
// cloned if it needs to be stored for later use.
// The callback should return the chunk length to continue, or 0 to abort early.
// Returns the total number of decompressed bytes delivered to the callback.
pub fn decompress_with_callback(data []u8, cb deflate.ChunkCallback, userdata voidptr, params DecompressParams) !int {
	return deflate.decompress_with_callback(data, cb, userdata)
}
