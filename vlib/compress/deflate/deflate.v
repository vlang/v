module deflate

import encoding.binary
import hash.adler32
import hash.crc32

pub type ChunkCallback = fn (chunk []u8, userdata voidptr) int

// CompressFormat selects the output container around the RFC 1951 payload.
pub enum CompressFormat {
	zlib
	gzip
	raw_deflate
}

@[params]
pub struct CompressParams {
pub:
	format CompressFormat = .zlib
}

pub struct RawInflateResult {
pub:
	decoded  []u8
	consumed int
}

pub struct ZlibHeader {
pub:
	payload_start int = 2
}

pub struct GzipHeader {
pub mut:
	flags             u8
	payload_start     int
	extra             []u8
	filename          []u8
	comment           []u8
	modification_time u32
	operating_system  u8
}

// validate_zlib_header validates a RFC 1950 zlib header.
@[direct_array_access]
pub fn validate_zlib_header(data []u8) !ZlibHeader {
	if data.len < 6 {
		return error('invalid zlib stream: too short')
	}
	if data[0] & 0x0f != 8 {
		return error('invalid zlib stream: unsupported compression method')
	}
	if (u32(data[0]) * 256 + u32(data[1])) % 31 != 0 {
		return error('invalid zlib stream: bad header checksum')
	}
	if data[1] & 0x20 != 0 {
		return error('invalid zlib stream: preset dictionary not supported')
	}
	return ZlibHeader{}
}

// validate_gzip_header validates a RFC 1952 gzip header and returns parsed fields.
@[direct_array_access]
pub fn validate_gzip_header(data []u8) !GzipHeader {
	if data.len < 18 {
		return error('invalid gzip stream: too short')
	}
	if data[0] != 0x1f || data[1] != 0x8b {
		return error('invalid gzip stream: bad magic')
	}
	if data[2] != 8 {
		return error('invalid gzip stream: unsupported compression method')
	}
	flg := data[3]
	if flg & 0xe0 != 0 {
		return error('invalid gzip stream: reserved flags set')
	}
	mut header := GzipHeader{
		flags:             flg
		payload_start:     10
		modification_time: binary.little_endian_u32_at(data, 4)
		operating_system:  data[9]
	}
	if flg & 0x04 != 0 {
		if header.payload_start + 2 > data.len {
			return error('invalid gzip stream: truncated extra')
		}
		xlen := int(u32(data[header.payload_start]) | u32(data[header.payload_start + 1]) << 8)
		header.payload_start += 2
		if header.payload_start + xlen > data.len {
			return error('invalid gzip stream: truncated extra')
		}
		header.extra = data[header.payload_start..header.payload_start + xlen]
		header.payload_start += xlen
	}
	if flg & 0x08 != 0 {
		for header.payload_start < data.len && data[header.payload_start] != 0 {
			header.filename << data[header.payload_start]
			header.payload_start++
		}
		header.payload_start++
	}
	if flg & 0x10 != 0 {
		for header.payload_start < data.len && data[header.payload_start] != 0 {
			header.comment << data[header.payload_start]
			header.payload_start++
		}
		header.payload_start++
	}
	if flg & 0x02 != 0 {
		if header.payload_start + 2 > data.len {
			return error('invalid gzip stream: truncated fhcrc')
		}
		expected_crc16 := u16(data[header.payload_start]) | (u16(data[header.payload_start + 1]) << 8)
		actual_crc16 := u16(crc32.sum(data[..header.payload_start]) & 0xffff)
		if actual_crc16 != expected_crc16 {
			return error('invalid gzip stream: header crc16 mismatch')
		}
		header.payload_start += 2
	}
	if header.payload_start + 8 > data.len {
		return error('invalid gzip stream: truncated payload')
	}
	return header
}

// compress compresses data as zlib, gzip, or raw DEFLATE.
pub fn compress(data []u8, format CompressParams) ![]u8 {
	return match format.format {
		.zlib { compress_zlib(data) }
		.gzip { compress_gzip(data) }
		.raw_deflate { deflate_compress_fixed(data)! }
	}
}

pub fn compress_zlib(data []u8) ![]u8 {
	mut payload := deflate_compress_fixed(data)!
	defer {
		unsafe { payload.free() }
	}
	cksum := adler32.sum(data)
	mut out := []u8{cap: 2 + payload.len + 4}
	out << u8(0x78) // CMF: CM=8 deflate, CINFO=7 (32K window)
	out << u8(0x9c) // FLG: default compression, FCHECK satisfies (CMF*256+FLG)%31==0
	out << payload
	out << binary.big_endian_get_u32(cksum)
	return out
}

// compress_gzip compresses data into a gzip stream (RFC 1952).
pub fn compress_gzip(data []u8) ![]u8 {
	mut payload := deflate_compress_fixed(data)!
	defer {
		unsafe { payload.free() }
	}
	mut out := []u8{cap: 10 + payload.len + 8}
	// 10-byte gzip header: ID1 ID2 CM FLG MTIME(4) XFL OS
	out << [u8(0x1f), 0x8b, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff]
	out << payload
	out << binary.little_endian_get_u32(crc32.sum(data))
	out << binary.little_endian_get_u32(u32(data.len))
	return out
}

// compress_raw compresses data to a raw RFC 1951 DEFLATE stream.
pub fn compress_raw(data []u8) ![]u8 {
	return deflate_compress_fixed(data)!
}

// decompress decompresses a zlib (RFC 1950), gzip (RFC 1952), or raw DEFLATE (RFC 1951) stream.
// The format is auto-detected.
pub fn decompress(data []u8) ![]u8 {
	if data.len >= 2 {
		// gzip magic: 0x1f 0x8b
		if data[0] == 0x1f && data[1] == 0x8b {
			return decompress_gzip(data)
		}
		// zlib: CM=8 and header checksum passes
		if data[0] & 0x0f == 8 && (u32(data[0]) * 256 + u32(data[1])) % 31 == 0 {
			return decompress_zlib(data)
		}
	}
	// raw DEFLATE
	return inflate(data)
}

// decompress_zlib decompresses a zlib stream (RFC 1950).
// It returns the decompressed bytes in a new array.
pub fn decompress_zlib(data []u8) ![]u8 {
	header := validate_zlib_header(data)!
	payload := data[header.payload_start..data.len - 4]
	expected := binary.big_endian_u32_at(data, data.len - 4)
	res := inflate_with_consumed(payload)!
	if res.consumed != payload.len {
		return error('invalid zlib stream: trailing data before adler32')
	}
	decoded := res.decoded
	if adler32.sum(decoded) != expected { return error('invalid zlib stream: adler32 mismatch') }
	return decoded
}

// decompress_gzip decompresses a gzip stream (RFC 1952).
// It returns the decompressed bytes in a new array.
pub fn decompress_gzip(data []u8) ![]u8 {
	header := validate_gzip_header(data)!
	payload := data[header.payload_start..data.len - 8]
	expected_crc := binary.little_endian_u32_at(data, data.len - 8)
	expected_size := binary.little_endian_u32_at(data, data.len - 4)
	res := inflate_with_consumed(payload)!
	if res.consumed != payload.len {
		return error('invalid gzip stream: trailing data before trailer')
	}
	decoded := res.decoded
	if crc32.sum(decoded) != expected_crc {
		return error('invalid gzip stream: crc32 mismatch')
	}
	if u32(decoded.len) != expected_size {
		return error('invalid gzip stream: size mismatch')
	}
	return decoded
}

// decompress_raw_with_consumed decompresses raw RFC 1951 DEFLATE data and tracks consumed bytes.
pub fn decompress_raw_with_consumed(data []u8) !RawInflateResult {
	res := inflate_with_consumed(data)!
	return RawInflateResult{
		decoded:  res.decoded
		consumed: res.consumed
	}
}

// decompress_with_callback decompresses a zlib/gzip/raw stream (RFC 1950, RFC 1952) using a callback for chunked delivery.
// The callback receives chunks of decompressed data and should return the chunk length to continue, or 0 to abort.
// Returns the total decompressed length.
pub fn decompress_with_callback(data []u8, cb ChunkCallback, userdata voidptr) !int {
	if data.len >= 2 {
		// gzip magic: 0x1f 0x8b
		if data[0] == 0x1f && data[1] == 0x8b {
			return decompress_gzip_with_callback(data, cb, userdata)
		}
		// zlib: CM=8 and header checksum passes
		if data[0] & 0x0f == 8 && (u32(data[0]) * 256 + u32(data[1])) % 31 == 0 {
			return decompress_zlib_with_callback(data, cb, userdata)
		}
	}
	// raw DEFLATE
	res := inflate_with_callback(data, cb, userdata)!
	return res.delivered
}

fn decompress_zlib_with_callback(data []u8, cb ChunkCallback, userdata voidptr) !int {
	header := validate_zlib_header(data)!
	payload := data[header.payload_start..data.len - 4]
	expected := binary.big_endian_u32_at(data, data.len - 4)
	res := inflate_with_callback(payload, cb, userdata)!
	if res.aborted {
		return res.delivered
	}
	if res.consumed != payload.len {
		return error('invalid zlib stream: trailing data before adler32')
	}
	if adler32.sum(res.decoded) != expected {
		return error('invalid zlib stream: adler32 mismatch')
	}
	return res.delivered
}

fn decompress_gzip_with_callback(data []u8, cb ChunkCallback, userdata voidptr) !int {
	header := validate_gzip_header(data)!
	payload := data[header.payload_start..data.len - 8]
	expected_crc := binary.little_endian_u32_at(data, data.len - 8)
	expected_size := binary.little_endian_u32_at(data, data.len - 4)
	res := inflate_with_callback(payload, cb, userdata)!
	if res.aborted {
		return res.delivered
	}
	if res.consumed != payload.len {
		return error('invalid gzip stream: trailing data before trailer')
	}
	if crc32.sum(res.decoded) != expected_crc {
		return error('invalid gzip stream: crc32 mismatch')
	}
	if u32(res.decoded.len) != expected_size {
		return error('invalid gzip stream: size mismatch')
	}
	return res.delivered
}

fn bit_reverse(v u32, n int) u32 {
	mut r := u32(0)
	mut val := v
	for _ in 0 .. n {
		r = (r << 1) | (val & 1)
		val >>= 1
	}
	return r
}
