module deflate

import encoding.binary
import hash.crc32

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

// compress compresses data as zlib, gzip, or raw DEFLATE.
pub fn compress(data []u8, format CompressParams) ![]u8 {
	payload := deflate_compress_fixed(data)
	match format.format {
		.zlib { return compress_zlib(data) }
		.gzip { return compress_gzip(data) }
		.raw_deflate { return payload }
	}
}

pub fn compress_zlib(data []u8) ![]u8 {
	payload := deflate_compress_fixed(data)
	cksum := adler32(data)
	mut out := []u8{cap: 2 + payload.len + 4}
	out << u8(0x78) // CMF: CM=8 deflate, CINFO=7 (32K window)
	out << u8(0x9c) // FLG: default compression, FCHECK satisfies (CMF*256+FLG)%31==0
	out << payload
	out << binary.big_endian_get_u32(cksum)
	return out
}

// compress_gzip compresses data into a gzip stream (RFC 1952).
pub fn compress_gzip(data []u8) ![]u8 {
	payload := deflate_compress_fixed(data)
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
	return deflate_compress_fixed(data)
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

fn decompress_zlib(data []u8) ![]u8 {
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
	payload := data[2..data.len - 4]
	expected := binary.big_endian_u32_at(data, data.len - 4)
	decoded := inflate(payload)!
	if adler32(decoded) != expected {
		return error('invalid zlib stream: adler32 mismatch')
	}
	return decoded
}

fn decompress_gzip(data []u8) ![]u8 {
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
	mut pos := 10 // fixed header size
	if flg & 0x04 != 0 { // FEXTRA
		if pos + 2 > data.len {
			return error('invalid gzip stream: truncated extra')
		}
		xlen := int(u32(data[pos]) | u32(data[pos + 1]) << 8)
		pos += 2 + xlen
	}
	if flg & 0x08 != 0 { // FNAME
		for pos < data.len && data[pos] != 0 {
			pos++
		}
		pos++
	}
	if flg & 0x10 != 0 { // FCOMMENT
		for pos < data.len && data[pos] != 0 {
			pos++
		}
		pos++
	}
	if flg & 0x02 != 0 { // FHCRC
		pos += 2
	}
	if pos + 8 > data.len {
		return error('invalid gzip stream: truncated payload')
	}
	payload := data[pos..data.len - 8]
	expected_crc := binary.little_endian_u32_at(data, data.len - 8)
	expected_size := binary.little_endian_u32_at(data, data.len - 4)
	decoded := inflate(payload)!
	if crc32.sum(decoded) != expected_crc {
		return error('invalid gzip stream: crc32 mismatch')
	}
	if u32(decoded.len) != expected_size {
		return error('invalid gzip stream: size mismatch')
	}
	return decoded
}

fn adler32(data []u8) u32 {
	mod_adler := u32(65521)
	mut a := u32(1)
	mut b := u32(0)
	for byte_ in data {
		a = (a + u32(byte_)) % mod_adler
		b = (b + a) % mod_adler
	}
	return (b << 16) | a
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
