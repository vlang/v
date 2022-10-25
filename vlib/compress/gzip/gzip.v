// [rfc1952](https://datatracker.ietf.org/doc/html/rfc1952) compliant
// gzip compression/decompression

module gzip

import compress
import hash.crc32

// compresses an array of bytes using gzip and returns the compressed bytes in a new array
// Example: compressed := gzip.compress(b)?
pub fn compress(data []u8) ![]u8 {
	compressed := compress.compress(data, 0)!
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
		u8(checksum >> 24),
		u8(checksum >> 16),
		u8(checksum >> 8),
		u8(checksum),
		u8(length >> 24),
		u8(length >> 16),
		u8(length >> 8),
		u8(length),
	] // 8 bytes
	return result
}

[params]
pub struct DecompressParams {
	verify_header_checksum bool = true
	verify_length          bool = true
	verify_checksum        bool = true
}

// decompresses an array of bytes using zlib and returns the decompressed bytes in a new array
// Example: decompressed := gzip.decompress(b)?
pub fn decompress(data []u8, params DecompressParams) ![]u8 {
	if data.len < 18 {
		return error('data is too short, not gzip compressed?')
	} else if data[0] != 0x1f || data[1] != 0x8b {
		return error('wrong magic numbers, not gzip compressed?')
	} else if data[2] != 0x08 {
		return error('gzip data is not compressed with DEFLATE')
	}
	mut header_length := 10

	// parse flags, we ignore most of them, but we still need to parse them
	// correctly, so we dont accidently decompress something that belongs
	// to the header

	if data[4] & 0b1110_0000 > 0 { // reserved bits
		// rfc 1952 2.3.1.2 Compliance
		// A compliant decompressor must give an error indication if any
		// reserved bit is non-zero, since such a bit could indicate the
		// presence of a new field that would cause subsequent data to be
		// interpreted incorrectly.
		return error('reserved flags are set, unsupported field detected')
	}

	// if data[4] & 0b0000_0001 {} // FTEXT
	if data[4] & 0b0000_0100 > 0 { // FEXTRA, extra data
		xlen := data[header_length]
		header_length += xlen + 1
	}
	if data[4] & 0b0000_1000 > 0 { // FNAME, file name
		// filename is zero-terminated, so skip until we hit a zero byte
		for header_length < data.len && data[header_length] != 0x00 {
			header_length++
		}
		header_length++
	}
	if data[4] & 0b0001_0000 > 0 { // FCOMMENT
		// comment is zero-terminated, so skip until we hit a zero byte
		for header_length < data.len && data[header_length] != 0x00 {
			header_length++
		}
		header_length++
	}
	if data[4] & 0b0000_0010 > 0 { // FHCRC, flag header crc
		if header_length + 12 > data.len {
			return error('data too short')
		}
		checksum_header := crc32.sum(data[..header_length])
		checksum_header_expected := (u32(data[header_length]) << 24) | (u32(data[header_length + 1]) << 16) | (u32(data[
			header_length + 2]) << 8) | data[header_length + 3]
		if params.verify_header_checksum && checksum_header != checksum_header_expected {
			return error('header checksum verification failed')
		}
		header_length += 4
	}
	if header_length + 8 > data.len {
		return error('data too short')
	}

	decompressed := compress.decompress(data[header_length..data.len - 8], 0)!
	length_expected := (u32(data[data.len - 4]) << 24) | (u32(data[data.len - 3]) << 16) | (u32(data[data.len - 2]) << 8) | data[data.len - 1]
	if params.verify_length && decompressed.len != length_expected {
		return error('length verification failed, got $decompressed.len, expected $length_expected')
	}
	checksum := crc32.sum(decompressed)
	checksum_expected := (u32(data[data.len - 8]) << 24) | (u32(data[data.len - 7]) << 16) | (u32(data[data.len - 6]) << 8) | data[data.len - 5]
	if params.verify_checksum && checksum != checksum_expected {
		return error('checksum verification failed')
	}
	return decompressed
}
