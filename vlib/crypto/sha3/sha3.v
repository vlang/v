// Copyright (c) 2023 Kim Shrier. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Package sha3 implements the 512, 384, 256, and 224
// bit hash algorithms and the 128 and 256 bit
// extended output functions as defined in
// https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.202.pdf.
// Last updated: August 2015
module sha3

import math

// size_224 is the size, in bytes, of a sha3 sum224 checksum.
pub const size_224 = 28
// size_256 is the size, in bytes, of a sha3 sum256 checksum.
pub const size_256 = 32
// size_384 is the size, in bytes, of a sha3 sum384 checksum.
pub const size_384 = 48
// size_512 is the size, in bytes, of a sha3 sum512 checksum.
pub const size_512 = 64

// rate_224 is the rate, in bytes, absorbed into the sponge on every permutation
pub const rate_224 = 144
// rate_256 is the rate, in bytes, absorbed into the sponge on every permutation
pub const rate_256 = 136
// rate_384 is the rate, in bytes, absorbed into the sponge on every permutation
pub const rate_384 = 104
// rate_512 is the rate, in bytes, absorbed into the sponge on every permutation
pub const rate_512 = 72

// xof_rate_128 is the capacity, in bytes, of a 128 bit extended output function sponge
pub const xof_rate_128 = 168
// xof_rate_256 is the capacity, in bytes, of a 256 bit extended output function sponge
pub const xof_rate_256 = 136

// new512 initializes the digest structure for a sha3 512 bit hash
pub fn new512() !&Digest {
	return new_digest(rate_512, size_512)!
}

// new384 initializes the digest structure for a sha3 384 bit hash
pub fn new384() !&Digest {
	return new_digest(rate_384, size_384)!
}

// new256 initializes the digest structure for a sha3 256 bit hash
pub fn new256() !&Digest {
	return new_digest(rate_256, size_256)!
}

// new224 initializes the digest structure for a sha3 224 bit hash
pub fn new224() !&Digest {
	return new_digest(rate_224, size_224)!
}

// new256keccak initializes the digest structure for a keccak 256 bit hash
pub fn new256keccak() !&Digest {
	return new_digest(rate_256, size_256, padding: .keccak)!
}

// new512keccak initializes the digest structure for a keccak 512 bit hash
pub fn new512keccak() !&Digest {
	return new_digest(rate_512, size_512, padding: .keccak)!
}

// new256_xof initializes the digest structure for a sha3 256 bit extended output function
pub fn new256xof(output_len int) !&Digest {
	return new_xof_digest(xof_rate_256, output_len)!
}

// new128_xof initializes the digest structure for a sha3 128 bit extended output function
pub fn new128xof(output_len int) !&Digest {
	return new_xof_digest(xof_rate_128, output_len)!
}

struct HashSizeError {
	Error
	size int
}

fn (err HashSizeError) msg() string {
	return 'Hash size ${err.size} must be ${size_224}, ${size_256}, ${size_384}, or ${size_512}'
}

struct AbsorptionRateError {
	Error
	size int
	rate int
}

fn (err AbsorptionRateError) msg() string {
	return 'Absorption rate ${err.rate} is not compatible with a hash size of ${err.size}'
}

struct XOFRateError {
	Error
	rate int
}

fn (err XOFRateError) msg() string {
	return 'Extended output rate ${err.rate} must be ${xof_rate_128} or ${xof_rate_256}'
}

struct XOFSizeError {
	Error
	size int
}

fn (err XOFSizeError) msg() string {
	return 'Extended output size ${err.size} must be > 0'
}

struct Digest {
	rate       int // the number of bytes absorbed per permutation
	suffix     u8  // the domain suffix, 0x06 for hash, 0x01 for keccak, 0x1f for extended output
	output_len int // the number of bytes to output
mut:
	input_buffer []u8  // temporary holding buffer for input bytes
	s            State // the state of a kaccak-p[1600, 24] sponge
}

// the low order pad bits for a hash function
pub enum Padding as u8 {
	keccak = 0x01
	sha3   = 0x06
	xof    = 0x1f
}

@[params]
pub struct PaddingConfig {
pub:
	padding Padding = .sha3
}

// new_digest creates an initialized digest structure based on
// the hash size.
//
// absorption_rate is the number of bytes to be absorbed into the
//     sponge per permutation.
//
// hash_size - the number if bytes in the generated hash.
//     Legal values are 224, 256, 384, and 512.
//
// config - the padding setting for hash generation. .sha3 should be used for FIPS PUB 202 compliant SHA3-224, SHA3-256, SHA3-384 and SHA3-512. Use .keccak if you want a legacy Keccak-224, Keccak-256, Keccak-384 or Keccak-512 algorithm. .xof is for extended output functions.
pub fn new_digest(absorption_rate int, hash_size int, config PaddingConfig) !&Digest {
	match config.padding {
		.sha3, .keccak { validate_sha3(absorption_rate, hash_size)! }
		.xof { validate_xof(absorption_rate, hash_size)! }
	}

	d := Digest{
		rate:       absorption_rate
		suffix:     u8(config.padding)
		output_len: hash_size
		s:          State{}
	}

	return &d
}

// new_xof_digest creates an initialized digest structure based on
// the absorption rate and how many bytes of output you need
//
// absorption_rate is the number of bytes to be absorbed into the
//     sponge per permutation.  Legal values are xof_rate_128 and
//     xof_rate_256.
//
// hash_size - the number if bytes in the generated hash.
//     Legal values are positive integers.
pub fn new_xof_digest(absorption_rate int, hash_size int) !&Digest {
	return new_digest(absorption_rate, hash_size, padding: .xof)
}

fn validate_sha3(absorption_rate int, hash_size int) ! {
	match hash_size {
		size_224 {
			if absorption_rate != rate_224 {
				return AbsorptionRateError{
					rate: absorption_rate
					size: hash_size
				}
			}
		}
		size_256 {
			if absorption_rate != rate_256 {
				return AbsorptionRateError{
					rate: absorption_rate
					size: hash_size
				}
			}
		}
		size_384 {
			if absorption_rate != rate_384 {
				return AbsorptionRateError{
					rate: absorption_rate
					size: hash_size
				}
			}
		}
		size_512 {
			if absorption_rate != rate_512 {
				return AbsorptionRateError{
					rate: absorption_rate
					size: hash_size
				}
			}
		}
		else {
			return HashSizeError{
				size: hash_size
			}
		}
	}
}

fn validate_xof(absorption_rate int, hash_size int) ! {
	match absorption_rate {
		xof_rate_128, xof_rate_256 {
			if hash_size < 1 {
				return XOFSizeError{
					size: hash_size
				}
			}
		}
		else {
			return XOFRateError{
				rate: absorption_rate
			}
		}
	}
}

// write adds bytes to the sponge.
//
// This is the absorption phase of the computation.
pub fn (mut d Digest) write(data []u8) ! {
	// if no data is being added to the hash,
	// just return
	if data.len == 0 {
		return
	}

	// absorb the input into the sponge
	mut bytes_remaining := unsafe { data[..] }

	if d.input_buffer.len != 0 {
		// see if we can accumulate rate bytes to be absorbed
		empty_space := d.rate - d.input_buffer.len

		if bytes_remaining.len < empty_space {
			d.input_buffer << bytes_remaining

			// we have not accumulated rate bytes yet.
			// just return.
			return
		} else {
			// we have enough bytes to add rate bytes to the
			// sponge.
			d.input_buffer << bytes_remaining[..empty_space]
			bytes_remaining = unsafe { bytes_remaining[empty_space..] }

			// absorb them
			d.s.xor_bytes(d.input_buffer[..d.rate], d.rate)
			d.s.kaccak_p_1600_24()

			d.input_buffer = ''.bytes()
		}
	}

	// absorb the remaining bytes
	for bytes_remaining.len >= d.rate {
		d.s.xor_bytes(bytes_remaining[..d.rate], d.rate)
		d.s.kaccak_p_1600_24()
		bytes_remaining = unsafe { bytes_remaining[d.rate..] }
	}

	if bytes_remaining.len > 0 {
		d.input_buffer = bytes_remaining
	}
}

// checksum finalizes the hash and returns the generated bytes.
pub fn (mut d Digest) checksum() []u8 {
	return d.checksum_internal() or { panic(err) }
}

fn (mut d Digest) checksum_internal() ![]u8 {
	// pad the last input bytes to have rate bytes
	if d.input_buffer.len == d.rate - 1 {
		// a single byte pad needs to be handled specially
		d.input_buffer << u8(0x80 | d.suffix)
	} else {
		zero_pads := (d.rate - d.input_buffer.len) - 2

		// add the first byte of padding
		d.input_buffer << d.suffix

		// add intermediate zero pad bytes
		for _ in 0 .. zero_pads {
			d.input_buffer << u8(0x00)
		}

		// add the last pad byte
		d.input_buffer << u8(0x80)
	}

	d.s.xor_bytes(d.input_buffer[..d.rate], d.rate)
	d.s.kaccak_p_1600_24()

	// absorption is done.  on to squeezing.

	// We know that we can extract rate bytes from the current
	// state.  If the output_len is <= rate, we don't need to
	// iterate and can just return the bytes from the current
	// state.

	if d.output_len <= d.rate {
		return d.s.to_bytes()[..d.output_len]
	}

	// we need to squeeze the sponge a little harder to get
	// longer strings of bytes.

	mut output_bytes := []u8{cap: d.output_len}
	mut remaining_ouput_len := d.output_len

	for remaining_ouput_len > 0 {
		mut byte_len_this_round := math.min[int](remaining_ouput_len, d.rate)
		output_bytes << d.s.to_bytes()[..byte_len_this_round]

		remaining_ouput_len -= byte_len_this_round

		if remaining_ouput_len > 0 {
			d.s.kaccak_p_1600_24()
		}
	}

	return output_bytes
}

// sum512 returns the sha3 512 bit checksum of the data.
pub fn sum512(data []u8) []u8 {
	mut d := new512() or { panic(err) }
	d.write(data) or { panic(err) }
	return d.checksum_internal() or { panic(err) }
}

// sum384 returns the sha3 384 bit checksum of the data.
pub fn sum384(data []u8) []u8 {
	mut d := new384() or { panic(err) }
	d.write(data) or { panic(err) }
	return d.checksum_internal() or { panic(err) }
}

// sum256 returns the sha3 256 bit checksum of the data.
pub fn sum256(data []u8) []u8 {
	mut d := new256() or { panic(err) }
	d.write(data) or { panic(err) }
	return d.checksum_internal() or { panic(err) }
}

// sum224 returns the sha3 224 bit checksum of the data.
pub fn sum224(data []u8) []u8 {
	mut d := new224() or { panic(err) }
	d.write(data) or { panic(err) }
	return d.checksum_internal() or { panic(err) }
}

// keccak256 returns the keccak 256 bit checksum of the data.
pub fn keccak256(data []u8) []u8 {
	mut d := new256keccak() or { panic(err) }
	d.write(data) or { panic(err) }
	return d.checksum_internal() or { panic(err) }
}

// keccak512 returns the keccak 512 bit checksum of the data.
pub fn keccak512(data []u8) []u8 {
	mut d := new512keccak() or { panic(err) }
	d.write(data) or { panic(err) }
	return d.checksum_internal() or { panic(err) }
}

// shake256 returns the sha3 shake256 bit extended output
pub fn shake256(data []u8, output_len int) []u8 {
	mut d := new256xof(output_len) or { panic(err) }
	d.write(data) or { panic(err) }
	return d.checksum_internal() or { panic(err) }
}

// shake128 returns the sha3 shake128 bit extended output
pub fn shake128(data []u8, output_len int) []u8 {
	mut d := new128xof(output_len) or { panic(err) }
	d.write(data) or { panic(err) }
	return d.checksum_internal() or { panic(err) }
}
