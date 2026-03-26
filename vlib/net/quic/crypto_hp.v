// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module quic

import crypto.aes

// QUIC header protection per RFC 9001 Section 5.4.
// Uses AES-ECB to derive the mask and applies it to the header.

// apply_header_protection applies QUIC header protection per RFC 9001 Section 5.4.1.
// For AES-based ciphers, the mask is produced by AES-ECB encryption of the sample
// using the header protection key (hp_key). The mask is applied as follows:
//   - mask[0] is applied to the first header byte (packet number length bits / key phase bit)
//   - mask[1..5] are XORed onto the packet number bytes
// The hp_key and sample must each be 16 bytes (AES block size).
pub fn (ctx CryptoContext) apply_header_protection(header []u8, sample []u8) ![]u8 {
	if sample.len < aes.block_size {
		return error('sample must be at least ${aes.block_size} bytes')
	}
	if ctx.tx_hp_key.len != aes.block_size {
		return error('tx_hp_key must be ${aes.block_size} bytes')
	}

	// Compute mask = AES-ECB(hp_key, sample[0..16]) per RFC 9001 §5.4.1
	mask := aes_ecb_encrypt(ctx.tx_hp_key, sample[..aes.block_size])!

	return apply_hp_mask(header, mask)
}

// remove_header_protection removes QUIC header protection per RFC 9001 Section 5.4.1.
// Uses the rx_hp_key. The mask derivation is identical to apply_header_protection;
// header protection is self-inverse when the same mask is applied twice.
pub fn (ctx CryptoContext) remove_header_protection(header []u8, sample []u8) ![]u8 {
	if sample.len < aes.block_size {
		return error('sample must be at least ${aes.block_size} bytes')
	}
	if ctx.rx_hp_key.len != aes.block_size {
		return error('rx_hp_key must be ${aes.block_size} bytes')
	}

	// Compute mask = AES-ECB(hp_key, sample[0..16]) per RFC 9001 §5.4.1
	mask := aes_ecb_encrypt(ctx.rx_hp_key, sample[..aes.block_size])!

	return apply_hp_mask(header, mask)
}

// aes_ecb_encrypt encrypts a single 16-byte block using AES-ECB.
// This is the core primitive for QUIC header protection per RFC 9001 §5.4.1.
fn aes_ecb_encrypt(key []u8, block []u8) ![]u8 {
	if block.len != aes.block_size {
		return error('aes_ecb_encrypt: block must be exactly ${aes.block_size} bytes')
	}
	cipher_block := aes.new_cipher(key)
	mut dst := []u8{len: aes.block_size}
	cipher_block.encrypt(mut dst, block)
	return dst
}

// apply_hp_mask applies the header protection mask per RFC 9001 §5.4.
// mask[0] is applied to first-byte bits; mask[1..5] XOR the packet number bytes.
fn apply_hp_mask(header []u8, mask []u8) ![]u8 {
	if header.len == 0 {
		return error('header must not be empty')
	}
	mut protected := header.clone()

	// Determine long vs short header from the high bit of the first byte
	is_long_header := (protected[0] & 0x80) != 0

	if is_long_header {
		// Long header: mask the low 4 bits of the first byte (packet number length)
		protected[0] ^= mask[0] & 0x0f
	} else {
		// Short header: mask the low 5 bits (key phase + packet number length bits)
		protected[0] ^= mask[0] & 0x1f
	}

	// XOR mask[1..5] onto the packet number bytes (last 4 bytes of header prefix)
	// Packet number starts right after the first byte in the simplest case;
	// for a full implementation the caller is responsible for passing the correct slice.
	pn_len := int(protected[0] & 0x03) + 1 // encoded packet number length
	pn_offset := header.len - pn_len
	for i in 0 .. pn_len {
		if pn_offset + i < protected.len && i + 1 < mask.len {
			protected[pn_offset + i] ^= mask[i + 1]
		}
	}

	return protected
}

// extract_and_unprotect_pn removes header protection and extracts the packet number.
// Per RFC 9001 §5.4, header protection must be removed before the packet number
// can be read. This function implements the full pipeline:
//   1. Determine PN offset from the header structure
//   2. Get the HP sample (16 bytes at pn_offset + 4, per RFC 9001 §5.4.2)
//   3. Compute the HP mask via AES-ECB(rx_hp_key, sample)
//   4. Unmask byte 0 to determine PN length, then unmask PN bytes
//   5. Return (packet_number, pn_length, unprotected_header)
// Returns an error when rx_hp_key is not set or the packet is too short.
pub fn (ctx CryptoContext) extract_and_unprotect_pn(packet []u8, dcid_len int) !(u64, int, []u8) {
	if ctx.rx_hp_key.len == 0 {
		return error('rx_hp_key is empty: header protection keys not derived')
	}
	if packet.len == 0 {
		return error('packet data is empty')
	}

	first_byte := packet[0]
	is_long := (first_byte & 0x80) != 0

	// 1. Determine PN offset (same logic as extract_packet_number)
	mut pn_offset := 0
	if is_long {
		if packet.len < 6 {
			return error('long header too short')
		}
		dcid_l := int(packet[5])
		if packet.len < 7 + dcid_l {
			return error('long header too short for DCID')
		}
		scid_l := int(packet[6 + dcid_l])
		pn_offset = 7 + dcid_l + scid_l
	} else {
		pn_offset = 1 + dcid_len
	}

	// 2. HP sample: 16 bytes starting at pn_offset + 4 (RFC 9001 §5.4.2)
	sample_offset := pn_offset + 4
	if sample_offset + aes.block_size > packet.len {
		return error('packet too short for HP sample')
	}
	sample := packet[sample_offset..sample_offset + aes.block_size]

	// 3. Compute mask = AES-ECB(rx_hp_key, sample) per RFC 9001 §5.4.1
	mask := aes_ecb_encrypt(ctx.rx_hp_key, sample)!

	// 4. Unmask byte 0 to determine PN length
	mut unprotected_byte0 := first_byte
	if is_long {
		unprotected_byte0 ^= mask[0] & 0x0f
	} else {
		unprotected_byte0 ^= mask[0] & 0x1f
	}
	pn_len := int(unprotected_byte0 & 0x03) + 1

	if pn_offset + pn_len > packet.len {
		return error('packet too short for packet number')
	}

	// 5. Build unprotected header: byte0 + bytes[1..pn_offset] + unmasked PN
	header_end := pn_offset + pn_len
	mut header := packet[..header_end].clone()
	header[0] = unprotected_byte0
	for i in 0 .. pn_len {
		header[pn_offset + i] ^= mask[1 + i]
	}

	// 6. Extract PN value from unprotected bytes
	mut pn := u64(0)
	for i in 0 .. pn_len {
		pn = (pn << 8) | u64(header[pn_offset + i])
	}

	return pn, pn_len, header
}

// extract_packet_number extracts the packet number from an already-unprotected
// QUIC packet header. If header protection has not been removed yet, use
// extract_and_unprotect_pn() instead.
// dcid_len is required for short headers (the DCID length is not encoded
// in the short header itself). Returns (packet_number, pn_length).
pub fn extract_packet_number(data []u8, dcid_len int) !(u64, int) {
	if data.len == 0 {
		return error('packet data is empty')
	}

	first_byte := data[0]
	pn_len := int(first_byte & 0x03) + 1
	is_long := (first_byte & 0x80) != 0

	mut pn_offset := 0
	if is_long {
		// Long header: [1:first][4:version][1:dcid_len][dcid][1:scid_len][scid][pn]
		if data.len < 6 {
			return error('long header too short')
		}
		dcid_l := int(data[5])
		if data.len < 7 + dcid_l {
			return error('long header too short for DCID')
		}
		scid_l := int(data[6 + dcid_l])
		pn_offset = 7 + dcid_l + scid_l
	} else {
		// Short header: [1:first][dcid][pn]
		pn_offset = 1 + dcid_len
	}

	if pn_offset + pn_len > data.len {
		return error('packet too short for packet number')
	}

	mut pn := u64(0)
	for i in 0 .. pn_len {
		pn = (pn << 8) | u64(data[pn_offset + i])
	}

	return pn, pn_len
}
