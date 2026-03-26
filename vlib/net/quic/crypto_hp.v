module quic

import crypto.aes

// QUIC header protection per RFC 9001 §5.4.

// apply_header_protection applies QUIC header protection per RFC 9001 §5.4.1.
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

// remove_header_protection removes QUIC header protection per RFC 9001 §5.4.1.
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

fn aes_ecb_encrypt(key []u8, block []u8) ![]u8 {
	if block.len != aes.block_size {
		return error('aes_ecb_encrypt: block must be exactly ${aes.block_size} bytes')
	}
	cipher_block := aes.new_cipher(key)
	mut dst := []u8{len: aes.block_size}
	cipher_block.encrypt(mut dst, block)
	return dst
}

fn apply_hp_mask(header []u8, mask []u8) ![]u8 {
	if header.len == 0 {
		return error('header must not be empty')
	}
	mut protected := header.clone()
	is_long_header := (protected[0] & 0x80) != 0

	if is_long_header {
		protected[0] ^= mask[0] & 0x0f
	} else {
		protected[0] ^= mask[0] & 0x1f
	}

	pn_len := int(protected[0] & 0x03) + 1
	pn_offset := header.len - pn_len
	for i in 0 .. pn_len {
		if pn_offset + i < protected.len && i + 1 < mask.len {
			protected[pn_offset + i] ^= mask[i + 1]
		}
	}

	return protected
}

// compute_pn_offset computes the packet number offset for the given QUIC packet.
// Returns the byte offset and whether the packet uses a long header.
fn compute_pn_offset(packet []u8, dcid_len int) !(int, bool) {
	first_byte := packet[0]
	is_long := (first_byte & 0x80) != 0

	if is_long {
		if packet.len < 6 {
			return error('long header too short')
		}
		dcid_l := int(packet[5])
		if packet.len < 7 + dcid_l {
			return error('long header too short for DCID')
		}
		scid_l := int(packet[6 + dcid_l])
		return 7 + dcid_l + scid_l, true
	}

	return 1 + dcid_len, false
}

// extract_and_unprotect_pn removes header protection and extracts the packet number.
pub fn (ctx CryptoContext) extract_and_unprotect_pn(packet []u8, dcid_len int) !(u64, int, []u8) {
	if ctx.rx_hp_key.len == 0 {
		return error('rx_hp_key is empty: header protection keys not derived')
	}
	if packet.len == 0 {
		return error('packet data is empty')
	}

	pn_offset, is_long := compute_pn_offset(packet, dcid_len)!

	// HP sample: 16 bytes at pn_offset + 4 (RFC 9001 §5.4.2)
	sample_offset := pn_offset + 4
	if sample_offset + aes.block_size > packet.len {
		return error('packet too short for HP sample')
	}
	sample := packet[sample_offset..sample_offset + aes.block_size]

	// AES-ECB(rx_hp_key, sample) per RFC 9001 §5.4.1
	mask := aes_ecb_encrypt(ctx.rx_hp_key, sample)!

	mut unprotected_byte0 := packet[0]
	if is_long {
		unprotected_byte0 ^= mask[0] & 0x0f
	} else {
		unprotected_byte0 ^= mask[0] & 0x1f
	}
	pn_len := int(unprotected_byte0 & 0x03) + 1

	if pn_offset + pn_len > packet.len {
		return error('packet too short for packet number')
	}

	// Build unprotected header
	header_end := pn_offset + pn_len
	mut header := packet[..header_end].clone()
	header[0] = unprotected_byte0
	for i in 0 .. pn_len {
		header[pn_offset + i] ^= mask[1 + i]
	}

	mut pn := u64(0)
	for i in 0 .. pn_len {
		pn = (pn << 8) | u64(header[pn_offset + i])
	}

	return pn, pn_len, header
}

// extract_packet_number extracts the packet number from an unprotected QUIC header.
pub fn extract_packet_number(data []u8, dcid_len int) !(u64, int) {
	if data.len == 0 {
		return error('packet data is empty')
	}

	first_byte := data[0]
	pn_len := int(first_byte & 0x03) + 1
	is_long := (first_byte & 0x80) != 0

	mut pn_offset := 0
	if is_long {
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
