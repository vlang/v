// Copyright (c) 2026 V project. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// AES-CCM (RFC 3610) for TLS_ECDHE_*_WITH_AES_128_CCM_8 (RFC 6655).
// Pure-V: built on crypto.aes block cipher only. Fixed parameters for this
// profile: 16-byte key, 12-byte nonce (Q=3), 8-byte tag (T=8).
module tls12

import crypto.aes
import crypto.subtle

// ccm_tag_len is the TLS CCM-8 authentication tag length (RFC 6655).
const ccm_tag_len = 8

// ccm_nonce_len is the CCM nonce length used by TLS (salt + explicit).
const ccm_nonce_len = 12

// aes_encrypt_block encrypts one 16-byte block with key.
fn aes_encrypt_block(key []u8, blk []u8) ![]u8 {
	block := aes.new_cipher(key)!
	mut out := []u8{len: 16}
	block.encrypt(mut out, blk)
	return out
}

// ccm_cbc_mac computes the raw CCM CBC-MAC (T bytes) over nonce/aad/plain.
fn ccm_cbc_mac(key []u8, nonce []u8, aad []u8, plaintext []u8) ![]u8 {
	if nonce.len != ccm_nonce_len {
		return error('tls12: bad CCM nonce length')
	}
	q := 15 - nonce.len
	mut flags := u8((q - 1) & 0x07)
	flags |= u8(((ccm_tag_len - 2) / 2) << 3)
	has_aad := aad.len > 0
	if has_aad {
		flags |= 0x40
	}
	mut b0 := [flags]
	b0 << nonce
	for i := q - 1; i >= 0; i-- {
		b0 << u8((u64(plaintext.len) >> u32(i * 8)) & 0xff)
	}
	mut chain := aes_encrypt_block(key, b0)!
	if has_aad {
		mut ablk := []u8{}
		ablk << u8(u16(aad.len) >> 8)
		ablk << u8(u16(aad.len) & 0xff)
		ablk << aad
		for ablk.len % 16 != 0 {
			ablk << u8(0)
		}
		for off := 0; off < ablk.len; off += 16 {
			mut x := []u8{len: 16}
			for i in 0 .. 16 {
				x[i] = chain[i] ^ ablk[off + i]
			}
			chain = aes_encrypt_block(key, x)!
		}
	}
	if plaintext.len > 0 {
		mut pblk := []u8{cap: (plaintext.len + 15) / 16 * 16}
		pblk << plaintext
		for pblk.len % 16 != 0 {
			pblk << u8(0)
		}
		for off2 := 0; off2 < pblk.len; off2 += 16 {
			mut x := []u8{len: 16}
			for i in 0 .. 16 {
				x[i] = chain[i] ^ pblk[off2 + i]
			}
			chain = aes_encrypt_block(key, x)!
		}
	}
	return chain[..ccm_tag_len].clone()
}

// ccm_ctr crypts data with CCM counter mode starting at counter 1.
fn ccm_ctr(key []u8, nonce []u8, data []u8) ![]u8 {
	if nonce.len != ccm_nonce_len {
		return error('tls12: bad CCM nonce length')
	}
	q := 15 - nonce.len
	mut out := []u8{cap: data.len}
	mut counter := u64(1)
	mut off := 0
	for off < data.len {
		mut a := [u8(q - 1) & 0x07]
		a << nonce
		for i := q - 1; i >= 0; i-- {
			a << u8((counter >> u32(i * 8)) & 0xff)
		}
		s := aes_encrypt_block(key, a)!
		n := if data.len - off < 16 { data.len - off } else { 16 }
		for i in 0 .. n {
			out << (data[off + i] ^ s[i])
		}
		off += n
		counter++
	}
	return out
}

// ccm_tag_mask computes S0 (counter 0 keystream) for tag masking.
fn ccm_tag_mask(key []u8, nonce []u8) ![]u8 {
	if nonce.len != ccm_nonce_len {
		return error('tls12: bad CCM nonce length')
	}
	q := 15 - nonce.len
	mut a0 := [u8(q - 1) & 0x07]
	a0 << nonce
	for _ in 0 .. q {
		a0 << u8(0)
	}
	return aes_encrypt_block(key, a0)
}

// ccm_encrypt encrypts plaintext, returning ciphertext || tag.
fn ccm_encrypt(key []u8, nonce []u8, aad []u8, plaintext []u8) ![]u8 {
	mac := ccm_cbc_mac(key, nonce, aad, plaintext)!
	mask := ccm_tag_mask(key, nonce)!
	mut tag := []u8{cap: ccm_tag_len}
	for i in 0 .. ccm_tag_len {
		tag << (mac[i] ^ mask[i])
	}
	ct := ccm_ctr(key, nonce, plaintext)!
	mut out := ct.clone()
	out << tag
	return out
}

// ccm_decrypt verifies the trailing tag and returns the plaintext.
fn ccm_decrypt(key []u8, nonce []u8, aad []u8, sealed []u8) ![]u8 {
	if sealed.len < ccm_tag_len {
		return error('tls12: short CCM record')
	}
	ct := sealed[..sealed.len - ccm_tag_len].clone()
	recv_tag := sealed[sealed.len - ccm_tag_len..].clone()
	pt := ccm_ctr(key, nonce, ct)!
	mac := ccm_cbc_mac(key, nonce, aad, pt)!
	mask := ccm_tag_mask(key, nonce)!
	mut want := []u8{cap: ccm_tag_len}
	for i in 0 .. ccm_tag_len {
		want << (mac[i] ^ mask[i])
	}
	if subtle.constant_time_compare(recv_tag, want) != 1 {
		return error('tls12: bad CCM tag')
	}
	return pt
}
