// Copyright (c) 2026 V project. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// TLS 1.2 record layer (RFC 5246 §6): framing plus AES-GCM (RFC 5288)
// and AES-CBC with HMAC-SHA256 (RFC 5289-style) protection.
// Pure-V: crypto.aes, crypto.hmac, crypto.sha256. No C code.
module tls12

import crypto.aes
import crypto.cipher
import crypto.hmac
import crypto.rand
import crypto.sha256
import crypto.subtle
import net

// Record content types (RFC 5246 §6.2.1).
const rectype_change_cipher_spec = u8(20)
const rectype_alert = u8(21)
const rectype_handshake = u8(22)
const rectype_application_data = u8(23)

// max_record_payload caps a single record plaintext (2^14 per RFC).
const max_record_payload = 16384

// read_exact reads exactly n bytes from tcp (looping over short reads).
fn read_exact(mut tcp net.TcpConn, n int) ![]u8 {
	mut out := []u8{cap: n}
	mut buf := []u8{len: if n < 4096 { n } else { 4096 }}
	for out.len < n {
		want := if n - out.len < buf.len { n - out.len } else { buf.len }
		got := tcp.read(mut buf[..want])!
		if got <= 0 {
			return error('tls12: connection closed while reading')
		}
		out << buf[..got]
	}
	return out
}

// read_record reads one TLS record; returns (content_type, payload).
fn read_record(mut tcp net.TcpConn) !(u8, []u8) {
	hdr := read_exact(mut tcp, 5)!
	typ := hdr[0]
	ver := (u16(hdr[1]) << 8) | u16(hdr[2])
	ln := (int(hdr[3]) << 8) | int(hdr[4])
	if ver < 0x0301 || ver > 0x0303 {
		return error('tls12: bad record version')
	}
	if ln < 0 || ln > max_record_payload + 2048 {
		return error('tls12: bad record length')
	}
	body := read_exact(mut tcp, ln)!
	return typ, body
}

// write_record writes one TLS record.
fn write_record(mut tcp net.TcpConn, typ u8, payload []u8) ! {
	mut hdr := []u8{cap: 5}
	hdr << typ
	hdr << u8(version_tls12 >> 8)
	hdr << u8(version_tls12 & 0xff)
	hdr << u8(u16(payload.len) >> 8)
	hdr << u8(u16(payload.len) & 0xff)
	hdr << payload
	mut off := 0
	for off < hdr.len {
		n := tcp.write(hdr[off..])!
		if n <= 0 {
			return error('tls12: connection closed while writing')
		}
		off += n
	}
}

// gcm_additional_data builds the 13-byte AEAD additional data (RFC 5288 §3).
fn gcm_additional_data(seq u64, typ u8, payload_len int) []u8 {
	mut ad := []u8{cap: 13}
	for i := 7; i >= 0; i-- {
		ad << u8((seq >> u32(i * 8)) & 0xff)
	}
	ad << typ
	ad << u8(version_tls12 >> 8)
	ad << u8(version_tls12 & 0xff)
	ad << u8(u16(payload_len) >> 8)
	ad << u8(u16(payload_len) & 0xff)
	return ad
}

// gcm_seal encrypts plaintext as explicit_nonce(8) || ciphertext || tag.
fn gcm_seal(key []u8, salt []u8, seq u64, typ u8, plaintext []u8) ![]u8 {
	g := aes.new_aes_gcm(key)!
	mut nonce := []u8{cap: 12}
	nonce << salt
	for i := 7; i >= 0; i-- {
		nonce << u8((seq >> u32(i * 8)) & 0xff)
	}
	ad := gcm_additional_data(seq, typ, plaintext.len)
	ct := g.encrypt(plaintext, nonce, ad)!
	explicit := nonce[4..].clone()
	mut out := []u8{cap: 8 + ct.len}
	out << explicit
	out << ct
	return out
}

// gcm_open decrypts a sealed record payload; verifies the tag.
fn gcm_open(key []u8, salt []u8, seq u64, typ u8, sealed []u8) ![]u8 {
	if sealed.len < 8 + 16 {
		return error('tls12: short GCM record')
	}
	explicit := sealed[..8]
	mut nonce := []u8{cap: 12}
	nonce << salt
	nonce << explicit
	ad := gcm_additional_data(seq, typ, sealed.len - 8 - 16)
	g := aes.new_aes_gcm(key)!
	return g.decrypt(sealed[8..], nonce, ad)
}

// cbc_mac computes HMAC-SHA256 over seq || type || version || len || plaintext.
fn cbc_mac(mac_key []u8, seq u64, typ u8, plaintext []u8) []u8 {
	mut data := []u8{cap: 8 + 1 + 2 + 2 + plaintext.len}
	for i := 7; i >= 0; i-- {
		data << u8((seq >> u32(i * 8)) & 0xff)
	}
	data << typ
	data << u8(version_tls12 >> 8)
	data << u8(version_tls12 & 0xff)
	data << u8(u16(plaintext.len) >> 8)
	data << u8(u16(plaintext.len) & 0xff)
	data << plaintext
	return hmac.new(mac_key, data, sha256.sum256, sha256.block_size)
}

// cbc_seal encrypts as explicit_iv(16) || CBC(content || MAC || padding).
fn cbc_seal(mac_key []u8, enc_key []u8, seq u64, typ u8, plaintext []u8) ![]u8 {
	mac := cbc_mac(mac_key, seq, typ, plaintext)
	mut content := []u8{cap: plaintext.len + mac.len + 16}
	content << plaintext
	content << mac
	// TLS padding: pad_len+1 copies of pad_len (always 1..16 bytes added).
	pad_len := (16 - (content.len % 16) - 1) % 16
	for _ in 0 .. pad_len + 1 {
		content << u8(pad_len)
	}
	iv := rand.bytes(16)!
	block := aes.new_cipher(enc_key)!
	ct := cbc_crypt(block, iv, content, true)!
	mut sealed := []u8{cap: 16 + ct.len}
	sealed << iv
	sealed << ct
	return sealed
}

// cbc_open decrypts and verifies MAC/padding.
fn cbc_open(mac_key []u8, enc_key []u8, seq u64, typ u8, sealed []u8) ![]u8 {
	if sealed.len < 32 || sealed.len % 16 != 0 {
		return error('tls12: bad CBC record length')
	}
	iv := sealed[..16]
	block := aes.new_cipher(enc_key)!
	content := cbc_crypt(block, iv, sealed[16..], false)!
	if content.len == 0 {
		return error('tls12: bad CBC padding')
	}
	pad_len := int(content.last())
	if content.len < pad_len + 1 + 32 {
		return error('tls12: bad CBC padding')
	}
	// Check all padding bytes (accumulate, compare at end).
	mut bad := 0
	for i in 0 .. pad_len + 1 {
		bad |= int(content[content.len - 1 - i]) ^ pad_len
	}
	// Split plaintext and MAC without branching on secret length.
	mut mac_ok := 0
	if bad == 0 {
		pt_len := content.len - pad_len - 1 - 32
		plaintext := content[..pt_len]
		recv_mac := content[pt_len..pt_len + 32]
		want_mac := cbc_mac(mac_key, seq, typ, plaintext)
		mac_ok = subtle.constant_time_compare(recv_mac, want_mac)
	}
	if bad != 0 || mac_ok != 1 {
		return error('tls12: CBC MAC/padding check failed')
	}
	pt_len := content.len - pad_len - 1 - 32
	return content[..pt_len].clone()
}

// cbc_crypt runs manual CBC encrypt (true) or decrypt (false) with an
// explicit IV. crypto.cipher's Cbc type is module-private, so the chaining
// is done here directly on the block cipher.
fn cbc_crypt(block cipher.Block, iv []u8, data []u8, encrypt bool) ![]u8 {
	if data.len % 16 != 0 {
		return error('tls12: CBC input not block-aligned')
	}
	mut out := []u8{len: data.len}
	mut prev := iv.clone()
	mut blk_in := []u8{len: 16}
	mut blk_out := []u8{len: 16}
	if encrypt {
		for off := 0; off < data.len; off += 16 {
			for i in 0 .. 16 {
				blk_in[i] = data[off + i] ^ prev[i]
			}
			block.encrypt(mut blk_out, blk_in)
			for i in 0 .. 16 {
				out[off + i] = blk_out[i]
				prev[i] = blk_out[i]
			}
		}
	} else {
		for off := 0; off < data.len; off += 16 {
			cur := data[off..off + 16]
			block.decrypt(mut blk_out, cur)
			for i in 0 .. 16 {
				out[off + i] = blk_out[i] ^ prev[i]
				prev[i] = cur[i]
			}
		}
	}
	return out
}
