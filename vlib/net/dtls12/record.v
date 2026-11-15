// Copyright (c) 2026 V project. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// DTLS 1.2 record layer (RFC 6347 minimal, epoch 0/1).
// File-disjoint from net.tls and net.tls12: own DTLS header, own replay/RTO
// logic and own GCM/CBC protection with DTLS additional data. Pure-V.
module dtls12

import crypto.aes
import crypto.cipher
import crypto.hmac
import crypto.rand
import crypto.sha256
import crypto.subtle
import net
import time

// dtls12_legacy_version is the on-wire legacy version for DTLS datagrams.
pub const dtls12_legacy_version = u16(0xfefd)

// dtls12_header_len is 1 (type) + 2 (version) + 2 (epoch) + 6 (seq) + 2 (len).
pub const dtls12_header_len = 13

// dtls12_max_fragment caps a single DTLS fragment.
pub const dtls12_max_fragment = 2048

// dtls12_max_datagram caps a UDP datagram we accept.
pub const dtls12_max_datagram = 4096

// dtls12_epoch_handshake carries plaintext handshake flights.
pub const dtls12_epoch_handshake = u16(0)

// dtls12_epoch_application carries encrypted application data.
pub const dtls12_epoch_application = u16(1)

// ContentType mirrors the TLS content types without importing net.tls.
pub enum ContentType {
	change_cipher_spec = 20
	alert              = 21
	handshake          = 22
	application_data   = 23
}

// Dtls12Record is one DTLS datagram payload (minimal RFC 6347 §4 header).
pub struct Dtls12Record {
pub:
	content_type    ContentType
	epoch           u16
	sequence_number u64
	fragment        []u8
}

// encode_dtls12_record encodes a record header plus fragment.
pub fn encode_dtls12_record(rec Dtls12Record) ![]u8 {
	if rec.sequence_number >= (u64(1) << 48) {
		return error('dtls12: sequence number too large')
	}
	if rec.fragment.len > dtls12_max_fragment {
		return error('dtls12: fragment too large')
	}
	mut out := []u8{cap: dtls12_header_len + rec.fragment.len}
	out << u8(rec.content_type)
	out << u8(dtls12_legacy_version >> 8)
	out << u8(dtls12_legacy_version & 0xff)
	out << u8(rec.epoch >> 8)
	out << u8(rec.epoch & 0xff)
	out << u8((rec.sequence_number >> 40) & 0xff)
	out << u8((rec.sequence_number >> 32) & 0xff)
	out << u8((rec.sequence_number >> 24) & 0xff)
	out << u8((rec.sequence_number >> 16) & 0xff)
	out << u8((rec.sequence_number >> 8) & 0xff)
	out << u8(rec.sequence_number & 0xff)
	out << u8(u16(rec.fragment.len) >> 8)
	out << u8(u16(rec.fragment.len) & 0xff)
	out << rec.fragment
	return out
}

// decode_dtls12_record decodes one datagram into a record.
pub fn decode_dtls12_record(data []u8) !Dtls12Record {
	if data.len < dtls12_header_len {
		return error('dtls12: truncated record header')
	}
	ct := match data[0] {
		20 { ContentType.change_cipher_spec }
		21 { ContentType.alert }
		22 { ContentType.handshake }
		23 { ContentType.application_data }
		else { return error('dtls12: invalid outer content type') }
	}
	ver := (u16(data[1]) << 8) | u16(data[2])
	if ver != dtls12_legacy_version {
		return error('dtls12: invalid legacy version')
	}
	epoch := (u16(data[3]) << 8) | u16(data[4])
	mut seq := u64(0)
	seq |= u64(data[5]) << 40
	seq |= u64(data[6]) << 32
	seq |= u64(data[7]) << 24
	seq |= u64(data[8]) << 16
	seq |= u64(data[9]) << 8
	seq |= u64(data[10])
	ln := (int(data[11]) << 8) | int(data[12])
	if ln > dtls12_max_fragment {
		return error('dtls12: fragment too large')
	}
	if dtls12_header_len + ln != data.len {
		return error('dtls12: length mismatch')
	}
	frag := data[dtls12_header_len..].clone()
	return Dtls12Record{
		content_type:    ct
		epoch:           epoch
		sequence_number: seq
		fragment:        frag
	}
}

// ReplayWindow is a 64-packet sliding window for one epoch.
pub struct ReplayWindow {
mut:
	highest  u64
	mask     u64
	seen_any bool
}

// is_replay reports whether seq was already seen or is too old.
pub fn (w &ReplayWindow) is_replay(seq u64) bool {
	if !w.seen_any {
		return false
	}
	if seq > w.highest {
		return false
	}
	diff := w.highest - seq
	if diff >= 64 {
		return true
	}
	return ((w.mask >> diff) & u64(1)) == u64(1)
}

// mark records seq, returning false on replay or too-old.
pub fn (mut w ReplayWindow) mark(seq u64) bool {
	if !w.seen_any {
		w.highest = seq
		w.mask = u64(1)
		w.seen_any = true
		return true
	}
	if seq > w.highest {
		shift := seq - w.highest
		if shift >= 64 {
			w.mask = u64(1)
		} else {
			w.mask = (w.mask << shift) | u64(1)
		}
		w.highest = seq
		return true
	}
	diff := w.highest - seq
	if diff >= 64 {
		return false
	}
	bit := u64(1) << diff
	if w.mask & bit != u64(0) {
		return false
	}
	w.mask |= bit
	return true
}

// reset clears the window (used when entering a new epoch).
pub fn (mut w ReplayWindow) reset() {
	w.highest = 0
	w.mask = 0
	w.seen_any = false
}

// RetransmitTimer tracks exponential-backoff timeouts (RFC 6347 §4.2.4).
pub struct RetransmitTimer {
pub mut:
	initial_ms int
	max_ms     int
	attempts   int
}

// new_retransmit_timer builds a timer with the given bounds.
pub fn new_retransmit_timer(initial_ms int, max_ms int) RetransmitTimer {
	return RetransmitTimer{
		initial_ms: initial_ms
		max_ms:     max_ms
	}
}

// backoff_for_attempt returns min(initial * 2^attempt, max).
pub fn backoff_for_attempt(initial_ms int, max_ms int, attempt int) int {
	mut v := initial_ms
	for _ in 0 .. attempt {
		v *= 2
		if v >= max_ms {
			return max_ms
		}
	}
	if v > max_ms {
		return max_ms
	}
	return v
}

// next_timeout returns the current RTO and advances the attempt counter.
pub fn (mut t RetransmitTimer) next_timeout() time.Duration {
	ms := backoff_for_attempt(t.initial_ms, t.max_ms, t.attempts)
	t.attempts++
	return time.Duration(i64(ms) * 1_000_000)
}

// reset clears the attempt counter after progress.
pub fn (mut t RetransmitTimer) reset() {
	t.attempts = 0
}

// is_timeout reports whether err is a UDP read timeout.
pub fn is_timeout(err IError) bool {
	return err.code() == net.err_timed_out_code
}

// dtls12_aad builds the 13-byte AEAD additional data for DTLS 1.2:
// epoch+seq (8) || type || version (DTLS 1.2) || length. The version here is
// the TLS 1.2 version (0x0303); the record header carries the DTLS legacy
// version (0xfefd) on the wire.
fn dtls12_aad(epoch u16, seq u64, typ u8, payload_len int) []u8 {
	mut ad := []u8{cap: 13}
	ad << u8(epoch >> 8)
	ad << u8(epoch & 0xff)
	// 6-byte sequence number big-endian.
	for i := 5; i >= 0; i-- {
		ad << u8((seq >> u32(i * 8)) & 0xff)
	}
	ad << typ
	ad << u8(0x03)
	ad << u8(0x03)
	ad << u8(u16(payload_len) >> 8)
	ad << u8(u16(payload_len) & 0xff)
	return ad
}

// gcm_seal encrypts plaintext as explicit_nonce(8) || ciphertext || tag.
fn gcm_seal(key []u8, salt []u8, epoch u16, seq u64, typ u8, plaintext []u8) ![]u8 {
	g := aes.new_aes_gcm(key)!
	mut nonce := []u8{cap: 12}
	nonce << salt
	for i := 7; i >= 0; i-- {
		nonce << u8((seq >> u32(i * 8)) & 0xff)
	}
	ad := dtls12_aad(epoch, seq, typ, plaintext.len)
	ct := g.encrypt(plaintext, nonce, ad)!
	explicit := nonce[4..].clone()
	mut out := []u8{cap: 8 + ct.len}
	out << explicit
	out << ct
	return out
}

// gcm_open decrypts a sealed record payload; verifies the tag.
fn gcm_open(key []u8, salt []u8, epoch u16, seq u64, typ u8, sealed []u8) ![]u8 {
	if sealed.len < 8 + 16 {
		return error('dtls12: short GCM record')
	}
	explicit := sealed[..8]
	mut nonce := []u8{cap: 12}
	nonce << salt
	nonce << explicit
	ad := dtls12_aad(epoch, seq, typ, sealed.len - 8 - 16)
	g := aes.new_aes_gcm(key)!
	return g.decrypt(sealed[8..], nonce, ad)
}

// cbc_mac computes HMAC-SHA256 over epoch+seq || type || version || len || plaintext.
fn cbc_mac(mac_key []u8, epoch u16, seq u64, typ u8, plaintext []u8) []u8 {
	mut data := []u8{cap: 8 + 1 + 2 + 2 + plaintext.len}
	data << u8(epoch >> 8)
	data << u8(epoch & 0xff)
	for i := 5; i >= 0; i-- {
		data << u8((seq >> u32(i * 8)) & 0xff)
	}
	data << typ
	data << u8(0x03)
	data << u8(0x03)
	data << u8(u16(plaintext.len) >> 8)
	data << u8(u16(plaintext.len) & 0xff)
	data << plaintext
	return hmac.new(mac_key, data, sha256.sum256, sha256.block_size)
}

// cbc_seal encrypts as explicit_iv(16) || CBC(content || MAC || padding).
fn cbc_seal(mac_key []u8, enc_key []u8, epoch u16, seq u64, typ u8, plaintext []u8) ![]u8 {
	mac := cbc_mac(mac_key, epoch, seq, typ, plaintext)
	mut content := []u8{cap: plaintext.len + mac.len + 16}
	content << plaintext
	content << mac
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
fn cbc_open(mac_key []u8, enc_key []u8, epoch u16, seq u64, typ u8, sealed []u8) ![]u8 {
	if sealed.len < 32 || sealed.len % 16 != 0 {
		return error('dtls12: bad CBC record length')
	}
	iv := sealed[..16]
	block := aes.new_cipher(enc_key)!
	content := cbc_crypt(block, iv, sealed[16..], false)!
	if content.len == 0 {
		return error('dtls12: bad CBC padding')
	}
	pad_len := int(content.last())
	if content.len < pad_len + 1 + 32 {
		return error('dtls12: bad CBC padding')
	}
	mut bad := 0
	for i in 0 .. pad_len + 1 {
		bad |= int(content[content.len - 1 - i]) ^ pad_len
	}
	mut mac_ok := 0
	if bad == 0 {
		pt_len := content.len - pad_len - 1 - 32
		plaintext := content[..pt_len]
		recv_mac := content[pt_len..pt_len + 32]
		want_mac := cbc_mac(mac_key, epoch, seq, typ, plaintext)
		mac_ok = subtle.constant_time_compare(recv_mac, want_mac)
	}
	if bad != 0 || mac_ok != 1 {
		return error('dtls12: CBC MAC/padding check failed')
	}
	pt_len := content.len - pad_len - 1 - 32
	return content[..pt_len].clone()
}

// cbc_crypt runs manual CBC encrypt/decrypt with an explicit IV.
fn cbc_crypt(block cipher.Block, iv []u8, data []u8, encrypt bool) ![]u8 {
	if data.len % 16 != 0 {
		return error('dtls12: CBC input not block-aligned')
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
