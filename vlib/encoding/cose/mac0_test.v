// Tests for COSE_Mac0. HMAC tags are deterministic, so every test can
// match the reference vector bytes-exactly.
module cose

import encoding.base64
import encoding.hex

// HMAC-enc-01.json from cose-wg/Examples (Unlicense): HS256 mac0,
// implicit "direct" recipient, empty unprotected, no external AAD.
const hmac_hs256_key_b64u = 'hJtXIZ2uSN5kbQfbtTNWbpdmhkV8FJG-Onbc6mxCcYg'
const hmac_enc01_message = 'D18443A10105A054546869732069732074686520636F6E74656E742E5820A1A848D3471F9D61EE49018D244C824772F223AD4F935293F1789FC3A08D8C58'

const sample_text = 'This is the content.'

fn test_mac0_hs256_matches_reference_vector() {
	k := base64.url_decode(hmac_hs256_key_b64u)
	key := Key.symmetric(k)
	mut hp := Headers{}
	hp.algorithm = .hmac_256_256
	got := mac0(sample_text.bytes(), key, protected: hp)!
	want := hex.decode(hmac_enc01_message)!
	assert got == want
}

fn test_verify_mac0_accepts_reference_vector() {
	k := base64.url_decode(hmac_hs256_key_b64u)
	key := Key.symmetric(k)
	msg := hex.decode(hmac_enc01_message)!
	payload := verify_mac0(msg, key)!
	assert payload == sample_text.bytes()
}

fn test_mac0_truncated_hs256_64_tag_size() {
	key := Key.symmetric([u8(0x42)].repeat(32))
	mut hp := Headers{}
	hp.algorithm = .hmac_256_64
	signed := mac0('hi'.bytes(), key, protected: hp)!
	msg := Mac0Message.decode(signed)!
	// HMAC 256/64 truncates to 8 bytes per RFC 9053 §3.1.
	assert msg.tag.len == 8
	got := verify_mac0(signed, key)!
	assert got == 'hi'.bytes()
}

fn test_mac0_hs384_roundtrip() {
	key := Key.symmetric([u8(0x33)].repeat(48))
	mut hp := Headers{}
	hp.algorithm = .hmac_384_384
	signed := mac0('hello'.bytes(), key, protected: hp)!
	msg := Mac0Message.decode(signed)!
	assert msg.tag.len == 48
	got := verify_mac0(signed, key)!
	assert got == 'hello'.bytes()
}

fn test_mac0_hs512_roundtrip() {
	key := Key.symmetric([u8(0x77)].repeat(64))
	mut hp := Headers{}
	hp.algorithm = .hmac_512_512
	signed := mac0('hello'.bytes(), key, protected: hp)!
	msg := Mac0Message.decode(signed)!
	assert msg.tag.len == 64
	got := verify_mac0(signed, key)!
	assert got == 'hello'.bytes()
}

// HMAC-ENC-02: HS384 mac0 reference vector (cose-wg/Examples).
fn test_mac0_hs384_matches_reference_vector() {
	k := base64.url_decode('hJtXIZ2uSN5kbQfbtTNWbpdmhkV8FJG-Onbc6mxCcYgAESIzd4iZqiEiIyQlJico')
	key := Key.symmetric(k)
	mut hp := Headers{}
	hp.algorithm = .hmac_384_384
	got := mac0(sample_text.bytes(), key, protected: hp)!
	want :=
		hex.decode('D18443A10106A054546869732069732074686520636F6E74656E742E5830998D26C6459AAEECF44ED20CE00C8CCEDF0A1F3D22A92FC05DB08C5AEB1CB594CAAF5A5C5E2E9D01CCE7E77A93AA8C62')!
	assert got == want
}

// HMAC-ENC-03: HS512 mac0 reference vector (cose-wg/Examples).
fn test_mac0_hs512_matches_reference_vector() {
	k :=
		base64.url_decode('hJtXIZ2uSN5kbQfbtTNWbpdmhkV8FJG-Onbc6mxCcYgAESIzd4iZqiEiIyQlJicoqrvM3e7_paanqKmgsbKztA')
	key := Key.symmetric(k)
	mut hp := Headers{}
	hp.algorithm = .hmac_512_512
	got := mac0(sample_text.bytes(), key, protected: hp)!
	want :=
		hex.decode('D18443A10107A054546869732069732074686520636F6E74656E742E58404A555BF971F7C1891D9DDF304A1A132E2D6F817449474D813E6D04D65962BED8BBA70C17E1F5308FA39962959A4B9B8D7DA8E6D849B209DCD3E98CC0F11EDDF2')!
	assert got == want
}

fn test_verify_mac0_rejects_wrong_key() {
	wrong := Key.symmetric([u8(0x00)].repeat(32))
	msg := hex.decode(hmac_enc01_message)!
	if _ := verify_mac0(msg, wrong) {
		assert false, 'wrong key must not verify'
	} else {
		assert err is VerificationFailed
	}
}

fn test_verify_mac0_rejects_tampered_tag() {
	k := base64.url_decode(hmac_hs256_key_b64u)
	key := Key.symmetric(k)
	mut msg := hex.decode(hmac_enc01_message)!
	// Last byte is the tail of the tag bstr; flip a bit.
	msg[msg.len - 1] ^= 0x01
	if _ := verify_mac0(msg, key) {
		assert false, 'tampered tag must not verify'
	} else {
		assert err is VerificationFailed
	}
}

fn test_mac0_external_aad_is_authenticated() {
	k := base64.url_decode(hmac_hs256_key_b64u)
	key := Key.symmetric(k)
	mut hp := Headers{}
	hp.algorithm = .hmac_256_256
	a := mac0(sample_text.bytes(), key, protected: hp)!
	b := mac0(sample_text.bytes(), key, protected: hp, external_aad: 'context'.bytes())!
	assert a != b
}

fn test_mac0_detached_payload() {
	k := base64.url_decode(hmac_hs256_key_b64u)
	key := Key.symmetric(k)
	mut hp := Headers{}
	hp.algorithm = .hmac_256_256
	signed := mac0([]u8{}, key, protected: hp, detached_payload: 'remote'.bytes())!
	msg := Mac0Message.decode(signed)!
	assert msg.payload == none
	got := verify_mac0(signed, key, detached_payload: 'remote'.bytes())!
	assert got == 'remote'.bytes()
}
