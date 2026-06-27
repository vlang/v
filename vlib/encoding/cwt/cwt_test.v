// Tests for the CWT module. Reference vectors come from RFC 8392
// Appendix A and the matching JSON files in the cose-wg/Examples
// repository (Unlicense). MAC algorithms produce deterministic tags so
// we can match output bytes-exactly; ECDSA-signed vectors are
// verified rather than reproduced.
module cwt

import encoding.hex
import encoding.cose

// Sample claims from RFC 8392 Appendix A.1.
const a1_payload_hex = 'a70175636f61703a2f2f61732e6578616d706c652e636f6d02656572696b77037818636f61703a2f2f6c696768742e6578616d706c652e636f6d041a5612aeb0051a5610d9f0061a5610d9f007420b71'

fn build_a1_claims() ClaimsSet {
	mut c := ClaimsSet{}
	c.iss = 'coap://as.example.com'
	c.sub = 'erikw'
	c.aud = ['coap://light.example.com']
	c.exp = 1444064944
	c.nbf = 1443944944
	c.iat = 1443944944
	c.cti = [u8(0x0b), 0x71]
	return c
}

fn test_claims_set_encodes_to_rfc_appendix_a1_bytes() {
	c := build_a1_claims()
	got := c.encode()!
	want := hex.decode(a1_payload_hex)!
	assert got == want
}

fn test_claims_set_decode_roundtrip() {
	want := build_a1_claims()
	encoded := want.encode()!
	got := ClaimsSet.decode(encoded)!
	assert (got.iss or { '' }) == (want.iss or { '' })
	assert (got.sub or { '' }) == (want.sub or { '' })
	assert got.aud == want.aud
	assert (got.exp or { 0 }) == (want.exp or { 0 })
	assert (got.nbf or { 0 }) == (want.nbf or { 0 })
	assert (got.iat or { 0 }) == (want.iat or { 0 })
	assert (got.cti or { []u8{} }) == (want.cti or { []u8{} })
}

fn test_claims_set_decode_real_appendix_a1_payload() {
	got := ClaimsSet.decode(hex.decode(a1_payload_hex)!)!
	assert (got.iss or { '' }) == 'coap://as.example.com'
	assert (got.sub or { '' }) == 'erikw'
	assert got.aud == ['coap://light.example.com']
	assert (got.exp or { 0 }) == 1444064944
	assert (got.nbf or { 0 }) == 1443944944
	assert (got.iat or { 0 }) == 1443944944
	assert (got.cti or { []u8{} }) == [u8(0x0b), 0x71]
}

// RFC 8392 Appendix A.3 — Signed CWT (ECDSA P-256). The signature is
// non-deterministic, so we only verify the reference message. Public
// key from A_3.json.
const a3_p256_x_hex = '143329cce7868e416927599cf65a34f3ce2ffda55a7eca69ed8919a394d42f0f'
const a3_p256_y_hex = '60f7f1a780d8a783bfb7a2dd6b2796e8128dbbcef9d3d168db9529971a36e7b9'
const a3_p256_d_hex = '6c1382765aec5358f117733d281c1c7bdc39884d04a45a1e6c67c858bc206c19'
const a3_signed_message_hex = 'D28443A10126A05850A70175636F61703A2F2F61732E6578616D706C652E636F6D02656572696B77037818636F61703A2F2F6C696768742E6578616D706C652E636F6D041A5612AEB0051A5610D9F0061A5610D9F007420B7158405427C1FF28D23FBAD1F29C4C7C6A555E601D6FA29F9179BC3D7438BACACA5ACD08C8D4D4F96131680C429A01F85951ECEE743A52B9B63632C57209120E1C9E30'

fn test_verify_signed_cwt_appendix_a3() {
	x := hex.decode(a3_p256_x_hex)!
	y := hex.decode(a3_p256_y_hex)!
	pub_key := cose.Key.ec2_public(.p_256, x, y)
	// The reference vector is NOT wrapped in tag 61, so verify accepts
	// the raw COSE message.
	token := hex.decode(a3_signed_message_hex)!
	claims := verify(token, pub_key)!
	assert (claims.iss or { '' }) == 'coap://as.example.com'
	assert (claims.sub or { '' }) == 'erikw'
}

fn test_sign_and_verify_cwt_roundtrip_es256() {
	x := hex.decode(a3_p256_x_hex)!
	y := hex.decode(a3_p256_y_hex)!
	d := hex.decode(a3_p256_d_hex)!
	priv := cose.Key.ec2_private(.p_256, x, y, d)
	pub_key := cose.Key.ec2_public(.p_256, x, y)
	mut hp := cose.Headers{}
	hp.algorithm = .es256
	claims := build_a1_claims()
	token := sign(claims, priv, protected: hp)!
	got := verify(token, pub_key)!
	assert (got.iss or { '' }) == 'coap://as.example.com'
}

// RFC 8392 Appendix A.4 — MACed CWT (HMAC 256/64). The tag is
// deterministic so we can match bytes-exactly.
const a4_key_hex = '403697de87af64611c1d32a05dab0fe1fcb715a86ab435f1ec99192d79569388'
const a4_maced_message_hex = 'D18443A10104A05850A70175636F61703A2F2F61732E6578616D706C652E636F6D02656572696B77037818636F61703A2F2F6C696768742E6578616D706C652E636F6D041A5612AEB0051A5610D9F0061A5610D9F007420B7148093101EF6D789200'

fn test_mac_cwt_matches_appendix_a4_vector() {
	k := hex.decode(a4_key_hex)!
	key := cose.Key.symmetric(k)
	mut hp := cose.Headers{}
	hp.algorithm = .hmac_256_64
	claims := build_a1_claims()
	got := mac(claims, key, protected: hp, tagged_cwt: false)!
	want := hex.decode(a4_maced_message_hex)!
	assert got == want
}

fn test_verify_maced_cwt_appendix_a4() {
	k := hex.decode(a4_key_hex)!
	key := cose.Key.symmetric(k)
	token := hex.decode(a4_maced_message_hex)!
	claims := verify_mac(token, key)!
	assert (claims.iss or { '' }) == 'coap://as.example.com'
	assert (claims.cti or { []u8{} }) == [u8(0x0b), 0x71]
}

fn test_cwt_outer_tag_61_roundtrip() {
	k := hex.decode(a4_key_hex)!
	key := cose.Key.symmetric(k)
	mut hp := cose.Headers{}
	hp.algorithm = .hmac_256_64
	claims := build_a1_claims()
	token := mac(claims, key, protected: hp, tagged_cwt: true)!
	// First two bytes must be the tag 61 wrapper (D8 3D).
	assert token[0] == 0xD8
	assert token[1] == 0x3D
	got := verify_mac(token, key)!
	assert (got.iss or { '' }) == 'coap://as.example.com'
}

fn test_aud_single_string_form_on_wire() {
	// Single-element aud must encode as tstr, not [tstr] — RFC 7519/8392.
	mut c := ClaimsSet{}
	c.aud = ['solo']
	encoded := c.encode()!
	parsed := ClaimsSet.decode(encoded)!
	assert parsed.aud == ['solo']
	// 0xA1 (map(1)) 0x03 (label 3 = aud) 0x64 (tstr len 4) "solo"
	assert encoded == [u8(0xA1), 0x03, 0x64, 0x73, 0x6F, 0x6C, 0x6F]
}

fn test_aud_multi_string_form_on_wire() {
	mut c := ClaimsSet{}
	c.aud = ['a', 'b']
	encoded := c.encode()!
	parsed := ClaimsSet.decode(encoded)!
	assert parsed.aud == ['a', 'b']
}

fn test_validate_time_passes_inside_window() {
	c := ClaimsSet{
		exp: 2_000_000_000
		nbf: 1_000_000_000
	}
	c.validate_time(1_500_000_000)!
}

fn test_validate_time_rejects_expired_token() {
	c := ClaimsSet{
		exp: 1_000_000_000
	}
	if _ := c.validate_time(2_000_000_000) {
		assert false, 'expired token must fail validation'
	} else {
		assert err is ClaimExpired
		assert err.msg().contains('expired')
	}
}

fn test_validate_time_rejects_token_before_nbf() {
	c := ClaimsSet{
		nbf: 2_000_000_000
	}
	if _ := c.validate_time(1_000_000_000) {
		assert false, 'pre-nbf token must fail validation'
	} else {
		assert err is ClaimNotYetValid
		assert err.msg().contains('not yet valid')
	}
}

fn test_validate_time_passes_when_neither_claim_set() {
	c := ClaimsSet{}
	c.validate_time(1_500_000_000)!
}

fn test_expired_helper_returns_false_without_exp() {
	c := ClaimsSet{}
	assert !c.expired(9_999_999_999)
}

fn test_extra_int_claim_preserved_on_roundtrip() {
	mut c := ClaimsSet{}
	c.extra_int_claims << ClaimEntry{
		label: 100
		value: cose.Headers{}.to_value() // any cbor.Value works; use empty map
	}
	encoded := c.encode()!
	parsed := ClaimsSet.decode(encoded)!
	assert parsed.extra_int_claims.len == 1
	assert parsed.extra_int_claims[0].label == 100
}
