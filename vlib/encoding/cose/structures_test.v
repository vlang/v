// Tests for Sig_structure / MAC_structure construction. Reference values
// come from the cose-wg/Examples repository (Unlicense / public domain).
module cose

import encoding.hex

const sample_payload = 'This is the content.'.bytes()

// From ECDSA-01.json (ES256 sign1):
//   protected = bstr-wrapped CBOR map { alg: -7, ctyp: 0 }
//   external_aad = empty
//   payload = "This is the content."
//   ToBeSign_hex = 846A5369676E61747572653145A2012603004054...
fn test_sig_structure_sign1_matches_ecdsa01_vector() {
	body_protected := hex.decode('A201260300')!
	want :=
		hex.decode('846A5369676E61747572653145A2012603004054546869732069732074686520636F6E74656E742E')!
	got := sig_structure_sign1(body_protected, []u8{}, sample_payload)
	assert got == want
}

// From EdDSA-01.json (sign1):
//   protected = bstr-wrapped CBOR map { alg: -8, ctyp: 0 }
//   ToBeSign_hex = 846A5369676E61747572653145A2012703004054...
fn test_sig_structure_sign1_matches_eddsa01_vector() {
	body_protected := hex.decode('A201270300')!
	want :=
		hex.decode('846A5369676E61747572653145A2012703004054546869732069732074686520636F6E74656E742E')!
	got := sig_structure_sign1(body_protected, []u8{}, sample_payload)
	assert got == want
}

// From CWT/A_3.json (RFC 8392 Appendix A.3 — Signed CWT w/ ES256):
//   protected = bstr-wrapped { alg: -7 }
//   payload = the encoded CWT claims set
fn test_sig_structure_sign1_matches_cwt_a3_vector() {
	body_protected := hex.decode('A10126')!
	cwt_claims :=
		hex.decode('a70175636f61703a2f2f61732e6578616d706c652e636f6d02656572696b77037818636f61703a2f2f6c696768742e6578616d706c652e636f6d041a5612aeb0051a5610d9f0061a5610d9f007420b71')!
	want :=
		hex.decode('846A5369676E61747572653143A10126405850A70175636F61703A2F2F61732E6578616D706C652E636F6D02656572696B77037818636F61703A2F2F6C696768742E6578616D706C652E636F6D041A5612AEB0051A5610D9F0061A5610D9F007420B71')!
	got := sig_structure_sign1(body_protected, []u8{}, cwt_claims)
	assert got == want
}

// Sig_structure for COSE_Sign uses the "Signature" (no "1") tag and
// adds a sign_protected slot.
fn test_sig_structure_sign_includes_sign_protected() {
	body_protected := []u8{} // empty body protected
	sign_protected := hex.decode('A10126')! // {alg:-7}
	got := sig_structure_sign(body_protected, sign_protected, []u8{}, sample_payload)
	// 85 array(5) | 69 "Signature" (text 9) | 40 (empty bstr) |
	// 43 A10126 (3-byte bstr) | 40 (empty external) |
	// 54 (payload bstr 20) | "This is..."
	assert got[0] == 0x85
	assert got[1] == 0x69 // text(9)
	assert got[2..11] == 'Signature'.bytes()
	assert got[11] == 0x40 // empty body_protected
	assert got[12] == 0x43 // bstr of length 3
}

// From HMac-enc-01.json (HS256 mac0):
//   ToMac_hex = 84644D41433043A101054054...
fn test_mac_structure_mac0_matches_hmac_enc01_vector() {
	body_protected := hex.decode('A10105')! // {alg: 5} (HS256)
	want := hex.decode('84644D41433043A101054054546869732069732074686520636F6E74656E742E')!
	got := mac_structure_mac0(body_protected, []u8{}, sample_payload)
	assert got == want
}

fn test_external_aad_is_signed_in_sig_structure() {
	body_protected := hex.decode('A10126')!
	external := 'application context'.bytes()
	with_ext := sig_structure_sign1(body_protected, external, sample_payload)
	without_ext := sig_structure_sign1(body_protected, []u8{}, sample_payload)
	assert with_ext != without_ext
}
