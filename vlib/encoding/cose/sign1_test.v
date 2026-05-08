// Tests for COSE_Sign1: roundtrip per algorithm and bytes-exact match
// against cose-wg/Examples reference vectors. EdDSA signatures are
// deterministic (RFC 8032 §5.1.6) so we can compare bytes directly;
// ECDSA signatures use a fresh nonce each run, so for ES* we test by
// verifying the message produced by another implementation and by
// round-tripping our own output through verify1.
module cose

import encoding.base64
import encoding.hex

// EdDSA ed25519-sig-01 from cose-wg/Examples (Unlicense).
const eddsa_d_hex = '9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60'
const eddsa_x_hex = 'd75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a'
const eddsa_sig01_message = 'D28445A201270300A10442313154546869732069732074686520636F6E74656E742E58407142FD2FF96D56DB85BEE905A76BA1D0B7321A95C8C4D3607C5781932B7AFB8711497DFA751BF40B58B3BCC32300B1487F3DB34085EEF013BF08F4A44D6FEF0D'

// ECDSA ecdsa-sig-01 from cose-wg/Examples.
const ecdsa_p256_x_b64u = 'usWxHK2PmfnHKwXPS54m0kTcGJ90UiglWiGahtagnv8'
const ecdsa_p256_y_b64u = 'IBOL-C3BttVivg-lSreASjpkttcsz-1rb7btKLv8EX4'
const ecdsa_p256_d_b64u = 'V8kgd2ZBRuh2dgyVINBUqpPDr7BOMGcF22CQMIUHtNM'
const ecdsa_sig01_message = 'D28445A201260300A10442313154546869732069732074686520636F6E74656E742E58406520BBAF2081D7E0ED0F95F76EB0733D667005F7467CEC4B87B9381A6BA1EDE8E00DF29F32A37230F39A842A54821FDD223092819D7728EFB9D3A0080B75380B'

const sample_text = 'This is the content.'

fn test_sign1_eddsa_matches_reference_vector() {
	// EdDSA is deterministic, so we can match the reference bytes-exact.
	d := hex.decode(eddsa_d_hex)!
	x := hex.decode(eddsa_x_hex)!
	key := Key.okp_private(.ed25519, x, d)
	mut hp := Headers{}
	hp.algorithm = .eddsa
	hp.content_type_int = u64(0)
	mut hu := Headers{}
	hu.kid = '11'.bytes()
	got := sign1(sample_text.bytes(), key, protected: hp, unprotected: hu)!
	want := hex.decode(eddsa_sig01_message)!
	assert got == want
}

fn test_verify1_accepts_reference_eddsa_message() {
	x := hex.decode(eddsa_x_hex)!
	pub_key := Key.okp_public(.ed25519, x)
	msg := hex.decode(eddsa_sig01_message)!
	payload := verify1(msg, pub_key)!
	assert payload == sample_text.bytes()
}

fn test_verify1_accepts_reference_ecdsa_p256_message() {
	x := base64.url_decode(ecdsa_p256_x_b64u)
	y := base64.url_decode(ecdsa_p256_y_b64u)
	pub_key := Key.ec2_public(.p_256, x, y)
	msg := hex.decode(ecdsa_sig01_message)!
	payload := verify1(msg, pub_key)!
	assert payload == sample_text.bytes()
}

fn test_sign1_ecdsa_p256_roundtrip() {
	x := base64.url_decode(ecdsa_p256_x_b64u)
	y := base64.url_decode(ecdsa_p256_y_b64u)
	d := base64.url_decode(ecdsa_p256_d_b64u)
	priv_key := Key.ec2_private(.p_256, x, y, d)
	pub_key := Key.ec2_public(.p_256, x, y)
	mut hp := Headers{}
	hp.algorithm = .es256
	mut hu := Headers{}
	hu.kid = '11'.bytes()
	signed := sign1('hello'.bytes(), priv_key, protected: hp, unprotected: hu)!
	got := verify1(signed, pub_key)!
	assert got == 'hello'.bytes()
}

fn test_verify1_rejects_tampered_payload() {
	x := hex.decode(eddsa_x_hex)!
	pub_key := Key.okp_public(.ed25519, x)
	mut msg := hex.decode(eddsa_sig01_message)!
	// Find the last byte of the payload bstr and flip a bit. The payload
	// "This is the content." (20 bytes) starts after the unprotected
	// map. We flip the first content byte by scanning for the bstr
	// header 0x54 (bstr of length 20).
	mut idx := 0
	for idx < msg.len {
		if msg[idx] == 0x54 && idx + 20 < msg.len {
			break
		}
		idx++
	}
	msg[idx + 1] ^= 0x01
	if _ := verify1(msg, pub_key) {
		assert false, 'tampered payload must not verify'
	} else {
		assert err is VerificationFailed
	}
}

fn test_verify1_rejects_wrong_key() {
	// Use a different Ed25519 public key (zeros) to verify the reference
	// message — it must fail.
	wrong_x := []u8{len: 32, init: 0}
	pub_key := Key.okp_public(.ed25519, wrong_x)
	msg := hex.decode(eddsa_sig01_message)!
	if _ := verify1(msg, pub_key) {
		assert false, 'wrong key must not verify'
	} else {
		assert err is VerificationFailed
	}
}

fn test_verify1_rejects_unknown_critical_label() {
	// Per RFC 9052 §3.1, a verifier MUST fail when `crit` lists a
	// label it does not understand. Build a message whose protected
	// header carries a crit list referencing label 99 (unknown).
	x := hex.decode(eddsa_x_hex)!
	d := hex.decode(eddsa_d_hex)!
	priv_key := Key.okp_private(.ed25519, x, d)
	pub_key := Key.okp_public(.ed25519, x)
	mut hp := Headers{}
	hp.algorithm = .eddsa
	hp.critical = [i64(99)]
	signed := sign1('payload'.bytes(), priv_key, protected: hp)!
	if _ := verify1(signed, pub_key) {
		assert false, 'must reject unknown crit label'
	} else {
		assert err is MalformedMessage
		assert err.msg().contains('crit lists unknown label')
	}
}

fn test_verify1_accepts_known_critical_label() {
	// `crit` listing only known labels (e.g. label 1 = alg) must
	// not block verification.
	x := hex.decode(eddsa_x_hex)!
	d := hex.decode(eddsa_d_hex)!
	priv_key := Key.okp_private(.ed25519, x, d)
	pub_key := Key.okp_public(.ed25519, x)
	mut hp := Headers{}
	hp.algorithm = .eddsa
	hp.critical = [i64(1)]
	signed := sign1('payload'.bytes(), priv_key, protected: hp)!
	got := verify1(signed, pub_key)!
	assert got == 'payload'.bytes()
}

fn test_sign1_rejects_key_alg_mismatch() {
	// A key declaring `alg = ES256` must not be silently used for an
	// EdDSA signing call: this catches a common copy-paste mistake.
	x := hex.decode(eddsa_x_hex)!
	d := hex.decode(eddsa_d_hex)!
	mut key := Key.okp_private(.ed25519, x, d)
	key.alg = .es256 // wrong intent for an OKP key
	mut hp := Headers{}
	hp.algorithm = .eddsa
	if _ := sign1('payload'.bytes(), key, protected: hp) {
		assert false, 'must reject alg mismatch'
	} else {
		assert err is AlgorithmMismatch
	}
}

fn test_sign1_external_aad_changes_signature() {
	x := hex.decode(eddsa_x_hex)!
	d := hex.decode(eddsa_d_hex)!
	key := Key.okp_private(.ed25519, x, d)
	mut hp := Headers{}
	hp.algorithm = .eddsa
	a := sign1('payload'.bytes(), key, protected: hp)!
	b := sign1('payload'.bytes(), key, protected: hp, external_aad: 'context'.bytes())!
	assert a != b
}

fn test_sign1_detached_payload_omits_payload_in_message() {
	x := hex.decode(eddsa_x_hex)!
	d := hex.decode(eddsa_d_hex)!
	priv_key := Key.okp_private(.ed25519, x, d)
	pub_key := Key.okp_public(.ed25519, x)
	mut hp := Headers{}
	hp.algorithm = .eddsa
	signed := sign1([]u8{}, priv_key,
		protected:        hp
		detached_payload: 'remote payload'.bytes()
	)!
	// The encoded message must contain a CBOR null where the payload
	// would normally be. Decode and check.
	msg := Sign1Message.decode(signed)!
	assert msg.payload == none
	// Verifier needs the detached bytes back.
	got := verify1(signed, pub_key, detached_payload: 'remote payload'.bytes())!
	assert got == 'remote payload'.bytes()
}

fn test_sign1_untagged_roundtrip() {
	x := hex.decode(eddsa_x_hex)!
	d := hex.decode(eddsa_d_hex)!
	priv_key := Key.okp_private(.ed25519, x, d)
	pub_key := Key.okp_public(.ed25519, x)
	mut hp := Headers{}
	hp.algorithm = .eddsa
	signed := sign1('payload'.bytes(), priv_key, protected: hp, untagged: true)!
	// First byte must be 0x84 (array(4)), not 0xD2 (tag 18).
	assert signed[0] == 0x84
	got := verify1(signed, pub_key)!
	assert got == 'payload'.bytes()
}
