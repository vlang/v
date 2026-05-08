// Tests for COSE_Sign — multi-signer signed messages.
module cose

import encoding.base64
import encoding.hex

const sign_p256_x = 'usWxHK2PmfnHKwXPS54m0kTcGJ90UiglWiGahtagnv8'
const sign_p256_y = 'IBOL-C3BttVivg-lSreASjpkttcsz-1rb7btKLv8EX4'
const sign_p256_d = 'V8kgd2ZBRuh2dgyVINBUqpPDr7BOMGcF22CQMIUHtNM'

const sign_eddsa_x = 'd75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a'
const sign_eddsa_d = '9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60'

fn test_sign_two_signers_roundtrip() {
	// One ES256 signer + one EdDSA signer over the same payload.
	x_ec := base64.url_decode(sign_p256_x)
	y_ec := base64.url_decode(sign_p256_y)
	d_ec := base64.url_decode(sign_p256_d)
	x_ed := hex.decode(sign_eddsa_x)!
	d_ed := hex.decode(sign_eddsa_d)!

	mut hp_ec := Headers{}
	hp_ec.algorithm = .es256
	mut hu_ec := Headers{}
	hu_ec.kid = '11'.bytes()
	signer_ec := Signer{
		key:         Key.ec2_private(.p_256, x_ec, y_ec, d_ec)
		protected:   hp_ec
		unprotected: hu_ec
	}

	mut hp_ed := Headers{}
	hp_ed.algorithm = .eddsa
	mut hu_ed := Headers{}
	hu_ed.kid = 'ed'.bytes()
	signer_ed := Signer{
		key:         Key.okp_private(.ed25519, x_ed, d_ed)
		protected:   hp_ed
		unprotected: hu_ed
	}

	signed := sign('multi-signed payload'.bytes(), [signer_ec, signer_ed])!
	msg := SignMessage.decode(signed)!
	assert msg.signatures.len == 2
	assert (msg.payload or { []u8{} }) == 'multi-signed payload'.bytes()

	// Verify the EC signer
	pub_ec := Key.ec2_public(.p_256, x_ec, y_ec)
	msg.verify(0, pub_ec)!
	// Verify the Ed signer
	pub_ed := Key.okp_public(.ed25519, x_ed)
	msg.verify(1, pub_ed)!
}

fn test_sign_rejects_signer_without_alg() {
	x := hex.decode(sign_eddsa_x)!
	d := hex.decode(sign_eddsa_d)!
	bad_signer := Signer{
		key: Key.okp_private(.ed25519, x, d)
		// no protected.algorithm
	}
	if _ := sign('payload'.bytes(), [bad_signer]) {
		assert false, 'must reject signer without algorithm'
	} else {
		assert err.msg().contains('must declare an algorithm')
	}
}

fn test_sign_decode_rejects_huge_signers_count() {
	// Hand-built COSE_Sign with a signatures-array length declared as
	// 0x1A 0xFFFFFFFF (≈ 4 billion). Without a cap this would trigger
	// a multi-GB allocation. With the cap we should fail fast.
	mut bad := []u8{}
	bad << 0xD8 // tag
	bad << 0x62 // tag 98
	bad << 0x84 // array(4)
	bad << 0x40 // bstr(0)
	bad << 0xA0 // map(0)
	bad << 0x40 // bstr(0) — payload
	// signatures array header: 0x9A LEN_4_BYTES = array of length n
	bad << 0x9A
	bad << 0xFF
	bad << 0xFF
	bad << 0xFF
	bad << 0xFF
	if _ := SignMessage.decode(bad) {
		assert false, 'must reject huge signers count'
	} else {
		assert err is MalformedMessage
	}
}

fn test_sign_verify_wrong_signer_fails() {
	x := hex.decode(sign_eddsa_x)!
	d := hex.decode(sign_eddsa_d)!
	mut hp := Headers{}
	hp.algorithm = .eddsa
	signer := Signer{
		key:       Key.okp_private(.ed25519, x, d)
		protected: hp
	}
	signed := sign('payload'.bytes(), [signer])!
	msg := SignMessage.decode(signed)!
	wrong := Key.okp_public(.ed25519, []u8{len: 32, init: 0})
	if _ := msg.verify(0, wrong) {
		assert false, 'must not verify with wrong key'
	} else {
		assert err is VerificationFailed
	}
}
