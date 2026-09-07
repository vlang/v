// COSE / CWT example program: produces a signed payload, a MACed
// payload and a signed CBOR Web Token, then verifies each.
//
// Run with:  v run examples/cose_cwt.v
import encoding.cose
import encoding.cwt
import encoding.hex

fn main() {
	demo_sign1_eddsa()!
	demo_mac0_hmac_256()!
	demo_signed_cwt_es256()!
}

// COSE_Sign1 with EdDSA over Ed25519. EdDSA signatures are
// deterministic, so signing the same payload twice produces identical
// bytes — useful for caching and reproducible builds.
fn demo_sign1_eddsa() ! {
	d := hex.decode('9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60')!
	x := hex.decode('d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a')!
	priv := cose.Key.okp_private(.ed25519, x, d)
	pub_key := cose.Key.okp_public(.ed25519, x)

	signed := cose.sign1('hello, COSE'.bytes(), priv,
		protected:   cose.Headers{
			algorithm: .eddsa
		}
		unprotected: cose.Headers{
			kid: 'demo-key'.bytes()
		}
	)!
	println('Sign1 (${signed.len} bytes): ${hex.encode(signed)}')

	payload := cose.verify1(signed, pub_key)!
	assert payload == 'hello, COSE'.bytes()
	println('  verified payload: ${payload.bytestr()}')
}

// COSE_Mac0 with HMAC-SHA256. Symmetric, useful when both ends share
// a secret out of band (e.g. a session key).
fn demo_mac0_hmac_256() ! {
	key := cose.Key.symmetric([u8(0xA5)].repeat(32))
	maced := cose.mac0('mac me'.bytes(), key,
		protected: cose.Headers{
			algorithm: .hmac_256_256
		}
	)!
	println('Mac0 (${maced.len} bytes): ${hex.encode(maced)}')

	got := cose.verify_mac0(maced, key)!
	assert got == 'mac me'.bytes()
	println('  verified payload: ${got.bytestr()}')
}

// Signed CBOR Web Token (RFC 8392) wrapped in a COSE_Sign1, with
// ECDSA P-256 + SHA-256. Key material taken from RFC 8392 Appendix A.
fn demo_signed_cwt_es256() ! {
	x := hex.decode('143329cce7868e416927599cf65a34f3ce2ffda55a7eca69ed8919a394d42f0f')!
	y := hex.decode('60f7f1a780d8a783bfb7a2dd6b2796e8128dbbcef9d3d168db9529971a36e7b9')!
	d := hex.decode('6c1382765aec5358f117733d281c1c7bdc39884d04a45a1e6c67c858bc206c19')!
	priv := cose.Key.ec2_private(.p_256, x, y, d)
	pub_key := cose.Key.ec2_public(.p_256, x, y)

	claims := cwt.ClaimsSet{
		iss: 'coap://as.example.com'
		sub: 'erikw'
		aud: ['coap://light.example.com']
		exp: 1444064944
		iat: 1443944944
	}
	token := cwt.sign(claims, priv,
		protected: cose.Headers{
			algorithm: .es256
		}
	)!
	println('CWT  (${token.len} bytes): ${hex.encode(token)}')

	verified := cwt.verify(token, pub_key)!
	iss := verified.iss or { '' }
	sub := verified.sub or { '' }
	exp := verified.exp or { 0 }
	println('  iss=${iss}  sub=${sub}  exp=${exp}')
}
