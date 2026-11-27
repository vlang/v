module jwt

import crypto.ed25519
import encoding.base64
import time

struct CustomUserClaims {
	StandardClaims
pub mut:
	user_id int
	role    string
}

fn test_hs256_sign_and_verify() {
	secret := 'my-super-secret-key-12345'
	claims := StandardClaims{
		sub: '1234567890'
		iss: 'vlang-test'
		exp: time.now().unix() + 3600
	}

	tok_str := sign_with_secret(claims, secret, SignOptions{ algorithm: .hs256 })!
	assert tok_str.split('.').len == 3

	tok := verify_with_secret[StandardClaims](tok_str, secret, VerifyOptions{
		algorithm:        .hs256
		expected_issuer:  'vlang-test'
		expected_subject: '1234567890'
	})!

	assert tok.header.alg == 'HS256'
	assert tok.header.typ == 'JWT'
	assert tok.claims.sub or { '' } == '1234567890'
	assert tok.claims.iss or { '' } == 'vlang-test'
	assert tok.claims.is_valid_at(time.now().unix())
}

fn test_hs384_sign_and_verify() {
	secret := 'my-hs384-secret-key'
	claims := StandardClaims{
		sub: 'alice'
		exp: time.now().unix() + 1800
	}

	tok_str := sign_with_secret(claims, secret, SignOptions{ algorithm: .hs384 })!
	tok := verify_with_secret[StandardClaims](tok_str, secret, VerifyOptions{
		algorithm: .hs384
	})!

	assert tok.header.alg == 'HS384'
	assert tok.claims.sub or { '' } == 'alice'
}

fn test_hs512_sign_and_verify() {
	secret := 'my-hs512-secret-key'
	claims := StandardClaims{
		sub: 'bob'
		exp: time.now().unix() + 1800
	}

	tok_str := sign_with_secret(claims, secret, SignOptions{ algorithm: .hs512 })!
	tok := verify_with_secret[StandardClaims](tok_str, secret, VerifyOptions{
		algorithm: .hs512
	})!

	assert tok.header.alg == 'HS512'
	assert tok.claims.sub or { '' } == 'bob'
}

fn test_eddsa_sign_and_verify() {
	pubkey, privkey := ed25519.generate_key()!
	claims := StandardClaims{
		sub: 'charlie'
		iss: 'auth-server'
		exp: time.now().unix() + 600
	}

	tok_str := sign(claims, privkey, SignOptions{ algorithm: .eddsa, kid: 'ed25519-key-1' })!

	// Verify header inspectability
	hdr := decode_header(tok_str)!
	assert hdr.alg == 'EdDSA'
	assert hdr.kid or { '' } == 'ed25519-key-1'

	// Verify with public key
	tok := verify[StandardClaims](tok_str, pubkey, VerifyOptions{
		algorithm:       .eddsa
		expected_issuer: 'auth-server'
	})!

	assert tok.header.alg == 'EdDSA'
	assert tok.claims.sub or { '' } == 'charlie'

	// Verification with wrong public key should fail
	wrong_pubkey, _ := ed25519.generate_key()!
	if _ := verify[StandardClaims](tok_str, wrong_pubkey, VerifyOptions{ algorithm: .eddsa }) {
		assert false
	} else {
		assert true
	}
}

fn test_custom_claims_struct() {
	secret := 'custom-claims-secret'
	now := time.now().unix()
	claims := CustomUserClaims{
		StandardClaims: StandardClaims{
			sub: 'user-42'
			iss: 'admin-portal'
			exp: now + 3600
		}
		user_id:        42
		role:           'admin'
	}

	tok_str := sign_with_secret(claims, secret, SignOptions{})!
	tok := verify_with_secret[CustomUserClaims](tok_str, secret, VerifyOptions{
		algorithm:       .hs256
		expected_issuer: 'admin-portal'
	})!

	assert tok.claims.user_id == 42
	assert tok.claims.role == 'admin'
	assert tok.claims.sub or { '' } == 'user-42'
}

fn test_map_claims() {
	secret := 'map-claims-secret'
	claims := {
		'sub':  'service-worker'
		'role': 'worker'
	}

	tok_str := sign_with_secret(claims, secret, SignOptions{})!
	tok := verify_with_secret[map[string]string](tok_str, secret, VerifyOptions{ algorithm: .hs256 })!

	assert tok.claims['sub'] == 'service-worker'
	assert tok.claims['role'] == 'worker'
}

fn test_expiration_validation() {
	secret := 'exp-secret'
	now := time.now().unix()
	expired_claims := StandardClaims{
		sub: 'expired-user'
		exp: now - 100 // expired 100 seconds ago
	}

	tok_str := sign_with_secret(expired_claims, secret, SignOptions{})!

	// Default validation should reject expired token
	if _ := verify_with_secret[StandardClaims](tok_str, secret, VerifyOptions{
		algorithm:    .hs256
		current_time: now
	}) {
		assert false
	} else {
		assert err.msg().contains('token has expired')
	}

	// With validate_exp = false, it should succeed
	tok := verify_with_secret[StandardClaims](tok_str, secret, VerifyOptions{
		algorithm:    .hs256
		validate_exp: false
	})!
	assert tok.claims.sub or { '' } == 'expired-user'
}

fn test_not_before_validation() {
	secret := 'nbf-secret'
	now := time.now().unix()
	future_claims := StandardClaims{
		sub: 'future-user'
		nbf: now + 500 // valid in 500 seconds
	}

	tok_str := sign_with_secret(future_claims, secret, SignOptions{})!

	// Default validation should reject premature token
	if _ := verify_with_secret[StandardClaims](tok_str, secret, VerifyOptions{
		algorithm:    .hs256
		current_time: now
	}) {
		assert false
	} else {
		assert err.msg().contains('not valid yet')
	}

	// With validate_nbf = false, it should succeed
	tok := verify_with_secret[StandardClaims](tok_str, secret, VerifyOptions{
		algorithm:    .hs256
		validate_nbf: false
	})!
	assert tok.claims.sub or { '' } == 'future-user'
}

fn test_leeway() {
	secret := 'leeway-secret'
	now := time.now().unix()
	expired_claims := StandardClaims{
		sub: 'clock-skew-user'
		exp: now - 5 // expired 5 seconds ago
	}

	tok_str := sign_with_secret(expired_claims, secret, SignOptions{})!

	// Without leeway, it fails
	if _ := verify_with_secret[StandardClaims](tok_str, secret, VerifyOptions{
		algorithm:    .hs256
		current_time: now
		leeway:       0
	}) {
		assert false
	} else {
		assert true
	}

	// With 10 seconds leeway, it passes
	tok := verify_with_secret[StandardClaims](tok_str, secret, VerifyOptions{
		algorithm:    .hs256
		current_time: now
		leeway:       10
	})!
	assert tok.claims.sub or { '' } == 'clock-skew-user'
}

fn test_tampered_token_fails() {
	secret := 'tamper-secret'
	claims := StandardClaims{
		sub: 'legit-user'
	}
	tok_str := sign_with_secret(claims, secret, SignOptions{})!
	parts := tok_str.split('.')

	// Tamper payload
	tampered_tok := '${parts[0]}.eyabc.${parts[2]}'
	if _ := verify_with_secret[StandardClaims](tampered_tok, secret, VerifyOptions{ algorithm: .hs256 }) {
		assert false
	} else {
		assert true
	}
}

fn test_wrong_secret_fails() {
	claims := StandardClaims{
		sub: 'user'
	}
	tok_str := sign_with_secret(claims, 'correct-secret', SignOptions{})!

	if _ := verify_with_secret[StandardClaims](tok_str, 'wrong-secret', VerifyOptions{ algorithm: .hs256 }) {
		assert false
	} else {
		assert err.msg().contains('invalid token signature')
	}
}

fn test_algorithm_mismatch_fails() {
	secret := 'mismatch-secret'
	claims := StandardClaims{
		sub: 'user'
	}
	// Signed with HS256
	tok_str := sign_with_secret(claims, secret, SignOptions{ algorithm: .hs256 })!

	// Verifier insists on HS512
	if _ := verify_with_secret[StandardClaims](tok_str, secret, VerifyOptions{
		algorithm: .hs512
	}) {
		assert false
	} else {
		assert err.msg().contains('algorithm mismatch')
	}
}

fn test_decode_without_verify() {
	claims := StandardClaims{
		sub: 'inspect-me'
		iss: 'issuer-abc'
	}
	tok_str := sign_with_secret(claims, 'some-key', SignOptions{ kid: 'key-42' })!

	tok := decode[StandardClaims](tok_str)!
	assert tok.header.kid or { '' } == 'key-42'
	assert tok.claims.sub or { '' } == 'inspect-me'
	assert tok.claims.iss or { '' } == 'issuer-abc'
}

fn test_rfc7515_vector() {
	// RFC 7515 Appendix A.1
	key := base64.url_decode('AyM1SysPpbyDfgZld3umj1qzKObwVMkoqQ-EstJQLr_T-1qS0gZH75aKtMN3Yj0iPS4hcgUuTwjAzZr1Z9CAow')
	rfc_token := 'eyJ0eXAiOiJKV1QiLA0KICJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk'

	hdr, payload := decode_raw(rfc_token)!
	assert hdr.alg == 'HS256'
	assert hdr.typ == 'JWT'

	verified_payload := verify_raw(rfc_token, key, VerifyOptions{
		algorithm:    .hs256
		validate_exp: false
	})!
	assert verified_payload == payload
}

fn test_eddsa_verification_rejects_hmac_with_public_key() {
	pubkey, _ := ed25519.generate_key()!
	for algorithm in [Algorithm.hs256, .hs384, .hs512] {
		// An attacker knows the public key and can use it as an HMAC secret.
		forged := sign(StandardClaims{ sub: 'admin' }, pubkey, SignOptions{
			algorithm: algorithm
		})!
		if _ := verify_raw(forged, pubkey, VerifyOptions{ algorithm: .eddsa }) {
			assert false, 'accepted ${algorithm} token with an Ed25519 verification key'
		} else {
			assert err.msg().contains('algorithm mismatch')
		}
		if _ := verify[StandardClaims](forged, pubkey, VerifyOptions{ algorithm: .eddsa }) {
			assert false, 'accepted ${algorithm} token with an Ed25519 verification key'
		} else {
			assert err.msg().contains('algorithm mismatch')
		}
	}
}

fn test_invalid_registered_claims_are_rejected() {
	key := 'claims-secret'.bytes()
	for payload in [
		'not json',
		'{"exp":1.5}',
		'{"exp":1,"iat":"not-a-date"}',
		'{"exp":1,"nbf":9223372036854775808}',
		'{"exp":1,"iat":9223372036854775808}',
		'{"nbf":1000,"exp":9223372036854775808}',
	] {
		token := sign_raw(Header{}, payload, key)!
		for validate_times in [true, false] {
			if _ := verify_raw(token, key, VerifyOptions{
				algorithm:    .hs256
				current_time: 200
				validate_exp: validate_times
				validate_nbf: validate_times
			}) {
				assert false, 'accepted invalid registered claims: ${payload}'
			} else {
				assert err.msg().starts_with('invalid JWT claims:')
			}
		}
	}
}

fn test_rs256_sign_and_verify() {
	priv_pem := r"-----BEGIN RSA PRIVATE KEY-----
MIICXwIBAAKBgQCBrmdFbTz+kFP6FGDvThQG7a2pMnnjV1aU9Jyal5kC0t10x7ZI
dmDutBgcDDsyL2hqwJN+Ndg8zX6aZgfFhQ3ddYt/ENiLmOg4zea/FaRS8vwFLijs
KvZejpg6+mfNv/XLrNsQ6ucddUts3GqygupBQoz8pCF+GsoUIGTDF7lVpQIDAQAB
AoGADAPUnaIZXJySq0DoGkPppxquVFbQ5DdbVkGzoL8a9iADVKaMRPpy0Kru8OpR
Mb0it4FQKQlkF34SlMuPLGnVTXp+uEjIoeIOi15Fot/aTujS8bSfZZ1OSsZVoLU7
0Mr41GNgmCr995b+PDb5tNnH4n2Gkvp749701zWsVHk8MKECRQCwkMls0G9iaiDL
ZJlD5WuV8RBlfAXtlBCfGNtlgdj4z7CcU1Crv9YvUoM34TqbB3LDfSRCzx9aTjvA
xiLpjMoDZucmSwI9ALwF6L0FO2k8eVr6fJ30NIZ/zXi8JK1sNPsSG6DEWBDfwfR7
zHvw+tUILAY8SIxOToZbZdMXPsvm6yS9zwJEVAhkxTQifu1RtFupkiXjYVZ83j/7
tknM61tyN8pn0GtQe+YvM762mokjyHmaYeRWm3G4y082dc1cJPBn9025a4sTIicC
PCD3nEZAO5mJG/Iix2Teo7MNh5ZqzreFqIh5NVTr+FEFGrnmVwVITIK7yxRQz5ui
Q0x1jBYGFmGkHIi1qwJEFvXww4oHg5nos+R4SQp/rIteRCFfeNgrmGlCXZhUjYkY
g/k6dgEEvvfrb/FExmg1Bq67LqDqPDiymI9PGSi6hHIF6wU=
-----END RSA PRIVATE KEY-----"
	pub_pem := r"-----BEGIN RSA PUBLIC KEY-----
MIGJAoGBAIGuZ0VtPP6QU/oUYO9OFAbtrakyeeNXVpT0nJqXmQLS3XTHtkh2YO60
GBwMOzIvaGrAk3412DzNfppmB8WFDd11i38Q2IuY6DjN5r8VpFLy/AUuKOwq9l6O
mDr6Z82/9cus2xDq5x11S2zcarKC6kFCjPykIX4ayhQgZMMXuVWlAgMBAAE=
-----END RSA PUBLIC KEY-----"
	claims := StandardClaims{
		sub: 'alice'
		iss: 'auth-server'
		exp: time.now().unix() + 600
	}
	tok_str := sign_with_secret(claims, priv_pem, SignOptions{ algorithm: .rs256 })!
	hdr := decode_header(tok_str)!
	assert hdr.alg == 'RS256'
	tok := verify_with_secret[StandardClaims](tok_str, pub_pem, VerifyOptions{
		algorithm:       .rs256
		expected_issuer: 'auth-server'
	})!
	assert tok.claims.sub or { '' } == 'alice'

	if _ := verify_with_secret[StandardClaims](tok_str, priv_pem, VerifyOptions{ algorithm: .rs256 }) {
		assert false
	} else {
		assert true
	}
}
