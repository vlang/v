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
	if _ := verify[StandardClaims](tok_str, wrong_pubkey, VerifyOptions{}) {
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
	tok := verify_with_secret[map[string]string](tok_str, secret, VerifyOptions{})!

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
		current_time: now
	}) {
		assert false
	} else {
		assert err.msg().contains('token has expired')
	}

	// With validate_exp = false, it should succeed
	tok := verify_with_secret[StandardClaims](tok_str, secret, VerifyOptions{
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
		current_time: now
	}) {
		assert false
	} else {
		assert err.msg().contains('not valid yet')
	}

	// With validate_nbf = false, it should succeed
	tok := verify_with_secret[StandardClaims](tok_str, secret, VerifyOptions{
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
		current_time: now
		leeway:       0
	}) {
		assert false
	} else {
		assert true
	}

	// With 10 seconds leeway, it passes
	tok := verify_with_secret[StandardClaims](tok_str, secret, VerifyOptions{
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
	if _ := verify_with_secret[StandardClaims](tampered_tok, secret, VerifyOptions{}) {
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

	if _ := verify_with_secret[StandardClaims](tok_str, 'wrong-secret', VerifyOptions{}) {
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
