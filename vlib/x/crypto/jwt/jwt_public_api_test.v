module main

import x.crypto.jwt

fn test_public_generic_sign_decode_and_verify() {
	token := jwt.sign_with_secret(jwt.StandardClaims{ sub: 'public-api' }, 'secret', jwt.SignOptions{})!
	decoded := jwt.decode[jwt.StandardClaims](token)!
	verified := jwt.verify_with_secret[jwt.StandardClaims](token, 'secret', jwt.VerifyOptions{
		algorithm: .hs256
	})!
	assert decoded.claims.sub? == 'public-api'
	assert verified.claims.sub? == 'public-api'
}
