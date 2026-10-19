# x.crypto.jwt

`x.crypto.jwt` provides JSON Web Token (JWT) signing, verification, and decoding
using the compact signed-token format from RFC 7519 and RFC 7515 (JWS), with
algorithms from RFC 7518 (JWA) and RFC 8037 (EdDSA).

## Supported Algorithms

- `HS256`: HMAC using SHA-256 (symmetric)
- `HS384`: HMAC using SHA-384 (symmetric)
- `HS512`: HMAC using SHA-512 (symmetric)
- `EdDSA`: Ed25519 signature algorithm (asymmetric)

In accordance with security best practices, `none` and arbitrary unapproved
algorithms are rejected by default.

Every verification call requires `VerifyOptions.algorithm`. Choose this algorithm
from trusted application configuration for the verification key. Do not derive it
from the token header. For Ed25519 public keys, always select `.eddsa`; selecting an
HMAC algorithm would allow anyone who knows the public key to forge tokens.

Registered timestamp claims (`exp`, `nbf`, `iat`) use signed 64-bit integer Unix
seconds. Verification rejects malformed JSON and claims that cannot be decoded as
`StandardClaims`, even when `validate_exp` or `validate_nbf` is disabled. Timestamp
strings, fractional timestamps, null registered claims, and duplicate top-level members
are rejected. Audience claims support a single string; audience arrays are not supported.
Headers require an explicit `alg`. Unsupported `crit` and `b64` extensions, padded or
noncanonical Base64url segments, and negative leeway are rejected. Leeway must be a
nonnegative number of seconds.

## Usage

### Signing and Verifying with StandardClaims

```v
import time
import x.crypto.jwt

fn main() {
	secret := 'my-secret-key'

	claims := jwt.StandardClaims{
		sub: 'user-12345'
		iss: 'auth.example.com'
		aud: 'api.example.com'
		exp: time.now().unix() + 3600
	}

	// Sign token
	token := jwt.sign_with_secret(claims, secret, jwt.SignOptions{
		algorithm: .hs256
	})!
	println('Token: ${token}')

	// Verify token
	tok := jwt.verify_with_secret[jwt.StandardClaims](token, secret, jwt.VerifyOptions{
		algorithm:         .hs256
		expected_issuer:   'auth.example.com'
		expected_audience: 'api.example.com'
	})!
	println('Subject: ${tok.claims.sub or { '' }}')
}
```

### Custom Claims Struct

You can embed `StandardClaims` into your own custom struct:

```v
import time
import x.crypto.jwt

struct MyClaims {
	jwt.StandardClaims
pub mut:
	role    string
	user_id int
}

fn main() {
	secret := 'my-secret-key'

	claims := MyClaims{
		StandardClaims: jwt.StandardClaims{
			sub: 'user-1'
			exp: time.now().unix() + 1800
		}
		role:           'admin'
		user_id:        101
	}

	token := jwt.sign_with_secret(claims, secret, jwt.SignOptions{})!
	tok := jwt.verify_with_secret[MyClaims](token, secret, jwt.VerifyOptions{
		algorithm: .hs256
	})!

	println('User ID: ${tok.claims.user_id}, Role: ${tok.claims.role}')
}
```

### Asymmetric Signatures (EdDSA / Ed25519)

```v
import crypto.ed25519
import x.crypto.jwt

fn main() {
	pubkey, privkey := ed25519.generate_key()!

	claims := jwt.StandardClaims{
		sub: 'ed25519-user'
	}

	// Sign using Ed25519 private key
	token := jwt.sign(claims, privkey, jwt.SignOptions{
		algorithm: .eddsa
		kid:       'key-2026'
	})!

	// Verify using Ed25519 public key
	tok := jwt.verify[jwt.StandardClaims](token, pubkey, jwt.VerifyOptions{
		algorithm: .eddsa
	})!
	println('Verified subject: ${tok.claims.sub or { '' }}')
}
```

### Inspecting Header or Claims Without Verifying

```v
import x.crypto.jwt

fn main() {
	token := '...'
	header := jwt.decode_header(token)!
	println('Algorithm: ${header.alg}, Key ID: ${header.kid or { 'none' }}')

	tok := jwt.decode[jwt.StandardClaims](token)!
	println('Claimed subject: ${tok.claims.sub or { '' }}')
}
```
