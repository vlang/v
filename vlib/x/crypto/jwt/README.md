# x.crypto.jwt

`x.crypto.jwt` provides JSON Web Token (JWT) signing, verification, and decoding
using the compact signed-token format from RFC 7519 and RFC 7515 (JWS), with
algorithms from RFC 7518 (JWA) and RFC 8037 (EdDSA), and JSON Web Key Sets from
RFC 7517.

## Supported Algorithms

- `HS256`: HMAC using SHA-256 (symmetric)
- `HS384`: HMAC using SHA-384 (symmetric)
- `HS512`: HMAC using SHA-512 (symmetric)
- `EdDSA`: Ed25519 signature algorithm (asymmetric)
- `RS256`, `RS384`, `RS512`: RSASSA-PKCS1-v1_5 with SHA-256/384/512 (asymmetric)
- `PS256`, `PS384`, `PS512`: RSASSA-PSS with SHA-256/384/512, and a salt as long
  as the digest (asymmetric)

In accordance with security best practices, `none` and arbitrary unapproved
algorithms are rejected by default.

Every verification call requires `VerifyOptions.algorithm`. Choose this algorithm
from trusted application configuration for the verification key. Do not derive it
from the token header. For Ed25519 and RSA public keys, always select their
asymmetric algorithm; selecting an HMAC algorithm would allow anyone who knows the
public key to forge tokens.

Registered timestamp claims (`exp`, `nbf`, `iat`) use signed 64-bit integer Unix
seconds. Verification rejects malformed JSON and claims that cannot be decoded as
`StandardClaims`, even when `validate_exp` or `validate_nbf` is disabled. Timestamp
strings, fractional timestamps, null registered claims, and duplicate top-level members
are rejected. The audience claim can be a string or an array of strings, both read
into `jwt.Audience`; an expected audience must be one of them.
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
		aud: jwt.Audience.new('api.example.com')
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

### RSA Signatures (RS256 / PS256)

Sign with a key parsed once by `crypto.rsa`, with `sign_with_rsa_key`; verify
with a public key in DER, SubjectPublicKeyInfo or PKCS #1. `sign` also accepts a
DER private key, PKCS #8 or PKCS #1, but parses and checks it at each call.

```v
import crypto.rsa
import x.crypto.jwt

fn main() {
	private_key := rsa.generate_key(2048)!

	claims := jwt.StandardClaims{
		sub: 'rsa-user'
	}
	token := jwt.sign_with_rsa_key(claims, private_key, jwt.SignOptions{
		algorithm: .ps256
		kid:       'key-2026'
	})!

	public_der := private_key.public_key().to_der()
	tok := jwt.verify[jwt.StandardClaims](token, public_der, jwt.VerifyOptions{
		algorithm: .ps256
	})!
	println('Verified subject: ${tok.claims.sub or { '' }}')
}
```

### JSON Web Key Sets (Keycloak and other OpenID providers)

`verify_with_jwks` picks the key named by the `kid` of the token in a JSON Web
Key Set, such as the one an OpenID provider publishes at its `jwks_uri`. The
algorithm still comes from your configuration, and the key must match it: its
type, and its `alg` and `use` when present. Exactly one key of the set must fit.

`StandardClaims.aud` is an `Audience`, which holds one or several recipients, as
in many Keycloak access tokens: test it with `contains`.

```v ignore
import net.http
import x.crypto.jwt

fn main() {
	certs := http.fetch(
		url:      'https://sso.example.com/realms/demo/protocol/openid-connect/certs'
		validate: true
	)!
	jwks := jwt.Jwks.parse(certs.body)!
	token := '...'
	tok := jwt.verify_with_jwks[jwt.StandardClaims](token, jwks, jwt.VerifyOptions{
		algorithm:         .rs256
		expected_issuer:   'https://sso.example.com/realms/demo'
		expected_audience: 'my-client'
	})!
	println('Verified subject: ${tok.claims.sub or { '' }}')
}
```

`Jwk.from_rsa_public_key` builds the JWK of an RSA key, to publish your own key
set, for instance for `private_key_jwt` client authentication;
`examples/oidc_private_key_jwt.v` shows that flow against an OpenID provider.

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
