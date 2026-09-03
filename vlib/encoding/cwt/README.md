# `encoding.cwt`

CBOR Web Tokens — pure-V implementation of [RFC 8392][rfc8392], the
CBOR analogue of JSON Web Tokens. A CWT is a Claims Set encoded as
CBOR and wrapped in a COSE message (COSE_Sign1 or COSE_Mac0 in the
current set of supported wrappers).

[rfc8392]: https://www.rfc-editor.org/rfc/rfc8392

This module is a thin layer over [`encoding.cose`](../cose/README.md)
— all the cryptography is handled there. CWT-specific concerns are:

- Modelling the standard claims (`iss`, `sub`, `aud`, `exp`, `nbf`,
  `iat`, `cti`) as typed fields on `ClaimsSet`.
- Serialising/parsing the CBOR claims map.
- Adding/removing the optional outer CBOR tag 61 (`tag_cwt`).

## Quick examples

### Sign a CWT (ES256)

```v
import encoding.cwt
import encoding.cose
import encoding.hex

fn main() {
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

	got := cwt.verify(token, pub_key)!
	assert (got.iss or { '' }) == 'coap://as.example.com'
}
```

### Verify a CWT and check expiry

`cwt.verify` only authenticates the token; the `exp` / `nbf` window
must be enforced by the application. Use `validate_time` right after
verification:

```v ignore
import time

claims := cwt.verify(token, pub_key)!
claims.validate_time(time.now().unix) or {
	if err is cwt.ClaimExpired { return error('token expired') }
	if err is cwt.ClaimNotYetValid { return error('token not yet valid') }
	return err
}
```

### MAC a CWT (HMAC 256/64)

```v ignore
key := cose.Key.symmetric(secret_bytes)
token := cwt.mac(claims, key, protected: cose.Headers{ algorithm: .hmac_256_64 })!
got := cwt.verify_mac(token, key)!
```

## Outer tag 61

The default for `sign` and `mac` is to wrap the COSE message in CBOR
tag 61 (the CWT marker, RFC 8392 §6). Set `tagged_cwt: false` in
options to emit just the inner COSE message — handy for contexts
where the surrounding protocol already disambiguates the payload
type. Verification accepts both forms.

## Standard claims modelled

| Claim | Label | V field   | Type   |
| ----- | ----- | --------- | ------ |
| iss   | 1     | `iss`     | `?string` |
| sub   | 2     | `sub`     | `?string` |
| aud   | 3     | `aud`     | `[]string` (single = string form on the wire) |
| exp   | 4     | `exp`     | `?i64` Unix seconds |
| nbf   | 5     | `nbf`     | `?i64` |
| iat   | 6     | `iat`     | `?i64` |
| cti   | 7     | `cti`     | `?[]u8` |

Application-specific claims go into `extra_int_claims` /
`extra_text_claims`, with raw `cbor.Value` payloads.

## Time handling

`exp`, `nbf` and `iat` are decoded into `i64` Unix seconds. Encoded
output uses the integer form of NumericDate (RFC 8392 §3); the
fractional form (CBOR float) is accepted on decode for interop.

`validate_time(now_unix i64)` does the standard `nbf` ≤ `now` < `exp`
check and returns a `ClaimExpired` or `ClaimNotYetValid` typed error.
The check is *not* run automatically by `verify` / `verify_mac` so
that callers stay in control of the clock source (real time, fixed
test time, replay-safe context, etc.).

For one-off boolean checks: `claims.expired(now_unix)` and
`claims.not_yet_valid(now_unix)`.

## See also

- [`encoding.cose`](../cose/README.md) — the underlying signature/MAC
  primitives.
- [`encoding.cbor`](../cbor/README.md) — the CBOR codec.
