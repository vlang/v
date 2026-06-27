# `encoding.cose`

CBOR Object Signing and Encryption — pure-V implementation of the
signing and MAC subset of [RFC 9052][rfc9052] and [RFC 9053][rfc9053].

[rfc9052]: https://www.rfc-editor.org/rfc/rfc9052
[rfc9053]: https://www.rfc-editor.org/rfc/rfc9053

## What it covers

| Message type     | Tag | Status |
| ---------------- | --- | ------ |
| `COSE_Sign1`     | 18  | ✅     |
| `COSE_Sign`      | 98  | ✅     |
| `COSE_Mac0`      | 17  | ✅     |
| `COSE_Mac`       | 97  | ✅ (direct mode) |
| `COSE_Encrypt0`  | 16  | ❌ (needs AEAD primitives in `vlib/crypto`) |
| `COSE_Encrypt`   | 96  | ❌ (idem) |

| Algorithm     | IANA | Status |
| ------------- | ---- | ------ |
| `ES256`       | -7   | ✅ |
| `ES384`       | -35  | ✅ |
| `ES512`       | -36  | ✅ (P-521 + SHA-512) |
| `EdDSA`       | -8   | ✅ Ed25519 |
| `HMAC 256/64` | 4    | ✅ |
| `HMAC 256/256`| 5    | ✅ |
| `HMAC 384/384`| 6    | ✅ |
| `HMAC 512/512`| 7    | ✅ |

## Quick examples

### Sign and verify a payload (COSE_Sign1, ES256)

The most common pattern — single-signer ECDSA P-256 over SHA-256.

```v
import encoding.cose
import encoding.hex

fn main() {
	x := hex.decode('143329cce7868e416927599cf65a34f3ce2ffda55a7eca69ed8919a394d42f0f')!
	y := hex.decode('60f7f1a780d8a783bfb7a2dd6b2796e8128dbbcef9d3d168db9529971a36e7b9')!
	d := hex.decode('6c1382765aec5358f117733d281c1c7bdc39884d04a45a1e6c67c858bc206c19')!
	priv := cose.Key.ec2_private(.p_256, x, y, d)
	pub_key := cose.Key.ec2_public(.p_256, x, y)

	signed := cose.sign1('hello'.bytes(), priv,
		protected: cose.Headers{
			algorithm: .es256
		}
	)!
	payload := cose.verify1(signed, pub_key)!
	assert payload == 'hello'.bytes()
}
```

### Sign and verify with EdDSA (Ed25519)

EdDSA is deterministic — the same payload + key always produces the
same signature, useful for caching and reproducible builds.

```v
import encoding.cose
import encoding.hex

fn main() {
	d := hex.decode('9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60')!
	x := hex.decode('d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a')!
	priv := cose.Key.okp_private(.ed25519, x, d)
	pub_key := cose.Key.okp_public(.ed25519, x)

	signed := cose.sign1('hello'.bytes(), priv,
		protected: cose.Headers{
			algorithm: .eddsa
		}
	)!
	payload := cose.verify1(signed, pub_key)!
	assert payload == 'hello'.bytes()
}
```

### MAC a payload (COSE_Mac0, HMAC-SHA256)

```v
import encoding.cose

fn main() {
	key := cose.Key.symmetric([u8(0x42)].repeat(32))
	tag := cose.mac0('payload'.bytes(), key,
		protected: cose.Headers{
			algorithm: .hmac_256_256
		}
	)!
	got := cose.verify_mac0(tag, key)!
	assert got == 'payload'.bytes()
}
```

### Multi-signer (COSE_Sign)

```v ignore
a := cose.Signer{
	key:       alice_key
	protected: cose.Headers{ algorithm: .eddsa }
}
b := cose.Signer{
	key:       bob_key
	protected: cose.Headers{ algorithm: .es256 }
}
msg := cose.sign('payload'.bytes(), [a, b])!
```

## Cookbook

### Verifying a webhook payload

```v ignore
import encoding.cose

fn handle_webhook(body []u8, pub_key cose.Key) ! {
	payload := cose.verify1(body, pub_key) or {
		match err {
			cose.VerificationFailed { return error('webhook signature invalid') }
			cose.MalformedMessage   { return error('webhook bytes malformed') }
			else                    { return err }
		}
	}
	process(payload)!
}
```

### Setting `kid` (key identifier) so the verifier can pick the right key

The `kid` lives in the *unprotected* header — it's just routing info,
not security-critical, and can change without breaking the signature.

```v ignore
signed := cose.sign1(body, priv,
	protected:   cose.Headers{ algorithm: .es256 }
	unprotected: cose.Headers{ kid: 'k-2026-01'.bytes() }
)!
```

On the verify side, decode first, look up the key by `kid`, then verify:

```v ignore
msg := cose.Sign1Message.decode(signed)!
key := key_lookup(msg.unprotected.kid or { return error('no kid') })!
msg.verify(key, msg.payload or { []u8{} }, []u8{})!
```

### Detached payload (sign without embedding the bytes)

Useful when the payload is large or already transmitted out of band:

```v ignore
sig_only := cose.sign1([]u8{}, priv,
	protected:        cose.Headers{ algorithm: .es256 }
	detached_payload: large_blob
)!
// Later, on the receiving side:
cose.verify1(sig_only, pub_key, detached_payload: large_blob)!
```

## API surface

- `cose.Algorithm` — typed enum mapped to the IANA registry.
- `cose.Key` — COSE_Key with constructors `Key.ec2_*`, `Key.okp_*`,
  `Key.symmetric`. CBOR encode/decode via `key.encode()` /
  `Key.decode()`.
- `cose.Headers` — typed protected/unprotected header bag. Well-known
  parameters as fields, others via `extra_int_labels` /
  `extra_text_labels`. Always serialised in canonical CBOR order.
- `cose.sign1` / `cose.verify1` — single-signer convenience helpers.
- `cose.sign` / `cose.SignMessage` — multi-signer.
- `cose.mac0` / `cose.verify_mac0` — single-recipient MAC.
- `cose.mac` / `cose.verify_mac` — multi-recipient MAC (direct mode).

Error variants: `VerificationFailed`, `MalformedMessage`,
`AlgorithmMismatch`, `UnsupportedAlgorithm`. Use `if err is X` to
discriminate.

## See also

- [`encoding.cbor`](../cbor/README.md) — the CBOR codec underneath.
- [`encoding.cwt`](../cwt/README.md) — CBOR Web Tokens (RFC 8392)
  built on top of this module.
