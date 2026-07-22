// CWT Claims Set as defined by RFC 8392 §3 and §4. The well-known
// claims are exposed as typed fields; application-specific claims can
// be carried via `extra_int_claims` / `extra_text_claims`.
module cwt

import encoding.cbor

// Standard claim labels from RFC 8392 §3, table 1, and the IANA "CBOR
// Web Token (CWT) Claims" registry.
const claim_iss = i64(1)
const claim_sub = i64(2)
const claim_aud = i64(3)
const claim_exp = i64(4)
const claim_nbf = i64(5)
const claim_iat = i64(6)
const claim_cti = i64(7)

// CBOR tag 61 (RFC 8392 §6) marks a CWT, distinguishing it from a
// generic COSE message.
pub const tag_cwt = u64(61)

// ClaimsSet is the V representation of a CWT Claims Set (RFC 8392 §3).
// Time-valued claims (`exp`, `nbf`, `iat`) are stored as Unix seconds
// (i64). RFC 8392 also allows fractional NumericDate via CBOR floats;
// the encoder always emits the integer form (what every real-world
// deployment uses) but the decoder accepts both.
pub struct ClaimsSet {
pub mut:
	iss ?string
	sub ?string
	// aud — RFC 7519 / 8392 allow either a single string or an array.
	// We model both as a slice for uniformity: empty = no audience,
	// single-element = string form on the wire, multi-element = array.
	aud []string
	exp ?i64
	nbf ?i64
	iat ?i64
	cti ?[]u8
	// extra_int_claims carries unmodelled integer-labelled claims.
	extra_int_claims []ClaimEntry
	// extra_text_claims carries unmodelled text-labelled claims.
	extra_text_claims []TextClaimEntry
}

// ClaimEntry is one (int label, value) pair.
pub struct ClaimEntry {
pub:
	label i64
	value cbor.Value
}

// TextClaimEntry is one (text label, value) pair.
pub struct TextClaimEntry {
pub:
	label string
	value cbor.Value
}

// encode returns the canonical CBOR encoding of the claims set as a
// CBOR map. The output is the bytes that go into the
// `payload` slot of the surrounding COSE message.
pub fn (c ClaimsSet) encode() ![]u8 {
	mut pairs := []cbor.MapPair{cap: 8 + c.extra_int_claims.len + c.extra_text_claims.len}
	if iss := c.iss {
		pairs << cbor.MapPair{
			key:   cbor.new_int(claim_iss)
			value: cbor.new_text(iss)
		}
	}
	if sub := c.sub {
		pairs << cbor.MapPair{
			key:   cbor.new_int(claim_sub)
			value: cbor.new_text(sub)
		}
	}
	if c.aud.len == 1 {
		pairs << cbor.MapPair{
			key:   cbor.new_int(claim_aud)
			value: cbor.new_text(c.aud[0])
		}
	} else if c.aud.len > 1 {
		mut arr := cbor.Array{}
		for a in c.aud {
			arr.elements << cbor.new_text(a)
		}
		pairs << cbor.MapPair{
			key:   cbor.new_int(claim_aud)
			value: arr
		}
	}
	if exp := c.exp {
		pairs << cbor.MapPair{
			key:   cbor.new_int(claim_exp)
			value: cbor.new_int(exp)
		}
	}
	if nbf := c.nbf {
		pairs << cbor.MapPair{
			key:   cbor.new_int(claim_nbf)
			value: cbor.new_int(nbf)
		}
	}
	if iat := c.iat {
		pairs << cbor.MapPair{
			key:   cbor.new_int(claim_iat)
			value: cbor.new_int(iat)
		}
	}
	if cti := c.cti {
		pairs << cbor.MapPair{
			key:   cbor.new_int(claim_cti)
			value: cbor.new_bytes(cti)
		}
	}
	for e in c.extra_int_claims {
		pairs << cbor.MapPair{
			key:   cbor.new_int(e.label)
			value: e.value
		}
	}
	for e in c.extra_text_claims {
		pairs << cbor.MapPair{
			key:   cbor.new_text(e.label)
			value: e.value
		}
	}
	return cbor.encode(cbor.Value(cbor.Map{ pairs: pairs }), cbor.EncodeOpts{
		canonical: true
	})!
}

// ClaimsSet.decode parses a CBOR-encoded Claims Set. Unknown claims
// are kept in `extra_int_claims` / `extra_text_claims`.
pub fn ClaimsSet.decode(data []u8) !ClaimsSet {
	v := cbor.decode[cbor.Value](data, cbor.DecodeOpts{})!
	m := if v is cbor.Map {
		v
	} else {
		return error('cwt: claims set is not a CBOR map')
	}
	mut c := ClaimsSet{}
	for pair in m.pairs {
		if int_key := pair.key.as_int() {
			match int_key {
				claim_iss {
					c.iss = pair.value.as_string() or { return error('cwt: iss is not text') }
				}
				claim_sub {
					c.sub = pair.value.as_string() or { return error('cwt: sub is not text') }
				}
				claim_aud {
					if s := pair.value.as_string() {
						c.aud = [s]
					} else if items := pair.value.as_array() {
						mut auds := []string{cap: items.len}
						for it in items {
							s := it.as_string() or {
								return error('cwt: aud array contains non-text')
							}
							auds << s
						}
						c.aud = auds
					} else {
						return error('cwt: aud is neither text nor array of text')
					}
				}
				claim_exp {
					c.exp = decode_numeric_date(pair.value)!
				}
				claim_nbf {
					c.nbf = decode_numeric_date(pair.value)!
				}
				claim_iat {
					c.iat = decode_numeric_date(pair.value)!
				}
				claim_cti {
					c.cti = pair.value.as_bytes() or { return error('cwt: cti is not bstr') }
				}
				else {
					c.extra_int_claims << ClaimEntry{
						label: int_key
						value: pair.value
					}
				}
			}
		} else if str_key := pair.key.as_string() {
			c.extra_text_claims << TextClaimEntry{
				label: str_key
				value: pair.value
			}
		} else {
			return error('cwt: claim label is neither int nor text')
		}
	}
	return c
}

// decode_numeric_date accepts the integer or float forms of NumericDate
// (RFC 8392 §3) and returns Unix seconds as i64.
fn decode_numeric_date(v cbor.Value) !i64 {
	if i := v.as_int() {
		return i
	}
	if f := v.as_float() {
		return i64(f)
	}
	return error('cwt: NumericDate is neither integer nor float')
}

// expired reports whether the claims set's `exp` (expiration time)
// is at or before `now_unix`. Returns `false` when `exp` is absent —
// per RFC 8392 §3.1.4 a CWT without `exp` does not expire on its own.
//
// Pass `time.now().unix` for the wall-clock check; pass a fixed
// timestamp for unit tests or replay-safe contexts.
pub fn (c ClaimsSet) expired(now_unix i64) bool {
	exp := c.exp or { return false }
	return now_unix >= exp
}

// not_yet_valid reports whether the claims set's `nbf` (not-before)
// is in the future relative to `now_unix`. Returns `false` when
// `nbf` is absent.
pub fn (c ClaimsSet) not_yet_valid(now_unix i64) bool {
	nbf := c.nbf or { return false }
	return now_unix < nbf
}

// validate_time runs the `nbf`/`exp` checks against `now_unix` and
// returns a typed error when the token is outside its validity window,
// or `none` when both checks pass (or the relevant claims are absent).
// This is the convenience helper most application code wants right
// after `cwt.verify(...)`.
pub fn (c ClaimsSet) validate_time(now_unix i64) ! {
	if c.not_yet_valid(now_unix) {
		return ClaimNotYetValid{
			nbf: c.nbf or { 0 }
			now: now_unix
		}
	}
	if c.expired(now_unix) {
		return ClaimExpired{
			exp: c.exp or { 0 }
			now: now_unix
		}
	}
}

// ClaimExpired is returned by `validate_time` when `now >= exp`.
pub struct ClaimExpired {
	Error
pub:
	exp i64
	now i64
}

// msg formats a ClaimExpired for `IError.msg()`.
pub fn (e &ClaimExpired) msg() string {
	return 'cwt: token expired (exp=${e.exp}, now=${e.now})'
}

// ClaimNotYetValid is returned by `validate_time` when `now < nbf`.
pub struct ClaimNotYetValid {
	Error
pub:
	nbf i64
	now i64
}

// msg formats a ClaimNotYetValid for `IError.msg()`.
pub fn (e &ClaimNotYetValid) msg() string {
	return 'cwt: token not yet valid (nbf=${e.nbf}, now=${e.now})'
}
