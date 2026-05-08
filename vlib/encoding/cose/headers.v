// Header parameters as defined by RFC 9052 §3 ("Header Parameters") and
// the IANA "COSE Header Parameters" registry. The well-known integer
// labels relevant to signature and MAC processing are exposed as typed
// fields on `Headers`; callers can carry additional parameters
// (private-use, experimental, application-specific) through
// `extra_int_labels` and `extra_text_labels`.
module cose

import encoding.cbor

// Well-known integer labels from RFC 9052 §3.1, table 2.
// Kept as private constants so the on-the-wire encoding can change
// independently from the public API.
const label_alg = i64(1)
const label_crit = i64(2)
const label_content_type = i64(3)
const label_kid = i64(4)
const label_iv = i64(5)
const label_partial_iv = i64(6)

// Headers carries the header parameters of a COSE message. Each COSE
// message has two header buckets: the *protected* bucket (integrity-
// covered) and the *unprotected* bucket (informational only). Both use
// this type — the bucket they live in is decided by the message struct,
// not by Headers itself.
//
// All well-known parameters are optional. To omit a parameter from the
// encoded output, leave its field as `none` (or empty for the slice
// fields). Use `extra_int_labels` / `extra_text_labels` for parameters
// not modelled here.
pub struct Headers {
pub mut:
	// algorithm — label 1.
	algorithm ?Algorithm
	// critical — label 2. Lists integer labels that MUST appear in the
	// protected header and that the recipient MUST understand. Text-
	// labelled crit entries are accepted on decode (kept in the cbor
	// `Value` of the surrounding map) but not modelled here, since
	// real-world COSE deployments always use integer labels.
	critical []i64
	// content_type — label 3. RFC 9052 allows either a uint (CoAP
	// content-format) or a tstr (IANA media type). Set at most one of
	// these two fields; if both are set, the int form wins on encode.
	content_type_int  ?u64
	content_type_text ?string
	// kid — label 4. Application-level key identifier. Octet string.
	kid ?[]u8
	// iv — label 5. Full initialization vector for AEAD/MAC-with-IV
	// algorithms. Modelled here so messages produced by other COSE
	// implementations round-trip cleanly even though the current set of
	// supported algorithms (HMAC variants) does not consume it.
	iv ?[]u8
	// partial_iv — label 6. Partial IV; XOR'd with the context IV to
	// derive the effective IV.
	partial_iv ?[]u8
	// extra_int_labels carries integer-labelled parameters not covered
	// above. Order is preserved on encode so callers can produce stable
	// output, though COSE itself does not require any particular order
	// in *un*protected headers. Protected headers are always re-sorted
	// canonically on encode regardless.
	extra_int_labels []HeaderEntry
	// extra_text_labels carries text-labelled parameters (private use).
	extra_text_labels []TextHeaderEntry
}

// HeaderEntry is one (int label, value) pair. The value is held as a
// cbor.Value so any CBOR datum can be carried.
pub struct HeaderEntry {
pub:
	label i64
	value cbor.Value
}

// TextHeaderEntry is one (text label, value) pair.
pub struct TextHeaderEntry {
pub:
	label string
	value cbor.Value
}

// is_empty reports whether the Headers contains no parameters at all.
// An empty *protected* Headers serialises to a zero-length bstr (`0x40`)
// per RFC 9052 §3.
pub fn (h Headers) is_empty() bool {
	return h.algorithm == none && h.critical.len == 0 && h.content_type_int == none
		&& h.content_type_text == none && h.kid == none && h.iv == none && h.partial_iv == none
		&& h.extra_int_labels.len == 0 && h.extra_text_labels.len == 0
}

// to_value returns the Headers as a `cbor.Value` (always a `Map`),
// suitable for embedding in another CBOR structure via
// `Packer.pack_value`. Pair order is the same as on the wire after
// canonical sorting (the Packer sorts when `canonical = true`).
pub fn (h Headers) to_value() cbor.Value {
	mut pairs := []cbor.MapPair{cap: 7 + h.extra_int_labels.len + h.extra_text_labels.len}
	h.append_pairs(mut pairs)
	return cbor.Map{
		pairs: pairs
	}
}

// encode_map returns the canonical CBOR encoding of the Headers as a
// CBOR map (definite length, sorted keys per RFC 8949 §4.2.1). This is
// the form used inside both the protected bstr wrapper and the
// unprotected slot.
pub fn (h Headers) encode_map() ![]u8 {
	return cbor.encode(h.to_value(), cbor.EncodeOpts{ canonical: true })!
}

// append_pairs writes the CBOR map pairs for this Headers into `pairs`.
// Used internally by `to_value` and `encode_map` to share construction
// logic without allocating intermediates.
fn (h Headers) append_pairs(mut pairs []cbor.MapPair) {
	if alg := h.algorithm {
		pairs << cbor.MapPair{
			key:   cbor.new_int(label_alg)
			value: cbor.new_int(i64(alg))
		}
	}
	if h.critical.len > 0 {
		mut crit_arr := cbor.Array{}
		for c in h.critical {
			crit_arr.elements << cbor.new_int(c)
		}
		pairs << cbor.MapPair{
			key:   cbor.new_int(label_crit)
			value: crit_arr
		}
	}
	// content_type: uint takes precedence over tstr if both are set
	if ct := h.content_type_int {
		pairs << cbor.MapPair{
			key:   cbor.new_int(label_content_type)
			value: cbor.new_uint(ct)
		}
	} else if ct := h.content_type_text {
		pairs << cbor.MapPair{
			key:   cbor.new_int(label_content_type)
			value: cbor.new_text(ct)
		}
	}
	if kid := h.kid {
		pairs << cbor.MapPair{
			key:   cbor.new_int(label_kid)
			value: cbor.new_bytes(kid)
		}
	}
	if iv := h.iv {
		pairs << cbor.MapPair{
			key:   cbor.new_int(label_iv)
			value: cbor.new_bytes(iv)
		}
	}
	if piv := h.partial_iv {
		pairs << cbor.MapPair{
			key:   cbor.new_int(label_partial_iv)
			value: cbor.new_bytes(piv)
		}
	}
	for e in h.extra_int_labels {
		pairs << cbor.MapPair{
			key:   cbor.new_int(e.label)
			value: e.value
		}
	}
	for e in h.extra_text_labels {
		pairs << cbor.MapPair{
			key:   cbor.new_text(e.label)
			value: e.value
		}
	}
}

// encode_protected returns the bstr-wrapped canonical CBOR encoding of
// the protected headers, as used in the wire message (a CBOR byte string
// containing either an empty buffer or the CBOR map). RFC 9052 §3:
// "the empty map is encoded as a zero-length string rather than as a
// h'A0'".
pub fn (h Headers) encode_protected() ![]u8 {
	if h.is_empty() {
		return []u8{}
	}
	return h.encode_map()!
}

// parse_headers_map decodes a CBOR map (already extracted from the
// surrounding message) into a Headers value. Unknown labels go into
// `extra_int_labels` / `extra_text_labels` instead of being dropped, so
// round-trips preserve the original parameters.
pub fn parse_headers_map(data []u8) !Headers {
	if data.len == 0 {
		return Headers{}
	}
	v := cbor.decode[cbor.Value](data, cbor.DecodeOpts{})!
	return parse_headers_value(v)!
}

// parse_headers_value is the same as parse_headers_map but takes an
// already-decoded cbor.Value. Used internally when the surrounding
// decoder has already consumed the bytes.
fn parse_headers_value(v cbor.Value) !Headers {
	if v is cbor.Map {
		mut h := Headers{}
		for pair in v.pairs {
			if int_key := pair.key.as_int() {
				match int_key {
					label_alg {
						code := pair.value.as_int() or {
							return MalformedMessage{
								reason: 'alg label has non-integer value'
							}
						}
						// Don't hard-fail when the algorithm isn't one
						// of the IANA values we model: it might be a
						// recipient routing marker (e.g. direct = -6
						// in COSE_Mac), or an algorithm we just don't
						// support yet. Surface it as an extra label
						// so the rest of the message can still be
						// inspected; high-level sign/verify routines
						// will return a clear error if the algorithm
						// is needed and unknown.
						if alg := algorithm_from_int(code) {
							h.algorithm = alg
						} else {
							h.extra_int_labels << HeaderEntry{
								label: label_alg
								value: pair.value
							}
						}
					}
					label_crit {
						items := pair.value.as_array() or {
							return MalformedMessage{
								reason: 'crit label is not an array'
							}
						}
						mut crit := []i64{cap: items.len}
						for item in items {
							c := item.as_int() or {
								return MalformedMessage{
									reason: 'crit array contains non-integer'
								}
							}
							crit << c
						}
						h.critical = crit
					}
					label_content_type {
						if u := pair.value.as_uint() {
							h.content_type_int = u
						} else if s := pair.value.as_string() {
							h.content_type_text = s
						} else {
							return MalformedMessage{
								reason: 'content type is neither uint nor tstr'
							}
						}
					}
					label_kid {
						b := pair.value.as_bytes() or {
							return MalformedMessage{
								reason: 'kid is not a byte string'
							}
						}
						h.kid = b
					}
					label_iv {
						b := pair.value.as_bytes() or {
							return MalformedMessage{
								reason: 'iv is not a byte string'
							}
						}
						h.iv = b
					}
					label_partial_iv {
						b := pair.value.as_bytes() or {
							return MalformedMessage{
								reason: 'partial iv is not a byte string'
							}
						}
						h.partial_iv = b
					}
					else {
						h.extra_int_labels << HeaderEntry{
							label: int_key
							value: pair.value
						}
					}
				}
			} else if str_key := pair.key.as_string() {
				h.extra_text_labels << TextHeaderEntry{
					label: str_key
					value: pair.value
				}
			} else {
				return MalformedMessage{
					reason: 'header label is neither int nor tstr'
				}
			}
		}
		return h
	}
	return MalformedMessage{
		reason: 'header bucket is not a CBOR map'
	}
}

// parse_protected decodes a protected header bstr (the unwrapped bytes,
// not the bstr itself). An empty buffer maps to an empty Headers.
pub fn parse_protected(data []u8) !Headers {
	if data.len == 0 {
		return Headers{}
	}
	return parse_headers_map(data)!
}

// check_critical enforces RFC 9052 §3.1: every integer label listed in
// `crit` MUST be one the receiver understands; otherwise verification
// MUST fail. We "understand" the IANA-registered labels 1..6 modelled
// by typed fields; any other label in `crit` is treated as a hard
// error to avoid silently ignoring a parameter the sender flagged as
// security-critical.
fn check_critical(h Headers) ! {
	for label in h.critical {
		if label !in [label_alg, label_crit, label_content_type, label_kid, label_iv,
			label_partial_iv] {
			return MalformedMessage{
				reason: 'crit lists unknown label ${label} (RFC 9052 §3.1)'
			}
		}
	}
}
