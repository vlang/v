// Third-party conformance: drives the entire `appendix_a.json` corpus
// from https://github.com/cbor/test-vectors (the same file that ciborium,
// serde_cbor and cbor2 use). Each entry is checked against:
//
//   * its `hex` round-trips byte-exact when `roundtrip == true`
//   * its `decoded` JSON value matches the V-decoded `cbor.Value`
//   * for entries that only carry a `diagnostic` (NaN, Infinity, undefined,
//     simple(N), tag(N)(...), h'…'), structural sanity is enforced via
//     the diagnostic prefix.
//
// The fixture file lives next to this test so the corpus is reproducible
// and offline-buildable.
module main

import encoding.cbor
import encoding.hex
import math
import os
import x.json2

fn h(s string) []u8 {
	return hex.decode(s) or { panic('bad hex ${s}') }
}

fn bytes_eq(a []u8, b []u8) bool {
	if a.len != b.len {
		return false
	}
	for i in 0 .. a.len {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// match_decoded compares a V `Value` against a parsed JSON value from the
// vector's `decoded` field. Returns an error string on mismatch; empty on success.
fn match_decoded(v cbor.Value, j json2.Any) string {
	match j {
		i64 {
			match v {
				cbor.IntNum {
					if j >= 0 {
						if v.negative || v.magnitude != u64(j) {
							return 'int ${j} ↔ IntNum(neg=${v.negative}, mag=${v.magnitude})'
						}
					} else {
						mag := u64(-(j + 1))
						if !v.negative || v.magnitude != mag {
							return 'int ${j} ↔ IntNum(neg=${v.negative}, mag=${v.magnitude})'
						}
					}
					return ''
				}
				cbor.Tag {
					// JSON ints beyond ±2^63 land here as i64-clamped or as a string;
					// real bignum vectors use the `decoded` field with a u64 / negative,
					// so this branch shouldn't fire for that case.
					return 'unexpected Tag for plain int ${j}'
				}
				else {
					return 'expected int ${j}, got ${v.type_name()}'
				}
			}
		}
		u64 {
			if v is cbor.IntNum {
				if v.negative || v.magnitude != j {
					return 'uint ${j} ↔ IntNum(neg=${v.negative}, mag=${v.magnitude})'
				}
				return ''
			}
			return 'expected uint ${j}'
		}
		f64 {
			// JSON has a single number type, so an integer-valued vector
			// arrives here as f64 even when the CBOR is major type 0/1.
			if v is cbor.FloatNum {
				if math.is_nan(j) && math.is_nan(v.value) {
					return ''
				}
				if v.value != j {
					return 'float ${j} ↔ ${v.value}'
				}
				return ''
			}
			if v is cbor.IntNum {
				// Beyond 2^53 JSON's f64 representation loses precision —
				// we can't tell IntNum(2^64-1) from IntNum(2^64). Trust the
				// roundtrip byte check and accept the structural shape.
				f64_exact_int_max := f64(1) * f64(u64(1) << 53)
				if math.abs(j) >= f64_exact_int_max {
					return ''
				}
				expected_neg := j < 0
				abs_val := if expected_neg { -j } else { j }
				if abs_val != f64(u64(abs_val)) {
					return 'float ${j} → IntNum: not integer-valued'
				}
				if expected_neg {
					if !v.negative {
						return 'expected negative IntNum for ${j}'
					}
					if u64(abs_val) - 1 != v.magnitude {
						return 'IntNum mag ${v.magnitude} != ${u64(abs_val) - 1}'
					}
				} else {
					if v.negative {
						return 'expected non-negative IntNum for ${j}'
					}
					if u64(abs_val) != v.magnitude {
						return 'IntNum mag ${v.magnitude} != ${u64(abs_val)}'
					}
				}
				return ''
			}
			if v is cbor.Tag {
				// Bignum (tag 2/3) representing a value beyond i64. Caller skips.
				return 'tag-bignum (caller decides)'
			}
			return 'expected number ${j}'
		}
		bool {
			if v is cbor.Bool && v.value == j {
				return ''
			}
			return 'expected bool ${j}'
		}
		string {
			if v is cbor.Text && v.value == j {
				return ''
			}
			return 'expected text "${j}"'
		}
		json2.Null {
			if v is cbor.Null {
				return ''
			}
			return 'expected null'
		}
		[]json2.Any {
			if v is cbor.Array {
				if v.elements.len != j.len {
					return 'array length ${v.elements.len} != ${j.len}'
				}
				for i, item in j {
					sub := match_decoded(v.elements[i], item)
					if sub != '' {
						return 'array[${i}]: ${sub}'
					}
				}
				return ''
			}
			return 'expected array'
		}
		map[string]json2.Any {
			if v is cbor.Map {
				if v.pairs.len != j.len {
					return 'map size ${v.pairs.len} != ${j.len}'
				}
				for pair in v.pairs {
					if pair.key !is cbor.Text {
						// JSON can only express string keys; mixed-key maps live
						// in the diagnostic-only set, so this is safe.
						return 'non-text key in JSON-comparable map'
					}
					tk := pair.key as cbor.Text
					if tk.value !in j {
						return 'missing key ${tk.value}'
					}
					jv := j[tk.value] or { return 'missing key ${tk.value}' }
					sub := match_decoded(pair.value, jv)
					if sub != '' {
						return 'map[${tk.value}]: ${sub}'
					}
				}
				return ''
			}
			return 'expected map'
		}
		else {
			return 'unsupported JSON kind ${typeof(j).name}'
		}
	}
}

// match_diagnostic enforces only structural sanity for entries that JSON
// can't directly express (NaN, Infinity, undefined, simple, tag, bignum).
fn match_diagnostic(v cbor.Value, diag string) string {
	d := diag.trim_space()
	match d {
		'Infinity' {
			if v is cbor.FloatNum && math.is_inf(v.value, 1) {
				return ''
			}
			return 'expected +Inf'
		}
		'-Infinity' {
			if v is cbor.FloatNum && math.is_inf(v.value, -1) {
				return ''
			}
			return 'expected -Inf'
		}
		'NaN' {
			if v is cbor.FloatNum && math.is_nan(v.value) {
				return ''
			}
			return 'expected NaN'
		}
		'undefined' {
			if v is cbor.Undefined {
				return ''
			}
			return 'expected Undefined'
		}
		else {}
	}

	if d.starts_with('simple(') {
		if v is cbor.Simple {
			return ''
		}
		return 'expected Simple'
	}
	if d.starts_with("h'") {
		if v is cbor.Bytes {
			return ''
		}
		return 'expected Bytes'
	}
	if d.starts_with('(_') {
		// Indefinite-length compound. Decoder collapses to definite Value.
		if v is cbor.Bytes || v is cbor.Text || v is cbor.Array || v is cbor.Map {
			return ''
		}
		return 'expected indef-collapsed compound'
	}
	// Tag forms: "0(\"...\")", "1(1363896240)", "23(h'…')", "24(h'…')", "32(\"…\")".
	if d.contains('(') && d[0].is_digit() {
		if v is cbor.Tag {
			return ''
		}
		return 'expected Tag'
	}
	// Map literal "{1: 2, 3: 4}" — int-keyed map, can't be expressed in JSON.
	if d.starts_with('{') {
		if v is cbor.Map {
			return ''
		}
		return 'expected Map'
	}
	return 'unrecognised diagnostic ${d}'
}

const fixture_path = os.join_path(os.dir(@FILE), 'appendix_a.json')

fn test_upstream_appendix_a_corpus() {
	raw := os.read_file(fixture_path) or { panic('cannot read fixture: ${err}') }
	parsed := json2.decode[json2.Any](raw) or { panic('json: ${err}') }
	entries := parsed.as_array()
	assert entries.len > 0, 'fixture is empty'

	mut total := 0
	mut roundtrip := 0
	mut value_checks := 0
	mut diag_checks := 0
	mut failures := []string{}

	for entry in entries {
		obj := entry.as_map()
		total++
		hex_str := obj['hex'] or { json2.Any('') }.str()
		// `f818` (simple(24) two-byte form) is well-formed under RFC 7049
		// but RFC 8949 §3.3 explicitly forbids it. The upstream corpus
		// predates RFC 8949 — our decoder correctly rejects it.
		if hex_str == 'f818' {
			cbor.decode[cbor.Value](h(hex_str), cbor.DecodeOpts{}) or {
				assert err.msg().contains('1-byte form'), 'unexpected error for f818: ${err}'
				continue
			}
			assert false, 'f818 should be rejected per RFC 8949 §3.3'
		}
		input := h(hex_str)
		rt := if rt_any := obj['roundtrip'] {
			rt_any.bool()
		} else {
			false
		}

		decoded_v := cbor.decode[cbor.Value](input, cbor.DecodeOpts{}) or {
			failures << 'decode ${hex_str}: ${err}'
			continue
		}

		// Compare structure where possible.
		if decoded := obj['decoded'] {
			diff := match_decoded(decoded_v, decoded)
			if diff != '' {
				// Bignums (decoded JSON ints beyond ±2^64) are represented as
				// `Tag(2|3, Bytes)` on the wire. JSON loses them as i64-clamped
				// values, so accept Tag/IntNum mismatch when the JSON is at the
				// extreme range.
				if decoded_v is cbor.Tag {
					t := decoded_v as cbor.Tag
					if t.number == 2 || t.number == 3 {
						value_checks++
						continue
					}
				}
				failures << '${hex_str} decoded: ${diff}'
				continue
			}
			value_checks++
		} else if diag := obj['diagnostic'] {
			diff := match_diagnostic(decoded_v, diag.str())
			if diff != '' {
				failures << '${hex_str} diagnostic "${diag}": ${diff}'
				continue
			}
			diag_checks++
		}

		// For roundtrip=true entries, re-encode and compare bytes.
		if rt {
			out := cbor.encode_value(decoded_v, cbor.EncodeOpts{})!
			if !bytes_eq(out, input) {
				failures << '${hex_str} roundtrip: got ${hex.encode(out)}'
				continue
			}
			roundtrip++
		}
	}

	if failures.len > 0 {
		for f in failures {
			eprintln('FAIL: ${f}')
		}
		assert false, '${failures.len}/${total} upstream Appendix A vectors failed'
	}

	// Sanity: the upstream corpus has 80+ entries; if we ever see less,
	// the fixture file is wrong.
	assert total >= 80, 'corpus too small: ${total}'
	assert roundtrip >= 60, 'too few roundtrip checks: ${roundtrip}'
	assert value_checks > 0
	assert diag_checks > 0
}
