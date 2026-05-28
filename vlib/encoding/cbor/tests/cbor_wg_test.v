// CBOR Working Group conformance corpus
// (https://github.com/cbor-wg/cbor-test-vectors).
//
// Two fixture files live next to this test:
//
//   * `cbor_wg/rfc8949_good.edn` — 88 well-formed payloads. Each must
//     decode without error.
//   * `cbor_wg/rfc8949_bad.edn`  — 47 malformed payloads. Each MUST be
//     rejected by the decoder per RFC 8949 §3.
//
// The fixtures are EDN (CBOR Diagnostic Notation, RFC 8610) — we only
// pull the `"encoded": h'…'` hex literals because that's what we need
// to drive the decoder. The expected `decoded` value is left to the
// other test files (rfc8949_appendix_a, upstream_appendix_a) which use
// the JSON-encoded corpus.
module main

import encoding.cbor
import encoding.hex
import os

const wg_dir = os.join_path(os.dir(@FILE), 'cbor_wg')

// extract_hex_literals pulls every `"encoded": h'…'` value out of an EDN
// file. The hex string can contain whitespace (visual grouping per
// RFC 8610) — we strip it before decoding.
fn extract_hex_literals(text string) []string {
	mut out := []string{}
	mut i := 0
	needle := '"encoded": h\''
	for {
		idx := text.index_after(needle, i) or { break }
		start := idx + needle.len
		end := text.index_after("'", start) or { break }
		raw := text[start..end]
		mut clean := []u8{cap: raw.len}
		for c in raw {
			if c == ` ` || c == `\t` || c == `\n` || c == `\r` {
				continue
			}
			clean << c
		}
		out << clean.bytestr()
		i = end + 1
	}
	return out
}

fn test_extractor_sanity() {
	good := os.read_file(os.join_path(wg_dir, 'rfc8949_good.edn')) or {
		panic('cannot read good.edn: ${err}')
	}
	bad := os.read_file(os.join_path(wg_dir, 'rfc8949_bad.edn')) or {
		panic('cannot read bad.edn: ${err}')
	}
	good_hexes := extract_hex_literals(good)
	bad_hexes := extract_hex_literals(bad)
	assert good_hexes.len == 88, 'good corpus drift: ${good_hexes.len} (expected 88)'
	assert bad_hexes.len == 47, 'bad corpus drift: ${bad_hexes.len} (expected 47)'
}

fn test_cbor_wg_good_corpus() {
	text := os.read_file(os.join_path(wg_dir, 'rfc8949_good.edn'))!
	hexes := extract_hex_literals(text)
	// The corpus deliberately stresses 256+ deep nesting; raise the cap.
	opts := cbor.DecodeOpts{
		max_depth: 4096
	}
	mut failures := []string{}
	for hex_str in hexes {
		bytes := hex.decode(hex_str) or {
			failures << '${hex_str}: hex decode: ${err}'
			continue
		}
		cbor.decode[cbor.Value](bytes, opts) or {
			failures << '${hex_str}: ${err}'
			continue
		}
	}
	if failures.len > 0 {
		for f in failures {
			eprintln('GOOD-FAIL: ${f}')
		}
		assert false, '${failures.len}/${hexes.len} good vectors rejected'
	}
}

// Per-major-type files (mt0..mt7) and the streaming/indefinite suite all
// hold well-formed entries. Each must decode without error.
const mt_files = [
	'appA_mt0.edn',
	'appA_mt1.edn',
	'appA_mt2.edn',
	'appA_mt3.edn',
	'appA_mt4.edn',
	'appA_mt5.edn',
	'appA_mt6.edn',
	'appA_mt7-float.edn',
	'appA_mt7-simple.edn',
	'appA_streaming.edn',
]

fn test_cbor_wg_per_major_type_corpus() {
	mut total := 0
	mut failures := []string{}
	for fname in mt_files {
		text := os.read_file(os.join_path(wg_dir, fname)) or {
			panic('cannot read ${fname}: ${err}')
		}
		hexes := extract_hex_literals(text)
		assert hexes.len > 0, '${fname} has no entries'
		for hex_str in hexes {
			total++
			bytes := hex.decode(hex_str) or {
				failures << '${fname} ${hex_str}: hex: ${err}'
				continue
			}
			cbor.decode[cbor.Value](bytes, cbor.DecodeOpts{}) or {
				failures << '${fname} ${hex_str}: ${err}'
				continue
			}
		}
	}
	if failures.len > 0 {
		for f in failures {
			eprintln('MT-FAIL: ${f}')
		}
		assert false, '${failures.len}/${total} per-major-type vectors rejected'
	}
	assert total >= 80, 'corpus too small: ${total}'
}

fn test_cbor_wg_bad_corpus() {
	text := os.read_file(os.join_path(wg_dir, 'rfc8949_bad.edn'))!
	hexes := extract_hex_literals(text)
	mut accepted_anyway := []string{}
	for hex_str in hexes {
		bytes := hex.decode(hex_str) or {
			// Malformed at the hex layer is still a rejection; skip.
			continue
		}
		if v := cbor.decode[cbor.Value](bytes, cbor.DecodeOpts{}) {
			accepted_anyway << '${hex_str} → ${v.type_name()}'
		}
	}
	if accepted_anyway.len > 0 {
		for a in accepted_anyway {
			eprintln('BAD-ACCEPTED: ${a}')
		}
		assert false, '${accepted_anyway.len}/${hexes.len} malformed vectors were not rejected'
	}
}
