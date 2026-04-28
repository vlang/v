// Tests that `time.Time` auto-tags as RFC 8949 tag 1 (epoch seconds)
// on encode and accepts both tag 0 (RFC 3339 text) and tag 1 (epoch
// seconds, integer or float) on decode.
module main

import encoding.cbor
import encoding.hex
import time

fn h(s string) []u8 {
	return hex.decode(s) or { panic('invalid hex: ${s}') }
}

fn test_time_encode_tag1() {
	t := time.unix(1363896240)
	bytes := cbor.encode[time.Time](t, cbor.EncodeOpts{})!
	// Wire form: c1 (tag 1) + 1a 51 4b 67 b0 (4-byte uint).
	assert bytes == h('c11a514b67b0'), 'got ${hex.encode(bytes)}'
}

fn test_time_decode_tag1() {
	got := cbor.decode[time.Time](h('c11a514b67b0'), cbor.DecodeOpts{})!
	assert got.unix() == 1363896240
}

fn test_time_decode_tag0_iso8601() {
	// Tag 0 + text "2013-03-21T20:04:00Z".
	got :=
		cbor.decode[time.Time](h('c074323031332d30332d32315432303a30343a30305a'), cbor.DecodeOpts{})!
	assert got.unix() == 1363896240
}

fn test_time_decode_tag1_float() {
	got := cbor.decode[time.Time](h('c1fb41d452d9ec200000'), cbor.DecodeOpts{})!
	// 1363896240.5 — half-second offset.
	assert got.unix() == 1363896240
	// Sub-second component is non-zero (~500ms).
	assert got.nanosecond > 0
}
