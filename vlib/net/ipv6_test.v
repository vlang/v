module net

import os

// Conformance tests for RFC 5952 (IPv6 text representation).
//
// Each vector is `(input_form, expected_canonical)`. Inputs include
// non-canonical legitimate forms (uppercase, leading zeros, partial
// "::" placement) so that canonical_ipv6() also exercises the parser.

struct V6Vec {
	input    string
	expected string
}

const rfc5952_vectors = [
	// -------- §4.1 Leading zeros MUST be suppressed --------
	V6Vec{'2001:0db8:0000:0000:0000:0000:0000:0001', '2001:db8::1'},
	V6Vec{'0000:0000:0000:0000:0000:0000:0000:0000', '::'},
	V6Vec{'0000:0000:0000:0000:0000:0000:0000:0001', '::1'},
	// "A single 16-bit 0000 field MUST be represented as 0."
	V6Vec{'2001:0db8:0000:0001:0001:0001:0001:0001', '2001:db8:0:1:1:1:1:1'},
	// -------- §4.2.1 Shorten as much as possible --------
	V6Vec{'2001:db8:0:0:0:0:2:1', '2001:db8::2:1'},
	// Counter-example from the RFC: "2001:db8::0:1 is not acceptable".
	V6Vec{'2001:db8:0:0:0:0:0:1', '2001:db8::1'},
	// -------- §4.2.2 "::" MUST NOT shorten just one 16-bit 0 field --------
	// "2001:db8::1:1:1:1:1 is not correct" — single 0, leave as 0.
	V6Vec{'2001:db8:0:1:1:1:1:1', '2001:db8:0:1:1:1:1:1'},
	// Several singletons, none shortened:
	V6Vec{'1:0:1:0:1:0:1:0', '1:0:1:0:1:0:1:0'},
	V6Vec{'0:1:0:1:0:1:0:1', '0:1:0:1:0:1:0:1'},
	// -------- §4.2.3 Longest run wins; ties → first run --------
	// Three zeros (0,0,0) at positions 1..3 outweighs two zeros at 4..5.
	V6Vec{'2001:0:0:0:1:0:0:1', '2001::1:0:0:1'},
	// Tie: two equal pairs (0,0) at positions 2..3 and 5..6 — first wins.
	V6Vec{'2001:db8:0:0:1:0:0:1', '2001:db8::1:0:0:1'},
	// Earlier example from §4.2.3 with three trailing zero fields:
	V6Vec{'2001:0:0:1:0:0:0:1', '2001:0:0:1::1'},
	// -------- §4.3 Lowercase --------
	V6Vec{'2001:0DB8:AC10:FE01:0000:0000:0000:0000', '2001:db8:ac10:fe01::'},
	V6Vec{'FE80::1', 'fe80::1'},
	V6Vec{'FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF', 'ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff'},
	// -------- §5 IPv4-mapped (::ffff:0:0/96) → mixed notation --------
	V6Vec{'0:0:0:0:0:ffff:c000:0201', '::ffff:192.0.2.1'},
	V6Vec{'::ffff:192.0.2.1', '::ffff:192.0.2.1'},
	V6Vec{'0000:0000:0000:0000:0000:FFFF:0A00:0001', '::ffff:10.0.0.1'},
	V6Vec{'::ffff:0.0.0.0', '::ffff:0.0.0.0'},
	V6Vec{'::ffff:255.255.255.255', '::ffff:255.255.255.255'},
	// -------- Run at start (>=2) --------
	V6Vec{'0:0:0:0:0:0:0:2', '::2'},
	V6Vec{'0:0:0:0:0:0:1:2', '::1:2'},
	V6Vec{'0:0:c:d:e:f:1:2', '::c:d:e:f:1:2'},
	// -------- Run at end (>=2) --------
	V6Vec{'1::', '1::'},
	V6Vec{'1:0:0:0:0:0:0:0', '1::'},
	V6Vec{'a:b:c:d:e:f:0:0', 'a:b:c:d:e:f::'},
	V6Vec{'2001:db8:ac10:fe01::', '2001:db8:ac10:fe01::'},
	// -------- Run in the middle --------
	V6Vec{'2001:db8::1', '2001:db8::1'},
	V6Vec{'fe80:0:0:0:0:0:0:1', 'fe80::1'},
	// -------- Loopback / unspecified --------
	V6Vec{'::1', '::1'},
	V6Vec{'::', '::'},
	// -------- No zero field at all --------
	V6Vec{'1:2:3:4:5:6:7:8', '1:2:3:4:5:6:7:8'},
	V6Vec{'2001:db8:1:2:3:4:5:6', '2001:db8:1:2:3:4:5:6'},
	// -------- Mixed leading-zero suppression with longer/shorter values --------
	V6Vec{'0001:0023:0456:7890:abcd:00ef:0:1', '1:23:456:7890:abcd:ef:0:1'},
	// -------- Two equal-length non-trivial runs (§4.2.3 tie) --------
	V6Vec{'0:0:1:2:3:4:0:0', '::1:2:3:4:0:0'},
	// -------- Run of length exactly 2 still triggers compression --------
	V6Vec{'1:2:0:0:3:4:5:6', '1:2::3:4:5:6'},
	// -------- Adjacent zero singletons: NEITHER must be compressed when
	//          a longer run exists --------
	V6Vec{'0:1:0:0:0:1:0:1', '0:1::1:0:1'},
	// -------- Parser robustness --------
	V6Vec{'2001:DB8::AB:CD', '2001:db8::ab:cd'},
	V6Vec{'::ffff:192.000.002.001', '::ffff:192.0.2.1'},
]!

fn test_rfc5952_vectors() {
	mut failures := []string{}
	for v in rfc5952_vectors {
		got := canonical_ipv6(v.input) or {
			failures << '${v.input} -> error: ${err}'
			continue
		}
		if got != v.expected {
			failures << '${v.input} -> got ${got}, want ${v.expected}'
		}
	}
	if failures.len > 0 {
		for f in failures {
			eprintln('  FAIL: ${f}')
		}
		assert false, '${failures.len}/${rfc5952_vectors.len} RFC 5952 vectors failed'
	}
}

fn test_from_bytes_basic() {
	zeros := []u8{len: 16}
	assert canonical_ipv6_from_bytes(zeros)! == '::'

	mut loop := []u8{len: 16}
	loop[15] = 1
	assert canonical_ipv6_from_bytes(loop)! == '::1'

	bytes := [u8(0x20), 0x01, 0x0d, 0xb8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
	assert canonical_ipv6_from_bytes(bytes)! == '2001:db8::1'

	mapped := [u8(0), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0xff, 0xff, 192, 0, 2, 1]
	assert canonical_ipv6_from_bytes(mapped)! == '::ffff:192.0.2.1'
}

fn test_from_bytes_rejects_wrong_length() {
	if _ := canonical_ipv6_from_bytes([u8(0)]) {
		assert false, 'must reject 1-byte input'
	}
	if _ := canonical_ipv6_from_bytes([]u8{len: 15}) {
		assert false, 'must reject 15-byte input'
	}
	if _ := canonical_ipv6_from_bytes([]u8{len: 17}) {
		assert false, 'must reject 17-byte input'
	}
}

fn test_canonical_is_idempotent() {
	for v in rfc5952_vectors {
		once := canonical_ipv6(v.input) or {
			assert false, 'first pass failed on ${v.input}: ${err}'
			continue
		}
		twice := canonical_ipv6(once) or {
			assert false, 'second pass failed on ${once}: ${err}'
			continue
		}
		assert once == twice, 'idempotence broken: ${v.input} -> ${once} -> ${twice}'
	}
}

fn test_parser_rejects_malformed() {
	bad := [
		'',
		'gggg::1', // non-hex
		'1::2::3', // two "::"
		'1:2:3:4:5:6:7:8:9', // too many groups, no "::"
		'1:2:3:4:5:6:7', // too few groups, no "::"
		'1:2:3:4:5:6:7:8::', // "::" with already 8 groups
		'12345::1', // group too long
		'1:2:3:4:5:6:1.2.3', // bad dotted quad (3 octets)
		'1:2:3:4:5:6:1.2.3.4.5', // bad dotted quad (5 octets)
		'::ffff:1.2.3.999', // octet > 255
		'::ffff:1.2.3.', // trailing dot in v4
	]
	for s in bad {
		if _ := canonical_ipv6(s) {
			assert false, 'parser must reject "${s}"'
		}
	}
}

// test_inet_ntop_corpus loads testdata/ipv6_rfc5952.tsv (113 vectors
// generated by Python's ipaddress.IPv6Address.compressed) and confirms
// our pure-V implementation produces byte-identical output.
fn test_inet_ntop_corpus() {
	path := os.join_path(os.dir(@FILE), 'testdata', 'ipv6_rfc5952.tsv')
	body := os.read_file(path) or {
		assert false, 'cannot read fixture ${path}: ${err}'
		return
	}
	mut total := 0
	mut failed := []string{}
	for raw in body.split('\n') {
		line := raw.trim_space()
		if line.len == 0 {
			continue
		}
		fields := line.split('\t')
		if fields.len != 2 {
			assert false, 'bad fixture line: ${line}'
			continue
		}
		hex_addr := fields[0]
		want := fields[1]
		bytes := hex_to_bytes(hex_addr) or {
			assert false, 'bad hex: ${hex_addr}'
			continue
		}
		got := canonical_ipv6_from_bytes(bytes) or {
			failed << '${hex_addr}: ${err}'
			continue
		}
		if got != want {
			failed << '${hex_addr}: got "${got}", want "${want}"'
		}
		total++
	}
	if failed.len > 0 {
		for f in failed {
			eprintln('  FAIL: ${f}')
		}
		assert false, '${failed.len}/${total} cross-check vectors failed'
	}
	assert total >= 100, 'expected >= 100 vectors, got ${total}'
}

fn hex_to_bytes(s string) ![]u8 {
	if s.len % 2 != 0 {
		return error('hex_to_bytes: odd length')
	}
	mut out := []u8{cap: s.len / 2}
	for i := 0; i < s.len; i += 2 {
		hi := hex_digit(s[i]) or { return error('bad hex at ${i}') }
		lo := hex_digit(s[i + 1]) or { return error('bad hex at ${i + 1}') }
		out << (hi << 4) | lo
	}
	return out
}

// Property: bytes -> canonical -> parse -> bytes round-trip + idempotence.
fn test_property_roundtrip_and_idempotence() {
	mut state := u64(0xCAFEBABEDEADBEEF)
	for iter in 0 .. 1000 {
		mut bytes := []u8{cap: 16}
		for _ in 0 .. 16 {
			state = state * 6364136223846793005 + 1442695040888963407
			bytes << u8(state >> 56)
		}
		if iter % 5 == 0 {
			run_start := int((state >> 8) & 0x7)
			run_len := int((state >> 16) & 0x7) + 1
			for k in 0 .. run_len {
				idx := 2 * (run_start + k)
				if idx + 1 < bytes.len {
					bytes[idx] = 0
					bytes[idx + 1] = 0
				}
			}
		}
		canon := canonical_ipv6_from_bytes(bytes) or {
			assert false, 'canonical_ipv6_from_bytes failed on iter ${iter}: ${err}'
			continue
		}
		parsed := parse_ipv6_to_bytes(canon) or {
			assert false, 'parse_ipv6_to_bytes failed on canonical "${canon}" (iter ${iter}): ${err}'
			continue
		}
		assert parsed == bytes, 'round-trip mismatch on iter ${iter}: bytes=${bytes.hex()}, canon="${canon}", parsed=${parsed.hex()}'

		canon2 := canonical_ipv6(canon) or {
			assert false, 'canonical_ipv6 failed on its own output "${canon}" (iter ${iter}): ${err}'
			continue
		}
		assert canon == canon2, 'idempotence broken on iter ${iter}: "${canon}" -> "${canon2}"'
	}
}

fn test_alternative_forms_canonicalize_identically() {
	cases := [
		['2001:db8:0:0:0:0:0:1', '2001:db8::1'],
		['2001:DB8:0000:0000:0000:0000:0000:0001', '2001:db8::1'],
		['2001:0db8::0001', '2001:db8::1'],
		['2001:DB8::1', '2001:db8::1'],
		['0:0:0:0:0:0:0:0', '::'],
		['0000:0000:0000:0000:0000:0000:0000:0000', '::'],
		['0:0:0:0:0:ffff:c0a8:0001', '::ffff:192.168.0.1'],
		['::FFFF:192.168.0.1', '::ffff:192.168.0.1'],
		['::ffff:c0a8:1', '::ffff:192.168.0.1'],
		['fe80:0:0:0:0:0:0:1', 'fe80::1'],
		['FE80::0:0:0:1', 'fe80::1'],
	]
	for c in cases {
		got := canonical_ipv6(c[0]) or {
			assert false, 'canonical_ipv6("${c[0]}") errored: ${err}'
			continue
		}
		assert got == c[1], '"${c[0]}" -> got "${got}", want "${c[1]}"'
	}
}

fn test_well_known_corpus() {
	assert canonical_ipv6('::')! == '::'
	assert canonical_ipv6('::1')! == '::1'
	assert canonical_ipv6('1::')! == '1::'
	assert canonical_ipv6('fe80::1')! == 'fe80::1'
	assert canonical_ipv6('2001:db8::')! == '2001:db8::'
	assert canonical_ipv6('2001:db8:0:0:0:0:0:1')! == '2001:db8::1'
	assert canonical_ipv6('2001:db8:0:0:1:0:0:1')! == '2001:db8::1:0:0:1'
	// §5 is opt-in for non-::ffff:0:0/96 prefixes; we keep them as hex.
	assert canonical_ipv6('64:ff9b::192.0.2.33')! == '64:ff9b::c000:221'
}
