// Copyright 2025 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Based off:   https://github.com/golang/go/blob/master/src/uuid/uuid_test.go
// and          https://github.com/golang/go/blob/master/src/uuid/example_test.go
//
// Go moves the clock of its UUIDv7 tests with testing/synctest; these tests give
// `new_v7_from` a fake clock instead.
@[has_globals]
module uuid

// synctest_start is the time at which a testing/synctest bubble starts in Go,
// 2000-01-01 00:00:00 UTC.
const synctest_start = u64(946_684_800)

const canonical = 'f81d4fae-7dec-11d0-a765-00a0c91e6bf6'

const u1 = UUID([u8(0xf8), 0x1d, 0x4f, 0xae, 0x7d, 0xec, 0x11, 0xd0, 0xa7, 0x65, 0x00, 0xa0, 0xc9,
	0x1e, 0x6b, 0xf6]!)

__global (
	fake_secs  u64
	fake_nanos u64
)

fn fake_now() (u64, u64) {
	return fake_secs, fake_nanos
}

fn version(u UUID) u8 {
	return u[6] >> 4
}

fn variant(u UUID) u8 {
	return u[8] >> 6
}

fn test_new() {
	u := new()
	assert version(u) == 4, u.str()
	assert variant(u) == 0b10, u.str()
	v4 := new_v4()
	assert version(v4) == 4, v4.str()
	assert variant(v4) == 0b10, v4.str()
	v7 := new_v7()
	assert version(v7) == 7, v7.str()
	assert variant(v7) == 0b10, v7.str()
	assert new_v4() != new_v4()
	assert new_v7() != new_v7()
}

// unix_ts_ms is the 48-bit millisecond field of the UUIDv7 `u`.
fn unix_ts_ms(u UUID) u64 {
	mut ms := u64(0)
	for i in 0 .. 6 {
		ms = (ms << 8) | u[i]
	}
	return ms
}

fn check_millis(secs u64, nanos u64) {
	fake_secs, fake_nanos = secs, nanos
	u := new_v7_from(fake_now)
	want := secs * 1000 + nanos / 1_000_000
	assert unix_ts_ms(u) == want, 'at ${secs}s ${nanos}ns, new_v7() = ${u.str()}'
}

// The unix_ts_ms field of a UUIDv7 is set correctly.
fn test_new_v7_millis() {
	check_millis(synctest_start, 0)
	check_millis(synctest_start + 3600, 0)
	// The maximum and the minimum fractional seconds.
	check_millis(synctest_start + 3600, 999_999_999)
	check_millis(synctest_start + 3601, 1)
	// Time goes backwards: UUIDs use the new time.
	check_millis(synctest_start, 0)
}

// The unix_ts_ms field of a UUIDv7 from the real clock is the current time.
fn test_new_v7_real_clock() {
	before, _ := utc_now()
	u := new_v7()
	after, _ := utc_now()
	ms := unix_ts_ms(u)
	assert ms >= before * 1000 && ms < (after + 1) * 1000, u.str()
}

// UUIDv7s generated at the same instant do not collide and are monotonically
// increasing.
fn test_new_v7_collision() {
	fake_secs, fake_nanos = synctest_start, 0
	mut last := new_v7_from(fake_now)
	for _ in 0 .. 3 {
		// Enough iterations to overflow the fractional millisecond component
		// several times.
		for _ in 0 .. (1 << 12) * 3 {
			u := new_v7_from(fake_now)
			assert u.compare(last) == 1, 'out of order:\nprevious: ${last.str()}\n current: ${u.str()}'
			last = u
		}
		// Time advances, but not as quickly as UUIDs are generated.
		fake_nanos += 1_000_000
	}
}

fn test_encode() {
	assert u1.str() == canonical
	assert '${u1}' == canonical
	assert 'urn:uuid:${u1}' == 'urn:uuid:' + canonical
}

fn test_parse_success() {
	assert parse('00000000-0000-0000-0000-000000000000')! == nil_uuid
	assert parse('ffffffff-ffff-ffff-ffff-ffffffffffff')! == max_uuid
	for s in [
		'f81d4fae-7dec-11d0-a765-00a0c91e6bf6',
		'F81D4FAE-7DEC-11D0-A765-00A0C91E6BF6',
		'f81d4fae7dec11d0a76500a0c91e6bf6',
		'{f81d4fae-7dec-11d0-a765-00a0c91e6bf6}',
		'urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6',
	] {
		u := parse(s) or { panic('parse(${s}): ${err}') }
		assert u == u1, 'parse(${s}) = ${u.str()}'
	}
}

fn test_parse_errors() {
	for s in [
		'',
		'0000000000000-0000-0000-000000000000',
		'00000000-000000000-0000-000000000000',
		'00000000-0000-000000000-000000000000',
		'00000000-0000-0000-00000000000000000',
		'00000000-0000-0000-0000-00000000000',
		'x0000000-0000-0000-0000-000000000000',
		'00000000-x000-0000-0000-000000000000',
		'00000000-0000-x000-0000-000000000000',
		'00000000-0000-0000-x000-000000000000',
		'00000000-0000-0000-0000-x00000000000',
		'{x0000000-0000-0000-0000-000000000000}',
		'urn:uuid:x000000-0000-0000-0000-000000000000',
		'x0000000000000000000000000000000',
		// Some parsers permit hyphens in non-standard locations,
		// but this one does not.
		'0000-0000-0000-0000-0000-0000-0000-0000',
		// Combinations of variant encodings that could be parsed, but are not.
		'{00000000000000000000000000000000}',
		'{urn:uuid:00000000-0000-0000-0000-000000000000}',
		'urn:uuid:00000000000000000000000000000000',
	] {
		mut message := ''
		u := parse(s) or {
			message = err.msg()
			nil_uuid
		}
		assert message == 'invalid uuid', 'parse(${s}) = ${u.str()}, want an error'
	}
}

fn test_compare() {
	uuids := [nil_uuid, u1, max_uuid]
	for i in 0 .. uuids.len {
		u := uuids[i]
		assert u.compare(u) == 0, u.str()
		if i == 0 {
			continue
		}
		prev := uuids[i - 1]
		assert u.compare(prev) == 1, '${u.str()} vs ${prev.str()}'
		assert prev.compare(u) == -1, '${prev.str()} vs ${u.str()}'
	}
}

fn test_map_key() {
	mut seen := map[UUID]int{}
	seen[u1] = 1
	seen[nil_uuid] = 2
	seen[parse(canonical)!] += 10
	assert seen.len == 2
	assert seen[u1] == 11
	assert seen[nil_uuid] == 2
}

fn test_example_parse() {
	for s in [
		'f81d4fae-7dec-11d0-a765-00a0c91e6bf6',
		'{f81d4fae-7dec-11d0-a765-00a0c91e6bf6}',
		'urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6',
		'f81d4fae7dec11d0a76500a0c91e6bf6',
	] {
		assert parse(s)!.str() == canonical
	}
}

fn test_example_compare() {
	mut ids := [
		parse('f81d4fae-7dec-11d0-a765-00a0c91e6bf6')!,
		parse('00000000-0000-0000-0000-000000000000')!,
		parse('ffffffff-ffff-ffff-ffff-ffffffffffff')!,
	]
	ids.sort_with_compare(fn (a &UUID, b &UUID) int {
		return a.compare(*b)
	})
	mut sorted := []string{}
	for id in ids {
		sorted << id.str()
	}
	assert sorted == [
		'00000000-0000-0000-0000-000000000000',
		'f81d4fae-7dec-11d0-a765-00a0c91e6bf6',
		'ffffffff-ffff-ffff-ffff-ffffffffffff',
	]
}

fn test_parse_checks_every_hex_position_and_byte() {
	for position in 0 .. 32 {
		for byte in 0 .. 256 {
			mut bytes := 'f81d4fae7dec11d0a76500a0c91e6bf6'.bytes()
			bytes[position] = u8(byte)
			raw := bytes.bytestr()
			dashed := raw[..8] + '-' + raw[8..12] + '-' + raw[12..16] + '-' + raw[16..20] + '-' + raw[20..]
			valid := (byte >= `0` && byte <= `9`) || (byte >= `a` && byte <= `f`)
				|| (byte >= `A` && byte <= `F`)
			for text in [raw, dashed, '{${dashed}}', 'urn:uuid:${dashed}'] {
				mut rejected := false
				u := parse(text) or {
					rejected = true
					nil_uuid
				}
				assert rejected == !valid, 'position ${position}, byte ${byte}'
				if valid {
					assert u.str().replace('-', '') == raw.to_lower()
				}
			}
		}
	}
}

fn test_new_v7_fraction_boundaries() {
	nanos := [u64(0), 244, 245, 999_999, 1_000_000, 1_000_245, 999_999_999]
	fractions := [u16(0), 0, 1, 4095, 0, 1, 4095]
	for i, nanosecond in nanos {
		fake_secs, fake_nanos = synctest_start + 86400 + u64(i), nanosecond
		u := new_v7_from(fake_now)
		assert unix_ts_ms(u) == fake_secs * 1000 + nanosecond / 1_000_000
		assert (u16(u[6] & 0x0f) << 8) | u16(u[7]) == fractions[i]
		assert version(u) == 7 && variant(u) == 0b10
	}
	// The largest timestamp representable in the 48-bit millisecond field.
	fake_secs, fake_nanos = 281_474_976_710, 655_000_000
	u := new_v7_from(fake_now)
	assert unix_ts_ms(u) == u64(0xffff_ffff_ffff)
	assert version(u) == 7 && variant(u) == 0b10
}
