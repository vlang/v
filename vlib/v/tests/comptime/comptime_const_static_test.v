import constevalhelper as evaluated

@[comptime]
fn const_crc32_table(poly u32) [256]u32 {
	mut table := [256]u32{}
	for i in 0 .. 256 {
		mut crc := u32(i)
		for _ in 0 .. 8 {
			crc = if crc & 1 == 1 { (crc >> 1) ^ poly } else { crc >> 1 }
		}
		table[i] = crc
	}
	return table
}

const imported_pair = evaluated.make_pair(10)

const computed_crc = const_crc32_table(0xedb88320)

@[comptime]
fn const_size(value int) int {
	mut result := 0
	for i := 0; i < value; i++ {
		if i == 2 { continue }
		result += i
		if result > 10 { break }
	}
	return result
}

const computed_size = const_size(5)

@[comptime]
fn const_wrap8(a u8, b u8) u8 { return a + b }

@[comptime]
fn const_half8(a u8, b u8) u8 { return (a + b) / 2 }

@[comptime]
fn const_widen8(a u8, b u8) int { return int(a + b) }

@[comptime]
fn const_invert8(a u8) int { return int(~a) / 2 }

@[comptime]
fn const_local_invert8(a u8) int {
	converted := ~a
	return int(converted) / 2
}

@[comptime]
fn const_unsigned(a u64) u64 { return (a >> 1) / 3 }

const computed_wrap = const_wrap8(250, 10)
const computed_half = const_half8(250, 10)
const computed_widen = const_widen8(250, 10)
const computed_invert = const_invert8(128)
const computed_local_invert = const_local_invert8(128)
const computed_unsigned = const_unsigned(0xffffffffffffffff)

struct ConstRecord {
mut:
	name  string
	flags [2]bool
	table [2]u32
}

@[comptime]
fn const_record(enabled bool, suffix string) ConstRecord {
	mut record := ConstRecord{
		name:  'a\n' + suffix
		flags: [false, true]!
		table: [
			u32(1),
			2,
		]!
	}
	record.flags[0] = !enabled
	mut copy := record
	copy.table[0] = 99
	return if enabled { record } else { copy }
}

const computed_record = const_record(true, 'b')

@[comptime]
fn const_fib(n int) int {
	if n <= 1 { return n }
	return const_fib(n - 1) + const_fib(n - 2)
}

const computed_fib = const_fib(10)

@[comptime]
fn const_shift(a u32, n int) u32 { return (a << n) ^ (a >> n) }

@[comptime]
fn const_guard(a bool) bool {
	if a { return true }
	panic('unreachable during evaluation')
}

const computed_shift_negative = const_shift(0x80000001, -1)
const computed_shift_zero = const_shift(0x80000001, 0)
const computed_shift_last = const_shift(0x80000001, 31)
const computed_shift_width = const_shift(0x80000001, 32)
const computed_guard = const_guard(true)

@[comptime]
fn const_unsigned_range(low u64, high u64) u64 {
	mut count := u64(0)
	for i in low .. high { count += i >> 63 }
	return count
}

const computed_unsigned_range = const_unsigned_range(0xfffffffffffffffd, 0xffffffffffffffff)

fn test_comptime_constants_match_runtime() {
	assert imported_pair == evaluated.make_pair(10)
	assert imported_pair == [u32(10), 17]!
	assert computed_crc == const_crc32_table(0xedb88320)
	assert computed_crc[1] == 0x77073096
	assert computed_crc[255] == 0x2d02ef8d
	assert computed_size == const_size(5)
	assert [computed_size]u8{}.len == 8
	assert computed_wrap == const_wrap8(250, 10)
	assert computed_half == const_half8(250, 10)
	assert computed_widen == const_widen8(250, 10)
	assert computed_invert == const_invert8(128)
	assert computed_local_invert == const_local_invert8(128)
	assert computed_unsigned == const_unsigned(0xffffffffffffffff)
	assert computed_record == const_record(true, 'b')
	assert computed_record.name == 'a\nb'
	assert computed_record.table[0] == 1
	assert computed_fib == const_fib(10)
	assert computed_shift_negative == const_shift(0x80000001, -1)
	assert computed_shift_zero == const_shift(0x80000001, 0)
	assert computed_shift_last == const_shift(0x80000001, 31)
	assert computed_shift_width == const_shift(0x80000001, 32)
	assert computed_unsigned_range == 2
	assert computed_unsigned_range == const_unsigned_range(0xfffffffffffffffd, 0xffffffffffffffff)
	assert computed_guard
}
