enum Flag {
	usa_old_glory
	all_other_bad_excuses_for_a_flag
}

struct Test {
	is_foo bool
	name   [5]u8
}

fn enc[T](item T) string {
	$if T is $int {
		len := match typeof(item).name {
			'i8', 'u8' { u8(2) }
			'i16', 'u16' { 4 }
			'int', 'u32', 'i32' { 8 }
			'i64', 'u64' { 16 }
			else { return '' }
		}
		return u64_to_hex(item, len)
	} $else $if T is $array {
		mut hex := ''
		for val in item {
			hex += enc(val)
		}
		return hex
	} $else $if T is $struct {
		mut hex := ''
		$for field in T.fields {
			hex += enc(item.$(field.name)) + '_'
		}
		return hex
	} $else {
		if typeof(item).name == 'bool' {
			return enc(int(item))
		}
		$if debug {
			println('cannot encode ${T}(s)')
		}
		return ''
	}
}

@[direct_array_access; inline]
fn u64_to_hex(nn u64, len u8) string {
	mut n := nn
	mut buf := [17]u8{}
	buf[len] = 0
	mut i := 0
	for i = len - 1; i >= 0; i-- {
		d := u8(n & 0xF)
		buf[i] = if d < 10 { d + `0` } else { d + 87 }
		n = n >> 4
	}
	return unsafe { tos(memdup(&buf[0], len + 1), len) }
}

fn test_main() {
	assert enc(Test{}) == '00000000_0000000000_'
	assert enc(Test{ is_foo: true }) == '00000001_0000000000_'
	assert enc(Test{ name: [u8(1), 2, 3, 4, 5]! }) == '00000000_0102030405_'
}
