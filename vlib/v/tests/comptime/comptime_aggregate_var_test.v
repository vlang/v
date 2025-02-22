module main

type Value = int | i64 | u64 | string | []u8

fn d(val Value) string {
	match val {
		int, i64, u64, []u8 {
			s := sizeof(val)
			x := val
			return 'Value is number or byte array, size=${s} ${x}'
		}
		string {
			x := val
			return 'Value is string: ${x}'
		}
	}
}

fn test_main() {
	assert d(Value(0)) == 'Value is number or byte array, size=4 0'
	assert d(Value(i64(1))) == 'Value is number or byte array, size=8 1'
	assert d(Value(u64(2))) == 'Value is number or byte array, size=8 2'
	assert d(Value([u8(1), 2])) == 'Value is number or byte array, size=32 [1, 2]'
	assert d(Value('')) == 'Value is string: '
}
