[name: 'abc']
[amount: 2]
[abc]
struct Abc {}

fn test_comptime_for_attributes() {
	mut res := ''
	mut amount := 0
	$for attr in Abc.attributes {
		if attr.name == 'amount' && attr.has_arg && attr.kind == .number {
			amount = attr.arg.int()
		}
		if attr.name == 'name' && attr.has_arg && attr.kind == .string {
			res = attr.arg
		}
	}
	res = res.repeat(amount)
	assert res == 'abcabc'
}

fn test_attributes() {
	$for attr in Abc.attributes {
		if attr.has_arg && attr.kind == .string {
			assert attr.name == 'name'
			assert attr.arg == 'abc'
		} else if attr.has_arg && attr.kind == .number {
			assert attr.name == 'amount'
			assert attr.arg == '2'
		} else {
			assert attr.name == 'abc'
		}
	}
}
