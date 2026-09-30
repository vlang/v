module eval

fn test_eval_match_byte_constant_uses_its_value() {
	mut e := create()
	e.run_text('
const byte = 8

fn classify(value int) string {
	return match value {
		byte { "matched" }
		else { "other" }
	}
}

fn main() {
	value := 9
	match value {
		byte { println("matched") }
		else { println("other") }
	}
	println(classify(8))
	println(classify(9))
}
') or { panic(err) }
	assert e.stdout() == 'other\nmatched\nother\n'
}

fn test_eval_sizeof_byte_constant_uses_its_declared_width() {
	mut e := create()
	e.run_text('
const byte = u8(1)

fn main() {
	println(sizeof(byte))
	println(sizeof(u8))
	println(sizeof(i32))
}
') or { panic(err) }
	assert e.stdout() == '1\n1\n4\n'
}

fn test_eval_sizeof_byte_local_uses_its_declared_width() {
	mut e := create()
	e.run_text('
fn main() {
	byte := i32(0)
	println(sizeof(byte))
}
') or { panic(err) }
	assert e.stdout() == '4\n'
}

fn test_eval_sizeof_byte_constant_ignores_caller_local_types() {
	mut e := create()
	e.run_text('
const narrow = u8(1)
const byte = narrow

fn main() {
	narrow := i32(0)
	println(sizeof(byte))
	println(sizeof(narrow))
}
') or { panic(err) }
	assert e.stdout() == '1\n4\n'
}

fn test_eval_sizeof_byte_alias_uses_its_underlying_width() {
	mut e := create()
	e.run_text('
type Small = u16
type Narrow = Small
const byte = Narrow(1)

fn main() {
	println(sizeof(byte))
	local := Narrow(0)
	println(sizeof(local))
	println(sizeof(Narrow))
}
') or { panic(err) }
	assert e.stdout() == '2\n2\n2\n'
}

fn test_eval_sizeof_byte_call_does_not_evaluate_the_constant() {
	mut e := create()
	e.run_text('
const byte = side_effect()

fn side_effect() u16 {
	println("evaluated")
	return u16(1)
}

fn main() {
	println(sizeof(byte))
}
') or { panic(err) }
	assert e.stdout() == '2\n'
}
