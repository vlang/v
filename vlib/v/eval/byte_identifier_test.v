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
