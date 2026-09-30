module eval

fn test_eval_escaped_enum_references_preserve_declared_values() {
	mut e := create()
	e.run_text('
enum Keyword {
	first
	struct = 7
}
enum Distinct {
	none = 2
	@none = 4
}
fn keyword() Keyword {
	return .@struct
}
fn score(value Keyword) int {
	return match value {
		.@struct { 1 }
		else { 0 }
	}
}
fn main() {
	println(int(Keyword.@struct))
	value := keyword()
	println(int(value))
	println(value == .@struct)
	println(Keyword.@struct == (.@struct))
	println(score(value))
	println(int(Distinct.none))
	println(int(Distinct.@none))
	println(Distinct.none == .none)
	println(Distinct.@none == .@none)
	println(Distinct.@none != .none)
}
') or { panic(err) }
	assert e.stdout() == '7\n7\ntrue\ntrue\n1\n2\n4\ntrue\ntrue\ntrue\n'
}

fn test_eval_left_enum_shorthand_in_value_and_flow_evaluation() {
	mut e := create()
	e.run_text('
enum Keyword {
	first
	struct = 7
}
enum Distinct {
	none = 2
	@none = 4
}
const plain_equal = .@struct == Keyword.@struct
const escaped_equal = (.@none) == Distinct.@none
const distinct_unequal = .none != Distinct.@none
__global calls int
fn keyword() Keyword {
	calls++
	return .@struct
}
fn main() {
	println(plain_equal)
	println(escaped_equal)
	println(distinct_unequal)
	value := Keyword.@struct
	println(.@struct == value)
	println((.@struct) == (Keyword.@struct))
	println(.@struct == keyword())
	println(calls)
	println(.none == Distinct.none)
	println(.@none == Distinct.@none)
	println(.none != Distinct.@none)
}
') or { panic(err) }
	assert e.stdout() == 'true\ntrue\ntrue\ntrue\ntrue\ntrue\n1\ntrue\ntrue\ntrue\n'
}
