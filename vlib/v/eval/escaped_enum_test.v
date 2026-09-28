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
