struct Number {}

struct String {}

struct Variable {}

struct FunctionCall {}

type Value = Number | String | Variable | FunctionCall
type Statement = Value

fn test_main() {
	s := Value(Statement(Value(Number{})))
	assert '${s}' == 'Value(Number{})'

	s2 := Statement(Value(Number{}))
	assert '${s2}' == 'Statement(Value(Number{}))'
}
