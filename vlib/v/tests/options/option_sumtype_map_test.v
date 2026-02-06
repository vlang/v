type Any = ?int | ?string

fn test_main() {
	nothing := ?int(none)
	zero := ?int(0)
	one := ?string('one')
	ten := ?int(10)

	mut m := map[string]Any{}
	m['nothing'] = Any(nothing)
	m['zero'] = Any(zero)
	m['one'] = Any(one)
	m['ten'] = Any(ten)

	assert m.str() == "{'nothing': Any(Option(none)), 'zero': Any(Option(0)), 'one': Any(Option('one')), 'ten': Any(Option(10))}"
}
