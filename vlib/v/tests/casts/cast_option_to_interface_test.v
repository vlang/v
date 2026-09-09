@[heap]
struct Package {
	str string
}

interface Parser {
	main &Package
}

struct ParserV1 {
mut:
	main &Package
}

fn new_0_parser() ?ParserV1 {
	return ParserV1{
		main: &Package{
			str: 'test'
		}
	}
}

fn new_parser() ?Parser {
	return Parser(new_0_parser()?)
}

struct Engine {
	parser Parser
}

fn test_cast_option_to_interface() {
	parser := new_parser()?
	assert parser.main.str == 'test'
	eprintln(voidptr(parser.main))
	e := Engine{
		parser: parser
	}
	assert e.parser.main.str == 'test'
	eprintln(voidptr(e.parser.main))
}

interface Issue27340Value {
}

struct Issue27340Cat {
	state int
}

fn maybe_issue_27340_cat() ?Issue27340Cat {
	return Issue27340Cat{
		state: 1
	}
}

fn test_option_none_guard_interface_cast() {
	x := maybe_issue_27340_cat()
	if x == none {
		assert false
		return
	}
	v := Issue27340Value(x)
	assert v is Issue27340Cat
	cat := v as Issue27340Cat
	assert cat.state == 1
}

fn issue_27340_value_state(v Issue27340Value) int {
	assert v is Issue27340Cat
	cat := v as Issue27340Cat
	return cat.state
}

fn test_option_none_guard_implicit_interface_arg() {
	x := maybe_issue_27340_cat()
	if x == none {
		assert false
		return
	}
	assert issue_27340_value_state(x) == 1
}

fn test_option_none_guard_interface_array_append() {
	x := maybe_issue_27340_cat()
	if x == none {
		assert false
		return
	}
	mut values := []Issue27340Value{}
	values << x
	assert issue_27340_value_state(values[0]) == 1
}

fn test_option_positive_none_guard_interface_cast() {
	x := maybe_issue_27340_cat()
	if x != none {
		v := Issue27340Value(x)
		assert issue_27340_value_state(v) == 1
		return
	}
	assert false
}

fn test_option_positive_none_guard_implicit_interface_arg() {
	x := maybe_issue_27340_cat()
	if x != none {
		assert issue_27340_value_state(x) == 1
		return
	}
	assert false
}

fn test_option_positive_none_guard_interface_array_append() {
	x := maybe_issue_27340_cat()
	if x != none {
		mut values := []Issue27340Value{}
		values << x
		assert issue_27340_value_state(values[0]) == 1
		return
	}
	assert false
}
