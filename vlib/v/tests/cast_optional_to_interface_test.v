[heap]
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

fn test_cast_optional_to_interface() {
	parser := new_parser()?
	assert parser.main.str == 'test'
	eprintln(voidptr(parser.main))
	e := Engine{
		parser: parser
	}
	assert e.parser.main.str == 'test'
	eprintln(voidptr(e.parser.main))
}
