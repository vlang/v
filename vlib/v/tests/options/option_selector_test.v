module main

struct JsonParser {
mut:
	js        string
	tokens    []string
	pos       int
	tok_next  int
	tok_super ?int
}

fn new_parser(qty int) JsonParser {
	return JsonParser{
		tokens: []string{cap: qty}
	}
}

fn (mut p JsonParser) parse() int {
	j := p.tok_super or { 999 }
	return j
}

fn test_main() {
	mut x := new_parser(100)
	i := x.parse()
	assert i == 999
}
