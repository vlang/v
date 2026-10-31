module parser

import toml.scanner
import toml.token

fn test_lookahead_only_reads_requested_tokens() {
	mut source := scanner.new_simple_text('a = 1')!
	mut parser := new_parser(Config{ scanner: &source })
	parser.init()!
	assert source.state().pos == 1
	assert parser.peek(2)!.kind == .assign
	assert source.state().pos == 3
	assert parser.peek(1)!.kind == .whitespace
	assert source.state().pos == 3
	expected := [
		token.Kind.bare,
		.whitespace,
		.assign,
		.whitespace,
		.number,
		.eof,
	]
	for kind in expected {
		parser.next()!
		assert parser.tok.kind == kind
	}
}
