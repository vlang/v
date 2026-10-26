module parser

import os
import v.pref

fn parse_diagnostics(name string, source string) []string {
	root := os.join_path(os.vtmp_dir(), 'negated_infix_operand_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, '${name}.v')
	os.write_file(path, 'module main\nfn main() {\n${source}\n}\n') or { panic(err) }
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(path)
	return p.diagnostics.map(it.message)
}

// V 0.5.2 accepts a negated right operand after a binary minus, and rejects only a literal
// that is negated twice.
fn test_negated_right_operand_of_minus_is_accepted() {
	for expr in ['5 - -a', 'c - -1.2', 'a - - b', '5 - - 3', '-a - -b', 'a - (-b)', 'x - -y - -y',
		'5 - - - a', '- -5', '5 - (-(5))', "'\${a - -b}'", 'x-y', 'x - -y'] {
		diagnostics := parse_diagnostics('accepted', '_ = ${expr}')
		assert diagnostics.len == 0, '`${expr}`: ${diagnostics}'
	}
}

fn test_postfix_decrement_still_parses() {
	diagnostics := parse_diagnostics('decrement', 'mut x := 1\nx--\n_ = x--y')
	assert diagnostics.len == 0, diagnostics.str()
}

fn test_literal_negated_twice_is_rejected() {
	for expr in ['5 - - -5', '5 - -(3)', '5 - - (((-5)))', '5.0 - -(1.2)'] {
		diagnostics := parse_diagnostics('rejected', '_ = ${expr}')
		assert diagnostics == ['invalid expression: unexpected token `-`'], '`${expr}`: ${diagnostics}'
	}
}
