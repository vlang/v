module parser

import os
import v.pref

fn test_parenthesized_match_header_accepts_postfix_expressions() {
	path := os.join_path(os.vtmp_dir(), 'match_parenthesized_postfix_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	for expression in ['(*value)[0]', '(((*value)))[indices[index(0)]]', '(*value)[0..2][1]',
		'(*value)[index(Holder{value: 0})]', '(*holder).value', '(*holder).byte()',
		'(*holder).bytes()[0]', '(factory)()[0]'] {
		os.write_file(path, 'fn main() {
	match ${expression} { `a` {} else {} }
	result := match ${expression} { `a` { 1 } else { 0 } }
	_ = result
}
')!
		mut p := Parser.new(pref.new_preferences())
		a := p.parse_file(path)
		assert p.diagnostics.len == 0, '${expression}: ${p.diagnostics}'
		assert a.nodes.count(it.kind == .match_stmt) == 2
	}
}

fn test_keyword_match_calls_with_postfix_remain_calls() {
	path := os.join_path(os.vtmp_dir(), 'match_keyword_postfix_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'fn match() string { return "abc" }
fn main() {
	match()
	_ = match()[0]
	_ = match().len
	_ = match().bytes()[0]
	_ = match() or { "fallback" }
}
')!
	mut p := Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	assert a.nodes.count(it.kind == .match_stmt) == 0
}
