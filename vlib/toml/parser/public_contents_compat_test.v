module parser

import toml.ast
import toml.scanner

fn test_public_nested_contents_preserves_the_next_header_for_continuation() {
	mut source := scanner.new_simple_text('name = "first"\n[[group.items]]\nname = "second"\n[[group.items]]\nname = "third"\n')!
	mut p := new_parser(Config{ scanner: &source })
	p.init()!
	values := p.double_array_of_tables_contents(DottedKey(['group', 'items']))!
	assert values.len == 1
	first := values[0] as map[string]ast.Value
	assert (first['name'] or { panic('missing first name') }).str() == '"first"'
	assert p.tok.kind == .lsbr
	assert p.peek_tok.kind == .lsbr
	// The normal table caller suppresses one next() after an array body returns.
	p.skip_next = true
	p.root_table()!
	assert p.tok.kind == .eof
	group := (p.root_map['group'] or { panic('missing group') }) as map[string]ast.Value
	remaining := (group['items'] or { panic('missing items') }) as []ast.Value
	assert remaining.len == 2
	second := remaining[0] as map[string]ast.Value
	third := remaining[1] as map[string]ast.Value
	assert (second['name'] or { panic('missing second name') }).str() == '"second"'
	assert (third['name'] or { panic('missing third name') }).str() == '"third"'
}
