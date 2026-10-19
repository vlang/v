import toml.ast
import toml.parser
import toml.scanner

fn root_table_reference(mut p parser.Parser) !&map[string]ast.Value {
	return p.find_table()!
}

fn child_table_reference(mut p parser.Parser) !&map[string]ast.Value {
	return p.find_sub_table(['child'])!
}

fn nested_table_reference(mut p parser.Parser, mut table map[string]ast.Value) !&map[string]ast.Value {
	return p.find_in_table(mut table, ['nested'])!
}

fn set_table_reference_value(table &map[string]ast.Value, key string, value ast.Value) {
	// Mutate through the original returned reference, including after its parent rehashes.
	unsafe { (*table)[key] = value }
}

fn table_reference_value(table &map[string]ast.Value, key string) ast.Value {
	return (*table)[key] or { panic('missing table key `${key}`') }
}

fn test_public_parser_table_references_share_mutations_after_parent_rehash() {
	mut source := scanner.new_simple_text('')!
	mut p := parser.new_parser(parser.Config{ scanner: &source })
	p.init()!
	mut root := root_table_reference(mut p)!
	mut child := child_table_reference(mut p)!
	set_table_reference_value(child, 'before', ast.Number{ text: '1' })
	root.reserve(2048)
	for i in 0 .. 1024 {
		set_table_reference_value(root, 'sibling${i}', ast.Number{ text: i.str() })
	}
	child.reserve(512)
	set_table_reference_value(child, 'after', ast.Number{ text: '2' })
	mut again := child_table_reference(mut p)!
	assert table_reference_value(again, 'before').str() == '1'
	assert table_reference_value(again, 'after').str() == '2'
	set_table_reference_value(again, 'from_parent', ast.Number{ text: '3' })
	assert table_reference_value(child, 'from_parent').str() == '3'
	parent_child := table_reference_value(root, 'child') as map[string]ast.Value
	assert (parent_child['after'] or { panic('missing after') }).str() == '2'
	assert (parent_child['from_parent'] or { panic('missing from_parent') }).str() == '3'
	assert root_table_reference(mut p)!.len == 1025
}

fn test_public_find_in_table_reference_survives_outer_and_inner_rehash() {
	mut source := scanner.new_simple_text('')!
	mut p := parser.new_parser(parser.Config{ scanner: &source })
	p.init()!
	mut outer := map[string]ast.Value{}
	mut nested := nested_table_reference(mut p, mut outer)!
	set_table_reference_value(nested, 'before', ast.Number{ text: '11' })
	outer.reserve(2048)
	for i in 0 .. 1024 {
		outer['sibling${i}'] = ast.Number{ text: i.str() }
	}
	nested.reserve(512)
	set_table_reference_value(nested, 'after', ast.Number{ text: '22' })
	mut again := nested_table_reference(mut p, mut outer)!
	set_table_reference_value(again, 'from_parent', ast.Number{ text: '33' })
	assert table_reference_value(nested, 'from_parent').str() == '33'
	assert table_reference_value(again, 'before').str() == '11'
	assert table_reference_value(again, 'after').str() == '22'
	parent_nested := (outer['nested'] or { panic('missing nested') }) as map[string]ast.Value
	assert (parent_nested['after'] or { panic('missing after') }).str() == '22'
}

fn test_public_nested_table_contents_returns_one_table_at_eof() {
	mut source := scanner.new_simple_text('name = "first"\ndetails.weight = 3')!
	mut p := parser.new_parser(parser.Config{ scanner: &source })
	p.init()!
	values := p.double_array_of_tables_contents(['group', 'items'])!
	assert values.len == 1
	entry := values[0] as map[string]ast.Value
	assert (entry['name'] or { panic('missing name') }).str() == '"first"'
	details := (entry['details'] or { panic('missing details') }) as map[string]ast.Value
	assert (details['weight'] or { panic('missing weight') }).str() == '3'
}
