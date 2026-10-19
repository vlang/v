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

fn test_public_parser_table_references_share_mutations_after_parent_rehash() {
	mut source := scanner.new_simple_text('')!
	mut p := parser.new_parser(parser.Config{ scanner: &source })
	p.init()!
	mut root := root_table_reference(mut p)!
	mut child := child_table_reference(mut p)!
	child['before'] = ast.Number{ text: '1' }
	root.reserve(2048)
	for i in 0 .. 1024 {
		root['sibling${i}'] = ast.Number{ text: i.str() }
	}
	child.reserve(512)
	child['after'] = ast.Number{ text: '2' }
	mut again := child_table_reference(mut p)!
	assert again['before'].str() == '1'
	assert again['after'].str() == '2'
	again['from_parent'] = ast.Number{ text: '3' }
	assert child['from_parent'].str() == '3'
	parent_child := root['child'] as map[string]ast.Value
	assert parent_child['after'].str() == '2'
	assert parent_child['from_parent'].str() == '3'
	assert root_table_reference(mut p)!.len == 1025
}

fn test_public_find_in_table_reference_survives_outer_and_inner_rehash() {
	mut source := scanner.new_simple_text('')!
	mut p := parser.new_parser(parser.Config{ scanner: &source })
	p.init()!
	mut outer := map[string]ast.Value{}
	mut nested := nested_table_reference(mut p, mut outer)!
	nested['before'] = ast.Number{ text: '11' }
	outer.reserve(2048)
	for i in 0 .. 1024 {
		outer['sibling${i}'] = ast.Number{ text: i.str() }
	}
	nested.reserve(512)
	nested['after'] = ast.Number{ text: '22' }
	mut again := nested_table_reference(mut p, mut outer)!
	again['from_parent'] = ast.Number{ text: '33' }
	assert nested['from_parent'].str() == '33'
	assert again['before'].str() == '11'
	assert again['after'].str() == '22'
	parent_nested := outer['nested'] as map[string]ast.Value
	assert parent_nested['after'].str() == '22'
}

fn test_public_nested_table_contents_returns_one_table_at_eof() {
	mut source := scanner.new_simple_text('name = "first"\ndetails.weight = 3')!
	mut p := parser.new_parser(parser.Config{ scanner: &source })
	p.init()!
	values := p.double_array_of_tables_contents(['group', 'items'])!
	assert values.len == 1
	entry := values[0] as map[string]ast.Value
	assert entry['name'].str() == '"first"'
	details := entry['details'] as map[string]ast.Value
	assert details['weight'].str() == '3'
}
