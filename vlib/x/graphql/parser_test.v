module graphql

import os

fn test_query_parse() ! {
	body := '{
        node(id: 4) {
          id,
          name
        }
	}'

	result := parse(body, none)!

	assert result.kind == Kind.document
	assert result.loc?.source.body == body
	assert result.definitions.len == 1

	def := result.definitions[0]
	assert def is OperationDefinitionNode
	op := def as OperationDefinitionNode
	assert op.selection_set.selections.len == 1

	sel0 := op.selection_set.selections[0]
	assert sel0 is FieldNode
	field := sel0 as FieldNode
	assert field.name.value == 'node'

	nested := field.selection_set or { panic('expected nested selection set') }
	assert nested.selections.len == 2

	sub0 := nested.selections[0]
	assert sub0 is FieldNode
	assert (sub0 as FieldNode).name.value == 'id'

	sub1 := nested.selections[1]
	assert sub1 is FieldNode
	assert (sub1 as FieldNode).name.value == 'name'
}

fn test_list_literals() ! {
	// Empty list
	doc1 := parse('{ users(ids: []) { id } }', none)!
	assert doc1.kind == Kind.document

	// Non-empty list
	doc2 := parse('{ users(ids: [1, 2]) { id } }', none)!
	def2 := doc2.definitions[0] as OperationDefinitionNode
	field2 := def2.selection_set.selections[0] as FieldNode
	args2 := field2.arguments or { panic('expected arguments') }
	assert args2.len == 1
	assert args2[0].name.value == 'ids'
	assert args2[0].value is ListValueNode
	list2 := args2[0].value as ListValueNode
	assert list2.values.len == 2
	assert (list2.values[0] as IntValueNode).value? == '1'
	assert (list2.values[1] as IntValueNode).value? == '2'

	// Nested list
	doc3 := parse('{ matrix(grid: [[1, 2], [3, 4]]) { id } }', none)!
	def3 := doc3.definitions[0] as OperationDefinitionNode
	field3 := def3.selection_set.selections[0] as FieldNode
	args3 := field3.arguments or { panic('expected arguments') }
	list3 := args3[0].value as ListValueNode
	assert list3.values.len == 2
	assert (list3.values[0] as ListValueNode).values.len == 2
	assert (list3.values[1] as ListValueNode).values.len == 2

	// List-valued variable default
	doc4 := parse('query getUserList(\$ids: [Int] = [1, 2]) { users(ids: \$ids) { id } }', none)!
	def4 := doc4.definitions[0] as OperationDefinitionNode
	var_defs := def4.variable_definitions or { panic('expected variable_definitions') }
	assert var_defs.len == 1
	var_def := var_defs[0]
	assert var_def.variable.name.value == 'ids'
	default_val := var_def.default_value or { panic('expected default value') }
	assert default_val is ListValueNode
	assert (default_val as ListValueNode).values.len == 2
}

fn test_fragments() ! {
	// Fragment spread
	doc1 := parse('{ user { ...UserFields } }', none)!
	def1 := doc1.definitions[0] as OperationDefinitionNode
	field1 := def1.selection_set.selections[0] as FieldNode
	nested1 := field1.selection_set or { panic('expected selection_set') }
	assert nested1.selections.len == 1
	assert nested1.selections[0] is FragmentSpreadNode
	spread := nested1.selections[0] as FragmentSpreadNode
	assert spread.name.value == 'UserFields'

	// Inline fragment with type condition
	doc2 := parse('{ user { ... on AdminUser { role } } }', none)!
	def2 := doc2.definitions[0] as OperationDefinitionNode
	field2 := def2.selection_set.selections[0] as FieldNode
	nested2 := field2.selection_set or { panic('expected selection_set') }
	assert nested2.selections.len == 1
	assert nested2.selections[0] is InlineFragmentNode
	inline := nested2.selections[0] as InlineFragmentNode
	tc := inline.type_condition or { panic('expected type condition') }
	assert tc.node_type.value == 'AdminUser'
	assert inline.selection_set.selections.len == 1
	assert (inline.selection_set.selections[0] as FieldNode).name.value == 'role'

	// Fragment definition
	doc3 := parse('fragment UserFields on User { id name }', none)!
	assert doc3.definitions.len == 1
	assert doc3.definitions[0] is FragmentDefinitionNode
	frag_def := doc3.definitions[0] as FragmentDefinitionNode
	assert frag_def.name.value == 'UserFields'
	assert frag_def.type_condition.node_type.value == 'User'
	assert frag_def.selection_set.selections.len == 2
}

fn test_string_escapes() ! {
	// Escaped newline at nonzero document offset
	doc1 := parse('{ f(s: "a\\nb") }', none)!
	def1 := doc1.definitions[0] as OperationDefinitionNode
	field1 := def1.selection_set.selections[0] as FieldNode
	args1 := field1.arguments or { panic('expected arguments') }
	assert (args1[0].value as StringValueNode).value? == 'a\nb'

	// Escaped quotes and backslash
	doc2 := parse('{ f(s: "hello \\"world\\" \\\\ path") }', none)!
	def2 := doc2.definitions[0] as OperationDefinitionNode
	field2 := def2.selection_set.selections[0] as FieldNode
	args2 := field2.arguments or { panic('expected arguments') }
	assert (args2[0].value as StringValueNode).value? == 'hello "world" \\ path'

	// Multiple escapes in one string
	doc3 := parse('{ f(s: "line1\\nline2\\ttab\\r\\n") }', none)!
	def3 := doc3.definitions[0] as OperationDefinitionNode
	field3 := def3.selection_set.selections[0] as FieldNode
	args3 := field3.arguments or { panic('expected arguments') }
	assert (args3[0].value as StringValueNode).value? == 'line1\nline2\ttab\r\n'

	// Unicode fixed width escapes
	doc4 := parse('{ f(s: "\\u0041\\u0042") }', none)!
	def4 := doc4.definitions[0] as OperationDefinitionNode
	field4 := def4.selection_set.selections[0] as FieldNode
	args4 := field4.arguments or { panic('expected arguments') }
	assert (args4[0].value as StringValueNode).value? == 'AB'

	// Unicode variable width escape
	doc5 := parse('{ f(s: "\\u{1F600}") }', none)!
	def5 := doc5.definitions[0] as OperationDefinitionNode
	field5 := def5.selection_set.selections[0] as FieldNode
	args5 := field5.arguments or { panic('expected arguments') }
	assert (args5[0].value as StringValueNode).value? == '😀'
}

fn test_error_propagation() {
	// Malformed character does not panic, returns error
	if _ := parse('%', none) {
		assert false
	} else {
		assert true
	}

	// Incomplete selection set does not panic, returns error
	if _ := parse('{ user {', none) {
		assert false
	} else {
		assert true
	}

	// Exceeded max tokens returns error via or block
	if _ := parse('{ a b c d e f g h }', ParserOptions{ max_tokens: 5 }) {
		assert false
	} else {
		assert true
	}

	// Unterminated string returns error
	if _ := parse('{ user(name: "unterminated) }', none) {
		assert false
	} else {
		assert true
	}
}

fn test_github_parse() ! {
	path := os.join_path(@DIR, 'testdata', 'github.graphql')

	body := os.read_file(path)!

	result := parse(body, none)!

	assert result.kind == Kind.document
	assert result.token_count == 19_882
}

fn test_simple_parse() ! {
	path := os.join_path(@DIR, 'testdata', 'simple.graphql')

	body := os.read_file(path)!

	result := parse(body, none)!

	assert result.kind == Kind.document
	assert result.token_count == 175
}

fn test_pokeapi_parse() ! {
	path := os.join_path(@DIR, 'testdata', 'pokeapi.graphql')

	body := os.read_file(path)!

	result := parse(body, none)!

	assert result.kind == Kind.document
	assert result.token_count == 27
}

fn test_variable_definition_parse() ! {
	body := 'query getUser(\$id: ID! = "default") {
		user(id: \$id) {
			name
		}
	}'

	result := parse(body, none)!

	assert result.kind == Kind.document
	assert result.token_count == 23
}