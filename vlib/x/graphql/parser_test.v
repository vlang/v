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
	operation := result.definitions[0] as OperationDefinitionNode
	assert operation.selection_set.selections.len == 1
	field := operation.selection_set.selections[0] as FieldNode
	assert field.name.value == 'node'
	selection_set := field.selection_set or { panic('missing nested selection set') }
	assert selection_set.selections.len == 2
	id := selection_set.selections[0] as FieldNode
	name := selection_set.selections[1] as FieldNode
	assert id.name.value == 'id'
	assert name.name.value == 'name'
	assert id.selection_set == none
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

fn test_nested_selection_alias_and_directives() ! {
	result := parse('{ viewer: user(id: 1) @include(if: true) { profile { name } } }', none)!
	operation := result.definitions[0] as OperationDefinitionNode
	user := operation.selection_set.selections[0] as FieldNode
	assert user.alias?.value == 'viewer'
	assert user.name.value == 'user'
	assert user.arguments?.len == 1
	assert user.directives[0].name.value == 'include'
	profile := user.selection_set?.selections[0] as FieldNode
	assert profile.name.value == 'profile'
	assert profile.alias == none
	name := profile.selection_set?.selections[0] as FieldNode
	assert name.name.value == 'name'
	assert name.selection_set == none
}

fn test_list_literals() ! {
	body := '{ users(ids: [1, 2], empty: [], nested: [[1], []]) { id } }'
	result := parse(body, none)!
	operation := result.definitions[0] as OperationDefinitionNode
	users := operation.selection_set.selections[0] as FieldNode
	arguments := users.arguments or { panic('missing list arguments') }
	assert arguments.len == 3
	ids := arguments[0].value as ListValueNode
	assert ids.values.len == 2
	first := ids.values[0] as IntValueNode
	second := ids.values[1] as IntValueNode
	assert first.value? == '1'
	assert second.value? == '2'
	location := ids.loc or { panic('missing list location') }
	assert body[location.start..location.end] == '[1, 2]'
	empty := arguments[1].value as ListValueNode
	assert empty.values.len == 0
	nested := arguments[2].value as ListValueNode
	assert nested.values.len == 2
	nested_first := nested.values[0] as ListValueNode
	nested_empty := nested.values[1] as ListValueNode
	assert nested_first.values.len == 1
	assert nested_empty.values.len == 0
}

fn test_list_variable_defaults() ! {
	result := parse(r'query Users($ids: [[Int!]] = [[1, 2], []]) { users(ids: $ids) { id } }',
		none)!
	operation := result.definitions[0] as OperationDefinitionNode
	variables := operation.variable_definitions or { panic('missing variable definitions') }
	assert variables.len == 1
	assert variables[0].variable.name.value == 'ids'
	default_value := variables[0].default_value or { panic('missing variable default') }
	list := default_value as ListValueNode
	assert list.values.len == 2
	first := list.values[0] as ListValueNode
	empty := list.values[1] as ListValueNode
	assert first.values.len == 2
	assert empty.values.len == 0
	first_value := first.values[0] as IntValueNode
	second_value := first.values[1] as IntValueNode
	assert first_value.value? == '1'
	assert second_value.value? == '2'
}

fn test_number_literals() ! {
	result := parse('{ field(zero: 0, negative: -1, decimal: 1.25, positive: -2e+3, small: 2E-4) }',
		none)!
	operation := result.definitions[0] as OperationDefinitionNode
	field := operation.selection_set.selections[0] as FieldNode
	arguments := field.arguments or { panic('missing number arguments') }
	assert arguments.len == 5
	zero := arguments[0].value as IntValueNode
	negative := arguments[1].value as IntValueNode
	decimal := arguments[2].value as FloatValueNode
	positive := arguments[3].value as FloatValueNode
	small := arguments[4].value as FloatValueNode
	assert zero.value? == '0'
	assert negative.value? == '-1'
	assert decimal.value? == '1.25'
	assert positive.value? == '-2e+3'
	assert small.value? == '2E-4'
}

fn test_fragment_spreads_inline_fragments_and_definitions() ! {
	body := 'query User {
		user {
			...UserFields @include(if: true)
			... on User @skip(if: false) { id }
			... @include(if: true) { name }
			... { nickname }
		}
	}
	fragment UserFields on User @include(if: true) { id name }'
	result := parse(body, none)!
	assert result.definitions.len == 2
	operation := result.definitions[0] as OperationDefinitionNode
	user := operation.selection_set.selections[0] as FieldNode
	selections := user.selection_set?.selections
	assert selections.len == 4
	spread := selections[0] as FragmentSpreadNode
	assert spread.kind == .fragment_spread
	assert spread.name.value == 'UserFields'
	assert spread.directives?[0].name.value == 'include'
	typed := selections[1] as InlineFragmentNode
	assert typed.kind == .inline_fragment
	assert typed.type_condition?.node_type.value == 'User'
	assert typed.directives?[0].name.value == 'skip'
	typed_field := typed.selection_set.selections[0] as FieldNode
	assert typed_field.name.value == 'id'
	untyped := selections[2] as InlineFragmentNode
	assert untyped.type_condition == none
	assert untyped.directives?[0].name.value == 'include'
	untyped_field := untyped.selection_set.selections[0] as FieldNode
	assert untyped_field.name.value == 'name'
	bare := selections[3] as InlineFragmentNode
	assert bare.type_condition == none
	bare_field := bare.selection_set.selections[0] as FieldNode
	assert bare_field.name.value == 'nickname'
	fragment := result.definitions[1] as FragmentDefinitionNode
	assert fragment.kind == .fragment_definition
	assert fragment.name.value == 'UserFields'
	assert fragment.type_condition.node_type.value == 'User'
	assert fragment.directives[0].name.value == 'include'
	assert fragment.selection_set.selections.len == 2
	fragment_id := fragment.selection_set.selections[0] as FieldNode
	fragment_name := fragment.selection_set.selections[1] as FieldNode
	assert fragment_id.name.value == 'id'
	assert fragment_name.name.value == 'name'
}

fn test_escaped_string_values() ! {
	cases := {
		r'a\nb':                     'a\nb'
		r'a\"b':                     'a"b'
		r'a\\b':                     'a\\b'
		r'a\/b':                     'a/b'
		r'a\b\f\n\r\tz':             'a\b\f\n\r\tz'
		r'\u0061\u03BB\uD83D\uDE00': 'aλ😀'
		r'a\u{1F600}b\u{03BB}c':     'a😀bλc'
		r'\u{00000061}\u{10FFFF}':   'a' + rune(0x10ffff).str()
	}
	for literal, expected in cases {
		body := '{ field(argument: "${literal}") }'
		result := parse(body, none)!
		operation := result.definitions[0] as OperationDefinitionNode
		field := operation.selection_set.selections[0] as FieldNode
		value := field.arguments?[0].value as StringValueNode
		assert value.value? == expected, literal
		location := value.loc or { panic('missing string location') }
		assert body[location.start..location.end] == '"${literal}"'
	}
}

fn test_malformed_input_returns_errors() {
	invalid_inputs := [
		'',
		'%',
		'{',
		'{ user { id }',
		'{ field(argument:) }',
		'{ field(argument: [1) }',
		'{ field(argument: {name:}) }',
		'{ field } %',
		'{ field } .',
		'.',
		'..',
		'"',
		'"unterminated',
		'"' + '\\',
		r'"""unterminated',
		r'"""unterminated\',
		r'{ field(argument: "bad\q") }',
		r'{ field(argument: "\u") }',
		r'{ field(argument: "\u12',
		r'{ field(argument: "\uZZZZ") }',
		r'{ field(argument: "\uD800") }',
		r'{ field(argument: "\uDC00") }',
		r'{ field(argument: "\uD800\u0041") }',
		r'{ field(argument: "\u{}") }',
		r'{ field(argument: "\u{110000}") }',
		r'{ field(argument: "\u{D800}") }',
		r'{ field(argument: "\u{xyz}") }',
		r'{ field(argument: "\u{000000000}") }',
		r'{ field(argument: "\u{1F600',
		'-',
		'0',
		'123',
		'1.',
		'1e',
		'1e+',
		'{ field(argument: --1) }',
		'{ field(argument: 01) }',
		'{ field(argument: 1a) }',
		'\r',
		'fragment on on User { id }',
		'fragment UserFields User { id }',
		'{ ... on }',
		r'query Q($x: Int = $other) { field }',
	]
	for body in invalid_inputs {
		parse(body, none) or {
			assert err.msg() != '', body
			continue
		}
		assert false, 'expected an error for ${body}'
	}
}

fn test_token_limit_returns_error() ! {
	body := '{ id } # ignored comment'
	result := parse(body, ParserOptions{ max_tokens: 3 })!
	assert result.token_count == 3
	parse(body, ParserOptions{ max_tokens: 2 }) or {
		assert err.msg().contains('max of 2 tokens')
		return
	}
	assert false, 'expected an error when the token limit is exceeded'
}
