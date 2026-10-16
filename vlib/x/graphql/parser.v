module graphql

pub struct ParserOptions {
pub:
	// By default, the parser creates AST nodes that know the location
	// in the source that they correspond to. This configuration flag
	// disables that behavior for performance or testing.
	no_location bool
	// Parser CPU and memory usage is linear to the number of tokens in a document
	// however in extreme cases it becomes quadratic due to memory exhaustion.
	// Parsing happens before validation so even invalid queries can burn lots of
	// CPU time and memory.
	// To prevent this you can set a maximum number of tokens allowed within a document.
	// @default 50K tokens
	max_tokens int = 50_000
	// @deprecated
	allow_legacy_fragment_variables            ?bool
	experimental_client_controlled_nullability ?bool
}

pub struct Parser {
	options ParserOptions
mut:
	lexer         Lexer
	token_counter int
}

type SourceOrString = Source | string

fn Parser.new(source SourceOrString, options ?ParserOptions) !&Parser {
	// initial empty source
	mut source_object := Source.new('', none, none)

	if source is string {
		source_object = Source.new(source, none, none)
	} else {
		source_object = source as Source
	}

	return &Parser{
		lexer: Lexer.new(source_object)
		options: options or { ParserOptions{} }
		token_counter: 0
	}
}

pub fn parse(source SourceOrString, options ?ParserOptions) !DocumentNode {
	mut parser := Parser.new(source, options)!
	return parser.parse_document()!
}

// pub fn parse_value(source SourceOrString, options ?ParserOptions) !ValueNode {
// 	mut parser := Parser.new(source, options)!

// 	parser.expect_token(TokenKind.sof)!

// 	value := parser
// }

fn (mut p Parser) parse_value_literal(is_const bool) !ValueNode {
	token := p.lexer.token

	match token.kind {
		.bracket_l {
			// println('== PARSE VALUE LITERAL (bracket_l)')
			return p.parse_list(is_const)!
		}
		.brace_l {
			// println('== PARSE VALUE LITERAL (brace_l)')
			return p.parse_object(is_const)!
		}
		.integer {
			// println('== PARSE VALUE LITERAL (integer)')
			p.advance_lexer()!

			return p.node[IntValueNode](token, mut IntValueNode{
				kind: Kind.int_value
				value: token.value
			})!
		}
		.float {
			// println('== PARSE VALUE LITERAL (float)')

			p.advance_lexer()!

			return p.node[FloatValueNode](token, mut FloatValueNode{
				kind: Kind.float_value
				value: token.value
			})!
		}
		.string_value, .block_string {
			// println('----- PARSING STRING LITERAL')
			return p.parse_string_literal()!
		}
		.dollar {
			// println('== PARSE VALUE LITERAL (dollar)')

			if is_const {
				p.expect_token(TokenKind.dollar)!

				if p.lexer.token.kind == TokenKind.name {
					var_name := p.lexer.token.value
					error('Unexpected variable ${var_name}')
				} else {
					// println('UNEXPECTED: parse_value_literal (dollar)')

					p.unexpected(token)!
				}
			}

			return p.parse_variable()!
		}
		.name {
			// println('== PARSE VALUE LITERAL (name)')

			p.advance_lexer()!

			match token.value or { '' } {
				'true' {
					return p.node[BooleanValueNode](token, mut BooleanValueNode{
						kind: Kind.boolean
						value: true
					})!
				}
				'false' {
					return p.node[BooleanValueNode](token, mut BooleanValueNode{
						kind: Kind.boolean
						value: false
					})!
				}
				'null' {
					return p.node[NullValueNode](token, mut NullValueNode{
						kind: Kind.null
					})!
				}
				else {
					return p.node[EnumValueNode](token, mut EnumValueNode{
						kind: Kind.enum_value
						value: token.value or { '' }
					})!
				}
			}
		}
		else {
			// println('UNEXPECTED: parse_value_literal')
			p.unexpected(token)!
			return error('')
		}
	}
}

fn (mut p Parser) parse_variable() !VariableNode {
	start := p.lexer.token

	p.expect_token(TokenKind.dollar)!

	return p.node[VariableNode](start, mut VariableNode{
		kind: Kind.variable
		name: p.parse_name()!
	})
}

fn (mut p Parser) parse_list(is_const bool) !ListValueNode {
	item := fn [mut p, is_const] () !ValueNode {
		return p.parse_value_literal(is_const)!
	}

	p.expect_token(TokenKind.dollar)!

	return p.node[ListValueNode](p.lexer.token, mut ListValueNode{
		kind: Kind.list
		values: p.any(TokenKind.bracket_l, item, TokenKind.bracket_r)
	})
}

fn (mut p Parser) parse_object(is_const bool) !ObjectValueNode {
	item := fn [mut p, is_const] () !ObjectFieldNode {
		return p.parse_object_field(is_const)
	}

	return p.node[ObjectValueNode](p.lexer.token, mut ObjectValueNode{
		kind: Kind.object
		fields: p.any(TokenKind.brace_l, item, TokenKind.brace_r)
	})
}

// ObjectField[Const] : Name : Value[?Const]
fn (mut p Parser) parse_object_field(is_const bool) !ObjectFieldNode {
	start := p.lexer.token
	name := p.parse_name()!

	p.expect_token(TokenKind.colon)!

	return p.node[ObjectFieldNode](start, mut ObjectFieldNode{
		kind: Kind.object_field
		name: name
		value: p.parse_value_literal(is_const)!
	})
}

fn (mut p Parser) parse_name() !NameNode {
	token := p.expect_token(TokenKind.name)!
	node := p.node[NameNode](token, mut NameNode{
		kind: Kind.name
		value: token.value or { return error('Missing name node') }
	})!

	return node
}

fn (mut p Parser) parse_document() !DocumentNode {
	p.expect_token(TokenKind.sof)!
	mut definitions := []DefinitionNode{}

	for {
		definitions << p.parse_definition()!

		if p.expect_optional_token(TokenKind.eof) {
			break
		}
	}

	// println('FINISHED doc ${definitions.len}')

	return p.node[DocumentNode](p.lexer.token, mut DocumentNode{
		kind: Kind.document
		definitions: definitions
		token_count: p.token_counter
	})!
}

// Definition :
//   - ExecutableDefinition
//   - TypeSystemDefinition
//   - TypeSystemExtension
//
// ExecutableDefinition :
//   - OperationDefinition
//   - FragmentDefinition
//
// TypeSystemDefinition :
//   - SchemaDefinition
//   - TypeDefinition
//   - DirectiveDefinition
//
// TypeDefinition :
//   - ScalarTypeDefinition
//   - ObjectTypeDefinition
//   - InterfaceTypeDefinition
//   - UnionTypeDefinition
//   - EnumTypeDefinition
//   - InputObjectTypeDefinition
fn (mut parser Parser) parse_definition() !DefinitionNode {
	// println('!!parse_definition')

	if parser.peek(TokenKind.brace_l) {
		return parser.parse_operation_definition()!
	}

	// Many definitions begin with a description and require a lookahead.
	has_description := parser.peek_description()

	keyword_token := if has_description {
		parser.lexer.lookahead()
	} else {
		parser.lexer.token
	}

	// println('+++ NAME ${keyword_token.kind.gql_str()} VALUE: ${keyword_token.value}')
	if keyword_token.kind == TokenKind.name {
		// println('KEYWORD ${keyword_token.value}')
		keyword := keyword_token.value or { return error('Missing keyword value') }
		match keyword {
			'schema' {
				return parser.parse_schema_definition()!
			}
			'scalar' {
				return parser.parse_scalar_type_definition()!
			}
			'type' {
				return parser.parse_object_type_definition()!
			}
			'interface' {
				return parser.parse_interface_type_definition()!
			}
			'union' {
				return parser.parse_union_type_definition()!
			}
			'enum' {
				return parser.parse_enum_type_definition()!
			}
			'input' {
				return parser.parse_input_object_type_definition()!
			}
			'directive' {
				// println('=== DIRECTIVE')
				// return parser.parsedire
			}
			else {
				// Unsupported
			}
		}

		if has_description {
			return error('Unexpected description for "${keyword_token.to_json()}", descriptions only in type definitions')
		}

		match keyword_token.value or { '' } {
			'query', 'mutation', 'subscription' {
				return parser.parse_operation_definition()!
			}
			'fragment' {
				return parser.parse_fragment_definition()
			}
			else {
				return error('not supported')
			}
		}
	}

	return error('Unexpected ${keyword_token.kind}. Previous: (${parser.lexer.last_token.kind}, ${parser.lexer.last_token.value})')
}

fn (mut parser Parser) parse_enum_type_definition() !EnumTypeDefinitionNode {
	start := parser.lexer.token
	description := parser.parse_description()

	parser.expect_keyword('enum')!

	name := parser.parse_enum_value_name()!
	directives := parser.parse_const_directives()!
	values := parser.parse_enum_values_definition()!

	return parser.node[EnumTypeDefinitionNode](start, mut EnumTypeDefinitionNode{
		kind: Kind.enum_type_definition
		name: name
		directives: directives
		values: values
		description: description
	})
}

fn (mut parser Parser) parse_enum_values_definition() ![]EnumValueDefinitionNode {
	mut nodes := []EnumValueDefinitionNode{}

	// optional_many
	if parser.expect_optional_token(TokenKind.brace_l) {
		for {
			nodes << parser.parse_enum_value_definition()!

			if parser.expect_optional_token(TokenKind.brace_r) {
				break
			}
		}
	}

	return nodes
}

fn (mut parser Parser) parse_enum_value_definition() !EnumValueDefinitionNode {
	start := parser.lexer.token
	description := parser.parse_description()
	name := parser.parse_name()!
	directives := parser.parse_const_directives()!

	return parser.node[EnumValueDefinitionNode](start, mut EnumValueDefinitionNode{
		kind: Kind.enum_value_definition
		name: name
		description: description
		directives: directives
	})
}

fn (mut parser Parser) parse_enum_value_name() !NameNode {
	match parser.lexer.token.value or { return error('No name node for ENUM') } {
		'true', 'false', 'null' {
			return error('${parser.lexer.token.value} is reserved and cannot be used for an enum value')
		}
		else {
			return parser.parse_name()!
		}
	}
}

fn (mut parser Parser) parse_union_type_definition() !UnionTypeDefinitionNode {
	start := parser.lexer.token
	description := parser.parse_description()

	parser.expect_keyword('union')!

	name := parser.parse_name()!
	directives := parser.parse_const_directives()!
	types := parser.parse_union_member_types()!

	return parser.node[UnionTypeDefinitionNode](start, mut UnionTypeDefinitionNode{
		kind: Kind.union_type_definition
		name: name
		directives: directives
		types: types
		description: description
	})
}

fn (mut parser Parser) parse_union_member_types() ![]NamedTypeNode {
	mut nodes := []NamedTypeNode{}

	// delimited_many
	if parser.expect_optional_token(TokenKind.equals) {
		parser.expect_optional_token(TokenKind.pipe)

		for {
			nodes << parser.parse_named_type()!

			if !parser.expect_optional_token(TokenKind.pipe) {
				break
			}
		}
	}

	return nodes
}

fn (mut parser Parser) parse_input_object_type_definition() !InputObjectTypeDefinitionNode {
	start := parser.lexer.token
	description := parser.parse_description()

	parser.expect_keyword('input')!

	name := parser.parse_name()!
	directives := parser.parse_directives(true)!
	fields := parser.parse_input_fields_definition()!

	return parser.node[InputObjectTypeDefinitionNode](start, mut InputObjectTypeDefinitionNode{
		kind: Kind.input_object_type_definition
		name: name
		directives: directives
		fields: fields
		description: description
	})
}

fn (mut parser Parser) parse_interface_type_definition() !InterfaceTypeDefinitionNode {
	start := parser.lexer.token
	description := parser.parse_description()

	parser.expect_keyword('interface')!

	name := parser.parse_name()!
	interfaces := parser.parse_implements_interfaces()!
	directives := parser.parse_directives(true)!
	fields := parser.parse_fields_definition()!

	return parser.node[InterfaceTypeDefinitionNode](start, mut InterfaceTypeDefinitionNode{
		kind: Kind.scalar_type_definition
		name: name
		fields: fields
		interfaces: interfaces
		directives: directives
		description: description
	})
}

fn (mut parser Parser) parse_input_object_type_extension() !InputObjectTypeExtensionNode {
	start := parser.lexer.token

	parser.expect_keyword('extend')!
	parser.expect_keyword('input')!

	name := parser.parse_name()!

	directives := parser.parse_directives(true)!
	fields := parser.parse_input_fields_definition()!

	if directives.len == 0 && fields.len == 0 {
		parser.unexpected(parser.lexer.token)!
	}

	return parser.node[InputObjectTypeExtensionNode](start, mut InputObjectTypeExtensionNode{
		kind: Kind.input_object_type_extension
		name: name
		directives: directives
		fields: fields
	})
}

fn (mut parser Parser) parse_input_value_def() !InputValueDefinitionNode {
	start := parser.lexer.token
	description := parser.parse_description()
	name := parser.parse_name()!

	parser.expect_token(TokenKind.colon)!

	type_node := parser.parse_type_reference()!

	if parser.expect_optional_token(TokenKind.equals) {
		default_value := parser.parse_const_value_literal()
		directives := parser.parse_directives(true)!
		return parser.node[InputValueDefinitionNode](start, mut InputValueDefinitionNode{
			kind: Kind.input_value_definition
			description: description
			name: name
			@type: type_node
			default_value: default_value
			directives: directives
		})
	}

	directives := parser.parse_directives(true)!
	return parser.node[InputValueDefinitionNode](start, mut InputValueDefinitionNode{
		kind: Kind.input_value_definition
		description: description
		name: name
		@type: type_node
		default_value: none
		directives: directives
	})!
}

fn (mut parser Parser) parse_input_fields_definition() ![]InputValueDefinitionNode {
	mut nodes := []InputValueDefinitionNode{}

	// optional_many
	if parser.expect_optional_token(TokenKind.brace_l) {
		for {
			nodes << parser.parse_input_value_def()!

			if parser.expect_optional_token(TokenKind.brace_r) {
				break
			}
		}
	}

	return nodes
}

fn (mut parser Parser) parse_object_type_definition() !ObjectTypeDefinitionNode {
	start := parser.lexer.token
	description := parser.parse_description()

	parser.expect_keyword('type')!

	name := parser.parse_name()!
	interfaces := parser.parse_implements_interfaces()!
	directives := parser.parse_directives(true)!
	fields := parser.parse_fields_definition()!

	return parser.node[ObjectTypeDefinitionNode](start, mut ObjectTypeDefinitionNode{
		kind: Kind.object_type_definition
		description: description
		name: name
		interfaces: interfaces
		directives: directives
		fields: fields
	})
}

fn (mut parser Parser) parse_implements_interfaces() ![]NamedTypeNode {
	mut nodes := []NamedTypeNode{}
	if parser.expect_optional_keyword('implements')! {
		parser.expect_optional_token(TokenKind.amp)

		for {
			nodes << parser.parse_named_type()!

			if !parser.expect_optional_token(TokenKind.amp) {
				break
			}
		}
	}

	return nodes
}

fn (mut parser Parser) parse_fields_definition() ![]FieldDefinitionNode {
	mut nodes := []FieldDefinitionNode{}

	// optional_many
	if parser.expect_optional_token(TokenKind.brace_l) {
		for {
			nodes << parser.parse_field_definition()!

			if parser.expect_optional_token(TokenKind.brace_r) {
				break
			}
		}
	}

	return nodes
}

fn (mut parser Parser) parse_field_definition() !FieldDefinitionNode {
	start := parser.lexer.token
	description := parser.parse_description()
	name := parser.parse_name()!
	args := parser.parse_argument_defs()!

	parser.expect_token(TokenKind.colon)!

	@type := parser.parse_type_reference()!
	directives := parser.parse_directives(true)!

	return parser.node[FieldDefinitionNode](start, mut FieldDefinitionNode{
		kind: Kind.field_definition
		description: description
		name: name
		arguments: args
		directives: directives
		@type: @type
	})
}

fn (mut parser Parser) parse_argument_defs() ![]InputValueDefinitionNode {
	mut nodes := []InputValueDefinitionNode{}

	// println('FN: parse_argument_defs')
	if parser.expect_optional_token(TokenKind.paren_l) {
		for {
			nodes << parser.parse_input_value_def()!

			if parser.expect_optional_token(TokenKind.paren_r) {
				break
			}
		}
	}

	return nodes
}

fn (mut parser Parser) parse_type_reference() !TypeNode {
	start := parser.lexer.token

	mut named_type_node := ?NamedTypeNode{}
	mut type_node := ListTypeNode{}

	if parser.expect_optional_token(TokenKind.bracket_l) {
		inner_type := parser.parse_type_reference()!

		parser.expect_token(TokenKind.bracket_r)!

		type_node = parser.node[ListTypeNode](start, mut ListTypeNode{
			kind: Kind.list_type
			node_type: inner_type
		})!
	} else {
		named_type_node = parser.parse_named_type()!
	}

	if final_type := named_type_node {
		if parser.expect_optional_token(TokenKind.bang) {
			return TypeNode(parser.node(start, mut NonNullTypeNode{
				kind: Kind.non_null_type
				node_type: final_type
			})!)
		}

		return final_type
	} else {
		if parser.expect_optional_token(TokenKind.bang) {
			return TypeNode(parser.node(start, mut NonNullTypeNode{
				kind: Kind.non_null_type
				node_type: type_node
			})!)
		}

		return type_node
	}

	return error('could not return type reference')
}

fn (mut parser Parser) parse_scalar_type_definition() !ScalarTypeDefinitionNode {
	start := parser.lexer.token
	description := parser.parse_description()

	parser.expect_keyword('scalar')!

	name := parser.parse_name()!
	directives := parser.parse_directives(true)!

	return parser.node[ScalarTypeDefinitionNode](start, mut ScalarTypeDefinitionNode{
		kind: Kind.scalar_type_extension
		description: description
		name: name
		directives: directives
	})
}

fn (mut p Parser) parse_description() ?StringValueNode {
	if p.peek_description() {
		return p.parse_string_literal() or { 
			panic(err) 
			return none
		}
	}

	return none
}

fn (mut p Parser) parse_string_literal() !StringValueNode {
	token := p.lexer.token
	p.advance_lexer()!

	node := p.node[StringValueNode](token, mut StringValueNode{
		kind: Kind.string_value
		value: token.value
		block: token.kind == TokenKind.block_string
	})!

	return node
}

fn (mut parser Parser) parse_const_value_literal() ?ValueNode {
	return parser.parse_value_literal(true) or { 
		panic(err)
		return none
	}
}

fn (mut p Parser) parse_schema_definition() !SchemaDefinitionNode {
	start := p.lexer.token
	description := p.parse_description()

	p.expect_keyword('schema')!
	p.expect_token(TokenKind.brace_l)!

	mut operation_types := []OperationTypeDefinitionNode{}

	for {
		operation_types << p.parse_operation_type_definition()!

		if p.expect_optional_token(TokenKind.brace_r) {
			break
		}
	}

	node := p.node[SchemaDefinitionNode](start, mut SchemaDefinitionNode{
		kind: Kind.schema_definition
		description: description
		directives: []
		operation_types: operation_types
	})!

	return node
}

fn (mut parser Parser) parse_operation_definition() !OperationDefinitionNode {
	// println('..parsing operation')
	start := parser.lexer.token

	if parser.peek(TokenKind.brace_l) {
		// println('...next node is brace_l')
		node := parser.node[OperationDefinitionNode](start, mut OperationDefinitionNode{
			kind: Kind.operation_definition
			operation: OperationTypeNode.query
			name: none
			variable_definitions: []
			directives: []
			selection_set: parser.parse_selection_set()
		})!

		// println('...created operationdefinitionnode')
		return node
	}

	// println('...operation defintino')

	operation := parser.parse_operation_type()!
	mut name := ?NameNode{}

	if parser.peek(TokenKind.name) {
		name = parser.parse_name()!
	}

	return parser.node[OperationDefinitionNode](start, mut OperationDefinitionNode{
		kind: Kind.operation_definition
		operation: operation
		name: name
		variable_definitions: parser.parse_variable_definitions()
		directives: parser.parse_directives(false) or { panic(err) }
		selection_set: parser.parse_selection_set()
	})!
}

fn (mut parser Parser) scoped_parse_section() SelectionNode {
	return parser.parse_selection()
}

fn (mut parser Parser) parse_selection_set() SelectionSetNode {
	// println('...parsing selectionsetnode')
	parser.expect_token(TokenKind.brace_l) or { panic(err) }

	mut selections := []SelectionNode{}

	for {
		selections << parser.parse_selection()

		if parser.expect_optional_token(TokenKind.brace_r) {
			break
		}
	}

	return parser.node[SelectionSetNode](parser.lexer.token, mut SelectionSetNode{
		kind: Kind.selection_set
		selections: selections
	}) or { panic('could not create selection nodes') }
}

fn (mut parser Parser) parse_selection() SelectionNode {
	if parser.peek(TokenKind.spread) {
		return parser.parse_fragment()
	} else {
		return parser.parse_field()
	}
}

fn (mut parser Parser) parse_fragment() FragmentSpreadNode {
	start := parser.lexer.token
	parser.expect_token(TokenKind.spread) or { panic(err) }

	has_type_condition := parser.expect_optional_keyword('on') or { false }

	if !has_type_condition && parser.peek(TokenKind.name) {
		return parser.node[FragmentSpreadNode](start, mut FragmentSpreadNode{
			kind: Kind.fragment_spread
			name: NameNode{}
			directives: parser.parse_directives(false) or { panic(err) }
		}) or { panic(err) }
	}

	return parser.node[FragmentSpreadNode](start, mut FragmentSpreadNode{
		kind: Kind.fragment_spread
		name: NameNode{}
		directives: parser.parse_directives(false) or { panic(err) }
	}) or { panic(err) }
}

fn (mut parser Parser) parse_fragment_definition() FragmentDefinitionNode {
	start := parser.lexer.token
	parser.expect_keyword('fragment') or { panic(err) }

	return parser.node[FragmentDefinitionNode](start, mut FragmentDefinitionNode{
		kind: Kind.fragment_definition
		directives: parser.parse_directives(false) or { panic(err) }
		selection_set: parser.parse_selection_set()
	}) or { panic(err) }
}

fn (mut parser Parser) parse_field() FieldNode {
	start := parser.lexer.token

	name_or_alias := parser.parse_name() or { panic('could not parse field name ${err}') }
	mut name := NameNode{}
	mut alias := NameNode{}

	if parser.expect_optional_token(TokenKind.colon) {
		alias = name_or_alias
		name = parser.parse_name() or { panic('cant parse alias name') }
	} else {
		name = name_or_alias
	}

	selection_set := fn [mut parser] () ?SelectionSetNode {
		if parser.peek(TokenKind.brace_l) {
			parser.parse_selection_set()
		}

		return none
	}

	return parser.node[FieldNode](start, mut FieldNode{
		kind: Kind.field
		alias: alias
		name: name
		arguments: parser.parse_arguments(false)
		nullability_assertion: none
		directives: parser.parse_directives(false) or { panic(err) }
		selection_set: selection_set()
	}) or { panic(err) }
}

fn (mut parser Parser) parse_argument_not_const() ArgumentNode {
	return parser.parse_argument(false)
}

fn (mut parser Parser) parse_const_argument() ArgumentNode {
	return parser.parse_argument(true)
}

fn (mut parser Parser) parse_arguments(is_const bool) []ArgumentNode {
	mut nodes := []ArgumentNode{}

	// optional_many
	if parser.expect_optional_token(TokenKind.paren_l) {
		for {
			if is_const {
				nodes << parser.parse_const_argument()
			} else {
				nodes << parser.parse_argument_not_const()
			}

			if parser.expect_optional_token(TokenKind.paren_r) {
				break
			}
		}
	}

	return nodes
}

fn (mut parser Parser) parse_argument(is_const bool) ArgumentNode {
	start := parser.lexer.token
	name := parser.parse_name() or { panic(err) }

	parser.expect_token(TokenKind.colon) or { panic(err) }

	return parser.node[ArgumentNode](start, mut ArgumentNode{
		kind: Kind.argument
		name: name
		value: parser.parse_value_literal(is_const) or { panic(err) }
	}) or { panic(err) }
}

fn (mut parser Parser) parse_variable_definitions() []VariableDefinitionNode {
	mut nodes := []VariableDefinitionNode{}

	if parser.expect_optional_token(TokenKind.paren_l) {
		for {
			nodes << parser.parse_variable_definition() or { panic(err) }

			if parser.expect_optional_token(TokenKind.paren_r) {
				break
			}
		}
	}

	return nodes
}

fn (mut parser Parser) parse_variable_definition() !VariableDefinitionNode {
	start := parser.lexer.token
	variable := parser.parse_variable()!
	parser.expect_token(TokenKind.colon)!
	var_type := parser.parse_type_reference()!
	mut default_value := ?ValueNode(none)
	if parser.expect_optional_token(TokenKind.equals) {
		default_value = parser.parse_const_value_literal()
	}
	directives := parser.parse_directives(true)!

	var_directives := if directives.len > 0 { ?[]DirectiveNode(directives) } else { none }
	mut var_node := VariableDefinitionNode{
		kind: Kind.variable_definition
		variable: variable
		@type: var_type
		default_value: default_value
		directives: var_directives
	}
	return parser.node[VariableDefinitionNode](start, mut var_node)!
}

fn (mut p Parser) parse_named_type() !NamedTypeNode {
	node := p.node[NamedTypeNode](p.lexer.token, mut NamedTypeNode{
		kind: Kind.named_type
		node_type: p.parse_name()!
	})!

	return node
}

fn (mut p Parser) parse_operation_type() !OperationTypeNode {
	operation_token := p.expect_token(TokenKind.name)!

	return match operation_token.value or { '' } {
		'query' {
			OperationTypeNode.query
		}
		'mutation' {
			OperationTypeNode.mutation
		}
		'subscription' {
			OperationTypeNode.subscription
		}
		else {
			error('Unexpected token')
		}
	}
}

fn (mut p Parser) parse_operation_type_definition() !OperationTypeDefinitionNode {
	start := p.lexer.token

	operation := p.parse_operation_type()!
	p.expect_token(TokenKind.colon)!

	@type := p.parse_named_type()!

	node := p.node[OperationTypeDefinitionNode](start, mut OperationTypeDefinitionNode{
		kind: Kind.operation_type_definition
		operation: operation
		@type: @type
	})!

	return node
}

fn (p Parser) peek_description() bool {
	return p.peek(TokenKind.string_value) || p.peek(TokenKind.block_string)
}

// Returns a node that, if configured to do so, sets a "loc" field as a
// location object, used to identify the place in the source that created a
// given parsed object.
fn (parser Parser) node[T](start_token &Token, mut node T) !T {
	if !parser.options.no_location {
		node.loc = Location.new(start_token, parser.lexer.last_token, parser.lexer.source)
	}

	return node

	// return error('failed to convert node')
}

// Determines if the next token is of a given kind
fn (p Parser) peek(kind TokenKind) bool {
	return p.lexer.token.kind == kind
}

// If the next token is of the given kind, return that token after advancing the lexer.
// Otherwise, do not change the parser state and throw an error.
fn (mut p Parser) expect_token(kind TokenKind) !&Token {
	token := p.lexer.token

	// println('prev token ${token.kind}')
	if token.kind == kind {
		p.advance_lexer()!
		return token
	}

	return error('Expected ${kind.gql_str()} got ${token.kind.gql_str()}')
}

// If the next token is of the given kind, return "true" after advancing the lexer.
// Otherwise, do not change the parser state and return "false".
fn (mut p Parser) expect_optional_token(kind TokenKind) bool {
	token := p.lexer.token

	// println('current lexer token: ${token.kind}, expected: ${kind}')

	if token.kind == kind {
		p.advance_lexer() or { return false }
		return true
	}

	return false
}

// If the next token is a given keyword, advance the lexer.
// Otherwise, do not change the parser state and throw an error.
fn (mut p Parser) expect_keyword(value string) ! {
	token := p.lexer.token
	val := token.value or { '' }

	// println('>>>>> TOKEN (${token.kind}) value: "${val}" compared to expected "${value}"')

	if token.kind == TokenKind.name && val == value {
		p.advance_lexer()!
		return
	}

	return error('Expected ${value}, got ${token.value}')
}

// If the next token is a given keyword, return "true" after advancing the lexer.
// Otherwise, do not change the parser state and return "false".
fn (mut p Parser) expect_optional_keyword(value string) !bool {
	token := p.lexer.token
	val := token.value or { '' }

	if token.kind == TokenKind.name && val == value {
		p.advance_lexer()!
		return true
	}

	return false
}

// Helper function for creating an error when an unexpected lexed token is encountered.
fn (parser Parser) unexpected(at_token ?&Token) ! {
	token := at_token or { parser.lexer.token }

	return error('Unexpected token ${token.to_json()}')
}

// Returns a possibly empty list of parse nodes, determined by the parseFn.
// This list begins with a lex token of openKind and ends with a lex token of closeKind.
// Advances the parser to the next lex token after the closing token.
fn (mut p Parser) any[T](open_kind TokenKind, parse_fn fn () T, close_kind TokenKind) []T {
	p.expect_token(open_kind) or { panic('err') }
	mut nodes := []T{}

	for {
		if p.expect_optional_token(close_kind) {
			break
		}

		nodes << parse_fn()
	}

	return nodes
}

fn (mut parser Parser) advance_lexer() ! {
	// println('...advancing lexer')
	token := parser.lexer.advance()

	// println('...advanced to ${token.kind} from previous')

	max_tokens := parser.options.max_tokens

	if token.kind != TokenKind.eof {
		parser.token_counter += 1

		if parser.token_counter > max_tokens {
			panic('Document currently contains (${parser.token_counter}) more than the defined max of ${max_tokens} tokens. Aborting')
		}
	}

	// println('...finished advancing')
}

fn (mut parser Parser) parse_directive(is_const bool) !DirectiveNode {
	start := parser.lexer.token

	parser.expect_token(TokenKind.at)!

	return parser.node[DirectiveNode](start, mut DirectiveNode{
		kind: Kind.directive
		name: parser.parse_name()!
		arguments: parser.parse_arguments(is_const)
		is_const: is_const
	})
}

fn (mut parser Parser) parse_directives(is_const bool) ![]DirectiveNode {
	mut directives := []DirectiveNode{}

	for {
		if !parser.peek(TokenKind.at) {
			break
		}

		directives << parser.parse_directive(is_const)!
	}

	return directives
}

fn (mut parser Parser) parse_const_directives() ![]DirectiveNode {
	return parser.parse_directives(true)
}

fn get_token_kind_desc(kind TokenKind) string {
	if is_punctuator_token_kind(kind) {
		return '"${kind.gql_str()}"'
	}

	return kind.gql_str()
}
