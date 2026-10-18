module graphql

pub struct Location {
pub:
	// The character offset at which this Node begins.
	start int
	// The character offset at which this Node ends.
	end int
	// The Token at which this Node begins.
	start_token ?&Token
	// The Token at which this Node ends.
	end_token ?&Token
	// The Source document the AST represents.
	source Source
}

fn Location.new(start_token &Token, end_token &Token, source Source) Location {
	return Location{
		start: start_token.start
		end: end_token.end
		start_token: start_token
		end_token: end_token
		source: source
	}
}

fn (l Location) get_symbol_tag() string {
	return 'Location'
}

fn (l Location) to_json() {
	// @todo
}

@[heap]
pub struct Token {
pub:
	kind TokenKind
	// The character offset at which this Node begins.
	start int
	// The character offset at which this Node ends.
	end int
	// The 1-indexed line number on which this Token appears.
	line int
	// The 1-indexed column number at which this Token begins.
	column int
	// For non-punctuation tokens, represents the interpreted value of the token.
	// <br>
	// **Note**: is `none` for punctuation tokens, but typed as string for
	// convenience in the parser.
	value ?string
mut:
	// Tokens exist as nodes in a double-linked-list amongst all tokens
	// including ignored tokens. <SOF> is always the first node and <EOF>
	// the last.
	prev ?&Token
	next ?&Token
}

fn Token.new(kind TokenKind, start int, end int, line int, column int) Token {
	return Token{
		kind: kind
		start: start
		end: end
		line: line
		column: column
	}
}

fn (t Token) get_symbol_tag() string {
	return 'Token'
}

fn (t Token) to_json() map[string]string {
	return {
		'kind':   t.kind.str()
		'value':  t.value or { '' }
		'line':   t.line.str()
		'column': t.column.str()
	}
}

// All AST Nodes should implement a MUTable common loc property
pub interface ASTNodeInterface {
mut:
	loc ?Location
}

// The list of all possible AST node types.
pub type ASTNode = ArgumentNode
	| BooleanValueNode
	| DirectiveDefinitionNode
	| DirectiveNode
	| DocumentNode
	| EnumTypeDefinitionNode
	| EnumTypeExtensionNode
	| EnumValueDefinitionNode
	| EnumValueNode
	| ErrorBoundaryNode
	| FieldDefinitionNode
	| FieldNode
	| FloatValueNode
	| FragmentDefinitionNode
	| FragmentSpreadNode
	| InlineFragmentNode
	| InputObjectTypeDefinitionNode
	| InputObjectTypeExtensionNode
	| InputValueDefinitionNode
	| IntValueNode
	| InterfaceTypeDefinitionNode
	| InterfaceTypeExtensionNode
	| ListNullabilityOperatorNode
	| ListTypeNode
	| ListValueNode
	| NameNode
	| NamedTypeNode
	| NonNullAssertionNode
	| NonNullTypeNode
	| NullValueNode
	| ObjectFieldNode
	| ObjectTypeDefinitionNode
	| ObjectTypeExtensionNode
	| ObjectValueNode
	| OperationDefinitionNode
	| OperationTypeDefinitionNode
	| ScalarTypeDefinitionNode
	| ScalarTypeExtensionNode
	| SchemaDefinitionNode
	| SchemaExtensionNode
	| SelectionSetNode
	| StringValueNode
	| UnionTypeDefinitionNode
	| UnionTypeExtensionNode
	| VariableDefinitionNode
	| VariableNode
