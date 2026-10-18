module graphql

pub struct NameNode {
pub:
	kind  Kind = Kind.name
	value string
pub mut:
	loc ?Location
}

pub struct DocumentNode {
pub:
	kind        Kind = Kind.document
	definitions []DefinitionNode
	token_count int
pub mut:
	loc ?Location
}

pub type TypeSystemExtensionNode = SchemaExtensionNode | TypeExtensionNode

pub type DefinitionNode = DirectiveDefinitionNode
	| EnumTypeDefinitionNode
	| FragmentDefinitionNode
	| InputObjectTypeDefinitionNode
	| InterfaceTypeDefinitionNode
	| ObjectTypeDefinitionNode
	| OperationDefinitionNode
	| ScalarTypeDefinitionNode
	| SchemaDefinitionNode
	| SchemaExtensionNode
	| TypeExtensionNode
	| UnionTypeDefinitionNode
pub type ExecutableDefinitionNode = FragmentDefinitionNode | OperationDefinitionNode

pub struct OperationDefinitionNode {
pub:
	kind                 Kind = Kind.operation_definition
	operation            OperationTypeNode
	name                 ?NameNode
	variable_definitions ?[]VariableDefinitionNode
	directives           ?[]DirectiveNode
	selection_set        SelectionSetNode
pub mut:
	loc ?Location
}

pub enum OperationTypeNode {
	query
	mutation
	subscription
}

pub fn (o OperationTypeNode) gql_str() string {
	return o.str()
}

pub struct VariableDefinitionNode {
pub:
	kind          Kind = Kind.variable_definition
	variable      VariableNode
	@type         TypeNode
	default_value ?ValueNode
	directives    ?[]DirectiveNode
pub mut:
	loc ?Location
}

pub struct VariableNode {
pub:
	kind Kind = Kind.variable
	name NameNode
pub mut:
	loc ?Location
}

pub struct SelectionSetNode {
pub:
	kind       Kind = Kind.selection_set
	selections []SelectionNode
pub mut:
	loc ?Location
}

pub type SelectionNode = FieldNode | FragmentSpreadNode | InlineFragmentNode

pub struct FieldNode {
pub:
	kind                  Kind = Kind.field
	alias                 ?NameNode
	name                  NameNode
	arguments             ?[]ArgumentNode
	nullability_assertion ?NullabilityAssertionNode
	directives            []DirectiveNode
	selection_set         ?SelectionSetNode
pub mut:
	loc ?Location
}

pub type NullabilityAssertionNode = ErrorBoundaryNode
	| ListNullabilityOperatorNode
	| NonNullAssertionNode

pub struct ListNullabilityOperatorNode {
pub:
	kind                  Kind = Kind.list_nullability_operator
	nullability_assertion ?NullabilityAssertionNode
pub mut:
	loc ?Location
}

pub struct NonNullAssertionNode {
pub:
	kind                  Kind = Kind.non_null_assertion
	nullability_assertion ?ListNullabilityOperatorNode
pub mut:
	loc ?Location
}

pub struct ErrorBoundaryNode {
pub:
	kind                  Kind = Kind.error_boundary
	nullability_assertion ?ListNullabilityOperatorNode
pub mut:
	loc ?Location
}

// Values

// --
pub type ValueNode = BooleanValueNode
	| ConstObjectValueNode
	| EnumValueNode
	| FloatValueNode
	| IntValueNode
	| ListValueNode
	| NullValueNode
	| ObjectValueNode
	| StringValueNode
	| VariableNode

struct ArgumentNode {
pub:
	kind  Kind = Kind.argument
	name  NameNode
	value ValueNode
pub mut:
	loc ?Location
}

interface IArgumentNode {
	kind  Kind
	name  NameNode
	value ValueNode
mut:
	loc ?Location
}

pub struct ConstArgumentNode {
pub:
	kind  Kind = Kind.argument
	name  NameNode
	value ValueNode
pub mut:
	loc ?Location
}

// Fragments
pub struct FragmentSpreadNode {
pub:
	kind       Kind = Kind.fragment_spread
	name       NameNode
	directives ?[]DirectiveNode
pub mut:
	loc ?Location
}

pub struct InlineFragmentNode {
pub:
	kind           Kind = Kind.inline_fragment
	type_condition ?NamedTypeNode
	directives     ?[]DirectiveNode
	selection_set  SelectionSetNode
pub mut:
	loc ?Location
}

pub struct FragmentDefinitionNode {
pub:
	kind           Kind = Kind.fragment_definition
	name           NameNode
	type_condition NamedTypeNode
	directives     []DirectiveNode
	selection_set  SelectionSetNode
pub mut:
	loc ?Location
}

pub struct IntValueNode {
pub:
	kind     Kind = Kind.int_value
	value    ?string
	is_const bool
pub mut:
	loc ?Location
}

pub struct FloatValueNode {
pub:
	kind     Kind = Kind.float_value
	value    ?string
	is_const bool
pub mut:
	loc ?Location
}

pub struct StringValueNode {
pub:
	kind     Kind = Kind.string_value
	value    ?string
	block    ?bool
	is_const bool
pub mut:
	loc ?Location
}

pub struct BooleanValueNode {
pub:
	kind     Kind = Kind.boolean
	value    bool
	is_const bool
pub mut:
	loc ?Location
}

pub struct NullValueNode {
pub:
	kind     Kind = Kind.null
	is_const bool
pub mut:
	loc ?Location
}

pub struct EnumValueNode {
pub:
	kind     Kind = Kind.enum_value
	value    string
	is_const bool
pub mut:
	loc ?Location
}

pub struct ListValueNode {
pub:
	kind     Kind = Kind.list
	values   []ValueNode
	is_const bool
pub mut:
	loc ?Location
}

pub struct ObjectValueNode {
pub:
	kind     Kind = Kind.object
	fields   []ObjectFieldNode
	is_const bool
pub mut:
	loc ?Location
}

pub struct ConstObjectValueNode {
pub:
	kind     Kind = Kind.object
	fields   []ValueNode
	is_const bool = true
pub mut:
	loc ?Location
}

pub struct ObjectFieldNode {
pub:
	kind     Kind = Kind.object_field
	name     NameNode
	value    ValueNode
	is_const bool
pub mut:
	loc ?Location
}

pub struct ConstObjectFieldNode {
pub:
	kind     Kind = Kind.object_field
	name     NameNode
	value    ValueNode
	is_const bool = true
pub mut:
	loc ?Location
}

// Directives

pub type TypeDirectives = DirectiveNode

pub interface IDirectiveNode {
	kind      Kind
	name      NameNode
	arguments ?[]ArgumentNode
mut:
	loc ?Location
}

pub struct DirectiveNode {
pub:
	kind      Kind = Kind.directive
	name      NameNode
	arguments ?[]ArgumentNode
pub mut:
	loc      ?Location
	is_const bool
}

// Type reference

pub type TypeNode = ListTypeNode | NamedTypeNode | NonNullTypeNode
pub type NonNullType = ListTypeNode | NamedTypeNode

pub struct NamedTypeNode {
pub:
	kind      Kind = Kind.named_type
	node_type NameNode
pub mut:
	loc ?Location
}

pub struct ListTypeNode {
pub:
	kind      Kind = Kind.list_type
	node_type TypeNode
pub mut:
	loc ?Location
}

pub struct NonNullTypeNode {
pub:
	kind      Kind = Kind.non_null_type
	node_type NonNullType
pub mut:
	loc ?Location
}

// Type system definition

pub type TypeSystemDefinitionNode = DirectiveDefinitionNode
	| SchemaDefinitionNode
	| TypeDefinitionNode

pub struct SchemaDefinitionNode {
pub:
	kind            Kind = Kind.schema_definition
	description     ?StringValueNode
	directives      ?[]DirectiveNode
	operation_types []OperationTypeDefinitionNode
pub mut:
	loc ?Location
}

pub struct OperationTypeDefinitionNode {
pub:
	kind      Kind = Kind.operation_type_definition
	operation OperationTypeNode
	@type     NamedTypeNode
pub mut:
	loc ?Location
}

// Type definition

pub type TypeDefinitionNode = EnumTypeDefinitionNode
	| InputObjectTypeDefinitionNode
	| InterfaceTypeDefinitionNode
	| ObjectTypeDefinitionNode
	| ScalarTypeDefinitionNode
	| UnionTypeDefinitionNode

pub struct ScalarTypeDefinitionNode {
	kind        Kind
	description ?StringValueNode
	name        NameNode
	directives  ?[]DirectiveNode
pub mut:
	loc ?Location
}

pub struct ObjectTypeDefinitionNode {
	kind        Kind
	description ?StringValueNode
	name        NameNode
	interfaces  ?[]NamedTypeNode
	directives  ?[]DirectiveNode
	fields      ?[]FieldDefinitionNode
pub mut:
	loc ?Location
}

pub struct FieldDefinitionNode {
	kind        Kind = Kind.field_definition
	description ?StringValueNode
	name        NameNode
	arguments   ?[]InputValueDefinitionNode
	@type       TypeNode
	directives  ?[]DirectiveNode
pub mut:
	loc ?Location
}

pub struct InputValueDefinitionNode {
	kind          Kind
	description   ?StringValueNode
	name          NameNode
	@type         TypeNode
	default_value ?ValueNode
	directives    ?[]DirectiveNode
pub mut:
	loc ?Location
}

pub struct InterfaceTypeDefinitionNode {
	kind        Kind = Kind.interface_type_definition
	description ?StringValueNode
	name        NameNode
	interfaces  ?[]NamedTypeNode
	directives  ?[]DirectiveNode
	fields      ?[]FieldDefinitionNode
pub mut:
	loc ?Location
}

pub struct UnionTypeDefinitionNode {
	kind        Kind = Kind.union_type_definition
	description ?StringValueNode
	name        NameNode
	directives  ?[]DirectiveNode
	types       ?[]NamedTypeNode
pub mut:
	loc ?Location
}

pub struct EnumTypeDefinitionNode {
	kind        Kind = Kind.enum_type_definition
	description ?StringValueNode
	name        NameNode
	directives  ?[]DirectiveNode
	values      ?[]EnumValueDefinitionNode
pub mut:
	loc ?Location
}

pub struct EnumValueDefinitionNode {
	kind        Kind = Kind.enum_value_definition
	description ?StringValueNode
	name        NameNode
	directives  ?[]DirectiveNode
pub mut:
	loc ?Location
}

pub struct InputObjectTypeDefinitionNode {
	kind        Kind = Kind.input_object_type_definition
	description ?StringValueNode
	name        NameNode
	directives  ?[]DirectiveNode
	fields      ?[]InputValueDefinitionNode
pub mut:
	loc ?Location
}

pub struct DirectiveDefinitionNode {
	kind        Kind = Kind.directive_definition
	description ?StringValueNode
	name        NameNode
	arguments   ?[]InputValueDefinitionNode
	repeatable  bool
	locations   []NameNode
pub mut:
	loc ?Location
}

pub struct SchemaExtensionNode {
	kind            Kind = Kind.schema_extension
	directives      ?[]DirectiveNode
	operation_types ?[]OperationTypeDefinitionNode
pub mut:
	loc ?Location
}

// Type Extensions

pub type TypeExtensionNode = EnumTypeExtensionNode
	| InputObjectTypeExtensionNode
	| InterfaceTypeExtensionNode
	| ObjectTypeExtensionNode
	| ScalarTypeExtensionNode
	| UnionTypeExtensionNode

pub struct ScalarTypeExtensionNode {
	kind       Kind = Kind.scalar_type_extension
	name       NameNode
	directives ?[]DirectiveNode
pub mut:
	loc ?Location
}

pub struct ObjectTypeExtensionNode {
	kind       Kind = Kind.object_type_extension
	name       NameNode
	interfaces ?[]NamedTypeNode
	directives ?[]DirectiveNode
	fields     ?[]FieldDefinitionNode
pub mut:
	loc ?Location
}

pub struct InterfaceTypeExtensionNode {
	kind       Kind = Kind.interface_type_extension
	name       NameNode
	interfaces ?[]NamedTypeNode
	directives ?[]DirectiveNode
	fields     ?[]FieldDefinitionNode
pub mut:
	loc ?Location
}

pub struct UnionTypeExtensionNode {
	kind       Kind = Kind.union_type_extension
	name       NameNode
	directives ?[]DirectiveNode
	types      ?[]NamedTypeNode
pub mut:
	loc ?Location
}

pub struct EnumTypeExtensionNode {
	kind       Kind = Kind.enum_type_extension
	name       NameNode
	directives ?[]DirectiveNode
	values     ?[]EnumValueDefinitionNode
pub mut:
	loc ?Location
}

pub struct InputObjectTypeExtensionNode {
	kind       Kind = Kind.object_type_extension
	name       NameNode
	directives ?[]DirectiveNode
	fields     ?[]InputValueDefinitionNode
pub mut:
	loc ?Location
}
