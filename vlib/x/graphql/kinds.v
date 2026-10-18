module graphql

// The set of allowed kind values for AST nodes.
const _kind_map = {
	Kind.name:                         'Name'
	Kind.document:                     'Document'
	Kind.operation_definition:         'OperationDefinition'
	Kind.variable_definition:          'VariableDefinition'
	Kind.selection_set:                'SelectionSet'
	Kind.field:                        'Field'
	Kind.argument:                     'Argument'
	Kind.list_nullability_operator:    'ListNullabilityOperator'
	Kind.non_null_assertion:           'NonNullAssertion'
	Kind.error_boundary:               'ErrorBoundary'
	Kind.fragment_spread:              'FragmentSpread'
	Kind.inline_fragment:              'InlineFragment'
	Kind.fragment_definition:          'FragmentDefinition'
	Kind.variable:                     'Variable'
	Kind.int_value:                    'IntValue'
	Kind.float_value:                  'FloatValue'
	Kind.string_value:                 'StringValue'
	Kind.boolean:                      'BooleanValue'
	Kind.null:                         'NullValue'
	Kind.enum_value:                   'EnumValue'
	Kind.list:                         'ListValue'
	Kind.object:                       'ObjectValue'
	Kind.object_field:                 'ObjectField'
	Kind.directive:                    'Directive'
	Kind.named_type:                   'NamedType'
	Kind.list_type:                    'ListType'
	Kind.non_null_type:                'NonNullType'
	Kind.schema_definition:            'SchemaDefinition'
	Kind.operation_type_definition:    'OperationTypeDefinition'
	Kind.scalar_type_definition:       'ScalarTypeDefinition'
	Kind.object_type_definition:       'ObjectTypeDefinition'
	Kind.field_definition:             'FieldDefinition'
	Kind.input_value_definition:       'InputValueDefinition'
	Kind.interface_type_definition:    'InterfaceTypeDefinition'
	Kind.union_type_definition:        'UnionTypeDefinition'
	Kind.enum_type_definition:         'EnumTypeDefinition'
	Kind.enum_value_definition:        'EnumValueDefinition'
	Kind.input_object_type_definition: 'InputObjectTypeDefinition'
	Kind.directive_definition:         'DirectiveDefinition'
	Kind.schema_extension:             'SchemaExtension'
	Kind.scalar_type_extension:        'ScalarTypeExtension'
	Kind.object_type_extension:        'ObjectTypeExtension'
	Kind.interface_type_extension:     'InterfaceTypeExtension'
	Kind.union_type_extension:         'UnionTypeExtension'
	Kind.enum_type_extension:          'EnumTypeExtension'
	Kind.input_object_type_extension:  'InputObjectTypeExtension'
}

pub enum Kind {
	name
	document
	// Documen
	operation_definition
	variable_definition
	selection_set
	field
	argument
	// Nullability Modifier
	list_nullability_operator
	non_null_assertion
	error_boundary
	// Fragments *
	fragment_spread
	inline_fragment
	fragment_definition
	// Value
	variable
	int_value
	float_value
	string_value
	boolean
	null
	enum_value
	list
	object
	object_field
	// Directive
	directive
	// Type
	named_type
	list_type
	non_null_type
	// Type System Definition
	schema_definition
	operation_type_definition
	// Type Definition
	scalar_type_definition
	object_type_definition
	field_definition
	input_value_definition
	interface_type_definition
	union_type_definition
	enum_type_definition
	enum_value_definition
	input_object_type_definition
	// Directive Definition
	directive_definition
	// Type System Extension
	schema_extension
	// Type Extension
	scalar_type_extension
	object_type_extension
	interface_type_extension
	union_type_extension
	enum_type_extension
	input_object_type_extension
}

// Returns the string value of the Kind enum
pub fn (k Kind) gql_str() string {
	return graphql._kind_map[k]
}
