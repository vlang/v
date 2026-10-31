module graphql

// The set of allowed kind values for AST nodes.
const _query_document_keys = {
	Kind.name:                         []string{}
	Kind.document:                     ['definitions']
	Kind.operation_definition:         ['name', 'variableDefinitions', 'directives', 'selectionSet']
	Kind.variable:                     ['name']
	Kind.variable_definition:          ['variable', 'type', 'defaultValue', 'directives']
	Kind.selection_set:                ['selections']
	Kind.field:                        ['alias', 'name', 'arguments', 'directives', 'selectionSet',
		'nullabilityAssertion']
	Kind.argument:                     ['name', 'value']
	Kind.list_nullability_operator:    ['nullabilityAssertion']
	Kind.non_null_assertion:           ['nullabilityAssertion']
	Kind.error_boundary:               ['nullabilityAssertion']
	Kind.fragment_spread:              ['name', 'directives']
	Kind.inline_fragment:              ['typeCondition', 'directives', 'selectionSet']
	Kind.fragment_definition:          ['name', 'variableDefinitions', 'typeCondition', 'directives',
		'selectionSet']
	Kind.int_value:                    []
	Kind.float_value:                  []
	Kind.string_value:                 []
	Kind.boolean:                      []
	Kind.null:                         []
	Kind.enum_value:                   []
	Kind.list:                         ['values']
	Kind.object:                       ['fields']
	Kind.object_field:                 ['name', 'value']
	Kind.directive:                    ['name', 'arguments']
	Kind.named_type:                   ['name']
	//
	Kind.list_type:                    ['type']
	Kind.non_null_type:                ['type']
	//
	Kind.schema_definition:            ['description', 'directives', 'operationTypes']
	Kind.operation_type_definition:    ['type']
	//
	Kind.scalar_type_definition:       ['description', 'name', 'directives']
	Kind.object_type_definition:       ['description', 'name', 'interfaces', 'directives', 'fields']
	Kind.field_definition:             ['description', 'name', 'arguments', 'type', 'directives']
	Kind.input_value_definition:       ['description', 'name', 'type', 'defaultValue', 'directives']
	Kind.interface_type_definition:    ['description', 'name', 'interfaces', 'directives', 'fields']
	Kind.union_type_definition:        ['description', 'name', 'directives', 'types']
	Kind.enum_type_definition:         ['description', 'name', 'directives', 'values']
	Kind.enum_value_definition:        ['description', 'name', 'directives']
	Kind.input_object_type_definition: ['description', 'name', 'directives', 'fields']
	Kind.directive_definition:         ['description', 'name', 'arguments', 'locations']
	//
	Kind.schema_extension:             ['directives', 'operationTypes']
	Kind.scalar_type_extension:        ['name', 'directives']
	Kind.object_type_extension:        ['name', 'interfaces', 'directives', 'fields']
	Kind.interface_type_extension:     ['name', 'interfaces', 'directives', 'fields']
	Kind.union_type_extension:         ['name', 'directives', 'types']
	Kind.enum_type_extension:          ['name', 'directives', 'values']
	Kind.input_object_type_extension:  ['name', 'directives', 'fields']
}
