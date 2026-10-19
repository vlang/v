module graphql

const _directive_location_map = {
	DirectiveLocation.query:                  'QUERY'
	DirectiveLocation.mutation:               'MUTATION'
	DirectiveLocation.subscription:           'SUBSCRIPTION'
	DirectiveLocation.field:                  'FIELD'
	DirectiveLocation.fragment_definition:    'FRAGMENT_DEFINITION'
	DirectiveLocation.fragment_spread:        'FRAGMENT_SPREAD'
	DirectiveLocation.inline_fragment:        'INLINE_FRAGMENT'
	DirectiveLocation.variable_definition:    'VARIABLE_DEFINITION'
	DirectiveLocation.schema:                 'SCHEMA'
	DirectiveLocation.scalar:                 'SCALAR'
	DirectiveLocation.object:                 'OBJECT'
	DirectiveLocation.field_definition:       'FIELD_DEFINITION'
	DirectiveLocation.argument_definition:    'ARGUMENT_DEFINITION'
	DirectiveLocation.@interface:             'INTERFACE'
	DirectiveLocation.@union:                 'UNION'
	DirectiveLocation.@enum:                  'ENUM'
	DirectiveLocation.enum_value:             'ENUM_VALUE'
	DirectiveLocation.input_object:           'INPUT_OBJECT'
	DirectiveLocation.input_field_definition: 'INPUT_FIELD_DEFINITION'
}

pub enum DirectiveLocation {
	// Request Definitions
	query
	mutation
	subscription
	field
	fragment_definition
	fragment_spread
	inline_fragment
	variable_definition
	// Type System Definitions
	schema
	scalar
	object
	field_definition
	argument_definition
	@interface
	@union
	@enum
	enum_value
	input_object
	input_field_definition
}

fn (d DirectiveLocation) gql_str() string {
	return graphql._directive_location_map[d]
}
