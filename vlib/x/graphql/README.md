# GraphQL

The `x.graphql` module provides parser and AST definitions for GraphQL queries,
mutations, subscriptions, and Schema Definition Language (SDL) documents.

Nested nullability assertions use `ListNullabilityOperatorNode.nullability_assertion`,
an optional pointer to `NullabilityAssertionNode`, to keep recursive AST nodes finite in size.
`ListTypeNode.node_type` likewise points to its nested `TypeNode`.

## Usage

### Parsing a query

```v
import x.graphql

doc := graphql.parse('{ hero { name } }', none)!
for def in doc.definitions {
	if def is graphql.OperationDefinitionNode {
		for sel in def.selection_set.selections {
			if sel is graphql.FieldNode {
				println('Field: ${sel.name.value}')
			}
		}
	}
}
```

### Parsing schema definition (SDL)

```v
import x.graphql

sdl := '
type User {
	id: ID!
	name: String!
}
'

doc := graphql.parse(sdl, none)!
for def in doc.definitions {
	if def is graphql.ObjectTypeDefinitionNode {
		println('Type: ${def.name.value}')
	}
}
```

### Parser options

You can control token limits and source location tracking via `ParserOptions`:

```v
import x.graphql

options := graphql.ParserOptions{
	no_location: true
	max_tokens:  10_000
}
doc := graphql.parse('{ ping }', options)!
```
