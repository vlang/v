# GraphQL parser

`x.graphql.parse` reads a GraphQL document and returns its abstract syntax tree.
Fields retain their nested selection sets. List values, named fragment spreads,
inline fragments, and fragment definitions are represented by their corresponding
AST node types. String values contain decoded escape sequences.

```v
import x.graphql

fn main() {
	document := graphql.parse('{ user { id name } }', none) or {
		eprintln('Invalid GraphQL document: ${err}')
		return
	}
	println(document.definitions.len)
}
```

Malformed documents and lexical errors are returned through the Result API.
Applications can also use `ParserOptions.max_tokens` to limit the number of tokens
in a document. Exceeding that limit returns an error. Whitespace and comments do
not count toward the limit.
