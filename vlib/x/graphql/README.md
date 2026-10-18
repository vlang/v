# GraphQL parsing

`graphql.parse(source, none)` parses a GraphQL document and returns a Result.
Malformed or truncated tokens return errors. Trailing whitespace, including a carriage return,
is accepted after a complete document.
