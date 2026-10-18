module graphql

struct LocationOffset {
pub:
	line   int
	column int
}

// A representation of source input to GraphQL. The `name` and `locationOffset` parameters are
// optional, but they are useful for clients who store GraphQL documents in source files.
// For example, if the GraphQL input starts at line 40 in a file named `Foo.graphql`, it might
// be useful for `name` to be `"Foo.graphql"` and location to be `{ line: 40, column: 1 }`.
// The `line` and `column` properties in `locationOffset` are 1-indexed.
pub struct Source {
pub:
	body            string
	name            string
	location_offset LocationOffset
}

fn Source.new(body string, name ?string, location_offset ?LocationOffset) Source {
	return Source{
		body: &body
		name: name or { 'GraphQL request' }
		location_offset: location_offset or {
			LocationOffset{
				line: 1
				column: 1
			}
		}
	}
}

fn (s Source) get() string {
	return 'Source'
}
