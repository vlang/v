module graphql

import os

fn test_query_parse() ! {
	body := '{
        node(id: 4) {
          id,
          name
        }
	}'

	result := parse(body, none)!

	assert result.kind == Kind.document
	assert result.loc?.source.body == body
}

fn test_github_parse() ! {
	path := os.join_path(@DIR, 'testdata', 'github.graphql')

	body := os.read_file(path)!

	result := parse(body, none)!

	assert result.kind == Kind.document
	assert result.token_count == 19_882
}

fn test_simple_parse() ! {
	path := os.join_path(@DIR, 'testdata', 'simple.graphql')

	body := os.read_file(path)!

	result := parse(body, none)!

	assert result.kind == Kind.document
	assert result.token_count == 175
}

fn test_pokeapi_parse() ! {
	path := os.join_path(@DIR, 'testdata', 'pokeapi.graphql')

	body := os.read_file(path)!

	result := parse(body, none)!

	assert result.kind == Kind.document
	assert result.token_count == 27
}

fn test_variable_definition_parse() ! {
	body := 'query getUser(\$id: ID! = "default") {
		user(id: \$id) {
			name
		}
	}'

	result := parse(body, none)!

	assert result.kind == Kind.document
	assert result.token_count == 23
}