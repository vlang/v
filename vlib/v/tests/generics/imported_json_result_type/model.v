module imported_json_result_type

import json2

// Token deliberately shares its name with json2.Token and main.Token.
pub struct Token {
pub:
	id int
}

pub type TokenAlias = Token

// token preserves the decoded type when an or block propagates the error.
pub fn token(input string) !Token {
	value := json2.decode[Token](input) or { return err }
	copied := value
	return copied
}

// propagated_token unwraps the same generic result with !.
pub fn propagated_token(input string) !Token {
	value := json2.decode[Token](input)!
	return value
}

// fallback_token uses a value from this module when decoding fails.
pub fn fallback_token(input string) Token {
	value := json2.decode[Token](input) or { Token{ id: 29 } }
	return value
}

fn result[T](value T) !T {
	return value
}

// aliased_token preserves an alias through a generic result temporary.
pub fn aliased_token() !TokenAlias {
	value := result[TokenAlias](TokenAlias(Token{ id: 31 })) or { return err }
	return value
}

// token_pointer preserves the pointer level of a generic result payload.
pub fn token_pointer() !&Token {
	value := result[&Token](&Token{ id: 37 })!
	return value
}

fn load[T]() !T {
	return json2.decode[T]('[{"id":1}]') or { return err }
}

// entries appends to an array selected from a generic result or an empty literal.
pub fn entries(present bool) ![]Token {
	mut values := if present {
		load[[]Token]()!
	} else {
		[]Token{}
	}
	values << Token{ id: 2 }
	return values
}
