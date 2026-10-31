module graphql

fn test_trailing_carriage_return() ! {
	for ending in ['\r', '\r\n', '\n\r'] {
		document := parse('{ a }' + ending, none)!
		assert document.definitions.len == 1
	}
}

fn test_truncated_tokens_return_errors() {
	for input in ['.', '..', '"', '"""', '-', '1.', '1e', '1e+', '1e-', '-0.'] {
		mut lexer := Lexer.new(Source.new(input, none, none))
		if token := lexer.advance() {
			assert false, '${input}: unexpected ${token.kind}'
		} else {
			assert err.msg().len > 0
		}
		if _ := parse(input, none) {
			assert false, '${input}: expected a parse error'
		}
	}
}

fn test_numbers_at_end_of_input() ! {
	for input in ['0', '-0', '123', '-123', '1.25', '1e3', '-0.5e-2', '2E+3'] {
		mut lexer := Lexer.new(Source.new(input, none, none))
		token := lexer.advance()!
		assert token.kind in [.integer, .float]
		assert token.value? == input
		end := lexer.advance()!
		assert end.kind == .eof
	}
}
