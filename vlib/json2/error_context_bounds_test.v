module json2

fn test_tab_expansion_cannot_make_syntax_error_context_start_negative() {
	for prefix in ['', '\n', 'previous line\n', 'long previous line '.repeat(4) + '\n'] {
		for tabs in [7, 8, 16, 64] {
			input := prefix + '"' + '\t'.repeat(tabs)
			mut decoder := Decoder{ json: input, checker_idx: prefix.len }
			mut rejected := false
			decoder.check_string() or {
				rejected = true
				if err is JsonDecodeError {
					assert err.message == 'Syntax: EOF: string not closed'
					assert err.line == if prefix == '' { 1 } else { 2 }
					assert !err.context.contains('previous line')
				}
			}
			assert rejected
		}
	}
}

fn test_decode_returns_errors_for_tab_heavy_malformed_and_mismatched_input() {
	for prefix in ['', '\n'] {
		mut syntax_rejected := false
		decode[Any](prefix + '"' + '\t'.repeat(8)) or { syntax_rejected = true }
		assert syntax_rejected
		mut type_rejected := false
		decode[int](prefix + '\t'.repeat(8) + '{}') or { type_rejected = true }
		assert type_rejected
	}
}

fn test_tab_expansion_cannot_make_decode_error_context_start_negative() {
	for prefix in ['', '\n', 'previous line\n', 'long previous line '.repeat(4) + '\n'] {
		// Leading non-whitespace would be a syntax error; position the decoder directly
		// after it to exercise the decoding diagnostic independently.
		for tabs in [7, 8, 16, 64] {
			input := prefix + '\t'.repeat(tabs) + '{}'
			mut decoder := Decoder{
				json:        input
				values_info: [ValueInfo{ position: input.len - 2, length: 2, value_kind: .object }]
			}
			mut rejected := false
			decoder.decode_error('expected number') or {
				rejected = true
				if err is JsonDecodeError {
					assert err.line == if prefix == '' { 1 } else { 2 }
					assert !err.context.contains('previous line')
				}
			}
			assert rejected
		}
	}
}
