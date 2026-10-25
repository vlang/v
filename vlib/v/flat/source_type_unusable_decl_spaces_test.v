module flat

// Keep the old allocating predicate as an independent oracle for byte-level
// spelling behavior. Production code should neither replace nor intern text.
fn unusable_decl_space_oracle(text string) bool {
	if text in ['unknown', 'array', 'map', 'struct'] || text.contains('unknown')
		|| (text.len >= 8 && text.contains('typeof(')) {
		return true
	}
	clean := text.replace(' ', '')
	return clean in ['Option', '__v_option', 'Result'] || clean.starts_with('Option_')
		|| clean.starts_with('__v_option_') || clean.starts_with('Result_')
}

fn assert_unusable_decl_space_oracle(text string) {
	mut shape := unsafe { SourceTypeShape(0) } // The empty set has no syntax flags.
	if text.len >= 8 && text.contains('typeof(') {
		shape.set(.typeof_expr)
	}
	assert source_type_spelling_unusable_decl(text, shape) == unusable_decl_space_oracle(text), text
}

fn test_unusable_decl_matches_allocating_oracle_at_all_space_boundaries() {
	for token in ['Option', '__v_option', 'Result'] {
		for mask in 0 .. (1 << (token.len + 1)) {
			mut text := ''
			for i, c in token {
				if mask & (1 << i) != 0 {
					text += ' '
				}
				text += c.ascii_str()
			}
			if mask & (1 << token.len) != 0 {
				text += ' '
			}
			for suffix in ['', ' ', '  ', '_', '_int', ' _ int', 'Thing', ' [int]', '\t', '\t_int',
				'\n', '\n_int'] {
				assert_unusable_decl_space_oracle(text + suffix)
			}
		}
	}
}

fn test_unusable_decl_keeps_non_space_bytes_and_raw_checks_exact() {
	for token in ['Option', '__v_option', 'Result'] {
		for i in 0 .. token.len + 1 {
			for separator in ['\t', '\n', '\r', '\v', '\f'] {
				assert_unusable_decl_space_oracle(token[..i] + separator + token[i..])
				assert_unusable_decl_space_oracle(token[..i] + separator + token[i..] + '_int')
			}
		}
	}
	for text in ['', ' ', '  ', 'unknown', '?unknown', 'XunknownY', 'unk nown', 'Unknown', 'array',
		'array ', ' map', 'ma p', 'struct', 'struct\t', 'typeof(', 'typeof( ', 'typeof(value)',
		'typeof (value)', 'TYPEOF(value)', 'Option_unknown', 'O ptionunk nown', 'option', '__V_option',
		'result', '?Option', '&Result', 'O', '__v_optio', 'Resul', 'OptionThing', 'Option[T]',
		'__v_optionThing', 'Resultant', 'O  p t i o n  _ int'] {
		assert_unusable_decl_space_oracle(text)
	}
}
