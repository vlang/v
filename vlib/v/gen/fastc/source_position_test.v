module fastc

import v.pref

fn test_fastc_unsupported_expression_source_positions() {
	for expression, error_token in {
		'1 << 2':      '<<'
		'4 / 2':       '/'
		'sizeof(int)': 'sizeof'
		'2 ** 3':      '**'
		'1 + 2 | 4':   '|'
	} {
		for declaration in ['struct Settings {\n value int = EXPRESSION\n}',
			'fn bad() int { return EXPRESSION }'] {
			source := 'module main\n// Removed from the filtered declaration buffer.\nfn before() { println("padding") }\n' +
				declaration.replace('EXPRESSION', expression) + '\n'
			mut message := ''
			_ := generate(source, 'unsupported_position.v', pref.new_preferences()) or {
				message = err.msg()
				''
			}
			expression_offset := source.index(expression) or { panic('missing expression') }
			token_offset := expression.index(error_token) or { panic('missing unsupported token') }
			offset := expression_offset + token_offset
			assert message.starts_with('fastc parser does not support '), '${source}: ${message}'
			assert message.contains('at byte ${offset} in unsupported_position.v'), '${source}: ${message}'
		}
	}
}

fn test_fastc_field_default_scanner_error_source_position() {
	source := 'module main\nfn before() { println("padding") }\nstruct Settings {\n value int = 1_\n}\n'
	mut message := ''
	_ := generate(source, 'field_scanner_position.v', pref.new_preferences()) or {
		message = err.msg()
		''
	}
	offset := source.index('1_') or { panic('missing invalid literal') }
	assert message.starts_with('fastc scanner error'), message
	assert message.contains('at byte ${offset + 1} in field_scanner_position.v'), message
}
