module fastc

import v.pref

fn test_fastc_rejects_if_condition_starting_with_if() {
	for selfhost in [false, true] {
		mut prefs := pref.new_preferences()
		prefs.building_v = selfhost
		for condition in ['if a { b } else { c }', '(if a { b } else { c })',
			'((if a { b } else { c }))', '(if a { 1 } else { 2 }) < 2',
			'(\n\tif a {\n\t\tb\n\t} else {\n\t\tc\n\t})'] {
			for body in ['if ${condition} { return 1 }; return 0',
				'if c { return 2 } else if ${condition} { return 1 }; return 0',
				'return if ${condition} { 1 } else { 0 }',
				'return if c { 2 } else if ${condition} { 1 } else { 0 }'] {
				mut message := ''
				_ := generate('module main\nfn choose(a bool, b bool, c bool) int { ${body} }\n',
					'if_condition.v', prefs) or {
					message = err.msg()
					''
				}
				assert message.contains('assign it to a variable first'), '${selfhost}: ${body}: ${message}'
			}
		}
	}
}

fn test_fastc_if_condition_error_points_at_inner_if() {
	for condition in ['if a { b } else { c }', '((if a { b } else { c }))'] {
		source := 'module main\nfn choose(a bool, b bool, c bool) int { if ${condition} { return 1 }; return 0 }\n'
		mut message := ''
		_ := generate(source, 'if_condition_position.v', pref.new_preferences()) or {
			message = err.msg()
			''
		}
		offset := source.index('if a') or { panic('missing nested if') }
		assert message.contains('at byte ${offset} in if_condition_position.v'), message
	}
}

fn test_fastc_accepts_other_if_conditions() {
	for selfhost in [false, true] {
		mut prefs := pref.new_preferences()
		prefs.building_v = selfhost
		for condition in ['a', '(a)', '((a))', '(a && b) || c'] {
			_ := generate('module main\nfn choose(a bool, b bool, c bool) int { if ${condition} { return 1 }; return 0 }\n',
				'if_condition.v', prefs) or { panic(err) }
		}
	}
}

fn test_fastc_preserves_translated_if_conditions_starting_with_if() {
	for selfhost in [false, true] {
		mut prefs := pref.new_preferences()
		prefs.building_v = selfhost
		for condition in ['if a { b } else { c }', '(if a { b } else { c })',
			'((if a { b } else { c }))'] {
			for body in ['if ${condition} { return 1 }; return 0',
				'return if ${condition} { 1 } else { 0 }'] {
				_ := generate('@[translated]\nmodule main\nfn choose(a bool, b bool, c bool) int { ${body} }\n',
					'translated_if_condition.v', prefs) or { panic(err) }
			}
		}
	}
}

fn test_fastc_translated_declaration_if_conditions() {
	for selfhost in [false, true] {
		mut prefs := pref.new_preferences()
		prefs.building_v = selfhost
		prefs.enable_globals = true
		for declaration in ['const value = EXPRESSION', '__global value = EXPRESSION',
			'struct Settings {\n value int = EXPRESSION\n}'] {
			for condition in ['if true { true } else { false }', '((if true { true } else { false }))'] {
				for selected in [false, true] {
					if selected && !selfhost {
						continue
					}
					body := declaration.replace('EXPRESSION', 'if ${condition} { 1 } else { 0 }')
					source := '@[translated]\nmodule main\nfn before() {}\n' +
						if selected { '\$if true {\n${body}\n}\n' } else { '${body}\n' }
					_ := generate(source, 'translated_declaration_if.v', prefs) or { panic('${source}: ${err}') }
				}
			}
		}
	}
}

fn test_fastc_declaration_if_condition_source_positions() {
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	for declaration in ['const value = EXPRESSION', 'const (\n first = 0\n value = EXPRESSION\n)',
		'__global value = EXPRESSION', '__global (\n first = 0\n value = EXPRESSION\n)',
		'__global (first = 0 value = EXPRESSION)', 'struct Settings {\n value int = EXPRESSION\n}'] {
		for condition in ['if true { true } else { false }', '((if true { true } else { false }))'] {
			for selected in [false, true] {
				if selected && declaration.starts_with('__global (first') {
					continue
				}
				body := declaration.replace('EXPRESSION', 'if ${condition} { 1 } else { 0 }')
				source := 'module main\n// Bytes removed by declaration filtering.\nfn before() { println("padding") }\n' +
					if selected { '\$if true {\n\$if true {\n${body}\n}\n}\n' } else { '${body}\n' }
				mut message := ''
				_ := generate(source, 'declaration_if_position.v', prefs) or {
					message = err.msg()
					''
				}
				offset := source.index('if true { true }') or { panic('missing nested if') }
				assert message.contains('assign it to a variable first'), '${source}: ${message}'
				assert message.contains('at byte ${offset} in declaration_if_position.v'), '${source}: ${message}'
			}
		}
	}
}

fn test_fastc_split_constant_if_condition_source_position() {
	source := 'module main\nfn before() { println("padding") }\n' +
		'const padding = "${'x'.repeat(fastc_constant_split_size)}"\n' +
		'fn between() { println("more padding") }\n' +
		'const value = if ((if true { true } else { false })) { 1 } else { 0 }\n' +
		'const after = 0\n'
	mut message := ''
	_ := generate(source, 'split_constant_if.v', pref.new_preferences()) or {
		message = err.msg()
		''
	}
	offset := source.index('if true { true }') or { panic('missing nested if') }
	assert message.contains('assign it to a variable first'), message
	assert message.contains('at byte ${offset} in split_constant_if.v'), message
}
