module fastc

import v.pref

// Use `<-` for invalid expressions: selfhost mode supports the power operator.

fn test_fastc_asm_operand_source_positions() {
	for arch in ['amd64', 'arm64'] {
		for selfhost in [false, true] {
			mut prefs := pref.new_preferences()
			prefs.building_v = selfhost
			prefs.target = pref.target_from('macos', arch) or { panic(err) }
			for translated in [false, true] {
				for operands in ['; ; r ( EXPRESSION )', '; =r (\n EXPRESSION\n )',
					'; [out] =r (value) /* removed output comment */\n; [first] r (flag), r ( EXPRESSION ) as result'] {
					for expression in [
						'if ((if flag { true } else { false })) { 1 } else { 0 }',
						'if ((/* inside condition */ if flag { true } else { false })) { 1 } else { 0 }',
						'1 /* before unsupported token */ <- 2',
						'if flag { 1 } else { 0 }',
					] {
						marker := if translated { '@[translated]\n' } else { '' }
						comments := if operands.starts_with('; ;') {
							''
						} else {
							'// removed line comment ;\n/* outer ; /* nested */\ncomment */\n'
						}
						source := '${marker}module main\n// Padding before assembly.\nfn main() { flag := true; mut value := 0; asm ${arch} {\n${comments}${operands.replace('EXPRESSION', expression)}\n} }\n'
						fastc_test_asm_source_position(source, expression, translated, prefs)
					}
				}
			}
		}
	}
}

fn test_fastc_asm_operand_positions_through_source_rewrites() {
	for arch in ['amd64', 'arm64'] {
		mut prefs := pref.new_preferences()
		prefs.building_v = true
		prefs.target = pref.target_from('macos', arch) or { panic(err) }
		for template in [
			'fn id[T](x T) T { return x }\nfn main() { _ := id(1); ASM }',
			'fn wrap[T](x T) int { _ = x; ASM; return 0 }\nfn main() { _ := wrap(1) }',
			r'fn main() { $if true { $if true { ASM } } }',
			'fn main() { callback := fn () { ASM }; callback() }',
			r'fn main() { $for field in Record.fields { _ := field.name; ASM } }',
			r'fn main() { $for field in Record.fields { _ := field.name; callback := fn () { ASM }; callback() } }',
		] {
			for translated in [false, true] {
				for expression in [
					'if ((if flag { true } else { false })) { 1 } else { 0 }',
					'1 /* removed */ <- 2',
				] {
					marker := if translated { '@[translated]\n' } else { '' }
					assembly := 'flag := true; asm ${arch} { /* removed */\n; ; r ( ${expression} )\n}'
					source := '${marker}module main\nstruct Record {\n long_field_name int\n x int\n}\n' +
						template.replace('ASM', assembly) + '\n'
					fastc_test_asm_source_position(source, expression, translated, prefs)
				}
			}
		}
	}
}

fn test_fastc_asm_operand_restores_outer_source_context() {
	for arch in ['amd64', 'arm64'] {
		mut prefs := pref.new_preferences()
		prefs.building_v = true
		prefs.target = pref.target_from('macos', arch) or { panic(err) }
		for discarded_error in [false, true] {
			// Errors in a generic placeholder are discarded before parsing main.
			prefix := if discarded_error {
				'struct Worker {}\nfn (worker Worker) probe[T](value T) { _ = value; flag := true; asm ${arch} { ; ; r (if ((if flag { true } else { false })) { 1 } else { 0 }) } }\n'
			} else {
				''
			}
			body := if discarded_error {
				'worker := Worker{}; worker.probe(1)'
			} else {
				'flag := true; asm ${arch} { /* removed */ ; ; r (if flag { 1 } else { 0 }) }'
			}
			expression := 'if ((if false { true } else { false })) { 1 } else { 0 }'
			source := 'module main\n${prefix}fn main() { ${body}; _ := ${expression} }\n'
			mut message := ''
			_ := generate(source, 'asm_restored_position.v', prefs) or {
				message = err.msg()
				''
			}
			offset := source.index('if false { true }') or { panic('missing outer condition') }
			assert message.contains('assign it to a variable first'), message
			assert message.contains('at byte ${offset} in asm_restored_position.v'), message
		}
	}
}

fn fastc_test_asm_source_position(source string, expression string, translated bool, prefs &pref.Preferences) {
	mut message := ''
	_ := generate(source, 'asm_position.v', prefs) or {
		message = err.msg()
		''
	}
	is_condition := expression.contains('if flag { true }')
	if (translated && is_condition) || expression == 'if flag { 1 } else { 0 }' {
		assert message == '', '${source}: ${message}'
		return
	}
	needle := if is_condition { 'if flag { true }' } else { '<-' }
	offset := source.index(needle) or { panic('missing unsupported expression') }
	if is_condition {
		assert message.contains('assign it to a variable first'), '${source}: ${message}'
	} else {
		assert message.contains('expression token `<-`'), '${source}: ${message}'
	}
	assert message.contains('at byte ${offset} in asm_position.v'), '${source}: ${message}'
}
