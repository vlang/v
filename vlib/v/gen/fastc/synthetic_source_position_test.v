module fastc

import v.pref

fn test_fastc_synthetic_source_positions() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	for template in [
		'fn main() { callback := fn () int { BROKEN; return 1 }; _ := callback() }',
		'fn main() { callback := fn () int { nested := fn () int { BROKEN; return 1 }; return nested() }; _ := callback() }',
		'fn id[T](x T) T { return x }\nfn main() { _ := id(1); callback := fn () int { BROKEN; return 1 }; _ := callback() }',
		'fn wrap[T](x T) int { _ = x; callback := fn () int { BROKEN; return 1 }; return callback() }\nfn main() { _ := wrap(1) }',
		'fn main() { callback := fn () int { return 1 }; _ := callback(); BROKEN }',
		r'fn main() { $for field in Record.fields { _ := field.name; callback := fn () int { BROKEN; return 1 }; _ := callback() } }',
		r'fn main() { callback := fn () int { $for field in Record.fields { _ := field.name; BROKEN }; return 1 }; _ := callback() }',
		r'fn main() { $for field in Record.fields { _ := field.name; BROKEN } }',
		r'fn main() { value := Record{}; $for field in Record.fields { _ := value.$(field.name); BROKEN } }',
		r'fn main() { $for field in Record.fields { $if field.typ is int { BROKEN } } }',
		r'fn main() { $for item in Shade.values { _ := item.name; _ := item.value; BROKEN } }',
		r'fn main() { $for field in Record.fields { $for item in Shade.values { _ := field.name; _ := item.name; BROKEN } } }',
		r'fn main() { $for field in Record.fields { _ := field.name }; BROKEN }',
		r'fn main() { $for field in Record.fields { BROKEN } }',
		'fn id[T](x T) T { return x }\n' + r'fn main() { _ := id(1); $for field in Record.fields { _ := field.name; BROKEN } }',
	] {
		for translated in [false, true] {
			for body in ['if ((if true { true } else { false })) { _ := 1 }',
				'_ := if ((if true { true } else { false })) { 1 } else { 0 }', '_ := 2 ** 3',
				'if true { _ := 1 }'] {
				marker := if translated { '@[translated]\n' } else { '' }
				source := '${marker}module main\n// Padding stays in the original file.\nstruct Record {\n very_long_field_name int\n x int\n}\nenum Shade {\n a\n longer_name\n}\n' +
					template.replace('BROKEN', body) + '\n'
				mut message := ''
				_ := generate(source, 'synthetic_position.v', prefs) or {
					message = err.msg()
					''
				}
				is_condition := body.contains('if true { true }')
				if body == 'if true { _ := 1 }' || (translated && is_condition) {
					assert message == '', '${source}: ${message}'
					continue
				}
				needle := if is_condition { 'if true { true }' } else { '**' }
				offset := source.index(needle) or { panic('missing unsupported expression') }
				if is_condition {
					assert message.contains('assign it to a variable first'), '${source}: ${message}'
				} else {
					assert message.contains('expression token `**`'), '${source}: ${message}'
				}
				assert message.contains('at byte ${offset} in synthetic_position.v'), '${source}: ${message}'
			}
		}
	}
}

fn test_fastc_capturing_literal_error_source_positions() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	for params in ['()', '(argument int)'] {
		// The invalid condition fails before closure-runtime helpers are emitted.
		source := 'module main\nimport builtin.closure as _\nfn main() { first := 1; second := "padding"; callback := fn [first, second] ${params} int { if ((if true { true } else { false })) { return first }; return 0 }; _ = callback }\n'
		mut message := ''
		_, _, _ := generate_source_files([
			FastcSourceFile{
				path:   'capture_position.v'
				source: source
				header: fastc_scan_source_header(source, 'capture_position.v', prefs) or { panic(err) }
			},
		], map[string]string{}, prefs) or {
			message = err.msg()
			'', false, []string{}
		}
		offset := source.index('if true { true }') or { panic('missing nested if') }
		assert message.contains('assign it to a variable first'), message
		assert message.contains('at byte ${offset} in capture_position.v'), message
	}
}

fn test_fastc_synthetic_error_restores_outer_source_context() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	for body in [
		'callback := fn () { if ((if true { true } else { false })) {} }; _ = callback',
		r'$for field in Record.fields { _ := field.name; if ((if true { true } else { false })) {} }',
	] {
		// A failed generic placeholder is discarded before parsing the main function.
		source := 'module main\nstruct Record {\n value int\n}\nstruct Worker {}\nfn (worker Worker) probe[T](value T) { _ = value; ${body} }\nfn main() { worker := Worker{}; worker.probe(1); if ((if false { true } else { false })) {} }\n'
		mut message := ''
		_ := generate(source, 'restored_position.v', prefs) or {
			message = err.msg()
			''
		}
		offset := source.index('if false { true }') or { panic('missing outer condition') }
		assert message.contains('assign it to a variable first'), message
		assert message.contains('at byte ${offset} in restored_position.v'), message
	}
}

fn test_fastc_function_literal_in_source_fragment_uses_relative_scanner_bounds() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	source := 'module main\nfn main() { callback := fn () int { return 42 }; assert callback() == 42 }\n'
	fragment_offset := 137
	generated, _, _ := generate_source_files([
		FastcSourceFile{
			path:          'fragment_literal.v'
			source:        source
			source_offset: fragment_offset
			header:        fastc_scan_source_header(source, 'fragment_literal.v', prefs) or { panic(err) }
		},
	], map[string]string{}, prefs) or { panic(err) }
	literal_offset := fragment_offset + source.index('fn ()') or { panic('missing literal') }
	assert generated.contains('__vf_anon_main_fragment_literal_v_${literal_offset}')
	assert generated.contains('return 42;')
}
