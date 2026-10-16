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
