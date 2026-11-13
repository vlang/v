module parser

import v.pref

fn fn_literal_binding_diagnostics(source string) []Diagnostic {
	mut p := Parser.new(pref.new_preferences())
	p.parse_text('fn_literal_binding_scope.v', source)
	return p.diagnostics
}

fn test_anonymous_function_loop_bindings_ignore_uncaptured_outer_names() {
	diagnostics := fn_literal_binding_diagnostics('fn outer(item int) {
	local := 1
	callback := fn () {
		mut values := [1, 2]
		for mut item in values {
			item++
			nested := fn () {
				for item in [3, 4] {
					_ = item
				}
			}
			nested()
		}
		for local in values {
			_ = local
		}
	}
	callback()
}')
	assert diagnostics.len == 0, diagnostics.str()
}

fn test_anonymous_function_loop_bindings_still_reject_parameters_and_captures() {
	for signature in ['fn (item int)', 'fn [item] ()'] {
		diagnostics := fn_literal_binding_diagnostics('fn outer(item int) {
		callback := ${signature} {
			for item in [1, 2] {}
		}
	}')
		assert diagnostics.len == 1, diagnostics.str()
		assert diagnostics[0].message.contains('redefinition of value iteration variable `item`')
	}
}

fn test_anonymous_function_restores_enclosing_bindings() {
	diagnostics := fn_literal_binding_diagnostics('fn outer(item int) {
	callback := fn () {
		for item in [1, 2] {}
	}
	for item in [3, 4] {}
}')
	assert diagnostics.len == 1, diagnostics.str()
	assert diagnostics[0].message.contains('redefinition of value iteration variable `item`')
	assert diagnostics[0].line == 5
}

fn test_nested_anonymous_function_cannot_capture_an_uncaptured_outer_binding() {
	diagnostics := fn_literal_binding_diagnostics('fn outer(item int) {
	callback := fn () {
		nested := fn [item] () {}
	}
}')
	assert diagnostics.len == 1, diagnostics.str()
	assert diagnostics[0].message == 'undefined ident: `item`'
}
