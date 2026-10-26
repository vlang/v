module fastc

import v.pref

// Use `<-` for invalid expressions: selfhost mode supports the power operator.

fn test_fastc_on_demand_generics_use_defining_source_context() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	for method in [false, true] {
		for caller_translated in [false, true] {
			for definition_translated in [false, true] {
				for body in ['return 1', 'return 2 <- 3',
					'return if ((if true { true } else { false })) { 1 } else { 0 }'] {
					marker := if definition_translated { '@[translated]\n' } else { '' }
					signature := if method {
						'pub fn (worker Worker) choose[T](value T) int'
					} else {
						'pub fn choose[T](value T) int'
					}
					definition := '${marker}module util\nfn before() {}\npub struct Worker {}\npub struct Box[T] {\n value T\n}\n${signature} {\n box := Box[T]{}; _ = box; _ = value\n ${body}\n}\n'
					caller_marker := if caller_translated { '@[translated]\n' } else { '' }
					call := if method {
						'worker := util.Worker{}; _ := worker.choose[int](1)'
					} else {
						'_ := util.choose[int](1)'
					}
					caller := '${caller_marker}module main\nimport util\nfn id[T](x T) T { return x }\nfn main() { _ := id(1); ${call} }\n'
					mut message := ''
					generated, _, _ := generate_source_files([
						FastcSourceFile{
							path:   'util/definition.v'
							source: definition
							header: fastc_scan_source_header(definition, 'util/definition.v', prefs) or { panic(err) }
						},
						FastcSourceFile{
							path:   'caller.v'
							source: caller
							header: fastc_scan_source_header(caller, 'caller.v', prefs) or { panic(err) }
						},
					], map[string]string{}, prefs) or {
						message = err.msg()
						'', false, []string{}
					}
					invalid_condition := body.contains('if true')
					context := '${method}, ${caller_translated}, ${definition_translated}, ${body}'
					if body == 'return 1' || (definition_translated && invalid_condition) {
						assert message == '', '${context}: ${message}'
						assert generated.contains('choose_mono_int'), context
						continue
					}
					needle := if invalid_condition { 'if true { true }' } else { '<-' }
					offset := definition.index(needle) or { panic('missing invalid expression') }
					assert message.contains('generic instance'), '${context}: ${message}'
					if invalid_condition {
						assert message.contains('assign it to a variable first'), '${context}: ${message}'
					} else {
						assert message.contains('expression token `<-`'), '${context}: ${message}'
					}
					assert message.contains('at byte ${offset} in util/definition.v'), '${context}: ${message}'
				}
			}
		}
	}
}

fn test_fastc_on_demand_method_preserves_rewritten_source_origins() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	for body in ['if ((if true { true } else { false })) { return 1 }; return 0', 'return 2 <- 3'] {
		source := 'module main\nfn id[T](x T) T { return x }\nstruct Worker {}\nfn (worker Worker) choose[T](value T) int { _ = value; ${body} }\nfn main() { _ := id(1); worker := Worker{}; _ := worker.choose[int](1) }\n'
		mut message := ''
		_ := generate(source, 'rewritten_method.v', prefs) or {
			message = err.msg()
			''
		}
		needle := if body.starts_with('if') { 'if true { true }' } else { '<-' }
		offset := source.index(needle) or { panic('missing invalid expression') }
		assert message.contains('generic instance'), message
		assert message.contains('at byte ${offset} in rewritten_method.v'), message
	}
}
