module types

import os
import v.parser
import v.pref

fn inferred_fields_errors(source string) []TypeError {
	path := os.join_path(os.vtmp_dir(), 'v3_inferred_fields_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics_opt(false)
	return tc.errors
}

fn test_inferred_anonymous_field_expressions_keep_their_types() {
	for body in [
		'value := struct { item: produce() }; consume(value.item)',
		'value := struct { nested: struct { item: produce() } }; consume(value.nested.item)',
		'mut value := struct { item: produce() }; value.item = "updated"; consume(value.item)',
		'value := struct { item: produce() }; copy := value; consume(copy.item)',
		'value := struct { item: produce() }; read := fn [value] () string { return value.item }; consume(read())',
	] {
		errors := inferred_fields_errors('module main\nfn produce() string { return "owned" }\nfn consume(value string) {}\nfn main() { ${body} }\n')
		assert errors.len == 0, '${body}: ${errors}'
	}
}

fn test_inferred_anonymous_fields_keep_type_and_mutability_diagnostics() {
	for index, body in [
		'value := struct { item: produce() }; value.item = "updated"',
		'value := struct { item: produce() }; consume(value.missing)',
		'value := struct { item: produce() }; number(value.item)',
	] {
		errors := inferred_fields_errors('module main\nfn produce() string { return "owned" }\nfn consume(value string) {}\nfn number(value int) {}\nfn main() { ${body} }\n')
		assert errors.len > 0, body
		assert !errors.any(it.msg.contains('has no field named `item`')), errors.str()
		match index {
			0 {
				assert errors.any(it.msg.contains('immutable')), errors.str()
			}
			1 {
				assert errors.any(it.msg.contains('missing')), errors.str()
			}
			2 {
				assert errors.any(it.msg.contains('`string`') && it.msg.contains('`int`')), errors.str()
			}
			else {}
		}
	}
}

fn test_inferred_anonymous_field_lookup_keeps_sibling_bindings_separate() {
	errors := inferred_fields_errors('module main
fn produce() string { return "owned" }
fn count() int { return 42 }
fn consume(value string) {}
fn number(value int) {}
fn main() {
	if true {
		value := struct { item: produce() }
		consume(value.item)
	}
	if true {
		value := struct { item: count() }
		number(value.item)
	}
}
')
	assert errors.len == 0, errors.str()
}

fn test_inferred_anonymous_field_lookup_preserves_parent_binding_after_invalid_shadow() {
	errors := inferred_fields_errors('module main
fn produce() string { return "owned" }
fn count() int { return 42 }
fn consume(value string) {}
fn number(value int) {}
fn main() {
	value := struct { item: produce() }
	if true {
		value := struct { item: count() }
		number(value.item)
	}
	consume(value.item)
}
')
	assert errors.len > 0
	assert errors.all(it.msg.contains('redefinition of `value`')), errors.str()
}
