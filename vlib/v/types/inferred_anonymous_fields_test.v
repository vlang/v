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

fn test_inferred_anonymous_fields_follow_implicit_array_bindings() {
	for body in [
		'values := rows.map(it.item); consume(values)',
		'values := rows.filter(it.item > 0).map(it.item); consume(values)',
		'assert rows.any(it.item > 0); assert rows.all(it.item > 0)',
		'assert rows.count(it.item > 0) == 1',
	] {
		errors := inferred_fields_errors('module main
fn text() string { return "outer" }
fn number() int { return 7 }
fn consume(values []int) {}
fn main() {
    it := struct { item: text() }
    rows := [struct { item: number() }]
    ${body}
    assert it.item == "outer"
}
')
		assert errors.len == 0, '${body}: ${errors}'
	}
}

fn test_inferred_anonymous_array_fields_do_not_come_from_outer_it() {
	errors := inferred_fields_errors('module main
fn text() string { return "outer" }
fn number() int { return 7 }
fn main() {
    it := struct { ghost: text() }
    rows := [struct { item: number() }]
    values := rows.map(it.ghost)
    assert values.len == 1
    assert it.ghost == "outer"
}
')
	assert errors.len > 0, errors.str()
	assert errors.any(it.msg.contains('ghost')), errors.str()
}
