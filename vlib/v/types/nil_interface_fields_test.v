module types

import os
import v.parser
import v.pref

fn nil_interface_field_errors(name string, body string) []TypeError {
	path := os.join_path(os.vtmp_dir(), 'nil_interface_fields_${name}_${os.getpid()}.v')
	os.write_file(path, 'interface Reader { read() int }
type ReaderAlias = Reader
@[params]
struct Config {
	reader Reader
	alias ReaderAlias
}
fn accept(config Config) { _ = config }
' + body) or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	return tc.errors.clone()
}

fn test_interface_fields_accept_explicit_unsafe_nil_in_both_initializer_forms() {
	errors := nil_interface_field_errors('unsafe_nil', 'fn main() {
	accept(reader: unsafe { nil }, alias: (unsafe { nil }))
	accept(Config{reader: unsafe { nil }, alias: (unsafe { nil })})
	unsafe {
		accept(reader: nil, alias: nil)
		accept(Config{reader: nil, alias: nil})
	}
}
')
	assert errors.len == 0, errors.str()
}

fn test_interface_fields_keep_voidptr_implementation_requirements() {
	errors := nil_interface_field_errors('voidptr', 'fn main() {
	p := unsafe { nil }
	accept(reader: p, alias: p)
	accept(Config{reader: p, alias: p})
}
')
	assert errors.len == 4, errors.str()
	assert errors.filter(it.node_value == 'reader').len == 2, errors.str()
	assert errors.filter(it.node_value == 'alias').len == 2, errors.str()
}

fn test_interface_fields_reject_nil_outside_unsafe() {
	errors := nil_interface_field_errors('safe_nil', 'fn main() {
	accept(reader: nil)
	accept(Config{reader: nil})
}
')
	assert errors.len == 2, errors.str()
	assert errors.all(it.kind == .assignment_mismatch && it.node_value == 'reader'), errors.str()
}

fn test_non_pointer_fields_still_reject_explicit_unsafe_nil() {
	errors := nil_interface_field_errors('scalar_nil', '@[params]
struct ScalarConfig { number int }
fn accept_scalar(config ScalarConfig) { _ = config }
fn main() {
	accept_scalar(number: unsafe { nil })
	accept_scalar(ScalarConfig{number: unsafe { nil }})
}
')
	assert errors.len == 2, errors.str()
	assert errors.any(it.msg == 'cannot assign to field `number`: expected `int`, not `voidptr`'), errors.str()
	assert errors.any(it.msg == 'cannot assign `nil` to struct field `number` with type `int`'), errors.str()
}

fn test_interface_field_declaration_defaults_still_reject_unsafe_nil() {
	errors := nil_interface_field_errors('default_nil', 'struct DefaultConfig {
	reader Reader = unsafe { nil }
}
fn main() { _ = DefaultConfig{} }
')
	assert errors.any(it.msg == 'cannot assign `nil` to a non-pointer field'), errors.str()
}
