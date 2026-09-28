module types

import os
import v.parser
import v.pref

fn test_optional_interface_argument_preserves_interface_requirements() {
	path := os.join_path(os.vtmp_dir(), 'optional_interface_argument_${os.getpid()}.v')
	os.write_file(path, 'interface Named { name() string }
struct Missing {}
fn accept(value ?Named) {}
fn main() { accept(&Missing{}) }
')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.any(it.msg.contains('Missing') && it.msg.contains('Named')), tc.errors.str()
}

fn test_optional_interface_argument_rejects_extra_pointer_layers() {
	path := os.join_path(os.vtmp_dir(), 'optional_interface_indirection_${os.getpid()}.v')
	os.write_file(path, 'interface Named { name() string }
struct Record {}
fn (r Record) name() string { return "record" }
type MaybeNamed = ?Named
fn accept(value ?Named) {}
fn accept_alias(value MaybeNamed) {}
fn main() {
	item := &Record{}
	pointer := &item
	accept(pointer)
	accept_alias(pointer)
}
')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.filter(it.msg.contains('&&Record') && it.msg.contains('Named')).len == 2, tc.errors.str()
}

fn test_optional_interface_argument_accepts_pointer_alias() {
	path := os.join_path(os.vtmp_dir(), 'optional_interface_pointer_alias_${os.getpid()}.v')
	os.write_file(path, 'interface Named { name() string }
struct Record {}
fn (r Record) name() string { return "record" }
type RecordPtr = &Record
fn accept(value ?Named) {}
fn main() { accept(RecordPtr(&Record{})) }
')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_optional_interface_argument_rejects_interface_pointer() {
	path := os.join_path(os.vtmp_dir(), 'optional_interface_pointer_${os.getpid()}.v')
	os.write_file(path, 'interface Named { name() string }
struct Record {}
fn (r Record) name() string { return "record" }
type MaybeNamed = ?Named
fn accept(value ?Named) {}
fn accept_alias(value MaybeNamed) {}
fn main() {
	mut boxed := Named(Record{})
	accept(&boxed)
	accept_alias(&boxed)
}
')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.filter(it.msg.contains('&Named') && it.msg.contains('Named')).len == 2, tc.errors.str()
}
