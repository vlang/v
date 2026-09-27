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
