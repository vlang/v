module wasm

import os
import v.parser
import v.pref
import v.types

fn test_ssa_configuration_keeps_implicit_main_dependencies() {
	dir := os.join_path(os.vtmp_dir(), 'wasm_script_reachability_${os.getpid()}')
	os.mkdir_all(os.join_path(dir, 'foo')) or { panic(err) }
	defer { os.rmdir_all(dir) or {} }
	main_source := os.join_path(dir, 'main.v')
	foo_source := os.join_path(dir, 'foo', 'foo.v')
	os.write_file(main_source, '
import foo as helper
println(helper.answer())
callback := helper.callback_value
println(callback())
') or { panic(err) }
	os.write_file(foo_source, '
module foo
fn value() int { return 21 }
pub fn answer() int { return value() * 2 }
pub fn callback_value() int { return value() * 3 }
fn ignored() int { return 0 }
fn prepare() {}
fn init() { prepare() }
') or { panic(err) }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([foo_source, main_source])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()
	tc.annotate_types()
	mut metadata := Gen.new(a, &tc, map[string]bool{})
	config := metadata.ssa_configuration()
	assert config.main_fn == ''
	assert config.used_fns['foo.answer'], config.used_fns.str()
	assert config.used_fns['foo.callback_value'], config.used_fns.str()
	assert config.used_fns['foo.value'], config.used_fns.str()
	assert config.used_fns['foo.prepare'], config.used_fns.str()
	assert !config.used_fns['foo.ignored'], config.used_fns.str()
	assert config.init_fns == ['foo.init'], config.init_fns.str()
}
