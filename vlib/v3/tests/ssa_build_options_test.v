import os
import v3.flat
import v3.parser
import v3.pref
import v3.ssa
import v3.types

fn parse_checked(name string, source string) (&flat.FlatAst, &types.TypeChecker) {
	src := os.join_path(os.temp_dir(), 'v3_ssa_buildopts_${name}.v')
	os.write_file(src, source) or { panic(err) }
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(src)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.annotate_types()
	assert tc.errors.len == 0
	return a, &tc
}

fn func_named(m &ssa.Module, name string) ssa.Function {
	for f in m.funcs {
		if f.name == name {
			return f
		}
	}
	assert false, 'function ${name} not found'
	return ssa.Function{}
}

const prog = 'fn helper() int {\n\treturn 41\n}\n\nfn other() int {\n\treturn 7\n}\n\nfn main() {\n\t_ := helper() + other()\n}\n'

fn test_module_name_is_set() {
	a, tc := parse_checked('modname', prog)
	m := ssa.build_with_used(a, map[string]bool{}, tc)
	assert m.name == 'main'
}

fn test_default_build_materializes_bodies() {
	a, tc := parse_checked('default', prog)
	m := ssa.build_with_used(a, map[string]bool{}, tc)
	// helper has a real body (blocks) and is not a prototype.
	helper := func_named(m, 'helper')
	assert helper.blocks.len > 0
	assert !helper.is_prototype
}

fn test_skip_fn_bodies_marks_prototypes() {
	a, tc := parse_checked('skipbodies', prog)
	m := ssa.build_with_options(a, map[string]bool{}, tc, ssa.BuildOptions{
		skip_fn_bodies: true
	})
	// User functions are registered but bodyless prototypes.
	helper := func_named(m, 'helper')
	assert helper.blocks.len == 0
	assert helper.is_prototype
	other := func_named(m, 'other')
	assert other.is_prototype
}

fn test_hot_fn_builds_only_target() {
	a, tc := parse_checked('hotfn', prog)
	m := ssa.build_with_options(a, map[string]bool{}, tc, ssa.BuildOptions{
		hot_fn: 'helper'
	})
	helper := func_named(m, 'helper')
	assert helper.blocks.len > 0 // hot target built
	assert !helper.is_prototype
	other := func_named(m, 'other')
	assert other.blocks.len == 0 // non-target stubbed
	assert other.is_prototype
}

fn test_c_externs_are_external_prototypes() {
	a, tc := parse_checked('externs', prog)
	m := ssa.build_with_used(a, map[string]bool{}, tc)
	mut found_extern := false
	for f in m.funcs {
		if f.is_c_extern {
			found_extern = true
			assert f.is_prototype
			assert f.linkage == .external
		}
	}
	assert found_extern
}
