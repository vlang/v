module types

import os
import v.parser
import v.pref

fn test_comptime_preflight_does_not_resolve_generic_parameter_in_a_stale_module() {
	root := os.join_path(os.vtmp_dir(), 'comptime_source_preflight_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	generic := os.join_path(root, 'generic.v')
	stale := os.join_path(root, 'stale.v')
	os.write_file(generic, 'module generic\npub fn walk[T](val T) {\n\t\$if T is \$sumtype {\n\t\t\$for variant in val.variants { _ = variant }\n\t}\n}\n')!
	os.write_file(stale, 'module stale\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([generic, stale])
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_comptime_for_source_types_preflight()
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.cur_file == stale
	assert tc.cur_module == 'stale'
	tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_comptime_preflight_checks_named_types_in_their_own_module() {
	root := os.join_path(os.vtmp_dir(), 'comptime_named_source_preflight_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	first := os.join_path(root, 'first.v')
	second := os.join_path(root, 'second.v')
	os.write_file(first, 'module first\nstruct Item {}\nfn walk() { \$for variant in Item.variants { _ = variant } }\n')!
	os.write_file(second, 'module second\ntype Item = int | string\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([first, second])
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_comptime_for_source_types_preflight()
	assert tc.errors.len == 1, tc.errors.str()
	assert tc.errors[0].file == first
	assert tc.errors[0].msg == 'Item is not Sum type to use with .variants'
}
