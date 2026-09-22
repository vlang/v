module types

import os
import v.parser
import v.pref

fn test_nested_module_path_allows_local_name() {
	for path, root in {
		'/tmp/project/sim/args/parser.v': '/tmp/project'
		r'C:\project\sim\args\parser.v':  r'C:\project'
	} {
		tc := TypeChecker{
			cur_file:               path
			cur_module:             'args'
			module_diagnostic_root: root
		}
		assert tc.current_file_uses_nested_module_path()
	}
}

fn test_current_file_path_not_matching_module_keeps_conflict() {
	direct_module := TypeChecker{
		cur_file:               '/tmp/project/args/parser.v'
		cur_module:             'args'
		module_diagnostic_root: '/tmp/project'
	}
	assert !direct_module.current_file_uses_nested_module_path()
	mismatched_module := TypeChecker{
		cur_file:   '/tmp/project/checker/tests/module_name.vv'
		cur_module: 'foo'
	}
	assert !mismatched_module.current_file_uses_nested_module_path()
}

fn test_local_can_match_module_when_file_is_in_module_directory() {
	root := os.join_path(os.vtmp_dir(), 'v3_module_name_path_${os.getpid()}')
	module_dir := os.join_path(root, 'sim', 'args')
	os.mkdir_all(module_dir)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	path := os.join_path(module_dir, 'parser.v')
	os.write_file(path, 'module args\nfn parse() int {\n\targs := 1\n\treturn args\n}\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.module_diagnostic_root = root
	tc.collect(a)
	tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()
}
