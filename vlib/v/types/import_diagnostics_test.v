module types

import os
import v.flat
import v.parser
import v.pref

fn test_cached_module_header_uses_original_source_identity() {
	root := os.join_path(os.vtmp_dir(), 'v3_cached_module_identity_${os.getpid()}')
	source := os.join_path(root, 'app', 'html', 'html.v')
	header := os.join_path(root, 'cache', 'html.vh')
	os.mkdir_all(os.dir(source))!
	os.mkdir_all(os.dir(header))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'project' }")!
	os.write_file(source, 'module html\n')!
	for import_path in ['net.html', 'app.html as own'] {
		os.write_file(header, 'module html\nimport ${import_path}\n')!
		mut p := parser.Parser.new(pref.new_preferences())
		mut a := p.parse_file(header)
		a.cached_header_sources[header] = source
		a.resolved_module_dirs['net.html'] = os.join_path(root, 'net', 'html')
		a.resolved_module_dirs['app.html'] = os.dir(source)
		mut tc := TypeChecker.new(a)
		tc.collect(a)
		tc.check_import_diagnostics()
		if import_path == 'net.html' {
			assert tc.errors.len == 0, tc.errors.str()
		} else {
			assert tc.errors.any(it.msg.contains('cannot import `app.html` into a module with the same name')), tc.errors.str()
		}
	}
}

fn test_synthetic_vsh_import_has_no_source_diagnostics() {
	path := os.join_path(os.vtmp_dir(), 'v3_synthetic_vsh_import_${os.getpid()}.vsh')
	os.write_file(path, '// a comment without it\nfn helper() {}\n')!
	defer {
		os.rm(path) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_import_diagnostics()
	tc.check_unused_import_diagnostics()
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.notices.len == 0, tc.notices.str()
}

fn test_synthetic_runtime_import_may_match_current_module() {
	path := os.join_path(os.vtmp_dir(), 'v3_synthetic_self_import_${os.getpid()}.v')
	os.write_file(path, 'module sync\n')!
	defer {
		os.rm(path) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	a.add_node(flat.Node{
		kind:  .import_decl
		value: 'sync'
		typ:   'sync'
	})
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_import_diagnostics()
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_explicit_import_keeps_source_diagnostics() {
	path := os.join_path(os.vtmp_dir(), 'v3_explicit_import_${os.getpid()}.v')
	os.write_file(path, 'import time math\n')!
	defer {
		os.rm(path) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_import_diagnostics()
	assert tc.errors.any(it.msg == 'cannot import multiple modules at a time'), tc.errors.str()
}

fn test_explicit_unused_import_keeps_source_diagnostics() {
	path := os.join_path(os.vtmp_dir(), 'v3_explicit_unused_import_${os.getpid()}.v')
	os.write_file(path, 'import os\n')!
	defer {
		os.rm(path) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_import_diagnostics()
	tc.check_unused_import_diagnostics()
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.notices.any(it.msg.contains("module 'os' is imported but never used")), tc.notices.str()
}

fn test_sql_table_references_use_imports() {
	path := os.join_path(os.vtmp_dir(), 'v3_sql_table_import_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	for import_path in ['schema_base', 'model.schema_base', 'model.schema_base as schema'] {
		qualifier := if import_path.ends_with(' as schema') { 'schema' } else { 'schema_base' }
		for statement in ['create table ${qualifier}.BaseRegion', 'drop table ${qualifier}.BaseRegion',
			'select from ${qualifier}.BaseRegion',
			'create table LocalRegion\ncreate table ${qualifier}.BaseRegion'] {
			os.write_file(path, 'import orm\nimport ${import_path}\nfn run(db orm.Connection) {\n sql db {\n ${statement}\n } or {}\n}\nfn main() {}\n')!
			mut p := parser.Parser.new(pref.new_preferences())
			a := p.parse_file(path)
			assert p.diagnostics.len == 0, p.diagnostics.str()
			mut tc := TypeChecker.new(a)
			tc.collect(a)
			tc.check_unused_import_diagnostics()
			assert tc.errors.len == 0, tc.errors.str()
			assert tc.notices.len == 0, '${import_path}: ${statement}: ${tc.notices}'
		}
	}
}

fn test_sql_table_references_leave_unrelated_imports_unused() {
	path := os.join_path(os.vtmp_dir(), 'v3_sql_unused_table_import_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	for table_name in ['BaseRegion', 'other.BaseRegion', 'other_schema_base.BaseRegion'] {
		os.write_file(path, 'import orm\nimport schema_base\nfn run(db orm.Connection) {\n sql db {\n create table ${table_name}\n } or {}\n}\nfn main() {}\n')!
		mut p := parser.Parser.new(pref.new_preferences())
		a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := TypeChecker.new(a)
		tc.collect(a)
		tc.check_unused_import_diagnostics()
		assert tc.errors.len == 0, tc.errors.str()
		assert tc.notices.len == 1, tc.notices.str()
		assert tc.notices[0].msg.contains("module 'schema_base' is imported but never used"), tc.notices.str()
	}
}

fn test_sql_table_import_usage_is_scoped_to_its_file() {
	root := os.join_path(os.vtmp_dir(), 'v3_sql_import_files_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	unused_path := os.join_path(root, 'unused.v')
	used_path := os.join_path(root, 'used.v')
	os.write_file(unused_path, 'import schema_base\nfn main() {}\n')!
	os.write_file(used_path, 'import orm\nimport schema_base\nfn run(db orm.Connection) {\n sql db {\n create table schema_base.BaseRegion\n } or {}\n}\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([unused_path, used_path])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_unused_import_diagnostics()
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.notices.len == 1, tc.notices.str()
	assert tc.notices[0].file == unused_path
	assert tc.notices[0].msg.contains("module 'schema_base' is imported but never used"), tc.notices.str()
}

fn test_unused_import_index_keeps_only_files_that_need_usage_diagnostics() {
	root := os.join_path(os.vtmp_dir(), 'v3_unused_import_index_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	used_path := os.join_path(root, 'used.v')
	unused_path := os.join_path(root, 'unused.v')
	dependency_path := os.join_path(root, 'dependency.v')
	plain_path := os.join_path(root, 'plain.v')
	os.write_file(used_path, 'import math\nfn main() { _ = math.sqrt(4) }\n')!
	os.write_file(unused_path, 'import os\nfn unused() {}\n')!
	os.write_file(dependency_path, 'module dep\nimport rand\npub fn helper() { _ = rand.int() }\n')!
	os.write_file(plain_path, 'fn local_helper() {}\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([used_path, unused_path, dependency_path, plain_path])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.diagnostic_files = {
		used_path:   true
		unused_path: true
	}
	indexed := tc.unused_import_nodes_by_file()
	assert indexed.len == 2
	for file_id, node_ids in indexed {
		file := a.source_files[file_id] or { panic('missing source file') }
		assert file.name in [used_path, unused_path]
		mut expected := []int{}
		for idx, node in a.nodes {
			if node.pos.id == file_id {
				expected << idx
			}
		}
		assert node_ids == expected
	}
	tc.check_unused_import_diagnostics()
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.notices.len == 1, tc.notices.str()
	assert tc.notices[0].file == unused_path
	assert tc.notices[0].msg.contains("module 'os' is imported but never used"), tc.notices.str()
	// A standalone checker with no selected-file filter still indexes every file
	// with a real import, leaving files with no imports outside the index.
	tc.diagnostic_files.clear()
	assert tc.unused_import_nodes_by_file().len == 3
}

fn unused_import_diagnostics(warns_are_errors bool, explicit_warns_are_errors bool) TypeChecker {
	path := os.join_path(os.vtmp_dir(), 'v3_unused_import_prod_${os.getpid()}.v')
	os.write_file(path, 'import os\n') or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	mut tc := TypeChecker.new(a)
	tc.warns_are_errors = warns_are_errors
	tc.explicit_warns_are_errors = explicit_warns_are_errors
	tc.collect(a)
	tc.check_import_diagnostics()
	tc.check_unused_import_diagnostics()
	return tc
}

fn test_unused_import_stays_a_warning_in_prod_builds() {
	// -prod makes checker warnings errors, but like V1 not the unused import
	// warning, which V1 reports from the parser.
	prod := unused_import_diagnostics(true, false)
	assert prod.errors.len == 0, prod.errors.str()
	assert prod.notices.any(it.msg.contains("module 'os' is imported but never used")), prod.notices.str()
	strict := unused_import_diagnostics(true, true)
	assert strict.errors.any(it.msg.contains("module 'os' is imported but never used")), strict.errors.str()
}

fn test_aliased_import_may_have_the_same_basename() {
	path := os.join_path(os.vtmp_dir(), 'v3_same_basename_import_${os.getpid()}.v')
	os.write_file(path, 'module html\n\nimport net.html as net_html\n')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_import_diagnostics()
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_nested_module_can_import_another_module_with_the_same_basename() {
	root := os.join_path(os.vtmp_dir(), 'v3_nested_import_${os.getpid()}')
	path := os.join_path(root, 'nn', 'layers', 'layer.v')
	os.mkdir_all(os.dir(path))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'project' }")!
	os.write_file(path, 'module layers\nimport project.nn.gates.layers\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	for diagnostic_root in [root, os.join_path(root, 'nn'), os.join_path(root, 'nn', 'models'),
		os.join_path(root, 'nn', 'layers')] {
		mut tc := TypeChecker.new(a)
		tc.module_diagnostic_root = diagnostic_root
		tc.collect(a)
		tc.check_import_diagnostics()
		assert tc.errors.len == 0, '${diagnostic_root}: ${tc.errors}'
	}
}

fn test_nested_module_rejects_its_canonical_self_import() {
	root := os.join_path(os.vtmp_dir(), 'v3_nested_self_import_${os.getpid()}')
	path := os.join_path(root, 'nn', 'layers', 'layer.v')
	os.mkdir_all(os.dir(path))!
	os.mkdir_all(os.join_path(root, 'layers'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'project' }")!
	os.write_file(path, 'module layers\nimport nn.layers as own\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_import_diagnostics()
	assert tc.errors.any(it.msg.contains('cannot import `nn.layers` into a module with the same name')), tc.errors.str()
}

fn test_self_import_identity_is_scoped_to_each_file() {
	root := os.join_path(os.vtmp_dir(), 'v3_multiple_module_identities_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	mut paths := []string{}
	for parent in ['first', 'second', 'third'] {
		path := os.join_path(root, parent, 'layers', 'layer.v')
		os.mkdir_all(os.dir(path))!
		import_path := if parent == 'second' { 'first.layers' } else { '${parent}.layers' }
		os.write_file(path, 'module layers\nimport os\nimport time\nimport ${import_path} as other\n')!
		paths << path
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'project' }")!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files(paths)
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_import_diagnostics()
	assert tc.errors.len == 2, tc.errors.str()
	assert tc.errors[0].msg == 'cannot import `first.layers` into a module with the same name'
	assert tc.errors[1].msg == 'cannot import `third.layers` into a module with the same name'
}

fn test_manifestless_nested_module_rejects_its_canonical_self_import() {
	root := os.join_path(os.vtmp_dir(), 'v3_manifestless_self_import_${os.getpid()}')
	path := os.join_path(root, 'nn', 'layers', 'layer.v')
	os.mkdir_all(os.dir(path))!
	os.mkdir_all(os.join_path(root, 'layers'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'layers', 'plain.v'), 'module layers\n')!
	os.write_file(path, 'module layers\nimport nn.layers as self\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	a.resolved_module_dirs['nn.layers'] = os.real_path(os.dir(path))
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	assert tc.current_file_module_path_identity() or { '' } == 'nn.layers'
	tc.check_import_diagnostics()
	assert tc.errors.any(it.msg.contains('cannot import `nn.layers` into a module with the same name')), tc.errors.str()
}

fn test_vlib_is_the_module_search_root_for_identity() {
	root := os.join_path(os.vtmp_dir(), 'v3_vlib_module_identity_${os.getpid()}')
	time_file := os.join_path(root, 'vlib', 'time', 'time.v')
	module_file := os.join_path(root, 'vlib', 'v', 'gen', 'v', 'module.v')
	os.mkdir_all(os.dir(time_file))!
	os.mkdir_all(os.dir(module_file))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'compiler' }")!
	os.write_file(time_file, 'module time\n')!
	os.write_file(module_file, 'module v\nimport v.gen.v as self\n')!
	mut tc := TypeChecker.new(&flat.FlatAst{})
	tc.compiler_vroot = root
	tc.cur_file = time_file
	tc.cur_module = 'time'
	assert !tc.current_file_uses_nested_module_path()
	tc.cur_file = module_file
	tc.cur_module = 'v'
	assert tc.current_file_module_path_identity() or { '' } == 'v.gen.v'
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(module_file)
	mut checked := TypeChecker.new(a)
	checked.compiler_vroot = root
	checked.collect(a)
	checked.check_import_diagnostics()
	assert checked.errors.any(it.msg.contains('cannot import `v.gen.v` into a module with the same name')), checked.errors.str()
}

fn test_explicit_module_search_root_defines_self_import_identity() {
	root := os.join_path(os.vtmp_dir(), 'v3_path_module_identity_${os.getpid()}')
	module_file := os.join_path(root, 'nn', 'layers', 'layer.v')
	os.mkdir_all(os.dir(module_file))!
	os.mkdir_all(os.join_path(root, 'layers'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(module_file, 'module layers\nimport nn.layers as self\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(module_file)
	mut tc := TypeChecker.new(a)
	tc.module_search_paths = [root]
	tc.collect(a)
	assert tc.current_file_module_path_identity() or { '' } == 'nn.layers'
	tc.check_import_diagnostics()
	assert tc.errors.any(it.msg.contains('cannot import `nn.layers` into a module with the same name')), tc.errors.str()
}

fn test_nested_vlib_project_manifest_defines_module_identity() {
	root := os.join_path(os.vtmp_dir(), 'v3_nested_vlib_project_${os.getpid()}')
	project := os.join_path(root, 'vlib', 'v', 'tests', 'project')
	module_file := os.join_path(project, 'mod1', 'mod1.v')
	os.mkdir_all(os.dir(module_file))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(project, 'v.mod'), "Module { name: 'project' }")!
	os.write_file(module_file, 'module mod1\n')!
	mut tc := TypeChecker.new(&flat.FlatAst{})
	tc.compiler_vroot = root
	tc.cur_file = module_file
	tc.cur_module = 'mod1'
	assert tc.current_file_module_source_root() or { '' } == os.real_path(project)
	assert !tc.current_file_uses_nested_module_path()
	assert tc.current_file_module_path_identity() or { '' } == 'mod1'
}

fn test_sql_expression_references_use_imports() {
	path := os.join_path(os.vtmp_dir(), 'v3_sql_expression_import_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	for import_path in ['time', 'time as clock', 'app.time as clock', 'strings', 'strings as clock'] {
		qualifier := if import_path.ends_with(' as clock') {
			'clock'
		} else {
			import_path
		}
		expression := if import_path.starts_with('strings') {
			"${qualifier}.trim_space('  x  ')"
		} else {
			'${qualifier}.now().format_ss()'
		}
		for statement in [
			'update Foo set updated_at = ${expression} where id == 1',
			'select from Foo where updated_at == ${expression}',
			'dynamic select from Foo where { updated_at == ${expression} }',
		] {
			os.write_file(path, 'import orm\nimport ${import_path}\nfn run(db orm.Connection) {\n sql db {\n ${statement}\n } or {}\n}\nfn main() {}\n')!
			mut p := parser.Parser.new(pref.new_preferences())
			a := p.parse_file(path)
			assert p.diagnostics.len == 0, p.diagnostics.str()
			mut tc := TypeChecker.new(a)
			tc.collect(a)
			tc.check_unused_import_diagnostics()
			assert tc.notices.len == 0, '${import_path}: ${statement}: ${tc.notices}'
		}
	}
}

fn test_sql_expression_literals_leave_imports_unused() {
	path := os.join_path(os.vtmp_dir(), 'v3_sql_literal_import_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	for expression in ["'time . now()'", "'a time . now() value'", "'time.now()'", r"r'time . now()'",
		r'r"time . now()"', r"'escaped \' time . now()'", r'"escaped \" time . now()"', r"r'time.now()\'",
		'\'r"time" time . now()\'', 'other_time.now()', 'other.time.now()', '1 // time.now()',
		'1 /* time.now() */'] {
		os.write_file(path, 'import orm\nimport time\nfn run(db orm.Connection) {\n sql db {\n select from Foo where updated_at == ${expression}\n } or {}\n}\nfn main() {}\n')!
		mut p := parser.Parser.new(pref.new_preferences())
		a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := TypeChecker.new(a)
		tc.collect(a)
		tc.check_unused_import_diagnostics()
		assert tc.notices.len == 1, tc.notices.str()
		assert tc.notices[0].msg.contains("module 'time' is imported but never used"), tc.notices.str()
	}
}

fn test_sql_literals_preserve_real_import_usage_after_them_and_in_interpolation() {
	path := os.join_path(os.vtmp_dir(), 'v3_sql_literal_followed_by_import_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	for expression in [r"r'ends\' + time.now().format_ss()", r"'${time.now()}'"] {
		os.write_file(path, 'import orm\nimport time\nfn run(db orm.Connection) {\n sql db {\n select from Foo where updated_at == ${expression}\n } or {}\n}\nfn main() {}\n')!
		mut p := parser.Parser.new(pref.new_preferences())
		a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := TypeChecker.new(a)
		tc.collect(a)
		tc.check_unused_import_diagnostics()
		assert tc.notices.len == 0, '${expression}: ${tc.notices}'
	}
}
