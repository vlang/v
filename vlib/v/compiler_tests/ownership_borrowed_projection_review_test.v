import os
import v.cmdexec
import v.tempname

const borrowed_projection_fixture_dir = os.join_path(os.dir(@FILE), 'testdata', 'ownership_borrowed_projection')

fn compile_borrowed_projection_fixture(compiler string, mode string, root string, output string) os.Result {
	return cmdexec.run_in_merged_with_env(compiler, ['-nocache', '-ownership', '-d', 'ownership',
		...(os.split_args(mode) or { panic(err) }), '-o', output, root], '', {
		'VFLAGS':               ''
		'VTOOLS_CACHE_VERBOSE': ''
	})
}

fn test_borrowed_projection_regressions_by_context() {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}
	root := os.join_path(os.vtmp_dir(), 'v3_borrowed_projection_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	common := os.read_file(os.join_path(borrowed_projection_fixture_dir, 'common.v.txt'))!
	mut parts := [common]
	mut checks := []string{}
	for name in ['assignments', 'calls_and_conversions', 'collections', 'array_initializers',
		'indexed_aliases', 'constants'] {
		fragment := os.read_file(os.join_path(borrowed_projection_fixture_dir, '${name}.v.txt'))!
		parts << fragment.replace_once('\nfn main() {', '\nfn check_${name}() {')
		checks << '\tcheck_${name}()'
	}
	files := os.join_path(borrowed_projection_fixture_dir, 'imported_const')
	for module_name in ['cachemod', 'imported_const'] {
		os.mkdir(os.join_path(root, module_name))!
	}
	os.cp(os.join_path(files, 'v.mod.txt'), os.join_path(root, 'v.mod'))!
	os.cp(os.join_path(files, 'cachemod.v.txt'), os.join_path(root, 'cachemod', 'cachemod.v'))!
	imported := os.read_file(os.join_path(files, 'main.v.txt'))!
	os.write_file(os.join_path(root, 'imported_const', 'projection.v'),
		imported.replace_once('module main', 'module imported_const').replace_once('fn main()',
			'pub fn check()'))!

	parts << 'fn check_contexts() {\n${checks.join('\n')}\n}\n'
	os.write_file(os.join_path(root, 'contexts.v'), parts.join('\n'))!
	os.write_file(os.join_path(root, 'main.v'), "module main

import imported_const

fn main() {
	match arguments()[1] {
		'contexts' { check_contexts() }
		'imported_const' { imported_const.check() }
		else { panic('unknown fixture') }
	}
}
")!
	for mode in ['-no-parallel', ''] {
		name := if mode == '' { 'program_parallel' } else { 'program_serial' }
		output := os.join_path(root, name + $if windows { '.exe' } $else { '' })
		compile := compile_borrowed_projection_fixture(compiler, mode, root, output)
		assert compile.exit_code == 0, '${mode}: ${compile.output}'
		contexts := os.exec([output, 'contexts'])
		assert contexts.exit_code == 0, '${mode}: ${contexts.output}'
		const_projection := os.exec([output, 'imported_const'])
		assert const_projection.exit_code == 0, '${mode}: ${const_projection.output}'
		assert const_projection.output.count('clone') == 1, '${mode}: ${const_projection.output}'
	}
}
