import os
import v.cmdexec
import v.tempname

const borrowed_projection_fixture_dir = os.join_path(os.dir(@FILE), 'testdata', 'ownership_borrowed_projection')

fn run_borrowed_projection_fixture(compiler string, mode string, source string) os.Result {
	return cmdexec.run_in_merged_with_env(compiler, ['-nocache', '-ownership', '-d', 'ownership',
		...(os.split_args(mode) or { panic(err) }), 'run', source], '', {
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
	parts << 'fn main() {\n${checks.join('\n')}\n}\n'
	source := os.join_path(root, 'main.v')
	os.write_file(source, parts.join('\n'))!
	for mode in ['-no-parallel', ''] {
		out := run_borrowed_projection_fixture(compiler, mode, source)
		assert out.exit_code == 0, '${mode}: ${out.output}'
	}
	run_imported_const_projection_is_cloned(compiler, root)
}

fn run_imported_const_projection_is_cloned(compiler string, root string) {
	project := os.join_path(root, 'v3_owned_const_shadow')
	os.mkdir_all(os.join_path(project, 'cachemod')) or { panic(err) }
	fixtures := os.join_path(borrowed_projection_fixture_dir, 'imported_const')
	os.cp(os.join_path(fixtures, 'v.mod.txt'), os.join_path(project, 'v.mod')) or { panic(err) }
	os.cp(os.join_path(fixtures, 'cachemod.v.txt'), os.join_path(project, 'cachemod', 'cachemod.v')) or { panic(err) }
	os.cp(os.join_path(fixtures, 'main.v.txt'), os.join_path(project, 'main.v')) or { panic(err) }
	for mode in ['-no-parallel', ''] {
		out := run_borrowed_projection_fixture(compiler, mode, os.join_path(project, 'main.v'))
		assert out.exit_code == 0, out.output
		assert out.output.count('clone') == 1, out.output
	}
}
