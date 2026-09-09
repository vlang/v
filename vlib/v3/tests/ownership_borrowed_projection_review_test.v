import os

const borrowed_projection_vexe = @VEXE
const borrowed_projection_tests_dir = os.dir(@FILE)
const borrowed_projection_v3_dir = os.dir(borrowed_projection_tests_dir)
const borrowed_projection_vlib_dir = os.dir(borrowed_projection_v3_dir)
const borrowed_projection_v3_src = os.join_path(borrowed_projection_v3_dir, 'v3.v')
const borrowed_projection_fixture_dir = os.join_path(borrowed_projection_tests_dir, 'testdata', 'ownership_borrowed_projection')

fn build_borrowed_projection_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_borrowed_projection_${os.getpid()}')
	os.rm(v3_bin) or {}
	build := os.execute('${borrowed_projection_vexe} -nocache -gc none -d ownership -path "${borrowed_projection_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${borrowed_projection_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn run_borrowed_projection_source(v3_bin string, name string, source_text string) {
	source := os.join_path(os.temp_dir(), 'v3_borrowed_projection_${name}_${os.getpid()}.v')
	defer {
		os.rm(source) or {}
	}
	os.write_file(source, source_text) or { panic(err) }
	for mode in ['-no-parallel', ''] {
		out := os.execute('${v3_bin} -nocache -ownership -d ownership ${mode} run ${source}')
		assert out.exit_code == 0, '${name} (${mode}): ${out.output}'
	}
}

fn test_borrowed_projection_regressions_by_context() {
	v3_bin := build_borrowed_projection_v3()
	defer {
		os.rm(v3_bin) or {}
	}
	common := os.read_file(os.join_path(borrowed_projection_fixture_dir, 'common.v.txt')) or {
		panic(err)
	}
	for name in ['assignments', 'calls_and_conversions', 'collections', 'indexed_aliases', 'constants'] {
		fragment := os.read_file(os.join_path(borrowed_projection_fixture_dir, '${name}.v.txt')) or { panic(err) }
		run_borrowed_projection_source(v3_bin, name, common + '\n' + fragment)
	}
	run_imported_const_projection_is_cloned(v3_bin)
}

fn run_imported_const_projection_is_cloned(v3_bin string) {
	project := os.join_path(os.temp_dir(), 'v3_owned_const_shadow_${os.getpid()}')
	os.rmdir_all(project) or {}
	os.mkdir_all(os.join_path(project, 'cachemod')) or { panic(err) }
	defer {
		os.rmdir_all(project) or {}
	}
	fixtures := os.join_path(borrowed_projection_fixture_dir, 'imported_const')
	os.cp(os.join_path(fixtures, 'v.mod.txt'), os.join_path(project, 'v.mod')) or { panic(err) }
	os.cp(os.join_path(fixtures, 'cachemod.v.txt'), os.join_path(project, 'cachemod', 'cachemod.v')) or { panic(err) }
	os.cp(os.join_path(fixtures, 'main.v.txt'), os.join_path(project, 'main.v')) or { panic(err) }
	for mode in ['-no-parallel', ''] {
		out := os.execute('${v3_bin} -nocache -ownership -d ownership ${mode} run ${os.join_path(project, 'main.v')}')
		assert out.exit_code == 0, out.output
		assert out.output.count('clone') == 1, out.output
	}
}
