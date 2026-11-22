import os

const fallback_reason = 'external C inputs cannot be assigned to cache units'
const marker_reason = 'native C inputs that an earlier build with these arguments found after parsing'

fn build_native_input_fixture(name string, source string, expected string, flags []string) (string, string) {
	root := os.join_path(os.vtmp_dir(), 'native_input_marker_${name}_${os.getpid()}')
	scratch := os.join_path(root, 'vtmp')
	os.mkdir_all(scratch) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	main := os.join_path(root, 'main.v')
	output := os.join_path(root, 'program')
	os.write_file(main, source) or { panic(err) }
	args := ['env', 'VTMP=${scratch}', 'V3CACHE=${os.join_path(root, 'cache')}',
		'VMODULES=${os.join_path(root, 'modules')}', 'VFLAGS=', 'VOSARGS=',
		'V3_DISABLE_UNCHANGED_BUILD_CACHE=1', 'V3_CACHE_DISABLE_PROGRAM_EXECUTABLE=1',
		'V3_CACHE_TRACE=1', @VEXE, '-cc', 'tcc', ...flags, '-o', output, main]
	mut traces := []string{cap: 2}
	for _ in 0 .. 2 {
		build := os.exec(args)
		assert build.exit_code == 0, build.output
		run := os.exec([output])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == expected, run.output
		traces << build.output
	}
	return traces[0], traces[1]
}

fn test_a_shipped_native_input_restarts_only_the_first_build() {
	$if macos || linux {
		// This header is shipped with V and has no module-cache ownership contract.
		first, second := build_native_input_fixture('fallback', '#include "@VEXEROOT/vlib/v/compiler_tests/testdata/vexeroot_insert.h"
fn C.v3_vexeroot_insert_value() int
fn main() { println(C.v3_vexeroot_insert_value()) }
', '37', [])
		assert first.contains(fallback_reason), first
		assert first.contains('vexeroot_insert.h'), first
		assert !first.contains(marker_reason), first
		assert second.contains(marker_reason), second
		assert !second.contains(fallback_reason), second
	}
}

fn test_cmdexec_native_interface_reuses_the_module_cache() {
	$if macos || linux {
		first, second := build_native_input_fixture('cmdexec', "import v.cmdexec
fn main() { println(cmdexec.run('true', []string{}).exit_code) }
", '0', ['-show-timings'])
		for trace in [first, second] {
			assert !trace.contains(fallback_reason), trace
			assert !trace.contains(marker_reason), trace
		}
		assert second.split_into_lines().any(it.trim_space().starts_with('.vh files: ')
			&& it.contains('/cmdexec_')), second
	}
}
