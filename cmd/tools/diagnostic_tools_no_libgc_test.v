// Regression test for https://github.com/vlang/v/issues/27148
// The diagnostic tools (vself, vup, vdoctor, vsymlink) are compiled with `-g` so
// their crash backtraces have .v line numbers. On macOS the default GC (boehm)
// made them link `@rpath/libgc.dylib`; if that dylib could not be found, the
// tool failed to even start with a dynamic loader error. They are now compiled
// with `-gc none` as well, so they carry no such runtime dependency.
import os

fn test_vdoctor_has_no_libgc_dependency() {
	$if !macos {
		// The dynamic-loader failure and the `otool -L` check are macOS specific.
		return
	}
	otool := os.find_abs_path_of_executable('otool') or {
		eprintln('skipping test, `otool` is not available')
		return
	}
	vexe := @VEXE
	// `v doctor` builds the vdoctor tool through `util.launch_tool` (the code path
	// that received the `-gc none` fix) and then runs it. A private tool cache makes
	// it build the tool from the current sources, at a place that this test can find.
	tools := os.join_path(os.vtmp_dir(), 'diagnostic_tools_${os.getpid()}')
	os.mkdir_all(tools, mode: 0o700)!
	defer {
		os.rmdir_all(tools) or {}
	}
	mut doctor := os.new_process(vexe)
	doctor.set_args(['doctor'])
	mut environment := os.environ()
	environment['VTOOLS_CACHE_DIR'] = tools
	doctor.set_environment(environment)
	doctor.set_redirect_stdio_merged()
	doctor.run()
	output := doctor.stdout_slurp()
	doctor.wait()
	code := doctor.code
	doctor.close()
	assert code == 0, output
	built := os.walk_ext(tools, '').filter(os.file_name(it) == 'vdoctor' && os.is_executable(it))
	if built.len == 0 {
		// VTOOLS_NO_CACHE builds the tool next to its source.
		eprintln('skipping test, `v doctor` did not build vdoctor in ${tools}')
		return
	}
	libs := os.exec(['${otool}', '-L', built[0]])
	assert libs.exit_code == 0, libs.output
	// The first line names the binary itself; the libraries follow it.
	dependencies := libs.output.split_into_lines()#[1..]
	assert !dependencies.any(it.contains('libgc')), 'vdoctor must not depend on libgc:\n${libs.output}'
}
