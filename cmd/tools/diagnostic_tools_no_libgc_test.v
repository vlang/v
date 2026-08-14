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
	// that received the `-gc none` fix) and then runs it.
	res := os.execute('${os.quoted_path(vexe)} doctor')
	assert res.exit_code == 0, res.output
	vdoctor_exe := os.join_path(os.dir(vexe), 'cmd', 'tools', 'vdoctor')
	if !os.exists(vdoctor_exe) {
		eprintln('skipping test, `${vdoctor_exe}` was not produced')
		return
	}
	libs := os.execute('${os.quoted_path(otool)} -L ${os.quoted_path(vdoctor_exe)}')
	assert libs.exit_code == 0, libs.output
	assert !libs.output.contains('libgc'), 'vdoctor must not depend on libgc:\n${libs.output}'
}
