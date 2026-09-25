import os

// A script (top-level statements, no `fn main`) written into the same directory as
// the V3 module cache must still build once another program has warmed that cache:
// the cached `.vh` module headers are not part of the script's project.
fn test_script_builds_next_to_a_warm_module_cache() {
	$if windows {
		return
	}
	// The module cache is only used with the platform `cc`, not with tcc.
	os.find_abs_path_of_executable('cc') or { return }
	dir := os.join_path(os.vtmp_dir(), 'v_script_warm_module_cache_${os.getpid()}')
	os.mkdir_all(dir)!
	old_vtmp := os.getenv('VTMP')
	os.setenv('VTMP', dir, true)
	defer {
		if old_vtmp == '' {
			os.unsetenv('VTMP')
		} else {
			os.setenv('VTMP', old_vtmp, true)
		}
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'warm.v'), "fn main() {\n\tprintln('warm')\n}\n")!
	os.write_file(os.join_path(dir, 'script.v'), "println('after warm cache')\n")!
	vexe := os.quoted_path(@VEXE)
	// Build every path from `dir` and a base name, so a `.v` inside VTMP is left alone.
	for name in ['warm', 'script'] {
		src := os.quoted_path(os.join_path(dir, '${name}.v'))
		exe := os.quoted_path(os.join_path(dir, '${name}.exe'))
		res := os.execute('${vexe} -cc cc -o ${exe} ${src}')
		if res.exit_code != 0 {
			eprintln(res.output)
		}
		assert res.exit_code == 0
	}
	res := os.execute(os.quoted_path(os.join_path(dir, 'script.exe')))
	assert res.exit_code == 0
	assert res.output.trim_space() == 'after warm cache'
}
