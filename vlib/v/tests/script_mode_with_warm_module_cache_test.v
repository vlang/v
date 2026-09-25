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
	warm_path := os.join_path(dir, 'warm.v')
	os.write_file(warm_path, "fn main() {\n\tprintln('warm')\n}\n")!
	script_path := os.join_path(dir, 'script.v')
	os.write_file(script_path, "println('after warm cache')\n")!
	vexe := os.quoted_path(@VEXE)
	for path in [warm_path, script_path] {
		exe := os.quoted_path(path.replace('.v', '.exe'))
		res := os.execute('${vexe} -cc cc -o ${exe} ${os.quoted_path(path)}')
		if res.exit_code != 0 {
			eprintln(res.output)
		}
		assert res.exit_code == 0
	}
	res := os.execute(os.quoted_path(script_path.replace('.v', '.exe')))
	assert res.exit_code == 0
	assert res.output.trim_space() == 'after warm cache'
}
