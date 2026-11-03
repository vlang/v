module fastc

import os
import v.cmdexec

fn test_fastc_nested_if_value_skips_fallback_braces_in_its_condition() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_nested_if_fallback_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main

__global fallbacks int

fn choose(values map[string]string, enabled bool) int {
	return if enabled {
		if values["type"] or { fallbacks++; "base" } == "noise" { 1 } else { 0 }
	} else { 2 }
}

fn main() {
	assert choose(map[string]string{}, true) == 0
	assert fallbacks == 1
	assert choose({"type": "noise"}, true) == 1
	assert fallbacks == 1
	assert choose(map[string]string{}, false) == 2
	assert fallbacks == 1
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-enable-globals', '-nocache', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
