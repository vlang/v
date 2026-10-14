module fastc

import os
import v.cmdexec

fn test_fastc_missing_map_array_values_keep_element_storage_when_appended_locally() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_missing_map_array_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main

type Names = []string

fn add(mut sources map[string][]string, key string, value string) {
	mut values := sources[key]
	if value !in values {
		values << value
		sources[key] = values
	}
}

fn main() {
	mut sources := map[string][]string{}
	add(mut sources, "pointer", "first")
	add(mut sources, "pointer", "second")
	add(mut sources, "pointer", "first")
	assert sources["pointer"] == ["first", "second"]
	mut seen := map[string]bool{}
	for name in sources["pointer"] {
		seen[name] = true
	}
	assert "first" in seen
	assert "second" in seen
	mut named := map[string]Names{}
	mut names := named["missing"]
	names << "alias"
	named["missing"] = names
	assert named["missing"][0] == "alias"
	mut counts := map[string][]int{}
	mut count_values := counts["missing"]
	count_values << 17
	count_values << 23
	counts["missing"] = count_values
	assert counts["missing"] == [17, 23]
	println("missing map arrays work")
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], os.real_path(os.join_path(@DIR, '../../../..')), {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'missing map arrays work', run.output
}
