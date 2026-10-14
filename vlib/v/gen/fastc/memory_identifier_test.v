module fastc

import os
import v.cmdexec

fn test_fastc_locals_do_not_shadow_tinycc_memory_helpers() {
	$if !macos && !linux {
		return
	}
	vroot := os.real_path(os.join_path(@DIR, '../../../..'))
	root := os.join_path(os.vtmp_dir(), 'fastc_memory_identifiers_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Record {
	name string
	values []int
}
fn main() {
	memset := 3
	memcpy := 4
	memmove := 5
	mut record := Record{}
	record.name = "memory helpers"
	record.values = [memset, memcpy, memmove]
	copy := record
	assert copy.name == "memory helpers"
	assert copy.values == [3, 4, 5]
	assert memset + memcpy + memmove == 12
	println("memory identifiers work")
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], vroot, {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'memory identifiers work', run.output
}
