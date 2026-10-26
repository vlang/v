module fastc

import os
import v.cmdexec
import v.flat

fn test_fastc_tagged_c_array_element_types() {
	assert flat.source_type_name(fastc_array_element_type(flat.source_type_id('Array_struct_iovec')) or { u32(0) }) == 'struct iovec'
	assert flat.source_type_name(fastc_array_element_type(flat.source_type_id('Array_struct_iovec_ptr')) or { u32(0) }) == 'struct iovec*'
	assert flat.source_type_name(fastc_fixed_array_element_type(flat.source_type_id(fastc_fixed_array_type('2', 'struct iovec'))) or { u32(0) }) == 'struct iovec'
	$if !macos && !linux {
		return
	}
	vroot := os.real_path(os.join_path(@DIR, '../../../..'))
	root := os.join_path(os.vtmp_dir(), 'fastc_c_native_arrays_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.c.v'), r'module main
#include <sys/uio.h>
struct C.iovec {
mut:
	iov_base voidptr
	iov_len uint
}
fn main() {
	mut values := []C.iovec{cap: 2}
	values << C.iovec{iov_len: 11}
	values << C.iovec{iov_len: 22}
	assert values[0].iov_len == 11
	assert values[1].iov_len == 22
	fixed := [C.iovec{iov_len: 33}, C.iovec{iov_len: 44}]!
	assert fixed[0].iov_len == 33
	assert fixed[1].iov_len == 44
	println("native arrays work")
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
	assert run.output.trim_space() == 'native arrays work', run.output
}
