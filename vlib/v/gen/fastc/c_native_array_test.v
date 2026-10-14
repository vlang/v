module fastc

import os
import v.cmdexec

fn test_fastc_tagged_c_array_element_types() {
	assert fastc_array_element_type('Array_struct_iovec') or { '' } == 'struct iovec'
	assert fastc_array_element_type('Array_struct_iovec_ptr') or { '' } == 'struct iovec*'
	assert fastc_fixed_array_element_type(fastc_fixed_array_type('2', 'struct iovec')) or { '' } == 'struct iovec'
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
	iov_len usize
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
