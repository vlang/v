module c

import os
import v.cmdexec

fn test_unreferenced_c_struct_binding_does_not_redefine_header_type() ! {
	root := os.join_path(os.vtmp_dir(), 'unreferenced_c_struct_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main.c')
	os.write_file(source, 'import net as _
struct C.pollfd {
mut:
 fd i32
 events i16
 revents i16
}
struct C.KeptLayout { value int }
struct C.DirectLayout { value int }
struct Holder { items [1]C.KeptLayout }
fn main() {
 direct := C.DirectLayout{7}
 assert direct.value == 7
 holder := Holder{}
 assert holder.items[0].value == 0
}')!
	generated_result := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', '-o', output, source])
	assert generated_result.exit_code == 0, generated_result.output
	generated := os.read_file(output)!
	assert !generated.contains('struct pollfd {')
	assert generated.contains('struct KeptLayout {')
	assert generated.contains('struct DirectLayout {')
	run_result := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', 'run', source])
	assert run_result.exit_code == 0, run_result.output
}
