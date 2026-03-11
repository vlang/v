import os

const vexe = @VEXE

const vroot = os.real_path(@VMODROOT)

const testdata_file = os.join_path(vroot, 'vlib/v/gen/c/testdata/no_builtin_no_preludes_types.vv')

fn test_no_builtin_no_preludes_types_are_lowered_to_c() {
	os.chdir(vroot) or {}
	cmd := '${os.quoted_path(vexe)} -no-builtin -no-preludes -enable-globals -gc none -no-skip-unused -o - ${os.quoted_path(testdata_file)}'
	compilation := os.execute(cmd)
	assert compilation.exit_code == 0
	generated_c := compilation.output.replace('\r\n', '\n')
	assert !generated_c.contains('typedef array Array_charptr;')
	assert !generated_c.contains('voidptr file')
	assert generated_c.contains('main__proc_read(void* file, char* buff, u32 size, u64* offset);')
	assert generated_c.contains('(void*,char*,u32,u64*);')
}
