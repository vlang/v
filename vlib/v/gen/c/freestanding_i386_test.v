module c

import os

const test_vexe = os.quoted_path(@VEXE)

fn test_freestanding_i386_c_output_uses_int80_and_static_memdup_decl() {
	workdir := os.join_path(os.vtmp_dir(), 'v_freestanding_i386_${os.getpid()}')
	os.mkdir_all(workdir)!
	defer {
		os.rmdir_all(workdir) or {}
	}
	main_v := os.join_path(workdir, 'main.v')
	out_c := os.join_path(workdir, 'main.c')
	os.write_file(main_v, "module main

@[export: 'kmain']
pub fn kmain() {
	for {}
}
")!
	cmd := '${test_vexe} -gc none -no-skip-unused -freestanding -no-std -is_o -m32 -o ${os.quoted_path(out_c)} ${os.quoted_path(main_v)}'
	res := os.execute(cmd)
	assert res.exit_code == 0, '${cmd}\n${res.output}'
	generated := os.read_file(out_c)!
	assert generated.contains('VV_LOC voidptr builtin__memdup(voidptr src, isize size);')
	assert !generated.contains('\nvoidptr builtin__memdup(voidptr src, isize size);\n')
	assert generated.contains('int $0x80')
	assert !generated.contains('"syscall \\n\\t"')
	assert !generated.contains('mov %[arg4], %%r10')
	assert !generated.contains('typedef long unsigned int size_t;')
}
