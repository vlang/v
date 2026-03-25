module c

import os

const test_vexe = os.quoted_path(@VEXE)

fn test_is_o_generated_c_files_can_be_linked_together() {
	cc_path := os.find_abs_path_of_executable('cc') or { return }
	cc := os.quoted_path(cc_path)
	workdir := os.join_path(os.vtmp_dir(), 'v_cgen_is_o_link_${os.getpid()}')
	os.mkdir_all(workdir)!
	defer {
		os.rmdir_all(workdir) or {}
	}
	a_v := os.join_path(workdir, 'a.v')
	b_v := os.join_path(workdir, 'b.v')
	host_c := os.join_path(workdir, 'main.c')
	a_c := os.join_path(workdir, 'a.c')
	b_c := os.join_path(workdir, 'b.c')
	prog := os.join_path(workdir, 'prog')
	os.write_file(a_v, 'module main

@[markused]
pub fn a() int {
	return 1
}
')!
	os.write_file(b_v, 'module main

@[markused]
pub fn b() int {
	return 2
}
')!
	os.write_file(host_c, '#include <stdio.h>
int main__a(void);
int main__b(void);

int main(void) {
	printf("%d %d\\n", main__a(), main__b());
	return 0;
}
')!
	for cmd in [
		'${test_vexe} -gc none -no-skip-unused -is_o -o ${os.quoted_path(a_c)} ${os.quoted_path(a_v)}',
		'${test_vexe} -gc none -no-skip-unused -is_o -o ${os.quoted_path(b_c)} ${os.quoted_path(b_v)}',
		'${cc} -o ${os.quoted_path(prog)} ${os.quoted_path(a_c)} ${os.quoted_path(b_c)} ${os.quoted_path(host_c)} -lm',
	] {
		res := os.execute(cmd)
		assert res.exit_code == 0, '${cmd}\n${res.output}'
	}
	res := os.execute(os.quoted_path(prog))
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == '1 2'
}
