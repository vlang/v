module c

import os
import v.builder
import v.pref

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

fn test_parallel_cc_windows_header_keeps_vv_loc_external() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'parallel_cc_windows_linkage_${os.getpid()}')
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'main.v')
	os.write_file(source_path, 'module main
fn helper() string {
	return "ok"
}

fn main() {
	println(helper())
}
')!
	mut prefs, _ := pref.parse_args_and_show_errors([], [
		'',
		'-parallel-cc',
		'-os',
		'windows',
		source_path,
	], false)
	mut b := builder.new_builder(prefs)
	mut files := b.get_builtin_files()
	files << b.get_user_files()
	b.set_module_lookup_paths()
	b.front_and_middle_stages(files)!
	result := gen(b.parsed_files, mut b.table, b.pref)
	header := result.header.replace('\r\n', '\n')
	assert header.contains('#define _VPARALLELCC (1)'), header
	assert header.contains('#ifdef _VPARALLELCC\n\t\t#define VV_LOC\n\t#else\n\t\t#define VV_LOC static\n\t#endif'), header
	assert header.contains('VV_LOC string main__helper(void);'), header
}
