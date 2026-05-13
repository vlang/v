import os

const vexe = @VEXE

const vroot = os.real_path(@VMODROOT)

fn test_noreturn_attr_generation_does_not_depend_on_stdnoreturn_h() {
	os.chdir(vroot) or {}
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_noreturn_attr_test')
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'noreturn_attr_case.v')
	source := [
		'@[noreturn]',
		'fn terminate() {',
		'\texit(1)',
		'}',
		'',
		'fn main() {',
		'\tterminate()',
		'}',
	].join('\n')
	os.write_file(source_path, source)!
	cmd := '${os.quoted_path(vexe)} -o - ${os.quoted_path(source_path)}'
	compilation := os.execute(cmd)
	assert compilation.exit_code == 0
	generated_c := compilation.output.replace('\r\n', '\n')
	assert generated_c.contains('#if !defined(VNORETURN)')
	assert generated_c.contains('#if defined(__TINYC__)')
	assert generated_c.contains('#define VNORETURN __attribute__((noreturn))')
	assert !generated_c.contains('#include <stdnoreturn.h>')
}
