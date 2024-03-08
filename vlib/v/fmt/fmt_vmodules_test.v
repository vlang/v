import os

const vexe = os.quoted_path(@VEXE)
const vmodules_tdir = os.join_path(os.vtmp_dir(), 'fmt_vmodules_test')
const module_tdir = os.join_path(vmodules_tdir, 'foo')

fn testsuite_begin() {
	os.mkdir_all(module_tdir) or {}
	os.setenv('VMODULES', vmodules_tdir, true)
}

fn test_fmt_vmodules() {
	tfile_content := [
		'import x.json2 as json',
		'import datatypes { Stack }',
		'',
		'const my_datatype = Stack[string]{}',
		'',
	].join_lines()
	os.write_file(os.join_path(module_tdir, 'main.v'), tfile_content)!
	os.execute_opt('${vexe} fmt -c ${module_tdir}') or { assert false, err.msg() }
}
