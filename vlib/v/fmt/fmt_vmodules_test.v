import os

const vexe = os.quoted_path(@VEXE)
const vmodules_tdir = os.join_path(os.vtmp_dir(), 'fmt_vmodules_test')

fn testsuite_begin() {
	os.mkdir_all(vmodules_tdir) or {}
	os.setenv('VMODULES', vmodules_tdir, true)
}

fn testsuite_end() {
	os.rmdir_all(vmodules_tdir) or {}
}

fn test_fmt_imports() {
	mod_tdir := os.join_path(vmodules_tdir, @FN)
	os.mkdir_all(mod_tdir)!
	tfile_content := [
		'import x.json2 as json',
		'import datatypes { Stack }',
		'',
		'const foo = Stack[string]{}',
		'',
	].join_lines()
	os.write_file(os.join_path(mod_tdir, 'main.v'), tfile_content)!
	os.execute_opt('${vexe} fmt -c ${mod_tdir}') or { assert false, err.msg() }
}

fn test_fmt_submod_type_alias() {
	mod_tdir := os.join_path(vmodules_tdir, @FN)
	mod_src_tdir := os.join_path(mod_tdir, 'src')
	submod_tdir := os.join_path(mod_tdir, 'bar', 'baz')
	os.mkdir_all(mod_src_tdir)!
	os.mkdir_all(submod_tdir)!
	tfile_content := [
		'module ${@FN}',
		'',
		'import bar.baz',
		'',
		'type MyAlias = baz.Baz',
		'',
	].join_lines()
	submod_tfile_content := [
		'module baz',
		'',
		'enum BarBaz {',
		'	bar',
		'	baz',
		'}',
		'',
	].join_lines()
	os.write_file(os.join_path(mod_src_tdir, 'foo.v'), tfile_content)!
	os.write_file(os.join_path(submod_tdir, 'baz.v'), submod_tfile_content)!
	os.execute_opt('${vexe} fmt -c ${mod_tdir}') or { assert false, err.msg() }
}
