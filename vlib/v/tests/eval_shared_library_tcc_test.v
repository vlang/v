import dl
import os
import rand

const vexe = @VEXE

fn test_eval_create_inside_tcc_shared_library_works() {
	if os.user_os() != 'linux' {
		return
	}
	workdir := os.join_path(os.vtmp_dir(), 'v_eval_shared_tcc_${rand.ulid()}')
	os.mkdir_all(workdir) or { panic(err) }
	defer {
		os.rmdir_all(workdir) or {}
	}
	lib_src := os.join_path(workdir, 'library.v')
	host_src := os.join_path(workdir, 'host.v')
	lib_bin := os.join_path(workdir, 'library')
	lib_so := os.join_path(workdir, 'library${dl.dl_ext}')
	lib_path := lib_so.replace('\\', '\\\\')
	library_code := [
		'module library',
		'',
		'import v.eval',
		'',
		"@[export: 'add_1']",
		'pub fn add_1(x int, y int) int {',
		'\tmut e := eval.create()',
		'\te.run("println(\'fromlib\')") or { panic(err) }',
		'\treturn 2',
		'}',
	].join('\n')
	host_code := [
		'module main',
		'',
		'import dl.loader',
		'import v.eval',
		'',
		'type FNAdder = fn (int, int) int',
		'',
		"const lib_path = '${lib_path}'",
		'',
		'fn main() {',
		'\tmut dl_loader := loader.get_or_create_dynamic_lib_loader(',
		'\t\tkey:   lib_path',
		'\t\tpaths: [lib_path]',
		'\t) or { panic(err) }',
		'\tdefer {',
		'\t\tdl_loader.unregister()',
		'\t}',
		"\tsym := dl_loader.get_sym('add_1') or { panic(err) }",
		'\tf := FNAdder(sym)',
		'\tres := f(1, 2)',
		"\tprintln('res: ' + res.str())",
		'\tmut e := eval.create()',
		'\te.run("println(\'hi\')") or { panic(err) }',
		'}',
	].join('\n')
	os.write_file(lib_src, library_code) or { panic(err) }
	os.write_file(host_src, host_code) or { panic(err) }
	compile_lib_cmd := '${os.quoted_path(vexe)} -cc tcc -d no_backtrace -shared -o ${os.quoted_path(lib_bin)} ${os.quoted_path(lib_src)}'
	run_cmd(compile_lib_cmd) or { panic(err) }
	run_host_cmd := '${os.quoted_path(vexe)} -cc tcc -d no_backtrace run ${os.quoted_path(host_src)}'
	res := run_cmd(run_host_cmd) or { panic(err) }
	assert res.output.contains('fromlib')
	assert res.output.contains('res: 2')
	assert res.output.contains('hi')
}

fn run_cmd(cmd string) !os.Result {
	res := os.execute(cmd)
	if res.exit_code != 0 {
		return error('command failed:\n${cmd}\n${res.output}')
	}
	return res
}
