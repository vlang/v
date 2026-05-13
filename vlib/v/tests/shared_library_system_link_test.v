import os
import rand

const vexe = @VEXE

fn test_shared_library_links_with_system_cc() {
	if os.user_os() != 'linux' {
		return
	}
	workdir := os.join_path(os.vtmp_dir(), 'v_shared_link_${rand.ulid()}')
	os.mkdir_all(workdir) or { panic(err) }
	defer {
		os.rmdir_all(workdir) or {}
	}
	lib_src := os.join_path(workdir, 'libfoo.v')
	lib_out := os.join_path(workdir, 'libfoo')
	lib_so := '${lib_out}.so'
	host_src := os.join_path(workdir, 'host.c')
	host_bin := os.join_path(workdir, 'host')
	os.write_file(lib_src, [
		'module libfoo',
		'',
		'pub fn square(x int) int {',
		'\treturn x * x',
		'}',
	].join('\n')) or { panic(err) }
	os.write_file(host_src, [
		'#include <stdio.h>',
		'',
		'int libfoo__square(int);',
		'',
		'int main(void) {',
		'\tprintf("%d\\n", libfoo__square(2));',
		'\treturn 0;',
		'}',
	].join('\n')) or { panic(err) }
	run_cmd('${os.quoted_path(vexe)} -shared -o ${os.quoted_path(lib_out)} ${os.quoted_path(lib_src)}') or {
		panic(err)
	}
	assert os.exists(lib_so)
	run_cmd('cc ${os.quoted_path(host_src)} -L${os.quoted_path(workdir)} -lfoo -o ${os.quoted_path(host_bin)}') or {
		panic(err)
	}
	res := run_cmd('LD_LIBRARY_PATH=${os.quoted_path(workdir)} ${os.quoted_path(host_bin)}') or {
		panic(err)
	}
	assert res.output.trim_space() == '4'
}

fn run_cmd(cmd string) !os.Result {
	res := os.execute(cmd)
	if res.exit_code != 0 {
		return error('command failed:\n${cmd}\n${res.output}')
	}
	return res
}
