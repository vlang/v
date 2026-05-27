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
		"@[export: 'libfoo_square']",
		'pub fn square(x int) int {',
		'\treturn x * x',
		'}',
	].join('\n')) or { panic(err) }
	os.write_file(host_src, [
		'#include <stdio.h>',
		'',
		'int libfoo_square(int);',
		'',
		'int main(void) {',
		'\tprintf("%d\\n", libfoo_square(2));',
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

fn test_shared_library_only_exports_tagged_symbols() {
	// Regression test for vlang/v#27167: `v -shared` used to export every
	// V runtime/stdlib symbol, not just functions tagged with `@[export: '…']`.
	uos := os.user_os()
	if uos !in ['linux', 'macos'] {
		return
	}
	// tcc does not honor `-fvisibility=hidden`, so the symbol-set check would be
	// noisy there; use a system compiler for Linux and clang on macOS.
	cc_flag := if uos == 'macos' { '-cc clang' } else { '-cc cc' }
	workdir := os.join_path(os.vtmp_dir(), 'v_shared_visibility_${rand.ulid()}')
	os.mkdir_all(workdir) or { panic(err) }
	defer {
		os.rmdir_all(workdir) or {}
	}
	lib_src := os.join_path(workdir, 'lib.v')
	lib_out := os.join_path(workdir, 'liblib')
	lib_so := if uos == 'macos' { '${lib_out}.dylib' } else { '${lib_out}.so' }
	os.write_file(lib_src, [
		'module main',
		'',
		"@[export: 'my_add']",
		'fn my_add(a int, b int) int {',
		'\treturn a + b',
		'}',
	].join('\n')) or { panic(err) }
	run_cmd('${os.quoted_path(vexe)} ${cc_flag} -shared -o ${os.quoted_path(lib_out)} ${os.quoted_path(lib_src)}') or {
		panic(err)
	}
	assert os.exists(lib_so)
	nm_flag := if uos == 'macos' { '-gU' } else { '-D --defined-only' }
	res := run_cmd('nm ${nm_flag} ${os.quoted_path(lib_so)}') or { panic(err) }
	symbol_token := if uos == 'macos' { ' _my_add' } else { ' my_add' }
	assert res.output.contains(symbol_token), 'expected exported `my_add` in:\n${res.output}'
	// None of the V runtime/stdlib helpers should leak into the exported ABI.
	leaky_prefixes := ['Array_string_', 'main__', 'builtin__', 'GC_', '_vinit', '_vcleanup',
		'_const_']
	for line in res.output.split('\n') {
		for prefix in leaky_prefixes {
			needles := if uos == 'macos' { [' _${prefix}', ' T _${prefix}', ' D _${prefix}'] } else { [
					' ${prefix}',
					' T ${prefix}',
					' D ${prefix}',
				] }
			for needle in needles {
				assert !line.contains(needle), 'unexpected exported symbol on line: ${line}'
			}
		}
	}
}

fn run_cmd(cmd string) !os.Result {
	res := os.execute(cmd)
	if res.exit_code != 0 {
		return error('command failed:\n${cmd}\n${res.output}')
	}
	return res
}
