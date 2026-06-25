import os

const wait_header_vexe = @VEXE
const wait_header_tests_dir = os.dir(@FILE)
const wait_header_v3_dir = os.dir(wait_header_tests_dir)
const wait_header_vlib_dir = os.dir(wait_header_v3_dir)
const wait_header_v3_src = os.join_path(wait_header_v3_dir, 'v3.v')

struct WaitHeaderProgram {
	c_code string
	out    string
}

fn wait_header_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_wait_header_test_${pid}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${wait_header_vexe} -gc none -path "${wait_header_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${wait_header_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn wait_header_compile(v3_bin string, name string, source string) WaitHeaderProgram {
	pid := os.getpid()
	src := os.join_path(os.temp_dir(), 'v3_wait_header_${name}_${pid}.v')
	out := os.join_path(os.temp_dir(), 'v3_wait_header_${name}_${pid}')
	os.write_file(src, source) or { panic(err) }
	os.rm(out) or {}
	os.rm(out + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	return WaitHeaderProgram{
		c_code: os.read_file(out + '.c') or { panic(err) }
		out:    out
	}
}

fn test_os_import_emits_sys_wait_header() {
	$if windows {
		return
	}
	v3_bin := wait_header_build_v3()
	with_os := wait_header_compile(v3_bin, 'with_os_execute', 'module main

import os

fn main() {
	result := os.execute(\'/bin/sh -c "printf waitpid-ok"\')
	assert result.exit_code == 0
	println(result.output)
}
')
	assert with_os.c_code.contains('#include <sys/wait.h>'), with_os.c_code
	assert with_os.c_code.contains('waitpid('), with_os.c_code
	run := os.execute(with_os.out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'waitpid-ok', run.output

	hello := wait_header_compile(v3_bin, 'hello', "module main

fn main() {
	println('hello')
}
")
	assert !hello.c_code.contains('#include <sys/wait.h>'), hello.c_code
}
