import os

const freestanding_vexe = @VEXE
const freestanding_tests_dir = os.dir(@FILE)
const freestanding_v3_dir = os.dir(freestanding_tests_dir)
const freestanding_vlib_dir = os.dir(freestanding_v3_dir)
const freestanding_v3_src = os.join_path(freestanding_v3_dir, 'v.v')

fn freestanding_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_freestanding_preamble_codegen_test')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${freestanding_vexe} -gc none -path "${freestanding_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${freestanding_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn freestanding_generate_c(v3_bin string, name string, target_os string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	c_path := os.join_path(os.temp_dir(), 'v3_${name}.c')
	os.write_file(src, source) or { panic(err) }
	generate := os.execute('${v3_bin} -os ${target_os} -nofloat -gc none -o ${c_path} ${src}')
	assert generate.exit_code == 0, generate.output
	return os.read_file(c_path) or { panic(err) }
}

// A kernel compiles with -nostdinc against its own freestanding headers, and its
// own `#include`s name those. Letting them select the host libc preamble put the
// POSIX networking headers into a translation unit that has none of them.
fn test_freestanding_target_does_not_include_host_libc_headers() {
	v3_bin := freestanding_build_v3()
	source := 'module main\n\n#include <symbols.h>\n\nfn main() {\n}\n'
	c_code := freestanding_generate_c(v3_bin, 'freestanding_no_posix', 'vinix', source)
	for header in ['<sys/un.h>', '<netdb.h>', '<arpa/inet.h>', '<sys/socket.h>', '<termios.h>',
		'<dirent.h>', '<pthread.h>'] {
		assert !c_code.contains('#include ${header}'), '${header} reached a freestanding target:\n${c_code#[0..2000]}'
	}
	// The project's own header is still emitted; only the host set is dropped.
	assert c_code.contains('#include <symbols.h>'), c_code#[0..2000]
}

// The host targets keep the preamble they had.
fn test_hosted_target_still_includes_host_libc_headers() {
	v3_bin := freestanding_build_v3()
	source := 'module main\n\n#include <stdio.h>\n\nfn main() {\n}\n'
	c_code := freestanding_generate_c(v3_bin, 'hosted_keeps_posix', 'linux', source)
	assert c_code.contains('#include <sys/un.h>'), c_code#[0..2000]
}
