import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn test_iconv_include_uses_headerless_declarations() {
	$if windows {
		return
	}
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_headerless_iconv_test_${pid}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_headerless_iconv_${pid}.v')
	out := os.join_path(os.temp_dir(), 'v3_headerless_iconv_${pid}')
	os.write_file(src, 'module main

#include <iconv.h>
#flag darwin -liconv

fn C.iconv_open(tocode charptr, fromcode charptr) voidptr
fn C.iconv_close(cd voidptr) int

fn main() {
	to := "UTF-8"
	from := "UTF-8"
	cd := C.iconv_open(charptr(to.str), charptr(from.str))
	if isize(cd) != -1 {
		C.iconv_close(cd)
	}
	println("iconv-ok")
}
') or {
		panic(err)
	}
	os.rm(out) or {}
	os.rm(out + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	c_code := os.read_file(out + '.c') or { panic(err) }
	assert !c_code.contains('#include <iconv.h>'), c_code

	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'iconv-ok'
}
