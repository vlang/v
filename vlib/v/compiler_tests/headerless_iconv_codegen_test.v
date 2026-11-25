import os
import v.tempname

// C headers are authoritative since V3 stopped scanning them: the system include
// is kept, and the V declarations of its functions do not emit C prototypes.
fn test_iconv_system_include_is_kept_and_its_functions_are_not_redeclared() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_headerless_iconv_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_headerless_iconv.v')
	out := os.join_path(root, 'v3_headerless_iconv')
	os.write_file(src, 'module main

#include <iconv.h>
#flag darwin -liconv

fn C.iconv_open(tocode charptr, fromcode charptr) voidptr
fn C.iconv_close(cd voidptr) int

fn main() {
	to := "UTF-8"
	from := "UTF-8"
	cd := C.iconv_open(charptr(to.str), charptr(from.str))
	if int(cd) != -1 {
		C.iconv_close(cd)
	}
	println("iconv-ok")
}
') or {
		panic(err)
	}
	compile := os.exec([@VEXE, '${src}', '-b', 'c', '-o', '${out}'])
	assert compile.exit_code == 0, compile.output
	c_code := os.read_file(out + '.c') or { panic(err) }
	assert c_code.contains('#include <iconv.h>'), c_code
	assert !c_code.contains('iconv_open(char*'), c_code
	assert !c_code.contains('iconv_close(void*'), c_code

	run := os.exec([out])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'iconv-ok'
}
