import os

const c_pointer_null_vexe = @VEXE
const c_pointer_null_tests_dir = os.dir(@FILE)
const c_pointer_null_v3_dir = os.dir(c_pointer_null_tests_dir)
const c_pointer_null_vlib_dir = os.dir(c_pointer_null_v3_dir)
const c_pointer_null_v3_src = os.join_path(c_pointer_null_v3_dir, 'v3.v')

fn c_pointer_null_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_c_pointer_null_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${c_pointer_null_vexe} -gc none -path "${c_pointer_null_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${c_pointer_null_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn c_pointer_null_write_source() string {
	src := os.join_path(os.temp_dir(), 'v3_c_pointer_null_${os.getpid()}.v')
	os.write_file(src, 'module main

fn C.take_charptr(ptr &char)
fn C.take_charptrptr(ptr &&char)
fn C.take_voidptr(ptr voidptr)

fn main() {
	C.take_charptrptr(nil)
	C.take_charptr(&char(0))
	C.take_voidptr(C.NULL)
}
	') or {
		panic(err)
	}
	return src
}

fn test_nil_for_c_pointer_to_pointer_arg_emits_null() {
	v3_bin := c_pointer_null_build_v3()
	src := c_pointer_null_write_source()
	c_path := src + '.c'
	compile := os.execute('${v3_bin} ${src} -b c -o ${c_path}')
	assert compile.exit_code == 0, compile.output

	c_code := os.read_file(c_path) or { panic(err) }
	compact := c_code.replace('\t', '').replace(' ', '').replace('\n', '')
	assert compact.contains('take_charptrptr(NULL);'), c_code
	assert !compact.contains('take_charptrptr((char*)(0));'), c_code
	assert compact.contains('take_charptr((char*)(0));'), c_code
	assert compact.contains('take_voidptr(NULL);'), c_code
	assert !compact.contains('&NULL'), c_code
}
