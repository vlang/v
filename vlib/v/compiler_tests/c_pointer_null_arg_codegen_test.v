import os
import v.tempname

fn c_pointer_null_write_source(root string) string {
	src := os.join_path(root, 'v3_c_pointer_null.v')
	os.write_file(src, 'module main

fn C.take_charptr(ptr &char)
fn C.take_charptrptr(ptr &&char)
fn C.take_voidptr(ptr voidptr)

fn main() {
	C.take_charptrptr(unsafe { nil })
	C.take_charptr(unsafe { nil })
	C.take_voidptr(unsafe { nil })
}
	') or {
		panic(err)
	}
	return src
}

fn test_nil_for_c_pointer_to_pointer_arg_emits_null() {
	root := os.join_path(os.vtmp_dir(), 'v3_c_pointer_null_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := c_pointer_null_write_source(root)
	c_path := src + '.c'
	compile := os.exec([@VEXE, '${src}', '-b', 'c', '-o', c_path])
	assert compile.exit_code == 0, compile.output

	c_code := os.read_file(c_path) or { panic(err) }
	compact := c_code.replace('\t', '').replace(' ', '').replace('\n', '')
	assert compact.contains('take_charptrptr(NULL);'), c_code
	assert !compact.contains('take_charptrptr((char*)(0));'), c_code
	assert compact.contains('take_charptr(NULL);'), c_code
	assert compact.contains('take_voidptr(NULL);'), c_code
	assert !compact.contains('&NULL'), c_code
}
