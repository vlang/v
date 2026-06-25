import os

const x11_alias_vexe = @VEXE
const x11_alias_tests_dir = os.dir(@FILE)
const x11_alias_v3_dir = os.dir(x11_alias_tests_dir)
const x11_alias_vlib_dir = os.dir(x11_alias_v3_dir)
const x11_alias_v3_src = os.join_path(x11_alias_v3_dir, 'v3.v')

fn x11_alias_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_x11_alias_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${x11_alias_vexe} -gc none -path "${x11_alias_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${x11_alias_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn x11_alias_write_project() string {
	root := os.join_path(os.temp_dir(), 'v3_x11_alias_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'native_abi.h'), '#ifndef V3_X11_ALIAS_NATIVE_ABI_H
#define V3_X11_ALIAS_NATIVE_ABI_H
typedef unsigned long NativeAtom;
typedef unsigned long NativeKeySym;
typedef unsigned long NativeWindow;
#endif
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

#include "@DIR/native_abi.h"

pub type Atom = C.NativeAtom
pub type KeySym = C.NativeKeySym
pub type Window = C.NativeWindow
pub type LocalId = u64

fn C.take_key(key &KeySym)
fn C.take_atom(atom &Atom)
fn C.take_window(window Window)
fn C.read_prop(window Window, property Atom, actual &Atom, count &usize, bytes &usize, prop &&u8)

fn helper(value &&u8) usize {
	mut actual_type := Atom(0)
	mut item_count := usize(0)
	mut bytes_after := usize(0)
	window := Window(0)
	property := Atom(0)
	C.read_prop(window, property, &actual_type, &item_count, &bytes_after, value)
	return item_count
}

fn main() {
	mut keysym := KeySym(0)
	C.take_key(&keysym)
	mut protocols := [1]Atom{}
	protocols[0] = Atom(1)
	C.take_atom(&protocols[0])
	window := Window(0)
	C.take_window(window)
	mut value := &u8(unsafe { nil })
	count := helper(&&u8(&value))
	mut local_id := LocalId(9)
	score := int(count) + int(local_id)
	println(int_str(score))
}
') or {
		panic(err)
	}
	return root
}

fn test_c_typedef_aliases_are_preserved_for_x11_abi_shapes() {
	v3_bin := x11_alias_build_v3()
	root := x11_alias_write_project()
	c_path := os.join_path(root, 'out.c')
	compile := os.execute('${v3_bin} ${root} -b c -o ${c_path}')
	assert compile.exit_code == 0, compile.output

	c_code := os.read_file(c_path) or { panic(err) }
	compact := c_code.replace('\t', '').replace(' ', '').replace('\n', '')
	assert compact.contains('NativeKeySymkeysym'), c_code
	assert compact.contains('NativeAtomprotocols[1]'), c_code
	assert compact.contains('NativeWindowwindow'), c_code
	assert compact.contains('NativeAtomactual_type'), c_code
	assert compact.contains('size_titem_count'), c_code
	assert compact.contains('size_tbytes_after'), c_code
	assert compact.contains('read_prop(window,property,&actual_type,&item_count,&bytes_after,value);'), c_code

	assert !compact.contains('u64keysym'), c_code
	assert !compact.contains('u64protocols[1]'), c_code
	assert !compact.contains('u64actual_type'), c_code
	assert !compact.contains('0&(u8)(value)'), c_code
	assert compact.contains('u64local_id'), c_code
	assert !compact.contains('LocalIdlocal_id'), c_code
}
