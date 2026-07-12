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
typedef unsigned long NativeULong;
typedef unsigned long NativeWindow;
#endif
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

#include "@DIR/native_abi.h"

pub type Atom = C.NativeAtom
pub type KeySym = C.NativeKeySym
pub type NativeULong = C.NativeULong
pub type Window = C.NativeWindow
pub type LocalId = u64

@[typedef]
struct C.Display {}

@[typedef]
struct C.XKeyEvent {}

fn C.XLookupString(event &C.XKeyEvent, buf &char, buf_len int, keysym &KeySym, status voidptr) int
fn C.XSetWMProtocols(display &C.Display, window Window, protocols &Atom, count int) int
fn C.XGetWindowProperty(display &C.Display, window Window, property Atom, offset i64, length i64, delete int, req_type Atom, actual &Atom, actual_format &int, count &NativeULong, bytes &NativeULong, prop &&u8) int

fn helper(display &C.Display, window Window, value &&u8) usize {
	mut actual_type := Atom(0)
	mut item_count := NativeULong(0)
	mut bytes_after := NativeULong(0)
	mut actual_format := 0
	property := Atom(0)
	C.XGetWindowProperty(display, window, property, 0, 1, 0, property, &actual_type,
		&actual_format, &item_count, &bytes_after, value)
	return usize(item_count)
}

fn main() {
	display := &C.Display(unsafe { nil })
	mut event := C.XKeyEvent{}
	mut keysym := KeySym(0)
	C.XLookupString(&event, nil, 0, &keysym, nil)
	mut protocols := [1]Atom{}
	protocols[0] = Atom(1)
	window := Window(0)
	C.XSetWMProtocols(display, window, &protocols[0], 1)
	mut value := &u8(unsafe { nil })
	count := helper(display, window, &&u8(&value))
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
	assert compact.contains('NativeULongitem_count'), c_code
	assert compact.contains('NativeULongbytes_after'), c_code
	assert compact.contains('XLookupString(&event,NULL,0,&keysym,NULL);'), c_code
	assert compact.contains('XSetWMProtocols(display,window,&protocols[0],1);'), c_code
	assert compact.contains('XGetWindowProperty(display,window,property,0,1,0,property,&actual_type,&actual_format,&item_count,&bytes_after,value);'), c_code

	assert !compact.contains('u64keysym'), c_code
	assert !compact.contains('u64protocols[1]'), c_code
	assert !compact.contains('u64actual_type'), c_code
	assert !compact.contains('u64item_count'), c_code
	assert !compact.contains('u64bytes_after'), c_code
	assert !compact.contains('0&(u8)(value)'), c_code
	assert compact.contains('u64local_id'), c_code
	assert !compact.contains('LocalIdlocal_id'), c_code
}
