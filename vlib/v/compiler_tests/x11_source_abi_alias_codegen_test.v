import os
import v.tempname

fn x11_alias_write_project(root string) {
	os.write_file(os.join_path(root, 'native_abi.h'), '#ifndef V3_X11_ALIAS_NATIVE_ABI_H
#define V3_X11_ALIAS_NATIVE_ABI_H
typedef unsigned long NativeAtom;
typedef unsigned long NativeKeySym;
typedef unsigned long NativeULong;
typedef unsigned long NativeWindow;
typedef long NativeTime;
static inline void take_time(NativeTime* value) { (void)value; }
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
type C.NativeTime = i64
pub type LocalId = u64

@[typedef]
struct C.Display {}

@[typedef]
struct C.XKeyEvent {}

fn C.XLookupString(event &C.XKeyEvent, buf &char, buf_len int, keysym &KeySym, status voidptr) int
fn C.XSetWMProtocols(display &C.Display, window Window, protocols &Atom, count int) int
fn C.XGetWindowProperty(display &C.Display, window Window, property Atom, offset i64, length i64, delete int, req_type Atom, actual &Atom, actual_format &int, count &NativeULong, bytes &NativeULong, prop &&u8) int
fn C.take_time(value &C.NativeTime)

fn helper(display &C.Display, window Window, value &&u8) uint {
	mut actual_type := Atom(0)
	mut item_count := NativeULong(0)
	mut bytes_after := NativeULong(0)
	mut actual_format := 0
	property := Atom(0)
	C.XGetWindowProperty(display, window, property, 0, 1, 0, property, &actual_type,
		&actual_format, &item_count, &bytes_after, value)
	return uint(item_count)
}

fn main() {
	display := &C.Display(unsafe { nil })
	mut event := C.XKeyEvent{}
	mut keysym := KeySym(0)
	C.XLookupString(&event, nil, 0, &keysym, nil)
	mut protocols := [1]Atom{}
	protocols[0] = Atom(1)
	window := Window(0)
	C.XSetWMProtocols(display, window, unsafe { &protocols[0] }, 1)
	mut value := &u8(unsafe { nil })
	count := helper(display, window, &&u8(&value))
	mut local_id := LocalId(9)
	mut native_time := i64(0)
	C.take_time(voidptr(&native_time))
	score := int(count) + int(local_id)
	println(int_str(score))
}
') or {
		panic(err)
	}
}

fn test_c_typedef_aliases_are_preserved_for_x11_abi_shapes() {
	root := os.join_path(os.vtmp_dir(), 'v3_x11_alias_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	x11_alias_write_project(root)
	c_path := os.join_path(root, 'out.c')
	compile := os.exec([@VEXE, root, '-b', 'c', '-o', c_path])
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
	// `actual_format` is a V `int` (i64) passed as a C `int *`, so it goes through a C `int`
	// temporary that is copied back after the call (25f5ced70b, int -> i64).
	assert compact.contains('i64*_cabi_out_addr_0=&actual_format;int_cabi_out_val_0=(int)(*_cabi_out_addr_0);'), c_code
	assert compact.contains('XGetWindowProperty(display,window,property,0,1,0,property,&actual_type,&_cabi_out_val_0,&item_count,&bytes_after,value);'), c_code
	// A `voidptr` argument converts implicitly to the `NativeTime *` of the C prototype.
	assert compact.contains('take_time((void*)(&native_time));'), c_code
	assert !compact.contains('take_time((i64*)'), c_code

	assert !compact.contains('u64keysym'), c_code
	assert !compact.contains('u64protocols[1]'), c_code
	assert !compact.contains('u64actual_type'), c_code
	assert !compact.contains('u64item_count'), c_code
	assert !compact.contains('u64bytes_after'), c_code
	assert !compact.contains('0&(u8)(value)'), c_code
	assert compact.contains('u64local_id'), c_code
	assert !compact.contains('LocalIdlocal_id'), c_code
}
