import os
import v.tempname

fn test_c_anonymous_params_keep_pointer_call_shape() {
	root := os.join_path(os.vtmp_dir(), 'v3_c_anon_param_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_c_anon_param.v')
	os.write_file(src, 'module main

@[typedef]
struct C.AnonNative {
	value int
}

// No header declares these, so `@[c_extern]` asks for the generated prototypes
// whose parameter lowering this test checks.
@[c_extern]
fn C.take_ptr(&C.AnonNative) int

@[c_extern]
fn C.take_named(stream &C.AnonNative) int

@[c_extern]
fn C.take_void(voidptr) int

@[c_extern]
fn C.take_primitive(int, u64) int

@[c_extern]
fn C.take_multi(&&C.AnonNative) int

@[c_extern]
fn C.take_mixed(&C.AnonNative, voidptr, int, &&C.AnonNative) int

@[c_extern]
fn C.take_fn(fn (&C.AnonNative) int) int

@[c_extern]
fn C.XLookupString(event &C.AnonNative) int

@[c_extern]
fn C.SSL_new() voidptr

@[c_extern]
fn C.load_matrix(m [16]f32) int

@[c_extern]
fn C.width_runes(r []rune) int

fn callback(n &C.AnonNative) int {
	return n.value
}

fn main() {
	mut native := C.AnonNative{
		value: 3
	}
	ptr := &native
	pptr := &ptr
	_ := C.take_ptr(ptr)
	_ = C.take_ptr(&native)
	_ = C.take_named(ptr)
	_ = C.take_void(voidptr(ptr))
	_ = C.take_void(voidptr(&ptr))
	_ = C.take_primitive(1, u64(2))
	_ = C.take_multi(pptr)
	_ = C.take_mixed(ptr, voidptr(ptr), 3, pptr)
	_ = C.take_fn(callback)
	_ = C.XLookupString(ptr)
	ssl := C.SSL_new()
	if ssl == unsafe { nil } {
	}
	mut matrix := [16]f32{}
	_ = C.load_matrix(matrix)
	runes := []rune{}
	_ = C.width_runes(runes)
}
') or {
		panic(err)
	}
	c_path := os.join_path(root, 'v3_c_anon_param.c')
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', c_path])
	assert compile.exit_code == 0, compile.output
	generated := os.read_file(c_path) or { panic(err) }
	assert generated.contains('take_ptr(ptr)'), generated
	assert generated.contains('take_ptr(&native)'), generated
	assert generated.contains('take_named(ptr)'), generated
	assert generated.contains('take_void((void*)(ptr))'), generated
	assert generated.contains('take_void((void*)(&ptr))'), generated
	assert generated.contains('take_multi(pptr)'), generated
	assert generated.contains('take_mixed(ptr, (void*)(ptr), 3, pptr)'), generated
	fn_ptr_idx := generated.index('typedef int (*_fn_ptr_') or { -1 }
	take_fn_proto_idx := generated.index('int take_fn(_fn_ptr_') or { -1 }
	assert fn_ptr_idx >= 0, generated
	assert take_fn_proto_idx >= 0, generated
	assert fn_ptr_idx < take_fn_proto_idx, generated
	assert generated.contains('int XLookupString(AnonNative* event);'), generated
	assert generated.contains('void* SSL_new(void);'), generated
	assert generated.contains('load_matrix(matrix)'), generated
	assert generated.contains('width_runes(runes)'), generated
	assert !generated.contains('take_ptr(*'), generated
	assert !generated.contains('take_named(*'), generated
	assert !generated.contains('take_void(*'), generated
	assert !generated.contains('take_multi(*'), generated
	assert !generated.contains('take_mixed(*'), generated
	assert !generated.contains('load_matrix(*'), generated
	assert !generated.contains('width_runes(*'), generated
	assert !generated.contains('*&native'), generated
	assert !generated.contains('*(AnonNative*)'), generated
	assert !generated.contains('*(struct AnonNative*)'), generated
}
