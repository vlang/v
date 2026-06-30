import os

const c_anon_param_vexe = @VEXE
const c_anon_param_tests_dir = os.dir(@FILE)
const c_anon_param_v3_dir = os.dir(c_anon_param_tests_dir)
const c_anon_param_vlib_dir = os.dir(c_anon_param_v3_dir)
const c_anon_param_v3_src = os.join_path(c_anon_param_v3_dir, 'v3.v')

fn c_anon_param_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_c_anon_param_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${c_anon_param_vexe} -gc none -path "${c_anon_param_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${c_anon_param_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_c_anonymous_params_keep_pointer_call_shape() {
	v3_bin := c_anon_param_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_c_anon_param_${os.getpid()}.v')
	os.write_file(src, 'module main

@[typedef]
struct C.AnonNative {
	value int
}

fn C.take_ptr(&C.AnonNative) int
fn C.take_named(stream &C.AnonNative) int
fn C.take_void(voidptr) int
fn C.take_primitive(int, u64) int
fn C.take_multi(&&C.AnonNative) int
fn C.take_mixed(&C.AnonNative, voidptr, int, &&C.AnonNative) int
fn C.take_fn(fn (&C.AnonNative) int) int
fn C.XLookupString(event &C.AnonNative) int
fn C.SSL_new() voidptr
fn C.load_matrix(m [16]f32) int
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
	c_path := os.join_path(os.temp_dir(), 'v3_c_anon_param_${os.getpid()}.c')
	os.rm(c_path) or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${c_path}')
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
