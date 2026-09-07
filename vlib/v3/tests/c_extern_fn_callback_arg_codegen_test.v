import os

const extern_cb_vexe = @VEXE
const extern_cb_tests_dir = os.dir(@FILE)
const extern_cb_v3_dir = os.dir(extern_cb_tests_dir)
const extern_cb_vlib_dir = os.dir(extern_cb_v3_dir)
const extern_cb_v3_src = os.join_path(extern_cb_v3_dir, 'v3.v')

fn extern_cb_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_extern_callback_arg_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${extern_cb_vexe} -gc none -path "${extern_cb_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${extern_cb_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// extern_cb_write_source mirrors the mbedtls shape that broke: `mbedtls_net_send`
// is handed to `mbedtls_ssl_set_bio`, whose callback slot takes a
// `const unsigned char *` buffer. A `fn C.` line cannot spell that `const`, so
// casting the argument to the V-declared signature is what breaks the call.
fn extern_cb_write_source() string {
	src := os.join_path(os.temp_dir(), 'v3_extern_callback_arg_${os.getpid()}.v')
	os.write_file(src, 'module main

fn C.native_send(voidptr, &u8, usize) i32
fn C.native_register(fn (voidptr, &u8, usize) i32) i32

fn v_send(ctx voidptr, buf &u8, len usize) i32 {
	return i32(len) + i32(unsafe { buf[0] }) + i32(ctx != unsafe { nil })
}

fn main() {
	println(C.native_register(C.native_send))
	println(C.native_register((C.native_send)))
	println(C.native_register(voidptr(C.native_send)))
	println(C.native_register(v_send))
}
') or { panic(err) }
	return src
}

fn test_c_extern_fn_callback_arg_is_not_cast() {
	v3_bin := extern_cb_build_v3()
	src := extern_cb_write_source()
	c_path := src + '.c'
	defer {
		os.rm(v3_bin) or {}
		os.rm(src) or {}
		os.rm(c_path) or {}
	}
	compile := os.execute('${v3_bin} ${src} -b c -o ${c_path}')
	assert compile.exit_code == 0, compile.output

	c_code := os.read_file(c_path) or { panic(err) }
	compact := c_code.replace('\t', '').replace(' ', '').replace('\n', '')
	// A `C.` function goes into the callback slot by name: C already has its real
	// prototype, and a cast to the V-declared signature would drop the parameter's
	// `const` and trip -Wincompatible-pointer-types at the call.
	assert compact.contains('native_register(native_send)'), c_code
	assert compact.contains('native_register((native_send))'), c_code
	// An explicit conversion is not a bare C function reference. Preserve it as
	// the inner expression and cast that result to the callback parameter type.
	assert !compact.contains('native_register((void*)(native_send))'), c_code
	assert compact.contains(')((void*)(native_send)))'), c_code
	// A V function still needs the C-ABI cast: C has no declaration of its own for
	// it, so the fn-pointer typedef is what gives the argument a type.
	assert compact.count('native_register((_fn_ptr_') == 2, c_code
}
