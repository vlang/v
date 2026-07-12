import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')
const hello_src = os.join_path(tests_dir, 'hello.v')

fn has_non_runtime_include(c_code string) bool {
	for line in c_code.split_into_lines() {
		trimmed := line.trim_space()
		if trimmed.starts_with('#include') {
			return true
		}
	}
	return false
}

// test_prod_flag_before_input_uses_optimized_c_compile validates this v3 regression case.
fn test_prod_flag_before_input_uses_optimized_c_compile() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_prod_flag_test')
	build := os.execute('${vexe} -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	out_bin := os.join_path(os.temp_dir(), 'v3_prod_hello')
	compile := os.execute('${v3_bin} -prod ${hello_src} -o ${out_bin}')
	assert compile.exit_code == 0, compile.output
	assert compile.output.contains('cc -std=gnu11 -O2')
	assert !compile.output.contains('tcc.exe')

	run := os.execute(out_bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'hello world'
}

// test_c99_flag_uses_c99_c_compile_mode validates this v3 regression case.
fn test_c99_flag_uses_c99_c_compile_mode() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_c99_flag_test')
	build := os.execute('${vexe} -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	out_bin := os.join_path(os.temp_dir(), 'v3_c99_hello')
	compile := os.execute('${v3_bin} -prod -c99 ${hello_src} -o ${out_bin}')
	assert compile.exit_code == 0, compile.output
	assert compile.output.contains('cc -std=c99 -O2')
	assert !compile.output.contains('cc -std=gnu11')
	assert !compile.output.contains('tcc.exe')

	run := os.execute(out_bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'hello world'
}

// test_c99_flag_emits_linux_feature_macros_in_headerless_preamble validates this v3 regression case.
fn test_c99_flag_emits_linux_feature_macros_in_headerless_preamble() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_c99_feature_macro_test')
	build := os.execute('${vexe} -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	out_c := os.join_path(os.temp_dir(), 'v3_c99_feature_macro_hello.c')
	compile := os.execute('${v3_bin} -c99 ${hello_src} -o ${out_c}')
	assert compile.exit_code == 0, compile.output

	c_code := os.read_file(out_c) or { panic(err) }
	gnu_idx := c_code.index('#define _GNU_SOURCE') or { -1 }
	posix_idx := c_code.index('#define _POSIX_C_SOURCE 200809L') or { -1 }
	typedef_idx := c_code.index('typedef signed char i8;') or { -1 }
	assert gnu_idx >= 0, c_code[..200]
	assert posix_idx >= 0, c_code[..200]
	assert typedef_idx >= 0, c_code[..200]
	assert gnu_idx < typedef_idx, c_code[..200]
	assert posix_idx < typedef_idx, c_code[..200]
	assert !has_non_runtime_include(c_code), c_code[..200]
}

// test_c99_flag_uses_headerless_stdatomic_fallback validates this v3 regression case.
fn test_c99_flag_uses_headerless_stdatomic_fallback() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_c99_stdatomic_test')
	build := os.execute('${vexe} -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_c99_stdatomic_swap.v')
	os.write_file(src,
		"import sync.stdatomic\n\nfn main() {\n\tmut b := u8(1)\n\told_b_add := C.atomic_fetch_add_byte(voidptr(&b), u8(2))\n\tassert old_b_add == u8(1)\n\tassert b == u8(3)\n\told_b_sub := C.atomic_fetch_sub_byte(voidptr(&b), u8(1))\n\tassert old_b_sub == u8(3)\n\tassert b == u8(2)\n\n\tmut h := u16(10)\n\told_h_add := C.atomic_fetch_add_u16(voidptr(&h), u16(4))\n\tassert old_h_add == u16(10)\n\told_h_sub := C.atomic_fetch_sub_u16(voidptr(&h), u16(3))\n\tassert old_h_sub == u16(14)\n\tassert h == u16(11)\n\n\tmut w := u32(20)\n\told_w_sub := C.atomic_fetch_sub_u32(voidptr(&w), u32(5))\n\tassert old_w_sub == u32(20)\n\tassert w == u32(15)\n\n\tmut x := u64(1)\n\told := C.atomic_exchange_u64(voidptr(&x), u64(7))\n\tassert old == u64(1)\n\tassert x == u64(7)\n\tmut expected := u64(7)\n\tassert C.atomic_compare_exchange_strong_u64(voidptr(&x), &expected, u64(9))\n\tassert x == u64(9)\n\tprintln('atomic helpers ok')\n}\n") or {
		panic(err)
	}

	out_bin := os.join_path(os.temp_dir(), 'v3_c99_stdatomic_swap')
	compile := os.execute('${v3_bin} -c99 ${src} -o ${out_bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(out_bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'atomic helpers ok'
}

// test_c99_flag_system_stdatomic_include_is_headerless validates this v3 regression case.
fn test_c99_flag_system_stdatomic_include_is_headerless() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_c99_system_stdatomic_test')
	build := os.execute('${vexe} -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_c99_system_stdatomic.v')
	os.write_file(src,
		"#include <stdatomic.h>\n\nfn main() {\n\tprintln('system stdatomic ok')\n}\n") or {
		panic(err)
	}

	out_c := os.join_path(os.temp_dir(), 'v3_c99_system_stdatomic.c')
	gen_c := os.execute('${v3_bin} -c99 ${src} -o ${out_c}')
	assert gen_c.exit_code == 0, gen_c.output
	c_code := os.read_file(out_c) or { panic(err) }
	assert !has_non_runtime_include(c_code), c_code
	assert !c_code.contains('typedef volatile uintptr_t atomic_uintptr_t;'), c_code
	assert c_code.contains('typedef void* atomic_uintptr_t;'), c_code
	assert c_code.contains('static inline byte atomic_fetch_add_byte'), c_code
	assert c_code.contains('static inline u16 atomic_fetch_add_u16'), c_code
	assert c_code.contains('static inline u64 atomic_fetch_add_u64'), c_code
	assert c_code.contains('static inline u16 atomic_fetch_sub_u16'), c_code
	assert c_code.contains('static inline u32 atomic_fetch_sub_u32'), c_code
	assert c_code.contains('static inline u64 atomic_exchange_u64'), c_code
	assert c_code.contains('static inline bool atomic_compare_exchange_strong_u64'), c_code

	out_bin := os.join_path(os.temp_dir(), 'v3_c99_system_stdatomic')
	compile := os.execute('${v3_bin} -prod -c99 ${src} -o ${out_bin}')
	assert compile.exit_code == 0, compile.output

	run := os.execute(out_bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'system stdatomic ok'

	gnu_bin := os.join_path(os.temp_dir(), 'v3_gnu11_system_stdatomic')
	gnu_compile := os.execute('${v3_bin} -prod ${src} -o ${gnu_bin}')
	assert gnu_compile.exit_code == 0, gnu_compile.output

	gnu_run := os.execute(gnu_bin)
	assert gnu_run.exit_code == 0, gnu_run.output
	assert gnu_run.output.trim_space() == 'system stdatomic ok'
}
