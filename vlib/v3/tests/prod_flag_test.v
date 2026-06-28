import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')
const hello_src = os.join_path(tests_dir, 'hello.v')

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

// test_c99_flag_emits_linux_feature_macros_before_includes validates this v3 regression case.
fn test_c99_flag_emits_linux_feature_macros_before_includes() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_c99_feature_macro_test')
	build := os.execute('${vexe} -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	out_c := os.join_path(os.temp_dir(), 'v3_c99_feature_macro_hello.c')
	compile := os.execute('${v3_bin} -c99 ${hello_src} -o ${out_c}')
	assert compile.exit_code == 0, compile.output

	c_code := os.read_file(out_c) or { panic(err) }
	gnu_idx := c_code.index('#define _GNU_SOURCE') or { -1 }
	posix_idx := c_code.index('#define _POSIX_C_SOURCE 200809L') or { -1 }
	include_idx := c_code.index('#include <stdio.h>') or { -1 }
	assert gnu_idx >= 0, c_code[..200]
	assert posix_idx >= 0, c_code[..200]
	assert include_idx >= 0, c_code[..200]
	assert gnu_idx < include_idx, c_code[..200]
	assert posix_idx < include_idx, c_code[..200]
}

// test_c99_flag_preserves_stdatomic_header validates this v3 regression case.
fn test_c99_flag_preserves_stdatomic_header() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_c99_stdatomic_test')
	build := os.execute('${vexe} -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_c99_stdatomic_swap.v')
	os.write_file(src,
		"import sync.stdatomic\n\nfn main() {\n\tmut x := u64(1)\n\told := C.atomic_exchange_u64(voidptr(&x), u64(7))\n\tassert old == u64(1)\n\tassert x == u64(7)\n\tprintln('atomic exchange ok')\n}\n") or {
		panic(err)
	}

	out_bin := os.join_path(os.temp_dir(), 'v3_c99_stdatomic_swap')
	compile := os.execute('${v3_bin} -c99 ${src} -o ${out_bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(out_bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'atomic exchange ok'
}

// test_c99_flag_system_stdatomic_include_suppresses_fallback_typedef validates this v3 regression case.
fn test_c99_flag_system_stdatomic_include_suppresses_fallback_typedef() {
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
	assert c_code.contains('#include <stdatomic.h>'), c_code
	assert !c_code.contains('typedef volatile uintptr_t atomic_uintptr_t;'), c_code
	assert c_code.contains('static inline u64 atomic_fetch_add_u64'), c_code

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
