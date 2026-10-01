import os

const voidptr_arg_vexe = @VEXE
const voidptr_arg_tests_dir = os.dir(@FILE)
const voidptr_arg_v3_dir = os.dir(voidptr_arg_tests_dir)
const voidptr_arg_vlib_dir = os.dir(voidptr_arg_v3_dir)
const voidptr_arg_v3_src = os.join_path(voidptr_arg_v3_dir, 'v.v')

fn voidptr_arg_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_voidptr_arg_cast_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${voidptr_arg_vexe} -gc none -path "${voidptr_arg_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${voidptr_arg_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_c_voidptr_parameter_preserves_v1_call_lowering() {
	root := os.join_path(os.temp_dir(), 'v3_voidptr_arg_cast_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	header := '#ifndef V3_VOIDPTR_ARG_NATIVE_H
#define V3_VOIDPTR_ARG_NATIVE_H
#include <stdint.h>
static inline int strict_accept_u32(uint32_t* p) { return p != 0; }
#define strict_get_count(p) ((p)->count)
#define strict_get_count_alias strict_get_count
#endif
'
	os.write_file(os.join_path(root, 'strict_native.h'), header) or { panic(err) }
	os.write_file(os.join_path(root, 'strict_forced.h'),
		'#define strict_get_forced_count(p) ((p)->count)\n') or { panic(err) }
	source_path := os.join_path(root, 'main.c.v')
	os.write_file(source_path, 'module main

#flag -include @DIR/strict_forced.h
#include "@DIR/strict_native.h"
#define strict_get_direct_count(p) ((p)->count)

fn C.strict_accept_u32(voidptr) int
fn C.strict_get_count(voidptr) u32
fn C.strict_get_count_alias(voidptr) u32
fn C.strict_get_forced_count(voidptr) u32
fn C.strict_get_direct_count(voidptr) u32

struct Item {
	count u32
}

fn main() {
	item := Item{
		count: 9
	}
	// When the real C prototype needs a different pointer type, make the
	// intentionally erased boundary explicit, as V1 did.
	println(C.strict_accept_u32(voidptr(&item)).str())
	// C macros keep their concrete pointer operand without compiler-side analysis.
	println(C.strict_get_count(&item).str())
	println(C.strict_get_count_alias(&item).str())
	println(C.strict_get_forced_count(&item).str())
	println(C.strict_get_direct_count(&item).str())
}
') or { panic(err) }

	v3_bin := voidptr_arg_build_v3()
	out := os.join_path(os.temp_dir(), 'v3_voidptr_arg_cast_out_${os.getpid()}')
	compile := os.execute('${v3_bin} ${source_path} -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.split_into_lines().map(it.trim_space()).filter(it != '') == ['1', '9', '9',
		'9', '9']
	generated := os.read_file(out + '.c') or { panic(err) }
	assert generated.contains('strict_accept_u32((void*)(&item))'), generated
	assert generated.contains('strict_get_count(&item)'), generated
	assert generated.contains('strict_get_count_alias(&item)'), generated
	assert generated.contains('strict_get_forced_count(&item)'), generated
	assert generated.contains('strict_get_direct_count(&item)'), generated
	assert !generated.contains('v_c_voidptr_arg'), generated
}
