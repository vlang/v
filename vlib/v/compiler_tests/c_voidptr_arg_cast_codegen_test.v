import os
import v.tempname

fn test_c_voidptr_parameter_preserves_v1_call_lowering() {
	root := os.join_path(os.vtmp_dir(), 'v3_voidptr_arg_cast_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
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

	out := os.join_path(root, 'v3_voidptr_arg_cast_out')
	compile := os.exec([@VEXE, source_path, '-b', 'c', '-o', '${out}'])
	assert compile.exit_code == 0, compile.output
	run := os.exec([out])
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
