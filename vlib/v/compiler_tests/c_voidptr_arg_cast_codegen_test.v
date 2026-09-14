import os

// A `fn C.` declaration can only spell a pointer slot it does not care about as
// `voidptr`, but the C function behind it is free to declare that slot as a
// concrete pointer — `atomic_load_u64(uint64_t*)` in thirdparty/stdatomic is the
// case that broke the bootstrap. C converts implicitly only to and from `void*`,
// so the argument has to go through `void*` at the call site or the C compiler
// reports incompatible pointer types (an error on GCC 14+ and Clang 16+).

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

fn voidptr_arg_write_project(source string) string {
	root := os.join_path(os.temp_dir(), 'v3_voidptr_arg_cast_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	// Deliberately strict prototypes: the slots the V side declares as `voidptr`
	// are `uint64_t*` here, exactly like the real atomics header.
	header := '#ifndef V3_VOIDPTR_ARG_NATIVE_H
#define V3_VOIDPTR_ARG_NATIVE_H
#include <stdint.h>
static inline uint64_t strict_load_u64(uint64_t* x) { return *x; }
static inline void strict_store_u64(uint64_t* x, uint64_t y) { *x = y; }
static inline uint64_t strict_load_any(void* x) { return *(uint64_t*)x; }
static inline int strict_is_nonnull_u64(uint64_t* x) { return x != 0; }
#define strict_get_count(p) ((p)->count)
#define strict_get_count_alias strict_get_count
#ifdef __GNUC__
#define strict_get_forward_count strict_get_forward_impl
#else
#define strict_get_forward_count strict_get_forward_impl
#endif
#define strict_get_forward_impl(p) ((p)->count)
#endif
'
	os.write_file(os.join_path(root, 'strict_atomic.h'), header) or { panic(err) }
	os.write_file(os.join_path(root, 'strict_forced_include.h'), '#define strict_get_forced_count(p) ((p)->count)\n') or { panic(err) }
	os.write_file(os.join_path(root, 'strict_forced_imacros.h'), '#define strict_get_imacros_count(p) ((p)->count)\n') or { panic(err) }
	os.write_file(os.join_path(root, 'strict_push_pop.h'), '#define strict_get_restored_count(p) ((p)->count)\n#pragma push_macro("strict_get_restored_count")\n#undef strict_get_restored_count\n#pragma pop_macro("strict_get_restored_count")\n') or { panic(err) }
	os.write_file(os.join_path(root, 'strict_preinclude.h'), '#define strict_get_preincluded_count(p) ((p)->count)\n') or { panic(err) }
	os.write_file(os.join_path(root, 'strict_reordered_define.h'), '#define strict_get_reordered_count(p) ((p)->count)\n') or { panic(err) }
	os.write_file(os.join_path(root, 'strict_reordered_undef.h'), '#undef strict_get_reordered_count\n') or { panic(err) }
	os.write_file(os.join_path(root, 'strict_nested_api.h'), '#define strict_get_nested_count(p) ((p)->count)\n') or { panic(err) }
	os.write_file(os.join_path(root, 'strict_nested_wrapper.h'), '#define V3_STRICT_NESTED_HEADER "strict_nested_api.h"\n#include V3_STRICT_NESTED_HEADER\n') or { panic(err) }
	os.write_file(os.join_path(root, 'strict_state_a.h'), '#define V3_STRICT_ORDERED_LOAD 1\n#define strict_ordered_load_u64(p) (*(p))\n') or { panic(err) }
	os.write_file(os.join_path(root, 'strict_state_b.h'), '#ifdef V3_STRICT_ORDERED_LOAD\n#undef strict_ordered_load_u64\n#endif\n#include <stdint.h>\nstatic inline uint64_t strict_ordered_load_u64(uint64_t* x) { return *x; }\n') or { panic(err) }
	os.write_file(os.join_path(root, 'strict_once_macro.h'), '#pragma once\n#define strict_once_load(p) ((p)->count)\n') or { panic(err) }
	os.write_file(os.join_path(root, 'strict_once_function.h'), '#include <stdint.h>\nstatic inline uint32_t strict_once_load(uint64_t* p) { (void)p; return 9; }\n') or { panic(err) }
	os.write_file(os.join_path(root, 'strict_compiler_selected.h'), '#include <stdint.h>\n#ifndef __GNUC__\n#define strict_compiler_selected(p) ((p)->count)\n#else\nstatic inline uint32_t strict_compiler_selected(uint64_t* p) { (void)p; return 9; }\n#endif\n') or { panic(err) }
	path := os.join_path(root, 'main.c.v')
	os.write_file(path, source) or { panic(err) }
	return path
}

fn test_c_voidptr_param_pointer_arg_goes_through_voidptr() {
	v3_bin := voidptr_arg_build_v3()
	src := voidptr_arg_write_project('module main

#flag -include @DIR/strict_forced_include.h
#flag -imacros @DIR/strict_forced_imacros.h
#preinclude "@DIR/strict_preinclude.h"
#include "@DIR/strict_atomic.h"
#include "@DIR/strict_push_pop.h"
#include "@DIR/strict_reordered_define.h"
#preinclude "@DIR/strict_reordered_undef.h"
#include "@DIR/strict_nested_wrapper.h"
#include "@DIR/strict_state_a.h"
#include "@DIR/strict_state_b.h"
#include "@DIR/strict_once_macro.h"
#undef strict_once_load
#include "@DIR/strict_once_function.h"
#include "@DIR/strict_once_macro.h"
#include "@DIR/strict_compiler_selected.h"
#define strict_get_direct_count(p) ((p)->count)
#if 0
#define strict_load_u64(p) (*(p))
#undef strict_get_direct_count
#endif

fn C.strict_load_u64(voidptr) u64
fn C.strict_store_u64(voidptr, u64)
fn C.strict_load_any(voidptr) u64
fn C.strict_is_nonnull_u64(voidptr) int
fn C.strict_ordered_load_u64(voidptr) u64
fn C.strict_get_count(voidptr) u32
fn C.strict_get_count_alias(voidptr) u32
fn C.strict_get_forward_count(voidptr) u32
fn C.strict_get_forced_count(voidptr) u32
fn C.strict_get_imacros_count(voidptr) u32
fn C.strict_get_restored_count(voidptr) u32
fn C.strict_get_direct_count(voidptr) u32
fn C.strict_get_preincluded_count(voidptr) u32
fn C.strict_get_reordered_count(voidptr) u32
fn C.strict_get_nested_count(voidptr) u32
fn C.strict_once_load(voidptr) u32
fn C.strict_compiler_selected(voidptr) u32

struct Table {
mut:
	count u32
}

type TableRef = &Table

fn main() {
	table := &Table{
		count: 7
	}
	mut slot := &Table(unsafe { nil })
	// `&slot` is a `Table**`, which is incompatible with the `uint64_t*` the C
	// prototype declares.
	C.strict_store_u64(&slot, u64(voidptr(table)))
	loaded := unsafe { &Table(voidptr(C.strict_load_u64(&slot))) }
	ordered_loaded := unsafe { &Table(voidptr(C.strict_ordered_load_u64(&slot))) }
	// An argument that is already `voidptr` needs no conversion of its own.
	same := C.strict_load_any(voidptr(&slot))
	// Aliases to pointer types need the same conversion as direct pointers.
	nonnull := C.strict_is_nonnull_u64(TableRef(table))
	// A macro can depend on the concrete pointer type of its operand.
	item := Table{
		count: 9
	}
	macro_count := C.strict_get_count(&item)
	alias_macro_count := C.strict_get_count_alias(&item)
	forward_macro_count := C.strict_get_forward_count(&item)
	forced_macro_count := C.strict_get_forced_count(&item)
	imacros_macro_count := C.strict_get_imacros_count(&item)
	restored_macro_count := C.strict_get_restored_count(&item)
	direct_macro_count := C.strict_get_direct_count(&item)
	preincluded_macro_count := C.strict_get_preincluded_count(&item)
	reordered_macro_count := C.strict_get_reordered_count(&item)
	nested_macro_count := C.strict_get_nested_count(&item)
	once_count := C.strict_once_load(&item)
	compiler_selected_count := C.strict_compiler_selected(&item)
	println(int(loaded.count).str())
	println(int(ordered_loaded.count).str())
	println((same == u64(voidptr(table))).str())
	println(nonnull.str())
	println(macro_count.str())
	println(alias_macro_count.str())
	println(forward_macro_count.str())
	println(forced_macro_count.str())
	println(imacros_macro_count.str())
	println(restored_macro_count.str())
	println(direct_macro_count.str())
	println(preincluded_macro_count.str())
	println(reordered_macro_count.str())
	println(nested_macro_count.str())
	println(once_count.str())
	println(compiler_selected_count.str())
}
')
	out := os.join_path(os.temp_dir(), 'v3_voidptr_arg_cast_out_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.split_into_lines().map(it.trim_space()).filter(it != '') == ['7', '7', 'true',
		'1', '9', '9', '9', '9', '9', '9', '9', '9', '9', '9', '9', '9']
	generated := os.read_file(out + '.c') or { panic(err) }
	// The macro is a no-op in C++, where `void*` does not convert back to a
	// concrete pointer and the argument has to stay as written.
	assert generated.contains('#define v_c_voidptr_arg(x) ((void*)(x))'), generated
	assert generated.contains('#define v_c_voidptr_arg(x) (x)'), generated
	// The `Table**` argument is routed through `void*` ...
	assert generated.contains('strict_load_u64(v_c_voidptr_arg(&slot))'), generated
	assert generated.contains('strict_store_u64(v_c_voidptr_arg(&slot)'), generated
	assert generated.contains('strict_is_nonnull_u64(v_c_voidptr_arg('), generated
	// Macro state carries across top-level includes: the second header undefines
	// the macro from the first one and exposes the real typed-pointer function.
	assert generated.contains('strict_ordered_load_u64(v_c_voidptr_arg(&slot))'), generated
	// ... but an active function-like macro receives the original typed pointer.
	assert generated.contains('strict_get_count(&item)'), generated
	assert !generated.contains('strict_get_count(v_c_voidptr_arg('), generated
	// Object-like aliases inherit function-like macro status.
	assert generated.contains('strict_get_count_alias(&item)'), generated
	assert !generated.contains('strict_get_count_alias(v_c_voidptr_arg('), generated
	// A possible forward alias is retained until its function-like target is seen.
	assert generated.contains('strict_get_forward_count(&item)'), generated
	assert !generated.contains('strict_get_forward_count(v_c_voidptr_arg('), generated
	// Forced include and imacros inputs are processed before the translation unit.
	assert generated.contains('strict_get_forced_count(&item)'), generated
	assert !generated.contains('strict_get_forced_count(v_c_voidptr_arg('), generated
	assert generated.contains('strict_get_imacros_count(&item)'), generated
	assert !generated.contains('strict_get_imacros_count(v_c_voidptr_arg('), generated
	// push_macro/pop_macro restores the original function-like definition.
	assert generated.contains('strict_get_restored_count(&item)'), generated
	assert !generated.contains('strict_get_restored_count(v_c_voidptr_arg('), generated
	// A function-like macro declared directly in V source also keeps the typed pointer.
	assert generated.contains('strict_get_direct_count(&item)'), generated
	assert !generated.contains('strict_get_direct_count(v_c_voidptr_arg('), generated
	// A function-like macro from a preincluded header also keeps the typed pointer.
	assert generated.contains('strict_get_preincluded_count(&item)'), generated
	assert !generated.contains('strict_get_preincluded_count(v_c_voidptr_arg('), generated
	// Preincludes are scanned in the order they are emitted, before ordinary headers.
	assert generated.contains('strict_get_reordered_count(&item)'), generated
	assert !generated.contains('strict_get_reordered_count(v_c_voidptr_arg('), generated
	// Literal-valued include macros are resolved so nested macro definitions are visible.
	assert generated.contains('strict_get_nested_count(&item)'), generated
	assert !generated.contains('strict_get_nested_count(v_c_voidptr_arg('), generated
	// A pragma-once header is not replayed after its macro is undefined.
	assert generated.contains('strict_once_load(v_c_voidptr_arg(&item))'), generated
	// The selected compiler's predefined macros choose the same header branch as CGen.
	$if !windows {
		assert generated.contains('strict_compiler_selected(v_c_voidptr_arg(&item))'), generated
	}
	// ... while an argument that is already `voidptr` is passed unchanged.
	assert !generated.contains('strict_load_any(v_c_voidptr_arg('), generated

	no_builtin_src := voidptr_arg_write_project('module main

#include "@DIR/strict_atomic.h"

fn C.strict_is_nonnull_u64(voidptr) int

struct Table {
	count u32
}

type TableRef = &Table

fn main() {
	table := Table{
		count: 7
	}
	_ = C.strict_is_nonnull_u64(TableRef(&table))
}
')
	no_builtin_out := os.join_path(os.temp_dir(), 'v3_voidptr_arg_cast_no_builtin_${os.getpid()}')
	no_builtin_compile := os.execute('${v3_bin} -no-builtin -gc none -b c -o ${no_builtin_out} ${no_builtin_src}')
	assert no_builtin_compile.exit_code == 0, no_builtin_compile.output
	no_builtin_generated := os.read_file(no_builtin_out + '.c') or { panic(err) }
	assert no_builtin_generated.contains('#define v_c_voidptr_arg(x) ((void*)(x))'), no_builtin_generated
	assert no_builtin_generated.contains('strict_is_nonnull_u64(v_c_voidptr_arg('), no_builtin_generated

	compiler_builtin_src := voidptr_arg_write_project('module main

fn C.__builtin_add_overflow(i32, i32, voidptr) bool

fn main() {
	mut sum := i32(0)
	overflowed := C.__builtin_add_overflow(i32(1), i32(2), &sum)
	println(overflowed.str())
	println(sum.str())
}
')
	compiler_builtin_out := os.join_path(os.temp_dir(), 'v3_voidptr_arg_cast_builtin_${os.getpid()}')
	compiler_builtin_compile := os.execute('${v3_bin} -check-overflow -cc clang -b c -o ${compiler_builtin_out} ${compiler_builtin_src}')
	assert compiler_builtin_compile.exit_code == 0, compiler_builtin_compile.output
	compiler_builtin_generated := os.read_file(compiler_builtin_out + '.c') or { panic(err) }
	assert compiler_builtin_generated.contains('__builtin_add_overflow((i32)(1), (i32)(2), &sum)'), compiler_builtin_generated
	assert !compiler_builtin_generated.contains('__builtin_add_overflow((i32)(1), (i32)(2), v_c_voidptr_arg('), compiler_builtin_generated

	$if macos {
		system_macro_src := voidptr_arg_write_project('module main

#include <sys/queue.h>

fn C.TAILQ_FIRST(voidptr) voidptr

struct QueueHead {
	tqh_first voidptr
}

fn main() {
	head := QueueHead{}
	_ = C.TAILQ_FIRST(&head)
}
')
		system_macro_out := os.join_path(os.temp_dir(), 'v3_voidptr_arg_cast_system_macro_${os.getpid()}')
		system_macro_compile := os.execute('${v3_bin} -cc clang -b c -o ${system_macro_out} ${system_macro_src}')
		assert system_macro_compile.exit_code == 0, system_macro_compile.output
		system_macro_generated := os.read_file(system_macro_out + '.c') or { panic(err) }
		assert system_macro_generated.contains('TAILQ_FIRST(&head)'), system_macro_generated
		assert !system_macro_generated.contains('TAILQ_FIRST(v_c_voidptr_arg('), system_macro_generated
	}
}
