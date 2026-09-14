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
#endif
'
	os.write_file(os.join_path(root, 'strict_atomic.h'), header) or { panic(err) }
	path := os.join_path(root, 'main.v')
	os.write_file(path, source) or { panic(err) }
	return path
}

fn test_c_voidptr_param_pointer_arg_goes_through_voidptr() {
	v3_bin := voidptr_arg_build_v3()
	src := voidptr_arg_write_project('module main

#include "@DIR/strict_atomic.h"

fn C.strict_load_u64(voidptr) u64
fn C.strict_store_u64(voidptr, u64)
fn C.strict_load_any(voidptr) u64
fn C.strict_is_nonnull_u64(voidptr) int

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
	// An argument that is already `voidptr` needs no conversion of its own.
	same := C.strict_load_any(voidptr(&slot))
	// Aliases to pointer types need the same conversion as direct pointers.
	nonnull := C.strict_is_nonnull_u64(TableRef(table))
	println(int(loaded.count).str())
	println((same == u64(voidptr(table))).str())
	println(nonnull.str())
}
')
	out := os.join_path(os.temp_dir(), 'v3_voidptr_arg_cast_out_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.split_into_lines().map(it.trim_space()).filter(it != '') == ['7', 'true',
		'1']
	generated := os.read_file(out + '.c') or { panic(err) }
	// The macro is a no-op in C++, where `void*` does not convert back to a
	// concrete pointer and the argument has to stay as written.
	assert generated.contains('#define v_c_voidptr_arg(x) ((void*)(x))'), generated
	assert generated.contains('#define v_c_voidptr_arg(x) (x)'), generated
	// The `Table**` argument is routed through `void*` ...
	assert generated.contains('strict_load_u64(v_c_voidptr_arg(&slot))'), generated
	assert generated.contains('strict_store_u64(v_c_voidptr_arg(&slot)'), generated
	assert generated.contains('strict_is_nonnull_u64(v_c_voidptr_arg('), generated
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
}
