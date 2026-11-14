module main

import os
import v.cmdexec

// The arm64 backend links programs that bring C code with the system linker:
// a `.c` source, a header's `static inline` function and function-like macro,
// C constants read from the headers, an `@[export]` callback called from C,
// and a framework.
const external_link_header = '#pragma once
#include <stdint.h>
enum ext_color { EXT_RED = 3, EXT_GREEN = 40 };
#define EXT_LIMIT (EXT_GREEN + 2)
#define EXT_SCALE 2.5
#define EXT_BIG 0xfedcba9876543210ULL
#define EXT_TWICE(x) ((x) * 2)
static inline int32_t ext_add_inline(int32_t a, int32_t b) { return a + b; }
typedef struct ext_pair { double x; double y; } ext_pair;
static inline double ext_pair_sum(ext_pair p) { return p.x + p.y; }
static inline ext_pair ext_make_pair(double x, int32_t times) { ext_pair p = { x, x * times }; return p; }
static inline float ext_half(float x) { return x / 2; }
int32_t ext_call_back(int32_t value);
'

const external_link_source = '#include "ext.h"
int32_t v_ext_callback(int32_t value);
int32_t ext_call_back(int32_t value) { return v_ext_callback(value) + 1; }
int32_t ext_same_name(void) { return 7; }
int32_t ext_call_same_name(void) { return ext_same_name(); }
'

const external_link_program = 'module main

#flag -I @VMODROOT
#flag -framework CoreFoundation
#include "ext.h"
#include "@VMODROOT/ext.c"

struct C.ext_pair {
	x f64
	y f64
}

fn C.ext_add_inline(a i32, b i32) i32
fn C.EXT_TWICE(x i32) i32
fn C.ext_pair_sum(p C.ext_pair) f64
fn C.ext_make_pair(x f64, times i32) C.ext_pair
fn C.ext_half(x f64) f64
fn C.ext_call_back(value i32) i32
fn C.CFAbsoluteTimeGetCurrent() f64
fn C.ext_call_same_name() i32

fn ext_same_name() i32 {
	return 40
}

@[export: \'v_ext_callback\']
fn ext_callback(value i32) i32 {
	return value * 10
}

fn main() {
	println(C.ext_add_inline(2, 3))
	println(C.EXT_TWICE(21))
	println(C.ext_pair_sum(C.ext_pair{ x: 1.5, y: 2.25 }))
	made := C.ext_make_pair(1.5, 3)
	println(made.x + made.y)
	println(C.ext_half(3.0))
	println(int(C.EXT_RED) + int(C.EXT_LIMIT))
	println(f64(C.EXT_SCALE))
	println(u64(C.EXT_BIG))
	println(ext_same_name() + C.ext_call_same_name())
	println(C.ext_call_back(4))
	println(C.CFAbsoluteTimeGetCurrent() > 0)
}
'

const external_link_expected = '5
42
3.75
6.0
1.5
45
2.5
18364758544493064720
47
41
true'

// With `-gc boehm`, the collector is linked in and owns every allocation of
// the generated runtime, including map storage and spawned threads.
const gc_program = '@[heap]
struct Node {
	next &Node = unsafe { nil }
	val  int
}

fn build(n int) &Node {
	mut head := &Node(unsafe { nil })
	for i in 0 .. n {
		head = &Node{
			next: head
			val:  i
		}
	}
	return head
}

fn sum(head &Node) int {
	mut total := 0
	mut p := unsafe { head }
	for p != unsafe { nil } {
		total += p.val
		p = p.next
	}
	return total
}

fn worker(id int) int {
	mut total := 0
	for _ in 0 .. 20 {
		total += sum(build(5000))
	}
	return total + id
}

fn main() {
	mut keep := map[string]&Node{}
	for round in 0 .. 50 {
		keep[round.str()] = build(2000)
		_ = build(20000)
	}
	mut total := 0
	for _, head in keep {
		total += sum(head)
	}
	threads := [spawn worker(1), spawn worker(2)]
	results := threads.wait()
	println(total)
	println(results)
	println(gc_heap_usage().heap_size > 0)
}
'

fn test_arm64_links_c_code_with_the_system_linker() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_external_link_${os.getpid()}')
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		os.write_file(os.join_path(root, 'v.mod'), '')!
		os.write_file(os.join_path(root, 'ext.h'), external_link_header)!
		os.write_file(os.join_path(root, 'ext.c'), external_link_source)!
		os.write_file(os.join_path(root, 'main.v'), external_link_program)!
		output := os.join_path(root, 'main')
		compiled := cmdexec.run(@VEXE, ['-b', 'arm64', '-o', output, os.join_path(root, 'main.v')])
		assert compiled.exit_code == 0, compiled.output
		run := cmdexec.run(output, []string{})
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == external_link_expected, run.output
	}
}

fn test_arm64_links_the_boehm_collector() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_external_gc_${os.getpid()}')
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'gc.v')
		output := os.join_path(root, 'gc')
		os.write_file(source, gc_program)!
		compiled := cmdexec.run(@VEXE, ['-b', 'arm64', '-gc', 'boehm', '-o', output, source])
		assert compiled.exit_code == 0, compiled.output
		run := cmdexec.run(output, []string{})
		assert run.exit_code == 0, run.output
		lines := run.output.trim_space().split_into_lines()
		assert lines.len == 3, run.output
		assert lines[0] == '${50 * (1999 * 2000 / 2)}', run.output
		assert lines[1] == '[${20 * (4999 * 5000 / 2) + 1}, ${20 * (4999 * 5000 / 2) + 2}]', run.output
		assert lines[2] == 'true', run.output
	}
}

// `top.h` needs `base.h` first, and the program imports `top` before `base`:
// the C side keeps the C backend's order, a module's imports before its own
// lines. `struct top_rec` is declared in V with two of its fields, out of order,
// so its layout must come from the C compiler.
const layout_base_header = '#ifndef BASE_H
#define BASE_H
#define BASE_SCALE 3
#endif
'

const layout_top_header = '#ifndef BASE_H
#error "include base.h before top.h"
#endif
struct top_rec {
	char tag;
	double value;
	short pad[5];
	int count;
};
static struct top_rec top_storage;
static inline struct top_rec* top_make(int count) {
	top_storage.tag = 7;
	top_storage.value = 2.5 * BASE_SCALE;
	top_storage.count = count;
	return &top_storage;
}
'

const layout_base_module = 'module base

#include "@VMODROOT/base.h"

pub fn scale() int {
	return C.BASE_SCALE
}
'

const layout_top_module = 'module top

import base

#include "@VMODROOT/top.h"

pub struct C.top_rec {
pub:
	count int
	value f64
}

fn C.top_make(count int) &C.top_rec

pub fn make(count int) &C.top_rec {
	_ = base.scale()
	return C.top_make(count)
}
'

const layout_program = 'import top
import base

fn main() {
	rec := top.make(41)
	println(rec.count)
	println(rec.value)
	println(base.scale())
}
'

fn test_arm64_lays_out_c_structs_and_orders_headers_like_the_c_backend() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_c_layout_${os.getpid()}')
		os.mkdir_all(os.join_path(root, 'base'))!
		os.mkdir_all(os.join_path(root, 'top'))!
		defer {
			os.rmdir_all(root) or {}
		}
		os.write_file(os.join_path(root, 'v.mod'), '')!
		os.write_file(os.join_path(root, 'base.h'), layout_base_header)!
		os.write_file(os.join_path(root, 'top.h'), layout_top_header)!
		os.write_file(os.join_path(root, 'base', 'base.v'), layout_base_module)!
		os.write_file(os.join_path(root, 'top', 'top.v'), layout_top_module)!
		os.write_file(os.join_path(root, 'main.v'), layout_program)!
		output := os.join_path(root, 'main')
		compiled := cmdexec.run(@VEXE, ['-b', 'arm64', '-o', output, root])
		assert compiled.exit_code == 0, compiled.output
		run := cmdexec.run(output, []string{})
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == '41\n7.5\n3', run.output
	}
}

// `__LD,__compact_unwind` lets an exception thrown below a V frame reach the
// handler above it.
fn test_arm64_objc_exception_crosses_v_frames() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_external_unwind_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		os.write_file(os.join_path(root, 'v.mod'), '')!
		os.write_file(os.join_path(root, 'ex.h'), '#import <Foundation/Foundation.h>
int v_callback(void);
static inline int ex_throw(void) { @throw [NSException exceptionWithName:@"x" reason:@"y" userInfo:nil]; }
static inline int ex_call(void) { @try { return v_callback(); } @catch (NSException *e) { return 42; } }
')!
		os.write_file(os.join_path(root, 'main.v'), '#flag -framework Foundation
#include "@VMODROOT/ex.h"

fn C.ex_throw() int
fn C.ex_call() int

@[export: \'v_callback\']
fn v_callback() int {
	return C.ex_throw() + 1
}

fn main() {
	println(C.ex_call())
}
')!
		output := os.join_path(root, 'main')
		compiled := cmdexec.run(@VEXE, ['-b', 'arm64', '-o', output, os.join_path(root, 'main.v')])
		assert compiled.exit_code == 0, compiled.output
		run := cmdexec.run(output, []string{})
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == '42', run.output
	}
}

// C probes are cached by their headers: editing a header must change the value
// a rebuild reads, and an unchanged rebuild must keep it.
fn test_arm64_c_probes_follow_their_headers() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_external_cache_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		os.write_file(os.join_path(root, 'v.mod'), '')!
		header := os.join_path(root, 'value.h')
		source := os.join_path(root, 'main.v')
		output := os.join_path(root, 'main')
		os.write_file(source, '#include "@VMODROOT/value.h"

fn main() {
	println(int(C.PROBE_VALUE))
}
')!
		for value in ['11', '11', '22'] {
			os.write_file(header, '#define PROBE_VALUE ${value}\n')!
			build_args := ['-b', 'arm64', '-d', 'arm64_external_link', '-o', output, source]
			compiled := cmdexec.run(@VEXE, build_args)
			assert compiled.exit_code == 0, compiled.output
			run := cmdexec.run(output, []string{})
			assert run.exit_code == 0, run.output
			assert run.output.trim_space() == value, run.output
		}
	}
}

// V functions use x19-x28 and restore them, as a C caller that keeps a value
// there expects.
fn test_arm64_v_callbacks_preserve_callee_saved_registers() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_external_callee_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		os.write_file(os.join_path(root, 'v.mod'), '')!
		os.write_file(os.join_path(root, 'keep.h'), 'long v_clobber(long n);
static inline long keep_x19(long n) {
	register long kept __asm__("x19") = 77;
	__asm__("" : "+r"(kept));
	long result = v_clobber(n);
	__asm__("" : "+r"(kept));
	return kept * 1000 + result;
}
')!
		os.write_file(os.join_path(root, 'main.v'), '#include "@VMODROOT/keep.h"

fn C.keep_x19(n i64) i64

@[export: \'v_clobber\']
fn v_clobber(n i64) i64 {
	mut total := i64(0)
	for i in 0 .. n {
		total += i * 3
	}
	return total
}

fn main() {
	println(C.keep_x19(10))
}
')!
		output := os.join_path(root, 'main')
		compiled := cmdexec.run(@VEXE, ['-b', 'arm64', '-o', output, os.join_path(root, 'main.v')])
		assert compiled.exit_code == 0, compiled.output
		run := cmdexec.run(output, []string{})
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == '77135', run.output
	}
}
