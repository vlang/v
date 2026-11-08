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
#define EXT_TWICE(x) ((x) * 2)
static inline int32_t ext_add_inline(int32_t a, int32_t b) { return a + b; }
typedef struct ext_pair { double x; double y; } ext_pair;
static inline double ext_pair_sum(ext_pair p) { return p.x + p.y; }
int32_t ext_call_back(int32_t value);
'

const external_link_source = '#include "ext.h"
int32_t v_ext_callback(int32_t value);
int32_t ext_call_back(int32_t value) { return v_ext_callback(value) + 1; }
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
fn C.ext_call_back(value i32) i32
fn C.CFAbsoluteTimeGetCurrent() f64

@[export: \'v_ext_callback\']
fn ext_callback(value i32) i32 {
	return value * 10
}

fn main() {
	println(C.ext_add_inline(2, 3))
	println(C.EXT_TWICE(21))
	println(C.ext_pair_sum(C.ext_pair{ x: 1.5, y: 2.25 }))
	println(int(C.EXT_RED) + int(C.EXT_LIMIT))
	println(f64(C.EXT_SCALE))
	println(C.ext_call_back(4))
	println(C.CFAbsoluteTimeGetCurrent() > 0)
}
'

const external_link_expected = '5
42
3.75
45
2.5
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
