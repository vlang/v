module fastc

import os
import v.cmdexec
import v.pref

// A spawn appended to a thread array (`threads << spawn f(arg)`) whose argument renders as
// a statement expression with a loop (`.map(...)`, `.filter(...)`, `[]T{len: n, init: ...}`,
// `[]Struct{len: n}` with field defaults) is re-rendered from its tokens by the append path.
// The spawn must still render as the call, not as an empty `()` element.
fn test_fastc_spawn_with_statement_expression_argument_appended_to_thread_array() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_spawn_statement_argument_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main

struct Item {
mut:
	names []string
	n     int = 3
}

fn sum(values []int) int {
	mut total := 0
	for v in values {
		total += v
	}
	return total
}

fn count_items(items []Item) int {
	mut total := 0
	for item in items {
		total += item.n + item.names.len
	}
	return total
}

fn twice(x int) int {
	return x * 2
}

fn main() {
	values := [1, 2, 3, 4]
	c := true
	mut threads := []thread int{}
	threads << spawn sum(values.map(it * 2))
	threads << spawn sum(values.filter(it % 2 == 0))
	threads << spawn twice(if c { 5 } else { 6 })
	threads << spawn count_items([]Item{len: 2})
	threads << spawn sum([]int{len: 4, init: index * 3})
	assert threads.wait() == [20, 6, 10, 6, 18]
	listed := [spawn sum(values.map(it + 1)), spawn sum(values.filter(it > 2))]
	assert listed.wait() == [14, 7]
	println("ok")
}
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	prefs.exclude = [os.join_path(@VEXEROOT, 'vlib/builtin/backtraces_nix.c.v')]
	c_source := generate_files([root], prefs) or { panic(err) }
	assert !c_source.contains(' = ();'), 'a spawn rendered as an empty expression'
	c_file := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-w', '-pthread', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}

// A fire-and-forget `spawn f()` statement discards the handle, so the thread is detached.
// The spawn stays in g.last_expression as one atom (for the append path above), so the
// statement must still be told apart from other thread-typed values.
fn test_fastc_spawn_statement_detaches_the_thread() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_spawn_statement_detach_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'hits.h'), '#include <stdatomic.h>
static atomic_long hit_total;
static void add_hit(long value) { atomic_fetch_add(&hit_total, value); }
static long hit_count(void) { return atomic_load(&hit_total); }
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r'module main

#include "@DIR/hits.h"

fn C.add_hit(value i64)
fn C.hit_count() i64

struct Scaler {
	factor int
}

fn add_one(x int) {
	C.add_hit(x + 1)
}

fn (s Scaler) scale(x int) {
	C.add_hit(x * s.factor)
}

fn main() {
	spawn add_one(5)
	s := Scaler{
		factor: 3
	}
	spawn s.scale(4)
	for i in 0 .. 2 {
		if i >= 0 {
			spawn add_one(i * 10)
		}
	}
	for C.hit_count() != 6 + 12 + 1 + 11 {
	}
	println("ok")
}
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	prefs.exclude = [os.join_path(@VEXEROOT, 'vlib/builtin/backtraces_nix.c.v')]
	c_source := generate_files([root], prefs) or { panic(err) }
	assert c_source.count('pthread_detach(') >= 3, c_source
	c_file := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-w', '-pthread', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
