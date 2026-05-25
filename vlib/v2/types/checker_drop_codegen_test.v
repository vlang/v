// vtest build: !linux && !windows
// End-to-end tests for Drop/RAII codegen (Phase 1).
// Builds the user program with -ownership, runs it, and asserts on
// the binary's stdout to confirm that `drop()` was invoked at scope
// exit in LIFO order for every still-owned Drop binding.
module types

import os

// drop_v2_ownership_exe returns the path to the v2_ownership binary,
// building it on demand. Inlined here so this test file stays
// self-contained — V's `v test` compiles each _test.v file in isolation,
// so helpers defined in sibling test files aren't visible.
fn drop_v2_ownership_exe() string {
	vroot := @VMODROOT
	v2_ownership := os.join_path(vroot, 'cmd', 'v2', 'v2_ownership')
	if os.is_file(v2_ownership) {
		return v2_ownership
	}
	vexe := os.join_path(vroot, 'v')
	res := os.execute('${os.quoted_path(vexe)} -d ownership -o ${os.quoted_path(v2_ownership)} ${os.quoted_path(os.join_path(vroot,
		'cmd', 'v2', 'v2.v'))}')
	if res.exit_code != 0 {
		panic('failed to build v2_ownership: ${res.output}')
	}
	return v2_ownership
}

fn run_drop_program(code string) (int, string) {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_drop_codegen_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic('cannot create ${tmp_dir}') }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	tmp_file := os.join_path(tmp_dir, 'test.v')
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	out_bin := os.join_path(tmp_dir, 'out')
	v2 := drop_v2_ownership_exe()
	compile :=
		os.execute('${os.quoted_path(v2)} -ownership -o ${os.quoted_path(out_bin)} ${os.quoted_path(tmp_file)} 2>&1')
	if compile.exit_code != 0 {
		return compile.exit_code, 'compile failed: ${compile.output}'
	}
	run := os.execute(os.quoted_path(out_bin))
	return run.exit_code, run.output
}

fn test_drop_fires_at_natural_fn_exit() {
	code := "
struct Foo implements Drop {
	id int
}

fn (mut self Foo) drop() {
	println('dropping ' + self.id.str())
}

fn main() {
	x := Foo{id: 7}
	println('before exit')
	_ = x.id
}
"
	exit_code, output := run_drop_program(code)
	assert exit_code == 0, 'program should exit 0, got ${exit_code}: ${output}'
	assert output.contains('before exit'), 'expected user print, got: ${output}'
	assert output.contains('dropping 7'), 'expected drop call, got: ${output}'
	// Drop must come AFTER the user print (natural fn exit, not before body).
	before_idx := output.index('before exit') or { -1 }
	drop_idx := output.index('dropping 7') or { -1 }
	assert before_idx >= 0 && drop_idx > before_idx, 'drop must fire at fn exit, got: ${output}'
}

fn test_drop_lifo_order_multiple_bindings() {
	code := "
struct Foo implements Drop {
	id int
}

fn (mut self Foo) drop() {
	println('dropping ' + self.id.str())
}

fn main() {
	a := Foo{id: 1}
	b := Foo{id: 2}
	c := Foo{id: 3}
	_ = a.id
	_ = b.id
	_ = c.id
}
"
	exit_code, output := run_drop_program(code)
	assert exit_code == 0, 'program should exit 0: ${output}'
	// Bindings declared in order a, b, c — drops fire in reverse: c, b, a.
	c_idx := output.index('dropping 3') or { -1 }
	b_idx := output.index('dropping 2') or { -1 }
	a_idx := output.index('dropping 1') or { -1 }
	assert c_idx >= 0 && b_idx > c_idx && a_idx > b_idx, 'drops must fire LIFO (c,b,a), got: ${output}'
}

fn test_drop_skipped_for_moved_binding() {
	code := "
struct Foo implements Drop, Owned {
	id int
}

fn (mut self Foo) drop() {
	println('dropping ' + self.id.str())
}

fn take(f Foo) {
	println('took ' + f.id.str())
}

fn main() {
	x := Foo{id: 1}
	y := Foo{id: 2}
	take(x)
	println('after take')
	_ = y.id
}
"
	exit_code, output := run_drop_program(code)
	assert exit_code == 0, 'program should exit 0: ${output}'
	assert output.contains('took 1'), 'expected take call, got: ${output}'
	assert output.contains('dropping 2'), 'y must be dropped at main exit, got: ${output}'
	// `x` was moved into take() — main must NOT emit a second drop for id=1.
	dropping_1_count := output.count('dropping 1')
	assert dropping_1_count == 0, 'moved value should not trigger main-exit drop, got ${dropping_1_count} occurrences in: ${output}'
}

fn test_drop_not_emitted_for_non_drop_struct() {
	// Plain struct without `implements Drop` must not get any drop emission.
	// Verifies that the codegen gate is the marker interface, not type creation.
	code := "
struct Bar {
	id int
}

fn main() {
	b := Bar{id: 42}
	println('got ' + b.id.str())
}
"
	exit_code, output := run_drop_program(code)
	assert exit_code == 0, 'program should exit 0: ${output}'
	assert output.contains('got 42'), 'expected user print, got: ${output}'
	assert !output.contains('dropping'), 'no drop should fire for non-Drop type, got: ${output}'
}

fn test_drop_fires_before_early_return() {
	// Phase 1B: early `return` mid-fn must drop every still-owned Drop binding
	// declared before it, in LIFO order. Without this, fns that bail out via
	// `if err { return }` leak every resource declared on the happy path.
	code := "
struct Foo implements Drop {
	id int
}

fn (mut self Foo) drop() {
	println('dropping ' + self.id.str())
}

fn run() {
	a := Foo{id: 1}
	b := Foo{id: 2}
	_ = a.id
	_ = b.id
	println('before return')
	return
}

fn main() {
	run()
	println('after run')
}
"
	exit_code, output := run_drop_program(code)
	assert exit_code == 0, 'program should exit 0: ${output}'
	// Drops must fire BEFORE the caller observes the return — i.e. before
	// 'after run' is printed — and in LIFO order (b first, then a).
	before_idx := output.index('before return') or { -1 }
	b_idx := output.index('dropping 2') or { -1 }
	a_idx := output.index('dropping 1') or { -1 }
	after_idx := output.index('after run') or { -1 }
	assert before_idx >= 0 && b_idx > before_idx && a_idx > b_idx && after_idx > a_idx, 'drops must fire LIFO at early return, before caller resumes, got: ${output}'
}

fn test_drop_early_return_skips_moved_binding() {
	// A binding moved into a fn call before the early return must NOT be
	// dropped again at the return — its drop responsibility transferred.
	code := "
struct Foo implements Drop, Owned {
	id int
}

fn (mut self Foo) drop() {
	println('dropping ' + self.id.str())
}

fn take(f Foo) {
	println('took ' + f.id.str())
}

fn run() {
	a := Foo{id: 1}
	b := Foo{id: 2}
	take(a)
	println('before return')
	return
	_ = b.id
}

fn main() {
	run()
}
"
	exit_code, output := run_drop_program(code)
	assert exit_code == 0, 'program should exit 0: ${output}'
	assert output.contains('took 1'), 'expected take call, got: ${output}'
	assert output.contains('dropping 2'), 'b must drop at early return, got: ${output}'
	// a was moved into take — must not produce a second drop for id=1.
	dropping_1_count := output.count('dropping 1')
	assert dropping_1_count == 0, 'moved binding must not drop at early return, got ${dropping_1_count} occurrences in: ${output}'
}

fn test_drop_multiple_returns_each_drop_independently() {
	// Each return point gets its own drop set computed at that point. A fn
	// with two return arms should drop the right bindings on each arm.
	code := "
struct Foo implements Drop {
	id int
}

fn (mut self Foo) drop() {
	println('dropping ' + self.id.str())
}

fn run(flag bool) {
	a := Foo{id: 1}
	_ = a.id
	if flag {
		b := Foo{id: 2}
		_ = b.id
		println('A')
		return
	}
	c := Foo{id: 3}
	_ = c.id
	println('B')
}

fn main() {
	run(true)
	println('---')
	run(false)
}
"
	exit_code, output := run_drop_program(code)
	assert exit_code == 0, 'program should exit 0: ${output}'
	// Path A (flag=true): drops b then a at the early return.
	// Path B (flag=false): drops c then a at natural exit.
	sep_idx := output.index('---') or { -1 }
	assert sep_idx > 0, 'expected separator, got: ${output}'
	head := output[..sep_idx]
	tail := output[sep_idx..]
	a_head_idx := head.index('dropping 1') or { -1 }
	b_head_idx := head.index('dropping 2') or { -1 }
	assert b_head_idx >= 0 && a_head_idx > b_head_idx, 'early-return path must drop b then a, got: ${head}'

	a_tail_idx := tail.index('dropping 1') or { -1 }
	c_tail_idx := tail.index('dropping 3') or { -1 }
	assert c_tail_idx >= 0 && a_tail_idx > c_tail_idx, 'natural-exit path must drop c then a, got: ${tail}'

	// b was never declared on the B path.
	assert !tail.contains('dropping 2'), 'b must not appear on B path, got: ${tail}'
}
