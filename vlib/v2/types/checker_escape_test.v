// Tests for body-level escape analysis (vlib/v2/types/checker_escape.v).
// Built with `-d ownership`. Mirrors the helpers in the sibling
// `checker_lifetimes_test.v` and `checker_ownership_test.v` files —
// `v test` compiles each test file in isolation, so module-private helpers
// don't cross file boundaries.
module types

import os

fn v2_ownership_escape_exe() string {
	vroot := @VMODROOT
	v2_ownership := os.join_path(vroot, 'cmd', 'v2', 'v2_ownership_escape')
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

fn run_escape_check(code string) (int, string) {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_escape_test_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic('cannot create ${tmp_dir}') }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	tmp_file := os.join_path(tmp_dir, 'test.v')
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	v2 := v2_ownership_escape_exe()
	res :=
		os.execute('${os.quoted_path(v2)} -ownership -o ${os.join_path(tmp_dir, 'out')} ${os.quoted_path(tmp_file)} 2>&1')
	return res.exit_code, res.output
}

fn test_escape_return_local_errors() {
	// Canonical dangling reference: returning `&x` where `x` is a local
	// declared in the function body.
	code := '
struct Bar {
	v int
}

fn dangling[^a](p &^a Bar) &^a Bar {
	x := Bar{v: 1}
	return &x
}

fn main() {}
'
	exit_code, output := run_escape_check(code)
	assert exit_code != 0, 'returning &local should fail'
	assert output.contains('cannot return reference to local variable `x`'), 'got: ${output}'
	assert output.contains('does not survive `dangling`'), 'should name the function, got: ${output}'
}

fn test_escape_return_local_field_errors() {
	// Selector chain rooted in a local — `&h.inner` is just as bad as `&h`.
	code := '
struct Bar {
	v int
}

struct Holder {
	inner Bar
}

fn dangling[^a](p &^a Bar) &^a Bar {
	h := Holder{inner: Bar{v: 1}}
	return &h.inner
}

fn main() {}
'
	exit_code, output := run_escape_check(code)
	assert exit_code != 0, 'returning &local.field should fail'
	assert output.contains('via field access'), 'should mention field access, got: ${output}'
	assert output.contains('local variable `h`'), 'should name the local, got: ${output}'
}

fn test_escape_return_local_index_errors() {
	// Index into a local container — the container goes out of scope at
	// return, so the element pointer is dangling.
	code := '
struct Bar {
	v int
}

fn dangling[^a](p &^a Bar) &^a Bar {
	arr := [Bar{v: 1}]
	return &arr[0]
}

fn main() {}
'
	exit_code, output := run_escape_check(code)
	assert exit_code != 0, 'returning &local[i] should fail'
	assert output.contains('index into local `arr`'), 'got: ${output}'
}

fn test_escape_return_transient_struct_literal_errors() {
	// Under an opted-in signature, `&Bar{...}` in a return is treated as
	// a transient address-take (matching Rust). Plain V code without an
	// opted-in signature is unaffected — see the legacy test below.
	code := '
struct Bar {
	v int
}

fn dangling[^a](p &^a Bar) &^a Bar {
	return &Bar{v: 1}
}

fn main() {}
'
	exit_code, output := run_escape_check(code)
	assert exit_code != 0, 'returning &Foo{...} from opted-in fn should fail'
	assert output.contains('transient struct literal'), 'got: ${output}'
}

fn test_escape_return_param_ok() {
	// Parameters outlive the call from the caller's view. `return p` is
	// the canonical safe ref return — Phase 1 already passes this; the
	// escape walker must NOT raise a false positive on top.
	code := '
struct Bar {
	v int
}

fn pass_through[^a](p &^a Bar) &^a Bar {
	return p
}

fn main() {}
'
	exit_code, output := run_escape_check(code)
	assert exit_code == 0, 'returning &param should compile: ${output}'
}

fn test_escape_legacy_unchanged() {
	// CRITICAL: legacy V code without `[^a]` must NOT be affected. In V
	// `&Foo{...}` and `return &local` are GC-safe (the compiler heap-
	// allocates). Forcing Rust-strict escape checking on all code would
	// break the entire stdlib (e.g. `fn new_node() &Node { return &Node{...} }`).
	code := '
struct Bar {
	v int
}

fn legacy_literal() &Bar {
	return &Bar{v: 1}
}

fn legacy_local() &Bar {
	x := Bar{v: 1}
	return &x
}

fn main() {}
'
	exit_code, output := run_escape_check(code)
	assert exit_code == 0, 'legacy un-opted-in code must not be affected: ${output}'
}

fn test_escape_caught_inside_nested_block() {
	// The walker descends into if/match/for branches — a dangling ref
	// inside a nested block is still caught.
	code := '
struct Bar {
	v int
}

fn dangling[^a](p &^a Bar, cond bool) &^a Bar {
	if cond {
		x := Bar{v: 1}
		return &x
	}
	return p
}

fn main() {}
'
	exit_code, output := run_escape_check(code)
	assert exit_code != 0, 'dangling ref inside if-branch should fail'
	assert output.contains('local variable `x`'), 'got: ${output}'
}

// === Field-store escape: `outer.field = &local` ===

struct EscapeBag {
mut:
	r &int = unsafe { nil }
}

fn test_escape_field_store_into_param_errors() {
	// Storing &local into a param-rooted field lets the borrow outlive
	// the function — classic dangling-reference shape.
	code := '
struct Bag {
mut:
	r &int = unsafe { nil }
}

fn stash[^a](mut b Bag) {
	x := 7
	b.r = &x
}

fn main() {}
'
	exit_code, output := run_escape_check(code)
	assert exit_code != 0, 'storing &local into param.field should fail'
	assert output.contains('stored into `b.r`'), 'should name the LHS path, got: ${output}'
	assert output.contains('local variable `x`'), 'should name the local, got: ${output}'
	assert output.contains('does not survive `stash`'), 'should name the function, got: ${output}'
}

fn test_escape_field_store_of_local_field_errors() {
	// `&local.field` is just as bad as `&local` — same root-is-local rule.
	code := '
struct Inner {
	v int
}

struct Outer {
	inner Inner
}

struct Bag {
mut:
	r &Inner = unsafe { nil }
}

fn stash[^a](mut b Bag) {
	o := Outer{inner: Inner{v: 1}}
	b.r = &o.inner
}

fn main() {}
'
	exit_code, output := run_escape_check(code)
	assert exit_code != 0, 'storing &local.field into outer should fail'
	assert output.contains('via field access'), 'should mention field access, got: ${output}'
}

fn test_escape_field_store_into_local_ok() {
	// LHS root is itself a local — the borrow can't outlive its container,
	// so the store is safe.
	code := '
struct Bag {
mut:
	r &int = unsafe { nil }
}

fn safe[^a](p &^a int) {
	mut b := Bag{}
	x := 7
	b.r = &x
	_ = p
}

fn main() {}
'
	exit_code, output := run_escape_check(code)
	assert exit_code == 0, 'storing &local into local.field is safe, got: ${output}'
}

fn test_escape_field_store_no_optin_ok() {
	// Fns without a lifetime generic param keep V's GC-backed semantics:
	// the escape walker does not run, so even `outer.r = &local` compiles.
	code := '
struct Bag {
mut:
	r &int = unsafe { nil }
}

fn stash(mut b Bag) {
	x := 7
	b.r = &x
}

fn main() {}
'
	exit_code, output := run_escape_check(code)
	assert exit_code == 0, 'legacy fn must not be affected: ${output}'
}

// === Array-push escape: `outer_arr << &local` ===

fn test_escape_array_push_into_param_errors() {
	code := '
fn collect[^a](mut out []&int) {
	x := 7
	out << &x
}

fn main() {}
'
	exit_code, output := run_escape_check(code)
	assert exit_code != 0, 'pushing &local into outer array should fail'
	assert output.contains('pushed into `out`'), 'should name the array, got: ${output}'
	assert output.contains('local variable `x`'), 'should name the local, got: ${output}'
}

fn test_escape_array_push_field_path_errors() {
	// `bag.arr << &local` — same shape with a selector LHS.
	code := '
struct Bag {
mut:
	arr []&int
}

fn collect[^a](mut b Bag) {
	x := 7
	b.arr << &x
}

fn main() {}
'
	exit_code, output := run_escape_check(code)
	assert exit_code != 0, 'pushing &local into param.field array should fail'
	assert output.contains('pushed into `b.arr`'), 'should name the array path, got: ${output}'
}

fn test_escape_array_push_into_local_ok() {
	// Pushing &local into another local array is safe — the array dies
	// with the borrow.
	code := '
fn safe[^a](p &^a int) {
	mut out := []&int{}
	x := 7
	out << &x
	_ = p
	_ = out
}

fn main() {}
'
	exit_code, output := run_escape_check(code)
	assert exit_code == 0, 'pushing &local into local array is safe, got: ${output}'
}
