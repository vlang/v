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

// === Closure-capture escape: `fn [&local] () {}` ===
//
// A closure that captures a body-local by reference holds a borrow that dies
// with the function. Letting the closure value itself escape (via return,
// field-store, or array-push) carries the borrow out with it.

fn test_escape_return_closure_borrow_errors() {
	code := '
fn dangling[^a]() fn () int {
	x := 7
	cb := fn [&x] () int {
		return *x
	}
	return cb
}

fn main() {}
'
	exit_code, output := run_escape_check(code)
	assert exit_code != 0, 'returning closure that borrows local should fail'
	assert output.contains('closure `cb` capturing local by reference'), 'should mention the bound closure name, got: ${output}'
	assert output.contains('does not survive `dangling`'), 'should name the function, got: ${output}'
}

fn test_escape_return_closure_by_value_ok() {
	// `[x]` is a by-value capture — the closure owns its own copy and the
	// outer `x` is irrelevant to its lifetime. Safe to return.
	code := '
fn safe[^a]() fn () int {
	x := 7
	cb := fn [x] () int {
		return x
	}
	return cb
}

fn main() {}
'
	exit_code, output := run_escape_check(code)
	assert exit_code == 0, 'returning closure with by-value capture is safe, got: ${output}'
}

fn test_escape_return_inline_closure_borrow_errors() {
	// Same shape but with the closure literal returned inline, not via a
	// local binding.
	code := '
fn dangling[^a]() fn () int {
	x := 7
	return fn [&x] () int {
		return *x
	}
}

fn main() {}
'
	exit_code, output := run_escape_check(code)
	assert exit_code != 0, 'returning inline closure that borrows local should fail'
	assert output.contains('captured by reference into closure'), 'got: ${output}'
}

fn test_escape_field_store_closure_borrow_errors() {
	code := '
struct Slot {
mut:
	cb fn () int = unsafe { nil }
}

fn stash[^a](mut s Slot) {
	x := 7
	s.cb = fn [&x] () int {
		return *x
	}
}

fn main() {}
'
	exit_code, output := run_escape_check(code)
	assert exit_code != 0, 'storing closure-borrow into param.field should fail'
	assert output.contains('stored into `s.cb`'), 'should name the LHS path, got: ${output}'
	assert output.contains('captured by reference into closure'), 'got: ${output}'
}

fn test_escape_array_push_closure_borrow_errors() {
	code := '
fn collect[^a](mut callbacks []fn () int) {
	x := 7
	callbacks << fn [&x] () int {
		return *x
	}
}

fn main() {}
'
	exit_code, output := run_escape_check(code)
	assert exit_code != 0, 'pushing closure-borrow into outer array should fail'
	assert output.contains('pushed into `callbacks`'), 'should name the array, got: ${output}'
	assert output.contains('captured by reference into closure'), 'got: ${output}'
}

fn test_escape_closure_mut_capture_errors() {
	// `[mut x]` is treated as a borrow (matches the ownership-checker
	// convention), so the same escape rule applies.
	code := '
fn dangling[^a]() fn () int {
	mut x := 7
	return fn [mut x] () int {
		x = x + 1
		return x
	}
}

fn main() {}
'
	exit_code, output := run_escape_check(code)
	assert exit_code != 0, 'returning closure with mut-borrow capture should fail'
	assert output.contains('captured by mutable reference into closure'), 'got: ${output}'
}

// ---------------------------------------------------------------------------
// Inter-procedural escape: a fn that returns one of its parameters laundering
// a `&local` borrow back to the caller. The pre-pass identifies pass-through
// fns; the call-site check fires for the return / field-store / array-push
// shapes the existing escape analyser already understands.

fn test_escape_return_passthrough_local_errors() {
	// `passthrough` returns its param. Calling it with `&local` would leak
	// a borrow of the body-local through the return value.
	code := '
struct Bar {
	v int
}

fn passthrough[^a](p &^a Bar) &^a Bar {
	return p
}

fn dangling[^a](caller &^a Bar) &^a Bar {
	local := Bar{v: 1}
	return passthrough(&local)
}

fn main() {}
'
	exit_code, output := run_escape_check(code)
	assert exit_code != 0, 'returning passthrough(&local) should fail'
	assert output.contains('local variable `local`'), 'should name the local, got: ${output}'
	assert output.contains('passed through `passthrough`'), 'should name the pass-through fn, got: ${output}'
	assert output.contains('does not survive `dangling`'), 'should name the outer fn, got: ${output}'
}

fn test_escape_field_store_passthrough_local_errors() {
	// Same rule for `outer.f = passthrough(&local)`.
	code := '
struct Bar {
	v int
}

struct Holder {
mut:
	r &Bar = unsafe { nil }
}

fn passthrough[^a](p &^a Bar) &^a Bar {
	return p
}

fn dangling[^a](mut h Holder) {
	local := Bar{v: 1}
	h.r = passthrough(&local)
}

fn main() {}
'
	exit_code, output := run_escape_check(code)
	assert exit_code != 0, 'storing passthrough(&local) into h.r should fail'
	assert output.contains('stored into `h.r`'), 'should name the LHS path, got: ${output}'
	assert output.contains('passed through `passthrough`'), 'got: ${output}'
}

fn test_escape_array_push_passthrough_local_errors() {
	// And for `outer_arr << passthrough(&local)`.
	code := '
struct Bar {
	v int
}

fn passthrough[^a](p &^a Bar) &^a Bar {
	return p
}

fn collect[^a](mut out []&Bar) {
	local := Bar{v: 1}
	out << passthrough(&local)
}

fn main() {}
'
	exit_code, output := run_escape_check(code)
	assert exit_code != 0, 'pushing passthrough(&local) into outer array should fail'
	assert output.contains('pushed into `out`'), 'should name the array, got: ${output}'
	assert output.contains('passed through `passthrough`'), 'got: ${output}'
}

fn test_escape_passthrough_with_param_borrow_ok() {
	// Same call shape, but the arg is `&caller` (a parameter borrow) — params
	// outlive the call from the caller'\''s perspective, so this is safe.
	code := '
struct Bar {
	v int
}

fn passthrough[^a](p &^a Bar) &^a Bar {
	return p
}

fn safe_chain[^a](caller &^a Bar) &^a Bar {
	return passthrough(caller)
}

fn main() {}
'
	exit_code, _ := run_escape_check(code)
	assert exit_code == 0, '`return passthrough(caller)` should be allowed (param outlives the call)'
}

fn test_escape_non_passthrough_call_ok() {
	// A non-pass-through call (returns a fresh value) hides nothing — even if
	// its arg is `&local`, the return value does not borrow from it.
	code := '
struct Bar {
	v int
}

fn make_copy[^a](p &^a Bar) Bar {
	return Bar{v: p.v}
}

fn safe[^a](caller &^a Bar) Bar {
	local := Bar{v: 1}
	return make_copy(&local)
}

fn main() {}
'
	exit_code, _ := run_escape_check(code)
	assert exit_code == 0, '`return make_copy(&local)` should be allowed (non-pass-through callee)'
}

fn test_escape_passthrough_non_optin_call_ignored() {
	// The callee `helper` does NOT declare `[^a]`, so it is not added to the
	// pass-through registry. The call site falls through to the existing
	// rules; nothing else triggers, so this compiles cleanly.
	code := '
struct Bar {
	v int
}

fn helper(p &Bar) &Bar {
	return p
}

fn caller[^a](outer &^a Bar) &Bar {
	local := Bar{v: 1}
	return helper(&local)
}

fn main() {}
'
	exit_code, _ := run_escape_check(code)
	assert exit_code == 0, 'non-opt-in callees are not tracked, so the call site is not flagged'
}
