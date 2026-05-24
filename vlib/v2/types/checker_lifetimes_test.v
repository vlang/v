// Tests for the Phase 1 lifetime validator (vlib/v2/types/checker_lifetimes.v).
// Built with `-d ownership`. The `run_lifetime_check` helper below is a
// local copy of `run_ownership_check` from checker_ownership_test.v —
// `v test` compiles each test file in isolation, so module-private helpers
// don't cross file boundaries.
module types

import os

fn v2_ownership_lifetimes_exe() string {
	vroot := @VMODROOT
	// Use a test-file-unique binary name so parallel `v test` runs of
	// sibling test files don't race on the same output path.
	v2_ownership := os.join_path(vroot, 'cmd', 'v2', 'v2_ownership_lifetimes')
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

fn run_lifetime_check(code string) (int, string) {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_lifetime_test_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic('cannot create ${tmp_dir}') }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	tmp_file := os.join_path(tmp_dir, 'test.v')
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	v2 := v2_ownership_lifetimes_exe()
	res :=
		os.execute('${os.quoted_path(v2)} -ownership -o ${os.join_path(tmp_dir, 'out')} ${os.quoted_path(tmp_file)} 2>&1')
	return res.exit_code, res.output
}

fn test_lifetime_declared_and_used_ok() {
	// Canonical "good" signature: declared `[^a]`, used in both an input
	// reference and the return type. Must compile without error.
	code := '
struct Holder {
	value int
}

fn first[^a](h &^a Holder) &^a Holder {
	return h
}

fn main() {
	h := Holder{value: 1}
	r := first(&h)
	println(r.value)
}
'
	exit_code, output := run_lifetime_check(code)
	assert exit_code == 0, 'valid lifetime signature should compile: ${output}'
}

fn test_lifetime_undeclared_in_param_errors() {
	// `^a` is used on the parameter type but never declared in the generic
	// params. Phase 1 catches this with a clear "undeclared lifetime"
	// diagnostic.
	code := '
struct Holder {
	value int
}

fn bad(h &^a Holder) int {
	return h.value
}

fn main() {
	h := Holder{value: 1}
	println(bad(&h))
}
'
	exit_code, output := run_lifetime_check(code)
	assert exit_code != 0, 'undeclared lifetime should fail'
	assert output.contains('undeclared lifetime `^a`'), 'got: ${output}'
	assert output.contains('fn `bad`'), 'should name the function, got: ${output}'
}

fn test_lifetime_undeclared_in_return_errors() {
	code := '
struct Holder {
	value int
}

fn bad(h Holder) &^a Holder {
	return &h
}

fn main() {}
'
	exit_code, output := run_lifetime_check(code)
	assert exit_code != 0, 'undeclared lifetime in return should fail'
	assert output.contains('undeclared lifetime `^a`'), 'got: ${output}'
}

fn test_lifetime_duplicate_decl_errors() {
	// Repeated lifetime in the generic-param list — typo / cut-and-paste
	// hazard. Caught at validation time.
	code := '
struct Holder {
	value int
}

fn dup[^a, ^a](h &^a Holder) &^a Holder {
	return h
}

fn main() {}
'
	exit_code, output := run_lifetime_check(code)
	assert exit_code != 0, 'duplicate lifetime declaration should fail'
	assert output.contains('lifetime `^a` declared more than once'), 'got: ${output}'
}

fn test_lifetime_return_unbound_errors() {
	// The return type mentions `^a`, but no parameter is borrowed at
	// `^a`. Signature-level "no dangling reference": the caller cannot
	// own the value the returned reference would point to.
	code := '
struct Holder {
	value int
}

fn fabricate[^a]() &^a Holder {
	h := Holder{value: 1}
	return &h
}

fn main() {}
'
	exit_code, output := run_lifetime_check(code)
	assert exit_code != 0, 'unbound return lifetime should fail'
	assert output.contains('references lifetime `^a` that does not appear in any parameter'), 'got: ${output}'
}

fn test_lifetime_method_receiver_counts_as_input() {
	// Methods: the receiver IS an input, so a method returning `&^a T`
	// is valid as long as the receiver is `&^a Self`.
	code := '
struct Holder[^a] {
	value int
}

fn (h &^a Holder[^a]) borrow[^a]() &^a Holder[^a] {
	return h
}

fn main() {}
'
	exit_code, output := run_lifetime_check(code)
	assert exit_code == 0, 'receiver should count as an input lifetime: ${output}'
}

fn test_lifetime_struct_field_undeclared_errors() {
	// Struct fields whose types reference a lifetime require that
	// lifetime to be declared on the struct's generic-param list.
	code := '
struct Inner {
	v int
}

struct BadHolder {
	inner &^a Inner
}

fn main() {}
'
	exit_code, output := run_lifetime_check(code)
	assert exit_code != 0, 'undeclared lifetime in struct field should fail'
	assert output.contains('undeclared lifetime `^a`'), 'got: ${output}'
	assert output.contains('struct `BadHolder`'), 'should name the struct, got: ${output}'
}

fn test_lifetime_struct_field_declared_ok() {
	code := '
struct Inner {
	v int
}

struct GoodHolder[^a] {
	inner &^a Inner
}

fn main() {}
'
	exit_code, output := run_lifetime_check(code)
	assert exit_code == 0, 'struct with properly declared lifetime should compile: ${output}'
}

fn test_lifetime_nested_in_generic_args_validated() {
	// Lifetimes appearing inside generic arguments (`Match[Foo[^a]]`)
	// are walked recursively and validated like any other use site.
	code := '
struct Inner[^a] {
	v int
}

struct Wrap[^a] {
	x Inner[^b]
}

fn main() {}
'
	exit_code, output := run_lifetime_check(code)
	assert exit_code != 0, 'nested undeclared lifetime should fail'
	assert output.contains('undeclared lifetime `^b`'), 'got: ${output}'
}

fn test_lifetime_no_lifetimes_unaffected() {
	// Code without any lifetime annotations must be entirely unaffected
	// by the new validator.
	code := '
struct Plain {
	x int
}

fn twice(p Plain) int {
	return p.x * 2
}

fn main() {
	p := Plain{x: 21}
	println(twice(p))
}
'
	exit_code, output := run_lifetime_check(code)
	assert exit_code == 0, 'plain code must not be affected: ${output}'
}

// ----- Phase 2: elision / anonymous lifetimes / variance -----

fn test_lifetime_elision_single_input_ok() {
	// Opt-in elision: the fn declares `[^a]`, so elision rules apply.
	// Single input reference + reference return → return implicitly
	// borrows from that input. No explicit `^a` needed at use sites.
	code := '
struct Holder {
	value int
}

fn first[^a](h &Holder) &Holder {
	return h
}

fn main() {
	h := Holder{value: 1}
	r := first(&h)
	println(r.value)
}
'
	exit_code, output := run_lifetime_check(code)
	assert exit_code == 0, 'single-input elision should compile: ${output}'
}

fn test_lifetime_anonymous_lifetime_ok() {
	// `&^_ T` is the placeholder ("anonymous") form — explicitly opting
	// into elision at the use site. Equivalent to a bare `&T` slot under
	// an opted-in signature.
	code := '
struct Holder {
	value int
}

fn first[^a](h &^_ Holder) &^_ Holder {
	return h
}

fn main() {
	h := Holder{value: 1}
	r := first(&h)
	println(r.value)
}
'
	exit_code, output := run_lifetime_check(code)
	assert exit_code == 0, 'anonymous lifetime should compile: ${output}'
}

fn test_lifetime_elision_method_receiver_ok() {
	// On methods, the receiver counts as the input lifetime when
	// elision needs to source a return lifetime.
	code := '
struct Cell {
	value int
}

fn (c &Cell) borrow[^a]() &Cell {
	return c
}

fn main() {
	c := Cell{value: 1}
	r := c.borrow()
	println(r.value)
}
'
	exit_code, output := run_lifetime_check(code)
	assert exit_code == 0, 'method-receiver elision should compile: ${output}'
}

fn test_lifetime_elision_no_input_errors() {
	// Return mentions a reference, but no parameter is borrowed —
	// the caller would receive a dangling reference. Caught at the
	// signature level before any body analysis.
	code := '
struct Holder {
	value int
}

fn fabricate[^a]() &Holder {
	return &Holder{value: 1}
}

fn main() {}
'
	exit_code, output := run_lifetime_check(code)
	assert exit_code != 0, 'no-input elided return should fail'
	assert output.contains('takes no reference parameters'), 'got: ${output}'
	assert output.contains('fn `fabricate`'), 'should name the function, got: ${output}'
}

fn test_lifetime_elision_ambiguous_errors() {
	// Two input references with distinct lifetimes — elision rule 2
	// only applies when there is exactly one. User must annotate.
	code := '
struct A {
	x int
}

struct B {
	y int
}

struct Out {
	z int
}

fn pick[^a](a &A, b &B) &Out {
	return &Out{z: a.x + b.y}
}

fn main() {}
'
	exit_code, output := run_lifetime_check(code)
	assert exit_code != 0, 'ambiguous elided return should fail'
	assert output.contains('cannot infer return lifetime'), 'got: ${output}'
	assert output.contains('fn `pick`'), 'should name the function, got: ${output}'
}

fn test_lifetime_elision_opt_in_legacy_unchanged() {
	// CRITICAL: legacy code without `[^a]` must NOT be affected by
	// elision rules. V has many existing fns like `fn (a array)
	// data_header() &ArrayDataHeader` (value receiver, ref return)
	// that would fail Rust-style strict elision. Opt-in keeps them
	// untouched.
	code := '
struct Thing {
	v int
}

fn legacy(x &Thing) &Thing {
	return x
}

fn (t Thing) by_val_returns_ref() &Thing {
	return &Thing{v: t.v}
}

fn no_params_returns_ref() &Thing {
	return &Thing{v: 0}
}

fn main() {}
'
	exit_code, output := run_lifetime_check(code)
	assert exit_code == 0, 'un-opted-in legacy code must not be affected: ${output}'
}
