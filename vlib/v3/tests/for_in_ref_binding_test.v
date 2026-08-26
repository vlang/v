import os
import rand

// Regression coverage for #28084: `for x in &arr` must bind elements by
// reference, not by value. The checker's `for_in_iterable_yields_ref` used to
// be a dead stub that always returned false, so `annotate_for_in` never
// recorded the loop variable as pointer-typed for this case and mutations
// through it were silently lost.

const for_in_ref_binding_vexe = @VEXE
const for_in_ref_binding_tests_dir = os.dir(@FILE)
const for_in_ref_binding_v3_dir = os.dir(for_in_ref_binding_tests_dir)
const for_in_ref_binding_vlib_dir = os.dir(for_in_ref_binding_v3_dir)
const for_in_ref_binding_v3_src = os.join_path(for_in_ref_binding_v3_dir, 'v3.v')

fn for_in_ref_binding_v3_bin_path() string {
	return os.join_path(os.temp_dir(), 'v3_for_in_ref_binding_test')
}

fn testsuite_begin() {
	v3_bin := for_in_ref_binding_v3_bin_path()
	if os.exists(v3_bin) {
		os.rm(v3_bin) or {}
	}
}

fn build_v3_for_in_ref_binding() string {
	v3_bin := for_in_ref_binding_v3_bin_path()
	if os.exists(v3_bin) {
		return v3_bin
	}
	build :=
		os.execute('${for_in_ref_binding_vexe} -gc none -path "${for_in_ref_binding_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${for_in_ref_binding_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn for_in_ref_binding_temp_path(name string) string {
	return os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}_${rand.ulid()}')
}

fn for_in_ref_binding_run_good(v3_bin string, name string, src string) string {
	out := for_in_ref_binding_temp_path(name)
	src_path := out + '.v'
	os.write_file(src_path, src) or { panic(err) }
	compile := os.execute('${v3_bin} ${src_path} -b c -o ${out}')
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed\n${compile.output}'
	run := os.execute(out)
	assert run.exit_code == 0, '${name}: run failed\n${run.output}'
	return run.output.trim_space()
}

fn for_in_ref_binding_run_bad(v3_bin string, name string, src string, expected string) {
	out := for_in_ref_binding_temp_path(name)
	src_path := out + '.v'
	os.write_file(src_path, src) or { panic(err) }
	result := os.execute('${v3_bin} ${src_path} -b c -o ${out}')
	assert result.exit_code != 0, '${name}: expected failure, got success\n${result.output}'
	assert result.output.contains(expected), '${name}: expected `${expected}` in\n${result.output}'
}

// A local fixed array cannot be referenced outside `unsafe` at all (it is
// stack-stored), independent of for-in - this must keep failing the same way
// it does for any other `&` of a local fixed array.
fn test_for_in_ref_of_local_fixed_array_requires_unsafe() {
	v3_bin := build_v3_for_in_ref_binding()
	for_in_ref_binding_run_bad(v3_bin, 'local_fixed_array_ref',
		'struct Item {\nmut:\n\tval int\n}\n\nfn main() {\n\tmut arr := [3]Item{}\n\tfor item in &arr {\n\t\tprintln(item.val)\n\t}\n}\n',
		'cannot reference fixed array `arr` outside `unsafe` blocks')
}

// A fixed array reached through a struct field is addressable without
// `unsafe`; `for item in &t.buf` must mutate the underlying elements in
// place, matching the exact shape of the #28084 report.
fn test_for_in_ref_mutates_fixed_array_struct_field() {
	v3_bin := build_v3_for_in_ref_binding()
	out := for_in_ref_binding_run_good(v3_bin, 'fixed_array_struct_field_ref',
		'struct Item {\nmut:\n\tval int\n}\n\nstruct Test {\nmut:\n\tbuf [10]Item\n}\n\nfn main() {\n\tmut t := Test{}\n\tfor i in 0 .. 10 {\n\t\tt.buf[i] = Item{val: i}\n\t}\n\tfor item in &t.buf {\n\t\tunsafe {\n\t\t\titem.val = item.val * 100\n\t\t}\n\t}\n\tmut total := 0\n\tfor item in t.buf {\n\t\ttotal += item.val\n\t}\n\tprintln(total)\n}\n')
	assert out == '4500'
}

// Ordinary dynamic arrays already worked before the fix; this guards against
// regressing that path while changing how the ref/value decision is made.
fn test_for_in_ref_mutates_dynamic_array() {
	v3_bin := build_v3_for_in_ref_binding()
	out := for_in_ref_binding_run_good(v3_bin, 'dynamic_array_ref',
		'struct Item {\nmut:\n\tval int\n}\n\nfn main() {\n\tmut arr := []Item{}\n\tfor i in 0 .. 5 {\n\t\tarr << Item{val: i}\n\t}\n\tfor item in &arr {\n\t\tunsafe {\n\t\t\titem.val = item.val * 100\n\t\t}\n\t}\n\tmut total := 0\n\tfor item in arr {\n\t\ttotal += item.val\n\t}\n\tprintln(total)\n}\n')
	assert out == '1000'
}

// `for mut item in arr` is a distinct binding mode from `for item in &arr`:
// both should be able to mutate through the loop variable.
fn test_for_mut_item_in_plain_array_still_mutates() {
	v3_bin := build_v3_for_in_ref_binding()
	out := for_in_ref_binding_run_good(v3_bin, 'for_mut_item_plain_array',
		'struct Item {\nmut:\n\tval int\n}\n\nfn main() {\n\tmut arr := [3]Item{}\n\tfor i in 0 .. 3 {\n\t\tarr[i] = Item{val: i + 1}\n\t}\n\tfor mut item in arr {\n\t\titem.val = item.val * 10\n\t}\n\tmut total := 0\n\tfor item in arr {\n\t\ttotal += item.val\n\t}\n\tprintln(total)\n}\n')
	assert out == '60'
}

// A `mut` array *parameter* stores its underlying array via a pointer at the
// ABI level even without any `&` in the for-in line. That must not be
// mistaken for an explicit `for x in &arr` - plain `for item in arr` inside
// such a function still binds `item` by value.
fn test_for_in_over_mut_param_without_amp_is_still_by_value() {
	v3_bin := build_v3_for_in_ref_binding()
	for_in_ref_binding_run_bad(v3_bin, 'mut_param_no_amp_by_value',
		'struct Item {\nmut:\n\tval int\n}\n\nfn touch(mut arr []Item) {\n\tfor item in arr {\n\t\titem.val = item.val * 1000\n\t}\n}\n\nfn main() {\n\tmut arr := []Item{}\n\tarr << Item{val: 1}\n\ttouch(mut arr)\n}\n',
		'`item` is immutable, declare it with `mut` to make it mutable')
}

// The two-loop-variable form (`for idx, item in &arr`) binds the *value*
// variable by reference, not the index - a separate code path from the
// single-variable form covered above (`annotate_for_in` inserts the index as
// a plain `int` regardless, and only wraps the value/element variable).
fn test_for_in_ref_indexed_form_mutates_value_var() {
	v3_bin := build_v3_for_in_ref_binding()
	out := for_in_ref_binding_run_good(v3_bin, 'for_in_ref_indexed_form',
		'struct Item {\nmut:\n\tval int\n}\n\nfn main() {\n\tmut arr := []Item{}\n\tfor i in 0 .. 4 {\n\t\tarr << Item{val: i}\n\t}\n\tfor idx, item in &arr {\n\t\tunsafe {\n\t\t\titem.val = item.val + idx\n\t\t}\n\t}\n\tmut total := 0\n\tfor item in arr {\n\t\ttotal += item.val\n\t}\n\tprintln(total)\n}\n')
	assert out == '12'
}

// transform/for.v now reads the element-binding mode back from the loop
// variable's own checker-resolved type (via raw_checker_node_type) instead
// of re-inspecting the container's raw AST shape, falling back to the AST
// shape when that lookup comes back empty. A monomorphized generic method's
// loop-variable node id is a clone created after the checker's original
// annotation pass ran on the generic template, and its cached type is not
// always available by the time transform runs - this must still bind by
// reference via the AST-shape fallback in that case.
fn test_for_in_ref_survives_generic_monomorphization() {
	v3_bin := build_v3_for_in_ref_binding()
	out := for_in_ref_binding_run_good(v3_bin, 'for_in_ref_generic_monomorphization',
		'struct Item {\nmut:\n\tval int\n}\n\nstruct Container[T] {\nmut:\n\titems []T\n}\n\nfn (mut c Container[T]) bump_all() {\n\tfor item in &c.items {\n\t\tunsafe {\n\t\t\titem.val = item.val * 10\n\t\t}\n\t}\n}\n\nfn main() {\n\tmut c := Container[Item]{}\n\tc.items << Item{val: 1}\n\tc.items << Item{val: 2}\n\tc.items << Item{val: 3}\n\tc.bump_all()\n\tmut total := 0\n\tfor it in c.items {\n\t\ttotal += it.val\n\t}\n\tprintln(total)\n}\n')
	assert out == '60'
}
