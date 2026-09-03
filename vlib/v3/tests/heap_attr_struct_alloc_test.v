import os
import rand
import v3.cmdexec

// Regression coverage for #28084: a `@[heap]`-tagged struct must always be
// heap-allocated at its own declaration (`t := Test{}`), not only when its
// address is later taken (`&Test{}`). Per doc/docs.md, `@[heap]` is meant as
// an unconditional promise used for escape-safety, but nothing in
// transform/codegen enforced it for plain declarations before this fix -
// heap_attr_struct_type/transform_decl_assign_stmt now hook the existing
// escape-analysis heap-promotion path (heap_escaping_source_decl)
// unconditionally for such types.

const heap_attr_vexe = @VEXE
const heap_attr_tests_dir = os.dir(@FILE)
const heap_attr_v3_dir = os.dir(heap_attr_tests_dir)
const heap_attr_vlib_dir = os.dir(heap_attr_v3_dir)
const heap_attr_v3_src = os.join_path(heap_attr_v3_dir, 'v3.v')

// heap_attr_v3_bin_path is process-unique (pid-qualified) so a concurrent
// invocation of this same file (a different `v test` process, e.g. two
// overlapping CI jobs on one machine) can never race this one for the same
// output path, on top of testsuite_begin building it exactly once before any
// test_* function in THIS process runs.
fn heap_attr_v3_bin_path() string {
	return os.join_path(os.temp_dir(), 'v3_heap_attr_struct_alloc_test_${os.getpid()}')
}

fn testsuite_begin() {
	v3_bin := heap_attr_v3_bin_path()
	os.rm(v3_bin) or {}
	build := cmdexec.run(heap_attr_vexe, ['-old-compiler', '-gc', 'none', '-path',
		'${heap_attr_vlib_dir}|@vlib|@vmodules', '-o', v3_bin, heap_attr_v3_src])
	assert build.exit_code == 0, build.output
}

fn testsuite_end() {
	os.rm(heap_attr_v3_bin_path()) or {}
}

// build_v3_for_heap_attr returns the v3 binary testsuite_begin already built.
fn build_v3_for_heap_attr() string {
	return heap_attr_v3_bin_path()
}

fn heap_attr_temp_path(name string) string {
	return os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}_${rand.ulid()}')
}

fn heap_attr_run_good(v3_bin string, name string, src string) string {
	out := heap_attr_temp_path(name)
	src_path := out + '.v'
	os.write_file(src_path, src) or { panic(err) }
	compile := cmdexec.run(v3_bin, [src_path, '-b', 'c', '-o', out])
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed\n${compile.output}'
	run := cmdexec.run(out, []string{})
	assert run.exit_code == 0, '${name}: run failed\n${run.output}'
	return run.output.trim_space()
}

// heap_attr_generated_c translates `src` to C only (no cc invocation), so
// tests can inspect the generated storage/allocation shape directly instead
// of inferring it indirectly from runtime behavior.
fn heap_attr_generated_c(v3_bin string, name string, src string) string {
	base := heap_attr_temp_path(name)
	src_path := base + '.v'
	c_path := base + '.c'
	os.write_file(src_path, src) or { panic(err) }
	translate := cmdexec.run(v3_bin, [src_path, '-b', 'c', '-o', c_path])
	assert translate.exit_code == 0, '${name}: translate to C failed\n${translate.output}'
	return os.read_file(c_path) or { panic(err) }
}

// A plain `t := Test{}` declaration of a `@[heap]`-tagged struct must
// allocate `t` itself on the heap (a pointer-typed local backed by a real
// allocation), while an otherwise-identical non-tagged struct keeps its
// ordinary stack-value declaration.
fn test_heap_attr_struct_decl_is_heap_allocated() {
	v3_bin := build_v3_for_heap_attr()
	c_src := heap_attr_generated_c(v3_bin, 'heap_attr_decl_shape',
		'@[heap]\nstruct Tagged {\nmut:\n\tval int\n}\n\nstruct Plain {\nmut:\n\tval int\n}\n\nfn main() {\n\ttagged := Tagged{val: 5}\n\tplain := Plain{val: 5}\n\tprintln(tagged.val + plain.val)\n}\n')
	assert c_src.contains('main__Tagged* tagged = (main__Tagged*)memdup('), 'expected heap allocation for @[heap]-tagged struct decl, got:\n${c_src}'

	assert c_src.contains('main__Plain plain = (main__Plain){'), 'expected plain stack-value decl for untagged struct, got:\n${c_src}'
}

// `y := t` from a `@[heap]` struct must produce an independent deep copy,
// not a second reference to the same heap block - mutating the copy must
// not be visible through the original.
fn test_heap_attr_struct_assign_is_deep_copy() {
	v3_bin := build_v3_for_heap_attr()
	out := heap_attr_run_good(v3_bin, 'heap_attr_deep_copy',
		'@[heap]\nstruct Test {\nmut:\n\tval int\n}\n\nfn main() {\n\tt := Test{val: 1}\n\tmut y := t\n\ty.val = 99\n\tprintln(t.val)\n\tprintln(y.val)\n}\n')
	assert out == '1\n99'
}

// String interpolation of a heap-promoted local must read its pointed-to
// value, not print pointer/nil-check machinery - a heap-promoted local is
// still read as a plain value everywhere else (pointer_value_rvalues).
fn test_heap_attr_struct_string_interpolation() {
	v3_bin := build_v3_for_heap_attr()
	out := heap_attr_run_good(v3_bin, 'heap_attr_string_interp',
		"@[heap]\nstruct Test {\nmut:\n\tname string\n\tval  int\n}\n\nfn main() {\n\tt := Test{name: 'x', val: 7}\n\tprintln('\${t.val}')\n\tprintln(t)\n}\n")
	assert out.starts_with('7\n')
	assert out.contains('name:')
	assert out.contains('val: 7')
}

// The heap promise must survive the declaring function returning - a
// pointer obtained from a `@[heap]` local remains valid and correct after
// its enclosing scope has returned, matching the escape-safety guarantee
// documented for `@[heap]`.
fn test_heap_attr_struct_pointer_survives_declaring_scope() {
	v3_bin := build_v3_for_heap_attr()
	out := heap_attr_run_good(v3_bin, 'heap_attr_escape_safety',
		'@[heap]\nstruct Test {\nmut:\n\tval int\n\tpad [256]u8\n}\n\nfn make(v int) &Test {\n\tt := Test{val: v}\n\treturn &t\n}\n\nfn other_work() int {\n\tmut junk := [256]u8{}\n\tfor i in 0 .. junk.len {\n\t\tjunk[i] = u8(i)\n\t}\n\treturn int(junk[255])\n}\n\nfn main() {\n\tr := make(42)\n\t_ = other_work()\n\tprintln(r.val)\n}\n')
	assert out == '42'
}

// Exact reproduction of the #28084 report: a `@[heap]`-tagged struct with a
// large fixed-array field, declared with a plain `t := Test{}`, then summed
// via `for i in &t.buf` - both fixes together must compile and run correctly.
fn test_heap_attr_issue_28084_exact_repro() {
	v3_bin := build_v3_for_heap_attr()
	out := heap_attr_run_good(v3_bin, 'heap_attr_28084_repro',
		'struct Item {\nmut:\n\tx f32\n}\n\n@[heap]\nstruct Test {\nmut:\n\tbuf [100000]Item\n}\n\nfn main() {\n\tmut t := Test{}\n\tfor i in 0 .. 100000 {\n\t\tt.buf[i] = Item{x: f32(i)}\n\t}\n\tmut sum := f32(0)\n\tfor i in &t.buf {\n\t\tsum += i.x\n\t}\n\tprintln(sum > f32(4_999_000_000))\n}\n')
	assert out == 'true'
}
