import os

const selfhost_regression_vexe = @VEXE
const selfhost_regression_tests_dir = os.dir(@FILE)
const selfhost_regression_v3_dir = os.dir(selfhost_regression_tests_dir)
const selfhost_regression_vlib_dir = os.dir(selfhost_regression_v3_dir)
const selfhost_regression_v3_src = os.join_path(selfhost_regression_v3_dir, 'v3.v')

fn selfhost_regression_v3_bin_path() string {
	return os.join_path(os.temp_dir(), 'v3_selfhost_transform_regression_test')
}

fn testsuite_begin() {
	os.rm(selfhost_regression_v3_bin_path()) or {}
}

fn selfhost_regression_build_v3() string {
	v3_bin := selfhost_regression_v3_bin_path()
	if os.exists(v3_bin) {
		return v3_bin
	}
	build := os.execute('${selfhost_regression_vexe} -gc none -path "${selfhost_regression_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${selfhost_regression_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn selfhost_regression_run(name string, source string) string {
	v3_bin := selfhost_regression_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_selfhost_regression_${name}.v')
	bin := os.join_path(os.temp_dir(), 'v3_selfhost_regression_${name}')
	os.write_file(src, source) or { panic(err) }
	compile := os.execute('${v3_bin} -nocache -b c -o ${bin} ${src}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

// A value `if`/`match` call argument is materialized into a value temp, and the call is rebuilt
// over the materialized operands and re-dispatched. In a generic clone the checker has no
// recorded type for an enum-shorthand `if`, so the materialization falls back to plain
// `transform_expr`, which rebuilds the branch unchanged in shape. The re-dispatch used to see a
// changed-but-still-a-branch operand and recurse forever, overflowing the stack.
fn test_untyped_enum_branch_argument_in_generic_clone_terminates() {
	out := selfhost_regression_run('untyped_enum_branch_arg', 'enum Op {
	dot
	arrow
}

struct Sel {
mut:
	n int
}

fn (mut s Sel) make_selector_op(base int, field string, typ string, op Op) string {
	s.n++
	return "\${base}-\${field}-\${typ}-\${op}"
}

fn (mut s Sel) wrap[U](v U, expr_type string) string {
	base := s.n + 1
	field := "\${v}"
	field_typ := "x\${field}"
	return s.make_selector_op(base, field, field_typ, if expr_type.starts_with("&") {
		.arrow
	} else {
		.dot
	})
}

fn main() {
	mut s := Sel{}
	println(s.wrap(1, "&int"))
	println(s.wrap("s", "int"))
}
')
	assert out.split_into_lines() == ['1-1-x1-arrow', '2-s-xs-dot']
}

// A module-qualified callee (`os.abs_path(...)`) is a selector whose base names an import, not a
// value. When a later argument hoists a value branch, the source-order guards used to snapshot
// that base into a temp, emitting `unknown __order_snapshot_0 = os;`.
fn test_module_qualified_call_with_branch_argument() {
	out := selfhost_regression_run('module_qualified_branch_arg', 'import os

fn pick(name string) !string {
	if name.len == 0 {
		return error("empty")
	}
	return name
}

fn base_dir(name string) string {
	return os.abs_path(pick(name) or {
		if os.getenv("V3_SELFHOST_REGRESSION_UNSET") == "1" {
			"/tmp/a"
		} else {
			"/tmp/b"
		}
	})
}

fn main() {
	println(base_dir("") == os.abs_path("/tmp/b"))
	println(base_dir("/tmp/c") == os.abs_path("/tmp/c"))
}
')
	assert out.split_into_lines() == ['true', 'true']
}

// Only `for k, mut v in m` binds the map value by reference. A container that is merely a map
// reference (`m &map[string]bool`) still binds a plain value copy, so the binding must not be
// typed `&V` — that made every use of it emit a dereference of a non-pointer local.
fn test_for_in_over_map_reference_binds_value() {
	out := selfhost_regression_run('for_in_map_reference', 'fn count_used(used &map[string]bool) int {
	mut n := 0
	for name, is_used in used {
		if !is_used || name.len == 0 {
			continue
		}
		n++
	}
	return n
}

fn double_values(mut m map[string]int) {
	for _, mut v in m {
		v = v * 2
	}
}

fn main() {
	flags := {
		"a": true
		"b": false
		"c": true
	}
	println(count_used(&flags))
	mut counts := {
		"a": 1
		"b": 2
	}
	double_values(mut counts)
	println(counts["a"])
	println(counts["b"])
}
')
	assert out.split_into_lines() == ['2', '2', '4']
}

// `mod.Type.make(...)` is a static associated call: its callee base is the *selector* `mod.Type`,
// which names a type, not a value. `static_assoc_fn_name` recognizes that selector shape, so the
// namespace guard must consult it for selector bases too — otherwise the call is classified as a
// method and a value-branch argument makes the ordering guards snapshot the type name as a
// receiver (`void __order_snapshot_0 = shapes__Box;`).
fn test_module_qualified_static_assoc_call_with_branch_argument() {
	v3_bin := selfhost_regression_build_v3()
	root := os.join_path(os.temp_dir(), 'v3_selfhost_regression_static_assoc_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'shapes')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'selfhost_regression_static_assoc' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'shapes/shapes.v'), 'module shapes

pub struct Box {
pub:
	w int
	h int
}

pub fn Box.make(w int) Box {
	return Box{
		w: w
		h: w * 2
	}
}
') or { panic(err) }
	main_path := os.join_path(root, 'main.v')
	os.write_file(main_path, 'module main

import shapes

fn pick(n int) !int {
	if n < 0 {
		return error("neg")
	}
	return n
}

fn build(n int) shapes.Box {
	return shapes.Box.make(pick(n) or {
		if n == -1 {
			7
		} else {
			9
		}
	})
}

fn main() {
	b := build(3)
	println("\${b.w}-\${b.h}")
	c := build(-1)
	println("\${c.w}-\${c.h}")
}
') or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_selfhost_regression_static_assoc_bin_${os.getpid()}')
	compile := os.execute('${v3_bin} -nocache -b c -o ${bin} ${main_path}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().split_into_lines() == ['3-6', '7-14']
}

// `strings.new_builder(if cond { a } else { b })` — the shape `strconv.format_es_old` uses — is a
// module-qualified call whose argument is a value `if`. The ordering guards used to snapshot the
// `strings` base into a temp typed by whatever the checker recorded for a module identifier,
// emitting `strconv__unknown __order_snapshot_1 = strings;`.
//
// This pins the reported program against a regression in the namespace classifier, which is what
// resolves this shape. The backstop behind the classifier is covered directly by
// vlib/v3/transform/ordering_guard_test.v.
fn test_module_qualified_call_with_branch_argument_in_imported_module() {
	out := selfhost_regression_run('module_qualified_branch_arg_imported', 'import strconv

fn main() {
	x := 3.141516
	println(unsafe { strconv.v_sprintf("aaa %G", x) })
	println(unsafe { strconv.v_sprintf("bbb %08.3f", x) })
}
')
	assert out.split_into_lines() == ['aaa 3.141516', 'bbb 0003.142']
}
