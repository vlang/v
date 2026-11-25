import os
import v.tempname

fn checker_assignment_fixture_dir() string {
	root := os.join_path(os.vtmp_dir(), 'v3_checker_assignment_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	return root
}

fn checker_assignment_run_bad(name string, source string, expected string) {
	root := checker_assignment_fixture_dir()
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_${name}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(root, 'v3_${name}')
	compile := os.exec([@VEXE, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains(expected), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn checker_assignment_run_good(name string, source string) string {
	root := checker_assignment_fixture_dir()
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_${name}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(root, 'v3_${name}')
	compile := os.exec([@VEXE, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_assignment_invalidates_written_lhs_smartcast() {
	checker_assignment_run_bad('bad_assignment_stale_smartcast', 'struct Foo {
	foo int
}

struct Bar {}

type Value = Bar | Foo

fn main() {
	mut x := Value(Foo{
		foo: 1
	})
	if x is Foo {
		x = Bar{}
		println(int_str(x.foo))
	}
}
',
		'field `foo` does not exist')
}

fn test_optional_assignment_keeps_wrapper_and_qualified_optional_cast_is_known() {
	out := checker_assignment_run_good('optional_assignment_and_qualified_cast', 'import time

type Values = []int

struct Holder {
	value  ?time.Time
	values ?Values
}

fn main() {
	mut first := ?int(none)
	mut second := ?int(none)
	first = 1
	second = 2
	assert first? == 1
	assert second? == 2
	copy := first
	assert copy? == first?
	holder := Holder{}
	casted := holder.value as ?time.Time
	assert casted == none
	if holder.values != none {
		assert holder.values ?.str() == "Values([])"
	}
	mut absent := ?map[string]string{}
	fallback := {
		"foo": "bar"
	}
	chosen := if absent != none { absent.clone() } else { fallback }
	assert absent == none
	assert chosen.len == 1
	println("ok")
}
')
	assert out == 'ok'
}

fn test_enum_alias_cast_contextualizes_short_enum_value() {
	out := checker_assignment_run_good('enum_alias_short_value_cast', 'enum Number {
	one = 1
	two
}

type NumberAlias = Number

fn main() {
	value := NumberAlias(.two)
	assert value == .two
	println(value)
}
')
	assert out == 'two'
}

fn test_container_stored_capturing_fn_literals() {
	alias_out := checker_assignment_run_good('append_alias_capturing_fn_literal', 'fn main() {
	x := 1
	f := fn [x] () int {
		return x
	}
	mut callbacks := []fn () int{}
	callbacks << f
	println(int_str(callbacks[0]()))
}
')
	assert alias_out == '1'
	direct_out := checker_assignment_run_good('append_direct_capturing_fn_literal', 'fn main() {
	x := 1
	mut callbacks := []fn () int{}
	callbacks << fn [x] () int {
		return x
	}
	println(int_str(callbacks[0]()))
}
')
	assert direct_out == '1'
}

fn test_nonlocal_assigned_capturing_fn_literals() {
	direct_out := checker_assignment_run_good('field_assign_direct_capturing_fn_literal', 'struct Holder {
mut:
	cb fn () int
}

fn plain() int {
	return 0
}

fn main() {
	x := 1
	mut holder := Holder{
		cb: plain
	}
	holder.cb = fn [x] () int {
		return x
	}
	println(int_str(holder.cb()))
}
')
	assert direct_out == '1'
	alias_out := checker_assignment_run_good('field_assign_alias_capturing_fn_literal', 'struct Holder {
mut:
	cb fn () int
}

fn plain() int {
	return 0
}

fn main() {
	x := 1
	f := fn [x] () int {
		return x
	}
	mut holder := Holder{
		cb: plain
	}
	holder.cb = f
	println(int_str(holder.cb()))
}
')
	assert alias_out == '1'
	index_out := checker_assignment_run_good('index_assign_capturing_fn_literal', 'fn plain() int {
	return 0
}

fn main() {
	x := 1
	mut callbacks := [plain]
	callbacks[0] = fn [x] () int {
		return x
	}
	println(int_str(callbacks[0]()))
}
')
	assert index_out == '1'
}

// A local cannot be redefined in a nested block, so the same name is bound in
// sibling blocks instead: each use must resolve to the binding of its own block.
fn test_capturing_fn_literal_marker_uses_nearest_binding() {
	out := checker_assignment_run_good('capturing_fn_literal_then_plain_fn', 'fn plain() int {
	return 2
}

fn main() {
	x := 1
	mut callbacks := []fn () int{}
	{
		f := fn [x] () int {
			return x
		}
		callbacks << f
	}
	{
		f := plain
		callbacks << f
	}
	assert callbacks[0]() == 1
	assert callbacks[1]() == 2
	println("ok")
}
')
	assert out == 'ok'
	plain_first_out := checker_assignment_run_good('plain_fn_then_capturing_fn_literal',
		'fn plain() int {
	return 2
}

fn main() {
	x := 1
	mut callbacks := []fn () int{}
	{
		f := plain
		callbacks << f
	}
	{
		f := fn [x] () int {
			return x
		}
		callbacks << f
	}
	assert callbacks[0]() == 2
	assert callbacks[1]() == 1
	println("ok")
}
')
	assert plain_first_out == 'ok'
}
