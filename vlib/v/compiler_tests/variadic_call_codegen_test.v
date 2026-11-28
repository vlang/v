import os
import v.tempname

fn test_fixed_slot_spread_variadic_call_rejects_trailing_args() {
	dir := os.join_path(os.vtmp_dir(), 'v3_variadic_call_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	bin := os.join_path(dir, 'v3_fixed_slot_spread_variadic_trailing')
	src := bin + '.v'
	os.write_file(src, 'fn take(first int, rest ...int) int {
	return first * 1000 + rest[0] * 100 + rest[1] * 10 + rest[2]
}

fn main() {
	xs := [2, 7, 8]
	println(int_str(take(...xs, 9)))
}
') or {
		panic(err)
	}
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('when forwarding a variadic variable, it must be the final argument'), compile.output
}

fn test_variadic_calls_expand_fixed_slots_and_forward_arrays() {
	dir := os.join_path(os.vtmp_dir(), 'v3_variadic_call_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	bin := os.join_path(dir, 'v3_variadic_arrays')
	src := bin + '.v'
	os.write_file(src, 'interface Displayable {
	display() string
}

interface Any {}

struct Item {
	text string
}

fn (item Item) display() string {
	return item.text
}

fn sink(items ...Displayable) string {
	mut out := ""
	for item in items {
		out += item.display()
	}
	return out
}

fn forward(items ...Displayable) string {
	return sink(items)
}

fn count(values ...Any) int {
	return values.len
}

fn take(first int, rest ...int) int {
	return first * 100 + rest[0] * 10 + rest[1]
}

fn main() {
	ints := [2, 7, 8]
	println(int_str(take(...ints)))
	println(forward(Item{
		text: "a"
	}, Item{
		text: "b"
	}))
	xs := []Any{}
	println(count(...xs))
	println(count(xs))
}
') or {
		panic(err)
	}
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	// A spread array is forwarded as is, while a bare array is packed as one variadic
	// element (d2e6876e38).
	assert run.output.trim_space() == '278\nab\n0\n1', run.output
}

fn test_source_var_with_generated_variadic_prefix_is_not_forwarded() {
	dir := os.join_path(os.vtmp_dir(), 'v3_variadic_call_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	bin := os.join_path(dir, 'v3_source_varargs_prefix')
	src := bin + '.v'
	os.write_file(src, 'interface Any {}

fn count(values ...Any) int {
	return values.len
}

fn main() {
	__varargs_items := []Any{}
	println(count(__varargs_items))
}
') or {
		panic(err)
	}
	compile := os.exec([@VEXE, '-translated', src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '1', run.output
}
