import os

const variadic_call_vexe = @VEXE
const variadic_call_tests_dir = os.dir(@FILE)
const variadic_call_v3_dir = os.dir(variadic_call_tests_dir)
const variadic_call_vlib_dir = os.dir(variadic_call_v3_dir)
const variadic_call_v3_src = os.join_path(variadic_call_v3_dir, 'v3.v')

fn variadic_call_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_variadic_call_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${variadic_call_vexe} -gc none -path "${variadic_call_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${variadic_call_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_fixed_slot_spread_variadic_call_expands_before_codegen() {
	v3_bin := variadic_call_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_fixed_slot_spread_variadic_${os.getpid()}.v')
	os.write_file(src, 'fn take(first int, rest ...int) int {
	return first * 100 + rest[0] * 10 + rest[1]
}

fn main() {
	xs := [2, 7, 8]
	println(int_str(take(...xs)))
}
') or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_fixed_slot_spread_variadic_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '278', run.output
}

fn test_fixed_slot_spread_variadic_call_keeps_trailing_args() {
	v3_bin := variadic_call_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_fixed_slot_spread_variadic_trailing_${os.getpid()}.v')
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
	bin := os.join_path(os.temp_dir(), 'v3_fixed_slot_spread_variadic_trailing_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '2789', run.output
}

fn test_interface_variadic_forwarded_array_is_not_repacked() {
	v3_bin := variadic_call_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_interface_variadic_forward_${os.getpid()}.v')
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

fn main() {
	println(forward(Item{
		text: "a"
	}, Item{
		text: "b"
	}))
	xs := []Any{}
	println(count(xs))
}
') or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_interface_variadic_forward_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ab\n0', run.output
}
