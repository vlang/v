import os

const pointer_voidptr_vexe = @VEXE
const pointer_voidptr_tests_dir = os.dir(@FILE)
const pointer_voidptr_v3_dir = os.dir(pointer_voidptr_tests_dir)
const pointer_voidptr_vlib_dir = os.dir(pointer_voidptr_v3_dir)
const pointer_voidptr_v3_src = os.join_path(pointer_voidptr_v3_dir, 'v3.v')

fn pointer_voidptr_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_pointer_voidptr_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${pointer_voidptr_vexe} -gc none -path "${pointer_voidptr_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${pointer_voidptr_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn pointer_voidptr_run_good(v3_bin string, name string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn pointer_voidptr_run_bad(v3_bin string, name string, source string, expected string) {
	src := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains(expected), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn test_bare_value_pointer_returns_are_heap_lowered() {
	v3_bin := pointer_voidptr_build_v3()
	out := pointer_voidptr_run_good(v3_bin, 'bare_pointer_return', 'struct Item {
	value int
}

fn make() &Item {
	item := Item{
		value: 23
	}
	return item
}

fn main() {
	item := make()
	println(int_str(item.value))
}
')
	assert out == '23'
	optional_out := pointer_voidptr_run_good(v3_bin, 'bare_optional_pointer_return', 'struct Item {
	value int
}

fn make() ?&Item {
	item := Item{
		value: 29
	}
	defer {
		_ := 0
	}
	return item
}

fn main() {
	item := make() or { return }
	println(int_str(item.value))
}
')
	assert optional_out == '29'
	pointer_voidptr_run_bad(v3_bin, 'bare_interface_pointer_return', 'interface Reader {
	read() int
}

struct File {}

fn (File) read() int {
	return 1
}

fn make() &Reader {
	file := File{}
	return file
}

fn main() {}
',
		'cannot return `File` as `&Reader`')
	pointer_voidptr_run_bad(v3_bin, 'bare_sum_pointer_return', 'struct A {}

struct B {}

type Item = A | B

fn make() &Item {
	value := A{}
	return value
}

fn main() {}
',
		'cannot return `A` as `&Item`')
}

fn test_c_voidptr_struct_argument_passes_its_address() {
	v3_bin := pointer_voidptr_build_v3()
	header := os.join_path(os.temp_dir(), 'v3_c_voidptr_struct_${os.getpid()}.h')
	source := os.join_path(os.temp_dir(), 'v3_c_voidptr_struct_${os.getpid()}.v')
	bin := os.join_path(os.temp_dir(), 'v3_c_voidptr_struct_${os.getpid()}')
	os.write_file(header, 'static int v3_read_cfg(void *ptr) { return *((int *)ptr); }\n') or {
		panic(err)
	}
	os.write_file(source, '#insert "${header}"

struct Cfg {
	value int
}

fn C.v3_read_cfg(voidptr) int

fn main() {
	cfg := Cfg{
		value: 17
	}
	println(int_str(C.v3_read_cfg(cfg)))
}
') or {
		panic(err)
	}
	compile := os.execute('${v3_bin} ${source} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '17'
}
