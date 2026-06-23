import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_spawn_args_test')
	build := os.execute('${vexe} -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn gen_c(v3_bin string, name string, src string) string {
	src_file := os.join_path(os.temp_dir(), '${name}.v')
	os.write_file(src_file, src) or { panic(err) }
	c_out := os.join_path(os.temp_dir(), '${name}.c')
	os.rm(c_out) or {}
	compile := os.execute('${v3_bin} ${src_file} -o ${c_out}')
	assert compile.exit_code == 0, compile.output
	return os.read_file(c_out) or { '' }
}

// A `spawn` of a free function with arguments must pack the arguments into a heap
// struct and run the real function through a wrapper, instead of emitting a
// `(void*)0` no-op that silently drops the call and its arguments.
fn test_spawn_free_function_with_arguments_packs_args() {
	v3_bin := build_v3()
	c_code := gen_c(v3_bin, 'v3_spawn_args_free', '
struct Counter {
mut:
	total int
}

fn add(mut c Counter, a int, b int) {
	c.total = a + b
}

fn main() {
	mut c := Counter{}
	_ := spawn add(mut c, 3, 4)
	println("ok")
}
')
	assert c_code.contains('add_thread_args'), c_code
	assert c_code.contains('pthread_create'), c_code
	assert c_code.contains('add(p->a0, p->a1, p->a2)'), c_code
}

// A `spawn` of a method with arguments must pack the receiver and arguments and
// dispatch the real method instead of dropping the call.
fn test_spawn_method_with_arguments_packs_receiver_and_args() {
	v3_bin := build_v3()
	c_code := gen_c(v3_bin, 'v3_spawn_args_method', '
struct Counter {
mut:
	total int
}

fn (mut c Counter) bump(x int) {
	c.total += x
}

fn main() {
	mut c := Counter{}
	_ := spawn c.bump(10)
	println("ok")
}
')
	assert c_code.contains('pthread_create'), c_code
	assert c_code.contains('Counter__bump(p->a0, p->a1)'), c_code
}
