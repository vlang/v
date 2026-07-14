import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_spawn_args_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
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

fn compact_c(c_code string) string {
	return c_code.replace('\t', '').replace('\n', '').replace('\r', '').replace(' ', '')
}

fn assert_spawn_pthread_decls(c_code string) {
	assert c_code.contains('int pthread_attr_init(pthread_attr_t* attr);'), c_code
	assert c_code.contains('int pthread_attr_destroy(pthread_attr_t* attr);'), c_code
	assert c_code.contains('int pthread_attr_setstacksize(void* attr, size_t stacksize);'), c_code
	assert c_code.contains('int pthread_create(void* thread, void* attr, void* start_routine, void* arg);'), c_code
	assert c_code.contains('int pthread_join(void* thread, void** retval);'), c_code
	assert !c_code.contains('i32 pthread_attr_init(void* attr);'), c_code
	assert !c_code.contains('i32 pthread_attr_destroy(void* attr);'), c_code
	assert c_code.contains('pthread_attr_setstacksize('), c_code
	assert c_code.contains(', 524288);'), c_code
	assert !c_code.contains(', 1048576);'), c_code
	assert !c_code.contains(', 2097152);'), c_code
	assert !c_code.contains(', 8388608);'), c_code
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
	c_compact := compact_c(c_code)
	assert c_code.contains('add_thread_args'), c_code
	assert c_code.contains('pthread_create'), c_code
	assert_spawn_pthread_decls(c_code)
	assert c_compact.contains('typedefstruct{Counter*a0;inta1;inta2;}add_thread_args;'), c_code
	assert c_compact.contains('->a0=&c;'), c_code
	assert c_compact.contains(',add_args_thread_wrapper,(void*)_sa'), c_code
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
	c_compact := compact_c(c_code)
	assert c_code.contains('pthread_create'), c_code
	assert c_compact.contains('typedefstruct{Counter*a0;inta1;}Counter__bump_thread_args;'), c_code
	assert c_compact.contains('->a0=&c;'), c_code
	assert c_compact.contains(',Counter__bump_args_thread_wrapper,(void*)_sa'), c_code
	assert c_code.contains('Counter__bump(p->a0, p->a1)'), c_code
}

// A no-arg `spawn` of a by-value receiver method must copy the receiver into the
// heap arg struct; casting the void* thread argument straight to the struct type
// (`(Greeter)arg`) is invalid C.
fn test_spawn_value_receiver_copies_receiver() {
	v3_bin := build_v3()
	c_code := gen_c(v3_bin, 'v3_spawn_value_receiver', '
struct Greeter {
	name string
}

fn (g Greeter) greet() {
	println(g.name)
}

fn main() {
	g := Greeter{name: "world"}
	_ := spawn g.greet()
	println("ok")
}
	')
	c_compact := compact_c(c_code)
	assert c_code.contains('pthread_create'), c_code
	assert c_compact.contains('typedefstruct{Greetera0;}Greeter__greet_thread_args;'), c_code
	assert c_compact.contains('->a0=g;'), c_code
	assert !c_compact.contains('->a0=&g;'), c_code
	assert c_compact.contains(',Greeter__greet_args_thread_wrapper,(void*)_sa'), c_code
	assert c_code.contains('Greeter__greet(p->a0)'), c_code
	assert !c_code.contains('(Greeter)arg'), c_code
}

// A pointer receiver/argument whose source is a rvalue must be stored by value in
// the heap argument packet, then passed to the spawned call as `&p->field`.
// Capturing the address of a stack temporary would race the caller's frame, and
// assigning the rvalue directly into a pointer field is invalid C.
fn test_spawn_pointer_rvalues_store_value_in_heap_packet() {
	v3_bin := build_v3()
	c_code := gen_c(v3_bin, 'v3_spawn_pointer_rvalue_receiver', '
struct Box {
	x int
}

fn make_box() Box {
	return Box{x: 7}
}

fn (b &Box) show() {
	println(b.x)
}

fn make_int() int {
	return 9
}

fn takes_ptr(p &int) {
	println(*p)
}

fn main() {
	_ := spawn make_box().show()
	_ := spawn takes_ptr(make_int())
	println("ok")
}
	')
	c_compact := compact_c(c_code)
	assert c_code.contains('pthread_create'), c_code
	assert c_compact.contains('typedefstruct{Boxa0;}Box__show_thread_args'), c_code
	assert c_compact.contains('Box__show(&p->a0)'), c_code
	assert !c_compact.contains('typedefstruct{Box*a0;}Box__show_thread_args'), c_code
	assert c_compact.contains('typedefstruct{inta0;}takes_ptr_thread_args'), c_code
	assert c_compact.contains('takes_ptr(&p->a0)'), c_code
	assert !c_compact.contains('typedefstruct{int*a0;}takes_ptr_thread_args'), c_code
}
