import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v.v')

fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_spawn_args_test')
	build :=
		os.exec([vexe, '-gc', 'none', '-path', '${vlib_dir}' + '|@vlib|@vmodules', '-o', v3_bin,
			'${v3_src}'])
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn gen_c(v3_bin string, name string, src string) string {
	src_file := os.join_path(os.temp_dir(), '${name}.v')
	os.write_file(src_file, src) or { panic(err) }
	c_out := os.join_path(os.temp_dir(), '${name}.c')
	os.rm(c_out) or {}
	compile := os.exec([v3_bin, src_file, '-o', '${c_out}'])
	assert compile.exit_code == 0, compile.output
	return os.read_file(c_out) or { '' }
}

fn compact_c(c_code string) string {
	return c_code.replace('\t', '').replace('\n', '').replace('\r', '').replace(' ', '')
}

fn assert_spawn_pthread_decls(c_code string) {
	assert c_code.contains('int pthread_attr_init(pthread_attr_t* attr);'), c_code
	assert c_code.contains('int pthread_attr_destroy(pthread_attr_t* attr);'), c_code
	assert c_code.contains('int pthread_attr_setstacksize(pthread_attr_t* attr, size_t stacksize);'), c_code
	assert c_code.contains('int pthread_create(void* thread, void* attr, void* start_routine, void* arg);'), c_code
	assert c_code.contains('int pthread_join(void* thread, void** retval);'), c_code
	assert !c_code.contains('i32 pthread_attr_init(void* attr);'), c_code
	assert !c_code.contains('i32 pthread_attr_destroy(void* attr);'), c_code
	assert c_code.contains('typedef struct { pthread_t handle; } __v_thread;'), c_code
	assert c_code.contains('static __v_thread __v_thread_spawn('), c_code
	assert c_code.contains('#define V_THREAD_STACK_SIZE 8388608'), c_code
	assert c_code.contains('static const size_t __v_thread_stack_size = V_THREAD_STACK_SIZE;'), c_code
	assert c_code.contains('pthread_attr_setstacksize(&attr, __v_thread_stack_size);'), c_code
	assert c_code.contains('pthread_create(&result.handle, &attr, (void*)start, arg);'), c_code
	assert c_code.contains('int attr_rc = pthread_attr_destroy(&attr);'), c_code
	assert c_code.contains('if (cleanup) cleanup(arg);'), c_code
	assert c_code.contains('V thread attribute initialization failed: %d'), c_code
	assert c_code.contains('V thread stack size setup failed: %d'), c_code
	assert c_code.contains('V thread creation failed: %d'), c_code
	assert c_code.contains('static void* __v_thread_join(__v_thread thread)'), c_code
	assert !c_code.contains('pthread_attr_setstacksize(&_at'), c_code
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
	mut c := &Counter{}
	_ := spawn add(mut c, 3, 4)
	println("ok")
}
	')
	c_compact := compact_c(c_code)
	assert c_code.contains('add_thread_args'), c_code
	assert c_code.contains('pthread_create'), c_code
	assert_spawn_pthread_decls(c_code)
	assert c_compact.contains('typedefstruct{main__Counter*a0;i64a1;i64a2;}add_thread_args;'), c_code
	assert c_compact.contains('->a0=c;'), c_code
	assert c_compact.contains('__v_thread_spawn(add_args_thread_wrapper,(void*)_sa'), c_code
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
	mut c := &Counter{}
	_ := spawn c.bump(10)
	println("ok")
}
	')
	c_compact := compact_c(c_code)
	assert c_code.contains('pthread_create'), c_code
	assert c_compact.contains('typedefstruct{main__Counter*a0;i64a1;}Counter__bump_thread_args;'), c_code
	assert c_compact.contains('->a0=c;'), c_code
	assert c_compact.contains('__v_thread_spawn_detached(Counter__bump_args_thread_wrapper_detached,(void*)_sa'), c_code

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
	assert c_compact.contains('typedefstruct{main__Greetera0;}Greeter__greet_thread_args;'), c_code
	assert c_compact.contains('->a0=g;'), c_code
	assert !c_compact.contains('->a0=&g;'), c_code
	assert c_compact.contains('__v_thread_spawn_detached(Greeter__greet_args_thread_wrapper_detached,(void*)_sa'), c_code

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
	assert c_compact.contains('typedefstruct{main__Boxa0;}Box__show_thread_args'), c_code
	assert c_compact.contains('Box__show(&p->a0)'), c_code
	assert !c_compact.contains('typedefstruct{main__Box*a0;}Box__show_thread_args'), c_code
	assert c_compact.contains('typedefstruct{i64a0;}takes_ptr_thread_args'), c_code
	assert c_compact.contains('takes_ptr(&p->a0)'), c_code
	assert !c_compact.contains('typedefstruct{i64*a0;}takes_ptr_thread_args'), c_code
}

fn test_spawn_mutable_local_address_preserves_escaping_pointer() {
	v3_bin := build_v3()
	c_code := gen_c(v3_bin, 'v3_spawn_mutable_local_address', '
fn takes_ptr(value &int) {
	println(*value)
}

fn main() {
	for i in 0 .. 1 {
		mut value := i + 9
		_ := spawn takes_ptr(&value)
		value = 10
	}
	println("ok")
}
	')
	c_compact := compact_c(c_code)
	assert c_compact.contains('typedefstruct{i64*a0;}takes_ptr_thread_args'), c_code
	assert c_compact.contains('->a0=value;'), c_code
	assert c_compact.contains('takes_ptr(p->a0)'), c_code
	assert !c_compact.contains('typedefstruct{i64a0;}takes_ptr_thread_args'), c_code
}

fn test_spawn_result_uses_checked_allocation_and_typed_join() {
	v3_bin := build_v3()
	c_code := gen_c(v3_bin, 'v3_spawn_checked_result', '
fn answer() int {
	return 42
}

fn main() {
	t := spawn answer()
	println(t.wait())
}
	')
	assert c_code.contains('(i64*)__v_thread_alloc(sizeof(i64))'), c_code
	assert c_code.contains('__v_thread_join(t)'), c_code
	assert !c_code.contains('pthread_join((pthread_t)'), c_code
}

// A spawned named closure remains caller-owned because it can be reused after join.
fn test_spawn_fn_value_closure_keeps_caller_ownership() {
	v3_bin := build_v3()
	c_code := gen_c(v3_bin, 'v3_spawn_fn_value_capture', '
fn main() {
	x := 11
	cb := fn [x] () int {
		return x
	}
	t := spawn (cb)()
	println(t.wait())
	println(cb())
}
	')
	c_compact := compact_c(c_code)
	assert c_code.contains('fn_value_args_thread_wrapper'), c_code
	assert c_compact.contains('f;}fn_value_thread_args_'), c_code
	assert c_compact.contains('p->f()'), c_code
	assert !c_compact.contains('closure__closure_try_destroy((void*)p->f);'), c_code
	assert !c_compact.contains('closure__closure_try_destroy((void*)cb);'), c_code
}

// A `spawn` whose handle is discarded can never be joined, so its thread must be
// detached. Left joinable, every such thread keeps its OS resources until exit
// (under Boehm GC on macOS, one mach port each, until the kernel kills the
// process). A handle that is kept must stay joinable for `.wait()`.
fn test_discarded_spawn_detaches_thread() {
	v3_bin := build_v3()
	c_code := gen_c(v3_bin, 'v3_spawn_discarded_detach', '
fn work() {}

fn answer() int {
	return 42
}

fn make_array() []int {
	return [1, 2, 3]
}

struct Owned {
	values []int
}

fn make_owned() Owned {
	return Owned{values: [4, 5]}
}

fn make_thread() thread int {
	return spawn answer()
}

fn make_array_thread() thread []int {
	return spawn make_array()
}

fn make_closure() fn () int {
	value := 42
	return fn [value] () int {
		return value
	}
}

fn add(a int, b int) int {
	return a + b
}

fn wait_for(t thread int) {
	println(t.wait())
}

fn main() {
	spawn work()
	_ := spawn work()
	_ = spawn work()
	mut b := 0
	b, _ = 2, spawn work()
	println(b)
	spawn answer()
	spawn make_array()
	spawn make_owned()
	spawn make_thread()
	spawn make_array_thread()
	spawn make_closure()
	spawn add(1, 2)
	spawn wait_for(spawn answer())
	t := spawn answer()
	println(t.wait())
	owned := spawn make_array()
	println(owned.wait())
}
	')
	c_compact := compact_c(c_code)
	assert c_code.contains('static __v_thread __v_thread_spawn_detached(__v_thread_start_fn start, void* arg, void (*cleanup)(void*))'), c_code
	assert c_compact.count('__v_thread_spawn_detached(work_thread_wrapper_detached,') == 4, c_code
	assert c_compact.count('__v_thread_spawn_detached(answer_thread_wrapper_detached,') == 1, c_code
	assert c_compact.contains('__v_thread_spawn_detached(add_args_thread_wrapper_detached,(void*)_sa'), c_code
	// The spawn nested in the arguments is joined by `wait_for`, so it stays joinable.
	assert c_compact.contains('__v_thread_spawn_detached(wait_for_args_thread_wrapper_detached,(void*)_sa'), c_code
	assert c_compact.count('__v_thread_spawn(answer_thread_wrapper,') == 3, c_code
	assert c_compact.contains('__v_threadt=__v_thread_spawn(answer_thread_wrapper,'), c_code
	assert c_code.contains('static void* make_array_thread_wrapper_detached(void* arg) { (void)arg; void* __v3_signal_stack = __v_thread_signal_stack_enter(&arg); Array __tr = make_array();'), c_code
	assert c_code.contains('array__free(&(__tr));'), c_code
	assert c_code.contains('static void* make_array_thread_wrapper(void* arg) { (void)arg; void* __v3_signal_stack = __v_thread_signal_stack_enter(&arg); Array* __tr = (Array*)__v_thread_alloc(sizeof(Array));'), c_code
	assert c_code.contains('static void* make_thread_thread_wrapper_detached(void* arg) { (void)arg; void* __v3_signal_stack = __v_thread_signal_stack_enter(&arg); __v_thread __tr = make_thread();'), c_code
	assert c_code.contains('__v_thread_join(__tr);'), c_code
	assert c_code.contains('static void* make_array_thread_thread_wrapper_detached(void* arg) { (void)arg; void* __v3_signal_stack = __v_thread_signal_stack_enter(&arg); __v_thread __tr = make_array_thread();'), c_code
	assert c_code.contains('array__free(&(__tr_inner'), c_code
	assert c_code.contains('static void* make_closure_thread_wrapper_detached(void* arg) { (void)arg;'), c_code
	assert c_code.contains('closure__closure_try_destroy((void*)(__tr));'), c_code
	assert c_compact.contains('__v_thread__twthread0=t;'), c_code
}

fn test_discarded_spawn_cleans_nested_thread_and_closure_results() {
	v3_bin := build_v3()
	c_code := gen_c(v3_bin, 'v3_spawn_nested_result_cleanup', '
struct Worker {
	child thread int
}

struct ClosureHolder {
	callback fn () int
}

fn answer() int {
	return 42
}

fn make_array() []thread int {
	return [spawn answer()]
}

fn make_optional() ?thread int {
	return spawn answer()
}

fn make_struct() Worker {
	return Worker{child: spawn answer()}
}

fn make_closure() ClosureHolder {
	value := 42
	return ClosureHolder{
		callback: fn [value] () int {
			return value
		}
	}
}

fn main() {
	spawn make_array()
	spawn make_optional()
	spawn make_struct()
	spawn make_closure()
}
	')
	for name in ['make_array', 'make_optional', 'make_struct'] {
		wrapper := c_code.all_after('static void* ${name}_thread_wrapper_detached').all_before('return NULL;')
		assert wrapper.contains('__v_thread_join('), wrapper
	}
	closure_wrapper := c_code.all_after('static void* make_closure_thread_wrapper_detached').all_before('return NULL;')
	assert closure_wrapper.contains('closure__closure_try_destroy('), closure_wrapper
}

fn test_discarded_aggregate_spawns_detach_threads() {
	v3_bin := build_v3()
	c_code := gen_c(v3_bin, 'v3_spawn_discarded_aggregate_detach', '
struct Holder {
	worker thread int
}

fn answer() int {
	return 42
}

fn wait_for(t thread int) int {
	return t.wait()
}

fn main() {
	_ := [spawn answer()]
	_ := [spawn answer()][0]
	_ := Holder{worker: spawn answer()}
	_ := Holder{worker: spawn answer()}.worker
	_ := (spawn answer()) == (spawn answer())
	_ := [[spawn answer()]]
	_ := {"worker": spawn answer()}
	_ := dump(spawn answer())
	dump(spawn answer())
	_ := [wait_for(spawn answer())]
	t := spawn answer()
	println(t.wait())
}
	')
	c_compact := compact_c(c_code)
	assert c_compact.count('__v_thread_spawn_detached(answer_thread_wrapper_detached,') == 8, c_code
	// Compared spawns keep their handles until the comparison finishes.
	assert c_compact.count('__v_thread_spawn_comparable(answer_thread_wrapper_detached,') == 2, c_code
	assert c_compact.count('__v_thread_spawn(answer_thread_wrapper,') == 2, c_code
}

// A discarded `if`/`match`, including one in a `_` slot of a multi-assignment, must
// detach the spawn in each branch that ends in one.
// The transformer would otherwise lower the value into a temporary that hides the
// spawns from cgen. A branch that yields an existing handle only evaluates it, so
// the handle stays joinable for its later `.wait()`.
fn test_discarded_conditional_spawns_detach_threads() {
	v3_bin := build_v3()
	c_code := gen_c(v3_bin, 'v3_spawn_discarded_conditional_detach', "
fn work() {}

fn other() {}

fn main() {
	flag := true
	n := 2
	_ := if flag { spawn work() } else { spawn other() }
	_ = if flag {
		println('a')
		spawn work()
	} else if n > 1 {
		spawn other()
	} else {
		spawn work()
	}
	_ := match n {
		1 { spawn work() }
		else { spawn other() }
	}
	_ := (spawn work())
	mut a := 0
	a, _ = 3, if flag { spawn work() } else { spawn other() }
	println(a)
	t := spawn work()
	_ := if flag { t } else { spawn other() }
	_ := match n {
		1 { t }
		else { spawn other() }
	}
	t.wait()
}
	")
	c_compact := compact_c(c_code)
	assert c_compact.count('__v_thread_spawn_detached(work_thread_wrapper_detached,') == 6, c_code
	assert c_compact.count('__v_thread_spawn_detached(other_thread_wrapper_detached,') == 6, c_code
	assert !c_compact.contains('__v_thread_spawn(other_thread_wrapper,'), c_code
	assert c_compact.contains('__v_threadt=__v_thread_spawn(work_thread_wrapper,'), c_code
	assert c_compact.count('(void)(t);') == 2, c_code
}
