import os
import v.tempname

struct SpawnArgsFixture {
	name   string
	source string
}

fn spawn_args_gen_c(cases []SpawnArgsFixture) string {
	dir := os.join_path(os.vtmp_dir(), 'v3_spawn_args_${tempname.unique_token()}')
	os.mkdir(dir) or { panic(err) }
	defer { os.rmdir_all(dir) or {} }
	mut imports := []string{cap: cases.len}
	mut calls := []string{cap: cases.len}
	for case in cases {
		module_dir := os.join_path(dir, case.name)
		os.mkdir(module_dir) or { panic(err) }
		imports << 'import ${case.name}'
		calls << '\t${case.name}.exercise()'
		source := 'module ${case.name}\n\n' + case.source
		os.write_file(os.join_path(module_dir, 'case.v'), source.replace_once('\nfn main()',
			'\npub fn exercise()')) or { panic(err) }
	}
	os.write_file(os.join_path(dir, 'main.v'), 'module main\n\n${imports.join('\n')}\n\nfn main() {\n${calls.join('\n')}\n}\n') or {
		panic(err)
	}
	c_out := os.join_path(dir, 'spawn.c')
	compile := os.exec([@VEXE, dir, '-o', c_out])
	assert compile.exit_code == 0, compile.output
	return os.read_file(c_out) or { panic(err) }
}

fn spawn_args_entry(c_code string, name string) string {
	header := '\nvoid ${name}__exercise(void) {\n'
	start := c_code.index(header) or { panic('missing C entry for ${name}') }
	body := c_code[start + header.len..]
	end := body.index('\n}\n') or { panic('missing end of C entry for ${name}') }
	return body[..end]
}

fn spawn_args_wrapper(c_code string, name string) string {
	header := '\nstatic void* ${name}(void* arg) {'
	start := c_code.index(header) or { panic('missing C wrapper ${name}') }
	body := c_code[start + header.len..]
	mut depth := 1
	for i, ch in body {
		match ch {
			`{` {
				depth++
			}
			`}` {
				depth--
				if depth == 0 {
					return body[..i]
				}
			}
			else {}
		}
	}
	panic('missing end of C wrapper ${name}')
}

fn spawn_args_compact_c(c_code string) string {
	return c_code.replace('\t', '').replace('\n', '').replace('\r', '').replace(' ', '')
}

fn assert_spawn_pthread_decls(c_code string) {
	// Headerless C declares the thread runtime's pthread functions once, from their V
	// bindings in builtin/cfns.c.v; the hand-written `pthread_attr_t *` prototypes of the
	// headerless preamble were removed (aae3f51f7d) and must not come back beside them.
	assert c_code.count('i32 pthread_attr_init(void* attr);') == 1, c_code
	assert c_code.count('i32 pthread_attr_destroy(void* attr);') == 1, c_code
	assert c_code.count('i32 pthread_attr_setstacksize(void* attr, size_t stacksize);') == 1, c_code
	assert c_code.count('i32 pthread_create(void* thread, void* attr, void* start_routine, void* arg);') == 1, c_code
	assert c_code.count('i32 pthread_join(void* thread, void* retval);') == 1, c_code
	assert !c_code.contains('pthread_attr_init(pthread_attr_t* attr);'), c_code
	assert !c_code.contains('pthread_attr_destroy(pthread_attr_t* attr);'), c_code
	assert !c_code.contains('pthread_attr_setstacksize(pthread_attr_t* attr,'), c_code
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

// Spawn packs the free function's arguments into a heap packet and dispatches
// the real function through a wrapper.
fn check_spawn_free_arguments(c_code string, compact string) {
	entry := spawn_args_compact_c(spawn_args_entry(c_code, 'free_args'))
	assert compact.contains('typedefstruct{free_args__Counter*a0;i64a1;i64a2;}free_args__add_thread_args;'), c_code
	assert entry.contains('->a0=c;'), entry
	// A discarded handle starts detached.
	assert entry.contains('__v_thread_spawn_detached(free_args__add_args_thread_wrapper_detached,(void*)_sa'), entry
	assert c_code.contains('free_args__add(__v3_spawn_args->a0, __v3_spawn_args->a1, __v3_spawn_args->a2)'), c_code
}

// Spawn packs the receiver along with the explicit method arguments.
fn check_spawn_method_arguments(c_code string, compact string) {
	entry := spawn_args_compact_c(spawn_args_entry(c_code, 'method_args'))
	assert compact.contains('typedefstruct{method_args__Counter*a0;i64a1;}method_args__Counter__bump_thread_args;'), c_code
	assert entry.contains('->a0=c;'), entry
	assert entry.contains('__v_thread_spawn_detached(method_args__Counter__bump_args_thread_wrapper_detached,(void*)_sa'), entry
	assert c_code.contains('method_args__Counter__bump(__v3_spawn_args->a0, __v3_spawn_args->a1)'), c_code
}

// A by-value receiver is copied into the heap packet.
fn check_spawn_value_receiver(c_code string, compact string) {
	entry := spawn_args_compact_c(spawn_args_entry(c_code, 'value_receiver'))
	assert compact.contains('typedefstruct{value_receiver__Greetera0;}value_receiver__Greeter__greet_thread_args;'), c_code
	assert entry.contains('->a0=g;'), entry
	assert !entry.contains('->a0=&g;'), entry
	assert entry.contains('__v_thread_spawn_detached(value_receiver__Greeter__greet_args_thread_wrapper_detached,(void*)_sa'), entry
	assert c_code.contains('value_receiver__Greeter__greet(__v3_spawn_args->a0)'), c_code
	assert !c_code.contains('(value_receiver__Greeter)arg'), c_code
}

// Pointer rvalues are stored by value and addressed from the heap packet.
fn check_spawn_pointer_rvalues(c_code string, compact string) {
	assert compact.contains('typedefstruct{pointer_rvalues__Boxa0;}pointer_rvalues__Box__show_thread_args'), c_code
	assert compact.contains('pointer_rvalues__Box__show(&__v3_spawn_args->a0)'), c_code
	assert !compact.contains('typedefstruct{pointer_rvalues__Box*a0;}pointer_rvalues__Box__show_thread_args'), c_code
	assert compact.contains('typedefstruct{i64a0;}pointer_rvalues__takes_ptr_thread_args'), c_code
	assert compact.contains('pointer_rvalues__takes_ptr(&__v3_spawn_args->a0)'), c_code
	assert !compact.contains('typedefstruct{i64*a0;}pointer_rvalues__takes_ptr_thread_args'), c_code
}

fn check_spawn_mutable_local_address(c_code string, compact string) {
	entry := spawn_args_compact_c(spawn_args_entry(c_code, 'local_address'))
	assert compact.contains('typedefstruct{i64*a0;}local_address__takes_ptr_thread_args'), c_code
	assert entry.contains('->a0=value;'), entry
	assert compact.contains('local_address__takes_ptr(__v3_spawn_args->a0)'), c_code
	assert !compact.contains('typedefstruct{i64a0;}local_address__takes_ptr_thread_args'), c_code
}

fn check_spawn_checked_result(c_code string) {
	wrapper := spawn_args_wrapper(c_code, 'checked_result__answer_thread_wrapper')
	assert wrapper.contains('(i64*)__v_thread_alloc(sizeof(i64))'), wrapper
	entry := spawn_args_entry(c_code, 'checked_result')
	// wait() copies the handle and panics on an uninitialized thread.
	index := entry.all_after('__v_thread __twthread').all_before(' = t;')
	assert entry.contains('__v_thread __twthread${index} = t; if (!__twthread${index}.handle) v_panic(_S("unable to join thread")); void* __twres${index} = __v_thread_join(__twthread${index});'), entry
	assert !c_code.contains('pthread_join((pthread_t)'), c_code
}

// The named closure remains caller-owned and can be reused after join.
fn check_spawn_captured_fn_value(c_code string, compact string) {
	entry := spawn_args_entry(c_code, 'captured_fn_value')
	wrapper_name := entry.all_after('__v_thread_spawn(').all_before(',')
	assert wrapper_name.starts_with('fn_value_args_thread_wrapper'), entry
	assert entry.contains('fn_value_thread_args_'), entry
	assert compact.contains('f;}fn_value_thread_args_'), c_code
	wrapper := spawn_args_wrapper(c_code, wrapper_name)
	assert wrapper.contains('__v3_spawn_args->f()'), wrapper
	assert !spawn_args_compact_c(wrapper).contains('closure__closure_try_destroy((void*)__v3_spawn_args->f);'), wrapper
	assert !spawn_args_compact_c(entry).contains('closure__closure_try_destroy((void*)cb);'), entry
}

// Discarded handles detach. Handles passed to a consumer or retained for wait()
// stay joinable, and detached wrappers release unused return values.
fn check_discarded_spawn(c_code string, compact string) {
	entry := spawn_args_entry(c_code, 'discarded_spawn')
	assert c_code.contains('static __v_thread __v_thread_spawn_detached(__v_thread_start_fn start, void* arg, void (*cleanup)(void*))'), c_code
	assert compact.count('__v_thread_spawn_detached(discarded_spawn__work_thread_wrapper_detached,') == 4, c_code
	assert compact.count('__v_thread_spawn_detached(discarded_spawn__answer_thread_wrapper_detached,') == 1, c_code
	assert compact.contains('__v_thread_spawn_detached(discarded_spawn__add_args_thread_wrapper_detached,(void*)_sa'), c_code
	// The spawn nested in the arguments is joined by wait_for.
	assert compact.contains('__v_thread_spawn_detached(discarded_spawn__wait_for_args_thread_wrapper_detached,(void*)_sa'), c_code
	assert compact.count('__v_thread_spawn(discarded_spawn__answer_thread_wrapper,') == 3, c_code
	assert compact.contains('__v_threadt=__v_thread_spawn(discarded_spawn__answer_thread_wrapper,'), c_code
	array_wrapper := spawn_args_wrapper(c_code, 'discarded_spawn__make_array_thread_wrapper_detached')
	assert array_wrapper.contains('(void)arg; void* __v3_signal_stack = __v_thread_signal_stack_enter(&arg); Array __tr = discarded_spawn__make_array();'), array_wrapper
	assert array_wrapper.contains('array__free(&(__tr));'), array_wrapper
	assert c_code.contains('static void* discarded_spawn__make_array_thread_wrapper(void* arg) { (void)arg; void* __v3_signal_stack = __v_thread_signal_stack_enter(&arg); Array* __tr = (Array*)__v_thread_alloc(sizeof(Array));'), c_code
	thread_wrapper := spawn_args_wrapper(c_code, 'discarded_spawn__make_thread_thread_wrapper_detached')
	assert thread_wrapper.contains('(void)arg; void* __v3_signal_stack = __v_thread_signal_stack_enter(&arg); __v_thread __tr = discarded_spawn__make_thread();'), thread_wrapper
	assert thread_wrapper.contains('__v_thread_join(__tr);'), thread_wrapper
	array_thread_wrapper := spawn_args_wrapper(c_code, 'discarded_spawn__make_array_thread_thread_wrapper_detached')
	assert array_thread_wrapper.contains('(void)arg; void* __v3_signal_stack = __v_thread_signal_stack_enter(&arg); __v_thread __tr = discarded_spawn__make_array_thread();'), array_thread_wrapper
	assert array_thread_wrapper.contains('array__free(&(__tr_inner'), array_thread_wrapper
	closure_wrapper := spawn_args_wrapper(c_code, 'discarded_spawn__make_closure_thread_wrapper_detached')
	assert closure_wrapper.starts_with(' (void)arg;'), closure_wrapper
	assert closure_wrapper.contains('closure__closure_try_destroy((void*)(__tr));'), closure_wrapper
	assert entry.all_after('__v_thread __twthread').all_before(';').ends_with(' = t'), entry
}

fn check_discarded_nested_results(c_code string) {
	for name in ['make_array', 'make_optional', 'make_struct'] {
		wrapper := spawn_args_wrapper(c_code, 'nested_result_cleanup__${name}_thread_wrapper_detached')
		assert wrapper.contains('__v_thread_join('), '${name}: ${wrapper}'
	}
	closure_wrapper := spawn_args_wrapper(c_code, 'nested_result_cleanup__make_closure_thread_wrapper_detached')
	assert closure_wrapper.contains('closure__closure_try_destroy('), closure_wrapper
}

fn check_discarded_aggregate_spawns(c_code string) {
	entry := spawn_args_compact_c(spawn_args_entry(c_code, 'discarded_aggregate'))
	assert entry.count('__v_thread_spawn_detached(discarded_aggregate__answer_thread_wrapper_detached,') == 8, entry
	// Compared spawns keep their handles until the comparison finishes.
	assert entry.count('__v_thread_spawn_comparable(discarded_aggregate__answer_thread_wrapper_detached,') == 2, entry
	assert entry.count('__v_thread_spawn(discarded_aggregate__answer_thread_wrapper,') == 2, entry
}

// A discarded conditional detaches spawns in its value branches. A branch
// yielding an existing handle leaves it joinable for a later wait().
fn check_discarded_conditional_spawns(c_code string) {
	entry := spawn_args_compact_c(spawn_args_entry(c_code, 'discarded_conditional'))
	assert entry.count('__v_thread_spawn_detached(discarded_conditional__work_thread_wrapper_detached,') == 6, entry
	assert entry.count('__v_thread_spawn_detached(discarded_conditional__other_thread_wrapper_detached,') == 6, entry
	assert !entry.contains('__v_thread_spawn(discarded_conditional__other_thread_wrapper,'), entry
	assert entry.contains('__v_threadt=__v_thread_spawn(discarded_conditional__work_thread_wrapper,'), entry
	assert entry.count('(void)(t);') == 2, entry
}

fn test_spawn_arguments_and_cleanup_codegen() {
	c_code := spawn_args_gen_c([
		SpawnArgsFixture{'free_args', '
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
	'},
		SpawnArgsFixture{'method_args', '
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
	'},
		SpawnArgsFixture{'value_receiver', '
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
	'},
		SpawnArgsFixture{'pointer_rvalues', '
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
	'},
		SpawnArgsFixture{'local_address', '
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
	'},
		SpawnArgsFixture{'checked_result', '
fn answer() int {
	return 42
}

fn main() {
	t := spawn answer()
	println(t.wait())
}
	'},
		SpawnArgsFixture{'captured_fn_value', '
fn main() {
	x := 11
	cb := fn [x] () int {
		return x
	}
	t := spawn (cb)()
	println(t.wait())
	println(cb())
}
	'},
		SpawnArgsFixture{'discarded_spawn', '
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
	'},
		SpawnArgsFixture{'nested_result_cleanup', '
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
	'},
		SpawnArgsFixture{'discarded_aggregate', '
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
	'},
		SpawnArgsFixture{'discarded_conditional', "
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
	"},
	])
	compact := spawn_args_compact_c(c_code)
	assert_spawn_pthread_decls(c_code)
	check_spawn_free_arguments(c_code, compact)
	check_spawn_method_arguments(c_code, compact)
	check_spawn_value_receiver(c_code, compact)
	check_spawn_pointer_rvalues(c_code, compact)
	check_spawn_mutable_local_address(c_code, compact)
	check_spawn_checked_result(c_code)
	check_spawn_captured_fn_value(c_code, compact)
	check_discarded_spawn(c_code, compact)
	check_discarded_nested_results(c_code)
	check_discarded_aggregate_spawns(c_code)
	check_discarded_conditional_spawns(c_code)
}
