import os

const fn_index_vexe = @VEXE
const fn_index_tests_dir = os.dir(@FILE)
const fn_index_v3_dir = os.dir(fn_index_tests_dir)
const fn_index_vlib_dir = os.dir(fn_index_v3_dir)
const fn_index_v3_src = os.join_path(fn_index_v3_dir, 'v3.v')

fn fn_index_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_fn_index_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${fn_index_vexe} -gc none -path "${fn_index_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${fn_index_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn fn_index_write_source(name string, source string) string {
	path := os.join_path(os.temp_dir(), 'v3_fn_index_${name}_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	return path
}

// A `recv.field[i]()` call invokes the function value stored at index `i`. The
// brackets used to be read as explicit generic type arguments, so the checker
// rejected the index variable as an unknown type and the C backend emitted a call
// to a nonexistent `Recv__field` method.
fn test_fn_value_stored_in_container_field_is_called_through_the_index() {
	v3_bin := fn_index_build_v3()
	source := fn_index_write_source('container_field', 'module main

struct Registry {
mut:
	fns      []fn (int) int
	named    map[string]fn (int) int
	void_fns []fn ()
}

fn double(x int) int {
	return x * 2
}

fn triple(x int) int {
	return x * 3
}

fn hello() {
	println("hello")
}

fn main() {
	mut r := Registry{}
	r.fns << double
	r.fns << triple
	r.named["d"] = double
	r.void_fns << hello

	mut total := 0
	for i := 0; i < r.fns.len; i++ {
		total += r.fns[i](i + 1)
	}
	key := "d"
	total += r.named[key](10)
	idx := 0
	r.void_fns[idx]()
	println(total)
}
')
	out := os.join_path(os.temp_dir(), 'v3_fn_index_container_field_${os.getpid()}')
	compile := os.execute('${v3_bin} ${source} -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.split_into_lines() == ['hello', '28']
	generated := os.read_file(out + '.c') or { panic(err) }
	assert !generated.contains('Registry__fns'), generated
	assert !generated.contains('Registry__void_fns'), generated
	assert !generated.contains('Registry__named'), generated
	assert generated.contains('array_get(r.fns,'), generated
}

// `arr << fn_value` stores the function pointer by address into the array buffer.
// Collapsing `&tmp` to the bare function value made `array_push` copy the element
// bytes out of the function's own code, so the later call crashed.
fn test_fn_value_pushed_into_a_local_container_is_callable() {
	v3_bin := fn_index_build_v3()
	source := fn_index_write_source('local_container', 'module main

fn shout() {
	println("called")
}

fn main() {
	mut fns := []fn (){}
	mut m := map[string]fn (){}
	for _ in 0 .. 3 {
		fns << shout
	}
	m["k"] = shout
	for slot := 0; slot < fns.len; slot++ {
		fns[slot]()
	}
	key := "k"
	m[key]()
}
')
	out := os.join_path(os.temp_dir(), 'v3_fn_index_local_container_${os.getpid()}')
	compile := os.execute('${v3_bin} ${source} -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.split_into_lines() == ['called', 'called', 'called', 'called']
	generated := os.read_file(out + '.c') or { panic(err) }
	assert generated.contains('array_push(&fns, &__arr_val_'), generated
}

// The container guards above must not swallow a real `recv.method[T]()` call.
fn test_explicit_generic_method_call_still_resolves() {
	v3_bin := fn_index_build_v3()
	source := fn_index_write_source('explicit_generic', 'module main

struct Box[T] {
mut:
	items []T
}

fn (b Box[T]) pick[U](d U) U {
	return d
}

fn apply[T](f fn (T) T, v T) T {
	return f(v)
}

fn double(x int) int {
	return x * 2
}

fn main() {
	b := Box[int]{
		items: [1, 2, 3]
	}
	println(b.pick[string]("generic-ok"))
	println(apply[int](double, 21))
}
')
	out := os.join_path(os.temp_dir(), 'v3_fn_index_explicit_generic_${os.getpid()}')
	compile := os.execute('${v3_bin} ${source} -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.split_into_lines() == ['generic-ok', '42']
}
