import os
import v.tempname

// A `recv.field[i]()` call invokes the function value stored at index `i`. The
// brackets used to be read as explicit generic type arguments, so the checker
// rejected the index variable as an unknown type and the C backend emitted a call
// to a nonexistent `Recv__field` method.
fn test_fn_value_index_calls_and_explicit_generic_calls() {
	root := os.join_path(os.vtmp_dir(), 'v3_fn_index_fixtures_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'v3_fn_index_container_calls.v')
	os.write_file(source, 'module main

struct Registry {
mut:
	fns      []fn (int) int
	named    map[string]fn (int) int
	void_fns []fn ()
}

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
	call_local_container_values()

	// Exercise explicit method type arguments alongside the indexed calls.
	b := Box[int]{
		items: [1, 2, 3]
	}
	println(b.pick[string]("generic-ok"))
	println(apply[int](double, 21))
}

fn shout() {
	println("called")
}

fn call_local_container_values() {
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
')!
	out := os.join_path(root, 'v3_fn_index_container_calls')
	compile := os.exec([@VEXE, source, '-b', 'c', '-o', '${out}'])
	assert compile.exit_code == 0, compile.output
	run := os.exec([out])
	assert run.exit_code == 0, run.output
	assert run.output.split_into_lines() == ['hello', '28', 'called', 'called', 'called', 'called',
		'generic-ok', '42'], run.output
	generated := os.read_file(out + '.c') or { panic(err) }
	assert !generated.contains('Registry__fns'), generated
	assert !generated.contains('Registry__void_fns'), generated
	assert !generated.contains('Registry__named'), generated
	assert generated.contains('array_get(r.fns,'), generated
	// array_push needs the address of the stored pointer so it can copy the pointer bytes.
	assert generated.contains('array_push(&fns, &__arr_val_'), generated
}
