import os

const shared_element_lock_decls = 'struct Foo {
mut:
	x int
}

fn (f Foo) get() int {
	return f.x
}

fn (mut f Foo) bump() int {
	f.x++
	return f.x
}
'

fn check_shared_element_lock(name string, source string) os.Result {
	file := os.join_path(os.vtmp_dir(), 'shared_element_lock_${name}_${os.getpid()}.v')
	os.write_file(file, shared_element_lock_decls + source) or { panic(err) }
	defer {
		os.rm(file) or {}
	}
	return os.exec([@VEXE, '-new-compiler', '-check', file])
}

// An element of a `[]shared T` local or parameter is a `shared T` value. Its binding is
// tracked per scope, because the ident's canonical type has no `shared` element marker.
fn test_shared_array_elements_of_locals_and_parameters_can_be_locked() {
	result := check_shared_element_lock('accepted', '
fn query(items []shared Foo) int {
	rlock items[0] {
		return items[0].get()
	}
	return 0
}

fn main() {
	mut items := []shared Foo{}
	items << Foo{}
	rlock items[0] {
		println(items[0].get())
	}
	lock items[0] {
		println(items[0].bump())
	}
	println(query(items))
}
')
	assert result.exit_code == 0, result.output
}

fn test_elements_of_ordinary_arrays_cannot_be_locked() {
	result := check_shared_element_lock('rejected', '
fn main() {
	mut values := [Foo{}]
	lock values[0] {
		println(values[0].get())
	}
}
')
	assert result.exit_code != 0, result.output
	assert result.output.contains('`values[0]` must be declared as `shared` struct element to be locked'), result.output
}

// As in V1, a method call on an element of a `[]shared T` array needs that element's lock:
// `rlock` or `lock` for a value receiver, `lock` for a `mut` receiver.
fn test_method_calls_on_shared_array_elements_need_their_lock() {
	result := check_shared_element_lock('receivers', '
fn main() {
	mut items := []shared Foo{}
	items << Foo{}
	mut other := []shared Foo{}
	other << Foo{}
	println(items[0].get())
	println(items[0].bump())
	rlock items[0] {
		println(items[0].bump())
		println(other[0].get())
	}
	lock items[0] {
		println(items[0].bump())
		println(items[0].get())
	}
}
')
	assert result.exit_code != 0, result.output
	errors := result.output.split_into_lines().filter(it.contains(' error: ')).map(it.all_after(' error: '))
	assert errors == [
		'you have to create a handle and `rlock` it to use a `shared` element as non-mut receiver',
		'you have to create a handle and `lock` it to modify `shared` array element',
		'items[0] has an `rlock` but needs a `lock`',
		'`other[0]` is `shared` and must be `rlock`ed or `lock`ed to be used as non-mut receiver',
	], result.output
}
