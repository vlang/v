fn test_decl_assignment() {
	my_var := 12
	c1 := fn [my_var] () int {
		return my_var
	}
	assert c1() == 12
}

fn test_assignment() {
	v1 := 1
	mut c := fn [v1] () int {
		return v1
	}
	v2 := 3
	c = fn [v2] () int {
		return v2
	}
	assert c() == 3
}

fn call_anon_with_3(f fn (int) int) int {
	return f(3)
}

fn test_closure_in_call() {
	my_var := int(12)
	r1 := call_anon_with_3(fn [my_var] (add int) int {
		return my_var + add
	})
	assert r1 == 15
}

fn create_simple_counter() fn () int {
	mut c := -1
	return fn [mut c] () int {
		c++
		return c
	}
}

fn override_stack() {
	// just create some variables to modify the stack
	a := 1
	b := a + 2
	c := b + 3
	d := c + 4
	e := d + 5
	f := e + 6
	g := f + 7
	_ = g
}

fn test_closure_exit_original_scope() {
	mut c := create_simple_counter()
	assert c() == 0
	override_stack()
	assert c() == 1
}

fn test_vars_are_changed() {
	mut my_var := 1
	f1 := fn [mut my_var] (expected int) {
		assert my_var == expected
	}
	f1(1)
	my_var = 3
	f1(1)
	my_var = -1
	f1(1)
}

struct Counter {
mut:
	i u64
}

fn (mut c Counter) incr() {
	c.i++
}

fn (mut c Counter) next() u64 {
	c.incr()
	return c.i
}

fn test_call_methods() {
	mut c := Counter{}
	f1 := fn [mut c] () u64 {
		return c.next()
	}
	assert f1() == 1
	assert f1() == 2
	c.incr()
	assert f1() == 3
}

fn test_call_methods_on_pointer() {
	mut c := &Counter{}
	f1 := fn [mut c] () u64 {
		return c.next()
	}
	assert f1() == 1
	assert f1() == 2
	c.incr()
	assert f1() == 4
}

/*
fn test_methods_as_variables() {
	mut c := Counter{}
	f1 := c.next
	assert f1() == 1
	assert f1() == 2
	c.incr()
	assert f1() == 3
}
*/

/*
fn takes_callback(get fn () u64) u64 {
	return get()
}

fn test_methods_as_callback() {
	mut c := Counter{}
	assert takes_callback(c.next) == 1
}
*/

struct ZeroSize {}

fn test_zero_size_ctx() {
	ctx := ZeroSize{}
	c1 := fn [ctx] () int {
		return 123
	}
	assert c1() == 123
}

/*
fn test_go_call_closure() {
	my_var := 12
	ch := chan int{}
	go fn [my_var, ch] () {
		ch <- my_var
	}()
	assert <-ch == 12
	go fn [ch] (arg int) {
		ch <- arg
	}(15)
	assert <-ch == 15
}
*/

fn test_closures_with_ifstmt() {
	a := 1
	f := fn [a] (x int) int {
		if a > x { return 1
		 } else { return -1
		 }
	}
	g := fn [a] () int {
		if true {
			return 1
		}
		return 0
	}
	assert f(0) == 1
	assert g() == 1
}

struct Command {
	a int = 1234
	b int = 2345
	c int = 3456
	d int = 4567
	e int = 5678
}

struct App {}

fn test_larger_closure_parameters() {
	mut app := &App{}
	eprintln('app ptr: ${u64(app)}')
	f := fn [mut app] (cmd Command) u64 {
		p := u64(app)
		println('>>   p: ${p}')
		println('>> cmd: ${cmd}')
		assert cmd.a == 1234
		assert cmd.b == 2345
		assert cmd.c == 3456
		assert cmd.d == 4567
		assert cmd.e == 5678
		return p
	}
	cmd := Command{}
	res := f(cmd)
	println('> res: ${res} | sizeof Command: ${sizeof(Command)}')
	assert res == u64(app)
}

fn test_closure_in_for_in_loop() {
	a := [2, 4, 6, 9]
	for v in a {
		func := fn [v] (msg string) string {
			return '${msg}: ${v}'
		}
		res := func('hello')
		assert res == 'hello: ${v}'
		// dump(res)
	}
}

fn ret_two() (int, int) {
	return 2, 5
}

fn test_closure_over_variable_that_is_returned_from_a_multi_value_function() {
	one, two := ret_two()
	a := fn [one] () {
		println(one)
	}
	a()
	println(two)
}

fn test_cross_var_assign_without_inherited() {
	f := fn () {
		mut left := 1
		mut right := 2
		left, right = right, left
		assert left == 2 && right == 1
	}
	f()
}

fn test_cross_var_assign_with_inherited() {
	mut left := 1
	mut right := 2
	f := fn [mut left, mut right] () {
		left, right = right, left
		assert left == 2 && right == 1
	}
	f()
}

// for issue 20498
// test array / string / map as closure params with -autofree
fn get_func_that_contains_closure() fn () {
	arr := [1, 2, 3]
	str := '${'a'}bcabc' // alloc on heap
	m := {
		'key1': 'abcabc'
		'key2': 'abcabc'
	}
	return fn [arr, str, m] () {
		assert arr == [1, 2, 3]
		assert str == 'abcabc'
		assert m == {
			'key1': 'abcabc'
			'key2': 'abcabc'
		}
	}
}

fn test_array_string_and_map_as_closure_params_with_autofree() {
	func := get_func_that_contains_closure()
	func()
	assert true
}

// for issue 20208
// phenomenon: cgen fails when the closure arg is auto_heap and is not reference.
@[heap]
struct Abc {
	value int
}

@[heap]
struct Container {
	abc &Abc = unsafe { nil }
}

fn (mut c Container) m() fn () {
	mut cr := &c
	assert voidptr(c.abc) == voidptr(cr.abc)
	f := fn [mut cr] () {
		assert cr.abc.value == 1234
	}
	return f
}

fn test_auto_heap_var_and_non_ptr_as_closure_arg() {
	mut c := &Container{
		abc: &Abc{1234}
	}
	f := c.m()
	f()
	assert true
}
