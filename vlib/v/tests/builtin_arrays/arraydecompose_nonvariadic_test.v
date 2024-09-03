type Any = f64 | int | string

struct Test {}

struct Test2 {}

struct Test3 {}

fn (t Test) test(args ...Any) {
	println('called with ${args}')
}

fn (t Test2) test_str(arg string) {
	println('called with ${arg}')
}

fn (t Test2) test_int(arg int, arg2 int) {
	println('called with ${arg}, ${arg2}')
}

fn (t Test3) test_int(arg int, arg2 int) {
	println('called with ${arg}, ${arg2}')
}

fn foo_any(i Any, k Any, j Any) string {
	return '${i} : ${k} : ${j}'
}

fn foo(i int, k int) string {
	return '${i} : ${k}'
}

fn bar(i f64, k f64, j f64) string {
	return '${i} : ${k} : ${j}'
}

fn baz(s string) string {
	return s
}

fn f_arr(i int, f f64) string {
	return '${i} : ${f}'
}

fn f_var(s string, args ...string) string {
	return '${s} [ ${args.map(it).join(',')} ]'
}

fn varargs[T](args ...T) string {
	assert args.len > 0
	return args.map(it.str()).join(' : ')
}

fn call[T](func_name string, args ...T) string {
	return match func_name {
		'foo' { foo(...args) }
		'bar' { bar(...args) }
		'baz' { baz(...args) }
		'varargs' { varargs(...args) }
		else { '' }
	}
}

fn call_any(func_name string, args ...Any) string {
	return match func_name {
		'foo_any' { foo_any(...args) }
		else { '' }
	}
}

fn comptime_call[T](instance T, method_name string, args ...Any) bool {
	$for method in T.methods {
		if method.name == method_name {
			instance.$method(...args)
			return true
		}
	}
	return false
}

fn comptime_call_vargs[T, R](instance T, method_name string, args ...R) bool {
	$for method in T.methods {
		if method.name == method_name {
			instance.$method(...args)
			return true
		}
	}
	return false
}

fn comptime_call_vargs2[T, R](instance T, method_name string, args ...R) bool {
	$for method in T.methods {
		if method.name == method_name {
			instance.$method(200, ...args)
			return true
		}
	}
	return false
}

fn test_main() {
	assert call('foo', 10, 100) == '10 : 100'
	assert call('bar', 1.1, 1.2, 1.3) == '1.1 : 1.2 : 1.3'
	assert call('baz', 'test') == 'test'
	assert call_any('foo_any', 10, 1.2, 'test') == "Any(10) : Any(1.2) : Any('test')"
	assert call[Any]('varargs', 10, 1.2, 'test') == "Any(10) : Any(1.2) : Any('test')"

	a := []int{len: 2, init: 50}
	assert foo(...a) == '50 : 50'

	b := []f64{len: 3, init: 1.2}
	assert bar(...b) == '1.2 : 1.2 : 1.2'

	mut c := []Any{}
	c << 10
	c << 1.2
	c << 'test'
	assert varargs(...c) == "Any(10) : Any(1.2) : Any('test')"

	var := [0.0]
	assert f_arr(1, ...var) == '1 : 0.0'

	var2 := ['a', 'b', 'c']
	assert f_var('foo', ...var2) == 'foo [ a,b,c ]'
}

fn test_comptime() {
	var := Test{}
	assert comptime_call(var, 'test', 1, 2.3, '')

	var2 := Test2{}
	assert comptime_call_vargs(var2, 'test_int', 1, 100)
	assert comptime_call_vargs(var2, 'test_str', 'foo')

	var3 := Test3{}
	assert comptime_call_vargs2(var3, 'test_int', 100)
}
