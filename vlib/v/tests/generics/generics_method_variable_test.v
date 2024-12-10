type Func = fn (string) string

struct Struct[T] {
	a T
mut:
	func Func
}

fn (st Struct[T]) foo[T](s string) string {
	println('${st.a} - ${s}')
	return '${st.a} - ${s}'
}

fn test_generic_method_variable() {
	mut st := Struct[int]{
		a: 22
	}
	st.func = st.foo
	ret := st.func('hello')
	assert ret == '22 - hello'
}
