struct Foo {
mut:
	bar ?fn (string) string
}

fn test_assign_option_of_struct_fntype_field() {
	mut foo := Foo{}
	foo.bar = fn (s string) string {
		return s
	}
	if ff := foo.bar {
		ret := ff('hello')
		println(ret)
		assert ret == 'hello'
	}
}
