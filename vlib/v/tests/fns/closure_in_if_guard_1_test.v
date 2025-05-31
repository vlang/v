struct Foo {
	optional ?int
}

fn test_closure_in_if_guard() {
	f := Foo{45}
	mut ret := ''
	if v := f.optional {
		func := fn [v] () string {
			println(v)
			return '${v}'
		}
		ret = func()
	}
	assert ret == '45'
}
