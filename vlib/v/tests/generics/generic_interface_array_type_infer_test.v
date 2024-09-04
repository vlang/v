struct Metadata {}

struct TagOrText {}

pub interface Layout[R] {
	render(content []R) string
}

struct DefaultLayout {
}

pub fn (self DefaultLayout) to_layout() Layout[TagOrText] {
	return self
}

pub fn (self DefaultLayout) render(content []TagOrText) string {
	return 'Hello world'
}

fn test_generic_interface_array_type_infer() {
	ret := DefaultLayout{}.to_layout().render([])
	println(ret)
	assert ret == 'Hello world'
}
