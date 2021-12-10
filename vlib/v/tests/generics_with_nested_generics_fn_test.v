struct NestedGeneric {
}

struct Context {
}

struct App {
mut:
	context Context
}

fn (ng NestedGeneric) nested_test<T>(mut app T) {
	app.context = Context{}
}

fn method_test<T>(mut app T) int {
	ng := NestedGeneric{}
	ng.nested_test<T>(app)
	return 22
}

fn test_generics_with_generics_fn() {
	mut app := App{}
	ret := method_test(mut app)
	println('result = $ret')
	assert ret == 22
}
