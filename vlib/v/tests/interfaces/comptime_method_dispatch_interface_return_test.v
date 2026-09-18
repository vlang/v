interface IFoo {
	name string
}

struct Foo {
	name string
}

struct Model {
mut:
	result string
}

fn (model Model) get_foo() IFoo {
	return Foo{
		name: 'interface value'
	}
}

fn (mut model Model) run() {
	value := model.get_foo()
	model.result = value.name
}

fn dispatch[T](mut model T) {
	$for method in T.methods {
		if method.name == 'run' {
			$if method.typ is fn ( ) {
				model.$method()
				return
			}
		}
	}
}

fn test_comptime_method_dispatch_preserves_interface_return_values() {
	mut model := Model{}
	dispatch(mut model)
	assert model.result == 'interface value'
}
