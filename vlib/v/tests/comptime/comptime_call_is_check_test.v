module main

struct Struct1 {
	num f64
}

pub type DoThingFunc = fn (f32) bool

fn (mut s Struct1) do_thing(a f32) bool {
	return false
}

fn register[T]() {
	obj := T{}
	mut called := false
	$for method in T.methods {
		$if method !is DoThingFunc {
			println('method ${method.name} is not do thing')
			assert false
		} $else {
			println('method ${method.name} is do thing')
			assert obj.$method(1.23) == false
			called = true
		}
	}
	assert called
}

fn test_main() {
	register[Struct1]()
}
