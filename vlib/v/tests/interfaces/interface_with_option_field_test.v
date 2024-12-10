import time

interface InterfaceObject {
mut:
	duration ?time.Duration
	update()
}

struct Object {
mut:
	duration ?time.Duration
}

fn (mut obj Object) update() {
	duration := obj.duration or { time.second }
	println(duration)
}

struct FooObject {
	Object
}

fn (mut obj FooObject) update() {
	duration := obj.duration or { time.millisecond * 500 }
	println(duration)
}

fn test_interface_with_option_field() {
	mut object := InterfaceObject(FooObject{})
	println(object)
	assert '${object.duration}' == 'Option(none)'
}
