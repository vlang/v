pub type EventListener[T] = fn (T) !

pub type Check[T] = fn (T) bool

pub struct EventController[T] {
mut:
	id        int
	listeners map[int]EventListener[T]
}

fn (mut ec EventController[T]) generate_id() int {
	return ec.id++
}

@[params]
pub struct EmitOptions {
pub:
	error_handler ?fn (int, IError)
}

pub fn (mut ec EventController[T]) emit(e T, options EmitOptions) {
	if ec.listeners.len == 1 {
		f := ec.listeners.values()[0]
		f(e) or {
			if g := options.error_handler {
				g(0, err)
			}
		}
		return
	}
}

struct Foo {}

struct Bar {}

fn test_generic_array_method_call_with_multi_types() {
	foo := EventController[Foo]{}
	println(foo)
	assert foo.id == 0
	bar := EventController[Bar]{}
	println(bar)
	assert bar.id == 0
}
