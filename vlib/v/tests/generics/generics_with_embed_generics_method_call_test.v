pub type Handler[T] = fn (mut T) bool

struct Collector[T] {
mut:
	handlers []Handler[T]
}

pub fn (mut c Collector[T]) use(func Handler[T]) {
	c.handlers << func
}

pub struct Context {}

fn handler(mut ctx Context) bool {
	println('from test_handler!')
	return true
}

pub struct MainStruct {
	Collector[Context]
}

fn test_generics_with_embed_generic_method_call() {
	mut s := MainStruct{}

	s.use(handler)

	assert s.handlers.len == 1
}
