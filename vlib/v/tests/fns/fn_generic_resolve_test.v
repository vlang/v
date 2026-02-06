pub type MyFn[T] = fn (mut my_state T)

fn higher_order_fn[T](initial_state T, callback MyFn[T]) T {
	mut my_state := initial_state

	callback[T](mut my_state)
	callback[T](mut my_state)

	return my_state
}

pub struct Stateless {}

pub fn higher_order_fn_stateless(callback MyFn[Stateless]) {
	initial_state := Stateless{}
	higher_order_fn[Stateless](initial_state, callback)
}

pub struct State {
mut:
	x int
}

fn handler(mut my_state State) {
	my_state.x += 1
}

fn test_main() {
	initial_state := State{}
	new_state := higher_order_fn[State](initial_state, handler)
	assert new_state.x == 2
}
