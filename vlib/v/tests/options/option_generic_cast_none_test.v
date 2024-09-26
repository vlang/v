pub type MyFn[T] = fn (mut my_state T)

fn higher_order_fn[T](initial_state T, maybe_callback ?MyFn[T]) State {
	mut my_state := initial_state
	if callback := maybe_callback {
		callback[T](mut my_state)
		callback[T](mut my_state)
	}
	return my_state
}

pub struct State {
mut:
	x int
}

fn handler(mut my_state State) {
	my_state.x += 1
}

fn test_main() {
	mut state := State{}
	state = higher_order_fn[State](state, ?MyFn[State](handler))
	state = higher_order_fn[State](state, ?MyFn[State](none))
	state = higher_order_fn[State](state, handler)
	assert state.x == 4
}
