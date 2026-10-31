// Regression test for https://github.com/vlang/v/issues/30842
//
// A generic fn with a `chan T` parameter, called without explicit type
// arguments, was type checked but never instantiated for the C backend:
// `T` was not inferred from the channel argument's element type.

struct Msg {
	text string
}

fn identity_chan[T](ch chan T) chan T {
	return ch
}

fn chan_elem_default[T](_ch chan T) T {
	return T{}
}

fn first_of[T](ch chan T) T {
	return <-ch
}

fn count_of[T](items []T) int {
	return items.len
}

fn test_implicit_chan_identity() {
	ch := chan string{cap: 2}
	x := identity_chan(ch)
	ch <- 'a'
	assert <-x == 'a'
	assert typeof(x).name == 'chan string'
}

fn test_implicit_chan_int() {
	ch := chan int{cap: 1}
	ch <- 42
	assert first_of(ch) == 42
	assert chan_elem_default(ch) == 0
}

fn test_implicit_chan_struct_elem() {
	ch := chan Msg{cap: 1}
	ch <- Msg{
		text: 'hello'
	}
	m := first_of(ch)
	assert m.text == 'hello'
	assert chan_elem_default(ch).text == ''
}

fn test_implicit_chan_nested_call() {
	ch := chan string{cap: 2}
	ch <- 'x'
	ch <- 'y'
	// the inner implicit call's return type feeds the outer inference
	y := identity_chan(identity_chan(ch))
	assert <-y == 'x'
	assert first_of(identity_chan(ch)) == 'y'
}

fn test_explicit_chan_still_works() {
	ch := chan string{cap: 1}
	ch <- 'e'
	assert <-identity_chan[string](ch) == 'e'
}
