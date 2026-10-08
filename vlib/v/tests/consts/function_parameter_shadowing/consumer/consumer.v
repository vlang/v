module consumer

// call invokes its callback parameter.
pub fn call(f fn () int) int {
	return f() + 1
}

// call_local invokes a locally stored callback.
pub fn call_local(g fn () int) int {
	f := g
	return f() + 2
}

// once returns its callback unchanged.
pub fn once(g fn () int) fn () int {
	return g
}
