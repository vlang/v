module input

pub fn init(cfg Config) &Context {
	panic("term.input: error: Windows support isn't implemented yet")
	return &Context{}
}

pub fn (mut ctx Context) run() {
	panic("term.input: error: Windows support isn't implemented yet")
}

// TODO: remove, only here so that the test compiles
fn escape_end(buf string) int {
	return 0
}
