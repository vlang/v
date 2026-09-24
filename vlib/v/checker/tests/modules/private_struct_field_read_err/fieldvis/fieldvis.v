module fieldvis

pub struct Inner {
	hidden int
pub:
	shown int
}

pub struct Box {
	Inner
	private int
	cb      fn () int = default_cb
mut:
	secret int
pub:
	readable int
pub mut:
	writable int
__global:
	global int
}

// BoxAlias is declared in this module, so the private fields of Box stay private through it.
pub type BoxAlias = Box

fn default_cb() int {
	return 8
}

pub fn new_box() Box {
	return Box{
		private:  1
		secret:   2
		readable: 3
		writable: 4
	}
}

// Fields are always accessible inside the module that declares their struct.
pub fn (b Box) sum() int {
	return b.private + b.secret + b.readable + b.writable + b.hidden + b.shown + b.cb()
}
