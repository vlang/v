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

// BoxRef is a pointer alias of Box; selectors see through it.
pub type BoxRef = &Box

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

// Config is an alias of an anonymous struct; its fields keep their `pub` sections.
pub type Config = struct {
	secret int
pub:
	shown int
}

pub fn new_config() Config {
	return Config{}
}

pub struct Circle {
	radius int
mut:
	secret int
pub:
	shown int
	width int
}

pub struct Square {
mut:
	secret int
pub:
	shown int
}

// Shape is a sum type whose variants share the private field `secret` and the public field `shown`.
pub type Shape = Circle | Square

// ShapeAlias is declared in this module, so the private fields of the variants stay private through it.
pub type ShapeAlias = Shape

pub fn new_shape() Shape {
	return Circle{}
}

// Fields shared by the variants are accessible inside the module that declares them.
pub fn (s Shape) total() int {
	return s.secret + s.shown
}

// Plain and Fancy are aliases of anonymous structs; their fields keep their `pub` sections.
pub type Plain = struct {
	extra int
mut:
	secret int
pub:
	shown int
}

pub type Fancy = struct {
mut:
	secret int
pub:
	shown int
}

// Look is a sum type of the anonymous struct aliases Plain and Fancy.
pub type Look = Plain | Fancy

pub fn new_look() Look {
	return Plain{}
}

pub fn (l Look) total() int {
	return l.secret + l.shown
}
