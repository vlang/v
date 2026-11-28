module inner32402

pub struct Ctx {
pub mut:
	n int
}

pub fn (mut c Ctx) put[T](status int, v T) {
	c.n = status
}

pub fn (mut c Ctx) bump() {
	c.n++
}
