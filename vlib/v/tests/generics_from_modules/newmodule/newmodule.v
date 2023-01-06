module newmodule

pub struct Params[T] {
mut:
	a []T
	b int
	c T
}

pub fn take_input[T](p Params[T]) []f32 {
	mut res := []f32{}
	for x in p.a {
		res << f32(x)
	}
	res << f32(p.b)
	res << f32(p.c)
	return res
}
