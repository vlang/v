module main

struct GenShape {
mut:
	cache []int
	stack []bool
}

fn new_gen_shape(n int) &GenShape {
	return &GenShape{
		cache: []int{len: n}
		stack: []bool{len: n}
	}
}

fn main() {
	g := new_gen_shape(256)
	mut sum := 0
	for i := 0; i < g.cache.len; i++ {
		sum += g.cache[i]
	}
	println(sum)
	println(g.cache.len)
}
