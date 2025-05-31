module simple

pub fn sum() int {
	mut res := 0
	for i in 1 .. 5 {
		res += i
	}
	return res
}

pub fn mul() int {
	mut res := 1
	for i in 1 .. 5 {
		res *= i
	}
	// the lines here are just to introduce an asymmetry in the reported coverage lines
	c := res * 2
	_ := c * 10
	return res
}

pub const a_const = f()

fn f() int {
	return 50 // this should be executed always
}
