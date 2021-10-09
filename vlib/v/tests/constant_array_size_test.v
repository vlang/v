const (
	c_a_s = 1
	c_b_s = 1 + 1
	c_c_s = c_b_s + 1 // this should be also fold by transformer since it's a constant
)

fn test_consant_array_size() {
	mut a := [c_a_s]int{}
	a = [1]!
	mut b := [c_b_s]int{}
	b = [1, 2]!
}
