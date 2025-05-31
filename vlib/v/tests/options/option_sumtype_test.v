pub type TType = []int | string

pub fn teste() ?TType {
	return TType([1, 2])
}

fn test_main() {
	x := teste()
	if x != none {
		if x is []int {
			dump(x)
			y := x as []int
			assert y == [1, 2]
			return
		}
	}
	assert false
}
