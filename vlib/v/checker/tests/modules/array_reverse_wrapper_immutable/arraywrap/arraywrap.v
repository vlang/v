module arraywrap

pub type MyArray = []int

pub fn (mut a MyArray) flip() {
	a.reverse_in_place()
}
