import rand
import strings

fn test_strings_builder_shift_array_reverse() {
	mut a := strings.new_builder(0)
	mut b := strings.new_builder(0)
	b << rand.ascii(5).bytes()
	a << b.reverse()
	ret := a.str()
	println(ret)
	assert ret.len == 5
}
