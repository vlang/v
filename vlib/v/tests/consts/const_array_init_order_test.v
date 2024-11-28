const cst3 = [[[cst1, cst2]]]
const cst1 = [11]
const cst2 = [22]

fn test_const_array_init_order() {
	println(cst3)
	assert cst3 == [[[[11], [22]]]]
}
