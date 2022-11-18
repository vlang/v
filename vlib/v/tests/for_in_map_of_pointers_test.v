fn test_for_in_map_of_pointers() {
	i := 123456
	pi := &i
	println('pi: ${pi}')
	mut my_map := map[int]&int{}
	my_map[0] = pi
	for k, wrong_value in my_map {
		good_value := my_map[k]
		println('k: ${k} \n wrong_value: ${wrong_value} \n good_value: ${good_value}')
		up_good_value := voidptr(good_value).hex_full()
		up_wrong_value := voidptr(wrong_value).hex_full()
		assert up_good_value == up_wrong_value
	}
}
