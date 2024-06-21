type Float = f32

fn test_increment_of_alias_in_for_c_loop() {
	min_value := Float(1)
	max_value := Float(10)
	step := Float(1)
	for n := min_value; n <= max_value; n += step {
		println('${n}')
		assert n > 0.0 && n < 11.0
	}
}
