import v.tests.generics_from_modules.newmodule as nm

fn test_infer_generic_struct() {
	a_in := [int(1), 2, 4]
	res := nm.take_input(a: a_in, b: 3, c: int(5))

	println(res)
	assert res == [f32(1.0), 2.0, 4.0, 3.0, 5.0]
}
