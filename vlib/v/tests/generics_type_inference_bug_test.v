import v.tests.generics_type_inference_bug

fn test_f64_multiplication_should_return_f64() {
	a1 := f64(10.0)
	b1 := f64(20.0)
	c1 := f64(30.0)
	a2 := f64(1.0)
	b2 := f64(2.0)
	c2 := f64(3.0)

	v := generics_type_inference_bug.vec3[f64](a1, b1, c1)
	u := generics_type_inference_bug.vec3[f64](a2, b2, c2)

	// Call the method that performs multiplication
	result := v.multiply_test(u)

	// The result should be f64, not f32!
	// This test will help us verify the type
	assert typeof(result).name == 'f64'
	println('Result type: ${typeof(result).name}, value: ${result}')
}
