fn test_map_of_f32() {
	mut m32 := map[f32]string{}
	m32[1.0] = 'one'
	println(m32)
	assert '$m32' == r"{1.: 'one'}"
	for k, v in m32 {
		assert typeof(k).name == 'f32'
		assert typeof(v).name == 'string'
		assert k == 1.0
		assert v == 'one'
	}
}

fn test_map_of_f64() {
	mut m64 := map{
		3.14: 'pi'
	}
	m64[1.0] = 'one'
	println(m64)
	assert '$m64' == r"{3.14: 'pi', 1.: 'one'}"
	for k, v in m64 {
		assert typeof(k).name == 'f64'
		assert typeof(v).name == 'string'
		assert k in [1.0, 3.14]
		assert v in ['pi', 'one']
	}
}
