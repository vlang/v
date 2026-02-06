import x.json2

struct TestStruct {
	age u16 = 25
}

fn test_u16_in_struct() {
	original := TestStruct{}
	encoded := json2.encode(original)
	println('Encoded: ${encoded}')

	decoded := json2.decode[TestStruct](encoded) or { panic('Failed to decode: ${err}') }
	println('Decoded: ${decoded}')
	println('Age value: ${decoded.age}')

	assert decoded.age == 25, 'Expected age 25, got ${decoded.age}'
}

fn test_u16_direct() {
	decoded := json2.decode[u16]('25') or { panic('Failed: ${err}') }
	println('Direct u16 decode of "25": ${decoded}')
	assert decoded == 25, 'Expected 25, got ${decoded}'
}

fn main() {
	test_u16_direct()
	test_u16_in_struct()
	println('All tests passed!')
}
