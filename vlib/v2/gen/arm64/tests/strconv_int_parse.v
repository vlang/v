module main

fn main() {
	assert '6'.int() == 6
	assert '10'.int() == 10
	assert '13'.int() == 13
	assert '-1'.int() == -1
	assert '10'.i32() == i32(10)
	println('32'.u64())
	println('32'.i64())
	println('-32'.i64())
	println('0x20'.u64())
}
