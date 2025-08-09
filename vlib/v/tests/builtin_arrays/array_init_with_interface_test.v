interface Value {}

fn test_main() {
	mut a := []Value{}
	a << [Value(i32(1)), 2.0, 3]
	println(a)
	assert a == [Value(i32(1)), 2.0, 3]
}
