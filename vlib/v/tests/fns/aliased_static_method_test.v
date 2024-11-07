module main

struct MyStruct {}

fn MyStruct.new() {}

type MyAlias = MyStruct

fn test_main() {
	MyStruct.new()
	MyAlias.new()
	assert true
}
