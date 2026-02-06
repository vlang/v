type Arr = [4]u8
type Arr2 = []u8
type ArrStr = [4]string

fn test_main() {
	a := Arr{}
	b := Arr{}

	assert a == b
	assert Arr{} == Arr{}
	assert Arr{} == [4]u8{}
	assert Arr{} == [u8(0), 0, 0, 0]!

	assert Arr2{} == Arr2{}
	assert Arr2{} == []u8{}

	assert ArrStr{} == ArrStr{}
}
