import x.new_json as json

struct StructType[T] {
mut:
	val T
}

pub struct StructTypeSub {
	test string
}

fn test_types() {
	assert json.decode[StructType[string]]('{"val": ""}')!.val == ''
	assert json.decode[StructType[string]]('{"val": "0"}')!.val == '0'
	assert json.decode[StructType[string]]('{"val": "1"}')!.val == '1'
	assert json.decode[StructType[string]]('{"val": "2"}')!.val == '2'

	assert json.decode[StructType[int]]('{"val": 0}')!.val == 0
	assert json.decode[StructType[int]]('{"val": 1}')!.val == 1
	assert json.decode[StructType[int]]('{"val": 2}')!.val == 2

	assert json.decode[StructType[StructTypeSub]]('{"val": {"test": "test"}}')!.val.test == 'test'
}
