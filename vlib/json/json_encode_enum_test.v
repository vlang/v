import json

enum TestEnum {
	one = 1
	two
}

struct TestStruct {
	test  []TestEnum
	test2 TestEnum
}

fn test_encode_with_enum() {
	out := json.encode(TestStruct{ test: [TestEnum.one, TestEnum.one], test2: TestEnum.two })
	assert out == '{"test":["one","one"],"test2":"two"}'
}

fn test_encode_direct_enum() {
	assert json.encode(TestEnum.one) == '"one"'
}
