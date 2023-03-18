import json

enum TestEnum {
	one = 1
	two
}

type TestAlias = TestEnum
type TestSum = TestEnum | string

struct TestStruct {
	test  []TestEnum
	test2 TestEnum
	test3 TestAlias
	test4 TestSum
}

fn test_encode_with_enum() {
	out := json.encode(TestStruct{
		test: [TestEnum.one, TestEnum.one]
		test2: TestEnum.two
		test3: TestEnum.one
		test4: TestEnum.two
	})
	assert out == '{"test":["one","one"],"test2":"two","test3":"one","test4":"two"}'
}

fn test_encode_direct_enum() {
	assert json.encode(TestEnum.one) == '"one"'
}

fn test_encode_alias_and_sumtype() {
	assert json.decode(TestStruct, '{"test":["one","one"],"test2":"two","test3": "one", "test4": "two"}')!.str() == "TestStruct{
    test: [one, one]
    test2: two
    test3:     TestAlias(one)
    test4: TestSum('two')
}"
}
