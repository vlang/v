import json

[json_as_number]
pub enum MessageType {
	error = 1
	warning = 2
	info = 3
	log = 4
}

pub enum MessageType2 {
	error = 1
	warning = 2
	info = 3
	log = 4
}

enum TestEnum {
	one = 1
	two
}

type TestAlias = TestEnum
type TestSum = TestEnum | string
type TestSum2 = MessageType | string
type TestAliasAttr = MessageType

struct TestStruct {
	test  []TestEnum
	test2 TestEnum
	test3 TestAlias
	test4 TestSum
	test5 MessageType
}

struct TestStruct2 {
	a TestAliasAttr
	b TestSum2
	c TestSum2
}

fn test_encode_with_enum() {
	out := json.encode(TestStruct{
		test: [TestEnum.one, TestEnum.one]
		test2: TestEnum.two
		test3: TestEnum.one
		test4: TestEnum.two
		test5: .log
	})
	assert out == '{"test":["one","one"],"test2":"two","test3":"one","test4":"two","test5":4}'
}

fn test_encode_direct_enum() {
	assert json.encode(TestEnum.one) == '"one"'
}

fn test_encode_alias_and_sumtype() {
	assert json.decode(TestStruct, '{"test":["one","one"],"test2":"two","test3": "one", "test4": "two", "test5":4}')! == TestStruct{
		test: [.one, .one]
		test2: .two
		test3: TestAlias(.one)
		test4: TestSum('two')
		test5: .log
	}
}

fn test_enum_attr() {
	assert dump(json.encode(MessageType.log)) == '4'
	assert dump(json.encode(MessageType.error)) == '1'
}

fn test_enum_attr_decode() {
	assert json.decode(TestStruct2, '{"a": 1, "b":4, "c": "test"}')! == TestStruct2{
		a: .error
		b: MessageType.log
		c: 'test'
	}
}

fn test_enum_attr_encode() {
	assert json.encode(TestStruct2{
		a: .error
		b: MessageType.log
		c: 'test'
	}) == '{"a":1,"b":4,"c":"test"}'
}
