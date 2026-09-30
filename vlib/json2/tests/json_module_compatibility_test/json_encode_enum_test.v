// vtest vflags: -w
import json2

@[json_as_number]
pub enum MessageType {
	error   = 1
	warning = 2
	info    = 3
	log     = 4
}

pub enum MessageType2 {
	error   = 1
	warning = 2
	info    = 3
	log     = 4
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

struct Test {
	ab ?int
	a  ?MessageType
}

struct Test2 {
	a ?MessageType2
}

type TSum = MessageType | string
type TSum2 = MessageType2 | string

struct Test3 {
	a ?TSum
}

struct Test4 {
	a ?TSum2
}

fn test_encode_with_enum() {
	out := json2.encode(TestStruct{
		test:  [TestEnum.one, TestEnum.one]
		test2: TestEnum.two
		test3: TestEnum.one
		test4: TestEnum.two
		test5: .log
	}, escape_unicode: true)
	assert out == '{"test":["one","one"],"test2":"two","test3":"one","test4":"two","test5":4}'
}

fn test_encode_direct_enum() {
	assert json2.encode(TestEnum.one, escape_unicode: true) == '"one"'
}

fn test_encode_alias_and_sumtype() {
	assert json2.decode[TestStruct]('{"test":["one","one"],"test2":"two","test3": "one", "test4": "two", "test5":4}')! == TestStruct{
		test:  [.one, .one]
		test2: .two
		test3: TestAlias(.one)
		test4: TestSum('two')
		test5: .log
	}
}

fn test_enum_attr() {
	assert dump(json2.encode(MessageType.log, escape_unicode: true)) == '4'
	assert dump(json2.encode(MessageType.error, escape_unicode: true)) == '1'
}

fn test_enum_attr_decode() {
	assert json2.decode[TestStruct2]('{"a": 1, "b":4, "c": "test"}')! == TestStruct2{
		a: .error
		b: MessageType.log
		c: 'test'
	}
}

fn test_enum_attr_encode() {
	assert json2.encode(TestStruct2{
		a: .error
		b: MessageType.log
		c: 'test'
	}, escape_unicode: true) == '{"a":1,"b":4,"c":"test"}'
}

fn test_option_enum() {
	assert dump(json2.encode(Test{none, none}, escape_unicode: true)) == '{}'
	assert dump(json2.encode(Test{none, MessageType.log}, escape_unicode: true)) == '{"a":4}'
	t := dump(json2.decode[Test]('{"a":4}')!)
	assert t.ab == none
	assert t.a? == .log

	t2 := dump(json2.decode[Test]('{"a":null}')!)
	assert t2.a == none

	assert json2.encode(Test2{none}, escape_unicode: true) == '{}'
	assert dump(json2.encode(Test2{MessageType2.log}, escape_unicode: true)) == '{"a":"log"}'
	z := dump(json2.decode[Test2]('{"a":"log"}')!)
	assert z.a? == .log
	a := dump(json2.decode[Test2]('{"a": null}')!)
	assert a.a == none
}

fn test_option_sumtype_enum() {
	assert dump(json2.encode(Test3{none}, escape_unicode: true)) == '{}'
	assert dump(json2.encode(Test3{ a: 'foo' }, escape_unicode: true)) == '{"a":"foo"}'
	assert dump(json2.encode(Test3{ a: MessageType.warning }, escape_unicode: true)) == '{"a":2}'

	assert dump(json2.encode(Test4{none}, escape_unicode: true)) == '{}'
	assert dump(json2.encode(Test4{ a: 'foo' }, escape_unicode: true)) == '{"a":"foo"}'
	assert dump(json2.encode(Test4{ a: MessageType2.warning }, escape_unicode: true)) == '{"a":"warning"}'
}
