module asn1

type MyOct = string

fn (mo MyOct) tag() Tag {
	return default_octetstring_tag
}

fn (mo MyOct) payload() ![]u8 {
	return mo.bytes()
}

type MyStr = string

fn (ms MyStr) tag() Tag {
	return default_utf8string_tag
}

fn (ms MyStr) payload() ![]u8 {
	return ms.bytes()
}

struct TestStruct {
	n int
	a MyOct
	b MyStr
}

fn (t TestStruct) tag() Tag {
	return Tag{.universal, true, int(TagType.sequence)} // 0x30
}

fn (t TestStruct) payload() ![]u8 {
	kd := KeyDefault(map[string]Element{})
	out := make_payload[TestStruct](t, kd)!
	return out
}

fn test_struct_make_payload() ! {
	st := TestStruct{
		a: MyOct('aku')
		b: MyStr('dia')
	}
	// TestStruct is sequence
	out := encode(st)!
	expected := [u8(0x30), 0x0a, (0x04), 0x03, 0x61, 0x6b, 0x75, (0x0c), 0x03, 0x64, 0x69, 0x61]
	// without field option passed, its should be expected value
	assert out == expected
}

fn test_into_optional() ! {
	el := Boolean.new(true)
	orig_expected := [u8(0x01), 0x01, 0xff]

	without_option := encode(el)!
	assert without_option == orig_expected

	// marked this element as optional, make its serialized into empty bytes
	with_option_1 := encode_with_options(el, 'optional')!
	assert with_option_1 == []u8{}
}

// test for wrapping functionality
struct WrapperTest {
	attr string
	err  IError
	out  []u8
}

fn test_wrapping_functionality() ! {
	// raw boolean element
	elem := Boolean.new(true)
	orig_expected := [u8(0x01), 0x01, 0xff]

	data := [
		WrapperTest{'', none, orig_expected},
		// Tag{.contex_specific, true, 1} = 0b1010_0001
		WrapperTest{'context_specific:1; explicit; inner:1', none, [
			u8(0xa1),
			0x03,
			0x01,
			0x01,
			0xff,
		]},
		// Tag{.contex_specific, false, 1} // 0b1000_0001 = 0x81 = 129
		WrapperTest{'context_specific:1; implicit; inner:1', none, [
			u8(0x81), //
			0x01,
			0xff,
		]},
		//
		WrapperTest{'context_specific:1;inner:2', error('Invalid zonk or uncorerct mode value'), [
			u8(0xa1),
			0x01,
			0xff,
		]},
		WrapperTest{'application:5;explicit;inner:1', none, [
			u8(0x65),
			0x03,
			0x01,
			0x01,
			0xff,
		]},
		// same as above, but with implicit mode, 0b_01_0_00101
		WrapperTest{'application:5;implicit;inner:1', none, [
			u8(0x45),
			0x01,
			0xff,
		]},
		// marked as optional would not be encoded,
		WrapperTest{'application:5; optional;inner:2', error('Invalid zonk or uncorerct mode value'), []u8{}},
		WrapperTest{'application:5; explicit;inner:2', none, [
			u8(0x65),
			0x03,
			0x01,
			0x01,
			0xff,
		]},
		// wrapped into universal is error
		WrapperTest{'universal:50; explicit; inner:3', error('You have not provides correct options marker'), orig_expected},
		// marked as an optional
		WrapperTest{'private:10; optional', error('Invalid zonk or uncorerct mode value'), []u8{}},
		WrapperTest{'application:5;implicit', error('You provides incorrect inner number'), []u8{}},
		// 0b_11_0_00101 = c5
		WrapperTest{'private:5; implicit; inner:1', none, [
			u8(0xC5),
			0x01,
			0xff,
		]},
	]
	for i, item in data {
		out := encode_with_options(elem, item.attr) or {
			assert err == item.err
			continue
		}
		assert out == item.out
	}
}

// test for ElementList
fn test_for_element_list() ! {
	mut fields := ElementList([]Element{})

	a := Boolean.new(true)
	b := Boolean.new(false)

	fields << a
	fields << b

	fields_payload := fields.payload()!

	expected := [u8(0x01), 0x01, 0xff, u8(0x01), 1, 0]
	assert fields_payload == expected
	assert fields.encoded_len() == 6

	els := ElementList.from_bytes(expected)!
	assert els.len == 2
	assert els[0] is Boolean
	assert els[1] is Boolean
}
