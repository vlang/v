import x.encoding.asn1

// This example test taken from https://en.wikipedia.org/wiki/ASN.1#Example_encoded_in_DER
// for testing public api of the `x.encoding.asn1` module.

// FooQuestion ::= SEQUENCE {
//      trackingNumber INTEGER(0..199),
//      question       IA5String
// }

// Instance of FooQuestion
// myQuestion FooQuestion ::= {
//    trackingNumber     5,
//    question           "Anybody there?"
// }

// Below is the data structure shown above as myQuestion encoded in DER format
// (all numbers are in hexadecimal):
// 30 13 02 01 05 16 0e 41 6e 79 62 6f 64 79 20 74 68 65 72 65 3f

// Above schema can be represented in v struct,
struct FooQuestion {
	tracking_number asn1.Integer
	question        asn1.IA5String
}

// To be supported as an Element by this module, your FooQuestion should fullfills
// required method of Element interface by providing `tag()` and `payload()` methods.
fn (foo FooQuestion) tag() asn1.Tag {
	return asn1.default_sequence_tag
}

fn (foo FooQuestion) payload() ![]u8 {
	mut out := []u8{}

	out << asn1.encode(foo.tracking_number)!
	out << asn1.encode(foo.question)!

	return out
}

// FooQuestion.decode is a routine decoder from bytes into FooQuestion.
fn FooQuestion.decode(bytes []u8) !FooQuestion {
	// decode the bytes, and check for sequence tag
	elem := asn1.decode(bytes)!
	assert elem.tag().equal(asn1.default_sequence_tag)

	// turn into Sequence
	seq := elem.into_object[asn1.Sequence]()!
	// get the fields, ie, arrays of Element
	fields := seq.fields()

	// turns the every sequence's fields into desired object
	trk_num := fields[0].into_object[asn1.Integer]()!
	question := fields[1].into_object[asn1.IA5String]()!

	foo := FooQuestion{
		tracking_number: trk_num
		question:        question
	}
	return foo
}

fn test_asn1_public_api_usage() ! {
	expected_foo := [u8(0x30), 0x13, 0x02, 0x01, 0x05, 0x16, 0x0e, 0x41, 0x6e, 0x79, 0x62, 0x6f,
		0x64, 0x79, 0x20, 0x74, 0x68, 0x65, 0x72, 0x65, 0x3f]

	foo_question := FooQuestion{
		tracking_number: asn1.Integer.from_int(5)
		question:        asn1.IA5String.new('Anybody there?')!
	}

	foo_encoded := asn1.encode(foo_question)!
	assert foo_encoded == expected_foo

	// decode back
	foo_decoded := FooQuestion.decode(expected_foo)!
	assert foo_decoded.tracking_number.as_i64()! == 5
	assert foo_decoded.question.value == 'Anybody there?'
}
