module main

import x.encoding.asn1

// This example was taken from https://www.oss.com/asn1/resources/asn1-made-simple/asn1-quick-reference/sequence.html
// But little modified with removed optional key in the structure and serialization.
// Example schema:
//
// ModuleName DEFINITIONS AUTOMATIC TAGS ::= BEGIN
// PersonelEntry ::= SEQUENCE {
//         name     OCTET STRING,
//         location INTEGER { home(0), field(1), roving(2)} OPTIONAL,
//         age      INTEGER   OPTIONAL
// }
// END

// The syntax above is equivalent with the following one:

// PersonelEntry ::= SEQUENCE {
//         name     [0] IMPLICIT OCTET STRING,
//         location [1] IMPLICIT INTEGER {home(0), field(1), roving(2)} OPTIONAL,
//         age      [2] IMPLICIT INTEGER OPTIONAL
// }
struct PersonelEntry {
mut:
	name     asn1.OctetString @[context_specific: 0; implicit; inner: 4]
	location asn1.Integer     @[context_specific: 1; implicit; inner: 2]
	age      asn1.Integer     @[context_specific: 2; implicit; inner: 2]
}

fn (pr PersonelEntry) tag() asn1.Tag {
	return asn1.default_sequence_tag
}

fn (pr PersonelEntry) payload() ![]u8 {
	mut out := []u8{}

	// by default, in .der, optional element was not included in the output, so, we remove optional options here.
	out << asn1.encode_with_options(pr.name, 'context_specific:0; implicit; inner:4')!
	out << asn1.encode_with_options(pr.location, 'context_specific:1; implicit; inner:2')!
	// the example the third element is optional, but in .der it would not be serializable until you set it to present
	out << asn1.encode_with_options(pr.age, 'context_specific:2; implicit; inner:2')!

	return out
}

// This is an example of way how we can write routine for decode PersonelEntry from bytes.
fn PersonelEntry.decode(bytes []u8) !PersonelEntry {
	// decode should produces Sequence type
	elem := asn1.decode(bytes)!
	assert elem.tag().equal(asn1.default_sequence_tag)

	// cast it into Sequence type and get the fields
	seq := elem as asn1.Sequence
	fields := seq.fields()

	// every fields of the sequence is raw of wrapped element, so we should unwrap it with
	// the same options used to wrap in encode step, and turn to the real underlying object.
	el_name := fields[0].unwrap_with_options('context_specific:0; implicit; inner:4')!
	name := el_name.into_object[asn1.OctetString]()!

	el_location := fields[1].unwrap_with_options('context_specific:1; implicit; inner:2')!
	location := el_location.into_object[asn1.Integer]()!

	el_age := fields[2].unwrap_with_options('context_specific:2; implicit; inner:2')!
	age := el_age.into_object[asn1.Integer]()!

	return PersonelEntry{
		name:     name
		location: location
		age:      age
	}
}

// expected output :
// 30 10
//   80 08 6269672068656164 // bytestr: 'big head'
//   81 01 02
//   82 01 1A
fn main() {
	expected_output := [u8(0x30), 0x10, u8(0x80), 0x08, 0x62, 0x69, 0x67, 0x20, 0x68, 0x65, 0x61,
		0x64, u8(0x81), 0x01, 0x02, u8(0x82), 0x01, 0x1A]

	rock_star1 := PersonelEntry{
		name:     asn1.OctetString.from_hexstring('6269672068656164')!
		location: asn1.Integer.from_int(2)
		age:      asn1.Integer.from_int(26)
	}

	// serializes the object into bytes array and check the result
	out := asn1.encode(rock_star1)!
	dump(out == expected_output) //  out == expected_output: true

	// deserializes bytes back into PersonelEntry object.
	out_back := PersonelEntry.decode(out)!
	dump(out_back)
	// out_back: PersonelEntry{
	// 	name: OctetString (big head)
	// 	location: Integer 2
	// 	age: Integer 26
	// }
	dump(out_back.name == rock_star1.name) // true
	dump(out_back.location == rock_star1.location) // true
	dump(out_back.age == rock_star1.age) // true
}
