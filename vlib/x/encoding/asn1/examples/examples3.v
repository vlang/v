module main

import x.encoding.asn1

// This example of encode a sequence containing a UTF-8 string, an integer
// and an explicitly tagged object identifier, conforming to the following
// ASN.1 schema specification:

// ```asn.1
// Example ::= SEQUENCE {
//    greeting    UTF8String,
//    answer      INTEGER,
//    type        [1] EXPLICIT OBJECT IDENTIFIER
// }
// ```

// You can represent above structure with related structure in `v`, similar like:

struct Example {
	greeting asn1.Utf8String
	answer   asn1.Integer
	// you can tag your struct fields with supported options.
	tipe asn1.ObjectIdentifier @[context_specific: 1; explicit; inner: 6]
}

fn (ex Example) tag() asn1.Tag {
	return asn1.default_sequence_tag
}

// you can build your payload manually or use `asn1.make_payload`, but with aware,
// if your structure contains generic, its maybe not work (currently).
fn (ex Example) payload() ![]u8 {
	kd := asn1.KeyDefault(map[string]asn1.Element{})
	payload := asn1.make_payload[Example](ex, kd)!
	return payload
}

// You can write routines for deserialize Example structure. This is only examples way,
// but its possible to use other way with the help from this module, like use
// `Parser` codec.
fn Example.decode(bytes []u8) !Example {
	// just call raw .decode on bytes
	// by example, its should produce sequence type.
	elem := asn1.decode(bytes)!
	assert elem.tag().equal(asn1.default_sequence_tag) // should true

	// cast produced element into Sequence type and get the fields.
	seq := elem.into_object[asn1.Sequence]()!
	fields := seq.fields()

	// and then, turn every field into desired object based your schema.
	// first two field is not wrapped element, so just turn into real object
	greeting := fields[0].into_object[asn1.Utf8String]()!
	answer := fields[1].into_object[asn1.Integer]()!

	// the third field is context_specific wrapped element, just unwrap it with the
	// same options used to encode
	oid_tipe := fields[2].unwrap_with_options('context_specific:1;explicit; inner:6')!
	tipe := oid_tipe.into_object[asn1.ObjectIdentifier]()!

	// then build your Example structure
	ex := Example{
		greeting: greeting
		answer:   answer
		tipe:     tipe
	}
	return ex
}

fn main() {
	expected_output := [u8(0x30), 18, u8(12), 5, 72, 101, 108, 108, 111, u8(2), 1, 42, u8(0xA1),
		6, 6, 4, 43, 6, 1, 3]
	ex := Example{
		greeting: asn1.Utf8String.new('Hello')!
		answer:   asn1.Integer.from_int(42)
		tipe:     asn1.ObjectIdentifier.new('1.3.6.1.3')!
	}

	// serialize the Example object
	out := asn1.encode(ex)!
	assert out == expected_output

	// test with data
	example_obj := Example.decode(out)!
	dump(ex.greeting == example_obj.greeting)
	dump(ex.answer == example_obj.answer)
	dump(ex.tipe == example_obj.tipe)
}
