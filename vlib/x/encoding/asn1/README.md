# asn1

`asn1` is a pure V Language module for X.690 Abstract Syntax Notation One (ASN.1)
Distinguished Encoding Rules (DER) encoding and decoding.

This module provides you with the ability to generate and parse ASN.1 encoded data.
More precisely, it provides you with the ability to generate and parse data encoded with ASN.1’s DER (Distinguished Encoding Rules) encoding.
It does not support other than DER.

## Status

> **Warning**
>
> This module is under development, its changed rapidly, and even
> its functionality mostly and hardly tested, i'm sure there are buggy code uncovered recently,
> so feel free to report an isdue or submit a bug report, or give a feedback.

## Supported ASN.1 Type

It's currently supports following basic ASN1 type:

- [x] Boolean
- [x] BitString
- [x] Integer (through int, i64, and `math.big.Integer`)
- [x] ObjectIdentifier
- [x] NumericString
- [x] Null
- [x] Enumerated
- [x] IA5String (ascii string)
- [x] OctetString
- [x] PrintableString
- [x] UTF8String
- [x] UTCTime
- [x] GeneralizedTime
- [x] VisibleString
- [x] Sequence,
- [x] SequenceOf
- [x] Set
- [x] SetOf

## **Features**

---

- Support mostly basic ASN.1 tag type, except for a few types.
- Supports single and multibyte (high form) tag format for tag number > 31
- Serializing and deserializing of ASN.1 objcet to bytes and vice versa.

## Code Examples

Here are some simple usage examples.

### Serializing ASN.1 Object.

Encode a sequence containing a UTF-8 string, an integer
and an explicitly tagged object identifier, conforming to the following
ASN.1 specification:

```asn.1
Example ::= SEQUENCE {
    greeting    UTF8String,
    answer      INTEGER,
    type        [1] EXPLICIT OBJECT IDENTIFIER
}
```

You can represent above structure with related structure in `v`, similar like:

```v
struct Example {
    greeting    asn1.Utf8String
    answer      asn1.Integer
    // you can tag your struct fields with supported options.
    tipe        asn1.ObjectIdentifier @[context_specific:1;explicit; inner:6]
}
```

For your structure to be treated as `asn1.Element`, you should provide
two methods on the object, ie,

- `fn (ex Example) tag() Tag` and
- `fn (ex Example) payload() ![]u8`

That's it, then you can use methods (functions) on this modules that operates on element.

```v
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

fn main() {
    expected_output := [u8(0x30), 18, u8(12), 5, 72, 101, 108, 108, 111, u8(2), 1, 42, u8(0xA1), 6, 6, 4, 43, 6, 1, 3]
    ex := Example {
        greeting : asn1.Utf8String.new('Hello')!
        answer : asn1.Integer.from_int(42)
        tipe : asn1.ObjectIdentifier.new('1.3.6.1.3')!
    }

    out := asn1.encode(ex)!
    assert out == expected_output
}
```

### Deserializing of DER encoded bytes into ASN.1 Object.

You can write routines for deserializing bytes into Example structure. This is only examples way,
but its possible to use other way with the help from this module, like the use of `Parser` codec.

```v
fn Example.decode(bytes []u8) !Example {
        // just call raw .decode on bytes, by example, its should produce sequence type.
	elem := asn1.decode(bytes)!
	assert elem.tag().equal(asn1.default_sequence_tag) // should true

	// cast produced element into Sequence type and get the fields.
	seq := elem.into_object[asn1.Sequence]()!
	fields := seq.fields()

	// and then, turn every field into desired object based on your schema.
	// first of the two fields is non-wrapped element, so just turn into real object.
	greeting := fields[0].into_object[asn1.Utf8String]()!
	answer := fields[1].into_object[asn1.Integer]()!

	// the third field is context_specific wrapped element, just unwrap it with the
	// same options used to encode, then turn into object.
	oid_tipe := fields[2].unwrap_with_options('context_specific:1;explicit; inner:6')!
	tipe := oid_tipe.into_object[asn1.ObjectIdentifier]()!

	// then build your Example struct
	ex := Example{
		greeting: greeting
		answer:   answer
		tipe:     tipe
	}
	return ex
}

// test with data
example_obj := Example.decode(out)!
dump(ex.greeting == example_obj.greeting)
dump(ex.answer == example_obj.answer)
dump(ex.tipe == example_obj.tipe)
```

## Documentation

See the [documentation](DOCS.md) for more detail information on how to use functionality in this module.

## License

This project is licensed under the MIT License (see LICENSE file)
