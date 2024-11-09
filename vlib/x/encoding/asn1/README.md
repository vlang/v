# asn1

`asn1` is a pure V Language module for X.690 Abstract Syntax Notation One (ASN.1)
Distinguished Encoding Rules (DER) encoding and decoding.

This module provides you with the ability to generate and parse ASN.1 encoded data.
More precisely, it provides you with the ability to generate and parse data encoded 
with ASN.1’s DER (Distinguished Encoding Rules) encoding. It does not support other than DER.

> [!CAUTION]
> This module is marked as an experimental, so, its a subject to change (even rapidly).
> Use it with caution, submit when found a bug and gives yours feedback and review.

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

```v
import x.encoding.asn1

fn main() {
	value := asn1.Integer.from_int(32)

	output := asn1.encode(value)!
	assert output == [u8(0x02), 0x01, 0x20]

	// you can encode with string options
	output2 := asn1.encode_with_options(value, 'context_specific:5;explicit;inner:2')!
	assert output2 == [u8(0xa5), 0x03, 0x02, 0x01, 0x20]

	// You can decode (deserializes) back
	el := asn1.decode_with_options([u8(0xa5), 0x03, 0x02, 0x01, 0x20], 'context_specific:5;explicit;inner:2')!
	// el is an Element, turn it into underlying object
	int_el := el.into_object[asn1.Integer]()!

	int_el_value := int_el.as_i64()!
	assert int_el_value == 32
}
```
See examples on [examples](examples/) directory for more completes examples.

## Documentation

See the [documentation](DOCS.md) for more detail information on 
how to use functionality in this module.

## License

This project is licensed under the MIT License (see LICENSE file)