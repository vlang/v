# asn1

`asn1` is a pure V Language module for X.690 Abstract Syntax Notation One (ASN.1)
Distinguished Encoding Rules (DER) encoding and decoding.

This module provides you with the ability to generate and parse ASN.1 encoded data.
More precisely, it provides you with the ability to generate and parse data encoded 
with ASN.1’s DER (Distinguished Encoding Rules) encoding. It does not support other than DER.

> [!CAUTION]
> This module is marked as an experimental, so its subject to change (possibly rapidly).
> Use with caution, submit bugs when found, and please gives feedback.

## Supported ASN.1 Type

Currently supports the following basic ASN1 types:

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

- Supports most basic ASN.1 tag types.
- Supports single and multi-byte (high form) tag formats for tag number > 31.
- Serializing and deserializing of ASN.1 objcet to bytes and vice versa.

## Code Examples

Here are some simple usage examples.

```v
import x.encoding.asn1

fn main() {
	value := asn1.Integer.from_int(32)

	output := asn1.encode(value)!
	assert output == [u8(0x02), 0x01, 0x20]

	// you can encode (serialize) with string options
	output2 := asn1.encode_with_options(value, 'context_specific:5;explicit;inner:2')!
	assert output2 == [u8(0xa5), 0x03, 0x02, 0x01, 0x20]

	// You can decode (deserialize) back the bytes into Element.
	el := asn1.decode_with_options([u8(0xa5), 0x03, 0x02, 0x01, 0x20], 'context_specific:5;explicit;inner:2')!

	// el is an Element, turn it into underlying object
	int_el := el.into_object[asn1.Integer]()!

	int_el_value := int_el.as_i64()!
	assert int_el_value == 32
}
```
See more complete examples in the [examples](https://github.com/vlang/v/tree/master/vlib/x/encoding/asn1/examples/) directory.

## Documentation

See the [documentation](https://github.com/vlang/v/blob/master/vlib/x/encoding/asn1/DOCS.md) for more detailed information on 
how to use functionality in this module.

## License

This project is licensed under the MIT License (see LICENSE file)