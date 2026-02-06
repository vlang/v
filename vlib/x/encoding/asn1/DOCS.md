# `asn1` module documentation.

> [!CAUTION]
> This module is marked as an experimental, so, its a subject to change (even rapidly).
> Use it with caution, submit when found a bug and gives yours feedback and review.

## About `asn1` module

`asn1` is a `experimental` pure V module for handling Abstract Syntax Notation One (ASN.1) [[X.680]](http://www.itu.int/rec/T-REC-X.680/en) objects encoded in Distinguished Encoding Rules (DER) [[X.690]](https://www.itu.int/rec/T-REC-X.690/en) encoding scheme.

## About this document

This document is intended to serve as documentation (actually as a note) of 
internal details of this `asn1` module.
Its describes some parts of the module in the way is implemented, 
the lack and also issues or limitation we have found around it.

## Table of Contents

- [About `asn1` module](#about-asn1-module)
- [What is ASN.1](#what-is-asn1)
- [ASN.1 Encoding](#encoding-of-asn1)
- [ASN.1 Type System](#asn1-type-system)
  - [Tag handling](#asn1-tag)
  - [Limitation](#limitation-of-the-tag-in-this-module)
  - [Create ASN.1 tag](#create-asn1-tag)
  - [Serializing tag into bytes](#serializing-tag-into-bytes)
  - [Read ASN.1 Tag from bytes](#read-asn1-tag-from-bytes)
  - [ASN.1 Length handling](#asn1-length-handling)
  - [Serializing ASN.1 Length](#serializing-asn1-length)
- [ASN.1 Element](#asn1-element)
  - [Build custom element payload](#build-custom-element-payload)
  - [Serializing ASN.1 Element](#serializing-asn1-element)
  - [Deserializing ASN.1 DER bytes into Element](#deserializing-asn1-der-bytes-into-element)
- [Element Serialization with FieldOptions](#element-serialization-with-fieldoptions)
  - [Wrapping an Element through FieldOptions](#wrapping-an-element-through-fieldoptions)
  - [Handling optional with FieldOptions](#handling-optional-with-fieldoptions)
  - [Handling element with DEFAULT keyword](#handling-element-with-default-keyword)
- [Supported basic UNIVERSAL ASN.1 Type](#supported-basic-universal-asn1-type)
- [Constructor of Universal ASN.1 Type](#constructor-of-universal-asn1-type)
- [Support for non-universal class element](#support-for-non-universal-class-element)
- [Reference](#reference)

## What is ASN.1

From [Wikipedia](https://en.wikipedia.org/wiki/ASN.1) says, Abstract Syntax Notation One (ASN.1) 
is a standard interface description language for defining data structures that can be serialized 
and deserialized in a cross-platform way. It is broadly used in telecommunications and 
computer networking, and especially in cryptography.

## Encoding of ASN.1

Encoding of ASN.1 is a set of encoding rules that specify how to represent a data structure as 
a series of bytes. There are multiple rules available that describes way of serializing 
ASN.1 object. The standard ASN.1 encoding rules include:

- Basic Encoding Rules (BER)
- Distinguished Encoding Rules (DER)
- Canonical Encoding Rules (CER)
- Basic XML Encoding Rules (XER)
- many other encoding rules availables.

See [[X.690]](https://www.itu.int/rec/T-REC-X.690/en) for more information about ASN.1 encoding.

> **Note**
>
> This module only support the DER encoding

## ASN.1 Type System

Fundamentally, DERencoding  of ASN.1 is serialization of a Tag, Length and Value (TLV) triplets. 
Every ASN.1 object has a tag that represents what is type of the object. 
The Tag part specifies the type of the data structure being sent, the Length part specifies 
the number of bytes of content being transferred, and the Value part contains the content. 
Note that the Value part can be a triplet if it contains a constructed data type.

### ASN.1 Tag

Every ASN.1 type has a tag which acts as an identifier of some ASN.1 element. 
The tag is byte or series of bytes that describing class of the ASN.1 object, 
constructed (contains other object) or primitive and a non negative tag number.

ASN.1 Tag identifier was represented by this compact structure, ie,

```codeblocks
struct Tag {
mut:
	class       TagClass
	constructed bool
	number      int
}
```

Where `TagClass` represent class of ASN.1 type. There are four class of ASN.1 type represented in:

```codeblocks
enum TagClass {
	universal 			= 0x00
	application 		= 0x01
	context_specific 	= 0x02
	private 			= 0x03
}
```

### Limitation of the Tag in this module.

There are two form how the ASN.1 tag was represented, ie, short form tag for tag number below 31 
and long form tag (multi byte tag) for representing tag number bigger than 31.

This module support both of form, but the size (length) is limited to `max_tag_length` constant, 
currently set to 3 bytes length. This effectively limits the tag number supported by this 
module to be in 0..16.383 number ranges. See comment on `core.v` file for the background on this.

When your tag has a class of `universal` type, your tag nunber also be limited to be under 255, 
hopefully if your tag is universal type, just use universal type supported by this module.

### Create ASN.1 Tag

Most of the time, you don't need create tag structure manually, all basic universal type 
constructor set it for you internally, but for convenience, you can create a new tag, 
with the following constructor:

```codeblocks
fn Tag.new(c TagClass, constructed bool, number int) !Tag
```

where `c` is the ASN.1 class this object belong to, `constructed` boolean flag tells 
if this object constructed or primitive, and provided tag `number`.

### Serializing tag into bytes

You can serialize (encode) your tga with method defined in this module,

```codeblocks
fn (t Tag) encode(mut dst []u8) !
```

By default, `encode` would try to serialize tag in DER rule into destination buffer provided in 
`dst`, or returns error on fails.

### Read ASN.1 Tag from bytes

This module provides routine for reading tag from bytes. You can use

```codeblocks
fn Tag.from_bytes(bytes []u8) !(Tag, []u8)
```

It would create a tag from bytes, return a tag and remaining bytes (bytes after tag) on success,
or returns error on fails.

### ASN.1 Length handling

ASN.1 length indicates how many bytes you should read to get values or contents part. 
It always represents the total number of bytes in the object including all sub-objects 
but does not include the lengths of the identifier or of the length field itself.

The standard of X.690 ITU document defines two length types

- definite length, and
- indefinite length.

ASN.1 definite length comes in two form: short and long form. The short form fits in single byte 
for length between 0 and 127, and the others is long form in multi byte form.

> **Note**
> This module only support definite length but its only limited to DER encoding of length.
> Theoretically, definite length support for very big number for length value, ie, value between 
0 and 2^1008-1, but in this module, this limited to defined constant, `max_definite_length_count` 
set to 6 bytes currently, and `max_definite_length_value` set to builtin `max_int`.

ASN.1 length was represented in a simple type, ie,

```codeblocks
type Length = int
```

You can create a length from regular integer with

```codeblocks
fn Length.new(v int) !Length
```

and, you can read a length from bytes with

```codeblocks
fn Length.from_bytes(bytes []u8) !(Length, []u8)
```

It would return a length and remaining bytes on succes or error on fails.

### Serializing ASN.1 Length

This module provides method to serialize the length into destination buffer,

```codeblocks
fn (v Length) encode(mut dst []u8) !
```

## ASN.1 Element

At the core for support handling element in generic and concise way, a fundamental 
and abstracted way provided in this module is an `Element` interface, dedined as:

```codeblocks
interface Element {
    tag()     Tag
    payload() ![]u8
}
```

where the `tag` acts as an identifier of the element and `payload` tells the value's part 
of the element. The `payload` methods of the `Element` does not dictates on how your element 
generates payload. Its up to specific encoding rules or other constraints.

> **Note**
> Most of the functions or methods defined in this module was accept or return an `Element`.
> Most of them is implemented with DER encoding in mind, so, your custom element hopefylly 
> would be supported by this functions (methods) if your element correctly 
> implemented required constraints in this module.

### Build custom element payload
Its possible to build payload for complex structure, your own defined struct contains multiples 
field of elements with the help of function on this modules. Of course, you can build 
your payload manually, but this `asn1` module has provides helper routine to do that, in the form:
```codeblocks
fn make_payload[T](val T, kd KeyDefault) ![]u8 
```
> ***Note***
> - `T` is struct contains one or more fields that fullfills Element interface.
> - KeyDefault is map of `field.name` key with some element value 
> (only the field has DEFAULT keyword) to setup default value.

Its would produces only element's payload without tag or length bytes included. 
When your structures does not contains the fields that 
fullfills interfaces, it would produces and return empty bytes.

### Serializing ASN.1 Element

This modules provides several functions for serializing ASN.1 Element, in three forms, ie:

```codeblocks
fn encode(el Element) ![]u8
fn encode_with_options(el Element, opt string) ![]u8
fn encode_with_field_options(el Element, fo FieldOptions) ![]u8
```
All of three's functions produces bytes result on success or error on fails. 
The two latest form is serialization routines intended for serializing element 
with wrapping, optional or default semantic to existing element, 
gives you a extra flexibility to the serialization (deserialization) process.
For more information in detail, see [FieldOptions](#element-serialization-with-fieldoptions)

#### Example 
A PrintableString containing “hi” was serialized into 13 02 68 69.
```codeblocks
obj := asn1.PrintableString.new('hi')!
output := asn1.encode(obj)!

assert output == [u8(0x13), 0x02, 0x68, 0x69]
```
When your element is tagged type element, defined with `[5] IMPLICIT PrintableString`, you can pass
a string option into `encode`, ie:
```codeblocks
output := encode_with_options(obj, 'context_specific:5;implicit;inner:19')!
assert output == [u8(0x85), 0x02, 0x68, 0x69]
```
Or when its a explicit tagged element defined as `[5] EXPLICIT PrintableString`
```codeblocks
output := encode_with_options(obj, 'context_specific:5;explicit;inner:0x13')!
assert output == [u8(0xA5), 0x04, 0x13, 0x02, 0x68, 0x69]
```

### Deserializing ASN.1 DER bytes into Element
For deserialization purposes, this module provides functions with similar in serialization parts, 
ie, in the form:
```codeblocks
fn decode(src []u8) !Element
fn decode_with_options(bytes []u8, opt string) !Element
fn decode_with_field_options(bytes []u8, fo FieldOptions) !Element
```
Technically, the deserialization mechanism is reverse of serialization process. 
When you pass an options to decode routine, you should ensure its a same options 
used for serialization in `encode` part, or the decode would result in undefined behaviour
if its differs.

The `decode` function families, accepts DER serialized bytes, and an options 
(if its should be) and return some `Element`, or return error on fails. 
When you get result an `Element` from `decode` routine, you can get underlying object 
by calling `into_object` method on the element.
```codeblocks
fn (el Element) into_object[T]() !T
```
Examples:
```codeblocks
el := asn1.decode([u8(0x13), 0x02, 0x68, 0x69])!
ps := el.into_object[asn1.PrintableString]()!
```
So, its also happens to pass an options string when this bytes comes from 
serialized tagged type element,
```codeblocks
bytes := [u8(0xA5), 0x04, 0x13, 0x02, 0x68, 0x69]
obj := asn1.decode_with_options(bytes, 'context_specific:5;explicit;inner:0x13')!
```

## Element Serialization with FieldOptions
For supporting more complex scenarios, inspired by the same options used in go version of 
`asn1` module, this module comes with support configures serialization (deserialization) process 
through configuration options stored in `FieldOptions` structure.

Consider some Certificate structure represents more complex ASN.1 schemas 
from [RFC 5280](https://datatracker.ietf.org/doc/html/rfc5280#section-4.1.1.2),
```asn1
 Certificate  ::=  SEQUENCE  {
        tbsCertificate       TBSCertificate,
        signatureAlgorithm   AlgorithmIdentifier,
        signatureValue       BIT STRING  
	}

   TBSCertificate  ::=  SEQUENCE  {
        version         [0]  EXPLICIT Version DEFAULT v1,
        serialNumber         CertificateSerialNumber,
        signature            AlgorithmIdentifier,
        issuer               Name,
        validity             Validity,
        subject              Name,
        subjectPublicKeyInfo SubjectPublicKeyInfo,
        issuerUniqueID  [1]  IMPLICIT UniqueIdentifier OPTIONAL
   }
```
This schema required support for other machinery in the form of tagged element 
with wrapping semantic, OPTIONAL keyword handling, and DEFAULT keyword handling, 
Its supported through the `FieldOptions` structures defined as:
```codeblocks
struct FieldOptions {
mut:
	// For wrapping purposes
	cls           string
	tagnum        int = -1
	mode          string
	inner         string

	// for OPTIONAL handling
	optional      bool
	present       bool
	
	// FOR DEFAULT handling
	has_default   bool
	default_value ?Element
}
```
The main purpose of this options structures is used for:
- handling of wrapping some element, turn some element into another element.
- handling of OPTIONAL element.
- handling of element with DEFAULT keywoard.

### Wrapping an Element through FieldOptions
There are two constructor for construct a `FieldOptions`, ie 
```codeblocks
fn FieldOptions.from_string(s string) !FieldOptions
fn FieldOptions.from_attrs(attrs []string) !FieldOptions
```
The first function allowing you pass a string as an options, likes an examples above.
Examples:
```codeblocks
fo := FieldOptions.from_string('context_specific:5;explicit;inner:0x13')!
fo := FieldOptions.from_string('context_specific:5;explicit;inner:0x13;optional')!
```

The second form, is gives more controllable options, and its allowing tag your field of struct
with the supported options,
Examples :
```codeblocks
struct PersonnelRecord {
mut:
	name     asn1.OctetString @[context_specific: 0; implicit; inner: 4]
	location asn1.Integer     @[context_specific: 1; implicit; inner: 2]
	age      asn1.Integer     @[context_specific: 2; implicit; inner: 2]
}

attrs := ['context_specific: 0', 'implicit', 'inner: 4']
fo := FieldOptions.from_attrs(attrs)!
// and then you can pass the options to serialization phase
out := asn1.encode_with_field_options(p.name, fo)!
```

### Handling optional with FieldOptions
The field `optional` and `present` of the `FieldOptions` was used for handling element 
with OPTINAL keyword within element definition.
The mean of the flags:
- when `optional` bit was set into `true`, thats mean, the element treated as element 
with OPTIONAL semantic.
- when `present` bit was set into `true`, this optional element mean was present in 
the encoding data, by default optional was not included in the encoding phase (not present)

### Handling element with DEFAULT keyword
Element with DEFAULT keyword, by DER encoding rule, when the element is equal with default value 
provided, its not present in the serialized bytes. For this purposes, before serialization, 
you should call:
```codeblocks
fn (el Element) set_default_value(mut fo FieldOptions, value Element) !
``` 
to setup default value within the options for the current element, or would be error 
if `has_default` flag is set but default value is not availables.

See [field_options.v](src/field_options.v) for more detail on this options.

## Supported basic UNIVERSAL ASN.1 Type

Basic ASN.1 type was a ASN.1 object which has universal class. 
It's currently supports following basic ASN1 type:

- [x] Boolean
- [x] BitString
- [x] Integer (through i32, i64, and `big.Integer`)
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

## Constructor of Universal ASN.1 Type
Most Universal class of ASN.1 type supported in this modules comes with builtin constructor. 
You should use this constructor when you hope an universal type. 
Its comes with common signature of the constructor (not at all, but most of them), 
see module doc for detail.
```codeblocks
fn T.new(value) !T
fn T.from_string(string) !T // for string-based type
fn T.from_int(int) !T // for integer-based
fn T.from_bigint(bigint) !T // for integer with big.Integer
```


## Support for non-universal class Element
When your element is non-universal class, this module has a limited support for 
this type of element. Its represented in several structures, defined as :
```codeblocks
pub struct RawElement {
mut:
	// The tag is the (outer) tag of the TLV, if this a wrpper.
	tag Tag
	// `content` is the value of a TLV. Its depends on the context.
	content []u8
	// optional fields
	inner_tag     ?Tag
	mode          ?TaggedMode
	default_value ?Element
}

pub struct ContextElement {
	RawElement
}

pub struct ApplicationElement {
	RawElement
}

pub struct PrivateELement {
	RawElement
}
```
Intended usage for this non-universal class is for wrapping semantic, 
instead create your own non-universal manually. For examples, if you have 
some element (maybe non-universal), you want create non-universal element from this element,
you can call (for creating private type element) routine :
```codeblocks
fn PrivateELement.from_element(inner Element, tagnum int, mode TaggedMode) !PrivateELement
```
Its currently support nested wrapping, but the unwrapping process should do by your self.

> **Warning**
> There are some limitation on this wrapping,
> - You can't wrap into universal class
> - You can't wrap an Optional Element.

See [other_element.v](src/other_element.v) on the repo for more details on this.

[[Return to contents]](#table-of-contents)

## Reference

1. [ASN.1](https://en.wikipedia.org/wiki/ASN.1)
2. [A Warm Welcome to ASN.1 and DER](https://letsencrypt.org/docs/a-warm-welcome-to-asn1-and-der/)
3. [A Layman's Guide to a Subset of ASN.1, BER, and DER](https://luca.ntop.org/Teaching/Appunti/asn1.html)
4. [X.690 PDF](https://www.itu.int/ITU-T/studygroups/com17/languages/X.690-0207.pdf)
5. [X.680 PDF](https://www.itu.int/ITU-T/studygroups/com17/languages/X.680-0207.pdf)
