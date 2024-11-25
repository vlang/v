// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

// for bytes compare
import crypto.internal.subtle { constant_time_compare }

// This file contains structures and routines for handling ASN.1 Element.
// Its includes:
// 	- basic Element interface, for support ASN.1 element in more generic way
//	- arrays of Element in the form of ElementList
//	- basic raw element in the RawElement structure, for handling arbitrary class
//	  and other undefined (unsupported) generic ASN.1 Element in this module.
//	- others structures, likes an Choice, AnyDefinedBy, Optional for representing other
//	  element

// Element represents a generic ASN.1 Element.
// Most of the standard Universal class element defined on this module
// satisfies this interface. This interface was also expanded by methods
// defined on this interface.
pub interface Element {
	// tag tells the ASN.1 identity of this Element.
	tag() Tag
	// payload tells the payload (values) of this Element.
	// The element's size was calculated implicitly from payload.len
	// Its depends on the tag how interpretes this payload.
	payload() ![]u8
}

// from_object[T] transforms and creates a new Element from generic type (maybe universal type, like an OctetString).
// Its accepts generic element t that you should pass to this function. You should make sure if this element implements
// required methods of the Element, or an error would be returned.
// FIXME: its not tested.
// Examples:
// ```v
// oc := asn1.OctetString.new("xxx")!
// el := asn1.Element.from_object[OctetString](oc)!
// ```
// and then treats your OctetString as an Element
pub fn Element.from_object[T](t T) !Element {
	$if T !is Element {
		return error('Not holding element')
	}
	return t
}

// into_object[T] transforms and tries to cast element el into generic object T
// if the element not holding object T, it would return error.
// NOTE: Not tested.
// Examples:
// ```v
// oc := asn1.OctetString.new("xxx")!
// el := asn1.Element.from_object[OctetString](oc)!
// ```
// cast back the element into OctetString.
// ```v
// os := el.into_object[OctetString]()!
// ```
// and then treats os as an OctetString.
pub fn (el Element) into_object[T]() !T {
	if el is T {
		return *el
	}
	return error('Element el does not holding T}')
}

// length tells the payload length of this element.
pub fn (el Element) length() !int {
	return el.payload()!.len
}

// UTILITY HELPER FOR ELEMENT
//

// Helper for validates FieldOptions.
//
// validate_options validates FieldOptions is a valid option agains current element.
fn (el Element) validate_options(fo FieldOptions) ! {
	el.validate_wrapper(fo)!
	el.validate_optional(fo)!
	el.validate_default(fo)!
}

// validate_wrapper validates wrapper's part of fields options.
fn (el Element) validate_wrapper(fo FieldOptions) ! {
	// Validates wrapper part
	// Its discard all check when fo.cls is empty string, its marked as non-wrapped element.
	if fo.cls != '' {
		fo.check_wrapper()!
	}
}

// validate_default validates has_default part of field options
fn (el Element) validate_default(fo FieldOptions) ! {
	// Validates default part
	if fo.has_default {
		if fo.default_value == none {
			return error('has_default withoud default value')
		}
		def := fo.default_value or { return err }
		if !el.tag().equal(def.tag()) {
			return error('You provides different tag of default_value with  tag of current element')
		}
	}
}

fn (el Element) validate_optional(fo FieldOptions) ! {
	// Validates Optional part
	// If the element is already optional, you cant make it optional again by setting optional=true
	if fo.optional {
		if el is Optional {
			return error('You cant mark Optional element as nested Optional')
		}
	}
}

// KeyDefault is map of string (field.name) into Element for element with default semantic.
// its is to be used for building payload of complex structures like sequence.
// see `make_payload` below.
pub type KeyDefault = map[string]Element

// new_key_default creates empty KeyDefault maps.
pub fn new_key_default() KeyDefault {
	return KeyDefault(map[string]Element{})
}

// `make_payload` builds bytes of payload for some structures contains field of Elements.
// Consider this examples from RFC 5280 defines schema.
//  ```v
// Certificate  ::=  SEQUENCE  {
//      tbsCertificate       TBSCertificate,
//      signatureAlgorithm   AlgorithmIdentifier,
//      signatureValue       BIT STRING  }
// ```
// where your structure defined as:.
// ```v
// struct Certificate {
// 		tbs_certificate 	TBSCertificate
//		signature_algorithm	AlgorithmIdentifier
// 		signature_value		BitString
// }
// ```
//
// Usually you can do.
//
// ```v
// cert := Certificate.new()!
// payload := asn1.make_payload[Certificate](cert)!
// ```
//
// and then you can use the produced payload.
//
// BUG: there are some issues would you encounter when your T contains
// fields that have generic in it, its would produce unexpected result,
// see  detail bug on: https://github.com/vlang/v/issues/22721.
// So, just use this when your T.fields is not contains generic within it.
// UPDATED: This issue has been fixed in this PR [#22724](https://github.com/vlang/v/pull/22724)
// Thanks to @felipensp
pub fn make_payload[T](val T, kd KeyDefault) ![]u8 {
	mut out := []u8{}
	$for field in val.fields {
		// only serialiaze field that implement interfaces
		// Issue: `$if field.typ is Element` check would false when field.type contains generic structure.
		// even the `field.type` is fullfills the interfaces.
		// see detail bug on: https://github.com/vlang/v/issues/22721
		$if field.typ is Element {
			// if there attributes option
			if field.attrs.len != 0 {
				mut fo := FieldOptions.from_attrs(field.attrs)!
				// TODO: add keyDefault support
				if fo.has_default {
					// install default by getting default element from map
					key := field.name
					def_elem := kd[key] or { return error('missing defaul element') }
					fo.install_default(def_elem, false)!
				}
				current := encode_with_field_options(val.$(field.name), fo)!
				out << current
			} else {
				// without  option
				current := encode(val.$(field.name))!
				out << current
			}
		}
	}
	return out
}

// `encoded_len` calculates the size in bytes when the el element was serialized.
pub fn encoded_len(el Element) int {
	return el.encoded_len()
}

// `encoded_len` calculates the length of bytes when this element was serialized.
pub fn (el Element) encoded_len() int {
	return el.encoded_len_with_rule(.der)
}

// encoded_len_with_rule informs us the length of bytes when this element serialized into bytes.
// Different rule maybe produces different result.
fn (el Element) encoded_len_with_rule(rule EncodingRule) int {
	mut n := 0
	n += el.tag().tag_size()
	payload := el.payload() or { panic(err) }
	length := Length.new(payload.len) or { panic(err) }
	n += length.length_size_with_rule(rule) or { panic(err) }
	n += payload.len

	return n
}

// equal checks whether this two element equal and holds the same tag and content
pub fn (el Element) equal(other Element) bool {
	return (el.tag().equal(other.tag())) && (el.equal_payload(other))
}

fn (el Element) equal_payload(other Element) bool {
	// taken from crypto.internal.subtle
	x := el.payload() or { panic(err) }
	y := other.payload() or { panic(err) }

	return constant_time_compare(x, y) == 1
}

fn Element.decode(src []u8) !(Element, int) {
	return Element.decode_with_rule(src, 0, .der)!
}

// decode deserializes back bytes in src from offet `loc` into Element.
// Basically, its tries to parse a Universal class Element when it is possible.
fn Element.decode_with_rule(src []u8, loc int, rule EncodingRule) !(Element, int) {
	tag, length_pos := Tag.decode_with_rule(src, loc, rule)!
	length, content_pos := Length.decode_with_rule(src, length_pos, rule)!
	// get the bytes
	bytes := if length == 0 {
		[]u8{}
	} else {
		if content_pos >= src.len || content_pos + length > src.len {
			return error('Need more bytes to read content')
		}
		unsafe { src[content_pos..content_pos + length] }
	}
	next_pos := content_pos + length
	elem := parse_element(tag, bytes)!

	return elem, next_pos
}

// ElementList.
//
// ElementList is arrays of Element.
// Many places maybe required this wells, likes Sequence or Set fields.
pub type ElementList = []Element

// payload produces bytes array from arays of Element.
pub fn (els ElementList) payload() ![]u8 {
	return els.payload_with_rule(.der)!
}

fn (els ElementList) payload_with_rule(rule EncodingRule) ![]u8 {
	mut out := []u8{}
	for el in els {
		bytes := encode_with_rule(el, rule)!
		out << bytes
	}
	return out
}

// encoded_len tells the length of bytes when the ElementList was serialized.
pub fn (els ElementList) encoded_len() int {
	mut n := 0
	for el in els {
		n += el.encoded_len()
	}
	return n
}

// ElementList.from_bytes parses bytes in src as series of Element or return error on fails.
// Its does not support trailing data.
pub fn ElementList.from_bytes(src []u8) ![]Element {
	mut els := []Element{}
	if src.len == 0 {
		// empty list
		return els
	}
	mut i := int(0)
	for i < src.len {
		el, pos := Element.decode_with_rule(src, i, .der)!
		i = pos
		els << el
	}
	if i > src.len {
		return error('i > src.len')
	}
	if i < src.len {
		return error('The src contains unprocessed bytes')
	}
	return els
}
