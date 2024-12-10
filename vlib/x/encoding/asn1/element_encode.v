// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

// Handling functionality of Element's serialization.
//

// `encode` serializes element into bytes array. By default, its encode in .der rule with empty options.
// See  `encode_with_options` if you want pass an option string. See `field.v` for more option in detail.
//
// Examples:
//
// ```v
// import x.encoding.asn1
//
// obj := asn1.Utf8String.new('hi')!
// out := asn1.encode(obj)!
// assert out == [u8(0x0C), 0x02, 0x68, 0x69]
// ```
pub fn encode(el Element) ![]u8 {
	// without options, we call `.encode_with_rule` directly on element.
	return encode_with_rule(el, .der)!
}

// `encode_with_options` serializes element into bytes array with options string passed to drive the result.
//
// Examples:
//
// `Utf8String` defined as `[5] IMPLICIT UTF8String` was encoded into `85 02 68 69`.
// `Utf8String` defined as `[5] EXPLICIT UTF8String` was encoded into `A5 04 0C 02 68 69`.
//
// ```v
// obj := asn1.Utf8String.new('hi')!
// implicit_out := asn1.encode_with_options(obj, 'context_specific:5;implicit;inner:12')!
// assert implicit_out == [u8(0x85), 0x02, 0x68, 0x69]
//
// explicit_out := asn1.encode_with_options(obj, 'context_specific:5;explicit;inner:0x0c')!
// assert explicit_out == [u8(0xA5), 0x04, 0x0C, 0x02, 0x68, 0x69]
// ```
pub fn encode_with_options(el Element, opt string) ![]u8 {
	// treated as without option when empty
	if opt.len == 0 {
		return encode_with_rule(el, .der)!
	}
	fo := FieldOptions.from_string(opt)!
	return encode_with_field_options(el, fo)!
}

// `encode_with_field_options` serializes this element into bytes array with options defined in fo.
pub fn encode_with_field_options(el Element, fo FieldOptions) ![]u8 {
	// validates options again this element.
	el.validate_options(fo)!

	// check for default_value for this element
	// if we have it matching with current element,
	// by default, in .der mode, it should not be serialized.
	if fo.has_default {
		def_element := fo.default_value or { return error('bad default_value') }
		// If this element is equal with default_value, by default its should not be serialized.
		if el.equal(def_element) {
			return []u8{}
		}
	}

	// apply field options, turns this element
	// into optional, wrapped element or original one.
	new_el := el.apply_field_options(fo)!

	// if new_el is Optional, encode with optional behaviour
	if new_el is Optional {
		return new_el.encode()!
	}
	// otherwise, just serializing it
	return encode_with_rule(new_el, .der)!
}

// Helper for wrapping element
//
//
// into_optional turns this element into Optional with present bit.
// When you set with_present into true, its makes this optional was present.
fn (el Element) into_optional(with_present bool) !Element {
	if el is Optional {
		return error('already optional element')
	}
	return Optional.new(el, with_present)!
}

// apply_field_options applies rules in field options into current element
// and turns this into another element.
// by default, optional attribute is more higher precedence over wrapper attribut, ie,
// take the wrap step and then turn into optional (if true)
fn (el Element) apply_field_options(fo FieldOptions) !Element {
	el.validate_options(fo)!
	// if there a wrapper
	if fo.cls != '' {
		wrapped_el := el.wrap_with_options(fo)!
		if fo.optional {
			return wrapped_el.into_optional(fo.present)!
		}
		// not-optional, just return wrapped element
		return wrapped_el
	}
	// no-wrapper, check for optional
	if fo.optional {
		return el.into_optional(fo.present)!
	}
	// otherwise, its no-wrapper and non-optional
	return el
}

// set_default_value installs default value within FieldOptions for the element
pub fn (el Element) set_default_value(mut fo FieldOptions, value Element) ! {
	// the default tag should match with the current tag
	if !el.tag().equal(value.tag()) {
		return error('unmatching tag of default value')
	}
	fo.install_default(value, false)!
	el.validate_default(fo)!
}

// wrap_with_rule wraps universal element into another class.
// we prohibit dan defines some rules when its happen and  returns an error instead
// 1. wrapping into .universal class is not allowed
// 2. wrapping with the same class is not allowed too
// 3. wrapping non-universal class element is not allowed (maybe removed on futures.)
// Notes :
// Three additional information about tagging:
//		CHOICEs are always explicitly tagged even if implicit tagging is in effect.
//		EXPLICIT TAGs are always constructed, they encapsulate the TLV they prefix.
//		An IMPLICIT TAG 'inherits' the constructed bit of the TLV whose 'T' is overwritten,
//		examples:
//		a) '[5] IMPLICIT INTEGER' has tag 0x85 (overwriting 0x02 = INTEGER)
//		b) '[5] IMPLICIT SEQUENCE' has tag 0xA5 (overwriting 0x30 = SEQUENCE, CONSTRUCTED)
fn (el Element) wrap_with_options(fo FieldOptions) !Element {
	// validates options.
	el.validate_options(fo)!

	mode := TaggedMode.from_string(fo.mode)!
	cls := TagClass.from_string(fo.cls)!

	return wrap(el, cls, fo.tagnum, mode)!
}

// wrao performs wrapping to element and turns this element into another one.
fn wrap(el Element, cls TagClass, number int, mode TaggedMode) !Element {
	if el is Optional {
		return error('Optional cant be wrapped')
	}
	if cls == .universal {
		return error('you cant wrap into universal')
	}
	match cls {
		.context_specific {
			return ContextElement.from_element(el, number, mode)!
		}
		.application {
			return ApplicationElement.from_element(el, number, mode)!
		}
		.private {
			return PrivateELement.from_element(el, number, mode)!
		}
		else {
			return error('Wraps to the wrong class')
		}
	}
}

// encode_with_rule encodes element into bytes array with rule.
fn encode_with_rule(el Element, rule EncodingRule) ![]u8 {
	if rule != .der && rule != .ber {
		return error('Element: unsupported rule')
	}
	mut dst := []u8{}

	// when this element is Optional without presence flag, by default would
	// serialize this element into empty bytes otherwise, would serialize underlying element.
	if el is Optional {
		return el.encode()!
	}
	// otherwise, just serializes as normal
	el.tag().encode_with_rule(mut dst, rule)!
	// calculates the length of element,  and serialize this length
	payload := el.payload()!
	length := Length.new(payload.len)!
	length.encode_with_rule(mut dst, rule)!
	// append the element payload to destination
	dst << payload

	return dst
}
