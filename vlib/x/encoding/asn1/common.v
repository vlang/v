// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

// common parsing routines
//
fn parse_element(tag Tag, content []u8) !Element {
	match tag.class {
		.universal {
			return parse_universal(tag, content)!
		}
		.context_specific {
			return parse_context_specific(tag, content)!
		}
		.application {
			return parse_application(tag, content)!
		}
		.private {
			return parse_private(tag, content)!
		}
	}
}

fn parse_universal(tag Tag, content []u8) !Element {
	if tag.class != .universal {
		return error('Non universal class')
	}
	if tag.constructed {
		return parse_universal_constructed(tag, content)!
	}
	return parse_universal_primitive(tag, content)!
}

fn parse_universal_primitive(tag Tag, content []u8) !Element {
	if tag.constructed {
		return error('parse on constructed type')
	}
	match tag.tag_number() {
		int(TagType.boolean) {
			return Boolean.from_bytes(content)!
		}
		int(TagType.null) {
			return Null.from_bytes(content)!
		}
		int(TagType.oid) {
			return ObjectIdentifier.from_bytes(content)!
		}
		int(TagType.integer) {
			return Integer.from_bytes(content)!
		}
		int(TagType.enumerated) {
			return Enumerated.from_bytes(content)!
		}
		int(TagType.bitstring) {
			return BitString.from_bytes(content)!
		}
		int(TagType.ia5string) {
			return IA5String.from_bytes(content)!
		}
		int(TagType.utf8string) {
			return Utf8String.from_bytes(content)!
		}
		int(TagType.numericstring) {
			return NumericString.from_bytes(content)!
		}
		int(TagType.printablestring) {
			return PrintableString.from_bytes(content)!
		}
		int(TagType.generalstring) {
			return GeneralString.from_bytes(content)!
		}
		int(TagType.octetstring) {
			return OctetString.from_bytes(content)!
		}
		int(TagType.visiblestring) {
			return VisibleString.from_bytes(content)!
		}
		int(TagType.utctime) {
			return UtcTime.from_bytes(content)!
		}
		int(TagType.generalizedtime) {
			return GeneralizedTime.from_bytes(content)!
		}
		else {
			// return the raw element
			return RawElement.new(tag, content)!
		}
	}
}

fn parse_universal_constructed(tag Tag, content []u8) !Element {
	if !tag.constructed {
		return error('parse on non-constructed type')
	}
	match tag.tag_number() {
		int(TagType.sequence) {
			// todo: handle SequenceOf
			return Sequence.from_bytes(content)!
		}
		int(TagType.set) {
			return Set.from_bytes(content)!
		}
		else {
			return RawElement.new(tag, content)!
		}
	}
}

fn parse_private(tag Tag, content []u8) !PrivateELement {
	return PrivateELement.new(tag, content)!
}

fn parse_application(tag Tag, content []u8) !ApplicationElement {
	return ApplicationElement.new(tag, content)!
}

// parse_context_specific_with_mode parses tag and content as ContextElement when mode is availables
fn parse_context_specific_with_mode(tag Tag, content []u8, inner_tag Tag, mode TaggedMode) !ContextElement {
	if tag.tag_class() != .context_specific {
		return error('parse on non-context-specific class')
	}
	if mode == .implicit {
		inner := parse_element(inner_tag, content)!
		ctx := ContextElement.from_element(inner, tag.number, mode)!
		return ctx
	}
	// explicit
	// read an inner tag from content
	mut p := Parser.new(content)
	intag := p.peek_tag()!
	if !intag.equal(inner_tag) {
		return error('Get unexpected inner tag')
	}
	inner_el := p.read_tlv()!
	// should finish
	p.finish()!

	ctx := ContextElement.from_element(inner_el, tag.number, .explicit)!

	return ctx
}

// parse_context_specific parses tag and content as ContextElement.
// The info of fields of ContextElement, ie, inner_tag and mode, is not availables here
// You should provides this later with the correct value.
fn parse_context_specific(tag Tag, content []u8) !ContextElement {
	// mode and inner_tag is not set here without additional information,
	// So its still none here, and you should set it with correct value
	ctx := ContextElement.new(tag, content)!

	return ctx
}
