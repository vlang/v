// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

// This file contains structures and routines for (limited) support for
// other class (type) of ASN.1 Element.
//

// ASN.1 Raw Element.
pub struct RawElement {
mut:
	// The tag is the (outer) tag of the TLV, if this a wrpper.
	tag Tag
	// `content` is the value of a TLV. Its depends on the context.
	content []u8
	// Optional fields
	inner_tag     ?Tag
	mode          ?TaggedMode
	default_value ?Element
}

// new creates a RawElement from tag and content. If your element is universal class,
// use universal type constructor instead, provided in this module.
pub fn RawElement.new(tag Tag, content []u8) !RawElement {
	if tag.number < 0 || tag.number > max_tag_number {
		return error('Unallowed tagnum was provided')
	}
	// universal class with constructed form only valid for sequence(of) and set(of) type.
	if tag.class == .universal {
		if tag.number > max_universal_tagnumber {
			return error('Tag number for universal class was exceed limit')
		}
		if tag.constructed {
			if tag.number != int(TagType.sequence) && tag.number != int(TagType.set) {
				return error('unexpected_tag_value required sequence or set number')
			}
		}
		if tag.number == int(TagType.sequence) || tag.number == int(TagType.set) {
			if !tag.constructed {
				return error('unexpected_tag_value sequence or set number should in constructed tag')
			}
		}
	}
	// otherwise, treats as a RawElement
	return RawElement{
		tag:     tag
		content: content
	}
}

// from_element creates a RawElement from another element (with wrapping semantic).
pub fn RawElement.from_element(el Element, cls TagClass, tagnum int, mode TaggedMode) !RawElement {
	// wrapping into .universal is not allowed.
	if cls == .universal {
		return error('wrap with universal class is unallowed')
	}
	inner_form := el.tag().constructed
	constructed := if mode == .explicit { true } else { inner_form }
	content := if mode == .explicit { encode_with_rule(el, .der)! } else { el.payload()! }

	outer_tag := Tag.new(cls, constructed, tagnum)!
	raw := RawElement{
		tag:       outer_tag
		content:   content
		inner_tag: el.tag()
		mode:      mode
	}

	return raw
}

// tag returns the tag of the RawElement, ie, outer tag when its a wrapper.
pub fn (r RawElement) tag() Tag {
	return r.tag
}

// payload returns the payload of the RawElement.
pub fn (r RawElement) payload() ![]u8 {
	return r.content
}

// set_mode sets the RawElement tagged mode, in explicit or implicit mode. If the mode has been set,
// it would drop into error until you forces it by setting force flag into true value, ie, replaces the old one.
pub fn (mut r RawElement) set_mode(mode TaggedMode, force bool) ! {
	if r.tag.class == .universal {
		return error('No need it on universal class')
	}
	if r.mode != none {
		if !force {
			return error('unallowed_operation r.mode != none')
		}
		r.mode = mode
		return
	}

	r.mode = mode
}

// set_inner_tag sets the inner tag of the RawElement into inner_tag value. If it has been already set,
// it would be an error until you setting force flag into true value to replace the old one.
pub fn (mut r RawElement) set_inner_tag(inner_tag Tag, force bool) ! {
	// not needed in universal class
	if r.tag.class == .universal {
		return error('No need it on universal class')
	}
	mode := r.mode or { return error('You dont set any mode') }
	// when its in explicit mode, compares the provided tag with tag from the inner element.
	if mode == .explicit {
		if !r.tag.constructed {
			return error('unmeet_requirement, explicit should be constructed')
		}
		// check inner_tag
		itt, _ := Tag.decode(r.content)!
		if !itt.equal(inner_tag) {
			return error('unmeet_requirement, unequal supplied tag')
		}
	}
	if r.inner_tag != none {
		if !force {
			return error('.unallowed_operation, r.inner_tag != none')
		}
		r.inner_tag = inner_tag
		return
	}

	r.inner_tag = inner_tag
}

// force_set_default_value forces set default value of this RawElement into value.
fn (mut r RawElement) set_default_value(value Element, force bool) ! {
	// default value of this element should have an equal tag.
	if !value.tag().equal(r.tag) {
		return error('You provides unequal tag for default value')
	}
	if r.default_value != none {
		if !force {
			return error('The RawElement already default_value being set')
		}
		r.default_value = value
		return
	}
	r.default_value = value
}

// inner_tag returns the inner tag of the RawElement if it exists, or error on fails.
pub fn (r RawElement) inner_tag() ?Tag {
	return r.inner_tag
}

// inner_element returns the inner element of the RawElement if its exists.
pub fn (r RawElement) inner_element() !Element {
	if r.tag.class == .universal {
		return error('inner element from universal class is not availables')
	}
	inner_tag := r.inner_tag or { return err }
	mode := r.mode or { return error('You dont set any mode') }
	if mode == .explicit {
		if !r.tag.constructed {
			return error('tag should be constructed when in explicit')
		}
	}
	// in implicit, r.content is inner element content with inner tag
	if mode == .implicit {
		elem := parse_element(inner_tag, r.content)!
		return elem
	}
	// otherwise, treats it in explicit mode.
	// read an inner tag from r.content
	mut p := Parser.new(r.content)
	tag := p.peek_tag()!
	if !tag.equal(inner_tag) {
		return error('gets unequal inner_tag')
	}
	el := p.read_tlv()!
	// should finish
	p.finish()!
	return el
}

fn (r RawElement) check_inner_tag() ! {
	if r.tag.class == .universal {
		return error('Universal class does not have inner tag')
	}
	mode := r.mode or { return error('You dont set any mode') }
	if mode != .explicit {
		return
	}
	// read an inner tag from content
	tag, _ := Tag.decode_with_rule(r.content, 0, .der)!
	inner_tag := r.inner_tag or { return error('You dont set an inner_tag') }
	if !tag.equal(inner_tag) {
		return error('Get unexpected inner tag from bytes')
	}
}

// ContextSpecific tagged type element.
pub struct ContextElement {
	RawElement
}

// new creates a raw context specific element. Use `ContextElement.from_element` instead
// if your context specific element is wrapper of another element.
pub fn ContextElement.new(tag Tag, content []u8) !ContextElement {
	if tag.class != .context_specific {
		return error('Your tag is not .context_specific')
	}
	raw := RawElement.new(tag, content)!
	return ContextElement{raw}
}

// from_element creates a new tagged type of ContextElement from some element in inner.
pub fn ContextElement.from_element(inner Element, tagnum int, mode TaggedMode) !ContextElement {
	raw := RawElement.from_element(inner, .context_specific, tagnum, mode)!

	ctx := ContextElement{raw}
	return ctx
}

// tag returns the tag of context specific element.
pub fn (ctx ContextElement) tag() Tag {
	return ctx.RawElement.tag
}

// payload returns the payload of the context specific element.
pub fn (ctx ContextElement) payload() ![]u8 {
	return ctx.RawElement.content
}

// explicit_context creates a new ContextElement with explicit mode.
pub fn ContextElement.explicit_context(inner Element, tagnum int) !ContextElement {
	return ContextElement.from_element(inner, tagnum, .explicit)!
}

// implicit_context creates new ContextElement with implicit mode.
pub fn ContextElement.implicit_context(inner Element, tagnum int) !ContextElement {
	return ContextElement.from_element(inner, tagnum, .implicit)!
}

fn ContextElement.decode_raw(bytes []u8) !(ContextElement, int) {
	tag, length_pos := Tag.decode_with_rule(bytes, 0, .der)!
	if tag.class != .context_specific {
		return error('tag required to be context_specific')
	}
	length, content_pos := Length.decode_with_rule(bytes, length_pos, .der)!
	content := if length == 0 {
		[]u8{}
	} else {
		if content_pos >= bytes.len || content_pos + length > bytes.len {
			return error('ContextElement: truncated payload bytes')
		}
		unsafe { bytes[content_pos..content_pos + length] }
	}
	next := content_pos + length
	// Raw ContextElement, you should provide mode and inner tag.
	ctx := ContextElement{
		tag:     tag
		content: content
	}
	return ctx, next
}

fn ContextElement.decode_with_options(bytes []u8, opt string) !(ContextElement, int) {
	if opt.len == 0 {
		return ContextElement.decode_raw(bytes)!
	}
	fo := FieldOptions.from_string(opt)!
	// get mode and inner tag
	if !valid_mode_value(fo.mode) {
		return error('Get unexpected mode option for ContextElement')
	}
	mode := TaggedMode.from_string(fo.mode)!
	inner_tag := fo.inner_tag()!

	// outer tag from bytes
	tag, length_pos := Tag.decode_with_rule(bytes, 0, .der)!
	if tag.class != .context_specific {
		return error('Get non ContextSpecific tag')
	}

	// if mode is explicit without constructed form, its would return on error.
	if mode == .explicit {
		if !tag.constructed {
			return error('explicit need constructed form')
		}
	}
	length, content_pos := Length.decode_with_rule(bytes, length_pos, .der)!
	content := if length == 0 {
		[]u8{}
	} else {
		if content_pos >= bytes.len || content_pos + length > bytes.len {
			return error('ContextElement: truncated payload bytes')
		}
		unsafe { bytes[content_pos..content_pos + length] }
	}
	next := content_pos + length

	if mode == .implicit {
		ctx := ContextElement{
			tag:       tag
			content:   content
			inner_tag: inner_tag
			mode:      .implicit
		}
		return ctx, next
	}
	// explicit one, build ContextElement and performs checks for inner_tag validity.
	ctx := ContextElement{
		tag:       tag
		content:   content
		inner_tag: inner_tag
		mode:      .explicit
	}
	ctx.check_inner_tag()!

	return ctx, next
}

// Limited support for APPLICATION CLASS Element.
pub struct ApplicationElement {
	RawElement
}

// new creates a new application class element.
pub fn ApplicationElement.new(tag Tag, content []u8) !ApplicationElement {
	if tag.class != .application {
		return error('Your tag is not .application')
	}
	raw := RawElement.new(tag, content)!
	return ApplicationElement{raw}
}

// from_element creates application class element from some element.
pub fn ApplicationElement.from_element(inner Element, tagnum int, mode TaggedMode) !ApplicationElement {
	raw := RawElement.from_element(inner, .application, tagnum, mode)!
	app := ApplicationElement{raw}
	return app
}

// tag returns the tag of the application class element.
pub fn (app ApplicationElement) tag() Tag {
	return app.RawElement.tag
}

// payload returns the payload of application class element.
pub fn (app ApplicationElement) payload() ![]u8 {
	return app.RawElement.content
}

// Limited support for PRIVATE CLASS Element.
pub struct PrivateELement {
	RawElement
}

// new creates a raw private class element.
pub fn PrivateELement.new(tag Tag, content []u8) !PrivateELement {
	if tag.class != .private {
		return error('Your tag is not private')
	}
	raw := RawElement.new(tag, content)!

	return PrivateELement{raw}
}

// from_element creates a new private class element from another element.
pub fn PrivateELement.from_element(inner Element, tagnum int, mode TaggedMode) !PrivateELement {
	raw := RawElement.from_element(inner, .private, tagnum, mode)!
	app := PrivateELement{raw}

	return app
}

// tag returns the tag of the private element.
pub fn (prv PrivateELement) tag() Tag {
	return prv.RawElement.tag
}

// payload returns the payload of the private element.
pub fn (prv PrivateELement) payload() ![]u8 {
	return prv.RawElement.content
}
