// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

// default_set_tag is the default tag of ASN.1 SET (SET OF) type.
pub const default_set_tag = Tag{.universal, true, int(TagType.set)}

const default_set_size = 64
const max_set_size = 255

// ASN.1 UNIVERSAL SET and SET OF TYPE.
//
// SET and SET OF contains an unordered series of fields of one or more types.
// This differs from a SEQUENCE which contains an ordered list.
// in DER encoding, SET types elements are sorted into tag order, and,
// for SET OF types elements are sorted into ascending order of encoding.
pub struct Set {
mut:
	//	maximal size of this set fields
	size int = default_set_size
	// fields is the elements of the set
	fields []Element
}

// creates a new Set with default size.
pub fn Set.new() !Set {
	return Set.new_with_size(default_set_size)!
}

// from_list creates a new Set from arrays of element in els.
pub fn Set.from_list(els []Element) !Set {
	if els.len > max_set_size {
		return error('Sequence size exceed limit')
	}
	return Set{
		fields: els
	}
}

fn Set.new_with_size(size int) !Set {
	if size > max_set_size {
		return error('size is exceed limit')
	}
	if size < 0 {
		return error('Provides with correct size')
	}

	// if size is 0, use default_set_size
	limit := if size == 0 { default_set_size } else { size }
	return Set{
		size: limit
	}
}

// tag returns the tag of Set element.
pub fn (s Set) tag() Tag {
	return default_set_tag
}

// payload returns the payload of the Set element.
//
// Note: Required for DER encoding.
// The encodings of the component values of a set value shall appear in an order determined by their tags.
// The canonical order for tags is based on the outermost tag of each type and is defined as follows:
//   a) those elements or alternatives with universal class tags shall appear first, followed by those with
//      application class tags, followed by those with context-specific tags, followed by those with private class
//      tags;
//   b) within each class of tags, the elements or alternatives shall appear in ascending order of their tag
//      numbers.
pub fn (s Set) payload() ![]u8 {
	// workaround by working with the copy of Set, changing it to 'mut s Set` would encounter some issues
	// ie, with messages `asn1.Set` incorrectly implements method `payload` of interface `asn1.Element`:
	// expected `asn1.Element` which is immutable, not `mut &asn1.Set`
	mut set := s
	return set.payload_with_rule(.der)!
}

fn (mut s Set) payload_with_rule(rule EncodingRule) ![]u8 {
	// first, we sort the set.fields and then serializing it.
	s.sort_set_fields()

	mut out := []u8{}
	for field in s.fields {
		item := unsafe { field }
		obj := encode_with_rule(item, rule)!
		out << obj
	}
	return out
}

// fields return the arrays of element's content in the Set.fields.
pub fn (set Set) fields() []Element {
	return set.fields
}

fn Set.parse(mut p Parser) !Set {
	return error('not yet implemented')
}

fn Set.decode(bytes []u8) !(Set, int) {
	return Set.decode_with_rule(bytes, 0, .der)!
}

fn Set.decode_with_rule(bytes []u8, loc int, rule EncodingRule) !(Set, int) {
	tag, length_pos := Tag.decode_with_rule(bytes, loc, rule)!
	if !tag.equal(default_set_tag) {
		return error('Get unexpected non-set tag')
	}
	length, content_pos := Length.decode_with_rule(bytes, length_pos, rule)!
	payload := if length == 0 {
		[]u8{}
	} else {
		if content_pos + length > bytes.len {
			return error('Not enought bytes to read on')
		}
		unsafe { bytes[content_pos..content_pos + length] }
	}
	next := content_pos + length
	set := Set.from_bytes(payload)!
	return set, next
}

// bytes should seq.fields payload, not includes the tag
fn Set.from_bytes(bytes []u8) !Set {
	mut set := Set{}
	if bytes.len == 0 {
		return set
	}
	mut i := 0
	for i < bytes.len {
		el, pos := Element.decode_with_rule(bytes, i, .der)!
		i = pos
		set.add_element(el)!
	}

	if i > bytes.len {
		return error('i > bytes.len')
	}
	if i < bytes.len {
		return error('The src contains unprocessed bytes')
	}
	return set
}

fn (s Set) equal(o Set) bool {
	for i, item in s.fields {
		for j, obj in o.fields {
			if i == j {
				if !item.equal(obj) {
					return false
				}
			} else {
				continue
			}
		}
	}
	return true
}

// add_element adds an element el into Set.
// By default it allowing adds element with the same tag
pub fn (mut set Set) add_element(el Element) ! {
	set.relaxed_add_element(el, true)!
}

// add_element allows adding a new element into current Set fields.
// Its does not allow adding element when is already the same tag in the fields.
// but, some exception when you set relaxed to true
fn (mut set Set) relaxed_add_element(el Element, relaxed bool) ! {
	if set.fields.len == 0 {
		// just adds it then return
		set.fields << el
		return
	}

	// remove this checks
	// for item in set.fields {
	//	if item.equal(el) {
	//		return error('has already in the fields')
	//	}
	// }
	filtered_by_tag := set.fields.filter(it.tag().equal(el.tag()))
	if filtered_by_tag.len == 0 {
		set.fields << el
		return
	} else {
		if !relaxed {
			return error('You can not insert element without forcing')
		}
		set.fields << el
		return
	}
}

// `set_size` set internal maximal size of this set fields.
pub fn (mut set Set) set_size(size int) ! {
	if size < 0 {
		return error('provides the correct size')
	}
	if size > max_set_size {
		return error('Provided limit was exceed current one')
	}
	set.size = size
}

// sort_set_fields sort the Set fields in places with sort_with_compare support
fn (mut set Set) sort_set_fields() {
	// without &, its return an error: sort_with_compare callback function parameter
	// `x` with type `asn1.Element` should be `&asn1.Element`
	set.fields.sort_with_compare(fn (x &Element, y &Element) int {
		if x.tag().class != y.tag().class {
			s := if int(x.tag().class) < int(y.tag().class) { -1 } else { 1 }
			return s
		}
		if x.tag() == y.tag() {
			// compare by contents instead just return 0
			xx := encode(x) or { panic(err) }
			yy := encode(y) or { panic(err) }
			return xx.bytestr().compare(yy.bytestr())
		}
		q := if x.tag().number < y.tag().number { -1 } else { 1 }
		return q
	})
}

// ASN.1 SET OF
//
pub struct SetOf[T] {
mut:
	size   int = default_set_size
	fields []T
}

// new creates an empty SetOf type T.
pub fn SetOf.new[T]() !SetOf[T] {
	$if T !is Element {
		return error('Yur T is not Element')
	}
	return SetOf[T]{}
}

// from_list creates new SetOf type T from arrays of T.
pub fn SetOf.from_list[T](els []T) !SetOf[T] {
	$if T !is Element {
		return error('Yur T is not Element')
	}
	if els.len > max_set_size {
		return error('[]T length is exceed limit')
	}
	return SetOf[T]{
		fields: els
	}
}

// tag returns the tag of SetOf[T] element.
pub fn (so SetOf[T]) tag() Tag {
	return default_set_tag
}

// fields returns underlying arrays of T from the SetOf[T].
pub fn (so SetOf[T]) fields() []T {
	return so.fields
}

// payload returns the payload of SetOf[T] element.
pub fn (so SetOf[T]) payload() ![]u8 {
	mut sto := so
	return sto.payload_with_rule(.der)!
}

// issues: el cannot be used as interface object outside `unsafe` blocks
// as it might be stored on stack.
// Consider wrapping the `T` object in a `struct` declared as `@[heap]`
fn (mut so SetOf[T]) payload_with_rule(rule EncodingRule) ![]u8 {
	$if T !is Element {
		return error('T is not an element')
	}
	// sort the fields and serialize
	so.sort_setof_fields()

	mut out := []u8{}
	for el in so.fields {
		// we encounter issues above at here
		// we can not directly pass el to encode routine
		elem := unsafe { el }
		obj := encode_with_rule(elem, rule)!
		out << obj
	}
	return out
}

fn (mut so SetOf[T]) sort_setof_fields() {
	so.fields.sort_with_compare(fn [T](x &T, y &T) int {
		xx := Element.from_object[T](x) or { panic(err) }
		yy := Element.from_object[T](y) or { panic(err) }
		aa := encode(xx) or { panic(err) }
		bb := encode(yy) or { panic(err) }
		return aa.bytestr().compare(bb.bytestr())
	})
}

// add_element adds an element el into SetOf[T] fields.
pub fn (mut so SetOf[T]) add_element(el Element) ! {
	so.relaxed_add_element(el, true)!
}

// add_element allows adding a new element into current Set fields.
// Its does not allow adding element when is already the same tag in the fields.
// but, some exception when you set relaxed to true
fn (mut so SetOf[T]) relaxed_add_element(el Element, relaxed bool) ! {
	the_t := el.into_object[T]()!
	if so.fields.len == 0 {
		// just adds it then return
		so.fields << the_t
		return
	}
	mut filtered_by_tag := []T{}
	for field in so.fields {
		item := Element.from_object(field)!
		if item.equal(el) {
			return error('has already in the fields')
		}
		if item.tag().equal(el.tag()) {
			// add this to filtered
			filtered_by_tag << field
		}
	}

	if filtered_by_tag.len == 0 {
		so.fields << the_t
		return
	} else {
		if !relaxed {
			return error('You can not insert element without forcing')
		}
		so.fields << the_t
		return
	}
}
