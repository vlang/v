// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

// default_sequence_tag is the default tag of ASN.1 SEQUENCE (SEQUENCE OF) type.
pub const default_sequence_tag = Tag{.universal, true, int(TagType.sequence)}

// constant for sequence(of) and set(of) internal value
// vfmt off
const max_sequence_size 		= 256 // max of seq size
const max_sequence_bytes_length = (1 << 23 - 1) // 
const default_sequence_size 	= 64 // default size
// vfmt on

// ASN.1 UNIVERSAL CLASS OF SEQUENCE and SEQUENCE OF TYPE.
//
// https://letsencrypt.org/docs/a-warm-welcome-to-asn1-and-der
// These are two very different types.
// A SEQUENCE is equivalent to “struct” in most programming languages.
// It holds a fixed number of fields of different types.
// A SEQUENCE OF, holds an arbitrary number of fields of a single type.
// This is analogous to an array or a list in a programming language.
// Sequence structure can represents both SEQUENCE and SEQUENCE OF type.
// The encoding of a sequence value shall be constructed.
// in DER encoded of SEQUENCE or SET, never encode a default value.
pub struct Sequence {
mut:
	//	maximal size of this sequence fields
	size int = default_sequence_size
	// fields is the elements of the sequence
	fields []Element
}

// new creates new Sequence with default size.
pub fn Sequence.new() !Sequence {
	return Sequence.new_with_size(default_sequence_size)!
}

// from_list creates new Sequence from list of elements.
pub fn Sequence.from_list(els []Element) !Sequence {
	if els.len > max_sequence_size {
		return error('Sequence size exceed limit')
	}
	return Sequence{
		fields: els
	}
}

fn Sequence.new_with_size(size int) !Sequence {
	if size > max_sequence_size {
		return error('size is exceed limit')
	}
	if size < 0 {
		return error('Provides with correct size')
	}

	// if size is 0, use default_sequence_size
	limit := if size == 0 { default_sequence_size } else { size }
	return Sequence{
		size: limit
	}
}

// tag returns the tag of Sequence element.
pub fn (seq Sequence) tag() Tag {
	return default_sequence_tag
}

// payload returns the payload of Sequence element.
pub fn (seq Sequence) payload() ![]u8 {
	return seq.payload_with_rule(.der)!
}

fn (seq Sequence) payload_with_rule(rule EncodingRule) ![]u8 {
	mut out := []u8{}
	for el in seq.fields {
		obj := encode_with_rule(el, rule)!
		out << obj
	}
	return out
}

// encoded_len tells the length of serialized Sequence element in bytes.
pub fn (seq Sequence) encoded_len() int {
	mut n := 0
	n += seq.tag().tag_size()
	fields := ElementList(seq.fields)
	len := fields.encoded_len()
	length := Length.new(len) or { panic(err) }
	n += length.length_size() or { panic(err) }
	n += len
	return n
}

// fields returns the Sequences fields.
pub fn (seq Sequence) fields() []Element {
	return seq.fields
}

// parse tries to parse into Sequence from ongoing Parser p.
// Uts return a parsed Sequence or error on fails.
fn Sequence.parse(mut p Parser) !Sequence {
	tag := p.read_tag()!
	if !tag.equal(default_sequence_tag) {
		return error('Get non Sequence tag')
	}
	length := p.read_length()!
	content := p.read_bytes(length)!

	seq := Sequence.from_bytes(content)!
	return seq
}

// decode tries to decode bytes into Sequence.
// Its return a decoded Sequence and next offset to read on
// if possible, or return error on fails.
fn Sequence.decode(bytes []u8) !(Sequence, int) {
	return Sequence.decode_with_rule(bytes, 0, .der)!
}

fn Sequence.decode_with_rule(bytes []u8, loc int, rule EncodingRule) !(Sequence, int) {
	tag, length_pos := Tag.decode_with_rule(bytes, loc, rule)!
	if !tag.equal(default_sequence_tag) {
		return error('Get unexpected non-sequence tag')
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
	seq := Sequence.from_bytes(payload)!
	return seq, next
}

// bytes should seq.fields payload, not includes the tag
fn Sequence.from_bytes(bytes []u8) !Sequence {
	mut seq := Sequence{}
	if bytes.len == 0 {
		return seq
	}
	mut i := 0
	for i < bytes.len {
		el, pos := Element.decode_with_rule(bytes, i, .der)!
		i = pos
		seq.add_element(el)!
	}

	if i > bytes.len {
		return error('i > bytes.len')
	}
	if i < bytes.len {
		return error('The src contains unprocessed bytes')
	}
	return seq
}

// set_size sets maximal size of this sequence fields.
pub fn (mut seq Sequence) set_size(size int) ! {
	if size <= 0 {
		return error('provides with correct limit')
	}
	if size > max_sequence_size {
		return error('Provided limit was exceed current one')
	}
	seq.size = size
}

// add_element adds an element el into Sequence fields.
// By default its allows adding element with the same tag.
pub fn (mut seq Sequence) add_element(el Element) ! {
	seq.relaxed_add_element(el, true)!
}

// add_element allows adding a new element into current sequence fields.
// Its does not allow adding element when is already the same tag in the fields.
// but, some exception when you set relaxed to true
fn (mut seq Sequence) relaxed_add_element(el Element, relaxed bool) ! {
	// todo: check against size
	if seq.fields.len == 0 {
		// just adds it then return
		seq.fields << el
		return
	}

	// for item in seq.fields {
	//	if item.equal_with(el) {
	//		return error('has already in the fields')
	//	}
	// }
	filtered_by_tag := seq.fields.filter(it.tag().equal(el.tag()))
	if filtered_by_tag.len == 0 {
		seq.fields << el
		return
	} else {
		if !relaxed {
			return error('You can not insert element without forcing')
		}
		seq.fields << el
		return
	}
}

// is_sequence_of[T] checks whether this sequence is SequenceOf[T] type.
pub fn (seq Sequence) is_sequence_of[T]() bool {
	return seq.fields.all(it is T)
}

// into_sequence_of[T] turns this sequence into SequenceOf[T] element.
pub fn (seq Sequence) into_sequence_of[T]() !SequenceOf[T] {
	if seq.is_sequence_of[T]() {
		return error('This sequence is not SequenceOf[T]')
	}
	mut sqof := SequenceOf[T]{}
	for el in seq.fields {
		obj := el.into_object[T]()!
		sqof.fields << obj
	}
	return sqof
}

// ASN.1 SEQUENCE OF TYPE.
// SequenceOf[T] is an arrays of generic T, so the generic T should fullfill Element interface.
// We dont use generic aliases because generic type aliases are not yet implemented.
pub struct SequenceOf[T] {
mut:
	size   int = default_sequence_size
	fields []T
}

// SequenceOf.new creates a new SequenceOf[T]
pub fn SequenceOf.new[T]() SequenceOf[T] {
	return SequenceOf[T]{}
}

// SequenceOf.from_list creates a new SequenceOf[T] from arrays of T type.
pub fn SequenceOf.from_list[T](els []T) !SequenceOf[T] {
	if els.len > max_sequence_size {
		return error('SequenceOf size exceed limit')
	}
	$if T !is Element {
		return error('T not hold element')
	}
	return SequenceOf[T]{
		fields: els
	}
}

// The tag of SequenceOf element.
pub fn (so SequenceOf[T]) tag() Tag {
	return default_sequence_tag
}

// The payload of SequenceOf element.
pub fn (so SequenceOf[T]) payload() ![]u8 {
	return so.payload_with_rule(.der)!
}

fn (so SequenceOf[T]) payload_with_rule(rule EncodingRule) ![]u8 {
	$if T !is Element {
		return error('T is not an element')
	}
	mut out := []u8{}
	for el in so.fields {
		// placing el directly bring into error: `el` cannot be used as interface object
		// outside `unsafe` blocks as it might be stored on stack.
		curr := unsafe { el }
		obj := encode_with_rule(curr, rule)!
		out << obj
	}
	return out
}

// fields returns underlying arrays of T from the SequenceOf[T].
pub fn (so SequenceOf[T]) fields() []T {
	return so.fields
}
