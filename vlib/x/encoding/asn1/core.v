// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

// vfmt off
// bit masking values for ASN.1 tag header
const tag_class_mask 	= 0xc0 // 192, bits 8-7
const constructed_mask 	= 0x20 //  32, bits 6
const tag_number_mask 	= 0x1f //  31, bits 1-5
// vfmt on

// Maximum number of bytes to represent tag number, includes the tag byte.
const max_tag_length = 3
const max_tag_number = 16383

// Maximum value for UNIVERSAL class tag number, see `TagType`,
// Tag number above this number should be considered to other class,
// PRIVATE, CONTEXT_SPECIFIC or APPLICATION class.
const max_universal_tagnumber = 255

// ASN.1 Tag identifier handling.
//
// Tag represents identifier of the ASN.1 element (object).
// ASN.1 Tag number can be represented in two form, the short form and the long form.
// The short form for tag number below <= 30 and stored enough in single byte.
// The long form for tag number > 30, and stored in two or more bytes.
//
// ASN.1 imposes no limit on the tag number, but the NIST Stable Implementation Agreements (1991)
// and its European and Asian counterparts limit the size of tags to 16383.
// see https://www.oss.com/asn1/resources/asn1-faq.html#tag-limitation
// We impose limit on the tag number to be in range 0..16383.
// Its big enough to accomodate and represent different of yours own tag number.
// Its represents 2 bytes length where maximum bytes arrays to represent tag number
// in multibyte (long) form is `[u8(0x1f), 0xff, 0x7f]` or 16383 in base 128.
pub struct Tag {
mut:
	class       TagClass = .universal
	constructed bool
	number      int
}

// `Tag.new` creates new ASN.1 tag identifier. Its accepts params of TagClass `cls`,
// the tag form in the constructed or primitive form in `constructed` flag, and the integer tag `number`.
pub fn Tag.new(cls TagClass, constructed bool, number int) !Tag {
	if number < 0 || number > max_tag_number {
		return error('Unallowed tag number, ${number} exceed limit')
	}
	// validates required criteria
	if cls == .universal {
		// UNIVERSAL tag number should below max_universal_tagnumber
		if number > max_universal_tagnumber {
			return error('Not a valid tag number for universal class=${number}')
		}
		// SEQUENCE (OF) or SET (OF) should be in constructed form
		if number == int(TagType.sequence) || number == int(TagType.set) {
			if !constructed {
				return error('For SEQUENCE(OF) or SET(OF) type, should be in constructed form')
			}
		}
	}

	// otherwise, ok
	tag := Tag{
		class:       cls
		constructed: constructed
		number:      number
	}
	return tag
}

// `tag_class` return the ASN.1 class of this tag
pub fn (t Tag) tag_class() TagClass {
	return t.class
}

// `is_constructed` tells us whether this tag is in constructed form
// or the primitive one, ie, not constructed.
pub fn (t Tag) is_constructed() bool {
	return t.constructed
}

// `tag_number` return the tag nunber of this tag
pub fn (t Tag) tag_number() int {
	return t.number
}

// `encode` serializes the tag into bytes array with default rule, ie, .der.
// Its serializes into dst bytes buffer or return error on fails.
pub fn (t Tag) encode(mut dst []u8) ! {
	t.encode_with_rule(mut dst, .der)!
}

// `Tag.from_bytes` creates a new Tag from bytes. Its return newly created tag and
// the rest of remaining bytes on success, or error on failures.
pub fn Tag.from_bytes(bytes []u8) !(Tag, []u8) {
	tag, next_pos := Tag.decode(bytes)!
	if next_pos < bytes.len {
		rest := unsafe { bytes[next_pos..] }
		return tag, rest
	}
	if next_pos == bytes.len {
		return tag, []u8{}
	}
	return error('Tag: too short data')
}

// `equal` checks whether this tag is equal with the other tag
pub fn (t Tag) equal(o Tag) bool {
	return t.class == o.class && t.constructed == o.constructed && t.number == o.number
}

// encode_with_rule serializes tag into bytes array
fn (t Tag) encode_with_rule(mut dst []u8, rule EncodingRule) ! {
	// we currently only support .der or (stricter) .ber
	if rule != .der && rule != .ber {
		return error('Tag: unsupported rule')
	}
	// makes sure tag number is valid
	if t.number > max_tag_number {
		return error('Tag: tag number exceed limit')
	}

	// get the class type and constructed bit and build the bytes tag.
	// if the tag number > 0x1f, represented in long form required two or more bytes,
	// otherwise, represented in short form, fit in single byte.
	mut b := (u8(t.class) << 6) & tag_class_mask
	if t.constructed {
		b |= constructed_mask
	}
	// The tag is in long form
	if t.number >= 0x1f {
		b |= tag_number_mask // 0x1f
		dst << b
		t.to_bytes_in_base128(mut dst)!
	} else {
		// short form
		b |= u8(t.number)
		dst << b
	}
}

// Tag.decode tries to deserializes bytes into Tag.
// Its return error on fails.
fn Tag.decode(bytes []u8) !(Tag, int) {
	tag, next := Tag.decode_from_offset(bytes, 0)!
	return tag, next
}

// Tag.decode tries to deserializes bytes into Tag. its return error on fails.
fn Tag.decode_from_offset(bytes []u8, pos int) !(Tag, int) {
	// default rule
	tag, next := Tag.decode_with_rule(bytes, pos, .der)!
	return tag, next
}

// Tag.decode_with_rule deserializes bytes back into Tag structure start from `loc` offset.
// By default, its decodes in .der encoding rule, if you want more control, pass your `Params`.
// Its return Tag and next offset to operate on, and return error if it fails to decode.
fn Tag.decode_with_rule(bytes []u8, loc int, rule EncodingRule) !(Tag, int) {
	// preliminary check
	if bytes.len < 1 {
		return error('Tag: bytes underflow')
	}
	if rule != .der && rule != .ber {
		return error('Tag: unsupported rule')
	}
	// when accessing byte at ofset `loc` within bytes, ie, `b := bytes[loc]`,
	// its maybe can lead to panic when the loc is not be checked.
	if loc >= bytes.len {
		return error('Tag: invalid pos')
	}
	mut pos := loc
	// first byte of tag bytes
	b := bytes[pos]
	pos += 1

	// First we get the first byte from the bytes, check and gets the class and constructed bits
	// and the tag number marker. If this marker == 0x1f, it tells whether the tag number is represented
	// in multibyte (long form), or short form otherwise.
	class := int((b & tag_class_mask) >> 6)
	constructed := b & constructed_mask == constructed_mask
	mut number := int(b & tag_number_mask)

	// check if this `number` is in long (multibyte) form, and interpretes more bytes as a tag number.
	if number == 0x1f {
		// we only allowed `max_tag_length` bytes following to represent tag number.
		number, pos = Tag.read_tagnum(bytes, pos)!

		// pos is the next position to read next bytes, so check tag bytes length
		if pos >= max_tag_length + loc + 1 {
			return error('Tag: tag bytes is too long')
		}
		if number < 0x1f && rule == .der {
			// requirement for DER encoding.
			// TODO: the other encoding may remove this restriction
			return error('Tag: non-minimal tag')
		}
	}
	// build the tag
	tag := Tag.new(TagClass.from_int(class)!, constructed, number)!

	return tag, pos
}

fn (t Tag) str() string {
	cls := t.class.str()
	form := if t.constructed { 'true' } else { 'false' }
	number := t.number.str()
	return '${cls}-${form}-${number}'
}

// bytes_len tells amount of bytes needed to store tag in base 128
fn (t Tag) bytes_len() int {
	if t.number == 0 {
		return 1
	}
	mut n := t.number
	mut ret := 0

	for n > 0 {
		ret += 1
		n >>= 7
	}

	return ret
}

// `tag_size` informs us how many bytes needed to store this tag includes one byte marker if in the long form.
pub fn (t Tag) tag_size() int {
	// when number is greater than 31 (0x1f), its need more bytes
	// to represent this number, includes one byte marker for long form tag
	len := if t.number < 0x1f { 1 } else { t.bytes_len() + 1 }
	return len
}

// to_bytes_in_base128 serializes tag number into bytes in base 128
fn (t Tag) to_bytes_in_base128(mut dst []u8) ! {
	n := t.bytes_len()
	for i := n - 1; i >= 0; i-- {
		mut o := u8(t.number >> u32(i * 7))
		o &= 0x7f
		if i != 0 {
			o |= 0x80
		}
		dst << o
	}
}

// Tag.read_tagnum read the tag number from bytes from offset pos in base 128.
// Its return deserialized Tag number and next offset to process on.
fn Tag.read_tagnum(bytes []u8, pos int) !(int, int) {
	tnum, next := Tag.read_tagnum_with_rule(bytes, pos, .der)!
	return tnum, next
}

// read_tagnum_with_rule is the main routine to read the tag number part in the bytes source,
// start from offset loc in base 128. Its return the tag number and next offset to process on, or error on fails.
fn Tag.read_tagnum_with_rule(bytes []u8, loc int, rule EncodingRule) !(int, int) {
	if loc > bytes.len {
		return error('Tag number: invalid pos')
	}
	mut pos := loc
	mut ret := 0
	for s := 0; pos < bytes.len; s++ {
		ret <<= 7
		b := bytes[pos]

		if s == 0 && b == 0x80 {
			if rule == .der {
				// requirement for DER encoding
				return error('Tag number: integer is not minimally encoded')
			}
		}
		ret |= b & 0x7f
		pos += 1

		if b & 0x80 == 0 {
			if ret > max_tag_number {
				return error('Tag number: base 128 integer too large')
			}
			if ret < 0 {
				return error('Negative tag number')
			}

			return ret, pos
		}
	}
	return error('Tag: truncated base 128 integer')
}

fn (t Tag) valid_supported_universal_tagnum() bool {
	return t.class == .universal && t.number < max_universal_tagnumber
}

// `universal_tag_type` turns this Tag into available UNIVERSAL class of TagType,
// or return error if it is unknown number.
pub fn (t Tag) universal_tag_type() !TagType {
	// currently, only support Standard universal tag number
	if t.number > max_universal_tagnumber {
		return error('Tag number: unknown TagType number=${t.number}')
	}
	match t.class {
		.universal {
			match t.number {
				// vfmt off
				0 { return .reserved }
				1 { return .boolean }
				2 { return .integer }
				3 { return .bitstring }
				4 { return .octetstring }
				5 { return .null }
				6 { return .oid }
				7 { return .objdesc }
				8 { return .external }
				9 { return .real }
				10 { return .enumerated }
				11 { return .embedded }
				12 { return .utf8string }
				13 { return .relativeoid }
				14 { return .time }
				16 { return .sequence }
				17 { return .set }
				18 { return .numericstring }
				19 { return .printablestring }
				20 { return .t61string }
				21 { return .videotexstring }
				22 { return .ia5string }
				23 { return .utctime }
				24 { return .generalizedtime }
				25 { return .graphicstring }
				26 { return .visiblestring }
				27 { return .generalstring }
				28 { return .universalstring }
				29 { return .characterstring }
				30 { return .bmpstring }
				31 { return .date }
				32 { return .time_of_day }
				33 { return .date_time }
				34 { return .duration }
				35 { return .i18_oid }
				36 { return .relative_i18_oid }
				else {
					return error('reserved or unknonw number')
				}
				// vfmt on
			}
		}
		else {
			return error('Not universal class type')
		}
	}
}

// TagClass is an enum of ASN.1 tag class.
//
// To make sure ASN.1 encodings are not ambiguous, every ASN.1 type is associated with a tag.
// A tag consists of three parts: the tag class, tag form and the tag number.
// The following classes are defined in the ASN.1 standard.
pub enum TagClass {
	universal        = 0x00 // 0b00
	application      = 0x01 // 0b01
	context_specific = 0x02 // 0b10
	private          = 0x03 // 0b11
}

// `TagClass.from_int` creates TagClass from integer v.
// Its return a new TagClass on success or error on fails.
fn TagClass.from_int(v int) !TagClass {
	match v {
		// vfmt off
		0x00 { return .universal }
		0x01 { return .application }
		0x02 { return .context_specific }
		0x03 { return .private }
		else {
			return error('Bad class number')
		}
		// vfmt on
	}
}

// `TagClass.from_string` creates a TagClass from string s.
// Its return a new TagClass on success or error on fails.
fn TagClass.from_string(s string) !TagClass {
	match s {
		// vfmt off
		'universal' { return .universal }
		'private' { return .private }
		'application' { return .application }
		'context_specific' { return .context_specific }
		else { 
			return error('bad class string') 
		}
		// vfmt on
	}
}

fn (c TagClass) str() string {
	match c {
		.universal { return 'universal' }
		.application { return 'application' }
		.context_specific { return 'context_specific' }
		.private { return 'private' }
	}
}

// TagType is standard UNIVERSAL tag number. Some of them was deprecated,
// so its not going to be supported on this module.
pub enum TagType {
	// vfmt off
	reserved 			= 0 	// reserved for BER
	boolean 			= 1 	// BOOLEAN type
	integer 			= 2 	// INTEGER type
	bitstring 			= 3 	// BIT STRING
	octetstring 		= 4 	// OCTET STRING
	null 				= 5 	// NULL
	oid 				= 6		// OBJECT IDENTIFIER
	objdesc 			= 7 	// OBJECT DESCRIPTOR
	external 			= 8 	// INSTANCE OF, EXTERNAL
	real 				= 9 	// REAL
	enumerated 			= 10 	// ENUMERATED
	embedded 			= 11 	// EMBEDDED PDV
	utf8string 			= 12 	// UTF8STRING
	relativeoid 		= 13 	// RELATIVE-OID
	// deprecated
	time 				= 14
	// 0x0f is reserved
	sequence 			= 16 	// SEQUENCE, SEQUENCE OF, Constructed
	set 				= 17 	// SET, SET OF, Constructed
	numericstring 		= 18 	// NUMERICSTRING
	printablestring 	= 19 	// PRINTABLESTRING
	t61string 			= 20 	// TELETEXSTRING, T61STRING
	videotexstring 		= 21 	// VIDEOTEXSTRING
	ia5string 			= 22 	// IA5STRING
	utctime 			= 23 	// UTCTIME
	generalizedtime 	= 24 	// GENERALIZEDTIME
	graphicstring 		= 25 	// GRAPHICSTRING
	visiblestring 		= 26 	// VISIBLESTRING, ISO646STRING
	generalstring 		= 27 	// GENERALSTRING
	universalstring 	= 28 	// UNIVERSALSTRING
	characterstring 	= 29 	// CHARACTER STRING
	bmpstring   		= 30 	// BMPSTRING
	// unsupported 
	date        		= 0x1f
	time_of_day 		= 0x20
	date_time   		= 0x21
	duration    		= 0x22
	// Internationalized OID
	i18_oid 			= 0x23
	// Internationalized Relative OID
	// Reserved 0x25 and above
	relative_i18_oid 	= 0x24
	// vfmt on
}

fn (t TagType) str() string {
	match t {
		.boolean { return 'BOOLEAN' }
		.integer { return 'INTEGER' }
		.bitstring { return 'BITSTRING' }
		.octetstring { return 'OCTETSTRING' }
		.null { return 'NULL' }
		.oid { return 'OID' }
		.objdesc { return 'OBJECT_DESCRIPTOR' }
		.external { return 'EXTERNAL' }
		.real { return 'REAL' }
		.enumerated { return 'ENUMERATED' }
		.embedded { return 'EMBEDDED' }
		.utf8string { return 'UTF8STRING' }
		.relativeoid { return 'RELATIVEOID' }
		.time { return 'TIME' }
		.sequence { return 'SEQUENCE_OR_SEQUENCEOF' }
		.set { return 'SET_OR_SET_OF' }
		.numericstring { return 'NUMERICSTRING' }
		.printablestring { return 'PRINTABLESTRING' }
		.t61string { return 'T61STRING' }
		.videotexstring { return 'VIDEOTEXSTRING' }
		.ia5string { return 'IA5STRING' }
		.utctime { return 'UTCTIME' }
		.generalizedtime { return 'GENERALIZEDTIME' }
		.graphicstring { return 'GRAPHICSTRING' }
		.visiblestring { return 'VISIBLESTRING' }
		.generalstring { return 'GENERALSTRING' }
		.universalstring { return 'UNIVERSALSTRING' }
		.characterstring { return 'CHARACTERSTRING' }
		.bmpstring { return 'BMPSTRING' }
		else { return 'UNSUPPORTED_TAG_TYPE' }
	}
}

// universal_tag_from_int gets default Tag for universal class type from integer value v.
fn universal_tag_from_int(v int) !Tag {
	if v < 0 || v > max_universal_tagnumber {
		return error('Get unexpected tag number for universal type')
	}
	match v {
		// vfmt off
		1 { return default_boolean_tag }
		2 { return default_integer_tag }
		3 { return default_bitstring_tag }
		4 { return default_octetstring_tag }
		5 { return default_null_tag }
		6 { return default_oid_tag }
		10 { return default_enumerated_tag }
		12 { return default_utf8string_tag }
		16 { return default_sequence_tag }
		17 { return default_set_tag }
		18 { return default_numericstring_tag }
		19 { return default_printablestring_tag }
		22 { return default_ia5string_tag }
		23 { return default_utctime_tag }
		24 { return default_generalizedtime_tag }
		26 { return default_visiblestring_tag }
		else {
			// should in primitive form 
			return Tag{.universal, false, v}
		}
		// vfmt on
	}
}

// EncodingRule is standard of rule thats drives how some ASN.1
// element was encoded or deserialized.
pub enum EncodingRule {
	// Distinguished Encoding Rules (DER)
	der = 0
	// Basic Encoding Rules (BER)
	ber = 1
	// Octet Encoding Rules (OER)
	oer = 2
	// Packed Encoding Rules (PER)
	per = 3
	// XML Encoding Rules (XER)
	xer = 4
}

// EXPLICIT and IMPLICIT MODE
//
// TaggedMode is way of rule how some element is being wrapped (unwrapped).
// Explicit rule add new (outer) tag to the existing element,
// where implicit rule replaces the tag of existing element.
pub enum TaggedMode {
	explicit
	implicit
}

// `TaggedMode.from_string` creates TaggedMode from string s.
fn TaggedMode.from_string(s string) !TaggedMode {
	match s {
		'explicit' { return .explicit }
		'implicit' { return .implicit }
		// empty string marked as .explicit
		'' { return .explicit }
		else { return error('Bad string for tagged mode') }
	}
}

fn (m TaggedMode) str() string {
	match m {
		.explicit { return 'explicit' }
		.implicit { return 'implicit' }
	}
}
