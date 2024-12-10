// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

// default_boolean_tag is the default tag of ASN.1 BOOLEAN type.
pub const default_boolean_tag = Tag{.universal, false, int(TagType.boolean)}

// ASN.1 UNIVERSAL CLASS OF BOOLEAN TYPE.
//
// A Boolean value can take true or false.
// ASN.1 DER encoding restricts encoding of boolean true value into 0xff
// and otherwise, encodes into zero (0x00) for false value.
// The encoding of a boolean value shall be primitive. The contents octets shall consist of a single octet.
pub struct Boolean {
mut:
	// boolean value represented in single  byte to allow stores multiple value represents
	// true value others than 0xff, ie., non-null byte representing true value.
	value u8
}

// new creates a new Boolean value from true or false value.
// By default, when you pass true, its would store 0xff as underlying byte value.
// If you want more to be relaxed, see from_u8 to creates with another byte value.
pub fn Boolean.new(value bool) Boolean {
	mut ret := Boolean{}
	val := if value { u8(0xff) } else { u8(0x00) }
	ret.value = val

	return ret
}

// from_u8 creates a new Boolean value from single byte value.
fn Boolean.from_u8(value u8) Boolean {
	return Boolean{
		value: value
	}
}

// tag returns the tag of Boolean type.
pub fn (v Boolean) tag() Tag {
	return default_boolean_tag
}

fn (v Boolean) str() string {
	res := if v.value() { 'Boolean (TRUE)' } else { 'Boolean (FALSE)' }
	return res
}

// payload returns the payload of Boolean type in .der rule.
pub fn (b Boolean) payload() ![]u8 {
	return b.payload_with_rule(.der)!
}

fn (b Boolean) payload_with_rule(rule EncodingRule) ![]u8 {
	// by default, true value is encoded to 0xff
	if rule == .der {
		if b.value != u8(0xff) && b.value != u8(0x00) {
			return error('Boolean: in .der, only 0xff or 0x00 are allowed')
		}
	}
	return [b.value]
}

// parse tries to read a Boolean type from parser or return error on fails
pub fn Boolean.parse(mut p Parser) !Boolean {
	tag := p.read_tag()!
	if !tag.equal(default_boolean_tag) {
		return error('Get unexpected non boolean tag')
	}
	length := p.read_length()!
	if length != 1 {
		return error('Bad boolean length')
	}
	bytes := p.read_bytes(length)!
	value := Boolean.from_bytes(bytes)!

	return value
}

// from_bytes creates a new ASN.1 BOOLEAN type from bytes b.
// Boolean type should fit in one byte length, otherwise it would return error.
// by default, p.rule == .der to follow DER restriction
fn Boolean.from_bytes(bytes []u8) !Boolean {
	return Boolean.from_bytes_with_rule(bytes, .der)
}

fn Boolean.from_bytes_with_rule(bytes []u8, rule EncodingRule) !Boolean {
	if bytes.len != 1 {
		return error('Boolean: bad bytes')
	}
	// for DER requirements that "If the encoding represents the boolean value TRUE,
	// its single contents octet shall have all eight bits set to one."
	// Thus only 0 and 255 are valid encoded values.
	// But, we relaxed this requirement to allow other than non-null
	// value to be treated as TRUE value, like in BER encoding.
	match bytes[0] {
		u8(0x00) {
			return Boolean.from_u8(0x00)
		}
		u8(0xff) {
			return Boolean.from_u8(0xff)
		}
		else {
			// other non-null value is treated as TRUE boolean value
			if rule == .der {
				return error('Boolean: in DER, other than 0xff is not allowed for true value')
			}
			return Boolean.from_u8(bytes[0])
		}
	}
}

// value gets the boolean value represented by underlying byte value.
// It returns FALSE if the byte == 0x00 and TRUE otherwise.
pub fn (b Boolean) value() bool {
	return b.value_with_rule(.der)
}

fn (b Boolean) value_with_rule(rule EncodingRule) bool {
	match b.value {
		u8(0xff) {
			return true
		}
		u8(0x00) {
			return false
		}
		else {
			if rule == .der {
				return false
			}
			// otherwise non-null is considered as true
			return true
		}
	}
}

// decode tries to decode bytes array into Booelan type or error on fails.
fn Boolean.decode(src []u8) !(Boolean, int) {
	return Boolean.decode_with_rule(src, 0, .der)!
}

fn Boolean.decode_with_rule(src []u8, loc int, rule EncodingRule) !(Boolean, int) {
	if src.len < 3 {
		return error('Boolean: bad length bytes')
	}
	if rule != .der && rule != .ber {
		return error('Boolean: not supported rule')
	}
	tag, length_pos := Tag.decode_with_rule(src, loc, rule)!
	if !tag.equal(default_boolean_tag) {
		return error('Unexpected non-boolean tag')
	}
	length, content_pos := Length.decode_with_rule(src, length_pos, rule)!
	if length != 1 {
		return error('Boolean: should have length 1')
	}
	if content_pos >= src.len || content_pos + length > src.len {
		return error('Boolean: truncated payload bytes')
	}
	payload := unsafe { src[content_pos..content_pos + length] }

	// boolean value should be encoded in single byte
	res := Boolean.from_bytes_with_rule(payload, rule)!
	next := content_pos + length
	return res, next
}
