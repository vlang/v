// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

import encoding.hex

// default_octetstring_tag is the default tag of ASN.1 OCTETSTRING type.
pub const default_octetstring_tag = Tag{.universal, false, int(TagType.octetstring)}

const max_octetstring_size = 1 << 32 - 1

// ASN.1 UNIVERSAL TYPE OF OCTETSTRING.
//
// The ASN.1 OCTET STRING type contains arbitrary strings of octets.
// This type is very similar to BIT STRING, except that all values must
// be an integral number of eight bits.
// You can use constraints to specify a maximum length for an OCTET STRING type.
pub struct OctetString {
pub:
	value string
}

// tag returns the tag of OctetString type.
pub fn (oct OctetString) tag() Tag {
	return default_octetstring_tag
}

// payload returns the payload of OctetString type.
pub fn (oct OctetString) payload() ![]u8 {
	return oct.payload_with_rule(.der)!
}

fn (oct OctetString) payload_with_rule(rule EncodingRule) ![]u8 {
	if rule != .der && rule != .ber {
		return error('not supported rule')
	}
	return oct.value.bytes()
}

fn (oct OctetString) str() string {
	if oct.value.len == 0 {
		return 'OctetString (<empty>)'
	}
	return 'OctetString (${oct.value})'
}

// new creates a new OctetString element from string s.
pub fn OctetString.new(s string) !OctetString {
	if !valid_octet_string(s) {
		return error('not valid octet string')
	}
	return OctetString{
		value: s
	}
}

// from_hexstring creates a new OctetString element from valid hex string or error on fails.
pub fn OctetString.from_hexstring(hs string) !OctetString {
	bytes := hex.decode(hs)!
	oct := OctetString.from_bytes(bytes)!

	return oct
}

// parse an OctetString from ongoing Parser
fn OctetString.parse(mut p Parser) !OctetString {
	tag := p.read_tag()!
	if !tag.equal(default_octetstring_tag) {
		return error('Bad octetstring tag')
	}
	length := p.read_length()!
	content := p.read_bytes(length)!

	payload := if length == 0 { []u8{} } else { content }

	oct := OctetString.from_bytes(payload)!
	return oct
}

fn OctetString.decode(src []u8) !(OctetString, int) {
	return OctetString.decode_with_rule(src, .der)!
}

fn OctetString.decode_with_rule(bytes []u8, rule EncodingRule) !(OctetString, int) {
	tag, length_pos := Tag.decode_with_rule(bytes, 0, rule)!
	if !tag.equal(default_octetstring_tag) {
		return error('Unexpected non-octetstring tag')
	}
	length, content_pos := Length.decode_with_rule(bytes, length_pos, rule)!
	content := if length == 0 {
		[]u8{}
	} else {
		if content_pos >= bytes.len || content_pos + length > bytes.len {
			return error('OctetString: truncated payload bytes')
		}
		unsafe { bytes[content_pos..content_pos + length] }
	}

	os := OctetString.from_bytes(content)!
	next := content_pos + length

	return os, next
}

fn OctetString.from_bytes(src []u8) !OctetString {
	return OctetString.new(src.bytestr())!
}

// UTILITY for OCTETSTRING
fn valid_octet_string(s string) bool {
	// just return true
	return true
}
