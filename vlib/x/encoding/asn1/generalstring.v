// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

// default_generalstring_tag is the default tag of ASN.1 GENERALSTRING type.
pub const default_generalstring_tag = Tag{.universal, false, int(TagType.generalstring)}

// ASN.1 GENERALSTRING Handling
// It may contain any characters from a "G" and "C" set of any standardized character sets.
// A "G" set contains some specified set of graphic (i.e., printable) characters,
// while a "C" set contains a group of control characters.
// For example, the "G" set in the ASCII character set consists of the characters with ASCII numbers 33 through 126,
// while the "C" set is those characters with ASCII numbers 0 through 31.
// For historical reasons, the characters SPACE (number 32) and DELETE (number 127)
// are not considered to be in either the C set or the G set, but instead stand on their own
// We only treated GeneralString as an us-ascii charset
pub struct GeneralString {
pub:
	value string
}

fn (g GeneralString) check() ! {
	if !g.value.is_ascii() {
		return error('GeneralString: contains non-ascii chars')
	}
}

// new creates a GeneralString element from string s.
pub fn GeneralString.new(s string) !GeneralString {
	if !s.is_ascii() {
		return error('GeneralString: contains non-ascii chars')
	}
	return GeneralString{
		value: s
	}
}

fn (gst GeneralString) str() string {
	if gst.value.len == 0 {
		return 'GeneralString (<empty>)'
	}
	return 'GeneralString (${gst.value})'
}

// tag returns the tag of GeneralString type element.
pub fn (gst GeneralString) tag() Tag {
	return default_generalstring_tag
}

// payload returns the payload of GeneralString type element.
pub fn (gst GeneralString) payload() ![]u8 {
	return gst.payload_with_rule(.der)!
}

fn (gst GeneralString) payload_with_rule(rule EncodingRule) ![]u8 {
	if rule != .der && rule != .ber {
		return error('GeneralString: not supported rule')
	}
	gst.check()!
	return gst.value.bytes()
}

// from_bytes creates GeneralString from bytes b
fn GeneralString.from_bytes(b []u8) !GeneralString {
	if b.any(it < u8(` `) || it > u8(`~`)) {
		return error('GeneralString: bytes contains non-ascii chars')
	}
	return GeneralString{
		value: b.bytestr()
	}
}

// parse tries to read into GeneralString from parser p or return error on fails.
fn GeneralString.parse(mut p Parser) !GeneralString {
	tag := p.read_tag()!
	if !tag.equal(default_generalstring_tag) {
		return error('Bad GeneralString tag')
	}
	length := p.read_length()!
	bytes := p.read_bytes(length)!

	res := GeneralString.from_bytes(bytes)!

	return res
}

// decode tries to decode bytes array into GeneralString or return error on fails.
fn GeneralString.decode(src []u8) !(GeneralString, int) {
	return GeneralString.decode_with_rule(src, .der)!
}

fn GeneralString.decode_with_rule(bytes []u8, rule EncodingRule) !(GeneralString, int) {
	tag, length_pos := Tag.decode_with_rule(bytes, 0, rule)!
	if !tag.equal(default_generalstring_tag) {
		return error('Unexpected non-generalstring tag')
	}
	length, content_pos := Length.decode_with_rule(bytes, length_pos, rule)!
	content := if length == 0 {
		[]u8{}
	} else {
		// non-null length should contains non-null bytes
		if content_pos >= bytes.len || content_pos + length > bytes.len {
			return error('GeneralString: truncated payload bytes')
		}
		unsafe { bytes[content_pos..content_pos + length] }
	}

	gst := GeneralString.from_bytes(content)!
	next := content_pos + length

	return gst, next
}

// Utility function
fn validate_general_string(s string) bool {
	if !s.is_ascii() {
		return false
	}
	return true
}
