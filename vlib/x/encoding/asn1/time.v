// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

import time

// default_utctime_tag is the default tag of ASN.1 UTCTIME type.
pub const default_utctime_tag = Tag{.universal, false, int(TagType.utctime)}
// default_generalizedtime_tag is the default tag of ASN.1 GENERALIZEDTIME type.
pub const default_generalizedtime_tag = Tag{.universal, false, int(TagType.generalizedtime)}

const default_utctime_format = 'YYMMDDHHmmssZ'
const default_genztime_format = 'YYYYMMDDHHmmssZ'

// ASN.1 UNIVERSAL CLASS OF UTCTIME TYPE.
//
// For this time, UtcTime represented by simple string with format "YYMMDDhhmmssZ"
// - the six digits YYMMDD where YY is the two low-order digits of the Christian year,
// (RFC 5280 defines it as a range from 1950 to 2049 for X.509), MM is the month
// (counting January as 01), and DD is the day of the month (01 to 31).
// - the four digits hhmm where hh is hour (00 to 23) and mm is minutes (00 to 59); (SEE NOTE BELOW)
// - the six digits hhmmss where hh and mm are as in above, and ss is seconds (00 to 59);
// - the character Z;
// - one of the characters + or -, followed by hhmm, where hh is hour and mm is minutes (NOT SUPPORTED)
//
// NOTE
// -----
// - Restrictions employed by DER, the encoding shall terminate with "Z".
// - The seconds element shall always be present, and DER (along with RFC 5280) specify that seconds must be present,
// - Fractional seconds must not be present.
//
// TODO:
// - check for invalid representation of date and hhmmss part.
// - represented UtcTime in time.Time
pub struct UtcTime {
pub:
	value string
}

// new_utctime creates a new UtcTime element from string s.
pub fn UtcTime.new(s string) !UtcTime {
	valid := validate_utctime(s)!

	if !valid {
		return error('UtcTime: fail on validate utctime')
	}
	return UtcTime{
		value: s
	}
}

// from_time creates a new UtcTime element from standard `time.Time` in UTC time format.
pub fn UtcTime.from_time(t time.Time) !UtcTime {
	// changes into utc time
	utime := t.local_to_utc()
	s := utime.custom_format(default_utctime_format) //  20241113060446+0
	// Its rather a hack, not efieient as should be.
	// TODO: make it better
	str := s.split_any('+-')
	val := str[0] + 'Z'
	utc := UtcTime.new(val)!
	return utc
}

fn UtcTime.from_bytes(b []u8) !UtcTime {
	return UtcTime.new(b.bytestr())!
}

fn (utc UtcTime) str() string {
	if utc.value.len == 0 {
		return 'UtcTime (<empty>)'
	}
	return 'UtcTime (${utc.value})'
}

// tag retursn the tag of the UtcTime element.
pub fn (utc UtcTime) tag() Tag {
	return default_utctime_tag
}

// payload returns the payload of the UtcTime element.
pub fn (utc UtcTime) payload() ![]u8 {
	return utc.payload_with_rule(.der)!
}

// into_utctime turns this UtcTime into corresponding UTC time.
pub fn (utc UtcTime) into_utctime() !time.Time {
	valid := validate_utctime(utc.value)!

	if !valid {
		return error('UtcTime: fail on validate utctime value')
	}

	st := time.parse_format(utc.value, default_utctime_format)!
	utime := st.local_to_utc()

	return utime
}

fn (utc UtcTime) payload_with_rule(rule EncodingRule) ![]u8 {
	valid := validate_utctime(utc.value)!

	if !valid {
		return error('UtcTime: fail on validate utctime')
	}

	return utc.value.bytes()
}

fn UtcTime.parse(mut p Parser) !UtcTime {
	tag := p.read_tag()!
	if !tag.equal(default_utctime_tag) {
		return error('Bad UtcTime tag')
	}
	length := p.read_length()!
	bytes := p.read_bytes(length)!

	res := UtcTime.from_bytes(bytes)!

	return res
}

// UtcTime.decode tries to decode bytes into UtcTime with DER rule
fn UtcTime.decode(src []u8) !(UtcTime, int) {
	return UtcTime.decode_with_rule(src, .der)!
}

fn UtcTime.decode_with_rule(bytes []u8, rule EncodingRule) !(UtcTime, int) {
	tag, length_pos := Tag.decode_with_rule(bytes, 0, rule)!
	if !tag.equal(default_utctime_tag) {
		return error('Unexpected non-utctime tag')
	}
	length, content_pos := Length.decode_with_rule(bytes, length_pos, rule)!
	content := if length == 0 {
		[]u8{}
	} else {
		if content_pos >= bytes.len || content_pos + length > bytes.len {
			return error('UtcTime: truncated payload bytes')
		}
		unsafe { bytes[content_pos..content_pos + length] }
	}

	utc := UtcTime.from_bytes(content)!
	next := content_pos + length

	return utc, next
}

// utility function for UtcTime
//
fn validate_utctime(s string) !bool {
	if !basic_utctime_check(s) {
		return false
	}
	// read contents
	src := s.bytes()
	mut pos := 0
	mut year, mut month, mut day := u16(0), u8(0), u8(0)
	mut hour, mut minute, mut second := u8(0), u8(0), u8(0)

	// UtcTime only encodes times prior to 2050
	year, pos = read_2_digits(src, pos)!
	year = u16(year)
	if year >= 50 {
		year = 1900 + year
	} else {
		year = 2000 + year
	}

	month, pos = read_2_digits(src, pos)!
	day, pos = read_2_digits(src, pos)!

	if !validate_date(year, month, day) {
		return false
	}

	// hhmmss parts
	hour, pos = read_2_digits(src, pos)!
	minute, pos = read_2_digits(src, pos)!
	second, pos = read_2_digits(src, pos)!

	if hour > 23 || minute > 59 || second > 59 {
		return false
	}
	// assert pos == src.len - 1
	if src[pos] != 0x5A {
		return false
	}
	return true
}

fn basic_utctime_check(s string) bool {
	return s.len == 13 && valid_time_contents(s)
}

fn valid_time_contents(s string) bool {
	return s.ends_with('Z') && s.contains_any('0123456789')
}

// ASN.1 UNIVERSAL CLASS OF GENERALIZEDTIME TYPE.
//
// In DER Encoding scheme, GeneralizedTime should :
// - The encoding shall terminate with a "Z"
// - The seconds element shall always be present
// - The fractional-seconds elements, if present, shall omit all trailing zeros;
// - if the elements correspond to 0, they shall be wholly omitted, and the decimal point element also shall be omitted
//
// GeneralizedTime values MUST be:
// - expressed in Greenwich Mean Time (Zulu) and MUST include seconds
// (i.e., times are `YYYYMMDDHHMMSSZ`), even where the number of seconds
// is zero.
// - GeneralizedTime values MUST NOT include fractional seconds.
pub struct GeneralizedTime {
pub:
	value string
}

// GeneralizedTime.new creates a new GeneralizedTime from string s
pub fn GeneralizedTime.new(s string) !GeneralizedTime {
	valid := validate_generalizedtime(s)!
	if !valid {
		return error('GeneralizedTime: failed on validate')
	}
	return GeneralizedTime{
		value: s
	}
}

// from_time creates GeneralizedTime element from standard `time.Time` (as an UTC time).
pub fn GeneralizedTime.from_time(t time.Time) !GeneralizedTime {
	u := t.local_to_utc()
	s := u.custom_format(default_genztime_format)
	// adds support directly from time.Time
	src := s.split_any('+-')
	val := src[0] + 'Z'
	gt := GeneralizedTime.new(val)!

	return gt
}

// into_utctime turns this GeneralizedTime into corresponding UTC time.
pub fn (gt GeneralizedTime) into_utctime() !time.Time {
	valid := validate_generalizedtime(gt.value)!

	if !valid {
		return error('GeneralizedTime: fail on validate value')
	}

	st := time.parse_format(gt.value, default_genztime_format)!
	utime := st.local_to_utc()

	return utime
}

// The tag of GeneralizedTime element.
pub fn (gt GeneralizedTime) tag() Tag {
	return default_generalizedtime_tag
}

// The payload of GeneralizedTime element.
pub fn (gt GeneralizedTime) payload() ![]u8 {
	valid := validate_generalizedtime(gt.value)!
	if !valid {
		return error('GeneralizedTime: failed on validate')
	}
	return gt.value.bytes()
}

fn GeneralizedTime.from_bytes(b []u8) !GeneralizedTime {
	return GeneralizedTime.new(b.bytestr())!
}

// GeneralizedTime.parse tries to parse throught ongoing Parser into GeneralizedTime
fn GeneralizedTime.parse(mut p Parser) !GeneralizedTime {
	tag := p.read_tag()!
	if !tag.equal(default_generalizedtime_tag) {
		return error('Bad GeneralizedTime tag')
	}
	length := p.read_length()!
	bytes := p.read_bytes(length)!

	res := GeneralizedTime.from_bytes(bytes)!

	return res
}

fn GeneralizedTime.decode(bytes []u8) !(GeneralizedTime, int) {
	return GeneralizedTime.decode_with_rule(bytes, .der)!
}

fn GeneralizedTime.decode_with_rule(bytes []u8, rule EncodingRule) !(GeneralizedTime, int) {
	tag, length_pos := Tag.decode_with_rule(bytes, 0, rule)!
	if !tag.equal(default_generalizedtime_tag) {
		return error('Get bad GeneralizedTime tag')
	}
	length, content_pos := Length.decode_with_rule(bytes, length_pos, rule)!
	content := if length == 0 {
		[]u8{}
	} else {
		if content_pos >= bytes.len || content_pos + length > bytes.len {
			return error('GeneralizedTime: truncated payload bytes')
		}
		unsafe { bytes[content_pos..content_pos + length] }
	}

	gtc := GeneralizedTime.from_bytes(content)!
	next := content_pos + length

	return gtc, next
}

// utility function for GeneralizedTime
// TODO: more clear and concise validation check

fn min_generalizedtime_length(s string) bool {
	// minimum length without fractional element
	return s.len >= 15
}

fn generalizedtime_contains_fraction(s string) bool {
	// contains '.' part
	return s.contains('.')
}

fn basic_generalizedtime_check(s string) bool {
	return min_generalizedtime_length(s) && valid_time_contents(s)
}

fn validate_generalizedtime(s string) !bool {
	if !basic_generalizedtime_check(s) {
		return false
	}
	// read contents
	src := s.bytes()
	mut pos := 0
	mut year, mut month, mut day := u16(0), u8(0), u8(0)
	mut hour, mut minute, mut second := u8(0), u8(0), u8(0)

	// Generalized time format was "YYYYMMDDhhmmssZ"
	// TODO: support for second fractions part
	year, pos = read_4_digits(src, pos)!
	// year = u16(year)
	month, pos = read_2_digits(src, pos)!
	day, pos = read_2_digits(src, pos)!

	if !validate_date(year, month, day) {
		return false
	}

	// hhmmss parts
	hour, pos = read_2_digits(src, pos)!
	minute, pos = read_2_digits(src, pos)!
	second, pos = read_2_digits(src, pos)!

	if hour > 23 || minute > 59 || second > 59 {
		return false
	}
	// assert pos == src.len - 1
	if src[pos] != 0x5A {
		return false
	}
	return true
}
