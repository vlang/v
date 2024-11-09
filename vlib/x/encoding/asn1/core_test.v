// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

struct TagCreation {
	class       TagClass
	constructed bool
	number      int
	err         IError
}

fn test_tag_new_constructor() {
	tags := [
		TagCreation{.universal, false, 1, none},
		TagCreation{.universal, true, 1, none},
		// sequence should be constructed set
		TagCreation{.universal, true, 16, none},
		TagCreation{.universal, false, 16, error('For SEQUENCE(OF) or SET(OF) type, should be in constructed form')},
		// set
		TagCreation{.universal, true, 17, none},
		TagCreation{.universal, false, 17, error('For SEQUENCE(OF) or SET(OF) type, should be in constructed form')},
		//
		TagCreation{.universal, false, 28, none},
		TagCreation{.universal, false, 0x1f, none}, // 31
		TagCreation{.universal, false, 0x7f, none},
		TagCreation{.universal, false, 255, none}, // 0xff
		TagCreation{.universal, true, 255, none}, // 0xff
		TagCreation{.universal, true, 16380, error('Not a valid tag number for universal class=16380')},
		//  Context Specific should be in constructed form
		TagCreation{.context_specific, true, 128, none}, // 0x80
		TagCreation{.context_specific, false, 128, error('Context Specific should be in constructed form')}, // 0x80
		// Others was as is
		TagCreation{.private, false, 256, none},
		TagCreation{.private, true, 16383, none},
		TagCreation{.private, true, 16500, error('Unallowed tag number, 16500 exceed limit')},
		TagCreation{.application, false, 16000, none},
		TagCreation{.application, false, 16384, error('Unallowed tag number, 16384 exceed limit')},
		TagCreation{.application, true, 65535, error('Unallowed tag number, 65535 exceed limit')},
	]
	for i, c in tags {
		// creates tag with constructed bit set
		t := Tag.new(c.class, c.constructed, c.number) or {
			assert err == c.err
			continue
		}
		assert t.tag_class() == c.class
		assert t.is_constructed() == c.constructed
		assert t.tag_number() == c.number
	}
}

struct TagLengthTest {
	class     TagClass
	number    int
	explength int
	err       IError
}

fn test_class_tag_length_handling() ! {
	tags := [
		TagLengthTest{.universal, 0, 1, none},
		TagLengthTest{.universal, 1, 1, none},
		TagLengthTest{.universal, 28, 1, none},
		TagLengthTest{.universal, 0x1f, 2, none}, // 31
		TagLengthTest{.universal, 0x7f, 2, none},
		TagLengthTest{.context_specific, 128, 3, none}, // 0x80
		TagLengthTest{.universal, 255, 3, none}, // 0xff
		TagLengthTest{.private, 256, 3, error('Not a valid tag number for universal class=256')},
		TagLengthTest{.universal, 16380, 3, error('Not a valid tag number for universal class=16380')},
		TagLengthTest{.private, 16383, 3, none},
		TagLengthTest{.application, 16384, 4, error('Unallowed tag number, 16384 exceed limit')},
		TagLengthTest{.application, 65535, 4, error('Unallowed tag number, 65535 exceed limit')},
	]

	for i, c in tags {
		// creates tag with constructed bit set
		t := Tag.new(c.class, true, c.number) or {
			assert err == c.err
			continue
		}
		n := t.tag_size()
		assert n == c.explength
	}
}

struct TagDecodeTest {
	bytes       []u8
	class       TagClass
	constructed bool
	number      int
	lastpos     int
	err         IError
}

fn test_tag_decode() ! {
	data := [
		// 0b1000_000 telss its class is .context_specific, but constructed bit was not set
		TagDecodeTest{[u8(0x80), 0x01], .context_specific, false, 0, 1, none},
		TagDecodeTest{[u8(0xa0), 0x01], .context_specific, true, 0, 1, none}, //{2, 0, 1, true}},
		TagDecodeTest{[u8(0x02), 0x00], .universal, false, 2, 1, none},
		TagDecodeTest{[u8(0xfe), 0x00], .private, true, 30, 1, none},
		TagDecodeTest{[u8(0x1f), 0x1f, 0x00], .universal, false, 31, 2, none}, // high tag form
		TagDecodeTest{[u8(0x1f), 0x81, 0x00, 0x00], .universal, false, 128, 3, none},
		TagDecodeTest{[u8(0x1f), 0x81, 0x80, 0x01, 0x00], .universal, false, 16385, 4, error('Tag number: base 128 integer too large')}, // 1x128^2 + 0x128^1 + 1x128*0
		TagDecodeTest{[u8(0x00), 0x81, 0x80], .universal, false, 0, 1, none},
		TagDecodeTest{[u8(0x00), 0x83, 0x01, 0x00], .universal, false, 0, 1, none},
		TagDecodeTest{[u8(0x1f), 0x85], .universal, false, 0, 1, error('Tag: truncated base 128 integer')},
		TagDecodeTest{[u8(0x1f), 0x85, 0x81], .universal, false, 0, 0, error('Tag: truncated base 128 integer')},
		TagDecodeTest{[u8(0x30), 0x80], .universal, true, 0x10, 1, none},
		TagDecodeTest{[u8(0xa0), 0x82, 0x00, 0xff], .context_specific, true, 0, 1, none},
	]

	for i, c in data {
		// dump(i)
		tag, pos := Tag.decode(c.bytes) or {
			assert err == c.err
			continue
		}
		assert tag.class == c.class
		assert tag.constructed == c.constructed
		assert tag.number == c.number
		assert pos == c.lastpos
	}
}

struct TagNumberTest {
	num         int
	class       TagClass
	constructed bool
	exp         []u8
	err         IError
}

fn test_serialize_tag() ! {
	data := [
		TagNumberTest{0, .universal, false, [u8(0x00)], none},
		TagNumberTest{32, .universal, false, [u8(0x1f), 0x20], none}, // multibyte tag: 0x1f 0x20
		TagNumberTest{255, .universal, false, [u8(0x1f), 0x81, 0x7f], none}, // multibyte tag: 0x1f 0x81 0x7f
		TagNumberTest{0, .universal, true, [u8(0x20)], none}, // bits 6 set, 0010 0000 == 32
		TagNumberTest{1, .universal, true, [u8(0x21)], none}, // bits 6 set, 0010 0001 == 31
		TagNumberTest{32, .universal, true, [u8(0x3f), 0x20], none}, // multibyte tag: 00111111 0x20
		TagNumberTest{32, .application, true, [u8(0x7f), 0x20], none}, // multibyte tag: 127 (01111111) 0x20
		TagNumberTest{32, .context_specific, true, [u8(0xbf), 0x20], none}, // multibyte tag: 197 (10111111) 0x20
		TagNumberTest{32, .private, true, [u8(0xff), 0x20], none}, // multibyte tag: 255 (11111111) 0x20
		TagNumberTest{255, .context_specific, true, [u8(0xbf), 0x81, 0x7f], none}, // multibyte tag: 0xbf 0x81 0x7f
		// private class 0xdf = 0b1101_1111
		TagNumberTest{16383, .private, false, [u8(0xdf), 0xff, 0x7f], none}, // multibyte tag: 0x1f 0xff 0x7f
		// overflow max_tag_value
		TagNumberTest{16385, .context_specific, true, [u8(0x1f), 0xff, 0x7f], error('Unallowed tag number, 16385 exceed limit')}, // multibyte tag: 0x1f 0xff 0x7f
	]

	for i, c in data {
		mut dst := []u8{}
		tag := Tag.new(c.class, c.constructed, c.num) or {
			assert err == c.err
			continue
		}
		tag.encode(mut dst)!
		assert dst == c.exp
	}
}
