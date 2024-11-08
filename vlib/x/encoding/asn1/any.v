// Copyright (c) 2022, 2023 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

// ANY DEFINED BY
//
@[noinit]
pub struct Any {
mut:
	marker string = 'any'
	params Element
}

// Any.new creates a new ANY DEFINED BY element with marker as an identifier.
pub fn Any.new(marker string, params Element) Any {
	return Any{marker, params}
}

fn Any.decode(bytes []u8) !Any {
	return error('not implemented')
}

// The tag of underlying ANY DEFINED BY element.
pub fn (a Any) tag() Tag {
	return a.params.tag()
}

// The payload of underlying ANY DEFINED BY element.
pub fn (a Any) payload() ![]u8 {
	return a.params.payload()!
}

fn (a Any) params() Element {
	return a.params
}
