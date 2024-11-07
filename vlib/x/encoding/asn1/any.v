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

pub fn Any.new(marker string, params Element) Any {
	return Any{marker, params}
}

fn Any.decode(bytes []u8) !Any {
	return error('not implemented')
}

pub fn (a Any) tag() Tag {
	return a.params.tag()
}

pub fn (a Any) payload() ![]u8 {
	return a.params.payload()!
}

fn (a Any) params() Element {
	return a.params
}
