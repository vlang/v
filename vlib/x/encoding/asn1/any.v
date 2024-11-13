// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

// ANY DEFINED BY
//
pub struct AnyDefinedBy {
pub:
	// default to null element
	params Element = Null{}
}

// AnyDefinedBy.new creates a new ANY DEFINED BY element.
pub fn AnyDefinedBy.new(params Element) AnyDefinedBy {
	return AnyDefinedBy{params}
}

// tag returns the underlying tag of ANY DEFINED BY element.
pub fn (a AnyDefinedBy) tag() Tag {
	return a.params.tag()
}

// payload returns the underlying payload of ANY DEFINED BY element.
pub fn (a AnyDefinedBy) payload() ![]u8 {
	return a.params.payload()!
}
