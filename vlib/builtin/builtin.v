// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
@[has_globals]
module builtin

// isnil returns true if an object is nil (only for C objects).
@[inline]
pub fn isnil(v voidptr) bool {
	return v == 0
}

struct VCastTypeIndexName {
	tindex int
	tname  string
}

// will be filled in cgen
__global as_cast_type_indexes []VCastTypeIndexName

// SliceIndex describes one overloaded `[]` index or slice part.
pub struct SliceIndex {
pub:
	is_range bool
	value    int
	low      int
	high     int
	has_low  bool
	has_high bool
}

@[direct_array_access; markused]
fn __as_cast(obj voidptr, obj_type int, expected_type int, obj_name string, expected_name string) voidptr {
	if obj_type != expected_type {
		panic('as cast: cannot cast `' + obj_name + '` to `' + expected_name + '`')
	}
	return obj
}

@[inline]
fn __v2_flag_has_int(receiver int, flag int) bool {
	return (receiver & flag) != 0
}

@[inline]
fn __v2_flag_all_int(receiver int, flags int) bool {
	return (receiver & flags) == flags
}
