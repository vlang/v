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

@[direct_array_access]
fn __as_cast(obj voidptr, obj_type int, expected_type int) voidptr {
	if obj_type != expected_type {
		mut obj_name := as_cast_type_indexes[0].tname.clone()
		mut expected_name := as_cast_type_indexes[0].tname.clone()
		for x in as_cast_type_indexes {
			if x.tindex == obj_type {
				obj_name = x.tname.clone()
			}
			if x.tindex == expected_type {
				expected_name = x.tname.clone()
			}
		}
		panic('as cast: cannot cast `' + obj_name + '` to `' + expected_name + '`')
	}
	return obj
}
