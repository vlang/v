// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

// this is a TEMPORARY system used to enable/disable parts of the
// builtin module until more of `builtin` is supported by native

/*
already compiling functions:
	string.+	
	string.clone
	string.free
	ArrayFlags.set
	ArrayFlags.clear
	u8.vstring_with_len
	Error.msg
	Error.code
	MessageError.code
	MessageError.free
	u64.hex
	VAssertMetaInfo.free"
	__new_array
	new_array_from_c_array
	new_array_from_c_array_no_alloc
	...
*/

// false: whitelist function
// true: blacklist function
const whitelist = {
	'main.main': false
}

fn (g Gen) is_blacklisted(name string, is_builtin bool) bool {
	return native.whitelist[name] or { is_builtin }
}
