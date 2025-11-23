// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
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
	VAssertMetaInfo.free
	__new_array
	new_array_from_c_array
	new_array_from_c_array_no_alloc
	...
*/

// false: whitelist function
// true: blacklist function
const blacklist = {
	'main.main':             false
	'c_error_number_str':    false
	'exit':                  false
	'gc_is_enabled':         false
	'int_max':               false
	'int_min':               false
	'flush_stdout':          false
	'flush_stderr':          false
	'print_character':       false
	'u8.is_alnum':           false
	'u8.is_bin_digit':       false
	'u8.is_capital':         false
	'u8.is_digit':           false
	'u8.is_hex_digit':       false
	'u8.is_letter':          false
	'u8.is_oct_digit':       false
	'u8.is_space':           false
	'string.is_capital':     false
	'string.is_ascii':       false
	'string.is_identifier':  false
	'string.is_blank':       false
	'string.indent_width':   false
	'string.index_u8':       false
	'string.last_index':     true
	'string.last_index_u8':  false
	'string.contains_u8':    false
	'malloc_noscan':         false
	'malloc':                false
	'is_nil':                false
	'memdup':                false
	'vcalloc':               false
	'vmemcpy':               false
	'eprint':                false
	'eprintln':              false
	'_write_buf_to_fd':      false
	'_writeln_to_fd':        false
	'_memory_panic':         false
	'panic':                 false
	'vcurrent_hash':         false
	'__new_array':           false
	'panic_on_negative_len': false
	'panic_on_negative_cap': false
	'panic_n':               false
	'impl_i64_to_string':    false
	'vmemmove':              false
	'tos':                   false
	'strings.new_builder':   true
	'new_array_from_c_array': false
}

const windows_blacklist = {
	'main.main':            false
	'c_error_number_str':   false
	'exit':                 false
	'gc_is_enabled':        false
	'int_max':              false
	'int_min':              false
	'u8.is_alnum':          false
	'u8.is_bin_digit':      false
	'u8.is_capital':        false
	'u8.is_digit':          false
	'u8.is_hex_digit':      false
	'u8.is_letter':         false
	'u8.is_oct_digit':      false
	'u8.is_space':          false
	'string.is_capital':    false
	'string.is_ascii':      false
	'string.is_identifier': false
	'string.is_blank':      false
	'string.indent_width':  false
	'string.index_u8':      false
	'string.last_index':    true
	'string.last_index_u8': false
	'string.contains_u8':   false
}

fn (g &Gen) is_blacklisted(name string, is_builtin bool) bool {
	if g.pref.os == .windows {
		return windows_blacklist[name] or { is_builtin }
	} else {
		return blacklist[name] or { is_builtin }
	}
}
