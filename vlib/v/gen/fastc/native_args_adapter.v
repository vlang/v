module fastc

import strings

// fastc_native_register_args is the number of integer, and separately floating-point,
// argument registers of the arm64 calling convention.
const fastc_native_register_args = 8

// FastcNativeArgsAdapter describes a native bridge function whose arguments do not all
// fit in registers. TinyCC does not lay out arm64 stack arguments as Apple's ABI packs
// them, so the TinyCC program passes such a call's arguments in one struct, and a
// platform-compiled adapter makes the call.
pub struct FastcNativeArgsAdapter {
pub:
	name        string
	return_type string
	// parameter_types are plain C types; a pointer of any kind is `void*`.
	parameter_types []string
}

// fastc_native_args_adapter returns the adapter that the native bridge function `name`,
// declared by `prototype` (see fastc_native_function_declaration), needs on arm64 macOS.
// A call whose arguments fit in registers, or that passes a function or a variable
// argument list, keeps its direct call. A struct counts as two integer and four
// floating-point registers, the most it can take; an unneeded adapter only adds a call.
pub fn fastc_native_args_adapter(name string, prototype string) ?FastcNativeArgsAdapter {
	head := name + '('
	start := prototype.index(head) or { return none }
	if !prototype.ends_with(');') {
		return none
	}
	parameters := prototype[start + head.len..prototype.len - 2]
	if parameters.contains('(') || parameters.contains('...') {
		return none
	}
	return_type := fastc_native_args_c_type(prototype[..start].trim_space()) or { return none }
	mut parameter_types := []string{}
	mut integer_args := 0
	mut float_args := 0
	if parameters.trim_space() != 'void' {
		for parameter in parameters.split(',') {
			typ := fastc_native_args_c_type(parameter.trim_space()) or { return none }
			if typ == 'void' {
				return none
			}
			if typ in ['float', 'double'] {
				float_args++
			} else if fastc_native_args_is_scalar(typ) {
				integer_args++
			} else {
				integer_args += 2
				float_args += 4
			}
			parameter_types << typ
		}
	}
	if integer_args <= fastc_native_register_args && float_args <= fastc_native_register_args {
		return none
	}
	return FastcNativeArgsAdapter{
		name:            name
		return_type:     return_type
		parameter_types: parameter_types
	}
}

// fastc_native_args_c_type maps a V-declared C binding type to a C type of the same
// size and class, or none for a type passed in some other way.
fn fastc_native_args_c_type(text string) ?string {
	typ := text.trim_string_left('const ').trim_space()
	if typ.ends_with('*') || typ in ['voidptr', 'byteptr', 'charptr'] {
		return 'void*'
	}
	match typ {
		'void' { return 'void' }
		'bool' { return '_Bool' }
		'i8' { return 'signed char' }
		'u8', 'byte' { return 'unsigned char' }
		'char' { return 'char' }
		'i16' { return 'short' }
		'u16' { return 'unsigned short' }
		'int', 'i32' { return 'int' }
		'u32', 'rune' { return 'unsigned int' }
		'i64' { return 'long long' }
		'u64' { return 'unsigned long long' }
		'isize' { return 'long' }
		'usize' { return 'unsigned long' }
		'f32' { return 'float' }
		'f64' { return 'double' }
		else {}
	}
	// A struct keeps its V name: both the program and the bridge define it.
	if typ.len > 0 && (typ[0].is_letter() || typ[0] == `_`)
		&& typ.bytes().all(it.is_letter() || it.is_digit() || it == `_`) {
		return typ
	}
	return none
}

fn fastc_native_args_is_scalar(typ string) bool {
	return typ in ['void*', '_Bool', 'signed char', 'unsigned char', 'char', 'short', 'unsigned short',
		'int', 'unsigned int', 'long long', 'unsigned long long', 'long', 'unsigned long', 'float',
		'double']
}

fn (adapter FastcNativeArgsAdapter) args_struct() string {
	mut out := strings.new_builder(64 + adapter.parameter_types.len * 16)
	out.write_string('typedef struct {')
	for i, typ in adapter.parameter_types {
		out.write_string(' ${typ} a${i};')
	}
	out.write_string(' } v3_native_args_${adapter.name};\n')
	return out.str()
}

// native_side defines the adapter in the platform-compiled bridge. It calls the native
// function through a pointer of the declared argument types, which have the sizes of the
// native ones, so no conversion of an Objective-C argument is involved.
pub fn (adapter FastcNativeArgsAdapter) native_side() string {
	mut out := strings.new_builder(256)
	out.write_string(adapter.args_struct())
	mut arguments := []string{cap: adapter.parameter_types.len}
	for i, _ in adapter.parameter_types {
		arguments << 'a->a${i}'
	}
	parameters := if adapter.parameter_types.len == 0 {
		'void'
	} else {
		adapter.parameter_types.join(', ')
	}
	call := '((${adapter.return_type} (*)(${parameters}))&${adapter.name})(${arguments.join(', ')})'
	out.write_string('static ${adapter.return_type} v3_native_adapter_${adapter.name}(v3_native_args_${adapter.name} *a) { ')
	if adapter.return_type == 'void' {
		out.write_string('${call};')
	} else {
		out.write_string('return ${call};')
	}
	out.write_string(' }\n')
	out.write_string('${adapter.return_type} (*v3_native_bridge_args_${adapter.name})(v3_native_args_${adapter.name} *) = &v3_native_adapter_${adapter.name};\n')
	return out.str()
}

// program_side routes the TinyCC program's calls of the bridge pointer through the
// adapter. A use of the pointer that is not a call keeps the direct pointer.
pub fn (adapter FastcNativeArgsAdapter) program_side() string {
	mut out := strings.new_builder(256)
	out.write_string(adapter.args_struct())
	out.write_string('extern ${adapter.return_type} (*v3_native_bridge_args_${adapter.name})(v3_native_args_${adapter.name} *);\n')
	out.write_string('#define v3_native_bridge_${adapter.name}(...) v3_native_bridge_args_${adapter.name}(&(v3_native_args_${adapter.name}){__VA_ARGS__})\n')
	return out.str()
}
