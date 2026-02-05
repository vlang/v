// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// BEAM backend JSON module
// On BEAM, V JSON operations map to Erlang's json module (OTP 27+)
// or fall back to a pure Erlang implementation
module json

// Any represents a dynamically typed JSON value
// On BEAM: represented as Erlang term (maps, lists, binaries, numbers, atoms)
pub type Any = Null
	| []Any
	| bool
	| f64
	| i64
	| int
	| map[string]Any
	| string

// Null represents JSON null value
// On BEAM: represented as atom 'null'
pub struct Null {}

// null is a constant null value for comparisons
pub const null = Null{}

// decode decodes a JSON string into a V type T
// On BEAM: Uses Erlang json:decode/1 (OTP 27+) or custom decoder
// Codegen: vbeam_json:decode(TypeMeta, JsonBinary)
pub fn decode[T](s string) !T {
	// Compiler implementation - the actual codegen generates Erlang code
	// that decodes JSON into the target V struct/type
	//
	// For BEAM, this becomes:
	//   case vbeam_json:decode(TypeInfo, Binary) of
	//       {ok, Value} -> Value;
	//       {error, Reason} -> error(Reason)
	//   end
	return error('json.decode: not implemented at runtime (compiler builtin)')
}

// decode tries to decode the provided JSON string, into a V structure.
// This is the two-argument form that takes type info and string.
// On BEAM: vbeam_json:decode(TypeMeta, Binary)
pub fn decode_with_type(typ voidptr, s string) !voidptr {
	// Compiler implementation - this signature matches what the compiler expects
	return error('json.decode: not implemented at runtime (compiler builtin)')
}

// encode serializes the provided V value as a JSON string
// On BEAM: Uses Erlang json:encode/1 (OTP 27+) or custom encoder
// Codegen: vbeam_json:encode(Value)
pub fn encode[T](val T) string {
	// Compiler implementation - generates Erlang code to encode V types to JSON
	//
	// For BEAM, this becomes:
	//   vbeam_json:encode(VValue)
	//
	// Which converts V structs (Erlang maps) to JSON strings
	return ''
}

// encode_pretty serializes the provided V value as a formatted JSON string
// On BEAM: vbeam_json:encode_pretty(Value)
pub fn encode_pretty[T](val T) string {
	// Compiler implementation - generates formatted JSON output
	return ''
}

// raw_decode decodes JSON string to a dynamic Any type
// On BEAM: json:decode(Binary) returning maps/lists/atoms
pub fn raw_decode(s string) !Any {
	return error('json.raw_decode: not implemented at runtime')
}

// fast_raw_decode is like raw_decode but optimized for performance
pub fn fast_raw_decode(s string) !Any {
	return raw_decode(s)
}

// ============================================================================
// Internal decode helpers (used by compiler-generated code)
// ============================================================================

// decode_int extracts an int from a JSON value
// On BEAM: Erlang integer from decoded JSON
@[markused]
fn decode_int(root Any) int {
	if root is int {
		return root
	}
	if root is i64 {
		return int(root)
	}
	if root is f64 {
		return int(root)
	}
	if root is string {
		return root.int()
	}
	return 0
}

@[markused]
fn decode_i8(root Any) i8 {
	return i8(decode_int(root))
}

@[markused]
fn decode_i16(root Any) i16 {
	return i16(decode_int(root))
}

@[markused]
fn decode_i64(root Any) i64 {
	if root is i64 {
		return root
	}
	if root is int {
		return i64(root)
	}
	if root is f64 {
		return i64(root)
	}
	if root is string {
		return root.i64()
	}
	return 0
}

@[markused]
fn decode_u8(root Any) u8 {
	return u8(decode_int(root))
}

@[markused]
fn decode_u16(root Any) u16 {
	return u16(decode_int(root))
}

@[markused]
fn decode_u32(root Any) u32 {
	return u32(decode_int(root))
}

@[markused]
fn decode_u64(root Any) u64 {
	if root is i64 {
		return u64(root)
	}
	if root is int {
		return u64(root)
	}
	if root is f64 {
		return u64(root)
	}
	return 0
}

@[markused]
fn decode_f32(root Any) f32 {
	if root is f64 {
		return f32(root)
	}
	if root is int {
		return f32(root)
	}
	if root is i64 {
		return f32(root)
	}
	if root is string {
		return root.f32()
	}
	return 0.0
}

@[markused]
fn decode_f64(root Any) f64 {
	if root is f64 {
		return root
	}
	if root is int {
		return f64(root)
	}
	if root is i64 {
		return f64(root)
	}
	if root is string {
		return root.f64()
	}
	return 0.0
}

@[markused]
fn decode_string(root Any) string {
	if root is string {
		return root
	}
	return ''
}

@[markused]
fn decode_bool(root Any) bool {
	if root is bool {
		return root
	}
	if root is string {
		return root == 'true'
	}
	return false
}

// ============================================================================
// Any type methods (dynamic JSON access)
// ============================================================================

// str returns the string representation of an Any value
pub fn (a Any) str() string {
	match a {
		Null {
			return 'null'
		}
		bool {
			return if a { 'true' } else { 'false' }
		}
		int {
			// Convert int to string without string interpolation to avoid recursion
			mut val := a
			if val == 0 {
				return '0'
			}
			mut s := ''
			mut neg := false
			if val < 0 {
				neg = true
				val = -val
			}
			for val > 0 {
				s = '${u8(val % 10 + 48)}' + s
				val = val / 10
			}
			return if neg { '-' + s } else { s }
		}
		i64 {
			// Convert i64 to string without recursion
			mut val := a
			if val == 0 {
				return '0'
			}
			mut s := ''
			mut neg := false
			if val < 0 {
				neg = true
				val = -val
			}
			for val > 0 {
				s = '${u8(val % 10 + 48)}' + s
				val = val / 10
			}
			return if neg { '-' + s } else { s }
		}
		f64 {
			// For now, return a simple representation
			// Full float-to-string conversion would require more complex code
			return '<f64>'
		}
		string {
			return a
		}
		[]Any {
			// Would need proper JSON encoding
			return '[]'
		}
		map[string]Any {
			// Would need proper JSON encoding
			return '{}'
		}
	}
}

// int returns the Any value as int
pub fn (a Any) int() int {
	return decode_int(a)
}

// i64 returns the Any value as i64
pub fn (a Any) i64() i64 {
	return decode_i64(a)
}

// f64 returns the Any value as f64
pub fn (a Any) f64() f64 {
	return decode_f64(a)
}

// bool returns the Any value as bool
pub fn (a Any) bool() bool {
	return decode_bool(a)
}

// as_array returns the Any value as array of Any
pub fn (a Any) as_array() []Any {
	if a is []Any {
		return a
	}
	return []
}

// as_map returns the Any value as map
pub fn (a Any) as_map() map[string]Any {
	if a is map[string]Any {
		return a
	}
	return {}
}

// ============================================================================
// Null type methods
// ============================================================================

// str returns "null"
pub fn (n Null) str() string {
	return 'null'
}
