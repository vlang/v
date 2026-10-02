module json5

import os
import strconv

// parse_text parses the JSON5 document in `text` and returns a `Doc`.
pub fn parse_text(text string) !Doc {
	mut p := new_parser(text)
	return Doc{
		root: p.parse()!
	}
}

// parse_file reads and parses the JSON5 document at `path`.
pub fn parse_file(path string) !Doc {
	return parse_text(os.read_file(path) or {
		return error('json5: could not read `${path}`: ${err.msg()}')
	})
}

// parse parses the JSON5 document in `text` and returns its root `Any`. It is
// the shortest path from text to a dynamic value.
pub fn parse(text string) !Any {
	doc := parse_text(text)!
	return doc.root
}

// decode parses the JSON5 document in `text` and decodes it into `T`.
//
// Supported targets are structs, enums, `map[string]V`, arrays (dynamic and
// fixed), options and all scalar types, plus the `Any` type itself. Struct
// fields are matched by name, or by an `@[json5: 'name']` attribute when the
// document key differs; `@[skip]` leaves a field at its default value. An
// embedded struct is flattened, so its fields are read from the enclosing
// object.
//
// A type may customise decoding by defining one of:
//
//   - `from_json5(value Any)`, to decode the whole value
//   - `from_json5_string(raw string) !`, `from_json5_number(raw string) !` or
//     `from_json5_boolean(value bool)`, to decode a single scalar
//
// A missing key leaves the field at its default. An explicit `null` clears an
// option field and leaves other fields at their default.
pub fn decode[T](text string) !T {
	return decode_any[T](parse(text)!)
}

// decode_file reads the JSON5 document at `path` and decodes it into `T`.
pub fn decode_file[T](path string) !T {
	return decode[T](os.read_file(path) or {
		return error('json5: could not read `${path}`: ${err.msg()}')
	})
}

// decode_any converts a parsed `Any` into `T`, applying the same rules as
// `decode` without re-parsing.
pub fn decode_any[T](value Any) !T {
	mut out := T{}
	decode_into(value, mut out)!
	return out
}

// decode_into decodes `value` into `mut out`, reporting an error when the value
// does not fit.
//
// It is the single recursive entry point of the decoder. Container types are
// handled by helpers that take the existing container as a parameter, because
// a comptime-derived element type such as `T.elem_type` cannot be used as a
// generic argument or a local variable type in this compiler.
fn decode_into[T](value Any, mut out T) ! {
	// The hooks are checked first so that a type can override decoding
	// regardless of the shape it would otherwise take.
	$for method in T.methods {
		$if method.name == 'from_json5' {
			out.$method(value)
			return
		} $else $if method.name == 'from_json5_string' {
			if value is string {
				out.$method(value) or { return err }
				return
			}
		} $else $if method.name == 'from_json5_number' {
			if value is Number {
				out.$method(value.text) or { return err }
				return
			}
		} $else $if method.name == 'from_json5_boolean' {
			if value is bool {
				out.$method(value)
				return
			}
		} $else $if method.name == 'from_json5_null' {
			if value is Null {
				out.$method()
				return
			}
		}
	}
	$if T is Any {
		out = value
	} $else $if T is $option {
		// A `null` or an absent value leaves the option empty.
		if value is Null {
			return
		}
		mut unwrapped := $zero(T.payload_type)
		decode_into(value, mut unwrapped)!
		out = unwrapped
	} $else $if T is bool {
		out = decode_bool[T](value)!
	} $else $if T is string {
		out = decode_string[T](value)!
	} $else $if T is $enum {
		out = decode_enum[T](value)!
	} $else $if T is $int {
		out = decode_int[T](value)!
	} $else $if T is $float {
		out = decode_float[T](value)!
	} $else $if T.unaliased_typ is $map {
		out = decode_map(value, out)!
	} $else $if T.unaliased_typ is $array_fixed {
		elems := decode_array(value, out[..])!
		for i, elem in elems {
			if i >= out.len {
				break
			}
			out[i] = elem
		}
	} $else $if T.unaliased_typ is $array_dynamic {
		out = decode_array(value, out)!
	} $else $if T is $struct {
		decode_struct_into(value, mut out)!
	} $else {
		return error('json5: cannot decode into `${T.name}`')
	}
}

// decode_struct_into decodes an object into the fields of `mut out`.
fn decode_struct_into[T](value Any, mut out T) ! {
	obj := match value {
		map[string]Any { value }
		else { return type_error(value, 'an object') }
	}
	// `$for` unrolls to straight-line code, so the field logic is expressed as
	// nested `if`s instead of `continue`.
	$for field in T.fields {
		key := field_key_name(field.name, field.attrs)
		if key != '' {
			$if field.is_embed {
				// Flattened: the embedded struct's fields live in the same object.
				$if field.unaliased_typ is $struct {
					mut embedded := $zero(field.typ)
					decode_struct_into(value, mut embedded)!
					out.$(field.name) = embedded
				}
			} $else {
				// A missing key, and an explicit `null`, both leave the field
				// alone: an option field stays `none` and everything else keeps
				// its default.
				if item := obj[key] {
					if item !is Null {
						mut slot := $zero(field.typ)
						decode_into(item, mut slot) or { return err }
						out.$(field.name) = slot
					}
				}
			}
		}
	}
}

// decode_map decodes an object into a `map[string]V`. Only string keys are
// supported, which is what a JSON5 document can express.
fn decode_map[E](value Any, current map[string]E) !map[string]E {
	obj := match value {
		map[string]Any { value }
		Null { return current }
		else { return type_error(value, 'an object') }
	}
	mut decoded := map[string]E{}
	for key, item in obj {
		mut slot := E{}
		decode_into(item, mut slot) or { continue }
		decoded[key] = slot
	}
	return decoded
}

// decode_array decodes an array into a slice. The `current` parameter exists
// only to let the caller name the element type, which this compiler cannot
// derive from `T.elem_type`. An element that does not fit its slot is skipped
// rather than failing the whole array.
fn decode_array[E](value Any, _current []E) ![]E {
	items := match value {
		[]Any { value }
		Null { return []E{} }
		else { return type_error(value, 'an array') }
	}
	mut decoded := []E{cap: items.len}
	for item in items {
		mut slot := E{}
		decode_into(item, mut slot) or { continue }
		decoded << slot
	}
	return decoded
}

// decode_bool converts `value` to a `bool`.
fn decode_bool[T](value Any) !T {
	$for method in T.methods {
		$if method.name == 'from_json5_boolean' {
			raw := match value {
				bool { value }
				string { value.bool() }
				Number { value.f64() != 0.0 }
				else { return type_error(value, 'a boolean') }
			}
			mut decoded := T(false)
			decoded.$method(raw)
			return decoded
		}
	}
	return match value {
		bool { value }
		string { value.bool() }
		Number { value.f64() != 0.0 }
		else { return type_error(value, 'a boolean') }
	}
}

// decode_string converts `value` to a `string`.
fn decode_string[T](value Any) !T {
	$for method in T.methods {
		$if method.name == 'from_json5_string' {
			raw := match value {
				string { value }
				Number { value.text }
				bool { value.str() }
				Null { '' }
				else { return type_error(value, 'a string') }
			}
			mut decoded := T('')
			decoded.$method(raw) or { return err }
			return decoded
		}
	}
	return match value {
		string { value }
		Number { value.text }
		bool { value.str() }
		else { return type_error(value, 'a string') }
	}
}

// decode_int converts `value` to the integer target `T`, signed or unsigned.
fn decode_int[T](value Any) !T {
	$for method in T.methods {
		$if method.name == 'from_json5_number' {
			raw := match value {
				Number { value.text }
				else { return type_error(value, 'a number') }
			}
			mut decoded := T(0)
			decoded.$method(raw) or { return err }
			return decoded
		}
	}
	return match value {
		Number { number_to_int[T](value) or { return type_error(value, 'an integer') } }
		f64 { T(value) }
		bool {
			if value { T(1) } else { T(0) }
		}
		string {
			parsed := strconv.parse_int(value, 10, 64) or {
				return type_error(value, 'an integer')
			}
			T(parsed)
		}
		else { return type_error(value, 'an integer') }
	}
}

// number_to_int converts `n` to the integer type `T`, rejecting values outside
// its range. V integer conversions are unchecked, so a bare `T(n)` would
// silently wrap.
fn number_to_int[T](n Number) ?T {
	v := n.i64()
	$if T is u8 || T is u16 || T is u32 || T is u64 || T is usize || T is u128 {
		if v < 0 {
			return none
		}
	}
	$if T is u8 {
		if v > 255 {
			return none
		}
	} $else $if T is u16 {
		if v > 65535 {
			return none
		}
	} $else $if T is u32 {
		if v > 4294967295 {
			return none
		}
	} $else $if T is i8 {
		if v < -128 || v > 127 {
			return none
		}
	} $else $if T is i16 {
		if v < -32768 || v > 32767 {
			return none
		}
	} $else $if T is i32 || T is int {
		if v < -2147483648 || v > 2147483647 {
			return none
		}
	}
	return T(v)
}

// decode_float converts `value` to the float target `T`.
fn decode_float[T](value Any) !T {
	$for method in T.methods {
		$if method.name == 'from_json5_number' {
			raw := match value {
				Number { value.text }
				else { return type_error(value, 'a number') }
			}
			mut decoded := T(0)
			decoded.$method(raw) or { return err }
			return decoded
		}
	}
	return match value {
		Number { T(value.f64()) }
		f64 { T(value) }
		bool {
			if value { T(1.0) } else { T(0.0) }
		}
		string {
			parsed := strconv.atof64(value, strconv.AtoF64Param{}) or {
				return type_error(value, 'a number')
			}
			T(parsed)
		}
		else { return type_error(value, 'a number') }
	}
}

// decode_enum converts `value` to the enum target `T`. Both the variant name
// and its integer value are accepted, and a `@[json5: 'name']` attribute on a
// variant overrides the document spelling.
fn decode_enum[T](value Any) !T {
	$for method in T.methods {
		$if method.name == 'from_json5_number' {
			raw := match value {
				Number { value.text }
				else { return type_error(value, 'an enum variant') }
			}
			mut decoded := T(0)
			decoded.$method(raw) or { return err }
			return decoded
		}
	}
	$if T.unaliased_typ is $enum {
		name := ''
		match value {
			string {
				name = value
			}
			Number {
				n := value.i64()
				$for variant in T.values {
					if int(variant.value) == int(n) {
						name = variant.name
					}
				}
				if name == '' {
					return &EnumError{
						enum_name: T.name
						value:     value.text
					}
				}
			}
			bool {
				name = value.str()
			}
			else {
				return type_error(value, 'an enum variant')
			}
		}
		mut result := T(0)
		mut found := false
		$for variant in T.values {
			// A `@[json5: 'name']` attribute overrides the document spelling.
			spelling := variant.name
			for attr in variant.attrs {
				if attr.starts_with('json5:') {
					spelling = unquote(attr.all_after(':').trim_space())
				}
			}
			if spelling == name {
				result = variant.value
				found = true
			}
		}
		if !found {
			return &EnumError{
				enum_name: T.name
				value:     name
			}
		}
		return result
	} $else {
		return type_error(value, 'an enum variant')
	}
}
