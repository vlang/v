module main

// namer turns a proto identifier into the V identifier the generator writes.
//
// Two rules do all the work, and both exist because the two languages disagree
// about how a name is spelled rather than about what it means:
//
//   * A message or enum name becomes PascalCase, so `GetRequest` and
//     `get_request` both arrive as `GetRequest` and a V reader is not asked to
//     hold two names for one type.
//   * A field name keeps its own spelling, because proto3 already uses
//     snake_case and V's own convention is snake_case. Renaming a field would
//     only obscure which field on the wire it is.

// v_keywords are V's reserved words. A proto field or message named after one
// of these has to be escaped, or the generated file does not compile.
const v_keywords = [
	'as',
	'asm',
	'assert',
	'atomic',
	'break',
	'const',
	'continue',
	'defer',
	'else',
	'enum',
	'false',
	'for',
	'fn',
	'go',
	'goto',
	'if',
	'import',
	'in',
	'interface',
	'is',
	'lock',
	'match',
	'module',
	'mut',
	'none',
	'null',
	'or',
	'pub',
	'return',
	'rlock',
	'select',
	'share',
	'sizeof',
	'spawn',
	'static',
	'struct',
	'type',
	'typeof',
	'unsafe',
	'volatile',
	'__global',
]

// is_v_keyword reports whether `name` is a V reserved word.
pub fn is_v_keyword(name string) bool {
	return name in v_keywords
}

// to_upper returns the uppercase form of an ASCII letter, and anything else
// unchanged. V3 has no `u8.to_ascii_upper`, and a name is the only place this
// is needed, so the two helpers live here.
pub fn to_upper(b u8) u8 {
	return if b >= `a` && b <= `z` { b - 32 } else { b }
}

// to_lower returns the lowercase form of an ASCII letter, and anything else
// unchanged.
pub fn to_lower(b u8) u8 {
	return if b >= `A` && b <= `Z` { b + 32 } else { b }
}

// is_upper_ascii reports whether `b` is an uppercase ASCII letter.
pub fn is_upper_ascii(b u8) bool {
	return b >= `A` && b <= `Z`
}

// is_lower_ascii reports whether `b` is a lowercase ASCII letter.
pub fn is_lower_ascii(b u8) bool {
	return b >= `a` && b <= `z`
}

// pascal_case returns `name` in PascalCase: `get_request` and `GetRequest` both
// give `GetRequest`, and a name that is already PascalCase is unchanged.
pub fn pascal_case(name string) string {
	mut out := []u8{}
	mut upper := true
	for c in name {
		if c == `_` {
			// An underscore separates words, so the next letter is capitalised.
			upper = true
			continue
		}
		if upper {
			out << to_upper(c)
			upper = false
		} else {
			out << c
		}
	}
	return out.bytestr()
}

// snake_case returns `name` in snake_case: `GetRequest` gives `get_request`. A
// name that is already snake_case is unchanged.
pub fn snake_case(name string) string {
	mut out := []u8{}
	mut prev_was_lower := false
	for c in name {
		if c == `_` {
			if out.len > 0 && out[out.len - 1] != `_` {
				out << `_`
			}
			prev_was_lower = false
			continue
		}
		is_upper := is_upper_ascii(c)
		if is_upper && prev_was_lower {
			// A lower-to-upper transition is a word boundary: `GetRequest`
			// splits as `Get` and `Request`, not `Ge` and `tRequest`.
			out << `_`
		}
		out << to_lower(c)
		prev_was_lower = is_lower_ascii(c)
	}
	return out.bytestr()
}

// safe_field_name returns a V field name for a proto field called `name`.
//
// A V keyword is suffixed rather than escaped, because V has no identifier
// escape: `type` becomes `type_`. The suffix is a compile error the generator
// cannot cause by accident, because every reserved word gets one.
pub fn safe_field_name(name string) string {
	if is_v_keyword(name) {
		return '${name}_'
	}
	return name
}

// safe_type_name returns a V type name for a proto message or enum called
// `name`, with a keyword collision handled the same way as a field.
pub fn safe_type_name(name string) string {
	// The pascal_case form is checked, not the proto name: a type name is
	// capitalised, so a lowercase keyword like `type` cannot collide and must
	// not be escaped into `Type_`. Only a capitalised V keyword is a real
	// collision, and there is no reason for a schema to contain one.
	// The local is not named `pascal`: that is a C++ keyword, and the identifier
	// reaches the generated C verbatim.
	out := pascal_case(name)
	if is_v_keyword(out) {
		return '${out}_'
	}
	return out
}

// method_name returns the V function name for a message's codec, e.g.
// `GetRequest` gives `get_request`.
pub fn method_name(name string) string {
	return snake_case(name)
}

// zero_value returns an expression for the zero value of a V type, in the form
// that can be assigned to a `mut` local.
//
// This cannot be `T{}` for a scalar. V parses `i32{}` as a composite literal
// for an unknown type and reports `undefined variable: i32`, so a generated
// scalar local has to be initialised through a conversion. A composite type
// keeps its literal form, which is what `string{}` and `map[K]V{}` need.
pub fn zero_value(v_type string) string {
	return match v_type {
		'i8', 'i16', 'int', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'isize',
		'usize', 'rune' {
			'${v_type}(0)'
		}
		'f32' { 'f32(0)' }
		'f64' { 'f64(0)' }
		'bool' { 'false' }
		'string' { "''" }
		else { '${v_type}{}' }
	}
}

// join_package flattens a dotted proto package into the prefix a flattened V
// name uses: `google.rpc` gives `GoogleRpc`, so a message `Status` in it becomes
// `GoogleRpcStatus`.
//
// V has no nested types, so `Outer.Inner` has to flatten too. Flattening the
// package as well as the message chain keeps two same-named messages in
// different packages from colliding.
pub fn join_package(parts []string, name string) string {
	mut out := []u8{}
	for p in parts {
		out << pascal_case(p).bytes()
	}
	if out.len > 0 {
		out << safe_type_name(name).bytes()
		return out.bytestr()
	}
	return safe_type_name(name)
}
