module flat

// named_variant_marker joins a sum type name and one of its named variants in
// the name of the hidden struct that stores that variant, e.g. `Expr@variant@Count`
// for `Expr.Count` in `type Expr = Count(int) | Void`. `@` cannot appear inside a
// source identifier, so a user type can neither collide with nor name the
// hidden struct.
pub const named_variant_marker = '@variant@'

// named_variant_payload_field is the field of a hidden named variant struct that
// stores the variant payload. Payload-less variants have no fields.
pub const named_variant_payload_field = 'payload'

// named_variant_type_name returns the hidden struct name for `variant` of the sum
// type `sum`. `sum` may be module qualified; generic arguments are not included.
pub fn named_variant_type_name(sum string, variant string) string {
	return '${sum}${named_variant_marker}${variant}'
}

// is_named_variant_type_name reports whether `name` mentions a hidden named variant struct.
@[inline]
pub fn is_named_variant_type_name(name string) bool {
	return name.contains(named_variant_marker)
}

// decode_named_variant_type_name splits a hidden named variant struct name such
// as `mod.Expr@variant@Count` or `Opt@variant@Some[int]` into the owning sum type
// (`mod.Expr`, `Opt[int]`) and the variant name (`Count`, `Some`).
pub fn decode_named_variant_type_name(name string) ?(string, string) {
	mut clean := name
	for clean.starts_with('&') {
		clean = clean[1..]
	}
	marker := clean.index(named_variant_marker) or { return none }
	variant_start := marker + named_variant_marker.len
	if marker == 0 || variant_start >= clean.len {
		return none
	}
	mut variant_end := variant_start
	for variant_end < clean.len && is_named_variant_ident_char(clean[variant_end]) {
		variant_end++
	}
	sum := clean[..marker]
	variant := clean[variant_start..variant_end]
	args := clean[variant_end..]
	return sum + args, variant
}

// named_variant_display_name returns the source spelling (`Expr.Count`) of a
// hidden named variant struct name, or `name` itself for any other name.
pub fn named_variant_display_name(name string) string {
	if !name.contains(named_variant_marker) {
		return name
	}
	return demangle_named_variants(name)
}

// demangle_named_variants replaces every hidden named variant struct name in
// `text` with its source spelling, so diagnostics and printed values show
// `Expr.Count` and `Opt[int].Some` instead of internal names.
pub fn demangle_named_variants(text string) string {
	if !text.contains(named_variant_marker) {
		return text
	}
	mut out := []u8{cap: text.len}
	mut i := 0
	for i < text.len {
		marker := text.index_after(named_variant_marker, i) or {
			out << text[i..].bytes()
			break
		}
		out << text[i..marker].bytes()
		mut j := marker + named_variant_marker.len
		variant_start := j
		for j < text.len && is_named_variant_ident_char(text[j]) {
			j++
		}
		variant := text[variant_start..j]
		// Generic arguments of the variant struct belong to the sum type.
		if j < text.len && text[j] == `[` {
			mut depth := 0
			args_start := j
			for j < text.len {
				if text[j] == `[` {
					depth++
				} else if text[j] == `]` {
					depth--
					if depth == 0 {
						j++
						break
					}
				}
				j++
			}
			out << demangle_named_variants(text[args_start..j]).bytes()
		}
		out << `.`
		out << variant.bytes()
		i = j
	}
	return out.bytestr()
}

@[inline]
fn is_named_variant_ident_char(c u8) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `_`
}
