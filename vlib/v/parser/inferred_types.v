module parser

import v.flat

// shift_inferred_type_id is `shift_inferred_type` for an interned node
// annotation; other spellings keep their ID without reading their bytes.
@[inline]
fn shift_inferred_type_id(typ u32, shift int) u32 {
	if !flat.source_type_shape(typ).has(.typeof_expr) {
		return typ
	}
	text := flat.source_type_name(typ)
	if !text.contains('typeof(__expr_') {
		return typ
	}
	return flat.source_type_id(shift_inferred_type(text, shift))
}

// Parser workers merge local ASTs with shifted node ids. `typeof(expr)` type
// annotations hold expression links and relocate with the ordinary child links.
fn shift_inferred_type(typ string, shift int) string {
	mut result := typ
	mut offset := 0
	marker := 'typeof(__expr_'
	for offset < result.len {
		start := result[offset..].index(marker) or { break } + offset
		end := result[start..].index(')') or { break } + start
		value := result[start + marker.len..end].int()
		replacement := marker + (value + shift).str() + ')'
		result = result[..start] + replacement + result[end + 1..]
		offset = start + replacement.len
	}
	return result
}
