module naming

pub fn sum_field_name(variant string) string {
	if variant.starts_with('&') {
		return '_ptr_${sum_field_name(variant[1..])}'
	}
	if variant.starts_with('?') {
		return '_Option_${c_name(variant[1..])}'
	}
	if variant.starts_with('!') {
		return '_Result_${c_name(variant[1..])}'
	}
	if variant.starts_with('[]') {
		return '_Array_${c_name(variant[2..])}'
	}
	if variant.starts_with('map[') {
		return '_Map_${c_name(variant[4..].replace(']', '_'))}'
	}
	if variant.starts_with('fn(') || variant.starts_with('fn (') {
		key := fn_signature_key(variant)
		mut hash := u64(1469598103934665603)
		for b in key {
			hash = (hash ^ u64(b)) * u64(1099511628211)
		}
		return '_Fn_${hash}'
	}
	if variant.contains('(') || variant.contains(')') || variant.contains(' ') {
		return '_${type_name_part(variant)}'
	}
	return match variant {
		'int' { '_int' }
		'i8' { '_i8' }
		'i16' { '_i16' }
		'i64' { '_i64' }
		'u8' { '_u8' }
		'u16' { '_u16' }
		'u32' { '_u32' }
		'u64' { '_u64' }
		'f32' { '_f32' }
		'f64' { '_f64' }
		'bool' { '_bool' }
		'string' { '_string' }
		else { c_name(variant) }
	}
}

pub fn fn_signature_key(variant string) string {
	clean := variant.trim_space()
	open := clean.index('(') or { return clean.replace(' ', '') }
	close := clean.last_index(')') or { return clean.replace(' ', '') }
	params := clean[open + 1..close]
	ret := clean[close + 1..].trim_space().replace(' ', '')
	mut parts := []string{}
	for part in sum_fn_split_top_level_commas(params) {
		ptyp := sum_fn_param_type(part)
		if ptyp.len > 0 {
			parts << ptyp
		}
	}
	return 'fn(${parts.join(',')})${ret}'
}

fn sum_fn_split_top_level_commas(params string) []string {
	mut parts := []string{}
	mut depth := 0
	mut start := 0
	for i := 0; i < params.len; i++ {
		ch := params[i]
		if ch == `(` || ch == `[` || ch == `{` {
			depth++
		} else if ch == `)` || ch == `]` || ch == `}` {
			if depth > 0 {
				depth--
			}
		} else if ch == `,` && depth == 0 {
			parts << params[start..i].trim_space()
			start = i + 1
		}
	}
	parts << params[start..].trim_space()
	return parts
}

fn sum_fn_param_type(param string) string {
	clean := param.trim_space()
	if clean.len == 0 {
		return ''
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		return fn_signature_key(clean)
	}
	space := clean.index(' ') or { return clean }
	first := clean[..space]
	if sum_fn_is_ident(first) && first !in ['fn', 'mut', 'shared'] {
		return clean[space + 1..].trim_space().replace(' ', '')
	}
	if first in ['mut', 'shared'] {
		rest := clean[space + 1..].trim_space()
		second_space := rest.index(' ') or { return clean.replace(' ', '') }
		second := rest[..second_space]
		if sum_fn_is_ident(second) {
			return '${first}${rest[second_space + 1..].trim_space().replace(' ', '')}'
		}
	}
	return clean.replace(' ', '')
}

fn sum_fn_is_ident(s string) bool {
	if s.len == 0 {
		return false
	}
	first := s[0]
	if !((first >= `a` && first <= `z`) || (first >= `A` && first <= `Z`) || first == `_`) {
		return false
	}
	for i := 1; i < s.len; i++ {
		ch := s[i]
		if !((ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
			|| (ch >= `0` && ch <= `9`) || ch == `_`) {
			return false
		}
	}
	return true
}
