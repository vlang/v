module c

import v.flat

// Only compiler-selected numeric conversions can return views of these buffers.
// User str() methods and dynamically sized formats keep their owning results.
fn (g &FlatGen) interpolation_buffer_call(id flat.NodeId) ?string {
	node := g.a.nodes[int(id)]
	if node.kind != .call || node.children_count < 2 { return none }
	callee := g.a.child_node(&node, 0)
	if callee.kind != .ident { return none }
	if resolved := g.resolved_call_name(id) {
		if resolved != callee.value && g.cname(resolved) != callee.value { return none }
	} else if node.pos.is_valid() {
		return none
	}
	return g.cname(callee.value)
}

fn (g &FlatGen) interpolation_buffer_constant(node flat.Node, index int, limit int) ?int {
	if index >= node.children_count { return none }
	value := g.a.child_node(&node, index)
	if value.kind != .int_literal { return none }
	n := value.value.int()
	if n < 0 || n > limit { return none }
	return n
}

fn (g &FlatGen) interpolation_buffer_size(id flat.NodeId, depth int) ?int {
	if depth > 12 { return none }
	name := g.interpolation_buffer_call(id) or { return none }
	node := g.a.nodes[int(id)]
	match name {
		'int__str', 'i8__str', 'i16__str', 'i32__str', 'i64__str', 'isize__str',
		'u8__str', 'u16__str', 'u32__str', 'u64__str', 'uint__str', 'usize__str' {
			if node.children_count != 2 { return none }
			return 66 // 64 binary digits, sign, terminator.
		}
		'strconv__format_int', 'strconv__format_uint' {
			base := g.interpolation_buffer_constant(node, 2, 36) or { return none }
			if base < 2 || node.children_count != 3 { return none }
			return 66
		}
		'v3_int_zpad', 'v3_i64_zpad', 'v3_u64_zpad' {
			width := g.interpolation_buffer_constant(node, 2, 4096) or { return none }
			return if width + 1 > 66 { width + 1 } else { 66 }
		}
		'v3_f64_fixed', 'v3_f64_trimmed' {
			precision := g.interpolation_buffer_constant(node, 2, 126) or { return none }
			if !g.can_use_fixed_float_buffer() { return none }
			return precision + 312
		}
		'v3_f64_exp', 'v3_f64_general' {
			precision := g.interpolation_buffer_constant(node, 2, 126) or { return none }
			_ := g.interpolation_buffer_constant(node, 3, 1) or { return none }
			return precision + 32
		}
		'v3_string_upper_ascii', 'v3_string_plus_sign' {
			size := g.interpolation_buffer_size(g.a.child(&node, 1), depth + 1) or { return none }
			return size + if name == 'v3_string_plus_sign' { 1 } else { 0 }
		}
		'v3_string_pad', 'v3_string_zpad', 'v3_f64_zpad', 'v3_string_rpad_zero' {
			size := g.interpolation_buffer_size(g.a.child(&node, 1), depth + 1) or { return none }
			width := g.interpolation_buffer_constant(node, 2, 4096) or { return none }
			if name == 'v3_string_pad' {
				_ := g.interpolation_buffer_constant(node, 3, 1) or { return none }
			}
			return if width + 1 > size { width + 1 } else { size }
		}
		else { return none }
	}
}

fn (mut g FlatGen) gen_buffered_interpolation_part(id flat.NodeId, buffer string, capacity int) {
	node := g.a.nodes[int(id)]
	name := g.cname(g.a.child_node(&node, 0).value)
	arg := g.a.child(&node, 1)
	match name {
		'v3_string_upper_ascii', 'v3_string_plus_sign' {
			operation := if name == 'v3_string_plus_sign' { 1 } else { 0 }
			g.write('v3_numeric_transform_buf(')
			g.gen_buffered_interpolation_part(arg, buffer, capacity)
			g.write(', ${operation}, ${buffer}, ${capacity})')
		}
		'v3_string_pad', 'v3_string_zpad', 'v3_f64_zpad', 'v3_string_rpad_zero' {
			mode := match name {
				'v3_string_pad' { g.a.child_node(&node, 3).value.int() }
				'v3_string_zpad' { 2 }
				'v3_string_rpad_zero' { 3 }
				else { 4 }
			}
			g.write('v3_numeric_pad_buf(')
			g.gen_buffered_interpolation_part(arg, buffer, capacity)
			g.write(', ${g.a.child_node(&node, 2).value}, ${mode}, ${buffer}, ${capacity})')
		}
		'v3_f64_fixed', 'v3_f64_trimmed', 'v3_f64_exp', 'v3_f64_general' {
			g.write('${name}_buf(')
			g.gen_string_interp_child_expr(arg)
			g.write(', ${g.a.child_node(&node, 2).value}')
			if name in ['v3_f64_exp', 'v3_f64_general'] {
				g.write(', ${g.a.child_node(&node, 3).value}')
			}
			g.write(', ${buffer}')
			if name != 'v3_f64_fixed' { g.write(', ${capacity}') }
			g.write(')')
		}
		else {
			padded := name in ['v3_int_zpad', 'v3_i64_zpad', 'v3_u64_zpad']
			if padded { g.write('v3_numeric_pad_buf(') }
			unsigned := name in ['u8__str', 'u16__str', 'u32__str', 'u64__str', 'uint__str',
				'usize__str', 'strconv__format_uint', 'v3_u64_zpad']
			g.write(if unsigned { 'v3_u64_buf((u64)(' } else { 'v3_i64_buf((i64)(' })
			// C promotes narrow arithmetic to int before the original str() call
			// truncates it back to the receiver type. Keep that conversion here too.
			narrow := name in ['i8__str', 'i16__str', 'i32__str', 'u8__str', 'u16__str', 'u32__str']
			if narrow { g.write('(${name.all_before('__str')})(') }
			g.gen_string_interp_child_expr(arg)
			if narrow { g.write(')') }
			base := if name in ['strconv__format_int', 'strconv__format_uint'] {
				g.a.child_node(&node, 2).value.int()
			} else {
				10
			}
			g.write('), ${base}, ${buffer})')
			if padded {
				g.write(', ${g.a.child_node(&node, 2).value}, 2, ${buffer}, ${capacity})')
			}
		}
	}
}

// All persistent numeric buffers share the same 16 KiB budget as fixed floats.
// Every part is evaluated before the consumer copies any of their bytes.
fn (mut g FlatGen) gen_interpolation_part_assignments(parts []flat.NodeId, name string, interpolation bool) {
	mut buffer_bytes := 0
	for i, part in parts {
		if size := g.interpolation_buffer_size(part, 0) {
			if buffer_bytes + size <= 16384 {
				buffer_bytes += size
				buffer := '${name}_number_${i}'
				g.write('u8 ${buffer}[${size}]; ${name}[${i}] = ')
				g.gen_buffered_interpolation_part(part, buffer, size)
				g.write('; ')
				continue
			}
		}
		g.write('${name}[${i}] = ')
		if interpolation {
			g.gen_string_interp_child_expr(part)
		} else {
			g.gen_expr_as_string(part)
		}
		g.write('; ')
	}
}

fn (mut g FlatGen) interpolation_buffer_helpers() {
	g.writeln('static inline string v3_integer_buf(u64 n, int base, int negative, u8* out) {')
	g.writeln(" int len = 0; do { unsigned digit = (unsigned)(n % (u64)base); out[len++] = (u8)(digit < 10 ? '0' + digit : 'a' + digit - 10); n /= (u64)base; } while (n);")
	g.writeln(" if (negative) out[len++] = '-';")
	g.writeln(' for (int i = 0; i < len / 2; ++i) { u8 c = out[i]; out[i] = out[len - i - 1]; out[len - i - 1] = c; }')
	g.writeln(' out[len] = 0; return (string){.str = out, .len = len, .is_lit = 1}; }')
	g.writeln('static inline string v3_u64_buf(u64 n, int base, u8* out) { return v3_integer_buf(n, base, 0, out); }')
	g.writeln('static inline string v3_i64_buf(i64 n, int base, u8* out) { u64 magnitude = (u64)n; if (n < 0) magnitude = (u64)0 - magnitude; return v3_integer_buf(magnitude, base, n < 0, out); }')
	for name in ['exp', 'general'] {
		lower := if name == 'exp' { 'e' } else { 'g' }
		upper := if name == 'exp' { 'E' } else { 'G' }
		g.writeln('static inline string v3_f64_${name}_buf(double x, int precision, int upper, u8* out, int capacity) {')
		g.writeln(' int n = snprintf((char*)out, (size_t)capacity, upper ? "%.*${upper}" : "%.*${lower}", precision, x);')
		g.writeln(' if (n < 0) return v3_c_lit("", 0); if (n >= capacity) return v3_f64_${name}(x, precision, upper);')
		g.writeln(' n = v3_float_normalize_exponent((char*)out, n); return (string){.str = out, .len = n, .is_lit = 1}; }')
	}
	if g.can_use_fixed_float_buffer() {
		g.writeln('static inline string v3_f64_trimmed_buf(double x, int precision, u8* out, int capacity) {')
		g.writeln(' if (x == 0.0) return signbit(x) ? v3_c_lit("-0", 2) : v3_c_lit("0", 1); double d = fabs(x);')
		g.writeln(' string s = !isfinite(x) || (d < 999999.0 && d >= 0.00001) ? v3_f64_fixed_buf(x, precision, out) : v3_f64_exp_buf(x, precision > 0 ? precision - 1 : 0, 0, out, capacity);')
		g.writeln(" if (s.str != out && s.is_lit) return s; u8* text = s.str; int mant = s.len, dot = -1; for (int i = 0; i < s.len; ++i) { if (text[i] == '.') dot = i; if (text[i] == 'e') { mant = i; break; } }")
		g.writeln(" if (dot < 0) return s; int end = mant; while (end > dot + 1 && text[end - 1] == '0') --end; if (end == dot + 1) --end;")
		g.writeln(' memmove(text + end, text + mant, (size_t)(s.len - mant)); s.len -= mant - end; text[s.len] = 0; return s; }')
	}
	g.writeln('static inline string v3_numeric_transform_buf(string s, int plus, u8* out, int capacity) {')
	g.writeln(" int add = plus && !(s.len > 0 && (s.str[0] == '-' || s.str[0] == '+')); if (s.len + add >= capacity) return plus ? v3_string_plus_sign(s) : v3_string_upper_ascii(s);")
	g.writeln(" memmove(out + add, s.str, (size_t)s.len); if (add) out[0] = '+'; int len = s.len + add;")
	g.writeln(" if (!plus) for (int i = 0; i < len; ++i) if (out[i] >= 'a' && out[i] <= 'z') out[i] -= 'a' - 'A';")
	g.writeln(' out[len] = 0; return (string){.str = out, .len = len, .is_lit = 1}; }')
	g.writeln('static inline string v3_numeric_pad_buf(string s, int width, int mode, u8* out, int capacity) {')
	g.writeln(" int sign = s.len > 0 && (s.str[0] == '-' || s.str[0] == '+'); if (mode == 4) mode = s.len <= sign || s.str[sign] < '0' || s.str[sign] > '9' ? 0 : 2;")
	g.writeln(' if (s.len >= width) return s; if (width >= capacity) { if (mode == 2) return v3_string_zpad(s, width); if (mode == 3) return v3_string_rpad_zero(s, width); return v3_string_pad(s, width, mode); }')
	g.writeln(" int pad = width - s.len; if (mode == 1 || mode == 3) { memmove(out, s.str, (size_t)s.len); memset(out + s.len, mode == 3 ? '0' : ' ', (size_t)pad); }")
	g.writeln(" else if (mode == 2) { u8 prefix = sign ? s.str[0] : 0; memmove(out + sign + pad, s.str + sign, (size_t)(s.len - sign)); if (sign) out[0] = prefix; memset(out + sign, '0', (size_t)pad); }")
	g.writeln(" else { memmove(out + pad, s.str, (size_t)s.len); memset(out, ' ', (size_t)pad); }")
	g.writeln(' out[width] = 0; return (string){.str = out, .len = width, .is_lit = 1}; }')
}
