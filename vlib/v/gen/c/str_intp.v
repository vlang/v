module c

import v.flat
import v.types

// gen_string_interp emits string interp output for c.
fn (mut g FlatGen) gen_string_interp(node flat.Node) {
	if node.value == '__v3_string_join' {
		g.gen_string_join(node)
		return
	}
	n := node.children_count
	if n == 0 {
		sid := g.intern_string('')
		g.write('_str_${sid}')
		return
	}
	g.write('string_plus_many(${n}, (string[${n}]){')
	for i in 0 .. n {
		if i > 0 {
			g.write(', ')
		}
		child_id := g.a.child(&node, i)
		mut expr_id := child_id
		mut child := g.a.nodes[int(child_id)]
		mut format := ''
		if child.kind == .directive && child.value == 'string_interp_format'
			&& child.children_count > 0 {
			expr_id = g.a.child(&child, 0)
			format = child.typ_text()
			child = g.a.nodes[int(expr_id)]
		}
		if child.kind == .string_literal {
			sid := g.intern_string(child.value)
			g.write('_str_${sid}')
		} else {
			typ := g.string_interp_child_type(expr_id, child)
			typ_name_id := types.Type(typ).source_type_id()
			if child.typ == flat.source_type_string || typ is types.String {
				if format.len > 0 {
					g.gen_formatted_string_interp_child_expr(expr_id, typ, format)
				} else {
					g.gen_string_interp_child_expr(expr_id)
				}
			} else if format.len > 0
				&& g.gen_formatted_string_interp_child_expr(expr_id, typ, format) {
				// emitted by gen_formatted_string_interp_child_expr
			} else if g.gen_map_str_expr(expr_id, typ) {
				// emitted by gen_map_str_expr
			} else if g.is_ierror_type_name(typ_name_id) {
				// IError may resolve as Interface/Alias/Struct depending on context. Dispatch
				// through msg() so boxed MessageError values do not depend on the legacy cache.
				g.gen_ierror_dynamic_method_expr(expr_id, typ, 'msg')
			} else if typ is types.Primitive {
				prim_name := types.Type(typ).name()
				g.write('${g.cname('${prim_name}.str')}(')
				g.gen_string_interp_child_expr(expr_id)
				g.write(')')
			} else if typ is types.ISize || typ is types.USize {
				g.write('${g.cname('${typ.name()}.str')}(')
				g.gen_string_interp_child_expr(expr_id)
				g.write(')')
			} else if typ is types.Enum {
				g.write('${g.enum_autostr_c_name(typ.source_type_id())}__autostr(')
				g.gen_string_interp_child_expr(expr_id)
				g.write(')')
			} else if typ is types.Interface {
				str_key := '${typ.name()}.str'
				if str_key in g.tc.fn_ret_types {
					g.write('${g.cname(str_key)}(')
					g.gen_string_interp_child_expr(child_id)
					g.write(')')
				} else {
					sid := g.intern_string('${typ.name().all_after_last('.')}{}')
					g.write('_str_${sid}')
				}
			} else if typ is types.Struct {
				g.write('${g.cname(typ.name())}__str(')
				g.gen_string_interp_child_expr(expr_id)
				g.write(')')
			} else if typ is types.SumType {
				g.write('${g.cname(typ.name())}__str(')
				g.gen_string_interp_child_expr(expr_id)
				g.write(')')
			} else {
				g.write('int__str(')
				g.gen_string_interp_child_expr(expr_id)
				g.write(')')
			}
		}
	}
	g.write('})')
}

// Lowered operands are already strings. Separate assignments preserve source
// evaluation order, including calls that mutate a value used by a later part.
fn (mut g FlatGen) gen_string_join(node flat.Node) {
	g.gen_string_join_parts(g.a.children_of(&node), true)
}

fn (mut g FlatGen) gen_string_join_parts(parts []flat.NodeId, interpolation bool) {
	name := '__v3_internal_symbol_join_${g.tmp_count}'
	g.tmp_count++
	g.write('({ string ${name}[${parts.len}]; ')
	mut float_buffer_bytes := 0
	for i, part in parts {
		if precision := g.fixed_float_buffer_precision(part) {
			// Bound total stack use even for interpolations with thousands of operands.
			if float_buffer_bytes + precision + 312 <= 16384 {
				float_buffer_bytes += precision + 312
				// Keep each buffer alive until string_plus_many copies all operands.
				g.write('u8 ${name}_float_${i}[${precision + 312}]; ${name}[${i}] = v3_f64_fixed_buf(')
				g.gen_string_interp_child_expr(g.a.child(&g.a.nodes[int(part)], 1))
				g.write(', ${precision}, ${name}_float_${i}); ')
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
	g.write('string_plus_many(${parts.len}, ${name}); })')
}

fn (g &FlatGen) can_use_fixed_float_buffer() bool {
	return g.target.os != 'vinix' && !isnil(g.tc)
		&& 'strconv.f64_to_decimal_exact_int' in g.tc.fn_ret_types
		&& 'strconv.f64_to_decimal' in g.tc.fn_ret_types
		&& (!g.has_used_fn_filter()
			|| (g.used_fn_contains('strconv.f64_to_decimal_exact_int')
				&& g.used_fn_contains('strconv.f64_to_decimal')))
}

fn (g &FlatGen) fixed_float_buffer_precision(id flat.NodeId) ?int {
	node := g.a.nodes[int(id)]
	if node.kind != .call || node.children_count != 3 {
		return none
	}
	callee := g.a.child_node(&node, 0)
	if callee.kind != .ident || callee.value != 'v3_f64_fixed' {
		return none
	}
	precision := g.a.child_node(&node, 2)
	// Bound stack use; dynamic and unusually large precisions retain the existing path.
	if precision.kind != .int_literal {
		return none
	}
	value := precision.value.int()
	if value < 0 || value > 126 || !g.can_use_fixed_float_buffer() {
		return none
	}
	return value
}

fn (g &FlatGen) string_interp_child_type(child_id flat.NodeId, child flat.Node) types.Type {
	if child.kind == .ident && g.current_param_is_mut(child.value) {
		if param_type := g.current_param_type(child.value) {
			if param_type is types.Pointer {
				if map_str_clean_type(param_type.base_type) is types.Map {
					return param_type
				}
				return *param_type.base_type
			}
			return *param_type
		}
	}
	mut typ := g.tc.resolve_type(child_id)
	// For a bare ident, prefer the live cgen scope binding when present: it reflects
	// locals introduced during generation (e.g. the `err` of an or-body lowered here,
	// or for-loop vars) that resolve_type may stale-cache as `int`.
	if child.kind == .ident {
		if scope_typ := g.tc.cur_scope.lookup(child.value) {
			if scope_typ !is types.Void {
				typ = scope_typ
			}
		}
	}
	return typ
}

struct StringInterpFormat {
mut:
	width         int
	precision     int
	has_precision bool
	verb          u8
	left          bool
	zero          bool
}

fn parse_string_interp_format(format string) StringInterpFormat {
	mut f := StringInterpFormat{}
	mut i := 0
	if i < format.len && format[i] == `-` {
		f.left = true
		i++
	}
	if i < format.len && format[i] == `0` {
		f.zero = true
		i++
	}
	for i < format.len && format[i] >= `0` && format[i] <= `9` {
		f.width = f.width * 10 + int(format[i] - `0`)
		i++
	}
	if i < format.len && format[i] == `.` {
		i++
		f.has_precision = true
		for i < format.len && format[i] >= `0` && format[i] <= `9` {
			f.precision = f.precision * 10 + int(format[i] - `0`)
			i++
		}
	}
	if i < format.len {
		f.verb = format[i]
	}
	return f
}

fn string_interp_type_name(typ &types.Type) u32 {
	name_id := typ.source_type_id()
	if flat.source_type_syntax(name_id).builtin_prefix() {
		return flat.source_type_syntax(name_id).unqualified()
	}
	return name_id
}

fn is_string_interp_float_type(name u32) bool {
	return name in [flat.source_type_f32, flat.source_type_f64, cgen_float_literal_source_type]
}

fn is_string_interp_signed_int_type(name u32) bool {
	return name in [flat.source_type_int, flat.source_type_i8, flat.source_type_i16,
		flat.source_type_i32, flat.source_type_i64, flat.source_type_isize,
		cgen_int_literal_source_type]
}

fn is_string_interp_unsigned_int_type(name u32) bool {
	return name in [flat.source_type_u8, flat.source_type_u16, flat.source_type_u32,
		flat.source_type_u64, flat.source_type_uint]
}

fn is_string_interp_char_code_type(name u32) bool {
	return is_string_interp_signed_int_type(name) || is_string_interp_unsigned_int_type(name)
		|| name == flat.source_type_rune
}

fn (mut g FlatGen) gen_formatted_string_interp_child_expr(child_id flat.NodeId, typ types.Type, format string) bool {
	f := parse_string_interp_format(format)
	type_name := string_interp_type_name(g.value_unalias_type(typ))
	left := if f.left { 1 } else { 0 }
	// An unsigned-backed enum must format as unsigned so values >= 1<<63 are not
	// rendered as negative; consult the enum backing type like the transformer does.
	enum_unsigned := if typ is types.Enum {
		enum_storage_c_type_is_unsigned(g.enum_storage_c_type(typ))
	} else {
		false
	}
	if is_string_interp_char_code_type(type_name) && f.verb == `c` {
		if f.width > 1 {
			g.write('v3_string_pad(')
		}
		g.write('rune__str((u32)(')
		g.gen_string_interp_child_expr(child_id)
		g.write('))')
		if f.width > 1 {
			g.write(', ${f.width}, ${left})')
		}
		return true
	}
	if (is_string_interp_signed_int_type(type_name) || is_string_interp_unsigned_int_type(type_name)
		|| typ is types.Enum) && f.verb in [`b`, `o`, `x`, `X`] {
		base := match f.verb {
			`b` { 2 }
			`o` { 8 }
			else { 16 }
		}

		zero_pad := f.zero && f.width > 0
		space_pad := !zero_pad && f.width > 0
		if zero_pad {
			g.write('v3_string_zpad(')
		} else if space_pad {
			g.write('v3_string_pad(')
		}
		if f.verb == `X` {
			g.write('v3_string_upper_ascii(')
		}
		if is_string_interp_unsigned_int_type(type_name) || enum_unsigned {
			g.write('strconv__format_uint((u64)(')
		} else {
			g.write('strconv__format_int((i64)(')
		}
		g.gen_string_interp_child_expr(child_id)
		g.write('), ${base})')
		if f.verb == `X` {
			g.write(')')
		}
		if zero_pad {
			g.write(', ${f.width})')
		} else if space_pad {
			g.write(', ${f.width}, ${left})')
		}
		return true
	}
	if typ is types.Enum && f.verb == 0 {
		if f.zero && f.width > 0 {
			g.write('v3_string_zpad(')
		} else if f.width > 0 {
			g.write('v3_string_pad(')
		}
		g.write('${g.enum_autostr_c_name(typ.source_type_id())}__autostr(')
		g.gen_string_interp_child_expr(child_id)
		g.write(')')
		if f.zero && f.width > 0 {
			g.write(', ${f.width})')
		} else if f.width > 0 {
			g.write(', ${f.width}, ${left})')
		}
		return true
	}
	if typ is types.Enum && f.verb == `d` {
		zpad_fn := if enum_unsigned { 'v3_u64_zpad' } else { 'v3_i64_zpad' }
		cast := if enum_unsigned { 'u64' } else { 'i64' }
		str_fn := if enum_unsigned { 'u64__str' } else { 'i64__str' }
		if f.zero && f.width > 0 {
			g.write('${zpad_fn}((${cast})(')
			g.gen_string_interp_child_expr(child_id)
			g.write('), ${f.width})')
			return true
		}
		if f.width > 0 {
			g.write('v3_string_pad(${str_fn}((${cast})(')
			g.gen_string_interp_child_expr(child_id)
			g.write(')), ${f.width}, ${left})')
			return true
		}
		if enum_unsigned {
			g.write('strconv__format_uint((u64)(')
		} else {
			g.write('strconv__format_int((i64)(')
		}
		g.gen_string_interp_child_expr(child_id)
		g.write('), 10)')
		return true
	}
	if is_string_interp_float_type(type_name) && (f.verb == `f` || f.verb == 0) && f.has_precision {
		fixed_fn := if f.verb == `f` { 'v3_f64_fixed' } else { 'v3_f64_trimmed' }
		g.write('v3_string_pad(${fixed_fn}((double)(')
		g.gen_string_interp_child_expr(child_id)
		g.write('), ${f.precision}), ${f.width}, ${left})')
		return true
	}
	if typ is types.String || type_name == flat.source_type_string {
		if f.width > 0 && (f.verb == `s` || f.verb == 0) {
			g.write('v3_string_pad(')
			g.gen_string_interp_child_expr(child_id)
			g.write(', ${f.width}, ${left})')
			return true
		}
		g.gen_string_interp_child_expr(child_id)
		return true
	}
	if is_string_interp_signed_int_type(type_name) && (f.verb == `d` || f.verb == 0) {
		if f.zero && f.width > 0 {
			g.write('v3_i64_zpad((i64)(')
			g.gen_string_interp_child_expr(child_id)
			g.write('), ${f.width})')
			return true
		}
		if f.width > 0 {
			g.write('v3_string_pad(i64__str((i64)(')
			g.gen_string_interp_child_expr(child_id)
			g.write(')), ${f.width}, ${left})')
			return true
		}
	}
	if is_string_interp_unsigned_int_type(type_name) && (f.verb == `d` || f.verb == 0) {
		if f.zero && f.width > 0 {
			g.write('v3_u64_zpad((u64)(')
			g.gen_string_interp_child_expr(child_id)
			g.write('), ${f.width})')
			return true
		}
		if f.width > 0 {
			g.write('v3_string_pad(u64__str((u64)(')
			g.gen_string_interp_child_expr(child_id)
			g.write(')), ${f.width}, ${left})')
			return true
		}
	}
	if signed := int128_signedness(typ) {
		// The arms above reach for `i64`/`u64`, which would cut a 128-bit value back
		// to its low half and, on the portable representation, cast a struct to an
		// integer. These keep the value whole and pad the printed text instead.
		if f.verb in [`b`, `o`, `x`, `X`] {
			base := match f.verb {
				`b` { 2 }
				`o` { 8 }
				else { 16 }
			}
			zero_pad := f.zero && f.width > 0
			space_pad := !zero_pad && f.width > 0
			if zero_pad {
				g.write('v3_string_zpad(')
			} else if space_pad {
				g.write('v3_string_pad(')
			}
			if f.verb == `X` {
				g.write('v3_string_upper_ascii(')
			}
			// Both signednesses format from the bit pattern, which is what a signed
			// 64-bit value prints as in this position too.
			g.write('u128__str_base((u128)(')
			g.gen_string_interp_child_expr(child_id)
			g.write('), ${base})')
			if f.verb == `X` {
				g.write(')')
			}
			if zero_pad {
				g.write(', ${f.width})')
			} else if space_pad {
				g.write(', ${f.width}, ${left})')
			}
			return true
		}
		if f.verb == `c` {
			if f.width > 1 {
				g.write('v3_string_pad(')
			}
			g.write('rune__str((u32)(')
			g.gen_string_interp_child_expr(child_id)
			g.write('))')
			if f.width > 1 {
				g.write(', ${f.width}, ${left})')
			}
			return true
		}
		if f.verb == 0 || f.verb == `d` || f.verb == `u` {
			str_fn := if signed { 'i128__str' } else { 'u128__str' }
			if f.zero && f.width > 0 {
				g.write('v3_string_zpad(${str_fn}(')
				g.gen_string_interp_child_expr(child_id)
				g.write('), ${f.width})')
				return true
			}
			if f.width > 0 {
				g.write('v3_string_pad(${str_fn}(')
				g.gen_string_interp_child_expr(child_id)
				g.write('), ${f.width}, ${left})')
				return true
			}
		}
	}
	return false
}

fn (mut g FlatGen) gen_string_interp_child_expr(child_id flat.NodeId) {
	child := g.a.nodes[int(child_id)]
	if child.kind == .ident && g.current_param_is_mut(child.value) {
		if param_type := g.current_param_type(child.value) {
			if param_type is types.Pointer {
				g.write('(*')
				g.gen_expr(child_id)
				g.write(')')
				return
			}
		}
	}
	g.gen_expr(child_id)
}

// is_string_node reports whether is string node applies in c.
fn (g &FlatGen) is_string_node(id flat.NodeId) bool {
	return g.tc.resolve_type(id) is types.String
}

// string_literals supports string literals handling for FlatGen.
fn (mut g FlatGen) string_literals() {
	g.string_literals_from(0)
}

// string_literals_from emits the interned literal table starting at index
// `start`. The postamble fork emits [0, snapshot) during the parallel region;
// anything interned later (worker novelties, the synthetic main) is emitted
// as a supplement after the joins — per-id definitions are order-independent.
fn (mut g FlatGen) string_literals_from(start int) {
	use_declaration_macro := g.str_lits.len > start && !g.cache_stable_symbols
	if use_declaration_macro {
		// Keep the helper out of C headers, extern declarations, and function bodies,
		// and give back a `_SL` macro that an included C header defined.
		g.writeln('#pragma push_macro("_SL")')
		g.writeln('#undef _SL')
		g.writeln('#define _SL(id, s, n) static const string id = {(u8*)s, n, 1}')
	}
	if g.cache_split {
		mut literals := g.str_lits[start..].clone()
		literals.sort()
		for s in literals {
			g.write_string_literal_def(g.str_lit_ids[s], s)
		}
	} else {
		for i := start; i < g.str_lits.len; i++ {
			g.write_string_literal_def(i, g.str_lits[i])
		}
	}
	if use_declaration_macro {
		g.writeln('#undef _SL')
		g.writeln('#pragma pop_macro("_SL")')
	}
	if g.str_lits.len > start {
		g.writeln('')
	}
}

// write_string_literal_def emits one literal table entry straight into the
// output builder: escaping into a temporary and interpolating the line would
// leave two copies of every literal behind in the cgen arena.
fn (mut g FlatGen) write_string_literal_def(id int, s string) {
	// Cached modules rewrite and share literal storage through their declaration prefix.
	// The declaration macro keeps the original initializer and V's byte length.
	if !g.cache_stable_symbols {
		g.write('_SL(_str_')
		g.sb.write_decimal(i64(id))
		g.sb.write_string(', "')
		c_literal_escape_into(mut g.sb, s)
		g.sb.write_string('", ')
		g.sb.write_decimal(i64(s.len))
		g.writeln(');')
		return
	}
	g.write('static const string _str_')
	g.sb.write_decimal(i64(id))
	g.sb.write_string(' = {"')
	c_literal_escape_into(mut g.sb, s)
	g.sb.write_string('", ')
	g.sb.write_decimal(i64(s.len))
	g.writeln(', 1};')
}

// intern_string supports intern string handling for FlatGen.
fn (mut g FlatGen) intern_string(s string) int {
	if id := g.str_lit_ids[s] {
		return id
	}
	if g.str_lits_shared {
		g.str_lits = g.str_lits.clone()
		g.str_lit_ids = g.str_lit_ids.clone()
		g.str_lits_shared = false
	}
	id := g.str_lits.len
	g.str_lits << s
	g.str_lit_ids[s] = id
	return id
}

// fixed_float_buffer_helper preserves v3_f64_fixed's shortest-decimal rounding without
// materializing its intermediate decimal string. The caller reserves precision + 312 bytes.
fn (mut g FlatGen) fixed_float_buffer_helper() {
	if !g.can_use_fixed_float_buffer() {
		return
	}
	g.writeln('multi_return_strconv__Dec64_bool strconv__f64_to_decimal_exact_int(u64 mant, u64 exp);')
	g.writeln('strconv__Dec64 strconv__f64_to_decimal(u64 mant, u64 exp);')
	g.writeln('static inline string v3_f64_fixed_buf(double x, int precision, u8* out) {')
	g.writeln(' if (!isfinite(x)) return f64__str(x);')
	g.writeln(' double y = fabs(x) + 0.5 * pow(10.0, -(precision > 35 ? 35 : precision));')
	g.writeln(' union { double f; u64 u; } bits; bits.f = y;')
	g.writeln(' u64 mant = bits.u & 0x000fffffffffffffull; u64 exp = (bits.u >> 52) & 0x7ff;')
	g.writeln(' multi_return_strconv__Dec64_bool exact = strconv__f64_to_decimal_exact_int(mant, exp);')
	g.writeln(' strconv__Dec64 d = exact.arg1 ? exact.arg0 : strconv__f64_to_decimal(mant, exp);')
	g.writeln(' u8 digits[32]; int count = 0; u64 m = d.m;')
	g.writeln(" do { digits[count++] = (u8)('0' + m % 10); m /= 10; } while (m != 0);")
	g.writeln(' int decimal_pos = count + d.e; int whole = decimal_pos > 0 ? decimal_pos : 1;')
	g.writeln(' int pos = 0;')
	g.writeln(" if (signbit(x)) out[pos++] = '-';")
	g.writeln(' for (int i = 0; i < whole + precision; ++i) {')
	g.writeln("  if (i == whole) out[pos++] = '.';")
	g.writeln('  int digit = decimal_pos <= 0 ? i + decimal_pos - 1 : i;')
	g.writeln("  out[pos++] = digit >= 0 && digit < count ? digits[count - digit - 1] : '0';")
	g.writeln(' }')
	g.writeln(' out[pos] = 0; return (string){.str = out, .len = pos, .is_lit = 1};')
	g.writeln('}')
}
