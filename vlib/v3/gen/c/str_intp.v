module c

import v3.flat
import v3.types

// gen_string_interp emits string interp output for c.
fn (mut g FlatGen) gen_string_interp(node flat.Node) {
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
			format = child.typ
			child = g.a.nodes[int(expr_id)]
		}
		if child.kind == .string_literal {
			sid := g.intern_string(child.value)
			g.write('_str_${sid}')
		} else {
			typ := g.string_interp_child_type(expr_id, child)
			typ_name := types.Type(typ).name()
			if child.typ == 'string' || typ is types.String {
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
			} else if g.is_ierror_type_name(typ_name) {
				// IError may resolve as Interface/Alias/Struct depending on context; match by
				// name and interpolate its `.message` (mirrors the transformer's IError path).
				g.gen_string_interp_child_expr(expr_id)
				g.write('.message')
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
				g.write('${g.cname(typ.name)}__autostr(')
				g.gen_string_interp_child_expr(expr_id)
				g.write(')')
			} else if typ is types.Interface {
				str_key := '${typ.name}.str'
				if str_key in g.tc.fn_ret_types {
					g.write('${g.cname(str_key)}(')
					g.gen_string_interp_child_expr(child_id)
					g.write(')')
				} else {
					sid := g.intern_string('${typ.name.all_after_last('.')}{}')
					g.write('_str_${sid}')
				}
			} else if typ is types.Struct {
				g.write('${g.cname(typ.name)}__str(')
				g.gen_string_interp_child_expr(expr_id)
				g.write(')')
			} else if typ is types.SumType {
				g.write('${g.cname(typ.name)}__str(')
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

fn (g &FlatGen) string_interp_child_type(child_id flat.NodeId, child flat.Node) types.Type {
	if child.kind == .ident && g.current_param_is_mut(child.value) {
		if param_type := g.current_param_type(child.value) {
			if param_type is types.Pointer {
				if map_str_clean_type(param_type.base_type) is types.Map {
					return param_type
				}
				return param_type.base_type
			}
			return param_type
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

fn string_interp_type_name(typ types.Type) string {
	mut name := types.Type(typ).name()
	if name.starts_with('builtin.') {
		name = name.all_after_last('.')
	}
	return name
}

fn is_string_interp_float_type(name string) bool {
	return name in ['f32', 'f64', 'float_literal']
}

fn is_string_interp_signed_int_type(name string) bool {
	return name in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'int_literal']
}

fn is_string_interp_unsigned_int_type(name string) bool {
	return name in ['u8', 'byte', 'u16', 'u32', 'u64', 'usize']
}

fn (mut g FlatGen) gen_formatted_string_interp_child_expr(child_id flat.NodeId, typ types.Type, format string) bool {
	f := parse_string_interp_format(format)
	type_name := string_interp_type_name(typ)
	left := if f.left { 1 } else { 0 }
	// An unsigned-backed enum must format as unsigned so values >= 1<<63 are not
	// rendered as negative; consult the enum backing type like the transformer does.
	enum_unsigned := if typ is types.Enum {
		enum_storage_c_type_is_unsigned(g.enum_storage_c_type(typ))
	} else {
		false
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
		g.write('${g.cname(typ.name)}__autostr(')
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
		precision := if f.verb == `f` {
			f.precision
		} else if f.precision > 0 {
			f.precision - 1
		} else {
			0
		}
		g.write('v3_string_pad(v3_f64_fixed((double)(')
		g.gen_string_interp_child_expr(child_id)
		g.write('), ${precision}), ${f.width}, ${left})')
		return true
	}
	if typ is types.String || type_name == 'string' {
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
	for i := start; i < g.str_lits.len; i++ {
		s := g.str_lits[i]
		escaped := c_escape(s)
		storage := if g.cache_split { 'static ' } else { '' }
		g.writeln('${storage}string _str_${i} = {"${escaped}", ${s.len}, 1};')
	}
	if g.str_lits.len > start {
		g.writeln('')
	}
}

// intern_string supports intern string handling for FlatGen.
fn (mut g FlatGen) intern_string(s string) int {
	if s in g.str_lit_ids {
		return g.str_lit_ids[s]
	}
	id := g.str_lits.len
	g.str_lits << s
	g.str_lit_ids[s] = id
	return id
}
