module c

import v3.flat
import v3.types

// optional_type_name supports optional type name handling for FlatGen.
fn (mut g FlatGen) optional_type_name(t types.Type) string {
	mut base_type := types.Type(types.void_)
	if t is types.OptionType {
		base_type = t.base_type
	} else if t is types.ResultType {
		base_type = t.base_type
	} else {
		return g.tc.c_type(t)
	}

	if base_type is types.Void {
		return 'Optional'
	}
	mut inner_ct := g.tc.c_type(base_type)
	if inner_ct.starts_with('fn_ptr:') {
		inner_ct = g.resolve_fn_ptr_type(inner_ct)
	}
	if inner_ct == 'int' {
		return 'Optional'
	}
	safe_name := inner_ct.replace('*', 'ptr').replace(' ', '_')
	opt_name := 'Optional_${safe_name}'
	g.needed_optional_types[opt_name] = inner_ct
	return opt_name
}

fn (mut g FlatGen) optional_type_name_for_context(t types.Type, concrete_optional bool) string {
	if concrete_optional && (t is types.OptionType || t is types.ResultType) {
		return g.concrete_optional_type_name(t)
	}
	return g.optional_type_name(t)
}

fn (mut g FlatGen) value_c_type(t types.Type) string {
	if t is types.OptionType || t is types.ResultType {
		return g.optional_type_name(t)
	}
	mut ct := g.tc.c_type(t)
	if ct.starts_with('fn_ptr:') {
		ct = g.resolve_fn_ptr_type(ct)
	}
	return ct
}

fn (mut g FlatGen) value_sizeof_target(t types.Type) string {
	if fixed := array_fixed_type(t) {
		c_elem, dims := g.fixed_array_decl_parts(fixed)
		return '${c_elem}${dims}'
	}
	return g.value_c_type(t)
}

fn (mut g FlatGen) cast_c_type(t types.Type) string {
	if t is types.Pointer {
		return '${g.value_c_type(t.base_type)}*'
	}
	return g.value_c_type(t)
}

// optional_value_ct supports optional value ct handling for FlatGen.
fn (mut g FlatGen) optional_value_ct(t types.Type) (string, types.Type) {
	if t is types.OptionType {
		if t.base_type is types.Void {
			return 'int', types.Type(types.int_)
		}
		return g.tc.c_type(t.base_type), t.base_type
	} else if t is types.ResultType {
		if t.base_type is types.Void {
			return 'int', types.Type(types.int_)
		}
		return g.tc.c_type(t.base_type), t.base_type
	}
	return 'int', types.Type(types.int_)
}

// optional_typedefs supports optional typedefs handling for FlatGen.
fn (mut g FlatGen) optional_typedefs() {
	g.collect_optional_typedefs()
	mut wrote := false
	for opt_name, val_type in g.needed_optional_types {
		if g.emit_optional_typedef(opt_name, val_type) {
			wrote = true
		}
	}
	if wrote {
		g.writeln('')
	}
}

fn (mut g FlatGen) collect_optional_typedefs() {
	for _, ret in g.tc.fn_ret_types {
		g.collect_optional_typedef_type(ret)
	}
	for _, params in g.tc.fn_param_types {
		for param in params {
			g.collect_optional_typedef_type(param)
		}
	}
	for _, fields in g.tc.structs {
		for field in fields {
			g.collect_optional_typedef_type(field.typ)
		}
	}
	for _, fields in g.tc.interface_fields {
		for field in fields {
			g.collect_optional_typedef_type(field.typ)
		}
	}
	for _, typ in g.tc.c_globals {
		g.collect_optional_typedef_type(typ)
	}
	for _, typ in g.tc.const_types {
		g.collect_optional_typedef_type(typ)
	}
	for idx, is_set in g.tc.expr_type_set {
		if !is_set || idx >= g.tc.expr_type_values.len {
			continue
		}
		g.collect_optional_typedef_type(g.tc.expr_type_values[idx])
	}
	for idx, node in g.a.nodes {
		if node.kind != .call || (idx < g.tc.expr_type_set.len && g.tc.expr_type_set[idx]) {
			continue
		}
		if node.typ.len > 0 && node.typ !in ['int', 'array', 'map', 'unknown'] {
			g.collect_optional_typedef_type(g.tc.parse_type(node.typ))
		} else if idx < g.tc.resolved_call_set.len && g.tc.resolved_call_set[idx] {
			name := g.tc.resolved_call_names[idx]
			if typ := g.tc.fn_ret_types[name] {
				g.collect_optional_typedef_type(typ)
			}
		}
	}
}

fn (mut g FlatGen) collect_optional_typedef_type(t types.Type) {
	match t {
		types.OptionType {
			g.optional_type_name(t)
			g.collect_optional_typedef_type(t.base_type)
		}
		types.ResultType {
			g.optional_type_name(t)
			g.collect_optional_typedef_type(t.base_type)
		}
		types.Array {
			g.collect_optional_typedef_type(t.elem_type)
		}
		types.ArrayFixed {
			g.collect_optional_typedef_type(t.elem_type)
		}
		types.Channel {
			g.collect_optional_typedef_type(t.elem_type)
		}
		types.Map {
			g.collect_optional_typedef_type(t.key_type)
			g.collect_optional_typedef_type(t.value_type)
		}
		types.Pointer {
			g.collect_optional_typedef_type(t.base_type)
		}
		types.FnType {
			for param in t.params {
				g.collect_optional_typedef_type(param)
			}
			g.collect_optional_typedef_type(t.return_type)
		}
		types.Alias {
			g.collect_optional_typedef_type(t.base_type)
		}
		types.MultiReturn {
			for typ in t.types {
				g.collect_optional_typedef_type(typ)
			}
		}
		else {}
	}
}

// emit_optional_typedef emits emit optional typedef output for c.
fn (mut g FlatGen) emit_optional_typedef(opt_name string, val_type string) bool {
	if opt_name in g.emitted_optional_types {
		return false
	}
	err_field := if g.has_ierror_interface() { 'IError err; ' } else { '' }
	g.writeln('typedef struct ${opt_name} { bool ok; ${err_field}${val_type} value; } ${opt_name};')
	g.emitted_optional_types[opt_name] = true
	return true
}

// enum_decls supports enum decls handling for FlatGen.
fn (mut g FlatGen) enum_decls() {
	mut cur_module := ''
	mut emitted := map[string]bool{}
	for node in g.a.nodes {
		match node.kind {
			.file {
				cur_module = ''
			}
			.module_decl {
				cur_module = node.value
			}
			.enum_decl {
				name := if cur_module.len > 0 && cur_module != 'main' && cur_module != 'builtin' {
					'${cur_module}.${node.value}'
				} else {
					node.value
				}
				cn := c_name(name)
				if emitted[cn] {
					continue
				}
				emitted[cn] = true
				g.writeln('typedef enum {')
				is_flag := node.typ == 'flag'
				mut val := 0
				for i in 0 .. node.children_count {
					f := g.a.child_node(&node, i)
					if f.children_count > 0 {
						if enum_val := g.enum_field_expr_value(g.a.child(f, 0)) {
							val = enum_val
						}
					}
					cfield := c_name(f.value)
					if is_flag {
						g.writeln('\t${cn}__${cfield} = ${1 << val},')
						val++
					} else {
						g.writeln('\t${cn}__${cfield} = ${val},')
						val++
					}
				}
				g.writeln('} ${cn};')
				g.writeln('')
			}
			else {}
		}
	}
}

// enum_str_forward_decls forward-declares the synthesized `<Enum>__autostr` helpers so
// const initializers / function bodies emitted later can call them. Bodies come from
// enum_str_defs (after `string` and `strconv__format_int` are available).
fn (mut g FlatGen) enum_str_forward_decls() {
	mut cur_module := ''
	mut emitted := map[string]bool{}
	for node in g.a.nodes {
		match node.kind {
			.file {
				cur_module = ''
			}
			.module_decl {
				cur_module = node.value
			}
			.enum_decl {
				name := if cur_module.len > 0 && cur_module != 'main' && cur_module != 'builtin' {
					'${cur_module}.${node.value}'
				} else {
					node.value
				}
				cn := c_name(name)
				if emitted[cn] {
					continue
				}
				emitted[cn] = true
				g.writeln('string ${cn}__autostr(${cn} it);')
			}
			else {}
		}
	}
	g.writeln('')
}

// enum_str_defs emits a `<Enum>__autostr` helper per enum: a switch mapping each field's
// value to its NAME string literal (V's auto-derived `.str()`), falling back to the integer
// for out-of-range / combined-flag values. This is what `${enum}` interpolation calls when
// the user has not defined a custom `.str()`.
fn (mut g FlatGen) enum_str_defs() {
	mut cur_module := ''
	mut emitted := map[string]bool{}
	for node in g.a.nodes {
		match node.kind {
			.file {
				cur_module = ''
			}
			.module_decl {
				cur_module = node.value
			}
			.enum_decl {
				name := if cur_module.len > 0 && cur_module != 'main' && cur_module != 'builtin' {
					'${cur_module}.${node.value}'
				} else {
					node.value
				}
				cn := c_name(name)
				if emitted[cn] {
					continue
				}
				emitted[cn] = true
				if node.typ == 'flag' {
					// `[flag]` enum: a value can combine several bits, so build the V
					// `Enum{.a | .b}` form by testing each field bit instead of matching a
					// single case (which would send any combination to the integer path).
					g.emit_flag_enum_autostr(node, cn)
				} else {
					g.writeln('string ${cn}__autostr(${cn} it) {')
					g.writeln('\tswitch (it) {')
					mut val := 0
					mut seen := map[int]bool{}
					for i in 0 .. node.children_count {
						f := g.a.child_node(&node, i)
						if f.children_count > 0 {
							if enum_val := g.enum_field_expr_value(g.a.child(f, 0)) {
								val = enum_val
							}
						}
						case_val := val
						val++
						// Duplicate field values would produce duplicate C `case` labels; keep first.
						if case_val in seen {
							continue
						}
						seen[case_val] = true
						fname := f.value
						cfield := c_name(fname)
						g.writeln('\t\tcase ${cn}__${cfield}: return (string){.str = (u8*)"${fname}", .len = ${fname.len}, .is_lit = 1};')
					}
					g.writeln('\t\tdefault: break;')
					g.writeln('\t}')
					g.writeln('\treturn strconv__format_int((i64)it, 10);')
					g.writeln('}')
					g.writeln('')
				}
			}
			else {}
		}
	}
}

// emit_flag_enum_autostr emits the `<Enum>__autostr` helper for a `[flag]` enum.
// Matching V, a combined value is rendered as `Enum{.a | .b}` by testing each
// field's bit; `Enum(0)` renders as `Enum{}`.
fn (mut g FlatGen) emit_flag_enum_autostr(node flat.Node, cn string) {
	short := node.value.all_after_last('.')
	g.writeln('string ${cn}__autostr(${cn} it) {')
	g.writeln('\tint __fe_v = (int)it;')
	g.writeln('\tstring __fe_res = (string){.str = (u8*)"${short}{", .len = ${short.len + 1}, .is_lit = 1};')
	g.writeln('\tbool __fe_first = true;')
	mut val := 0
	mut seen := map[int]bool{}
	for i in 0 .. node.children_count {
		f := g.a.child_node(&node, i)
		if f.children_count > 0 {
			if enum_val := g.enum_field_expr_value(g.a.child(f, 0)) {
				val = enum_val
			}
		}
		bit := 1 << val
		val++
		if bit in seen {
			continue
		}
		seen[bit] = true
		fname := f.value
		cfield := c_name(fname)
		g.writeln('\tif (${cn}__${cfield} != 0 && (__fe_v & ${cn}__${cfield}) == ${cn}__${cfield}) {')
		g.writeln('\t\tif (!__fe_first) { __fe_res = string__plus(__fe_res, (string){.str = (u8*)" | ", .len = 3, .is_lit = 1}); }')
		g.writeln('\t\t__fe_res = string__plus(__fe_res, (string){.str = (u8*)".${fname}", .len = ${
			fname.len + 1}, .is_lit = 1});')
		g.writeln('\t\t__fe_first = false;')
		g.writeln('\t}')
	}
	g.writeln('\t__fe_res = string__plus(__fe_res, (string){.str = (u8*)"}", .len = 1, .is_lit = 1});')
	g.writeln('\treturn __fe_res;')
	g.writeln('}')
	g.writeln('')
}

// enum_field_expr_value supports enum field expr value handling for FlatGen.
fn (g &FlatGen) enum_field_expr_value(id flat.NodeId) ?int {
	if int(id) < 0 {
		return none
	}
	node := g.a.nodes[int(id)]
	match node.kind {
		.int_literal {
			return node.value.int()
		}
		.paren {
			if node.children_count == 0 {
				return none
			}
			return g.enum_field_expr_value(g.a.child(&node, 0))
		}
		.prefix {
			if node.children_count == 0 {
				return none
			}
			value := g.enum_field_expr_value(g.a.child(&node, 0))?
			return match node.op {
				.plus { value }
				.minus { -value }
				.bit_not { ~value }
				else { none }
			}
		}
		.infix {
			if node.children_count < 2 {
				return none
			}
			left := g.enum_field_expr_value(g.a.child(&node, 0))?
			right := g.enum_field_expr_value(g.a.child(&node, 1))?
			return match node.op {
				.plus {
					left + right
				}
				.minus {
					left - right
				}
				.mul {
					left * right
				}
				.div {
					if right == 0 {
						none
					} else {
						left / right
					}
				}
				.mod {
					if right == 0 {
						none
					} else {
						left % right
					}
				}
				.amp {
					left & right
				}
				.pipe {
					left | right
				}
				.xor {
					left ^ right
				}
				.left_shift {
					int(u64(left) << right)
				}
				.right_shift, .right_shift_unsigned {
					left >> right
				}
				else {
					none
				}
			}
		}
		else {
			return none
		}
	}
}

// type_alias_decls returns type alias decls data for FlatGen.
fn (mut g FlatGen) type_alias_decls() {
	mut emitted := false
	for name, target in g.tc.type_aliases {
		if target.starts_with('fn_ptr:') || target.starts_with('C.') {
			continue
		}
		if g.has_builtins {
			continue
		}
		ct := g.tc.c_type(g.tc.parse_type(target))
		if ct == 'void' || ct == name {
			continue
		}
		g.writeln('typedef ${ct} ${c_name(name)};')
		emitted = true
	}
	if emitted {
		g.writeln('')
	}
}
