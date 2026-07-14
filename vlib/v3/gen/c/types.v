module c

import strconv
import v3.flat
import v3.gen.c.naming
import v3.types

fn enum_decl_is_flag(node flat.Node) bool {
	return node.typ == 'flag'
}

fn enum_decl_backing_type(node flat.Node) ?string {
	if node.generic_params.len > 0 && node.generic_params[0].len > 0 {
		return node.generic_params[0]
	}
	return none
}

fn (g &FlatGen) enum_backing_storage_c_type(backing string) string {
	return g.tc.c_type(g.tc.parse_type(backing))
}

fn (g &FlatGen) enum_emit_storage_c_type(enum_name string, backing string) string {
	if info := g.enum_backing_info(enum_name) {
		return info.storage_c_type
	}
	return g.enum_backing_storage_c_type(backing)
}

fn enum_storage_c_type_is_unsigned(storage_ct string) bool {
	return storage_ct in ['u8', 'u16', 'u32', 'u64', 'size_t']
}

fn (mut g FlatGen) register_enum_backing_info(enum_name string, backing string) {
	info := EnumBackingInfo{
		c_name:         g.cname(enum_name)
		storage_c_type: g.enum_backing_storage_c_type(backing)
	}
	g.enum_backing_infos[enum_name] = info
	short := enum_name.all_after_last('.')
	if short !in g.enum_backing_infos {
		g.enum_backing_infos[short] = info
	}
}

fn (g &FlatGen) enum_backing_info(enum_name string) ?EnumBackingInfo {
	if info := g.enum_backing_infos[enum_name] {
		return info
	}
	short := enum_name.all_after_last('.')
	if info := g.enum_backing_infos[short] {
		return info
	}
	return none
}

fn (g &FlatGen) enum_value_c_type(enum_type types.Enum) string {
	if info := g.enum_backing_info(enum_type.name) {
		return info.c_name
	}
	return g.tc.c_type(enum_type)
}

fn (g &FlatGen) enum_storage_c_type(enum_type types.Enum) string {
	if info := g.enum_backing_info(enum_type.name) {
		return info.storage_c_type
	}
	return g.tc.c_type(enum_type)
}

// optional_type_name supports optional type name handling for FlatGen.
fn (mut g FlatGen) optional_type_name(t types.Type) string {
	mut base_type := types.Type(types.void_)
	if t is types.OptionType {
		base_type = t.base_type
	} else if t is types.ResultType {
		base_type = t.base_type
	} else {
		if t is types.MultiReturn {
			// The checker-level name spells fn-type parts as `fn_ptr_void_void`;
			// the emitted typedef uses the resolved `_fn_ptr_<hash>` form.
			return g.multi_return_c_type_name(t)
		}
		return g.tc.c_type(t)
	}

	if base_type is types.Void {
		return 'Optional'
	}
	if g.type_contains_generic_placeholder(base_type) {
		return 'Optional'
	}
	mut inner_ct := g.optional_payload_c_type(base_type)
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
	if t is types.MultiReturn {
		return g.multi_return_c_type_name(t)
	}
	if t is types.Enum {
		return g.enum_value_c_type(t)
	}
	if t is types.ArrayFixed {
		return g.fixed_array_c_type(t)
	}
	mut ct := g.tc.c_type(t)
	if ct.starts_with('fn_ptr:') {
		ct = g.resolve_fn_ptr_type(ct)
	}
	return ct
}

fn (mut g FlatGen) multi_return_c_type_name(t types.MultiReturn) string {
	mut parts := []string{cap: t.types.len}
	for item in t.types {
		parts << naming.type_name_part(g.multi_return_field_c_type(item))
	}
	return 'multi_return_${parts.join('_')}'
}

fn (mut g FlatGen) multi_return_field_c_type(t types.Type) string {
	// Plain enums use integer storage in the C ABI, while backed enums use
	// their emitted typedef so wide values keep the declared storage width.
	if t is types.Enum {
		return g.enum_value_c_type(t)
	}
	return g.value_c_type(t)
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
		return g.optional_payload_c_type(t.base_type), t.base_type
	} else if t is types.ResultType {
		if t.base_type is types.Void {
			return 'int', types.Type(types.int_)
		}
		return g.optional_payload_c_type(t.base_type), t.base_type
	}
	return 'int', types.Type(types.int_)
}

fn (mut g FlatGen) optional_payload_c_type(t types.Type) string {
	if t is types.ArrayFixed {
		return g.fixed_array_c_type(t)
	}
	return g.value_c_type(t)
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
	if g.type_contains_generic_placeholder(t) {
		return
	}
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

fn (g &FlatGen) type_contains_generic_placeholder(t types.Type) bool {
	match t {
		types.Unknown {
			return true
		}
		types.Array {
			return g.type_contains_generic_placeholder(t.elem_type)
		}
		types.ArrayFixed {
			return g.type_contains_generic_placeholder(t.elem_type)
		}
		types.Channel {
			return g.type_contains_generic_placeholder(t.elem_type)
		}
		types.Map {
			return g.type_contains_generic_placeholder(t.key_type)
				|| g.type_contains_generic_placeholder(t.value_type)
		}
		types.Pointer {
			return g.type_contains_generic_placeholder(t.base_type)
		}
		types.FnType {
			for param in t.params {
				if g.type_contains_generic_placeholder(param) {
					return true
				}
			}
			return g.type_contains_generic_placeholder(t.return_type)
		}
		types.OptionType {
			return g.type_contains_generic_placeholder(t.base_type)
		}
		types.ResultType {
			return g.type_contains_generic_placeholder(t.base_type)
		}
		types.Struct {
			return g.type_name_contains_generic_placeholder(t.name)
		}
		types.Interface {
			return g.type_name_contains_generic_placeholder(t.name)
		}
		types.Enum {
			return g.type_name_contains_generic_placeholder(t.name)
		}
		types.SumType {
			return g.type_name_contains_generic_placeholder(t.name)
		}
		types.Alias {
			return g.type_name_contains_generic_placeholder(t.name)
				|| g.type_contains_generic_placeholder(t.base_type)
		}
		types.MultiReturn {
			for typ in t.types {
				if g.type_contains_generic_placeholder(typ) {
					return true
				}
			}
			return false
		}
		else {
			return false
		}
	}
}

fn (g &FlatGen) type_name_contains_generic_placeholder(name string) bool {
	clean := trimmed_space(name)
	if clean.len == 0 {
		return false
	}
	if !clean.contains('[') {
		return g.is_bare_generic_placeholder_name(clean)
	}
	mut depth := 0
	mut start := 0
	for i in 0 .. clean.len {
		ch := clean[i]
		if ch == `[` {
			if depth == 0 {
				start = i + 1
			}
			depth++
		} else if ch == `]` {
			depth--
			if depth == 0 {
				if g.generic_args_contain_placeholder(clean[start..i]) {
					return true
				}
			}
		}
	}
	return false
}

fn (g &FlatGen) generic_args_contain_placeholder(args string) bool {
	mut depth := 0
	mut start := 0
	for i in 0 .. args.len {
		ch := args[i]
		if ch == `[` || ch == `(` {
			depth++
		} else if ch == `]` || ch == `)` {
			if depth > 0 {
				depth--
			}
		} else if ch == `,` && depth == 0 {
			if g.type_name_contains_generic_placeholder(args[start..i].trim_space()) {
				return true
			}
			start = i + 1
		}
	}
	return g.type_name_contains_generic_placeholder(args[start..].trim_space())
}

fn (g &FlatGen) is_bare_generic_placeholder_name(name string) bool {
	if name.len != 1 || name[0] < `A` || name[0] > `Z` {
		return false
	}
	if types.is_builtin_type_name(name) || name in ['C', 'JS'] {
		return false
	}
	return !g.type_name_known(name)
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
				cn := g.cname(name)
				if emitted[cn] {
					continue
				}
				emitted[cn] = true
				is_flag := enum_decl_is_flag(node)
				if backing := enum_decl_backing_type(node) {
					storage_ct := g.enum_emit_storage_c_type(name, backing)
					g.writeln('typedef ${storage_ct} ${cn};')
					if is_flag {
						mut val := 0
						for i in 0 .. node.children_count {
							f := g.a.child_node(&node, i)
							if f.children_count > 0 {
								if enum_val := g.enum_field_expr_value(g.a.child(f, 0)) {
									val = enum_val
								}
							}
							cfield := g.cname(f.value)
							g.writeln('static const ${cn} ${cn}__${cfield} = (${cn})((${storage_ct})1 << ${val});')
							val++
						}
					} else {
						mut field_names := map[string]bool{}
						mut field_exprs := map[string]flat.NodeId{}
						for i in 0 .. node.children_count {
							f := g.a.child_node(&node, i)
							field_names[f.value] = true
							if f.children_count > 0 {
								field_exprs[f.value] = g.a.child(f, 0)
							}
						}
						mut field_values := map[string]i64{}
						mut next_value := i64(0)
						mut next_value_known := true
						mut next_value_expr := '0'
						for i in 0 .. node.children_count {
							f := g.a.child_node(&node, i)
							mut value := next_value
							mut value_known := next_value_known
							mut value_expr := if next_value_known {
								value.str()
							} else {
								next_value_expr
							}
							if f.children_count > 0 {
								expr_id := g.a.child(f, 0)
								mut resolving := map[string]bool{}
								if enum_val := g.enum_field_expr_value_with_enum(expr_id,
									cur_module, node.value, mut field_values, field_exprs, mut
									resolving)
								{
									value = enum_val
									value_known = true
									value_expr = enum_val.str()
								} else {
									// Preserve expressions outside V's 32-bit `int` range so the C
									// storage type can represent wide backed enum values without truncation.
									value_known = false
									value_expr = g.enum_field_expr_to_string_with_enum(expr_id,
										cur_module, node.value, cn, field_names) or {
										g.expr_to_string(expr_id)
									}
								}
							}
							if value_known {
								field_values[f.value] = value
							}
							cfield := g.cname(f.value)
							g.writeln('#define ${cn}__${cfield} ((${cn})(${value_expr}))')
							if value_known {
								next_value = value + 1
								next_value_known = true
								next_value_expr = next_value.str()
							} else {
								next_value_known = false
								next_value_expr = '(${value_expr}) + 1'
							}
						}
					}
					g.writeln('')
					continue
				}
				g.writeln('typedef enum {')
				mut val := 0
				mut field_values := map[string]i64{}
				mut field_exprs := map[string]flat.NodeId{}
				for i in 0 .. node.children_count {
					f := g.a.child_node(&node, i)
					if f.children_count > 0 {
						field_exprs[f.value] = g.a.child(f, 0)
					}
				}
				for i in 0 .. node.children_count {
					f := g.a.child_node(&node, i)
					if f.children_count > 0 {
						mut resolving := map[string]bool{}
						if enum_val := g.enum_field_expr_value_with_enum(g.a.child(f, 0),
							cur_module, node.value, mut field_values, field_exprs, mut resolving)
						{
							val = int(enum_val)
						}
					}
					field_values[f.value] = i64(val)
					cfield := g.cname(f.value)
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
				cn := g.cname(name)
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
				cn := g.cname(name)
				if emitted[cn] {
					continue
				}
				emitted[cn] = true
				if node.typ == 'flag' {
					// `[flag]` enum: a value can combine several bits, so build the V
					// `Enum{.a | .b}` form by testing each field bit instead of matching a
					// single case (which would send any combination to the integer path).
					g.emit_flag_enum_autostr(node, name, cn)
				} else if backing := enum_decl_backing_type(node) {
					storage_ct := g.enum_emit_storage_c_type(name, backing)
					g.writeln('string ${cn}__autostr(${cn} it) {')
					for i in 0 .. node.children_count {
						f := g.a.child_node(&node, i)
						fname := f.value
						cfield := g.cname(fname)
						g.writeln('\tif (it == ${cn}__${cfield}) return (string){.str = (u8*)"${fname}", .len = ${fname.len}, .is_lit = 1};')
					}
					if enum_storage_c_type_is_unsigned(storage_ct) {
						g.writeln('\treturn strconv__format_uint((u64)(${storage_ct})it, 10);')
					} else {
						g.writeln('\treturn strconv__format_int((i64)(${storage_ct})it, 10);')
					}
					g.writeln('}')
					g.writeln('')
				} else {
					g.writeln('string ${cn}__autostr(${cn} it) {')
					for i in 0 .. node.children_count {
						f := g.a.child_node(&node, i)
						fname := f.value
						cfield := g.cname(fname)
						// Use ordered comparisons instead of switch cases: enums may opt in
						// to duplicate values, and the first declared name is their auto-str.
						g.writeln('\tif (it == ${cn}__${cfield}) return (string){.str = (u8*)"${fname}", .len = ${fname.len}, .is_lit = 1};')
					}
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
fn (mut g FlatGen) emit_flag_enum_autostr(node flat.Node, name string, cn string) {
	short := node.value.all_after_last('.')
	mut storage_ct := 'int'
	if backing := enum_decl_backing_type(node) {
		storage_ct = g.enum_emit_storage_c_type(name, backing)
	}
	g.writeln('string ${cn}__autostr(${cn} it) {')
	g.writeln('\t${storage_ct} __fe_v = (${storage_ct})it;')
	g.writeln('\tstring __fe_res = (string){.str = (u8*)"${short}{", .len = ${short.len + 1}, .is_lit = 1};')
	g.writeln('\tbool __fe_first = true;')
	mut val := 0
	mut seen := map[int]bool{}
	mut field_exprs := map[string]flat.NodeId{}
	for i in 0 .. node.children_count {
		f := g.a.child_node(&node, i)
		if f.children_count > 0 {
			field_exprs[f.value] = g.a.child(f, 0)
		}
	}
	mut field_values := map[string]i64{}
	enum_module := if name.contains('.') { name.all_before_last('.') } else { '' }
	for i in 0 .. node.children_count {
		f := g.a.child_node(&node, i)
		if f.children_count > 0 {
			mut resolving := map[string]bool{}
			if enum_val := g.enum_field_expr_value_with_enum(g.a.child(f, 0), enum_module,
				node.value, mut field_values, field_exprs, mut resolving)
			{
				val = int(enum_val)
			}
		}
		field_values[f.value] = i64(val)
		if val in seen {
			val++
			continue
		}
		seen[val] = true
		val++
		fname := f.value
		cfield := g.cname(fname)
		field_expr := '${cn}__${cfield}'
		g.writeln('\tif (${field_expr} != 0 && (__fe_v & (${storage_ct})${field_expr}) == (${storage_ct})${field_expr}) {')
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
				.right_shift {
					left >> right
				}
				.right_shift_unsigned {
					int(u64(left) >> right)
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

fn (g &FlatGen) enum_field_expr_value_with_enum(id flat.NodeId, enum_module string, enum_name string, mut field_values map[string]i64, field_exprs map[string]flat.NodeId, mut resolving map[string]bool) ?i64 {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return none
	}
	node := g.a.nodes[int(id)]
	match node.kind {
		.int_literal {
			return enum_foldable_int_literal(node.value)
		}
		.ident, .enum_val {
			if ev := g.enum_decl_field_ref_value(node.value, enum_module, enum_name, mut
				field_values, field_exprs, mut resolving)
			{
				return ev
			}
			lookup_module := if enum_module.len > 0 { enum_module } else { g.tc.cur_module }
			return i64(g.tc.const_int_value_in_module(node.value, lookup_module, []string{})?)
		}
		.paren {
			if node.children_count == 0 {
				return none
			}
			return g.enum_field_expr_value_with_enum(g.a.child(&node, 0), enum_module, enum_name, mut
				field_values, field_exprs, mut resolving)
		}
		.cast_expr {
			if node.children_count == 0 {
				return none
			}
			return g.enum_field_expr_value_with_enum(g.a.child(&node, 0), enum_module, enum_name, mut
				field_values, field_exprs, mut resolving)
		}
		.call {
			return g.enum_comptime_call_value(id, enum_module, enum_name, mut field_values,
				field_exprs, mut resolving)
		}
		.prefix {
			if node.children_count == 0 {
				return none
			}
			value := g.enum_field_expr_value_with_enum(g.a.child(&node, 0), enum_module, enum_name, mut
				field_values, field_exprs, mut resolving)?
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
			left := g.enum_field_expr_value_with_enum(g.a.child(&node, 0), enum_module, enum_name, mut
				field_values, field_exprs, mut resolving)?
			right := g.enum_field_expr_value_with_enum(g.a.child(&node, 1), enum_module, enum_name, mut
				field_values, field_exprs, mut resolving)?
			if (node.op == .div || node.op == .mod) && right == 0 {
				return none
			}
			if (node.op == .left_shift || node.op == .right_shift
				|| node.op == .right_shift_unsigned) && (right < 0 || right >= 64) {
				return none
			}
			return match node.op {
				.plus { left + right }
				.minus { left - right }
				.mul { left * right }
				.div { left / right }
				.mod { left % right }
				.amp { left & right }
				.pipe { left | right }
				.xor { left ^ right }
				.left_shift { i64(u64(left) << u64(right)) }
				.right_shift { left >> right }
				.right_shift_unsigned { i64(u64(left) >> u64(right)) }
				else { none }
			}
		}
		.selector {
			if field := g.enum_decl_selector_ref_field(id, enum_module, enum_name) {
				return g.enum_decl_field_ref_value(field, enum_module, enum_name, mut field_values,
					field_exprs, mut resolving)
			}
			return none
		}
		else {
			return none
		}
	}
}

fn (g &FlatGen) enum_comptime_call_value(id flat.NodeId, enum_module string, enum_name string, mut field_values map[string]i64, field_exprs map[string]flat.NodeId, mut resolving map[string]bool) ?i64 {
	call := g.a.nodes[int(id)]
	if call.children_count == 0 {
		return none
	}
	callee := g.a.child_node(&call, 0)
	if callee.kind != .ident {
		return none
	}
	// An unqualified helper call in an enum initializer resolves to a function in the
	// enum's own module, so prefer an exact candidate declared in `enum_module` before
	// a same-module short-name (receiver/static method) suffix match. Fall back to an
	// exact name match and then a suffix match across modules.
	short := callee.value.all_after_last('.')
	mut cur_mod := ''
	mut module_exact_node := flat.Node{}
	mut module_exact_found := false
	mut module_suffix_node := flat.Node{}
	mut module_suffix_found := false
	mut exact_node := flat.Node{}
	mut exact_found := false
	mut suffix_node := flat.Node{}
	mut suffix_found := false
	for candidate in g.a.nodes {
		if candidate.kind == .file {
			cur_mod = ''
			continue
		}
		if candidate.kind == .module_decl {
			cur_mod = candidate.value
			continue
		}
		if candidate.kind != .fn_decl {
			continue
		}
		if candidate.value != callee.value && candidate.value.all_after_last('.') != short {
			continue
		}
		if cur_mod == enum_module {
			if !module_exact_found && candidate.value == callee.value {
				module_exact_node = candidate
				module_exact_found = true
			} else if !module_suffix_found {
				module_suffix_node = candidate
				module_suffix_found = true
			}
		}
		if !exact_found && candidate.value == callee.value {
			exact_node = candidate
			exact_found = true
		}
		if !suffix_found {
			suffix_node = candidate
			suffix_found = true
		}
	}
	fn_node := if module_exact_found {
		module_exact_node
	} else if module_suffix_found {
		module_suffix_node
	} else if exact_found {
		exact_node
	} else if suffix_found {
		suffix_node
	} else {
		return none
	}
	mut locals := map[string]i64{}
	mut arg_idx := 1
	for i in 0 .. fn_node.children_count {
		param := g.a.child_node(&fn_node, i)
		if param.kind != .param {
			continue
		}
		if arg_idx >= call.children_count {
			return none
		}
		arg_id := g.a.child(&call, arg_idx)
		locals[param.value] = g.enum_field_expr_value_with_enum(arg_id, enum_module, enum_name, mut
			field_values, field_exprs, mut resolving)?
		arg_idx++
	}
	for i in 0 .. fn_node.children_count {
		stmt := g.a.child_node(&fn_node, i)
		if stmt.kind in [.decl_assign, .assign] {
			g.enum_comptime_update_locals(stmt, mut locals, enum_module)
			continue
		}
		if stmt.kind == .return_stmt && stmt.children_count > 0 {
			return g.enum_comptime_expr_value(g.a.child(stmt, 0), locals, enum_module)
		}
	}
	return none
}

fn (g &FlatGen) enum_comptime_update_locals(stmt flat.Node, mut locals map[string]i64, enum_module string) {
	if stmt.children_count < 2 || stmt.children_count % 2 != 0 {
		return
	}
	mut i := 0
	for i < stmt.children_count {
		lhs := g.a.child_node(&stmt, i)
		if lhs.kind == .ident && lhs.value.len > 0 && lhs.value != '_' {
			if stmt.kind == .decl_assign || stmt.op == .assign {
				rhs_id := g.a.child(&stmt, i + 1)
				if value := g.enum_comptime_expr_value(rhs_id, locals, enum_module) {
					locals[lhs.value] = value
				} else {
					locals.delete(lhs.value)
				}
			} else {
				locals.delete(lhs.value)
			}
		}
		i += 2
	}
}

fn (g &FlatGen) enum_comptime_expr_value(id flat.NodeId, locals map[string]i64, enum_module string) ?i64 {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return none
	}
	node := g.a.nodes[int(id)]
	match node.kind {
		.int_literal {
			return enum_foldable_int_literal(node.value)
		}
		.ident {
			if value := locals[node.value] {
				return value
			}
			lookup_module := if enum_module.len > 0 { enum_module } else { g.tc.cur_module }
			return i64(g.tc.const_int_value_in_module(node.value, lookup_module, []string{})?)
		}
		.paren, .cast_expr {
			if node.children_count == 0 {
				return none
			}
			return g.enum_comptime_expr_value(g.a.child(&node, 0), locals, enum_module)
		}
		.prefix {
			if node.children_count == 0 {
				return none
			}
			value := g.enum_comptime_expr_value(g.a.child(&node, 0), locals, enum_module)?
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
			left := g.enum_comptime_expr_value(g.a.child(&node, 0), locals, enum_module)?
			right := g.enum_comptime_expr_value(g.a.child(&node, 1), locals, enum_module)?
			if (node.op in [.div, .mod] && right == 0)
				|| (node.op in [.left_shift, .right_shift, .right_shift_unsigned] && (right < 0
				|| right >= 64)) {
				return none
			}
			return match node.op {
				.plus { left + right }
				.minus { left - right }
				.mul { left * right }
				.div { left / right }
				.mod { left % right }
				.amp { left & right }
				.pipe { left | right }
				.xor { left ^ right }
				.left_shift { i64(u64(left) << u64(right)) }
				.right_shift { left >> right }
				.right_shift_unsigned { i64(u64(left) >> u64(right)) }
				else { none }
			}
		}
		else {
			return none
		}
	}
}

fn enum_foldable_int_literal(value string) ?i64 {
	clean := value.replace('_', '')
	parsed := strconv.common_parse_int(clean, 0, 64, true, true) or { return none }
	return parsed
}

fn (mut g FlatGen) enum_field_expr_to_string_with_enum(id flat.NodeId, enum_module string, enum_name string, enum_c_name string, field_names map[string]bool) ?string {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return none
	}
	node := g.a.nodes[int(id)]
	match node.kind {
		.ident, .enum_val {
			if node.value in field_names {
				return '${enum_c_name}__${g.cname(node.value)}'
			}
			return g.expr_to_string(id)
		}
		.selector {
			if field := g.enum_decl_selector_ref_field(id, enum_module, enum_name) {
				if field in field_names {
					return '${enum_c_name}__${g.cname(field)}'
				}
			}
			return g.expr_to_string(id)
		}
		.int_literal, .bool_literal, .char_literal, .string_literal {
			return g.expr_to_string(id)
		}
		.paren {
			if node.children_count == 0 {
				return none
			}
			inner := g.enum_field_expr_to_string_with_enum(g.a.child(&node, 0), enum_module,
				enum_name, enum_c_name, field_names)?
			return '(${inner})'
		}
		.cast_expr {
			if node.children_count == 0 {
				return none
			}
			target_type := g.tc.parse_type(node.value)
			mut ct := g.cast_c_type(target_type)
			if ct.starts_with('fn_ptr:') {
				ct = g.resolve_fn_ptr_type(ct)
			}
			inner := g.enum_field_expr_to_string_with_enum(g.a.child(&node, 0), enum_module,
				enum_name, enum_c_name, field_names)?
			return '(${ct})(${inner})'
		}
		.prefix {
			if node.children_count == 0 {
				return none
			}
			op := g.op_str(node.op)
			if op.len == 0 {
				return none
			}
			inner := g.enum_field_expr_to_string_with_enum(g.a.child(&node, 0), enum_module,
				enum_name, enum_c_name, field_names)?
			return '${op}${inner}'
		}
		.infix {
			if node.children_count < 2 {
				return none
			}
			op := g.op_str(node.op)
			if op.len == 0 {
				return none
			}
			left := g.enum_field_expr_to_string_with_enum(g.a.child(&node, 0), enum_module,
				enum_name, enum_c_name, field_names)?
			right := g.enum_field_expr_to_string_with_enum(g.a.child(&node, 1), enum_module,
				enum_name, enum_c_name, field_names)?
			return '${left} ${op} ${right}'
		}
		else {
			return g.expr_to_string(id)
		}
	}
}

fn (g &FlatGen) enum_decl_field_ref_value(field_name string, enum_module string, enum_name string, mut field_values map[string]i64, field_exprs map[string]flat.NodeId, mut resolving map[string]bool) ?i64 {
	if field_name in field_values {
		return field_values[field_name]
	}
	expr_id := field_exprs[field_name] or { return none }
	if resolving[field_name] {
		return none
	}
	resolving[field_name] = true
	maybe_val := g.enum_field_expr_value_with_enum(expr_id, enum_module, enum_name, mut
		field_values, field_exprs, mut resolving)
	resolving.delete(field_name)
	val := maybe_val?
	field_values[field_name] = val
	return val
}

fn (g &FlatGen) enum_decl_selector_ref_field(id flat.NodeId, enum_module string, enum_name string) ?string {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return none
	}
	node := g.a.nodes[int(id)]
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	prefix := g.enum_decl_selector_base_text(g.a.child(&node, 0))
	if !enum_ref_prefix_matches(prefix, enum_module, enum_name) {
		return none
	}
	return node.value
}

fn (g &FlatGen) enum_decl_selector_base_text(id flat.NodeId) string {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return ''
	}
	node := g.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value
		}
		.selector {
			if node.children_count == 0 {
				return node.value
			}
			base := g.enum_decl_selector_base_text(g.a.child(&node, 0))
			if base.len == 0 {
				return node.value
			}
			return '${base}.${node.value}'
		}
		else {
			return ''
		}
	}
}

fn enum_ref_prefix_matches(prefix string, enum_module string, enum_name string) bool {
	if prefix.len == 0 || enum_name.len == 0 {
		return false
	}
	short := enum_name.all_after_last('.')
	if prefix == enum_name || prefix == short {
		return true
	}
	if enum_module.len > 0 && prefix == '${enum_module}.${short}' {
		return true
	}
	return false
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
		g.writeln('typedef ${ct} ${g.cname(name)};')
		emitted = true
	}
	if emitted {
		g.writeln('')
	}
}
