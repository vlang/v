module transform

import v3.flat
import v3.types

fn arr1(a flat.NodeId) []flat.NodeId {
	mut r := []flat.NodeId{}
	r << a
	return r
}

fn arr2(a flat.NodeId, b flat.NodeId) []flat.NodeId {
	mut r := []flat.NodeId{}
	r << a
	r << b
	return r
}

fn arr3(a flat.NodeId, b flat.NodeId, c flat.NodeId) []flat.NodeId {
	mut r := []flat.NodeId{}
	r << a
	r << b
	r << c
	return r
}

fn node_kind_id(node flat.Node) int {
	mut kind_id := node.kind_id
	if kind_id == 0 && int(node.kind) != 0 {
		kind_id = int(node.kind)
	}
	return kind_id
}

pub struct SmartcastContext {
pub:
	expr_name     string // the expression being smartcast (e.g. "node")
	variant_name  string // the variant type name (e.g. "Ident")
	sum_type_name string // the parent sum type name (e.g. "Expr")
}

pub struct Transformer {
mut:
	a                     &flat.FlatAst      = unsafe { nil }
	tc                    &types.TypeChecker = unsafe { nil }
	structs               map[string]StructInfo
	unique_fields         map[string]string
	alias_methods         map[string]string
	globals               map[string]string
	sum_types             map[string][]string
	sum_variant_parents   map[string][]string
	sum_variant_fields    map[string]string
	qualified_types       map[string]string
	fn_ret_types          map[string]string
	const_suffixes        map[string]string
	enum_types            map[string][]string
	cur_file              string
	cur_module            string
	cur_fn_name           string
	cur_fn_ret_type       string
	var_types             []VarTypeBinding
	pointer_value_lvalues map[string]bool
	temp_counter          int
	pending_stmts         []flat.NodeId
	smartcast_stack       []SmartcastContext
	in_call_callee        bool
	in_const_init         bool
	in_return_expr        bool
	alias_cache           &AliasCache = unsafe { nil }
	sum_cache             &AliasCache = unsafe { nil }
	used_fns              map[string]bool
}

// AliasCache memoizes normalize_type_alias results. It lives on the heap so the
// many `&Transformer` (read-only) query methods can populate it through the
// pointer. normalize_type_alias is a pure function of (cur_module, typ) plus the
// collected type maps (which never change during transform), so the cache is
// keyed by typ and cleared whenever cur_module changes.
struct AliasCache {
mut:
	module  string
	entries map[string]string
}

pub struct StructInfo {
pub:
	name      string
	module    string
	is_params bool
	fields    []FieldInfo
}

pub struct FieldInfo {
pub:
	name         string
	typ          string
	raw_typ      string
	default_expr flat.NodeId
}

struct TupleBlockParts {
	prefix []flat.NodeId
	values []flat.NodeId
}

struct StructFieldLookup {
	info       StructInfo
	owner_type string
}

struct VarTypeBinding {
	name string
	typ  string
}

struct GenericFnDecl {
	id     flat.NodeId
	node   flat.Node
	module string
	key    string
}

// --- entry point ---

pub fn transform(mut a flat.FlatAst, tc &types.TypeChecker) {
	transform_with_used(mut a, tc, map[string]bool{})
}

pub fn transform_with_used(mut a flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool) map[string]bool {
	mut augmented_used_fns := used_fns.clone()
	mut t := new_transformer(mut a, tc, augmented_used_fns)
	t.prepare()
	t.transform_all()
	return augmented_used_fns
}

pub fn monomorphize_with_used(mut a flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool) map[string]bool {
	mut augmented_used_fns := used_fns.clone()
	mut t := new_transformer(mut a, tc, augmented_used_fns)
	t.prepare()
	for name in t.monomorphize_pass() {
		augmented_used_fns[name] = true
		augmented_used_fns[c_name(name)] = true
		t.used_fns[name] = true
		t.used_fns[c_name(name)] = true
	}
	t.materialize_generic_structs()
	return augmented_used_fns
}

fn new_transformer(mut a flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool) Transformer {
	return Transformer{
		a:                     a
		tc:                    unsafe { tc }
		pointer_value_lvalues: map[string]bool{}
		used_fns:              used_fns.clone()
	}
}

fn (mut t Transformer) prepare() {
	t.collect_types()
	t.collect_const_suffixes()
	t.collect_alias_methods()
	// Enable the alias cache only now that the type maps are fully populated.
	// During collection those maps are incomplete, so caching there would poison
	// entries with results computed against a partial view.
	t.alias_cache = &AliasCache{}
	t.sum_cache = &AliasCache{}
}

fn (mut t Transformer) reset_var_types() {
	t.var_types.clear()
}

fn (mut t Transformer) set_var_type(name string, typ string) {
	if name.len == 0 {
		return
	}
	for i, binding in t.var_types {
		if binding.name == name {
			t.var_types[i] = VarTypeBinding{
				name: name
				typ:  typ
			}
			return
		}
	}
	t.var_types << VarTypeBinding{
		name: name
		typ:  typ
	}
}

fn (mut t Transformer) unset_var_type(name string) {
	for i, binding in t.var_types {
		if binding.name == name {
			t.var_types.delete(i)
			return
		}
	}
}

fn (t &Transformer) var_type(name string) string {
	for binding in t.var_types {
		if binding.name == name {
			return binding.typ
		}
	}
	return ''
}

// --- type collection ---

fn (mut t Transformer) collect_types() {
	mut cur_mod := ''
	for node in t.a.nodes {
		match node.kind {
			.module_decl {
				cur_mod = node.value
			}
			.struct_decl {
				owner_type := if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
					'${cur_mod}.${node.value}'
				} else {
					node.value
				}
				mut fields := []FieldInfo{}
				for i in 0 .. node.children_count {
					f := t.a.child_node(&node, i)
					if f.kind != .field_decl {
						continue
					}
					default_expr := if f.children_count > 0 {
						t.a.child(f, 0)
					} else {
						flat.empty_node
					}
					fields << FieldInfo{
						name:         f.value
						typ:          t.normalize_field_type(f.typ, owner_type)
						raw_typ:      f.typ
						default_expr: default_expr
					}
				}
				info := StructInfo{
					name:      node.value
					module:    cur_mod
					is_params: node.typ.contains('params')
					fields:    fields
				}
				t.structs[node.value] = info
				if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
					qname := '${cur_mod}.${node.value}'
					t.structs[qname] = info
					if node.value !in t.qualified_types {
						t.qualified_types[node.value] = qname
					}
				}
				for f in fields {
					t.add_unique_field_type(f.name, f.typ)
				}
			}
			.type_decl {
				if node.children_count > 0 {
					mut variants := []string{}
					for i in 0 .. node.children_count {
						v := t.a.child_node(&node, i)
						variants << t.normalize_sum_variant_type(v.value, cur_mod)
					}
					t.sum_types[node.value] = variants
					for variant in variants {
						t.add_sum_variant_parent(variant, node.value)
					}
					if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
						qname := '${cur_mod}.${node.value}'
						t.sum_types[qname] = variants
						if node.value !in t.qualified_types {
							t.qualified_types[node.value] = qname
						}
						for variant in variants {
							t.add_sum_variant_parent(variant, qname)
						}
					}
				}
			}
			.enum_decl {
				mut field_names := []string{}
				for i in 0 .. node.children_count {
					f := t.a.child_node(&node, i)
					if f.kind == .enum_field {
						field_names << f.value
					}
				}
				t.enum_types[node.value] = field_names
				if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
					t.enum_types['${cur_mod}.${node.value}'] = field_names
				}
			}
			.global_decl {
				for i in 0 .. node.children_count {
					f := t.a.child_node(&node, i)
					mut typ := t.normalize_type_in_module(f.typ, cur_mod)
					if typ.len == 0 && f.children_count > 0 {
						typ = t.normalize_type_in_module(t.node_type(t.a.child(f, 0)), cur_mod)
					}
					t.globals[f.value] = typ
					if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
						t.globals['${cur_mod}.${f.value}'] = typ
					}
				}
			}
			.fn_decl {
				if node.typ.len > 0 {
					ret_typ := t.normalize_type_in_module(node.typ, cur_mod)
					t.fn_ret_types[node.value] = ret_typ
					lowered := c_name(node.value)
					if lowered != node.value {
						t.fn_ret_types[lowered] = ret_typ
					}
					if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
						qname := '${cur_mod}.${node.value}'
						t.fn_ret_types[qname] = ret_typ
						qlowered := c_name(qname)
						if qlowered != qname {
							t.fn_ret_types[qlowered] = ret_typ
						}
					}
				}
			}
			.c_fn_decl {
				if node.typ.len > 0 {
					ret_typ := t.normalize_type_in_module(node.typ, cur_mod)
					t.fn_ret_types[node.value] = ret_typ
					if node.value.starts_with('C.') {
						t.fn_ret_types[node.value[2..]] = ret_typ
					} else {
						t.fn_ret_types['C.${node.value}'] = ret_typ
					}
				}
			}
			else {}
		}
	}
}

fn (mut t Transformer) add_sum_variant_parent(variant string, sum_name string) {
	if variant.len == 0 || sum_name.len == 0 {
		return
	}
	field_name := t.sum_field_name(variant)
	if field_name.contains('__') && field_name !in t.sum_variant_fields {
		t.sum_variant_fields[field_name] = variant
	}
	t.add_sum_variant_parent_key(variant, sum_name)
	if variant.contains('.') {
		t.add_sum_variant_parent_key(variant.all_after_last('.'), sum_name)
	}
}

fn (mut t Transformer) add_sum_variant_parent_key(key string, sum_name string) {
	mut parents := t.sum_variant_parents[key] or { []string{} }
	if sum_name !in parents {
		parents << sum_name
		t.sum_variant_parents[key] = parents
	}
}

fn (mut t Transformer) add_unique_field_type(name string, typ string) {
	if name.len == 0 || typ.len == 0 {
		return
	}
	if existing := t.unique_fields[name] {
		if existing != typ {
			t.unique_fields[name] = ''
		}
	} else {
		t.unique_fields[name] = typ
	}
}

fn (mut t Transformer) collect_const_suffixes() {
	if isnil(t.tc) {
		return
	}
	// Register every dot-delimited suffix of each const key so that both
	// unqualified (`foo`) and partially-qualified (`mod.foo`) lookups resolve
	// in O(1) via const_type_key, instead of scanning all consts per ident.
	for key, _ in t.tc.const_types {
		if !key.contains('.') {
			t.add_const_suffix(key, key)
			continue
		}
		mut i := 0
		for i < key.len {
			if key[i] == `.` {
				t.add_const_suffix(key[i + 1..], key)
			}
			i++
		}
	}
}

fn (mut t Transformer) add_const_suffix(suffix string, key string) {
	if existing := t.const_suffixes[suffix] {
		if existing != key {
			t.const_suffixes[suffix] = ''
		}
	} else {
		t.const_suffixes[suffix] = key
	}
}

fn (mut t Transformer) collect_alias_methods() {
	if isnil(t.tc) {
		return
	}
	for name, params in t.tc.fn_param_types {
		if params.len == 0 || !name.contains('.') {
			continue
		}
		receiver_name := name.all_before_last('.')
		if receiver_name.len == 0 || receiver_name !in t.tc.type_aliases {
			continue
		}
		method := name.all_after_last('.')
		param_name := params[0].name()
		clean_alias := if param_name.starts_with('&') { param_name[1..] } else { param_name }
		alias_target := t.normalize_type_alias(clean_alias)
		if alias_target.len == 0 {
			continue
		}
		key := '${alias_target}.${method}'
		if key !in t.alias_methods {
			t.alias_methods[key] = name
		}
	}
}

fn (t &Transformer) normalize_sum_variant_type(typ string, mod string) string {
	clean := typ.trim_space()
	if clean.len == 0 {
		return clean
	}
	if clean.starts_with('&') {
		return '&' + t.normalize_sum_variant_type(clean[1..], mod)
	}
	if clean.starts_with('mut ') {
		return '&' + t.normalize_sum_variant_type(clean[4..], mod)
	}
	if clean.starts_with('?') {
		return '?' + t.normalize_sum_variant_type(clean[1..], mod)
	}
	if clean.starts_with('!') {
		return '!' + t.normalize_sum_variant_type(clean[1..], mod)
	}
	if clean.starts_with('[]') {
		return '[]' + t.normalize_sum_variant_type(clean[2..], mod)
	}
	if clean.starts_with('map[') {
		bracket_end := clean.index(']') or { return clean }
		key := t.normalize_sum_variant_type(clean[4..bracket_end], mod)
		value := t.normalize_sum_variant_type(clean[bracket_end + 1..], mod)
		return 'map[${key}]${value}'
	}
	if clean.starts_with('[') {
		bracket_end := clean.index(']') or { return clean }
		return clean[..bracket_end + 1] + t.normalize_sum_variant_type(clean[bracket_end +
			1..], mod)
	}
	if clean.contains('.') || mod.len == 0 || mod == 'main' || mod == 'builtin'
		|| types.is_builtin_type_name(clean) {
		return clean
	}
	return '${mod}.${clean}'
}

// --- main transform pass ---

fn (mut t Transformer) transform_all() {
	for i in 0 .. t.a.nodes.len {
		node := t.a.nodes[i]
		kind_id := node_kind_id(node)
		if kind_id == 77 {
			t.cur_file = node.value
		}
		if kind_id == 73 {
			t.cur_module = node.value
		}
		if kind_id == 61 {
			if !t.should_transform_fn(node) {
				continue
			}
			t.transform_fn_body(i)
		} else if kind_id == 65 {
			t.transform_const_decl(node)
		} else if kind_id == 64 {
			t.transform_global_decl(node)
		}
	}
}

fn (t &Transformer) should_transform_fn(node flat.Node) bool {
	if t.used_fns.len == 0 {
		return true
	}
	if node.value in t.used_fns {
		return true
	}
	qname := transform_qualified_fn_name(t.cur_module, node.value)
	if qname in t.used_fns {
		return true
	}
	cname := c_name(qname)
	if cname != qname && cname in t.used_fns {
		return true
	}
	return false
}

fn transform_qualified_fn_name(mod string, name string) string {
	if mod.len == 0 || mod == 'main' || mod == 'builtin' {
		return name
	}
	return '${mod}.${name}'
}

// transform_const_decl transforms the initializer expression of each const field
// so that const-level lowering (e.g. string concatenation in the prelude's
// embedded data tables) happens in the transformer rather than the backend.
fn (mut t Transformer) transform_const_decl(node flat.Node) {
	old_in_const_init := t.in_const_init
	t.in_const_init = true
	for ci in 0 .. node.children_count {
		cf_id := t.a.child(&node, ci)
		if int(cf_id) < 0 {
			continue
		}
		cf := t.a.nodes[int(cf_id)]
		if cf.kind == .const_field && cf.children_count >= 1 && cf.children_start >= 0 {
			val_id := t.a.child(&cf, 0)
			if int(val_id) < 0 {
				continue
			}
			val := t.a.nodes[int(val_id)]
			if block_val := t.const_block_value(val) {
				new_val := t.transform_const_value(block_val)
				t.a.children[cf.children_start] = new_val
			} else if val.kind == .string_interp {
				new_val := t.transform_const_string_interp(val_id, val)
				t.a.children[cf.children_start] = new_val
			} else if val.kind == .or_expr {
				const_typ := t.const_field_type_name(cf)
				new_val := t.transform_const_or_expr(val_id, val, const_typ)
				t.a.children[cf.children_start] = new_val
			} else if val.kind == .struct_init || val.kind == .cast_expr || val.kind == .call {
				new_val := t.transform_expr(val_id)
				t.a.children[cf.children_start] = new_val
			} else if val.kind == .infix && val.children_count >= 2 {
				new_val := t.transform_expr(val_id)
				// Overwrite the field's value slot in place (each const_field owns
				// its own single-element child range, so this is safe).
				t.a.children[cf.children_start] = new_val
			}
		}
	}
	t.in_const_init = old_in_const_init
}

fn (t &Transformer) const_field_type_name(field flat.Node) string {
	if field.value.len > 0 {
		if t.cur_module.len > 0 {
			if typ := t.const_type_name('${t.cur_module}.${field.value}') {
				return typ
			}
		}
		if typ := t.const_type_name(field.value) {
			return typ
		}
	}
	return field.typ
}

fn (mut t Transformer) transform_const_or_expr(_id flat.NodeId, node flat.Node, const_typ string) flat.NodeId {
	if node.children_count < 2 {
		return _id
	}
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if i == 0 {
			children << t.transform_expr(child_id)
		} else {
			children << child_id
		}
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	return t.a.add_node(flat.Node{
		kind:           .or_expr
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            if const_typ.len > 0 { const_typ } else { node.typ }
	})
}

fn (mut t Transformer) transform_global_decl(node flat.Node) {
	for ci in 0 .. node.children_count {
		gf_id := t.a.child(&node, ci)
		if int(gf_id) < 0 {
			continue
		}
		gf := t.a.nodes[int(gf_id)]
		if gf.kind == .field_decl && gf.children_count >= 1 && gf.children_start >= 0 {
			val_id := t.a.child(&gf, 0)
			if int(val_id) < 0 {
				continue
			}
			val := t.a.nodes[int(val_id)]
			if preserved := t.transform_global_amp_initializer(val_id, val) {
				t.a.children[gf.children_start] = preserved
				continue
			}
			old_pending := t.pending_stmts.clone()
			t.pending_stmts.clear()
			new_val := t.transform_expr(val_id)
			has_pending := t.pending_stmts.len > 0
			t.pending_stmts.clear()
			t.pending_stmts = old_pending
			if !has_pending {
				t.a.children[gf.children_start] = new_val
			}
		}
	}
}

fn (mut t Transformer) transform_global_amp_initializer(val_id flat.NodeId, val flat.Node) ?flat.NodeId {
	if val.kind != .prefix || val.op != .amp || val.children_count != 1 {
		return none
	}
	child_id := t.a.child(&val, 0)
	child := t.a.nodes[int(child_id)]
	if child.kind == .assoc {
		return val_id
	}
	old_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	mut result := flat.empty_node
	if child.kind == .struct_init {
		if preserved := t.transform_amp_struct_init_for_type(val_id, val, val.typ) {
			result = preserved
		}
	} else if child.kind == .cast_expr {
		if preserved := t.transform_global_amp_interface_cast(val, val.typ) {
			result = preserved
		}
	}
	has_pending := t.pending_stmts.len > 0
	t.pending_stmts.clear()
	t.pending_stmts = old_pending
	if has_pending || int(result) < 0 {
		return none
	}
	return result
}

fn (mut t Transformer) transform_const_value(id flat.NodeId) flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	if block_val := t.const_block_value(node) {
		return t.transform_const_value(block_val)
	}
	return t.transform_expr(id)
}

fn (t &Transformer) const_block_value(node flat.Node) ?flat.NodeId {
	if node.kind != .block || node.children_count == 0 {
		return none
	}
	for i := int(node.children_count) - 1; i >= 0; i-- {
		stmt_id := t.a.child(&node, i)
		stmt := t.a.nodes[int(stmt_id)]
		if stmt.kind == .empty {
			continue
		}
		if stmt.kind == .expr_stmt && stmt.children_count == 1 {
			return t.a.child(&stmt, 0)
		}
		break
	}
	return none
}

fn (mut t Transformer) transform_const_string_interp(_id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return t.make_string_literal('')
	}
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	mut parts := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		transformed := t.transform_expr(child_id)
		mut typ := t.reliable_stringify_type(child_id)
		if typ.len == 0 {
			typ = t.node_type(transformed)
		}
		if typ.len == 0 {
			typ = 'string'
		}
		parts << t.wrap_string_conversion(transformed, typ)
	}
	mut expr := parts[0]
	for i in 1 .. parts.len {
		expr = t.make_call_typed('string__plus', arr2(expr, parts[i]), 'string')
	}
	mut stmts := []flat.NodeId{}
	t.drain_pending(mut stmts)
	t.pending_stmts = outer_pending
	if stmts.len == 0 {
		return expr
	}
	stmts << t.make_expr_stmt(expr)
	return t.make_block(stmts)
}

fn (mut t Transformer) transform_fn_body(fn_idx int) {
	fn_node := t.a.nodes[fn_idx]
	t.cur_fn_name = fn_node.value
	param_count := t.fn_body_param_count(fn_node)
	param_types := t.fn_body_param_types(fn_node, param_count)
	t.cur_fn_ret_type = t.fn_body_return_type(fn_node)
	t.reset_var_types()
	t.smartcast_stack.clear()
	// Collect param types
	mut param_idx := 0
	for i in 0 .. fn_node.children_count {
		child_id := t.a.children[fn_node.children_start + i]
		if int(child_id) < 0 {
			continue
		}
		child := t.a.nodes[int(child_id)]
		if node_kind_id(child) == 75 && child.value.len > 0 && child.typ.len > 0 {
			raw_typ := t.normalize_type_alias(child.typ)
			mut typ := if raw_typ.len > 0 {
				raw_typ
			} else if param_idx < param_types.len {
				t.normalize_type_alias(param_types[param_idx].name())
			} else {
				''
			}
			if typ.starts_with('&') && raw_typ.len > 0 && !raw_typ.starts_with('&')
				&& t.normalize_type_alias(typ[1..]) == raw_typ {
				typ = raw_typ
			}
			t.set_var_type(child.value, typ)
			param_idx++
		}
	}
	mut body_ids := []flat.NodeId{cap: int(fn_node.children_count)}
	for i in 0 .. fn_node.children_count {
		child_id := t.a.children[fn_node.children_start + i]
		if int(child_id) < 0 {
			continue
		}
		child := t.a.nodes[int(child_id)]
		if node_kind_id(child) != 75 {
			body_ids << child_id
		}
	}
	new_body := t.transform_stmts(body_ids)
	// Rebuild function children: params then new body
	start := t.a.children.len
	for i in 0 .. fn_node.children_count {
		child_id := t.a.children[fn_node.children_start + i]
		if int(child_id) < 0 {
			continue
		}
		child := t.a.nodes[int(child_id)]
		if node_kind_id(child) == 75 {
			t.a.children << child_id
		}
	}
	for id in new_body {
		t.a.children << id
	}
	count := t.a.children.len - start
	t.a.nodes[fn_idx] = flat.Node{
		kind:           .fn_decl
		kind_id:        61
		op:             fn_node.op
		children_start: start
		children_count: flat.child_count(count)
		pos:            fn_node.pos
		value:          fn_node.value
		typ:            fn_node.typ
		generic_params: fn_node.generic_params
	}
	t.smartcast_stack.clear()
}

fn (t &Transformer) fn_body_param_types(fn_node flat.Node, expected int) []types.Type {
	if isnil(t.tc) {
		return []types.Type{}
	}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${fn_node.value}'
		if params := t.fn_param_types_for_name(qname, expected) {
			return params
		}
		cqname := c_name(qname)
		if cqname != qname {
			if params := t.fn_param_types_for_name(cqname, expected) {
				return params
			}
		}
	}
	if params := t.fn_param_types_for_name(fn_node.value, expected) {
		return params
	}
	cname := c_name(fn_node.value)
	if cname != fn_node.value {
		if params := t.fn_param_types_for_name(cname, expected) {
			return params
		}
	}
	return []types.Type{}
}

fn (t &Transformer) fn_param_types_for_name(name string, expected int) ?[]types.Type {
	params := t.tc.fn_param_types[name] or { return none }
	if expected != 0 && params.len != expected {
		return none
	}
	return params
}

fn (t &Transformer) fn_body_param_count(fn_node flat.Node) int {
	mut n := 0
	for i in 0 .. fn_node.children_count {
		child := t.a.child_node(&fn_node, i)
		if child.kind == .param {
			n++
		}
	}
	return n
}

fn (t &Transformer) fn_body_return_type(fn_node flat.Node) string {
	if !isnil(t.tc) {
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			qname := '${t.cur_module}.${fn_node.value}'
			if ret := t.fn_return_type_for_name(qname) {
				return ret
			}
			cqname := c_name(qname)
			if cqname != qname {
				if ret := t.fn_return_type_for_name(cqname) {
					return ret
				}
			}
		}
		if ret := t.fn_return_type_for_name(fn_node.value) {
			return ret
		}
		cname := c_name(fn_node.value)
		if cname != fn_node.value {
			if ret := t.fn_return_type_for_name(cname) {
				return ret
			}
		}
	}
	return t.normalize_type_alias(fn_node.typ)
}

fn (t &Transformer) fn_return_type_for_name(name string) ?string {
	ret := t.tc.fn_ret_types[name] or { return none }
	return t.normalize_type_alias(ret.name())
}

// --- statement list driver ---

pub fn (mut t Transformer) transform_stmts(ids []flat.NodeId) []flat.NodeId {
	mut result := []flat.NodeId{cap: ids.len}
	mut i := 0
	for i < ids.len {
		id := ids[i]
		if int(id) >= 0 && i + 1 < ids.len {
			node := t.a.nodes[int(id)]
			if node_kind_id(node) == 44 && node.children_count == 0 && t.cur_fn_ret_type.len > 0
				&& t.cur_fn_ret_type != 'void' {
				next_id := ids[i + 1]
				next_node := t.a.nodes[int(next_id)]
				if node_kind_id(next_node) == 39 && next_node.children_count > 0 {
					expr_id := t.a.child(&next_node, 0)
					start := t.a.children.len
					t.a.children << expr_id
					merged_return := t.a.add_node(flat.Node{
						kind:           .return_stmt
						children_start: start
						children_count: 1
						typ:            node.typ
					})
					expanded := t.transform_stmt(merged_return)
					t.drain_pending(mut result)
					for eid in expanded {
						result << eid
					}
					i += 2
					continue
				}
			}
			if node.kind == .label_stmt {
				next_id := ids[i + 1]
				next_node := t.a.nodes[int(next_id)]
				if next_node.kind in [.for_stmt, .for_in_stmt] {
					expanded := t.transform_labeled_loop(node.value, next_id, next_node)
					t.drain_pending(mut result)
					for eid in expanded {
						result << eid
					}
					i += 2
					continue
				}
			}
		}
		expanded := t.transform_stmt(id)
		t.drain_pending(mut result)
		for eid in expanded {
			result << eid
		}
		i++
	}
	t.drain_pending(mut result)
	return result
}

fn (mut t Transformer) transform_labeled_loop(label string, loop_id flat.NodeId, loop_node flat.Node) []flat.NodeId {
	if label.len == 0 {
		return t.transform_stmt(loop_id)
	}
	continue_label := '${label}_continue'
	break_label := '${label}_break'
	body_start := if loop_node.kind == .for_in_stmt { loop_node.value.int() } else { 3 }
	mut children := []flat.NodeId{cap: int(loop_node.children_count) + 1}
	for i in 0 .. loop_node.children_count {
		children << t.a.child(&loop_node, i)
	}
	if body_start <= children.len {
		children << t.a.add_val(.label_stmt, continue_label)
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	new_loop := t.a.add_node(flat.Node{
		kind:           loop_node.kind
		op:             loop_node.op
		children_start: start
		children_count: flat.child_count(children.len)
		pos:            loop_node.pos
		value:          loop_node.value
		typ:            loop_node.typ
	})
	mut result := []flat.NodeId{}
	result << t.a.add_val(.label_stmt, label)
	result << t.transform_stmt(new_loop)
	result << t.a.add_val(.label_stmt, break_label)
	return result
}

pub fn (mut t Transformer) transform_stmt(id flat.NodeId) []flat.NodeId {
	if int(id) < 0 {
		return arr1(id)
	}
	node := t.a.nodes[int(id)]
	kind_id := node_kind_id(node)
	if kind_id == 44 {
		return t.transform_return_stmt(id, node)
	}
	if kind_id == 40 || kind_id == 42 || kind_id == 43 {
		return t.transform_assign_stmt(id, node)
	}
	if kind_id == 41 {
		return t.transform_decl_assign_stmt(id, node)
	}
	if kind_id == 39 {
		return t.transform_expr_stmt(id, node)
	}
	if kind_id == 46 {
		return t.transform_for_stmt(id, node)
	}
	if kind_id == 47 {
		return t.transform_for_in_stmt(id, node)
	}
	if kind_id == 45 {
		return t.transform_block_stmt(id, node)
	}
	if kind_id == 15 {
		return t.transform_if_stmt(id, node)
	}
	if kind_id == 50 {
		return arr1(t.lower_one_match(node))
	}
	if kind_id == 52 {
		return t.transform_defer_stmt(id, node)
	}
	if kind_id == 53 || kind_id == 56 {
		return t.transform_children_stmt(id, node)
	}
	match node.kind {
		.return_stmt {
			return t.transform_return_stmt(id, node)
		}
		.assign, .selector_assign, .index_assign {
			return t.transform_assign_stmt(id, node)
		}
		.decl_assign {
			return t.transform_decl_assign_stmt(id, node)
		}
		.expr_stmt {
			return t.transform_expr_stmt(id, node)
		}
		.for_stmt {
			return t.transform_for_stmt(id, node)
		}
		.for_in_stmt {
			return t.transform_for_in_stmt(id, node)
		}
		.block {
			return t.transform_block_stmt(id, node)
		}
		.if_expr {
			return t.transform_if_stmt(id, node)
		}
		.match_stmt {
			return arr1(t.lower_one_match(node))
		}
		.defer_stmt {
			return t.transform_defer_stmt(id, node)
		}
		.assert_stmt {
			return t.transform_children_stmt(id, node)
		}
		.select_stmt {
			return t.transform_children_stmt(id, node)
		}
		else {
			return arr1(id)
		}
	}
}

pub fn (mut t Transformer) transform_expr(id flat.NodeId) flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	kind_id := node_kind_id(node)
	if kind_id == 8 {
		return t.transform_infix_expr(id, node)
	}
	if kind_id == 12 {
		return t.transform_call_expr(id, node)
	}
	if kind_id == 15 {
		return t.transform_if_expr(id, node)
	}
	if kind_id == 16 {
		return t.transform_struct_init(id, node)
	}
	if kind_id == 17 {
		return t.transform_field_init_expr(id, node)
	}
	if kind_id == 14 {
		return t.transform_index_expr(id, node)
	}
	if kind_id == 6 {
		return t.transform_string_interp(id, node)
	}
	if kind_id == 13 {
		return t.transform_selector_expr(id, node)
	}
	if kind_id == 22 {
		return t.transform_or_expr(id, node)
	}
	if kind_id == 24 {
		return t.transform_as_expr(id, node)
	}
	if kind_id == 9 {
		return t.transform_prefix_expr(id, node)
	}
	if kind_id == 11 {
		return t.transform_paren_expr(id, node)
	}
	if kind_id == 10 {
		return t.transform_postfix_expr(id, node)
	}
	if kind_id == 23 {
		return t.transform_cast_expr(id, node)
	}
	if kind_id == 18 {
		return t.transform_array_literal(id, node)
	}
	if kind_id == 19 {
		return t.transform_array_init_expr(id, node)
	}
	if kind_id == 20 {
		return t.transform_map_init(id, node)
	}
	if kind_id == 38 {
		return t.transform_in_expr(id, node)
	}
	if kind_id == 37 {
		return t.transform_is_expr(id, node)
	}
	if kind_id == 50 {
		return t.lower_one_match(node)
	}
	if kind_id == 45 {
		return t.transform_block_expr(id, node)
	}
	if kind_id == 31 {
		return t.transform_lock_expr(id, node)
	}
	if kind_id == 34 {
		return t.transform_typeof_expr(id, node)
	}
	if kind_id == 7 {
		return t.transform_ident_expr(id, node)
	}
	if kind_id == 26 {
		return t.transform_assoc_expr(id, node)
	}
	if kind_id == 21 {
		return t.lift_fn_literal(id, node)
	}
	if kind_id == 30 || kind_id == 35 || kind_id == 27 || kind_id == 56 || kind_id == 57 {
		return t.transform_children_expr(id, node)
	}
	if kind_id == 1 || kind_id == 2 || kind_id == 3 || kind_id == 4 || kind_id == 5 || kind_id == 28
		|| kind_id == 29 || kind_id == 25 || kind_id == 33 || kind_id == 36 {
		return id
	}
	match node.kind {
		.infix {
			return t.transform_infix_expr(id, node)
		}
		.call {
			return t.transform_call_expr(id, node)
		}
		.if_expr {
			return t.transform_if_expr(id, node)
		}
		.struct_init {
			return t.transform_struct_init(id, node)
		}
		.field_init {
			return t.transform_field_init_expr(id, node)
		}
		.index {
			return t.transform_index_expr(id, node)
		}
		.string_interp {
			return t.transform_string_interp(id, node)
		}
		.selector {
			return t.transform_selector_expr(id, node)
		}
		.or_expr {
			return t.transform_or_expr(id, node)
		}
		.as_expr {
			return t.transform_as_expr(id, node)
		}
		.prefix {
			return t.transform_prefix_expr(id, node)
		}
		.paren {
			return t.transform_paren_expr(id, node)
		}
		.postfix {
			return t.transform_postfix_expr(id, node)
		}
		.cast_expr {
			return t.transform_cast_expr(id, node)
		}
		.array_literal {
			return t.transform_array_literal(id, node)
		}
		.array_init {
			return t.transform_array_init_expr(id, node)
		}
		.map_init {
			return t.transform_map_init(id, node)
		}
		.sql_expr {
			return t.transform_sql_expr(id, node)
		}
		.in_expr {
			return t.transform_in_expr(id, node)
		}
		.is_expr {
			return t.transform_is_expr(id, node)
		}
		.match_stmt {
			return t.lower_one_match(node)
		}
		.block {
			return t.transform_block_expr(id, node)
		}
		.lock_expr {
			return t.transform_lock_expr(id, node)
		}
		.typeof_expr {
			return t.transform_typeof_expr(id, node)
		}
		.ident {
			return t.transform_ident_expr(id, node)
		}
		.assoc {
			return t.transform_assoc_expr(id, node)
		}
		.fn_literal {
			return t.lift_fn_literal(id, node)
		}
		.lambda_expr, .spawn_expr, .dump_expr, .range, .select_stmt, .select_branch {
			return t.transform_children_expr(id, node)
		}
		.int_literal, .float_literal, .bool_literal, .char_literal, .string_literal, .nil_literal,
		.none_expr, .enum_val, .sizeof_expr, .offsetof_expr {
			// leaf/simple nodes - pass through unchanged
			return id
		}
		else {
			return id
		}
	}
}

pub fn (mut t Transformer) transform_lvalue(id flat.NodeId) flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			return id
		}
		.selector {
			if node.children_count == 0 {
				return id
			}
			if t.selector_chain_has_sum_shared_field(id) {
				value := t.transform_selector_expr(id, node)
				mut value_type := t.node_type(id)
				if value_type.len == 0 {
					value_type = t.node_type(value)
				}
				return t.stable_transformed_expr_for_reuse(value, value_type, 'lvalue')
			}
			full_key := t.expr_key(id)
			if t.has_smartcast(full_key) {
				return t.transform_selector_expr(id, node)
			}
			base_id := t.a.child(&node, 0)
			base_key := t.expr_key(base_id)
			if t.has_smartcast(base_key) {
				return t.transform_selector_expr(id, node)
			}
			base := t.transform_lvalue(t.a.child(&node, 0))
			mut new_children := []flat.NodeId{cap: int(node.children_count)}
			new_children << base
			for i in 1 .. node.children_count {
				new_children << t.transform_expr(t.a.child(&node, i))
			}
			start := t.a.children.len
			for child in new_children {
				t.a.children << child
			}
			return t.a.add_node(flat.Node{
				kind:           .selector
				op:             node.op
				children_start: start
				children_count: flat.child_count(new_children.len)
				pos:            node.pos
				value:          node.value
				typ:            node.typ
			})
		}
		.index {
			if node.children_count == 0 {
				return id
			}
			mut new_children := []flat.NodeId{cap: int(node.children_count)}
			new_children << t.transform_expr(t.a.child(&node, 0))
			for i in 1 .. node.children_count {
				new_children << t.transform_expr(t.a.child(&node, i))
			}
			start := t.a.children.len
			for child in new_children {
				t.a.children << child
			}
			return t.a.add_node(flat.Node{
				kind:           .index
				op:             node.op
				children_start: start
				children_count: flat.child_count(new_children.len)
				pos:            node.pos
				value:          node.value
				typ:            node.typ
			})
		}
		.prefix {
			if node.op == .mul && node.children_count > 0 {
				child := t.transform_expr(t.a.child(&node, 0))
				start := t.a.children.len
				t.a.children << child
				return t.a.add_node(flat.Node{
					kind:           .prefix
					op:             node.op
					children_start: start
					children_count: 1
					pos:            node.pos
					value:          node.value
					typ:            node.typ
				})
			}
			return t.transform_expr(id)
		}
		.paren {
			if node.children_count == 0 {
				return id
			}
			child := t.transform_lvalue(t.a.child(&node, 0))
			start := t.a.children.len
			t.a.children << child
			return t.a.add_node(flat.Node{
				kind:           .paren
				op:             node.op
				children_start: start
				children_count: 1
				pos:            node.pos
				value:          node.value
				typ:            node.typ
			})
		}
		else {
			return t.transform_expr(id)
		}
	}
}

// --- stmt handlers (skeleton - identity transforms with child recursion) ---

fn (mut t Transformer) transform_return_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return arr1(id)
	}
	if expanded := t.try_expand_return_if(id, node) {
		return expanded
	}
	if expanded := t.try_expand_return_match(id, node) {
		return expanded
	}
	if direct := t.try_return_direct_optional_expr(node) {
		return direct
	}
	if expanded := t.try_expand_return_optional_expr(node) {
		return expanded
	}
	if node.children_count == 1 {
		child_id := t.a.child(&node, 0)
		if t.is_optional_type_name(t.cur_fn_ret_type) && t.return_expr_is_err(child_id) {
			return t.with_pending_before(t.make_none_return_stmt())
		}
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if converted := t.fixed_array_return_value(child_id) {
			new_children << converted
		} else if copied := t.heap_copy_local_address_return(child_id) {
			new_children << copied
		} else if t.cur_fn_ret_type.len > 0 && t.cur_fn_ret_type !in t.sum_types
			&& !t.is_optional_type_name(t.cur_fn_ret_type) {
			new_children << t.transform_expr_for_type(child_id, t.cur_fn_ret_type)
		} else {
			new_children << t.wrap_sum_return_expr(child_id)
		}
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	new_id := t.a.add_node(flat.Node{
		kind:           .return_stmt
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
	return t.with_pending_before(new_id)
}

fn (mut t Transformer) heap_copy_local_address_return(child_id flat.NodeId) ?flat.NodeId {
	if !t.cur_fn_ret_type.starts_with('&') || int(child_id) < 0 {
		return none
	}
	node := t.a.nodes[int(child_id)]
	if node.kind != .prefix || node.op != .amp || node.children_count != 1 {
		return none
	}
	inner_id := t.a.child(&node, 0)
	inner := t.a.nodes[int(inner_id)]
	if inner.kind != .ident || inner.value.len == 0 {
		return none
	}
	local_type := t.var_type(inner.value)
	if local_type.len == 0 {
		return none
	}
	ret_base_type := t.cur_fn_ret_type[1..]
	if ret_base_type.len == 0 {
		return none
	}
	clean_local_type := t.normalize_type_alias(local_type)
	clean_ret_type := t.normalize_type_alias(ret_base_type)
	if clean_local_type != clean_ret_type && local_type != ret_base_type {
		return none
	}
	addr := t.make_prefix(.amp, t.make_ident(inner.value))
	size := t.make_sizeof_type(ret_base_type)
	dup := t.make_call_typed('memdup', arr2(addr, size), 'voidptr')
	return t.make_cast(t.cur_fn_ret_type, dup, t.cur_fn_ret_type)
}

fn (mut t Transformer) fixed_array_return_value(child_id flat.NodeId) ?flat.NodeId {
	mut ret_type := t.cur_fn_ret_type
	if t.is_optional_type_name(ret_type) {
		ret_type = t.optional_base_type(ret_type)
	}
	mut array_type := ret_type
	if is_fixed_array_type(ret_type) {
		array_type = '[]${fixed_array_elem_type(ret_type)}'
	}
	if !array_type.starts_with('[]') {
		return none
	}
	child_type := t.node_type(child_id)
	if !is_fixed_array_type(child_type) || fixed_array_elem_type(child_type) != array_type[2..] {
		return none
	}
	return t.fixed_array_value_to_array(child_id, child_type, array_type)
}

fn (mut t Transformer) transform_assign_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return arr1(id)
	}
	if expanded := t.try_expand_multi_return_assign(node) {
		return expanded
	}
	if expanded := t.try_expand_plain_multi_assign(node) {
		return expanded
	}
	if lowered := t.try_lower_sum_shared_field_assign(node) {
		return lowered
	}
	if lowered := t.try_lower_pointer_value_assign(node) {
		return lowered
	}
	if lowered := t.try_lower_map_index_assign(node) {
		return lowered
	}
	// string `s += x` on a plain ident -> `s = string__plus(s, x)` (only when detectable as string)
	if expanded := t.try_lower_string_compound_assign(id, node) {
		return expanded
	}
	if expanded := t.try_lower_struct_compound_assign(node) {
		return expanded
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if i % 2 == 0 {
			new_children << t.transform_lvalue(child_id)
		} else {
			lhs_id := t.a.child(&node, i - 1)
			lhs := t.a.nodes[int(lhs_id)]
			mut lhs_type := if lhs.kind in [.selector, .index] {
				t.lvalue_type(lhs_id)
			} else {
				t.original_expr_type(lhs_id)
			}
			if lhs_type.len == 0 {
				lhs_type = t.lvalue_type(lhs_id)
			}
			sum_target := t.assignment_sum_target(lhs_id, child_id, lhs_type)
			if node.op == .assign && sum_target.len > 0 {
				new_children << t.wrap_sum_value(child_id, sum_target)
			} else {
				new_children << t.transform_expr_for_type(child_id, lhs_type)
			}
		}
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	new_id := t.a.add_node(flat.Node{
		kind:           node.kind
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
	if node.kind == .assign && node.op == .left_shift_assign {
		t.annotate_left_shift_assign(new_id)
	}
	return t.with_pending_before(new_id)
}

fn (mut t Transformer) try_lower_struct_compound_assign(node flat.Node) ?[]flat.NodeId {
	if node.kind != .assign || node.children_count != 2 {
		return none
	}
	op_name := compound_assign_struct_operator_symbol(node.op) or { return none }
	lhs_id := t.a.child(&node, 0)
	rhs_id := t.a.child(&node, 1)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 {
		return none
	}
	mut lhs_type := t.original_expr_type(lhs_id)
	if lhs_type.starts_with('&') {
		return none
	}
	struct_type := t.struct_lookup_name(lhs_type)
	if struct_type.len == 0 {
		return none
	}
	method_name := t.struct_operator_fn_name(struct_type, op_name) or { return none }
	rhs := t.transform_expr_for_type(rhs_id, lhs_type)
	call := t.make_call_typed(method_name, arr2(t.make_ident(lhs.value), rhs), lhs_type)
	return arr1(t.make_assign(t.make_ident(lhs.value), call))
}

fn compound_assign_struct_operator_symbol(op flat.Op) ?string {
	match op {
		.plus_assign { return '+' }
		.minus_assign { return '-' }
		.mul_assign { return '*' }
		.div_assign { return '/' }
		.mod_assign { return '%' }
		else {}
	}

	return none
}

fn (mut t Transformer) try_lower_sum_shared_field_assign(node flat.Node) ?[]flat.NodeId {
	if node.kind !in [.assign, .selector_assign] || node.children_count != 2 {
		return none
	}
	lhs_id := t.a.child(&node, 0)
	rhs_id := t.a.child(&node, 1)
	if int(lhs_id) < 0 || int(rhs_id) < 0 {
		return none
	}
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .selector || lhs.children_count == 0 || lhs.value.len == 0 {
		return none
	}
	base_id := t.a.child(&lhs, 0)
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 {
		base_type = t.original_expr_type(base_id)
	}
	field_type := t.sum_shared_field_type_name(base_type, lhs.value) or { return none }
	mut base := t.transform_lvalue(base_id)
	mut sum_type := base_type
	if !t.is_stable_expr_for_reuse(base) {
		clean_sum := t.trim_pointer_type(sum_type)
		ptr_type := if sum_type.starts_with('&') { sum_type } else { '&${clean_sum}' }
		addr := if sum_type.starts_with('&') {
			base
		} else {
			mut addr_expr := t.make_prefix(.amp, base)
			t.a.nodes[int(addr_expr)].typ = ptr_type
			addr_expr
		}
		tmp_name := t.new_temp('sum_lhs')
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, addr, ptr_type)
		base = t.make_ident(tmp_name)
		sum_type = ptr_type
	}
	mut rhs := if node.op == .assign {
		t.transform_expr_for_type(rhs_id, field_type)
	} else {
		t.transform_expr(rhs_id)
	}
	mut rhs_type := t.node_type(rhs)
	if rhs_type.len == 0 {
		rhs_type = t.node_type(rhs_id)
	}
	if rhs_type.len == 0 {
		rhs_type = field_type
	}
	rhs = t.stable_transformed_expr_for_reuse(rhs, rhs_type, 'sum_assign')
	resolved_sum := t.resolve_sum_name(t.trim_pointer_type(sum_type))
	variants := t.sum_types[resolved_sum] or { return none }
	stmt := t.build_sum_shared_field_assign_chain(base, sum_type, resolved_sum, variants,
		lhs.value, field_type, rhs, node.op, 0)
	return t.with_pending_before(stmt)
}

fn (mut t Transformer) build_sum_shared_field_assign_chain(base flat.NodeId, sum_type string, resolved_sum string, variants []string, field string, field_type string, rhs flat.NodeId, op flat.Op, idx int) flat.NodeId {
	if idx >= variants.len {
		return t.make_empty()
	}
	variant := variants[idx]
	tag := t.make_selector_op(base, 'typ', 'int', if sum_type.starts_with('&') {
		.arrow
	} else {
		.dot
	})
	cond := t.make_infix(.eq, tag, t.make_int_literal(t.sum_type_index(resolved_sum, variant)))
	qv := t.resolve_variant(resolved_sum, variant)
	sum_field := t.sum_field_name(qv)
	use_ptr := t.variant_references_sum(qv, resolved_sum)
	variant_base := t.make_selector_op(base, sum_field, if use_ptr { '&${qv}' } else { qv }, if sum_type.starts_with('&') {
		.arrow
	} else {
		.dot
	})
	mut then_stmt := t.make_empty()
	if nested_field_type := t.sum_shared_field_type_name(qv, field) {
		nested_sum := t.resolve_sum_name(qv)
		if nested_variants := t.sum_types[nested_sum] {
			then_stmt = t.build_sum_shared_field_assign_chain(variant_base, qv, nested_sum,
				nested_variants, field, nested_field_type, rhs, op, 0)
		}
	} else {
		field_lhs := t.make_selector_op(variant_base, field, field_type, if use_ptr {
			.arrow
		} else {
			.dot
		})
		then_stmt = t.make_assign_op(field_lhs, rhs, op)
	}
	then_block := t.make_block(arr1(then_stmt))
	else_stmt := t.build_sum_shared_field_assign_chain(base, sum_type, resolved_sum, variants,
		field, field_type, rhs, op, idx + 1)
	return t.make_if(cond, then_block, else_stmt)
}

fn (t &Transformer) assignment_sum_target(lhs_id flat.NodeId, rhs_id flat.NodeId, lhs_type string) string {
	if lhs_type.starts_with('&') {
		return ''
	}
	if lhs_type.starts_with('[]') || is_fixed_array_type(lhs_type) {
		return ''
	}
	if t.is_sum_type_name(lhs_type) {
		return lhs_type
	}
	if int(lhs_id) < 0 || int(rhs_id) < 0 {
		return ''
	}
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .selector || lhs.value.len == 0 {
		return ''
	}
	if lhs.value == 'obj' {
		sum_name := t.resolve_sum_name('ScopeObject')
		if t.is_sum_type_name(sum_name) {
			return sum_name
		}
	}
	rhs := t.a.nodes[int(rhs_id)]
	if inferred_sum := t.sum_type_for_field_variant(lhs.value, rhs_id, rhs) {
		return inferred_sum
	}
	if lhs.value == 'info' {
		if type_info_sum := t.type_info_sum_name() {
			return type_info_sum
		}
	}
	return ''
}

fn (t &Transformer) type_info_sum_name() ?string {
	for sum_name, _ in t.sum_types {
		if sum_name == 'TypeInfo' || sum_name.ends_with('.TypeInfo') {
			return sum_name
		}
	}
	return none
}

fn (mut t Transformer) try_lower_pointer_value_assign(node flat.Node) ?[]flat.NodeId {
	if node.kind != .assign || node.children_count != 2 {
		return none
	}
	lhs_id := t.a.child(&node, 0)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 {
		return none
	}
	mut lhs_type := t.var_type(lhs.value)
	if lhs_type.len == 0 {
		lhs_type = t.node_type(lhs_id)
	}
	if !lhs_type.starts_with('&') {
		return none
	}
	rhs_id := t.a.child(&node, 1)
	rhs_type := t.node_type(rhs_id)
	lhs_value_type := t.normalize_type_alias(lhs_type[1..])
	if node.op != .assign {
		if !t.pointer_value_lvalues[lhs.value] {
			return none
		}
		new_lhs := t.make_prefix(.mul, t.make_ident(lhs.value))
		return arr1(t.make_assign_op(new_lhs, t.transform_expr(rhs_id), node.op))
	}
	if rhs_type.len == 0
		|| (rhs_type != lhs_value_type && !t.type_alias_targets_type(lhs_type[1..], rhs_type)) {
		return none
	}
	new_lhs := t.make_prefix(.mul, t.make_ident(lhs.value))
	return arr1(t.make_assign(new_lhs, t.transform_expr(rhs_id)))
}

fn (mut t Transformer) transform_expr_for_type(id flat.NodeId, target_type string) flat.NodeId {
	if int(id) >= 0 && target_type.len > 0 {
		node := t.a.nodes[int(id)]
		if node.kind == .none_expr && t.is_ierror_type(target_type) {
			return t.make_struct_init('IError')
		}
		if target_type.starts_with('&') {
			if expr := t.transform_amp_struct_init_for_type(id, node, target_type) {
				return expr
			}
		}
		if expr := t.transform_interface_value_for_type(id, target_type) {
			return expr
		}
		if node.kind == .block {
			if lowered := t.transform_block_expr_for_type(id, node, target_type) {
				return lowered
			}
		}
		if node.kind == .if_expr {
			if lowered := t.try_expand_if_expr_value_for_type(id, node, target_type) {
				return lowered
			}
		}
		if node.kind == .match_stmt {
			if lowered := t.transform_match_expr_for_type(id, node, target_type) {
				return lowered
			}
		}
		if node.kind == .or_expr && !t.is_optional_type_name(target_type) {
			old_typ := node.typ
			t.a.nodes[int(id)].typ = target_type
			expr := t.transform_expr(id)
			t.a.nodes[int(id)].typ = old_typ
			return t.coerce_transformed_expr_to_type(expr, id, target_type)
		}
		if node.kind == .array_literal {
			if lowered := t.transform_fixed_array_literal_for_type(id, node, target_type) {
				return lowered
			}
			if lowered := t.transform_array_literal_for_type(id, node, target_type) {
				return lowered
			}
		}
		if node.kind == .array_init {
			if lowered := t.transform_empty_array_init_for_type(node, target_type) {
				return lowered
			}
		}
	}
	expr := t.transform_expr(id)
	return t.coerce_transformed_expr_to_type(expr, id, target_type)
}

fn (mut t Transformer) transform_block_expr_for_type(_id flat.NodeId, node flat.Node, target_type string) ?flat.NodeId {
	if node.kind != .block || node.children_count == 0 || target_type.len == 0 {
		return none
	}
	last_id := t.a.child(&node, node.children_count - 1)
	last := t.a.nodes[int(last_id)]
	tail_expr_id := if last.kind == .expr_stmt && last.children_count > 0 {
		t.a.child(&last, 0)
	} else if last.kind == .block && t.stmt_value_type(last_id).len > 0 {
		last_id
	} else if !t.is_stmt_kind(last.kind) {
		last_id
	} else {
		return none
	}
	mut prefix := []flat.NodeId{cap: int(node.children_count - 1)}
	for i in 0 .. node.children_count - 1 {
		prefix << t.a.child(&node, i)
	}
	mut new_children := t.transform_stmts(prefix)
	tail_expr := t.transform_expr_for_type(tail_expr_id, target_type)
	tail_stmt := t.make_expr_stmt(tail_expr)
	for stmt in t.with_pending_before(tail_stmt) {
		new_children << stmt
	}
	new_block := t.make_block(new_children)
	block_typ := t.stmt_value_type(new_block)
	t.a.nodes[int(new_block)].typ = if block_typ.len > 0 { block_typ } else { node.typ }
	return new_block
}

fn (mut t Transformer) transform_match_expr_for_type(_id flat.NodeId, node flat.Node, target_type string) ?flat.NodeId {
	if target_type.len == 0 || node.kind != .match_stmt {
		return none
	}
	mut actual_result_type := t.match_expr_type(node)
	if actual_result_type.len == 0 || actual_result_type == 'void' {
		actual_result_type = target_type
	}
	tmp_name := t.new_temp('match_val')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()

	mut prelude := []flat.NodeId{}
	prelude << t.make_decl_assign_typed(tmp_name, t.zero_value_for_type(actual_result_type),
		actual_result_type)
	for stmt in t.build_match_value_stmts(node, tmp_name, actual_result_type) {
		prelude << stmt
	}

	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	tmp := t.make_ident(tmp_name)
	t.a.nodes[int(tmp)].typ = actual_result_type
	return tmp
}

fn (mut t Transformer) transform_amp_struct_init_for_type(_id flat.NodeId, node flat.Node, target_type string) ?flat.NodeId {
	if node.kind != .prefix || node.op != .amp || node.children_count != 1 {
		return none
	}
	child_id := t.a.child(&node, 0)
	child := t.a.nodes[int(child_id)]
	if child.kind != .struct_init {
		return none
	}
	new_child := t.transform_struct_init(child_id, child)
	start := t.a.children.len
	t.a.children << new_child
	return t.a.add_node(flat.Node{
		kind:           .prefix
		op:             node.op
		children_start: start
		children_count: 1
		pos:            node.pos
		value:          node.value
		typ:            if target_type.len > 0 { target_type } else { node.typ }
	})
}

fn (mut t Transformer) coerce_transformed_expr_to_type(expr flat.NodeId, source_id flat.NodeId, target_type string) flat.NodeId {
	mut target := t.normalize_type_alias(target_type)
	if target.len == 0 || int(expr) < 0 {
		return expr
	}
	mut expr_type := t.node_type(expr)
	if expr_type.len == 0 {
		expr_type = t.node_type(source_id)
	}
	if expr_type.len == 0 {
		expr_type = t.resolve_expr_type(source_id)
	}
	expr_type = t.normalize_type_alias(expr_type)
	mut optional_target := if t.is_optional_type_name(target_type) {
		t.qualify_optional_type(target_type)
	} else {
		target
	}
	optional_target = t.infer_typed_optional_target(optional_target, expr_type)
	if t.is_optional_type_name(optional_target) && !t.is_optional_type_name(expr_type) {
		source := if int(source_id) >= 0 { t.a.nodes[int(source_id)] } else { flat.Node{} }
		if source.kind != .none_expr {
			return t.make_optional_some(expr, optional_target)
		}
	}
	if expr_type.len == 0 || expr_type == target {
		return expr
	}
	if target.starts_with('&') {
		if t.expr_is_nil_like(source_id) {
			t.a.nodes[int(expr)].typ = target
			return expr
		}
		target_value_type := t.normalize_type_alias(target[1..])
		expr_value_type := if expr_type.starts_with('&') {
			t.normalize_type_alias(expr_type[1..])
		} else {
			expr_type
		}
		if t.is_sum_type_name(target_value_type)
			&& t.find_sum_type_for_variant(t.trim_pointer_type(expr_type)).len > 0 {
			if t.resolve_sum_name(t.trim_pointer_type(expr_type)) == t.resolve_sum_name(target_value_type) {
				if expr_type.starts_with('&') {
					return expr
				}
				if t.expr_can_take_address(expr) {
					addr := t.make_prefix(.amp, expr)
					t.a.nodes[int(addr)].typ = target
					return addr
				}
				tmp_name := t.new_temp('sum_ref')
				t.pending_stmts << t.make_decl_assign_typed(tmp_name, expr, target_value_type)
				addr := t.make_prefix(.amp, t.make_ident(tmp_name))
				t.a.nodes[int(addr)].typ = target
				return addr
			}
			source := t.a.nodes[int(source_id)]
			wrap_source_id := if source.kind == .prefix && source.op == .amp
				&& source.children_count > 0 {
				t.a.child(&source, 0)
			} else {
				source_id
			}
			wrapped := t.wrap_sum_value(wrap_source_id, target_value_type)
			tmp_name := t.new_temp('sum_ref')
			t.pending_stmts << t.make_decl_assign_typed(tmp_name, wrapped, target_value_type)
			addr := t.make_prefix(.amp, t.make_ident(tmp_name))
			t.a.nodes[int(addr)].typ = target
			return addr
		}
		if expr_value_type == target_value_type
			|| t.type_alias_targets_type(target[1..], expr_value_type) {
			if expr_type.starts_with('&') {
				return expr
			}
			if t.expr_can_take_address(expr) {
				addr := t.make_prefix(.amp, expr)
				t.a.nodes[int(addr)].typ = target
				return addr
			}
			tmp_name := t.new_temp('addr')
			t.pending_stmts << t.make_decl_assign_typed(tmp_name, expr, expr_value_type)
			addr := t.make_prefix(.amp, t.make_ident(tmp_name))
			t.a.nodes[int(addr)].typ = target
			return addr
		}
		return expr
	}
	if expr_type.starts_with('&') {
		expr_value_type := t.normalize_type_alias(expr_type[1..])
		if expr_value_type == target || t.type_alias_targets_type(expr_type[1..], target) {
			deref := t.make_prefix(.mul, expr)
			t.a.nodes[int(deref)].typ = target
			return deref
		}
	}
	return expr
}

fn (t &Transformer) is_ierror_type(name string) bool {
	clean := t.trim_pointer_type(t.normalize_type_alias(name))
	return clean == 'IError' || clean.ends_with('.IError')
}

fn (t &Transformer) expr_is_nil_like(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .nil_literal {
		return true
	}
	if node.kind != .block || node.children_count == 0 {
		return false
	}
	last_id := t.a.child(&node, node.children_count - 1)
	last := t.a.nodes[int(last_id)]
	if last.kind == .expr_stmt && last.children_count > 0 {
		return t.expr_is_nil_like(t.a.child(&last, 0))
	}
	return t.expr_is_nil_like(last_id)
}

fn (t &Transformer) infer_typed_optional_target(optional_target string, expr_type string) string {
	if expr_type.len == 0 {
		return optional_target
	}
	mut value_type := expr_type
	if !value_type.contains('.') {
		qualified := t.qualify_type(value_type)
		if qualified != value_type {
			value_type = qualified
		}
	}
	if !isnil(t.tc) {
		parsed := t.tc.parse_type(value_type)
		parsed_name := parsed.name()
		if parsed_name.len > 0 && parsed_name != 'unknown' {
			value_type = parsed_name
		}
	}
	if t.is_optional_type_name(optional_target) {
		base := t.optional_base_type(optional_target)
		if value_type.contains('.') && base == value_type.all_after_last('.') {
			return '?${value_type}'
		}
		return optional_target
	}
	if optional_target != 'Optional' || isnil(t.tc) {
		return optional_target
	}
	typ := t.tc.parse_type(value_type)
	if typ is types.Primitive || typ is types.Enum || typ is types.Void {
		return optional_target
	}
	return '?${value_type}'
}

fn (mut t Transformer) make_optional_some(value flat.NodeId, optional_type string) flat.NodeId {
	ok_field := t.make_sum_literal_field('ok', t.make_bool_literal(true), 'bool')
	base_type := t.optional_base_type(optional_type)
	mut fields := []flat.NodeId{cap: 2}
	fields << ok_field
	if base_type.len > 0 && base_type != 'void' {
		fields << t.make_sum_literal_field('value', value, base_type)
	}
	start := t.a.children.len
	for field in fields {
		t.a.children << field
	}
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: start
		children_count: flat.child_count(fields.len)
		value:          optional_type
		typ:            optional_type
	})
}

fn (mut t Transformer) make_optional_none(optional_type string) flat.NodeId {
	ok_field := t.make_sum_literal_field('ok', t.make_bool_literal(false), 'bool')
	start := t.a.children.len
	t.a.children << ok_field
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: start
		children_count: 1
		value:          optional_type
		typ:            optional_type
	})
}

fn (mut t Transformer) make_optional_none_with_err(optional_type string, err_expr flat.NodeId) flat.NodeId {
	ok_field := t.make_sum_literal_field('ok', t.make_bool_literal(false), 'bool')
	err_field := t.make_sum_literal_field('err', err_expr, 'IError')
	start := t.a.children.len
	t.a.children << ok_field
	t.a.children << err_field
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: start
		children_count: 2
		value:          optional_type
		typ:            optional_type
	})
}

fn (t &Transformer) expr_can_take_address(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident, .index {
			return true
		}
		.selector {
			if node.children_count == 0 {
				return false
			}
			if t.selector_chain_has_sum_variant_field(id) {
				return false
			}
			return t.expr_can_take_address(t.a.child(&node, 0))
		}
		.prefix {
			return node.op == .mul
		}
		.paren {
			if node.children_count == 0 {
				return false
			}
			return t.expr_can_take_address(t.a.child(&node, 0))
		}
		else {
			return false
		}
	}
}

fn (t &Transformer) type_alias_targets_type(alias_name string, target_type string) bool {
	if alias_name.len == 0 || target_type.len == 0 || isnil(t.tc) {
		return false
	}
	for name, target in t.tc.type_aliases {
		if name == alias_name || name.all_after_last('.') == alias_name {
			if t.normalize_type_alias(target) == target_type {
				return true
			}
		}
	}
	return false
}

fn (mut t Transformer) try_lower_string_compound_assign(_id flat.NodeId, node flat.Node) ?[]flat.NodeId {
	if node.kind != .assign || node.op != .plus_assign || node.children_count != 2 {
		return none
	}
	lhs_id := t.a.child(&node, 0)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident {
		return none
	}
	rhs_id := t.a.child(&node, 1)
	rhs := t.a.nodes[int(rhs_id)]
	is_string := t.resolve_expr_type(lhs_id) == 'string' || rhs.kind == .string_literal
		|| rhs.kind == .string_interp || t.resolve_expr_type(rhs_id) == 'string'
	if !is_string {
		return none
	}
	new_rhs := t.transform_expr(rhs_id)
	lhs_copy := t.make_ident(lhs.value)
	concat := t.make_call('string__plus', arr2(lhs_copy, new_rhs))
	new_lhs := t.make_ident(lhs.value)
	return arr1(t.make_assign(new_lhs, concat))
}

fn (mut t Transformer) transform_decl_assign_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return arr1(id)
	}
	mut has_empty_child := false
	for i in 0 .. node.children_count {
		if int(t.a.child(&node, i)) < 0 {
			has_empty_child = true
		}
	}
	if has_empty_child {
		mut parts := []string{}
		for i in 0 .. node.children_count {
			child_id := t.a.child(&node, i)
			if int(child_id) < 0 {
				parts << '${i}:empty'
			} else {
				child := t.a.nodes[int(child_id)]
				parts << '${i}:${child.kind}:${child.value}:${child.typ}'
			}
		}
		panic('internal error: empty decl_assign child in ${t.cur_fn_name}: count=${node.children_count} typ=${node.typ} value=${node.value} children=${parts.join('|')}')
	}
	mut inferred_typ := ''
	if node.children_count > 2 && !isnil(t.tc) {
		rhs_id := t.a.child(&node, 1)
		if rhs_types := t.multi_return_types_for_expr(rhs_id, node.children_count - 1) {
			for j, field_type in rhs_types {
				lhs_idx := if j == 0 { 0 } else { j + 1 }
				if lhs_idx >= node.children_count {
					continue
				}
				lhs := t.a.child_node(&node, lhs_idx)
				if lhs.kind == .ident && lhs.value.len > 0 && lhs.value != '_' {
					t.set_var_type(lhs.value, t.normalize_type_alias(field_type.name()))
				}
			}
		}
	}
	if expanded := t.try_expand_multi_return_decl(node) {
		return expanded
	}
	if expanded := t.try_expand_plain_multi_decl(node) {
		return expanded
	}
	// Track the variable type for the common 2-child case.
	if node.children_count == 2 {
		lhs := t.a.child_node(&node, 0)
		if lhs.kind == .ident && lhs.value.len > 0 {
			mut typ := t.infer_decl_type(node)
			rhs_id := t.a.child(&node, 1)
			rhs := t.a.nodes[int(rhs_id)]
			if rhs.kind == .call && t.is_strings_builder_new_call(rhs_id, rhs) {
				typ = 'strings.Builder'
			} else if rhs.kind == .if_expr {
				if_typ := t.if_expr_result_type(rhs_id, rhs)
				if if_typ.len > 0 {
					typ = if_typ
				}
			} else if rhs.kind == .match_stmt {
				match_typ := t.match_expr_type(rhs)
				if match_typ.len > 0 {
					typ = match_typ
				}
			} else if rhs.kind == .block {
				block_typ := t.stmt_value_type(rhs_id)
				if block_typ.len > 0 {
					typ = block_typ
				}
			} else if rhs.kind == .or_expr && rhs.children_count > 0 {
				or_source_id := t.a.child(&rhs, 0)
				if info := t.map_index_info(or_source_id) {
					typ = info.value_type
				} else if info := t.array_index_info(or_source_id) {
					typ = info.value_type
				} else {
					or_body_id := if rhs.children_count > 1 {
						t.a.child(&rhs, 1)
					} else {
						flat.empty_node
					}
					fallback_type := if typ.len > 0 { typ } else { t.stmt_value_type(or_body_id) }
					expr_type, value_type := t.or_expr_types(or_source_id, fallback_type)
					if t.is_optional_type_name(expr_type) && value_type.len > 0
						&& value_type != 'void' {
						typ = value_type
					}
				}
			}
			if node.typ.len == 0 {
				if rhs.kind == .array_literal && is_fixed_array_type(typ) {
					typ = '[]${fixed_array_elem_type(typ)}'
					t.a.nodes[int(rhs_id)].typ = typ
				}
			}
			if typ.len > 0 {
				t.set_var_type(lhs.value, typ)
				inferred_typ = typ
			}
		}
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if i == 0 || (node.children_count > 2 && i > 1) {
			new_children << t.transform_lvalue(child_id)
		} else {
			lhs_id := t.a.child(&node, 0)
			lhs_type := if inferred_typ.len > 0 {
				inferred_typ
			} else if node.typ.len > 0 {
				node.typ
			} else {
				t.lvalue_type(lhs_id)
			}
			sum_target := t.assignment_sum_target(lhs_id, child_id, lhs_type)
			if sum_target.len > 0 && !t.expr_has_smartcast(child_id) {
				new_children << t.wrap_sum_value(child_id, sum_target)
			} else {
				new_children << t.transform_expr_for_type(child_id, lhs_type)
			}
		}
	}
	if node.children_count == 2 && node.typ.len == 0 {
		lhs := t.a.nodes[int(new_children[0])]
		if lhs.kind == .ident && lhs.value.len > 0 {
			rhs_typ := t.node_type(new_children[1])
			if rhs_typ.len > 0
				&& (inferred_typ.len == 0 || inferred_typ in ['array', 'map', 'unknown']) {
				t.set_var_type(lhs.value, rhs_typ)
				inferred_typ = rhs_typ
			}
		}
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	new_id := t.a.add_node(flat.Node{
		kind:           .decl_assign
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            if inferred_typ.len > 0 { inferred_typ } else { node.typ }
	})
	return t.with_pending_before(new_id)
}

fn (mut t Transformer) try_expand_plain_multi_decl(node flat.Node) ?[]flat.NodeId {
	if node.kind != .decl_assign || node.children_count < 4 || node.children_count % 2 != 0 {
		return none
	}
	mut result := []flat.NodeId{}
	for i := 0; i < node.children_count; i += 2 {
		lhs_id := t.a.child(&node, i)
		rhs_id := t.a.child(&node, i + 1)
		lhs := t.a.nodes[int(lhs_id)]
		rhs := t.transform_expr(rhs_id)
		t.drain_pending(mut result)
		if lhs.kind != .ident || lhs.value == '_' {
			continue
		}
		mut typ := if lhs.typ.len > 0 { lhs.typ } else { t.node_type(rhs) }
		if typ.len == 0 {
			typ = t.node_type(rhs_id)
		}
		if typ.len == 0 {
			typ = t.resolve_expr_type(rhs_id)
		}
		if typ.len > 0 {
			typ = t.normalize_type_alias(typ)
			t.set_var_type(lhs.value, typ)
			result << t.make_decl_assign_typed(lhs.value, rhs, typ)
		} else {
			result << t.make_decl_assign(lhs.value, rhs)
		}
	}
	return result
}

fn (t &Transformer) expr_has_smartcast(id flat.NodeId) bool {
	key := t.expr_key(id)
	return t.has_smartcast(key)
}

fn (mut t Transformer) try_expand_multi_return_decl(node flat.Node) ?[]flat.NodeId {
	if node.kind != .decl_assign || node.children_count < 3 || isnil(t.tc) {
		return none
	}
	rhs_id := t.a.child(&node, 1)
	rhs := t.a.nodes[int(rhs_id)]
	lhs_ids := t.multi_assign_lhs_ids(node)
	if rhs.kind == .if_expr {
		return t.expand_multi_return_if_decl(rhs_id, rhs, lhs_ids)
	}
	if rhs_types := t.multi_return_types_for_expr(rhs_id, lhs_ids.len) {
		tmp_name := t.new_temp('multi_ret')
		mut result := []flat.NodeId{}
		new_rhs := t.transform_expr(rhs_id)
		t.drain_pending(mut result)
		result << t.make_decl_assign_typed(tmp_name, new_rhs, t.multi_return_type_name(rhs_types))
		for j, field_type in rhs_types {
			if j >= lhs_ids.len {
				continue
			}
			lhs_id := lhs_ids[j]
			lhs := t.a.nodes[int(lhs_id)]
			if lhs.kind != .ident || lhs.value == '_' {
				continue
			}
			field_name := 'arg${j}'
			field_type_name := field_type.name()
			field := t.make_selector(t.make_ident(tmp_name), field_name, field_type_name)
			t.set_var_type(lhs.value, t.normalize_type_alias(field_type_name))
			result << t.make_decl_assign_typed(lhs.value, field, field_type_name)
		}
		return result
	}
	return none
}

fn (mut t Transformer) try_expand_multi_return_assign(node flat.Node) ?[]flat.NodeId {
	if node.kind != .assign || node.children_count < 3 || isnil(t.tc) {
		return none
	}
	rhs_id := t.a.child(&node, 1)
	rhs := t.a.nodes[int(rhs_id)]
	lhs_ids := t.multi_assign_lhs_ids(node)
	if rhs.kind == .if_expr {
		return t.expand_multi_return_if_assign(rhs_id, rhs, lhs_ids)
	}
	if rhs_types := t.multi_return_types_for_expr(rhs_id, lhs_ids.len) {
		tmp_name := t.new_temp('multi_ret')
		mut result := []flat.NodeId{}
		new_rhs := t.transform_expr(rhs_id)
		t.drain_pending(mut result)
		result << t.make_decl_assign_typed(tmp_name, new_rhs, t.multi_return_type_name(rhs_types))
		for j, field_type in rhs_types {
			if j >= lhs_ids.len {
				continue
			}
			lhs_id := lhs_ids[j]
			lhs := t.a.nodes[int(lhs_id)]
			if lhs.kind == .ident && lhs.value == '_' {
				continue
			}
			field_name := 'arg${j}'
			field_type_name := field_type.name()
			field := t.make_selector(t.make_ident(tmp_name), field_name, field_type_name)
			result << t.make_assign(t.transform_lvalue(lhs_id), field)
		}
		return result
	}
	return none
}

fn (mut t Transformer) try_expand_plain_multi_assign(node flat.Node) ?[]flat.NodeId {
	if node.kind != .assign || node.op != .assign || node.children_count < 4
		|| node.children_count % 2 != 0 {
		return none
	}
	mut result := []flat.NodeId{}
	mut lhs_ids := []flat.NodeId{}
	mut tmp_names := []string{}
	for i := 0; i < node.children_count; i += 2 {
		lhs_id := t.a.child(&node, i)
		rhs_id := t.a.child(&node, i + 1)
		lhs_ids << lhs_id
		lhs_type := t.lvalue_type(lhs_id)
		rhs := if lhs_type.len > 0 {
			t.transform_expr_for_type(rhs_id, lhs_type)
		} else {
			t.transform_expr(rhs_id)
		}
		t.drain_pending(mut result)
		tmp_name := t.new_temp('assign')
		tmp_type := if lhs_type.len > 0 { lhs_type } else { t.node_type(rhs_id) }
		result << t.make_decl_assign_typed(tmp_name, rhs, tmp_type)
		tmp_names << tmp_name
	}
	for i, lhs_id in lhs_ids {
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind == .ident && lhs.value == '_' {
			continue
		}
		result << t.make_assign(t.transform_lvalue(lhs_id), t.make_ident(tmp_names[i]))
	}
	return result
}

fn (t &Transformer) multi_assign_lhs_ids(node flat.Node) []flat.NodeId {
	mut lhs_ids := []flat.NodeId{}
	if node.children_count > 0 {
		lhs_ids << t.a.child(&node, 0)
	}
	for i in 2 .. node.children_count {
		lhs_ids << t.a.child(&node, i)
	}
	return lhs_ids
}

fn (t &Transformer) multi_return_types_for_expr(id flat.NodeId, expected_count int) ?[]types.Type {
	if int(id) < 0 || isnil(t.tc) {
		return none
	}
	if typ := t.tc.expr_type(id) {
		if items := multi_return_types_from_type(typ, expected_count) {
			return items
		}
	}
	node := t.a.nodes[int(id)]
	if node.kind == .or_expr && node.children_count > 0 {
		return t.multi_return_types_for_expr(t.a.child(&node, 0), expected_count)
	}
	if node.kind == .match_stmt {
		return t.match_multi_return_types(node, expected_count)
	}
	if node.kind == .expr_stmt {
		return t.expr_stmt_multi_return_types(node, expected_count)
	}
	if node.kind == .block {
		return t.block_multi_return_types(node, expected_count)
	}
	mut typ_name := node.typ
	if node.kind == .call {
		ret := t.get_call_return_type(id, node)
		if ret.len > 0 {
			typ_name = ret
		}
	} else if typ_name.len == 0 {
		typ_name = t.resolve_expr_type(id)
	}
	if typ_name.len == 0 {
		if node.kind == .call {
			return t.find_multi_return_call_types(node, expected_count)
		}
		return none
	}
	typ := t.tc.parse_type(typ_name)
	if items := multi_return_types_from_type(typ, expected_count) {
		return items
	}
	if node.kind == .call {
		return t.find_multi_return_call_types(node, expected_count)
	}
	return none
}

fn (t &Transformer) match_multi_return_types(node flat.Node, expected_count int) ?[]types.Type {
	if node.children_count < 2 {
		return none
	}
	for i in 1 .. node.children_count {
		branch := t.a.child_node(&node, i)
		if branch.kind != .match_branch {
			continue
		}
		body_start := if branch.value == 'else' { 0 } else { t.count_conds(*branch) }
		if branch.children_count <= body_start {
			continue
		}
		tail_id := t.a.child(branch, branch.children_count - 1)
		if items := t.multi_return_types_for_expr(tail_id, expected_count) {
			return items
		}
	}
	return none
}

fn (t &Transformer) expr_stmt_multi_return_types(node flat.Node, expected_count int) ?[]types.Type {
	if expected_count <= 0 || node.children_count != expected_count {
		return none
	}
	mut result := []types.Type{cap: expected_count}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		mut typ_name := t.node_type(child_id)
		if typ_name.len == 0 {
			typ_name = t.resolve_expr_type(child_id)
		}
		if typ_name.len == 0 {
			return none
		}
		result << t.tc.parse_type(typ_name)
	}
	return result
}

fn (t &Transformer) block_multi_return_types(node flat.Node, expected_count int) ?[]types.Type {
	if expected_count <= 0 || node.children_count != expected_count {
		return none
	}
	mut result := []types.Type{cap: expected_count}
	for i in 0 .. node.children_count {
		stmt_id := t.a.child(&node, i)
		stmt := t.a.nodes[int(stmt_id)]
		if stmt.kind != .expr_stmt || stmt.children_count != 1 {
			return none
		}
		child_id := t.a.child(&stmt, 0)
		mut typ_name := t.node_type(child_id)
		if typ_name.len == 0 {
			typ_name = t.resolve_expr_type(child_id)
		}
		if typ_name.len == 0 {
			return none
		}
		result << t.tc.parse_type(typ_name)
	}
	return result
}

fn multi_return_types_from_type(typ types.Type, expected_count int) ?[]types.Type {
	if typ is types.MultiReturn {
		if expected_count <= 0 || typ.types.len == expected_count {
			return typ.types.clone()
		}
		return none
	}
	if typ is types.OptionType {
		return multi_return_types_from_type(typ.base_type, expected_count)
	}
	if typ is types.ResultType {
		return multi_return_types_from_type(typ.base_type, expected_count)
	}
	return none
}

fn (t &Transformer) find_multi_return_call_types(node flat.Node, expected_count int) ?[]types.Type {
	if node.kind != .call || node.children_count == 0 || isnil(t.tc) {
		return none
	}
	fn_node := t.a.child_node(&node, 0)
	mut candidates := []string{}
	if fn_node.kind == .ident {
		candidates << fn_node.value
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			candidates << '${t.cur_module}.${fn_node.value}'
		}
	} else if fn_node.kind == .selector {
		if fn_node.children_count > 0 {
			base_id := t.a.child(fn_node, 0)
			mut base_type := t.resolve_expr_type(base_id)
			if base_type.starts_with('&') {
				base_type = base_type[1..]
			}
			if base_type.len > 0 {
				candidates << '${base_type}.${fn_node.value}'
				if base_type.contains('.') {
					candidates << '${base_type.all_after_last('.')}.${fn_node.value}'
				}
			}
		}
		candidates << '.${fn_node.value}'
	}
	for candidate in candidates {
		if ret := t.tc.fn_ret_types[candidate] {
			if items := multi_return_types_from_type(ret, expected_count) {
				return items
			}
		}
	}
	for candidate in candidates {
		for key, ret in t.tc.fn_ret_types {
			matches := if candidate.starts_with('.') {
				key.ends_with(candidate)
			} else {
				key == candidate || key.ends_with('.${candidate}')
			}
			if matches {
				if items := multi_return_types_from_type(ret, expected_count) {
					return items
				}
			}
		}
	}
	return none
}

fn (t &Transformer) multi_return_type_name(items []types.Type) string {
	mut names := []string{cap: items.len}
	for item in items {
		names << item.name()
	}
	return '(${names.join(', ')})'
}

fn (mut t Transformer) expand_multi_return_if_decl(_rhs_id flat.NodeId, rhs flat.Node, lhs_ids []flat.NodeId) ?[]flat.NodeId {
	if lhs_ids.len == 0 {
		return none
	}
	value_types := t.infer_multi_if_value_types(rhs, lhs_ids.len)
	mut result := []flat.NodeId{}
	for i, lhs_id in lhs_ids {
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind != .ident || lhs.value == '_' {
			continue
		}
		typ := if i < value_types.len { value_types[i] } else { 'int' }
		result << t.make_decl_assign_typed(lhs.value, t.zero_value_for_type(typ), typ)
	}
	if_stmts := t.expand_multi_return_if_assign(_rhs_id, rhs, lhs_ids) or { return none }
	for stmt in if_stmts {
		result << stmt
	}
	return result
}

fn (mut t Transformer) expand_multi_return_if_assign(_rhs_id flat.NodeId, rhs flat.Node, lhs_ids []flat.NodeId) ?[]flat.NodeId {
	if rhs.kind != .if_expr || lhs_ids.len == 0 {
		return none
	}
	return t.lower_multi_if_assign(rhs, lhs_ids)
}

fn (mut t Transformer) lower_multi_if_assign(node flat.Node, lhs_ids []flat.NodeId) []flat.NodeId {
	if node.children_count < 2 {
		return []
	}
	cond_id := t.a.child(&node, 0)
	then_id := t.a.child(&node, 1)
	mut result := []flat.NodeId{}
	new_cond := t.transform_expr(cond_id)
	t.drain_pending(mut result)
	then_block := t.multi_if_assign_block(then_id, lhs_ids)
	mut else_block := t.make_empty()
	if node.children_count >= 3 {
		else_id := t.a.child(&node, 2)
		else_node := t.a.nodes[int(else_id)]
		if else_node.kind == .if_expr {
			else_stmts := t.lower_multi_if_assign(else_node, lhs_ids)
			else_block = t.make_block(else_stmts)
		} else {
			else_block = t.multi_if_assign_block(else_id, lhs_ids)
		}
	}
	result << t.make_if(new_cond, then_block, else_block)
	return result
}

fn (mut t Transformer) multi_if_assign_block(block_id flat.NodeId, lhs_ids []flat.NodeId) flat.NodeId {
	block := t.a.nodes[int(block_id)]
	parts := t.tuple_block_parts(block_id, lhs_ids.len) or { return t.transform_expr(block_id) }
	mut stmts := t.transform_stmts(parts.prefix)
	for i, value_id in parts.values {
		value := t.transform_expr(value_id)
		t.drain_pending(mut stmts)
		if i >= lhs_ids.len {
			stmts << t.make_expr_stmt(value)
			continue
		}
		lhs_id := lhs_ids[i]
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind == .ident && lhs.value == '_' {
			stmts << t.make_expr_stmt(value)
			continue
		}
		stmts << t.make_assign(t.transform_lvalue(lhs_id), value)
	}
	if block.kind == .block {
		return t.make_block(stmts)
	}
	return t.make_block(stmts)
}

fn (t &Transformer) tuple_block_parts(block_id flat.NodeId, count int) ?TupleBlockParts {
	if int(block_id) < 0 || count <= 0 {
		return none
	}
	block := t.a.nodes[int(block_id)]
	if block.kind != .block {
		return none
	}
	children := t.a.children_of(&block).clone()
	if children.len == 0 {
		return none
	}
	last_id := children[children.len - 1]
	last := t.a.nodes[int(last_id)]
	if last.kind == .block {
		if nested := t.tuple_block_parts(last_id, count) {
			if nested.prefix.len == 0 && nested.values.len == count {
				return TupleBlockParts{
					prefix: children[..children.len - 1].clone()
					values: nested.values.clone()
				}
			}
		}
	}
	mut values := []flat.NodeId{}
	mut prefix_end := children.len
	for i := children.len - 1; i >= 0; i-- {
		child_id := children[i]
		child := t.a.nodes[int(child_id)]
		if child.kind != .expr_stmt || child.children_count == 0 {
			break
		}
		values.prepend(t.a.child(&child, 0))
		prefix_end = i
		if values.len == count {
			return TupleBlockParts{
				prefix: children[..prefix_end].clone()
				values: values.clone()
			}
		}
	}
	return none
}

fn (t &Transformer) infer_multi_if_value_types(node flat.Node, count int) []string {
	mut result := []string{cap: count}
	if node.kind != .if_expr || node.children_count < 2 {
		return result
	}
	then_id := t.a.child(&node, 1)
	if parts := t.tuple_block_parts(then_id, count) {
		for value_id in parts.values {
			mut typ := t.tuple_value_type(value_id)
			if typ.len == 0 {
				typ = 'int'
			}
			result << typ
		}
	}
	for result.len < count {
		result << 'int'
	}
	return result
}

fn (t &Transformer) tuple_value_type(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.cast_expr {
			return node.value
		}
		.prefix {
			if node.children_count > 0 {
				inner := t.tuple_value_type(t.a.child(&node, 0))
				if node.op == .amp && inner.len > 0 {
					return '&${inner}'
				}
				if node.op == .mul && inner.starts_with('&') {
					return inner[1..]
				}
			}
			return ''
		}
		.paren {
			if node.children_count > 0 {
				return t.tuple_value_type(t.a.child(&node, 0))
			}
			return ''
		}
		else {
			mut typ := t.resolve_expr_type(id)
			if typ.len == 0 {
				typ = t.node_type(id)
			}
			return typ
		}
	}
}

fn (mut t Transformer) transform_expr_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return arr1(id)
	}
	child_id := t.a.children[node.children_start]
	child := t.a.nodes[int(child_id)]
	if child.kind == .call && t.is_disabled_fn_call(child_id, child) {
		return []flat.NodeId{}
	}
	if child.kind == .or_expr && !t.is_map_index_or_expr(child) {
		_ = t.lower_or_expr_to_temp(child_id, child)
		mut result := []flat.NodeId{}
		t.drain_pending(mut result)
		return result
	}
	if child.kind == .lock_expr {
		return t.transform_lock_stmt(child_id, child)
	}
	if lowered := t.try_lower_map_index_append_stmt(child_id) {
		return lowered
	}
	if lowered := t.try_lower_map_index_postfix_stmt(child_id) {
		return lowered
	}
	if lowered := t.try_lower_array_append_stmt(child_id) {
		return lowered
	}
	if lowered := t.try_lower_flag_enum_stmt(child_id) {
		return arr1(lowered)
	}
	new_child := t.transform_expr(child_id)
	start := t.a.children.len
	t.a.children << new_child
	new_id := t.a.add_node(flat.Node{
		kind:           .expr_stmt
		op:             node.op
		children_start: start
		children_count: 1
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
	return t.with_pending_before(new_id)
}

fn (mut t Transformer) transform_lock_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return arr1(id)
	}
	body_id := t.a.child(&node, node.children_count - 1)
	if int(body_id) < 0 {
		return arr1(id)
	}
	body := t.a.nodes[int(body_id)]
	if body.kind == .block {
		return t.transform_stmts(t.a.children_of(&body))
	}
	if t.is_stmt_kind_id(node_kind_id(body)) {
		return t.transform_stmt(body_id)
	}
	new_child := t.transform_expr(body_id)
	return t.with_pending_before(t.make_expr_stmt(new_child))
}

fn (mut t Transformer) transform_for_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	return t.transform_for_body(id, node)
}

fn (mut t Transformer) transform_for_in_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	return t.transform_for_in_body(id, node)
}

fn (mut t Transformer) transform_block_stmt(_id flat.NodeId, node flat.Node) []flat.NodeId {
	mut child_ids := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_ids << t.a.children[node.children_start + i]
	}
	new_children := t.transform_stmts(child_ids)
	new_block := t.make_block(new_children)
	return arr1(new_block)
}

fn (mut t Transformer) transform_block_expr(_id flat.NodeId, node flat.Node) flat.NodeId {
	mut child_ids := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_ids << t.a.children[node.children_start + i]
	}
	new_children := t.transform_stmts(child_ids)
	new_block := t.make_block(new_children)
	block_typ := t.stmt_value_type(new_block)
	t.a.nodes[int(new_block)].typ = if block_typ.len > 0 { block_typ } else { node.typ }
	return new_block
}

fn (mut t Transformer) transform_lock_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	body_id := t.a.child(&node, node.children_count - 1)
	if int(body_id) < 0 {
		return id
	}
	body := t.a.nodes[int(body_id)]
	if body.kind == .block {
		mut new_block := t.transform_block_expr(body_id, body)
		block_typ := t.stmt_value_type(new_block)
		if node.typ == 'void' || block_typ == 'void' {
			mut children := t.a.children_of(&t.a.nodes[int(new_block)]).clone()
			children << t.make_expr_stmt(t.make_int_literal(0))
			new_block = t.make_block(children)
			t.a.nodes[int(new_block)].typ = 'int'
		} else {
			t.a.nodes[int(new_block)].typ = node.typ
		}
		return new_block
	}
	return t.transform_expr(body_id)
}

fn (mut t Transformer) transform_if_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	if expanded := t.try_expand_if_guard(id, node) {
		return expanded
	}
	new_id := t.transform_if_branches_with_smartcast(id, node)
	return t.with_pending_before(new_id)
}

fn (mut t Transformer) transform_defer_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return arr1(id)
	}
	body_id := t.a.child(&node, 0)
	if int(body_id) < 0 {
		return arr1(id)
	}
	body := t.a.nodes[int(body_id)]
	new_body := if body.kind == .block {
		t.transform_block_expr(body_id, body)
	} else if t.is_stmt_kind_id(node_kind_id(body)) {
		t.make_block(t.transform_stmt(body_id))
	} else {
		t.make_block(arr1(t.transform_expr(body_id)))
	}
	start := t.a.children.len
	t.a.children << new_body
	new_id := t.a.add_node(flat.Node{
		kind:           .defer_stmt
		children_start: start
		children_count: 1
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
	return arr1(new_id)
}

// Generic handler: rebuild a node with all children recursively transformed.
fn (mut t Transformer) transform_children_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return arr1(id)
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if t.is_stmt_kind_id(node_kind_id(child)) {
			expanded := t.transform_stmt(child_id)
			for eid in expanded {
				new_children << eid
			}
		} else {
			new_children << t.transform_expr(child_id)
		}
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	count := new_children.len
	new_id := t.a.add_node(flat.Node{
		kind:           node.kind
		op:             node.op
		children_start: start
		children_count: flat.child_count(count)
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
	return arr1(new_id)
}

// --- expr handlers (skeleton - identity transforms with child recursion) ---

fn (mut t Transformer) transform_children_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if int(child_id) < 0 {
			new_children << child_id
			continue
		}
		child := t.a.nodes[int(child_id)]
		if t.is_stmt_kind_id(node_kind_id(child)) {
			expanded := t.transform_stmt(child_id)
			if expanded.len == 1 {
				new_children << expanded[0]
			} else {
				new_children << t.make_block(expanded)
			}
		} else {
			new_children << t.transform_expr(child_id)
		}
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	return t.a.add_node(flat.Node{
		kind:           node.kind
		op:             node.op
		children_start: start
		children_count: flat.child_count(new_children.len)
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

fn (mut t Transformer) transform_infix_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count < 2 {
		return id
	}
	if node.op == .logical_and {
		return t.transform_and_chain_smartcasts(id)
	}
	if node.op == .left_shift {
		lhs_id := t.a.children[node.children_start]
		rhs_id := t.a.children[node.children_start + 1]
		new_lhs := t.transform_expr(lhs_id)
		new_rhs := t.transform_expr(rhs_id)
		start := t.a.children.len
		t.a.children << new_lhs
		t.a.children << new_rhs
		new_id := t.a.add_node(flat.Node{
			kind:           .infix
			op:             node.op
			children_start: start
			children_count: 2
			pos:            node.pos
			value:          node.value
			typ:            node.typ
		})
		t.annotate_left_shift(new_id)
		return new_id
	}
	if str_result := t.transform_infix_string_ops(id, node) {
		return str_result
	}
	if array_result := t.transform_infix_array_ops(id, node) {
		return array_result
	}
	if map_result := t.transform_infix_map_ops(id, node) {
		return map_result
	}
	if optional_result := t.transform_infix_optional_none_ops(id, node) {
		return optional_result
	}
	if sum_result := t.transform_infix_sum_ops(id, node) {
		return sum_result
	}
	if struct_result := t.transform_infix_struct_ops(id, node) {
		return struct_result
	}
	lhs_id := t.a.children[node.children_start]
	rhs_id := t.a.children[node.children_start + 1]
	new_lhs := t.transform_expr(lhs_id)
	new_rhs := t.transform_expr(rhs_id)
	if struct_result := t.transform_transformed_struct_eq(node, new_lhs, new_rhs) {
		return struct_result
	}
	start := t.a.children.len
	t.a.children << new_lhs
	t.a.children << new_rhs
	return t.a.add_node(flat.Node{
		kind:           .infix
		op:             node.op
		children_start: start
		children_count: 2
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

fn (mut t Transformer) transform_call_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	call_id := t.normalize_generic_call_expr(id, node)
	mut call_node := t.a.nodes[int(call_id)]
	resolved_typ := t.get_call_return_type(call_id, call_node)
	if resolved_typ.len > 0 {
		t.a.nodes[int(call_id)].typ = resolved_typ
		call_node.typ = resolved_typ
	}
	if t.is_disabled_fn_call(call_id, call_node) {
		if resolved_typ.len == 0 || resolved_typ == 'void' {
			return t.make_empty()
		}
		return t.zero_value_for_type(resolved_typ)
	}
	if lowered := t.try_lower_builtin_call(call_id, call_node) {
		return lowered
	}
	if lowered := t.try_lower_join_path_call(call_id, call_node) {
		return lowered
	}
	return t.transform_call_args(call_id, call_node)
}

fn (t &Transformer) is_disabled_fn_name(name string) bool {
	if name in t.a.disabled_fns {
		return true
	}
	if !name.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		return '${t.cur_module}.${name}' in t.a.disabled_fns
	}
	return false
}

fn (t &Transformer) is_disabled_fn_call(id flat.NodeId, node flat.Node) bool {
	name := t.call_name_for_node(id, node)
	return t.is_disabled_fn_name(name)
}

fn (t &Transformer) is_strings_builder_new_call(id flat.NodeId, node flat.Node) bool {
	// Only `strings.new_builder` returns a `strings.Builder` (an alias for `[]u8`).
	// A bare `new_builder` must NOT be assumed to be the strings one: other modules
	// (e.g. `builder.new_builder`) and user code define their own `new_builder` that
	// return unrelated struct types. Resolve the call to its qualified name and only
	// match when it is genuinely the strings module's function.
	call_name := t.call_name_for_node(id, node)
	if call_name == 'strings.new_builder' {
		return true
	}
	if node.children_count == 0 {
		return false
	}
	fn_id := t.a.child(&node, 0)
	if int(fn_id) < 0 {
		return false
	}
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind == .ident {
		return fn_node.value == 'strings.new_builder'
	}
	if fn_node.kind == .selector && fn_node.value == 'new_builder' && fn_node.children_count > 0 {
		base := t.a.child_node(&fn_node, 0)
		return base.kind == .ident && base.value == 'strings'
	}
	return false
}

fn (mut t Transformer) transform_if_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if lowered := t.try_expand_if_expr_value(id, node) {
		return lowered
	}
	return t.transform_if_branches_with_smartcast(id, node)
}

fn (mut t Transformer) transform_struct_init(id flat.NodeId, node flat.Node) flat.NodeId {
	return t.transform_struct_fields(id, node)
}

fn (mut t Transformer) transform_index_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	if lowered := t.try_lower_map_index_expr(id, node) {
		return lowered
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		mut new_child := t.transform_expr(child_id)
		if i == 0 {
			base := t.a.nodes[int(new_child)]
			if base.kind == .cast_expr {
				base_type := t.node_type(new_child)
				new_child = t.make_paren(new_child)
				if base_type.len > 0 {
					t.a.nodes[int(new_child)].typ = base_type
				}
			}
		}
		new_children << new_child
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	return t.a.add_node(flat.Node{
		kind:           .index
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            if node.typ.len > 0 { node.typ } else { node.value }
	})
}

fn (mut t Transformer) transform_string_interp(_id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return t.make_string_literal('')
	}
	mut parts := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		transformed := t.transform_expr(child_id)
		mut typ := t.node_type(transformed)
		if typ.len == 0 {
			typ = t.reliable_stringify_type(transformed)
		}
		if typ.len == 0 {
			typ = t.reliable_stringify_type(child_id)
		}
		if typ.len == 0 {
			typ = t.node_type(child_id)
		}
		if typ.len == 0 {
			typ = 'string'
		}
		parts << t.wrap_string_conversion(transformed, typ)
	}
	mut result := if parts.len == 0 { t.make_string_literal('') } else { parts[0] }
	for i in 1 .. parts.len {
		result = t.string_plus(result, parts[i])
	}
	t.a.nodes[int(result)].typ = 'string'
	return result
}

fn (mut t Transformer) transform_selector_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	if node.value in t.sum_variant_fields {
		return id
	}
	base_id0 := t.a.child(&node, 0)
	if variant_type := t.generated_variant_access_type(base_id0) {
		new_base := t.transform_expr(base_id0)
		clean_variant_type := t.trim_pointer_type(variant_type)
		sel_typ := if node.typ.len > 0 {
			node.typ
		} else if ftyp := t.lookup_struct_field_type(clean_variant_type, node.value) {
			ftyp
		} else {
			t.resolve_selector_type(node)
		}
		return t.make_selector_op(new_base, node.value, sel_typ, if variant_type.starts_with('&') {
			.arrow
		} else {
			node.op
		})
	}
	base_node0 := t.a.nodes[int(base_id0)]
	if base_node0.kind == .typeof_expr {
		if node.value == 'name' {
			return t.transform_typeof_expr(base_id0, base_node0)
		}
		if node.value == 'idx' {
			return t.transform_typeof_idx_expr(base_node0)
		}
	}
	if fixed_len := t.transform_fixed_array_len(id, node) {
		return fixed_len
	}
	full_key := t.expr_key(id)
	if full_key.len > 0 {
		contexts := t.smartcasts_for(full_key)
		if contexts.len > 0 {
			plain := t.make_plain_selector_expr(id, node)
			return t.apply_smartcast_contexts(plain, t.original_expr_type(id), contexts)
		}
	}
	base_id := base_id0
	sc_key := t.expr_key(base_id)
	if sc_key.len > 0 {
		contexts := t.smartcasts_for(sc_key)
		if contexts.len > 0 {
			plain_base := t.make_plain_expr_for_smartcast(base_id)
			variant_sel := t.apply_smartcast_contexts(plain_base, t.original_expr_type(base_id),
				contexts)
			variant_type := t.node_type(variant_sel)
			if shared_typ := t.sum_shared_field_type_name(variant_type, node.value) {
				return t.lower_sum_shared_field_selector(variant_sel, variant_type, node.value,
					shared_typ)
			}
			sel_start := t.a.children.len
			t.a.children << variant_sel
			sel_typ := if node.typ.len > 0 { node.typ } else { t.resolve_selector_type(node) }
			return t.a.add_node(flat.Node{
				kind:           .selector
				op:             if node.op == .arrow || variant_type.starts_with('&') {
					flat.Op.arrow
				} else {
					flat.Op.dot
				}
				children_start: sel_start
				children_count: 1
				pos:            node.pos
				value:          node.value
				typ:            sel_typ
			})
		}
	}
	base_type0 := t.node_type(base_id)
	base_clean := if base_type0.starts_with('&') { base_type0[1..] } else { base_type0 }
	if info := t.lookup_struct_info(base_clean) {
		has_direct_field := (t.struct_field_type(info, node.value) or { '' }).len > 0
		if !has_direct_field {
			if embedded := t.embedded_field_for_promoted_field(info, node.value) {
				new_base := t.transform_expr(base_id)
				embedded_op := if base_type0.starts_with('&') { flat.Op.arrow } else { flat.Op.dot }
				embedded_sel := t.make_selector_op(new_base, embedded.name, embedded.typ,
					embedded_op)
				sel_typ := if node.typ.len > 0 { node.typ } else { t.resolve_selector_type(node) }
				final_op := if embedded.typ.starts_with('&') { flat.Op.arrow } else { flat.Op.dot }
				return t.make_selector_op(embedded_sel, node.value, sel_typ, final_op)
			}
		}
	}
	if shared_typ := t.sum_shared_field_type_name(base_type0, node.value) {
		transformed_base := t.transform_expr(base_id)
		transformed_base_type := t.node_type(transformed_base)
		clean_transformed_base_type := if transformed_base_type.starts_with('&') {
			transformed_base_type[1..]
		} else {
			transformed_base_type
		}
		if clean_transformed_base_type.len > 0
			&& t.normalize_type_alias(clean_transformed_base_type) != t.normalize_type_alias(base_type0) {
			if ftyp := t.lookup_struct_field_type(clean_transformed_base_type, node.value) {
				new_base := t.selector_base_for_field(transformed_base, transformed_base_type)
				return t.make_selector_op(new_base, node.value, if node.typ.len > 0 {
					node.typ
				} else {
					ftyp
				}, if transformed_base_type.starts_with('&') {
					.arrow
				} else {
					.dot
				})
			}
			if new_shared_typ := t.sum_shared_field_type_name(transformed_base_type, node.value) {
				new_base := t.selector_base_for_field(transformed_base, transformed_base_type)
				return t.lower_sum_shared_field_selector(new_base, transformed_base_type,
					node.value, new_shared_typ)
			}
		}
		new_base := t.selector_base_for_field(transformed_base, base_type0)
		return t.lower_sum_shared_field_selector(new_base, base_type0, node.value, shared_typ)
	}
	new_base := t.transform_expr(base_id)
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	new_children << new_base
	for i in 1 .. node.children_count {
		child_id := t.a.child(&node, i)
		new_children << t.transform_expr(child_id)
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	sel_typ := if node.typ.len > 0 { node.typ } else { t.resolve_selector_type(node) }
	base_type := t.node_type(base_id)
	sel_op := if node.op == .arrow || base_type.starts_with('&') { flat.Op.arrow } else { node.op }
	return t.a.add_node(flat.Node{
		kind:           .selector
		op:             sel_op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            sel_typ
	})
}

fn (mut t Transformer) make_plain_selector_expr(_id flat.NodeId, node flat.Node) flat.NodeId {
	base_id := t.a.child(&node, 0)
	base_type := t.node_type(base_id)
	new_base := t.selector_base_for_field(t.transform_expr(base_id), base_type)
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	new_children << new_base
	for i in 1 .. node.children_count {
		child_id := t.a.child(&node, i)
		new_children << t.transform_expr(child_id)
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	sel_typ := if node.typ.len > 0 { node.typ } else { t.resolve_selector_type(node) }
	sel_op := if node.op == .arrow || base_type.starts_with('&') { flat.Op.arrow } else { node.op }
	return t.a.add_node(flat.Node{
		kind:           .selector
		op:             sel_op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            sel_typ
	})
}

fn (mut t Transformer) make_plain_expr_for_smartcast(id flat.NodeId) flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			expr := t.make_ident(node.value)
			typ := t.original_expr_type(id)
			if typ.len > 0 {
				t.a.nodes[int(expr)].typ = typ
			}
			return expr
		}
		.selector {
			return t.make_plain_selector_expr(id, node)
		}
		.index {
			mut new_children := []flat.NodeId{cap: int(node.children_count)}
			for i in 0 .. node.children_count {
				new_children << t.transform_expr(t.a.child(&node, i))
			}
			start := t.a.children.len
			for nc in new_children {
				t.a.children << nc
			}
			return t.a.add_node(flat.Node{
				kind:           .index
				op:             node.op
				children_start: start
				children_count: node.children_count
				pos:            node.pos
				value:          node.value
				typ:            if node.typ.len > 0 { node.typ } else { node.value }
			})
		}
		else {
			return t.transform_expr(id)
		}
	}
}

fn (mut t Transformer) selector_base_for_field(base flat.NodeId, typ string) flat.NodeId {
	if int(base) < 0 {
		return base
	}
	node := t.a.nodes[int(base)]
	if node.kind in [.if_expr, .block] {
		return t.stable_transformed_expr_for_reuse(base, typ, 'sel_base')
	}
	return base
}

fn (t &Transformer) sum_shared_field_type_name(sum_type string, field string) ?string {
	clean_sum := if sum_type.starts_with('&') { sum_type[1..] } else { sum_type }
	resolved_sum := t.resolve_sum_name(clean_sum)
	variants := t.sum_types[resolved_sum] or { return none }
	mut common := ''
	for variant in variants {
		ftyp := t.sum_variant_field_type_name(variant, field) or { return none }
		if common.len == 0 {
			common = ftyp
			continue
		}
		if t.normalize_type_alias(common) != t.normalize_type_alias(ftyp) {
			return none
		}
	}
	if common.len == 0 {
		return none
	}
	return common
}

fn (t &Transformer) sum_variant_field_type_name(variant string, field string) ?string {
	if ftyp := t.lookup_struct_field_type(variant, field) {
		return ftyp
	}
	if ftyp := t.sum_shared_field_type_name(variant, field) {
		return ftyp
	}
	return none
}

fn (mut t Transformer) lower_sum_shared_field_selector(base flat.NodeId, sum_type string, field string, field_type string) flat.NodeId {
	clean_sum := if sum_type.starts_with('&') { sum_type[1..] } else { sum_type }
	resolved_sum := t.resolve_sum_name(clean_sum)
	variants := t.sum_types[resolved_sum] or { return base }
	return t.build_sum_shared_field_chain(base, sum_type, resolved_sum, variants, field,
		field_type, 0)
}

fn (mut t Transformer) build_sum_shared_field_chain(base flat.NodeId, sum_type string, resolved_sum string, variants []string, field string, field_type string, idx int) flat.NodeId {
	if idx >= variants.len {
		return t.zero_value_for_type(field_type)
	}
	variant := variants[idx]
	tag := t.make_selector_op(base, 'typ', 'int', if sum_type.starts_with('&') {
		.arrow
	} else {
		.dot
	})
	cond := t.make_infix(.eq, tag, t.make_int_literal(t.sum_type_index(resolved_sum, variant)))
	qv := t.resolve_variant(resolved_sum, variant)
	sum_field := t.sum_field_name(qv)
	use_ptr := t.variant_references_sum(qv, resolved_sum)
	variant_base := t.make_selector_op(base, sum_field, if use_ptr { '&${qv}' } else { qv }, if sum_type.starts_with('&') {
		.arrow
	} else {
		.dot
	})
	value := if _ := t.sum_shared_field_type_name(qv, field) {
		t.lower_sum_shared_field_selector(variant_base, qv, field, field_type)
	} else {
		t.make_selector_op(variant_base, field, field_type, if use_ptr { .arrow } else { .dot })
	}
	then_block := t.make_block(arr1(t.make_expr_stmt(value)))
	else_expr := t.build_sum_shared_field_chain(base, sum_type, resolved_sum, variants, field,
		field_type, idx + 1)
	else_block := t.make_block(arr1(t.make_expr_stmt(else_expr)))
	start := t.a.children.len
	t.a.children << cond
	t.a.children << then_block
	t.a.children << else_block
	return t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: start
		children_count: 3
		typ:            field_type
	})
}

fn (mut t Transformer) transform_or_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count < 2 {
		return id
	}
	if t.is_map_index_or_expr(node) {
		return t.transform_map_index_or_expr(id, node)
	}
	if t.is_array_index_or_expr(node) {
		return t.transform_array_index_or_expr(id, node)
	}
	if t.is_enum_from_string_or_expr(node) {
		return t.transform_enum_from_string_or_expr(id, node)
	}
	return t.lower_or_expr_to_temp(id, node)
}

fn (mut t Transformer) transform_prefix_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	if node.op == .mul && node.children_count == 1 {
		child_id := t.a.child(&node, 0)
		child := t.a.nodes[int(child_id)]
		mut child_type := t.node_type(child_id)
		if child_type.len == 0 {
			child_type = t.original_expr_type(child_id)
		}
		if child.kind != .cast_expr && child_type.len > 0 && !child_type.starts_with('&') {
			return t.transform_expr(child_id)
		}
	}
	if node.op == .amp && node.children_count == 1 {
		child_id := t.a.child(&node, 0)
		child := t.a.nodes[int(child_id)]
		if t.in_return_expr && child.kind == .struct_init {
			if expr := t.transform_amp_struct_init_for_type(id, node, node.typ) {
				return expr
			}
		}
		if expr := t.transform_amp_assoc_expr_for_type(id, node, node.typ) {
			return expr
		}
		if child.kind == .cast_expr && child.children_count > 0 {
			cast_arg_id := t.a.child(&child, 0)
			target_sum := t.resolve_sum_name(t.normalize_type_alias(child.value))
			if target_sum.len > 0 && target_sum in t.sum_types {
				cast_arg := t.a.nodes[int(cast_arg_id)]
				if cast_arg.kind == .nil_literal {
					return t.make_cast('&${child.value}', t.transform_expr(cast_arg_id),
						'&${child.value}')
				}
				wrapped := t.wrap_sum_value(cast_arg_id, target_sum)
				addr := t.make_prefix(.amp, wrapped)
				t.a.nodes[int(addr)].typ = if node.typ.len > 0 { node.typ } else { '&${target_sum}' }
				return addr
			}
			// `&InterfaceType(x)` (e.g. `&PRNG(rng)`): box the concrete into a
			// heap-allocated interface so the resulting pointer stays valid, rather
			// than emitting a plain `(Interface*)x` reinterpret cast.
			iface := t.resolve_interface_type_name(child.value)
			if iface.len > 0 && iface.all_after_last('.') != 'IError' {
				if boxed := t.transform_interface_value_for_type(cast_arg_id, '&${child.value}') {
					return boxed
				}
			}
			cast_arg := t.a.nodes[int(cast_arg_id)]
			if cast_arg.kind == .nil_literal {
				return t.make_cast('&${child.value}', t.transform_expr(cast_arg_id),
					'&${child.value}')
			}
			return t.make_cast('&${child.value}', t.transform_expr(cast_arg_id), '&${child.value}')
		}
		if child.kind == .or_expr && child.children_count >= 2
			&& t.or_body_is_nil(t.a.child(&child, 1)) {
			index_id := t.a.child(&child, 0)
			if info := t.map_index_info(index_id) {
				map_expr := t.stable_expr_for_reuse(info.base_id)
				key_name := t.new_temp('map_key')
				t.pending_stmts << t.make_decl_assign_typed(key_name,
					t.transform_expr(info.key_id), info.key_type)
				ptr := t.make_map_get_check_expr(map_expr, info.base_type, key_name)
				return t.make_cast('&${info.value_type}', ptr, '&${info.value_type}')
			}
		}
		if child.kind == .call && child.children_count == 2 {
			callee := t.a.child_node(&child, 0)
			arg_id := t.a.child(&child, 1)
			arg := t.a.nodes[int(arg_id)]
			if callee.kind == .selector && callee.children_count > 0
				&& (arg.kind == .nil_literal || callee.value.len > 0) {
				base := t.a.child_node(callee, 0)
				if base.kind == .ident && callee.value.len > 0
					&& (base.value == 'C' || (callee.value[0] >= `A` && callee.value[0] <= `Z`)) {
					target_type := '${base.value}.${callee.value}'
					return t.make_cast('&${target_type}', t.transform_expr(arg_id),
						'&${target_type}')
				}
			}
		}
		if child.kind == .selector && (t.selector_chain_has_sum_shared_field(child_id)
			|| t.selector_chain_has_sum_variant_field(child_id)) {
			value := t.transform_expr(child_id)
			mut value_type := t.node_type(child_id)
			if value_type.len == 0 {
				value_type = t.node_type(value)
			}
			stable := t.stable_transformed_expr_for_reuse(value, value_type, 'addr')
			addr := t.make_prefix(.amp, stable)
			if value_type.len > 0 {
				t.a.nodes[int(addr)].typ = '&${value_type}'
			}
			return addr
		}
		if child.kind == .ident && child.value.len > 0 && t.has_smartcast(child.value)
			&& node.typ.starts_with('&') && t.is_sum_type_name(node.typ[1..]) {
			sum_type := node.typ[1..]
			wrapped := t.wrap_sum_value(child_id, sum_type)
			tmp_name := t.new_temp('sum_ref')
			t.pending_stmts << t.make_decl_assign_typed(tmp_name, wrapped, sum_type)
			addr := t.make_prefix(.amp, t.make_ident(tmp_name))
			t.a.nodes[int(addr)].typ = node.typ
			return addr
		}
		value := t.transform_expr(child_id)
		if !t.expr_can_take_address(value) {
			mut value_type := t.node_type(child_id)
			if value_type.len == 0 {
				value_type = t.node_type(value)
			}
			stable := t.stable_transformed_expr_for_reuse(value, value_type, 'addr')
			addr := t.make_prefix(.amp, stable)
			if value_type.len > 0 {
				t.a.nodes[int(addr)].typ = '&${value_type}'
			}
			return addr
		}
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		mut new_child := t.transform_expr(child_id)
		if node.op == .not {
			child := t.a.nodes[int(new_child)]
			if child.kind == .infix {
				new_child = t.make_paren(new_child)
			}
		}
		new_children << new_child
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	new_id := t.a.add_node(flat.Node{
		kind:           .prefix
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
	if node.children_count == 1 {
		child_type := t.node_type(new_children[0])
		if node.op == .amp && child_type.len > 0 {
			t.a.nodes[int(new_id)].typ = '&${child_type}'
		} else if node.op == .mul && child_type.starts_with('&') {
			t.a.nodes[int(new_id)].typ = child_type[1..]
		}
	}
	return new_id
}

fn (mut t Transformer) transform_amp_sum_cast_from_as_expr(cast_node flat.Node, cast_arg_id flat.NodeId) ?flat.NodeId {
	target_sum := t.resolve_sum_name(cast_node.value)
	if target_sum.len == 0 || target_sum !in t.sum_types || int(cast_arg_id) < 0 {
		return none
	}
	mut arg_id := cast_arg_id
	for {
		arg0 := t.a.nodes[int(arg_id)]
		if arg0.kind != .paren || arg0.children_count == 0 {
			break
		}
		arg_id = t.a.child(&arg0, 0)
	}
	arg := t.a.nodes[int(arg_id)]
	if arg.kind != .as_expr || arg.children_count == 0 || arg.value.len == 0 {
		return none
	}
	source_id := t.a.child(&arg, 0)
	mut source_type := t.node_type(source_id)
	if source_type.len == 0 {
		source_type = t.original_expr_type(source_id)
	}
	mut source_sum := t.resolve_sum_name(t.trim_pointer_type(source_type))
	mut use_plain_source := false
	if source_sum.len == 0 || source_sum !in t.sum_types {
		raw_source_type := t.raw_expr_type_without_smartcast(source_id)
		raw_source_sum := t.resolve_sum_name(t.trim_pointer_type(raw_source_type))
		if raw_source_sum.len > 0 && raw_source_sum in t.sum_types {
			source_type = raw_source_type
			source_sum = raw_source_sum
			use_plain_source = true
		}
	}
	if source_sum.len == 0 || source_sum !in t.sum_types {
		return none
	}
	variant := t.resolve_variant(source_sum, arg.value)
	if variant.len == 0 || !t.variant_references_sum(variant, source_sum) {
		return none
	}
	source := if use_plain_source {
		t.make_plain_expr_for_smartcast(source_id)
	} else {
		t.transform_expr(source_id)
	}
	field_name := t.sum_field_name(variant)
	field_sel := t.make_selector_op(source, field_name, '&${variant}', if source_type.starts_with('&') {
		.arrow
	} else {
		.dot
	})
	return t.make_cast('&${cast_node.value}', field_sel, '&${cast_node.value}')
}

fn (t &Transformer) raw_expr_type_without_smartcast(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			typ := t.normalize_type_alias(t.var_type(node.value))
			if typ.len > 0 {
				return typ
			}
			return t.normalize_type_alias(node.typ)
		}
		.selector {
			return t.raw_selector_type_without_smartcast(id)
		}
		else {
			return t.normalize_type_alias(node.typ)
		}
	}
}

fn (t &Transformer) raw_selector_type_without_smartcast(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	if node.kind != .selector || node.children_count == 0 {
		return ''
	}
	base_id := t.a.child(&node, 0)
	mut base_type := t.raw_expr_type_without_smartcast(base_id)
	if base_type.len == 0 {
		base_type = t.original_expr_type(base_id)
	}
	if ftyp := t.lookup_struct_field_type(base_type, node.value) {
		return ftyp
	}
	return t.normalize_type_alias(node.typ)
}

fn (t &Transformer) selector_chain_has_sum_shared_field(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind != .selector || node.children_count == 0 {
		return false
	}
	base_id := t.a.child(&node, 0)
	base_type := t.node_type(base_id)
	if _ := t.sum_shared_field_type_name(base_type, node.value) {
		return true
	}
	return t.selector_chain_has_sum_shared_field(base_id)
}

fn (t &Transformer) selector_chain_has_sum_variant_field(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind != .selector || node.children_count == 0 {
		return false
	}
	base_id := t.a.child(&node, 0)
	base_type := t.node_type(base_id)
	if t.sum_has_variant_field(base_type, node.value) {
		return true
	}
	return t.selector_chain_has_sum_variant_field(base_id)
}

fn (t &Transformer) sum_has_variant_field(sum_type string, field string) bool {
	clean_sum := if sum_type.starts_with('&') { sum_type[1..] } else { sum_type }
	resolved_sum := t.resolve_sum_name(clean_sum)
	variants := t.sum_types[resolved_sum] or { return false }
	for variant in variants {
		if _ := t.sum_variant_field_type_name(variant, field) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) transform_paren_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	child_id := t.a.child(&node, 0)
	new_child := t.transform_expr(child_id)
	start := t.a.children.len
	t.a.children << new_child
	return t.a.add_node(flat.Node{
		kind:           .paren
		op:             node.op
		children_start: start
		children_count: 1
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

fn (mut t Transformer) transform_postfix_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	child_id := t.a.child(&node, 0)
	child := t.a.nodes[int(child_id)]
	new_child := if child.kind == .ident && t.pointer_value_lvalues[child.value] {
		t.make_paren(t.make_prefix(.mul, t.make_ident(child.value)))
	} else {
		t.transform_expr(child_id)
	}
	start := t.a.children.len
	t.a.children << new_child
	return t.a.add_node(flat.Node{
		kind:           .postfix
		op:             node.op
		children_start: start
		children_count: 1
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

fn (mut t Transformer) transform_cast_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	target_type := t.normalize_type_alias(node.value)
	if target_type.starts_with('&') && !t.is_interface_type(target_type) {
		mut new_children := []flat.NodeId{cap: int(node.children_count)}
		for i in 0 .. node.children_count {
			child_id := t.a.child(&node, i)
			new_children << t.transform_expr(child_id)
		}
		start := t.a.children.len
		for nc in new_children {
			t.a.children << nc
		}
		return t.a.add_node(flat.Node{
			kind:           .cast_expr
			op:             node.op
			children_start: start
			children_count: node.children_count
			pos:            node.pos
			value:          node.value
			typ:            node.typ
		})
	}
	if t.is_optional_type_name(node.value) {
		child_id := t.a.child(&node, 0)
		expr := t.transform_expr(child_id)
		mut expr_type := t.node_type(expr)
		if expr_type.len == 0 {
			expr_type = t.resolve_expr_type(child_id)
		}
		if t.is_optional_type_name(expr_type) {
			return t.coerce_transformed_expr_to_type(expr, child_id, node.value)
		}
		return t.make_optional_some(expr, t.qualify_optional_type(node.value))
	}
	if t.is_sum_type_name(target_type) {
		return t.wrap_sum_value(t.a.child(&node, 0), target_type)
	}
	// An explicit cast to an interface (`Animal(dog)`, `&PRNG(rng)`) boxes the
	// concrete value into the interface representation, just like an implicit
	// conversion does.
	if t.is_interface_type(target_type) {
		if boxed := t.transform_interface_value_for_type(t.a.child(&node, 0), node.value) {
			return boxed
		}
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		new_children << t.transform_expr(child_id)
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	return t.a.add_node(flat.Node{
		kind:           .cast_expr
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

fn (mut t Transformer) transform_array_literal(id flat.NodeId, node flat.Node) flat.NodeId {
	lowered := t.lower_array_literal_to_runtime(id, node)
	if lowered != id {
		return lowered
	}
	if node.children_count == 0 {
		return id
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		new_children << t.transform_expr(child_id)
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	return t.a.add_node(flat.Node{
		kind:           .array_literal
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

fn (mut t Transformer) transform_map_init(id flat.NodeId, node flat.Node) flat.NodeId {
	return t.transform_map_init_expr(id, node)
}

fn (mut t Transformer) transform_typeof_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.value.len > 0 {
		return t.make_string_literal(node.value)
	}
	if node.children_count == 0 {
		return id
	}
	expr_id := t.a.child(&node, 0)
	expr := t.a.nodes[int(expr_id)]
	if expr.kind == .int_literal {
		return t.make_string_literal('int literal')
	}
	mut typ := t.node_type(expr_id)
	if typ.len == 0 {
		typ = t.reliable_stringify_type(expr_id)
	}
	if typ.len == 0 {
		typ = t.resolve_expr_type(expr_id)
	}
	if typ.len == 0 {
		typ = 'unknown'
	}
	return t.make_string_literal(typ)
}

fn (mut t Transformer) transform_typeof_idx_expr(node flat.Node) flat.NodeId {
	type_name := t.typeof_type_name(node)
	return t.make_int_literal(t.type_index_for_type_name(type_name))
}

fn (t &Transformer) typeof_type_name(node flat.Node) string {
	if node.value.len > 0 {
		return node.value
	}
	if node.children_count == 0 {
		return ''
	}
	expr_id := t.a.child(&node, 0)
	mut typ := t.node_type(expr_id)
	if typ.len == 0 {
		typ = t.reliable_stringify_type(expr_id)
	}
	if typ.len == 0 {
		typ = t.resolve_expr_type(expr_id)
	}
	return typ
}

fn (t &Transformer) type_index_for_type_name(type_name string) int {
	if type_name.len == 0 {
		return 0
	}
	mut variants := []string{cap: 2}
	variants << type_name
	normalized := t.normalize_type_in_module(type_name, t.cur_module)
	if normalized.len > 0 && normalized !in variants {
		variants << normalized
	}
	mut sum_names := []string{}
	if t.cur_module.len > 0 {
		sum_names << '${t.cur_module}.Primitive'
	}
	sum_names << 'orm.Primitive'
	sum_names << 'Primitive'
	for sum_name in sum_names {
		if sum_name !in t.sum_types {
			continue
		}
		for variant in variants {
			idx := t.sum_type_index(sum_name, variant)
			if idx != 0 {
				return idx
			}
		}
	}
	for variant in variants {
		sum_name := t.find_sum_type_for_variant(variant)
		if sum_name.len > 0 {
			idx := t.sum_type_index(sum_name, variant)
			if idx != 0 {
				return idx
			}
		}
	}
	return 0
}

fn (mut t Transformer) transform_ident_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	match node.value {
		'@VMODROOT' {
			return t.make_string_literal(t.vmod_root())
		}
		else {
			if smartcasted := t.smartcast_ident_value(node.value) {
				return smartcasted
			}
			if !t.in_call_callee {
				if fn_name := t.resolve_fn_value_ident(node.value) {
					return t.a.add_node(flat.Node{
						kind:  .ident
						value: fn_name
						typ:   node.typ
						pos:   node.pos
					})
				}
			}
			typ := t.var_type(node.value)
			if typ.len > 0 {
				return t.a.add_node(flat.Node{
					kind:  .ident
					value: node.value
					typ:   typ
					pos:   node.pos
				})
			}
			return id
		}
	}
}

fn (mut t Transformer) smartcast_ident_value(name string) ?flat.NodeId {
	if t.smartcast_stack.len == 0 {
		return none
	}
	contexts := t.smartcasts_for(name)
	if contexts.len == 0 {
		return none
	}
	return t.apply_smartcast_contexts(t.make_ident(name), t.var_type(name), contexts)
}

fn (mut t Transformer) apply_smartcast_contexts(base flat.NodeId, typ string, contexts []SmartcastContext) flat.NodeId {
	mut current := base
	mut current_type := typ
	for i, sc in contexts {
		if t.is_interface_type_name(sc.sum_type_name) {
			qv := t.interface_variant_type(sc.variant_name)
			field_op := if current_type.starts_with('&') { flat.Op.arrow } else { flat.Op.dot }
			object := t.make_selector_op(current, '_object', 'voidptr', field_op)
			cast := t.make_cast('&${qv}', object, '&${qv}')
			current = t.make_prefix(.mul, cast)
			t.a.nodes[int(current)].typ = qv
			current_type = qv
			continue
		}
		qv := t.resolve_variant(sc.sum_type_name, sc.variant_name)
		if t.expr_is_variant_access(current, qv) {
			current_type = qv
			continue
		}
		field := t.sum_field_name(qv)
		use_ptr := t.variant_references_sum(qv, sc.sum_type_name)
		field_typ := if use_ptr { '&${qv}' } else { qv }
		field_op := if current_type.starts_with('&') { flat.Op.arrow } else { flat.Op.dot }
		field_sel := t.make_selector_op(current, field, field_typ, field_op)
		if use_ptr && i == contexts.len - 1 {
			current = t.make_prefix(.mul, field_sel)
			t.a.nodes[int(current)].typ = qv
			current_type = qv
		} else {
			current = field_sel
			current_type = field_typ
		}
	}
	return current
}

fn (t &Transformer) expr_is_variant_access(id flat.NodeId, variant string) bool {
	if int(id) < 0 || variant.len == 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	field := t.sum_field_name(variant)
	match node.kind {
		.selector {
			return node.value == field
		}
		.prefix {
			if node.op == .mul && node.children_count > 0 {
				return t.expr_is_variant_access(t.a.child(&node, 0), variant)
			}
			return false
		}
		.paren {
			if node.children_count > 0 {
				return t.expr_is_variant_access(t.a.child(&node, 0), variant)
			}
			return false
		}
		else {
			return false
		}
	}
}

fn (t &Transformer) is_sum_variant_field_name(name string) bool {
	return name in t.sum_variant_fields
}

fn (t &Transformer) variant_type_from_sum_field_name(name string) ?string {
	if variant := t.sum_variant_fields[name] {
		return variant
	}
	return none
}

fn (t &Transformer) generated_variant_access_type(id flat.NodeId) ?string {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.selector {
			variant := t.variant_type_from_sum_field_name(node.value) or { return none }
			if node.typ.starts_with('&') {
				return node.typ
			}
			return variant
		}
		.prefix {
			if node.op == .mul && node.children_count > 0 {
				variant := t.generated_variant_access_type(t.a.child(&node, 0)) or { return none }
				return t.trim_pointer_type(variant)
			}
			return none
		}
		.paren {
			if node.children_count > 0 {
				return t.generated_variant_access_type(t.a.child(&node, 0))
			}
			return none
		}
		else {
			return none
		}
	}
}

fn (t &Transformer) original_expr_type(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			typ := t.normalize_type_alias(t.var_type(node.value))
			if typ.len > 0 {
				return typ
			}
			if node.typ.len > 0 {
				return t.normalize_type_alias(node.typ)
			}
			return ''
		}
		.selector {
			if node.typ.len > 0 {
				return t.normalize_type_alias(node.typ)
			}
			return t.resolve_selector_type(node)
		}
		else {
			if node.typ.len > 0 {
				return t.normalize_type_alias(node.typ)
			}
			return t.resolve_expr_type(id)
		}
	}
}

fn (t &Transformer) smartcasts_for(expr_name string) []SmartcastContext {
	if expr_name.len == 0 || t.smartcast_stack.len == 0 {
		return []SmartcastContext{}
	}
	mut result := []SmartcastContext{cap: 1}
	for sc in t.smartcast_stack {
		if sc.expr_name == expr_name {
			result << sc
		}
	}
	return result
}

fn (t &Transformer) has_smartcast(expr_name string) bool {
	if expr_name.len == 0 || t.smartcast_stack.len == 0 {
		return false
	}
	for sc in t.smartcast_stack {
		if sc.expr_name == expr_name {
			return true
		}
	}
	return false
}

fn (t &Transformer) resolve_fn_value_ident(name string) ?string {
	if name.len == 0 || name.contains('.') || t.var_type(name).len > 0 {
		return none
	}
	mut candidates := []string{}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		candidates << '${t.cur_module}.${name}'
	}
	candidates << name
	for candidate in candidates {
		if candidate in t.fn_ret_types {
			return candidate
		}
		if !isnil(t.tc) && (candidate in t.tc.fn_ret_types || candidate in t.tc.fn_param_types) {
			return candidate
		}
	}
	return none
}

// --- helper methods ---

pub fn (mut t Transformer) new_temp(prefix string) string {
	name := '__${prefix}_${t.temp_counter}'
	t.temp_counter++
	return name
}

pub fn (mut t Transformer) make_ident(name string) flat.NodeId {
	return t.a.add_val(.ident, name)
}

pub fn (mut t Transformer) make_decl_assign(name string, rhs flat.NodeId) flat.NodeId {
	lhs := t.make_ident(name)
	start := t.a.children.len
	t.a.children << lhs
	t.a.children << rhs
	return t.a.add_node(flat.Node{
		kind:           .decl_assign
		children_start: start
		children_count: 2
	})
}

pub fn (mut t Transformer) make_expr_stmt(expr flat.NodeId) flat.NodeId {
	start := t.a.children.len
	t.a.children << expr
	return t.a.add_node(flat.Node{
		kind:           .expr_stmt
		children_start: start
		children_count: 1
	})
}

pub fn (mut t Transformer) make_assign(lhs flat.NodeId, rhs flat.NodeId) flat.NodeId {
	return t.make_assign_op(lhs, rhs, .assign)
}

pub fn (mut t Transformer) make_assign_op(lhs flat.NodeId, rhs flat.NodeId, op flat.Op) flat.NodeId {
	start := t.a.children.len
	t.a.children << lhs
	t.a.children << rhs
	return t.a.add_node(flat.Node{
		kind:           .assign
		op:             op
		children_start: start
		children_count: 2
	})
}

pub fn (mut t Transformer) make_block(stmts []flat.NodeId) flat.NodeId {
	start := t.a.children.len
	for id in stmts {
		t.a.children << id
	}
	return t.a.add_node(flat.Node{
		kind:           .block
		children_start: start
		children_count: flat.child_count(stmts.len)
	})
}

pub fn (mut t Transformer) make_infix(op flat.Op, lhs flat.NodeId, rhs flat.NodeId) flat.NodeId {
	start := t.a.children.len
	t.a.children << lhs
	t.a.children << rhs
	return t.a.add_node(flat.Node{
		kind:           .infix
		op:             op
		children_start: start
		children_count: 2
	})
}

pub fn (mut t Transformer) make_prefix(op flat.Op, expr flat.NodeId) flat.NodeId {
	start := t.a.children.len
	t.a.children << expr
	return t.a.add_node(flat.Node{
		kind:           .prefix
		op:             op
		children_start: start
		children_count: 1
	})
}

pub fn (mut t Transformer) make_paren(expr flat.NodeId) flat.NodeId {
	start := t.a.children.len
	t.a.children << expr
	return t.a.add_node(flat.Node{
		kind:           .paren
		children_start: start
		children_count: 1
	})
}

pub fn (mut t Transformer) make_if(cond flat.NodeId, then_block flat.NodeId, else_block flat.NodeId) flat.NodeId {
	start := t.a.children.len
	t.a.children << cond
	t.a.children << then_block
	if int(else_block) >= 0 {
		t.a.children << else_block
		return t.a.add_node(flat.Node{
			kind:           .if_expr
			children_start: start
			children_count: 3
		})
	}
	return t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: start
		children_count: 2
	})
}

pub fn (mut t Transformer) push_smartcast(expr_name string, variant string, sum_type string) {
	t.smartcast_stack << SmartcastContext{
		expr_name:     expr_name
		variant_name:  variant
		sum_type_name: sum_type
	}
}

pub fn (mut t Transformer) pop_smartcast() {
	if t.smartcast_stack.len > 0 {
		t.smartcast_stack.delete_last()
	}
}

pub fn (t &Transformer) find_smartcast(expr_name string) ?SmartcastContext {
	// Search from top of stack (most recent) to bottom
	mut i := t.smartcast_stack.len - 1
	for i >= 0 {
		if t.smartcast_stack[i].expr_name == expr_name {
			return t.smartcast_stack[i]
		}
		i--
	}
	return none
}

fn (t &Transformer) expr_key(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		return node.value
	}
	if node.kind == .selector && node.children_count >= 1 {
		base_id := t.a.child(&node, 0)
		base_key := t.expr_key(base_id)
		if base_key.len > 0 {
			return '${base_key}.${node.value}'
		}
	}
	if node.kind == .index && node.children_count >= 2 {
		base_key := t.expr_key(t.a.child(&node, 0))
		index_key := t.expr_key_part(t.a.child(&node, 1))
		if base_key.len > 0 && index_key.len > 0 {
			return '${base_key}[${index_key}]'
		}
	}
	if node.kind in [.as_expr, .paren] && node.children_count >= 1 {
		return t.expr_key(t.a.child(&node, 0))
	}
	return ''
}

fn (t &Transformer) expr_key_part(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value
		}
		.int_literal, .string_literal, .char_literal, .enum_val {
			return node.value
		}
		else {
			return t.expr_key(id)
		}
	}
}

fn (t &Transformer) qualify_variant(variant string, sum_type_name string) string {
	if variant.contains('.') {
		return variant
	}
	resolved_sum := t.resolve_sum_name(sum_type_name)
	if resolved_variant := t.sum_variant_name(resolved_sum, variant) {
		return resolved_variant
	}
	if sum_type_name.contains('.') {
		mod := sum_type_name.all_before_last('.')
		return '${mod}.${variant}'
	}
	return variant
}

fn (t &Transformer) sum_variant_name(sum_name string, variant string) ?string {
	resolved_sum := t.resolve_sum_name(sum_name)
	variants := t.sum_types[resolved_sum] or { return none }
	for v in variants {
		if t.variant_names_match(v, variant) {
			return v
		}
	}
	return none
}

fn (t &Transformer) variant_names_match(a string, b string) bool {
	return a == b || t.variant_short_name(a) == t.variant_short_name(b)
}

fn (t &Transformer) variant_short_name(name string) string {
	return variant_short_name_text(name)
}

fn variant_short_name_text(name string) string {
	if name.starts_with('&') {
		return '&' + variant_short_name_text(name[1..])
	}
	if name.starts_with('[]') {
		return '[]' + variant_short_name_text(name[2..])
	}
	if name.starts_with('map[') {
		bracket_end := name.index(']') or { return name }
		key := name[4..bracket_end]
		value := name[bracket_end + 1..]
		return 'map[${variant_short_name_text(key)}]${variant_short_name_text(value)}'
	}
	return if name.contains('.') { name.all_after_last('.') } else { name }
}

fn (t &Transformer) sum_field_name(variant string) string {
	if variant.starts_with('&') {
		return t.sum_field_name(variant[1..])
	}
	if variant.starts_with('ptr') && variant.len > 3 && variant[3..].contains('.') {
		return t.sum_field_name(variant[3..])
	}
	if variant.starts_with('ptr') && variant.len > 3 && variant[3..].contains('__') {
		return t.sum_field_name(variant[3..].replace('__', '.'))
	}
	if variant.starts_with('[]') {
		return '_Array_${c_name(variant[2..])}'
	}
	if variant.starts_with('map[') {
		return '_Map_${c_name(variant[4..].replace(']', '_'))}'
	}
	return match variant {
		'int' { '_int' }
		'i8' { '_i8' }
		'i16' { '_i16' }
		'i64' { '_i64' }
		'u8', 'byte' { '_u8' }
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

fn (t &Transformer) variant_references_sum(variant string, sum_name string) bool {
	_ = t
	_ = variant
	_ = sum_name
	return true
}

fn (t &Transformer) tc_variant_refs_sum_inner(variant string, sum_name string, mut visited map[string]bool) bool {
	if variant == sum_name || variant.all_after_last('.') == sum_name.all_after_last('.') {
		return true
	}
	if variant in visited {
		return false
	}
	visited[variant] = true
	mut lookup := variant
	if lookup !in t.tc.structs && !lookup.contains('.') && sum_name.contains('.') {
		qlookup := '${sum_name.all_before_last('.')}.${lookup}'
		if qlookup in t.tc.structs {
			lookup = qlookup
		}
	}
	if lookup !in t.tc.structs && lookup.contains('.') {
		short := lookup.all_after_last('.')
		if short in t.tc.structs {
			lookup = short
		}
	}
	if lookup in t.tc.structs {
		for f in t.tc.structs[lookup] {
			if t.tc_type_references_sum(f.typ, sum_name, mut visited) {
				return true
			}
		}
	}
	return false
}

fn (t &Transformer) tc_type_references_sum(typ types.Type, sum_name string, mut visited map[string]bool) bool {
	clean := types.unwrap_pointer(typ)
	if clean is types.Struct && clean.name == sum_name {
		return true
	}
	if clean is types.SumType && clean.name == sum_name {
		return true
	}
	if clean is types.SumType {
		return true
	}
	if clean is types.Struct {
		if t.tc_variant_refs_sum_inner(clean.name, sum_name, mut visited) {
			return true
		}
	}
	if clean is types.Array {
		return t.tc_type_references_sum(clean.elem_type, sum_name, mut visited)
	}
	return false
}

fn (t &Transformer) variant_refs_sum_inner(variant string, sum_name string, mut visited map[string]bool) bool {
	short_v := if variant.contains('.') { variant.all_after_last('.') } else { variant }
	short_s := if sum_name.contains('.') { sum_name.all_after_last('.') } else { sum_name }
	if short_v == short_s {
		return true
	}
	if variant in visited {
		return false
	}
	visited[variant] = true
	qualified := if sum_name.contains('.') && !variant.contains('.') {
		'${sum_name.all_before_last('.')}.${variant}'
	} else {
		variant
	}
	lookup := if qualified in t.structs {
		qualified
	} else if variant in t.structs {
		variant
	} else {
		short_v
	}
	if lookup in t.structs {
		for f in t.structs[lookup].fields {
			if f.typ.starts_with('&') || f.typ.starts_with('[]') {
				continue
			}
			ftyp := f.typ
			short_f := if ftyp.contains('.') { ftyp.all_after_last('.') } else { ftyp }
			if ftyp == sum_name || short_f == short_s {
				return true
			}
			qftyp := if sum_name.contains('.') && !ftyp.contains('.') {
				'${sum_name.all_before_last('.')}.${ftyp}'
			} else {
				ftyp
			}
			if qftyp in t.sum_types {
				return true
			}
			if ftyp in t.structs || short_f in t.structs || qftyp in t.structs {
				inner_lookup := if ftyp in t.structs {
					ftyp
				} else if short_f in t.structs {
					short_f
				} else {
					qftyp
				}
				if t.variant_refs_sum_inner(inner_lookup, sum_name, mut visited) {
					return true
				}
			}
		}
	}
	return false
}

pub fn (mut t Transformer) drain_pending(mut result []flat.NodeId) {
	for id in t.pending_stmts {
		result << id
	}
	t.pending_stmts.clear()
}

fn (mut t Transformer) with_pending_before(stmt flat.NodeId) []flat.NodeId {
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	result << stmt
	return result
}

fn (t &Transformer) is_stmt_kind_id(kind_id int) bool {
	return kind_id == 39 || kind_id == 40 || kind_id == 41 || kind_id == 42 || kind_id == 43
		|| kind_id == 44 || kind_id == 45 || kind_id == 46 || kind_id == 47 || kind_id == 48
		|| kind_id == 49 || kind_id == 50 || kind_id == 52 || kind_id == 53 || kind_id == 54
		|| kind_id == 55 || kind_id == 15 || kind_id == 56 || kind_id == 57 || kind_id == 60
}

fn (t &Transformer) is_stmt_kind(kind flat.NodeKind) bool {
	return t.is_stmt_kind_id(int(kind))
}

// --- type resolution helpers (will move to types.v later) ---

fn (t &Transformer) infer_decl_type(node &flat.Node) string {
	if node.typ.len > 0 {
		return node.typ
	}
	if node.children_count >= 2 {
		rhs_id := t.a.child(node, 1)
		return t.node_type(rhs_id)
	}
	return ''
}

fn (t &Transformer) resolve_expr_type(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			if sc := t.find_smartcast(node.value) {
				return t.smartcast_target_type(sc)
			}
			local_type := t.normalize_type_alias(t.var_type(node.value))
			if local_type.len > 0 {
				return local_type
			}
			if global_type := t.globals[node.value] {
				return t.normalize_type_alias(global_type)
			}
			if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
				qglobal := '${t.cur_module}.${node.value}'
				if global_type := t.globals[qglobal] {
					return t.normalize_type_alias(global_type)
				}
			}
			if !isnil(t.tc) {
				if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
					qname := '${t.cur_module}.${node.value}'
					if name := t.const_type_name(qname) {
						return name
					}
				}
				if name := t.const_type_name(node.value) {
					return name
				}
			}
			return ''
		}
		.call {
			if node.typ.len > 0 {
				typ := t.normalize_type_alias(node.typ)
				if typ !in ['array', 'map'] {
					return typ
				}
			}
			ret := t.get_call_return_type(id, node)
			if ret.len > 0 {
				return ret
			}
			return ''
		}
		.cast_expr {
			if node.value.len > 0 {
				return node.value
			}
			return node.typ
		}
		.array_literal {
			if node.typ.len > 0 {
				typ := t.normalize_type_alias(node.typ)
				if typ != 'array' {
					return typ
				}
			}
			if node.children_count > 0 {
				elem_type := t.node_type(t.a.child(&node, 0))
				if elem_type.len > 0 {
					return '[]${elem_type}'
				}
			}
			return '[]int'
		}
		.array_init {
			if node.value.starts_with('[]') {
				return '[]${node.value}'
			}
			if node.typ.len > 0 {
				typ := t.normalize_type_alias(node.typ)
				if typ != 'array' {
					return typ
				}
			}
			if is_fixed_array_type(node.value) {
				return node.value
			}
			if node.value.len > 0 {
				return '[]${node.value}'
			}
			return '[]int'
		}
		.map_init {
			if node.value.len > 0 {
				return node.value
			}
			if node.children_count >= 2 {
				key_type := t.node_type(t.a.child(&node, 0))
				value_type := t.node_type(t.a.child(&node, 1))
				if key_type.len > 0 && value_type.len > 0 {
					return 'map[${key_type}]${value_type}'
				}
			}
			return ''
		}
		.selector {
			if !isnil(t.tc) && node.children_count > 0 {
				base := t.a.child_node(&node, 0)
				if base.kind == .ident {
					qname := '${base.value}.${node.value}'
					if name := t.const_type_name(qname) {
						return name
					}
				}
			}
			return t.resolve_selector_type(node)
		}
		.index {
			return t.resolve_index_elem_type(node)
		}
		.paren {
			if node.children_count > 0 {
				return t.node_type(t.a.child(&node, 0))
			}
			return ''
		}
		.prefix {
			if node.children_count > 0 {
				child_type := t.node_type(t.a.child(&node, 0))
				if node.op == .amp && child_type.len > 0 {
					return '&${child_type}'
				}
				if node.op == .mul && child_type.starts_with('&') {
					return child_type[1..]
				}
				if node.op == .not {
					return 'bool'
				}
			}
			return ''
		}
		.block {
			return t.stmt_value_type(id)
		}
		.bool_literal {
			return 'bool'
		}
		.float_literal {
			return 'f64'
		}
		.char_literal {
			return 'u8'
		}
		.string_literal, .string_interp {
			return 'string'
		}
		.int_literal {
			return 'int'
		}
		.nil_literal {
			return 'voidptr'
		}
		.none_expr {
			return '?void'
		}
		.infix {
			if node.children_count >= 2 {
				if node.op in [.eq, .ne, .lt, .gt, .le, .ge, .logical_and, .logical_or] {
					return 'bool'
				}
				ret_type := t.infix_struct_operator_result_type(node)
				if ret_type.len > 0 {
					return ret_type
				}
				lhs_type := t.resolve_expr_type(t.a.child(&node, 0))
				if node.op == .plus && lhs_type == 'string' {
					return 'string'
				}
				rhs_type := t.resolve_expr_type(t.a.child(&node, 1))
				if node.op == .plus && rhs_type == 'string' {
					return 'string'
				}
			}
			return ''
		}
		.or_expr {
			if node.children_count > 0 {
				inner_type := t.resolve_expr_type(t.a.child(&node, 0))
				if inner_type.starts_with('!') {
					return inner_type[1..]
				}
				if inner_type.starts_with('?') {
					return inner_type[1..]
				}
				return inner_type
			}
			return ''
		}
		.if_expr {
			return t.if_expr_result_type(id, node)
		}
		.match_stmt {
			return t.match_expr_type(node)
		}
		else {
			return ''
		}
	}
}

fn (t &Transformer) const_type_name(name string) ?string {
	if isnil(t.tc) || name.len == 0 {
		return none
	}
	key := t.const_type_key(name) or { return none }
	typ := t.tc.const_types[key] or { return none }
	if tname := t.const_entry_type_name(key, typ) {
		return tname
	}
	return none
}

fn (t &Transformer) const_type_key(name string) ?string {
	if name.len == 0 || isnil(t.tc) {
		return none
	}
	if name in t.tc.const_types {
		return name
	}
	if key := t.const_suffixes[name] {
		if key.len > 0 {
			return key
		}
	}
	return none
}

fn (t &Transformer) const_entry_type_name(name string, typ types.Type) ?string {
	tname := t.normalize_type_alias(typ.name())
	if tname.len > 0 && tname != 'unknown' {
		if is_fixed_array_type(tname) {
			if expr_id := t.tc.const_exprs[name] {
				expr := t.a.nodes[int(expr_id)]
				if expr.kind == .call {
					return '[]${fixed_array_elem_type(tname)}'
				}
			}
		}
		return tname
	}
	if name.ends_with('.scanner_matcher') {
		mod_name := name.all_before_last('.')
		return '${mod_name}.KeywordsMatcherTrie'
	}
	if expr_id := t.tc.const_exprs[name] {
		if etyp := t.tc.expr_type(expr_id) {
			ename := t.normalize_type_alias(etyp.name())
			if ename.len > 0 && ename != 'unknown' {
				return ename
			}
		}
		ename := t.resolve_expr_type(expr_id)
		if ename.len > 0 && ename != 'unknown' {
			return ename
		}
	}
	return none
}

fn (t &Transformer) match_expr_type(node flat.Node) string {
	if node.kind != .match_stmt || node.children_count < 2 {
		return ''
	}
	match_expr_id := t.a.child(&node, 0)
	for i in 1 .. node.children_count {
		branch := t.a.child_node(&node, i)
		if branch.kind != .match_branch {
			continue
		}
		body_start := if branch.value == 'else' { 0 } else { t.count_conds(*branch) }
		if branch.children_count <= body_start {
			continue
		}
		contexts := t.match_branch_type_contexts(match_expr_id, *branch)
		for j := branch.children_count - 1; j >= body_start; j-- {
			stmt_id := t.a.child(branch, j)
			typ := if contexts.len > 0 {
				t.stmt_value_type_with_smartcasts(stmt_id, contexts)
			} else {
				t.stmt_value_type(stmt_id)
			}
			if typ.len > 0 {
				return typ
			}
		}
	}
	return ''
}

fn (t &Transformer) match_branch_type_contexts(match_expr_id flat.NodeId, branch flat.Node) []SmartcastContext {
	if branch.value == 'else' {
		return []SmartcastContext{}
	}
	n_conds := t.count_conds(branch)
	if n_conds != 1 {
		return []SmartcastContext{}
	}
	cond_val_id := t.a.child(&branch, 0)
	variant_name := t.match_type_pattern(cond_val_id) or { return []SmartcastContext{} }
	subj := t.expr_key(match_expr_id)
	sum_name := t.sum_type_for_is_expr(t.original_expr_type(match_expr_id), variant_name)
	if subj.len == 0 || sum_name.len == 0 {
		return []SmartcastContext{}
	}
	return [
		SmartcastContext{
			expr_name:     subj
			variant_name:  variant_name
			sum_type_name: sum_name
		},
	]
}

fn (t &Transformer) stmt_value_type(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.expr_stmt {
			if node.children_count > 0 {
				return t.node_type(t.a.child(&node, node.children_count - 1))
			}
			return ''
		}
		.block {
			for i := node.children_count - 1; i >= 0; i-- {
				typ := t.stmt_value_type(t.a.child(&node, i))
				if typ.len > 0 {
					return typ
				}
			}
			return ''
		}
		else {
			return t.node_type(id)
		}
	}
}

// --- match lowering (existing, will move to expr.v later) ---

fn (mut t Transformer) lower_match_stmts() {
	for i, node in t.a.nodes {
		if node.kind == .match_stmt {
			if_id := t.lower_one_match(node)
			t.a.nodes[i] = t.a.nodes[int(if_id)]
		} else if node.kind == .expr_stmt && node.children_count == 1 {
			child_id := t.a.child(&node, 0)
			child := t.a.nodes[int(child_id)]
			if child.kind == .match_stmt {
				if_id := t.lower_one_match(child)
				t.a.nodes[i] = flat.Node{
					kind:           .expr_stmt
					children_start: t.a.children.len
					children_count: 1
				}
				t.a.children << if_id
			}
		}
	}
}

fn (mut t Transformer) lower_one_match(node flat.Node) flat.NodeId {
	match_expr_id := t.a.child(&node, 0)
	match_expr := t.a.nodes[int(match_expr_id)]
	result_type := t.match_expr_type(node)

	needs_temp := match_expr.kind !in [.ident, .int_literal, .bool_literal, .string_literal,
		.char_literal]

	mut actual_expr_id := match_expr_id
	mut prefix_id := flat.empty_node

	if needs_temp {
		tmp_name := '__match_tmp_${int(match_expr_id)}'
		match_type := t.node_type(match_expr_id)
		transformed_match_expr := t.transform_expr(match_expr_id)
		tmp_ident := t.a.add_val(.ident, tmp_name)
		t.a.nodes[int(tmp_ident)].typ = match_type
		decl_start := t.a.children.len
		t.a.children << tmp_ident
		t.a.children << transformed_match_expr
		prefix_id = t.a.add_node(flat.Node{
			kind:           .decl_assign
			children_start: decl_start
			children_count: 2
			typ:            match_type
		})
		actual_expr_id = t.a.add_val(.ident, tmp_name)
		t.a.nodes[int(actual_expr_id)].typ = match_type
	}

	mut branches := []flat.NodeId{}
	for i in 1 .. node.children_count {
		branches << t.a.child(&node, i)
	}
	if_id := t.build_match_chain(actual_expr_id, match_expr_id, branches, 0)

	if needs_temp {
		block_start := t.a.children.len
		t.a.children << prefix_id
		t.a.children << if_id
		block_id := t.a.add_node(flat.Node{
			kind:           .block
			children_start: block_start
			children_count: 2
			typ:            result_type
		})
		return block_id
	}
	if result_type.len > 0 {
		t.a.nodes[int(if_id)].typ = result_type
	}
	return if_id
}

fn (mut t Transformer) build_match_chain(match_expr_id flat.NodeId, orig_expr_id flat.NodeId, branches []flat.NodeId, idx int) flat.NodeId {
	if idx >= branches.len {
		return t.a.add(flat.NodeKind.empty)
	}
	branch := t.a.nodes[int(branches[idx])]
	is_else := branch.value == 'else'

	body_start_idx := if is_else { 0 } else { t.count_conds(branch) }
	if !is_else && t.match_branch_all_type_patterns(branch) && t.count_conds(branch) > 1 {
		return t.build_match_type_branch_chain(match_expr_id, orig_expr_id, branch, branches, idx,
			0)
	}
	// Push a smartcast around the body transform when this branch matches a
	// single sum-type variant, so selectors inside the body get narrowed.
	mut sc_pushed := 0
	if !is_else {
		n_conds := t.count_conds(branch)
		if n_conds == 1 {
			cond_val_id := t.a.child(&branch, 0)
			if variant_name := t.match_type_pattern(cond_val_id) {
				subj := t.expr_key(match_expr_id)
				sum_name := t.sum_type_for_is_expr(t.original_expr_type(match_expr_id),
					variant_name)
				if subj.len > 0 && sum_name.len > 0 {
					t.push_smartcast(subj, variant_name, sum_name)
					sc_pushed++
				}
				orig_subj := t.expr_key(orig_expr_id)
				if orig_subj.len > 0 && orig_subj != subj && sum_name.len > 0 {
					t.push_smartcast(orig_subj, variant_name, sum_name)
					sc_pushed++
				}
			}
		}
	}
	mut body_ids := []flat.NodeId{}
	for i in body_start_idx .. branch.children_count {
		body_ids << t.a.child(&branch, i)
	}
	new_body := t.transform_stmts(body_ids)
	for _ in 0 .. sc_pushed {
		t.pop_smartcast()
	}
	body_block := t.make_block(new_body)

	if is_else {
		return body_block
	}

	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	cond_id := t.build_match_cond(match_expr_id, branch)
	mut cond_prelude := []flat.NodeId{}
	t.drain_pending(mut cond_prelude)
	t.pending_stmts = outer_pending

	mut if_ids := []flat.NodeId{}
	if_ids << cond_id
	if_ids << body_block
	if idx + 1 < branches.len {
		else_part := t.build_match_chain(match_expr_id, orig_expr_id, branches, idx + 1)
		if_ids << else_part
	}

	if_start := t.a.children.len
	for id in if_ids {
		t.a.children << id
	}
	if_id := t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: if_start
		children_count: flat.child_count(if_ids.len)
	})
	if cond_prelude.len > 0 {
		cond_prelude << if_id
		return t.make_block(cond_prelude)
	}
	return if_id
}

fn (mut t Transformer) build_match_value_stmts(node flat.Node, target_name string, target_type string) []flat.NodeId {
	match_expr_id := t.a.child(&node, 0)
	match_expr := t.a.nodes[int(match_expr_id)]
	needs_temp := match_expr.kind !in [.ident, .int_literal, .bool_literal, .string_literal,
		.char_literal]

	mut actual_expr_id := match_expr_id
	mut result := []flat.NodeId{}
	if needs_temp {
		tmp_name := '__match_tmp_${int(match_expr_id)}'
		match_type := t.node_type(match_expr_id)
		transformed_match_expr := t.transform_expr(match_expr_id)
		t.drain_pending(mut result)
		tmp_ident := t.a.add_val(.ident, tmp_name)
		t.a.nodes[int(tmp_ident)].typ = match_type
		result << t.make_decl_assign_typed(tmp_name, transformed_match_expr, match_type)
		actual_expr_id = t.a.add_val(.ident, tmp_name)
		t.a.nodes[int(actual_expr_id)].typ = match_type
	}

	mut branches := []flat.NodeId{}
	for i in 1 .. node.children_count {
		branches << t.a.child(&node, i)
	}
	result << t.build_match_value_chain(actual_expr_id, match_expr_id, branches, 0, target_name,
		target_type)
	return result
}

fn (mut t Transformer) build_match_value_chain(match_expr_id flat.NodeId, orig_expr_id flat.NodeId, branches []flat.NodeId, idx int, target_name string, target_type string) flat.NodeId {
	if idx >= branches.len {
		return t.a.add(flat.NodeKind.empty)
	}
	branch := t.a.nodes[int(branches[idx])]
	is_else := branch.value == 'else'
	body_start_idx := if is_else { 0 } else { t.count_conds(branch) }
	if !is_else && t.match_branch_all_type_patterns(branch) && t.count_conds(branch) > 1 {
		return t.build_match_value_type_branch_chain(match_expr_id, orig_expr_id, branch, branches,
			idx, 0, target_name, target_type)
	}

	mut sc_pushed := 0
	if !is_else {
		n_conds := t.count_conds(branch)
		if n_conds == 1 {
			cond_val_id := t.a.child(&branch, 0)
			if variant_name := t.match_type_pattern(cond_val_id) {
				subj := t.expr_key(match_expr_id)
				sum_name := t.sum_type_for_is_expr(t.original_expr_type(match_expr_id),
					variant_name)
				if subj.len > 0 && sum_name.len > 0 {
					t.push_smartcast(subj, variant_name, sum_name)
					sc_pushed++
				}
				orig_subj := t.expr_key(orig_expr_id)
				if orig_subj.len > 0 && orig_subj != subj && sum_name.len > 0 {
					t.push_smartcast(orig_subj, variant_name, sum_name)
					sc_pushed++
				}
			}
		}
	}

	mut body_ids := []flat.NodeId{}
	for i in body_start_idx .. branch.children_count {
		body_ids << t.a.child(&branch, i)
	}
	raw_body := t.make_block(body_ids)
	body_block := t.if_value_branch_block(raw_body, target_name, target_type)
	for _ in 0 .. sc_pushed {
		t.pop_smartcast()
	}

	if is_else {
		return body_block
	}
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	cond_id := t.build_match_cond(match_expr_id, branch)
	mut cond_prelude := []flat.NodeId{}
	t.drain_pending(mut cond_prelude)
	t.pending_stmts = outer_pending
	mut if_ids := []flat.NodeId{}
	if_ids << cond_id
	if_ids << body_block
	if idx + 1 < branches.len {
		else_part := t.build_match_value_chain(match_expr_id, orig_expr_id, branches, idx + 1,
			target_name, target_type)
		if_ids << else_part
	}
	if_start := t.a.children.len
	for id in if_ids {
		t.a.children << id
	}
	if_id := t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: if_start
		children_count: flat.child_count(if_ids.len)
	})
	if cond_prelude.len > 0 {
		cond_prelude << if_id
		return t.make_block(cond_prelude)
	}
	return if_id
}

fn (mut t Transformer) build_match_value_type_branch_chain(match_expr_id flat.NodeId, orig_expr_id flat.NodeId, branch flat.Node, branches []flat.NodeId, idx int, cond_idx int, target_name string, target_type string) flat.NodeId {
	n_conds := t.count_conds(branch)
	if cond_idx >= n_conds {
		return if idx + 1 < branches.len {
			t.build_match_value_chain(match_expr_id, orig_expr_id, branches, idx + 1, target_name,
				target_type)
		} else {
			t.a.add(flat.NodeKind.empty)
		}
	}
	cond_val_id := t.a.child(&branch, cond_idx)
	variant_name := t.match_type_pattern(cond_val_id) or {
		return t.build_match_value_chain(match_expr_id, orig_expr_id, branches, idx + 1,
			target_name, target_type)
	}
	is_start := t.a.children.len
	t.a.children << match_expr_id
	is_id := t.a.add_node(flat.Node{
		kind:           .is_expr
		value:          variant_name
		children_start: is_start
		children_count: 1
	})
	cond_id := t.transform_is_expr(is_id, t.a.nodes[int(is_id)])

	mut sc_pushed := 0
	subj := t.expr_key(match_expr_id)
	sum_name := t.sum_type_for_is_expr(t.original_expr_type(match_expr_id), variant_name)
	if subj.len > 0 && sum_name.len > 0 {
		t.push_smartcast(subj, variant_name, sum_name)
		sc_pushed++
	}
	orig_subj := t.expr_key(orig_expr_id)
	if orig_subj.len > 0 && orig_subj != subj && sum_name.len > 0 {
		t.push_smartcast(orig_subj, variant_name, sum_name)
		sc_pushed++
	}

	mut body_ids := []flat.NodeId{}
	for i in n_conds .. branch.children_count {
		body_ids << t.a.child(&branch, i)
	}
	raw_body := t.make_block(body_ids)
	body_block := t.if_value_branch_block(raw_body, target_name, target_type)
	for _ in 0 .. sc_pushed {
		t.pop_smartcast()
	}

	else_part := t.build_match_value_type_branch_chain(match_expr_id, orig_expr_id, branch,
		branches, idx, cond_idx + 1, target_name, target_type)
	start := t.a.children.len
	t.a.children << cond_id
	t.a.children << body_block
	t.a.children << else_part
	return t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: start
		children_count: 3
	})
}

fn (mut t Transformer) build_match_type_branch_chain(match_expr_id flat.NodeId, orig_expr_id flat.NodeId, branch flat.Node, branches []flat.NodeId, idx int, cond_idx int) flat.NodeId {
	n_conds := t.count_conds(branch)
	if cond_idx >= n_conds {
		return if idx + 1 < branches.len {
			t.build_match_chain(match_expr_id, orig_expr_id, branches, idx + 1)
		} else {
			t.a.add(flat.NodeKind.empty)
		}
	}
	cond_val_id := t.a.child(&branch, cond_idx)
	variant_name := t.match_type_pattern(cond_val_id) or {
		return t.build_match_chain(match_expr_id, orig_expr_id, branches, idx + 1)
	}
	is_start := t.a.children.len
	t.a.children << match_expr_id
	is_id := t.a.add_node(flat.Node{
		kind:           .is_expr
		value:          variant_name
		children_start: is_start
		children_count: 1
	})
	cond_id := t.transform_is_expr(is_id, t.a.nodes[int(is_id)])

	mut sc_pushed := 0
	subj := t.expr_key(match_expr_id)
	sum_name := t.sum_type_for_is_expr(t.original_expr_type(match_expr_id), variant_name)
	if subj.len > 0 && sum_name.len > 0 {
		t.push_smartcast(subj, variant_name, sum_name)
		sc_pushed++
	}
	orig_subj := t.expr_key(orig_expr_id)
	if orig_subj.len > 0 && orig_subj != subj && sum_name.len > 0 {
		t.push_smartcast(orig_subj, variant_name, sum_name)
		sc_pushed++
	}

	mut body_ids := []flat.NodeId{}
	for i in n_conds .. branch.children_count {
		body_ids << t.a.child(&branch, i)
	}
	body_block := t.make_block(t.transform_stmts(body_ids))
	for _ in 0 .. sc_pushed {
		t.pop_smartcast()
	}

	else_part := t.build_match_type_branch_chain(match_expr_id, orig_expr_id, branch, branches,
		idx, cond_idx + 1)
	start := t.a.children.len
	t.a.children << cond_id
	t.a.children << body_block
	t.a.children << else_part
	return t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: start
		children_count: 3
	})
}

// make_match_eq builds the equality test between a match subject and a branch
// value, lowering string comparisons to string__eq (the transformer owns string
// lowering; the backend no longer special-cases it).
fn (mut t Transformer) make_match_eq(lhs flat.NodeId, rhs flat.NodeId) flat.NodeId {
	if t.is_string_type(lhs) || t.is_string_type(rhs) {
		return t.make_call('string__eq', arr2(lhs, rhs))
	}
	start := t.a.children.len
	t.a.children << lhs
	t.a.children << rhs
	return t.a.add_node(flat.Node{
		kind:           .infix
		op:             .eq
		children_start: start
		children_count: 2
	})
}

fn (mut t Transformer) make_match_range(lhs flat.NodeId, range_id flat.NodeId) flat.NodeId {
	range := t.a.nodes[int(range_id)]
	if range.children_count < 2 {
		return t.make_bool_literal(false)
	}
	low_id := t.a.children[range.children_start]
	high_id := t.a.children[range.children_start + 1]
	low := t.match_cond_value(lhs, low_id)
	high := t.match_cond_value(lhs, high_id)
	ge_cmp := t.make_infix(.ge, lhs, low)
	le_cmp := t.make_infix(.le, lhs, high)
	return t.make_infix(.logical_and, ge_cmp, le_cmp)
}

fn (mut t Transformer) match_cond_value(match_expr_id flat.NodeId, cond_val_id flat.NodeId) flat.NodeId {
	cond_val := t.a.nodes[int(cond_val_id)]
	if cond_val.kind == .enum_val {
		return t.transform_enum_shorthand(cond_val_id, cond_val, t.node_type(match_expr_id))
	}
	return t.transform_expr(cond_val_id)
}

fn (mut t Transformer) build_match_cond(match_expr_id flat.NodeId, branch flat.Node) flat.NodeId {
	n_conds := t.count_conds(branch)
	if n_conds == 1 {
		cond_val_id := t.a.child(&branch, 0)
		cond_val := t.a.nodes[int(cond_val_id)]
		if variant_name := t.match_type_pattern(cond_val_id) {
			is_start := t.a.children.len
			t.a.children << match_expr_id
			is_id := t.a.add_node(flat.Node{
				kind:           .is_expr
				value:          variant_name
				children_start: is_start
				children_count: 1
			})
			return t.transform_is_expr(is_id, t.a.nodes[int(is_id)])
		}
		if cond_val.kind == .range {
			return t.make_match_range(match_expr_id, cond_val_id)
		}
		return t.make_match_eq(match_expr_id, t.match_cond_value(match_expr_id, cond_val_id))
	}
	mut result := flat.empty_node
	for i in 0 .. n_conds {
		cond_val_id := t.a.child(&branch, i)
		cond_val := t.a.nodes[int(cond_val_id)]
		variant_name := t.match_type_pattern(cond_val_id) or { '' }
		cmp := if variant_name.len > 0 {
			is_start := t.a.children.len
			t.a.children << match_expr_id
			is_id := t.a.add_node(flat.Node{
				kind:           .is_expr
				value:          variant_name
				children_start: is_start
				children_count: 1
			})
			t.transform_is_expr(is_id, t.a.nodes[int(is_id)])
		} else if cond_val.kind == .range {
			t.make_match_range(match_expr_id, cond_val_id)
		} else {
			t.make_match_eq(match_expr_id, t.match_cond_value(match_expr_id, cond_val_id))
		}
		if int(result) < 0 {
			result = cmp
		} else {
			or_start := t.a.children.len
			t.a.children << result
			t.a.children << cmp
			result = t.a.add_node(flat.Node{
				kind:           .infix
				op:             .logical_or
				children_start: or_start
				children_count: 2
			})
		}
	}
	return result
}

fn (t &Transformer) match_type_pattern(cond_val_id flat.NodeId) ?string {
	if int(cond_val_id) < 0 {
		return none
	}
	pattern := t.type_pattern_name(cond_val_id)
	if pattern.len > 0 && t.is_sum_variant(pattern) {
		return pattern
	}
	return none
}

fn (t &Transformer) match_branch_all_type_patterns(branch flat.Node) bool {
	n_conds := t.count_conds(branch)
	if n_conds == 0 {
		return false
	}
	for i in 0 .. n_conds {
		cond_val_id := t.a.child(&branch, i)
		if _ := t.match_type_pattern(cond_val_id) {
			continue
		}
		return false
	}
	return true
}

fn (t &Transformer) count_conds(branch flat.Node) int {
	if branch.value.len > 0 && branch.value != 'else' {
		if branch.value[0] >= `0` && branch.value[0] <= `9` {
			return branch.value.int()
		}
	}
	mut count := 0
	for i in 0 .. branch.children_count {
		child := t.a.child_node(&branch, i)
		if child.kind == .int_literal || child.kind == .ident || child.kind == .string_literal
			|| child.kind == .enum_val || child.kind == .bool_literal || child.kind == .char_literal
			|| child.kind == .selector || child.kind == .range || child.kind == .prefix {
			count++
		} else {
			break
		}
	}
	return count
}

pub fn (t &Transformer) is_sum_variant(name string) bool {
	short_name := t.variant_short_name(name)
	for _, variants in t.sum_types {
		for v in variants {
			short_v := t.variant_short_name(v)
			if v == name || short_v == short_name {
				return true
			}
		}
	}
	return false
}

// --- array append lowering (existing, will move to expr.v later) ---

fn (mut t Transformer) lower_array_appends() {
	for i, node in t.a.nodes {
		if node.kind == .module_decl {
			t.cur_module = node.value
			continue
		}
		if node.kind == .fn_decl {
			t.reset_var_types()
			t.annotate_fn_body(node)
			continue
		}
		if node.kind == .decl_assign && node.children_count >= 2 {
			lhs := t.a.child_node(&node, 0)
			if lhs.kind == .ident && lhs.value.len > 0 {
				typ := t.infer_decl_type(node)
				if typ.len > 0 {
					t.set_var_type(lhs.value, typ)
				}
			}
		}
		if node.kind == .expr_stmt && node.children_count == 1 {
			child_id := t.a.child(&node, 0)
			mut child := &t.a.nodes[int(child_id)]
			if child.kind == .infix && child.op == .left_shift {
				t.annotate_left_shift(child_id)
			}
		}
		if node.kind == .assign && node.op == .left_shift_assign && node.children_count >= 2 {
			lhs_id := t.a.child(&node, 0)
			lhs_type := t.lvalue_type(lhs_id)
			clean_lhs_type := t.clean_array_append_lhs_type(lhs_type)
			if clean_lhs_type.starts_with('[]') {
				rhs_id := t.a.child(&node, 1)
				rhs_type := t.lvalue_type(rhs_id)
				elem_type := clean_lhs_type[2..]
				val := if t.array_append_rhs_is_push_many(lhs_id, rhs_id, rhs_type, elem_type) {
					'push_many'
				} else {
					'push'
				}
				t.a.nodes[i] = flat.Node{
					kind:           node.kind
					op:             node.op
					children_start: node.children_start
					children_count: node.children_count
					value:          val
					typ:            elem_type
				}
			}
		}
	}
}

fn (mut t Transformer) annotate_fn_body(fn_node flat.Node) {
	for i in 0 .. fn_node.children_count {
		child_id := t.a.child(&fn_node, i)
		if int(child_id) < 0 {
			continue
		}
		child := t.a.nodes[int(child_id)]
		if child.kind == .param && child.value.len > 0 && child.typ.len > 0 {
			t.set_var_type(child.value, t.normalize_type_alias(child.typ))
		}
		if child.kind == .decl_assign && child.children_count >= 2 {
			lhs := t.a.child_node(&child, 0)
			if lhs.kind == .ident && lhs.value.len > 0 {
				typ := t.infer_decl_type(child)
				if typ.len > 0 {
					t.set_var_type(lhs.value, typ)
				}
			}
		}
		if child.kind == .expr_stmt && child.children_count == 1 {
			inner_id := t.a.child(&child, 0)
			inner := t.a.nodes[int(inner_id)]
			if inner.kind == .infix && inner.op == .left_shift {
				t.annotate_left_shift(inner_id)
			}
		}
		t.annotate_block_stmts(child_id)
	}
}

fn (mut t Transformer) annotate_block_stmts(node_id flat.NodeId) {
	if int(node_id) < 0 {
		return
	}
	node := t.a.nodes[int(node_id)]
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if int(child_id) < 0 {
			continue
		}
		child := t.a.nodes[int(child_id)]
		if child.kind == .decl_assign && child.children_count >= 2 {
			lhs := t.a.child_node(&child, 0)
			if lhs.kind == .ident && lhs.value.len > 0 {
				typ := t.infer_decl_type(child)
				if typ.len > 0 {
					t.set_var_type(lhs.value, typ)
				}
			}
		}
		if child.kind == .expr_stmt && child.children_count == 1 {
			inner_id := t.a.child(&child, 0)
			inner := t.a.nodes[int(inner_id)]
			if inner.kind == .infix && inner.op == .left_shift {
				t.annotate_left_shift(inner_id)
			}
		}
		t.annotate_block_stmts(child_id)
	}
}

fn (mut t Transformer) annotate_left_shift(node_id flat.NodeId) {
	node := t.a.nodes[int(node_id)]
	if node.children_count < 2 {
		return
	}
	lhs_id := t.a.child(&node, 0)
	mut lhs_type := t.lvalue_type(lhs_id)
	if lhs_type == 'strings.Builder' || lhs_type == '&strings.Builder' || lhs_type == 'Builder'
		|| lhs_type == '&Builder' {
		lhs_type = '[]u8'
	}
	clean_lhs_type := t.clean_array_append_lhs_type(lhs_type)
	if !clean_lhs_type.starts_with('[]') {
		return
	}
	rhs_id := t.a.child(&node, 1)
	rhs_type := t.lvalue_type(rhs_id)
	elem_type := clean_lhs_type[2..]
	if t.array_append_rhs_is_push_many(lhs_id, rhs_id, rhs_type, elem_type) {
		t.a.nodes[int(node_id)] = flat.Node{
			kind:           .infix
			op:             .left_shift
			children_start: node.children_start
			children_count: node.children_count
			value:          'push_many'
			typ:            elem_type
		}
	} else {
		t.a.nodes[int(node_id)] = flat.Node{
			kind:           .infix
			op:             .left_shift
			children_start: node.children_start
			children_count: node.children_count
			value:          'push'
			typ:            elem_type
		}
	}
}

fn (mut t Transformer) annotate_left_shift_assign(node_id flat.NodeId) {
	node := t.a.nodes[int(node_id)]
	if node.kind != .assign || node.op != .left_shift_assign || node.children_count < 2 {
		return
	}
	lhs_id := t.a.child(&node, 0)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident {
		return
	}
	lhs_type := t.lvalue_type(lhs_id)
	clean_lhs_type := t.clean_array_append_lhs_type(lhs_type)
	if !clean_lhs_type.starts_with('[]') {
		return
	}
	rhs_id := t.a.child(&node, 1)
	rhs_type := t.lvalue_type(rhs_id)
	elem_type := clean_lhs_type[2..]
	val := if t.array_append_rhs_is_push_many(lhs_id, rhs_id, rhs_type, elem_type) {
		'push_many'
	} else {
		'push'
	}
	t.a.nodes[int(node_id)] = flat.Node{
		kind:           node.kind
		op:             node.op
		children_start: node.children_start
		children_count: node.children_count
		value:          val
		typ:            elem_type
	}
}

// --- public query helpers ---

pub fn (t &Transformer) get_struct_info(name string) ?StructInfo {
	if info := t.structs[name] {
		return info
	}
	return none
}

pub fn (t &Transformer) get_global_type(name string) ?string {
	if typ := t.globals[name] {
		return typ
	}
	return none
}
