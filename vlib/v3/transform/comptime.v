module transform

import os
import v3.flat

// vmod_root supports vmod root handling for Transformer.
fn (t &Transformer) vmod_root() string {
	mut dir := if t.cur_file.len > 0 { os.dir(t.cur_file) } else { os.getwd() }
	if dir.len == 0 {
		dir = os.getwd()
	}
	for {
		if os.exists(os.join_path(dir, 'v.mod')) {
			return dir
		}
		parent := os.dir(dir)
		if parent == dir || parent.len == 0 {
			return if t.cur_file.len > 0 { os.dir(t.cur_file) } else { os.getwd() }
		}
		dir = parent
	}
	return dir
}

// Compile-time reflection: `$for field in T.fields { ... }`.
//
// The parser emits a `comptime_for` node (value `<var>|<kind>`, typ = base type) and defers the
// loop body verbatim. Once the base type is concrete (after generic monomorphization) the loop is
// unrolled here: the body is cloned once per field with the loop variable's compile-time members
// substituted, then transformed normally. Members mirror V's `FieldData`.

// FieldMeta is the compile-time metadata exposed by the `$for field in T.fields` loop variable.
struct FieldMeta {
	name          string
	typ           string
	unaliased_typ string
	typ_id        int
	unaliased_id  int
	is_option     bool
	is_embed      bool
	is_array      bool
	is_map        bool
	is_chan       bool
	is_struct     bool
	is_enum       bool
	is_alias      bool
	is_shared     bool
	is_atomic     bool
	is_mut        bool
	is_pub        bool
	attrs         []string
	indirections  int
}

struct EnumValueMeta {
	name  string
	value int
	attrs []string
}

fn comptime_for_parts(value string) (string, string) {
	if idx := value.index('|') {
		return value[..idx], value[idx + 1..]
	}
	return value, 'fields'
}

fn comptime_for_declares_var(node flat.Node, var_name string) bool {
	if node.kind != .comptime_for {
		return false
	}
	loop_var, _ := comptime_for_parts(node.value)
	return loop_var == var_name
}

// comptime_for_base_type resolves the loop source type to a concrete name. Generic `T` was already
// substituted to the concrete type in `node.typ` during monomorphization.
fn (t &Transformer) comptime_for_base_type(raw string) string {
	return t.comptime_normalize_type_alias_chain(raw.trim_space())
}

fn (t &Transformer) comptime_normalize_type_alias_chain(raw string) string {
	mut typ := raw.trim_space()
	mut seen := map[string]bool{}
	for typ.len > 0 && typ !in seen {
		seen[typ] = true
		next := t.normalize_type_alias(typ).trim_space()
		if next == typ {
			break
		}
		typ = next
	}
	return typ
}

// expand_comptime_for unrolls a `$for` loop into concrete per-field statements. Kinds other than
// `fields` (methods/variants/values/attributes) are not yet supported and expand to nothing,
// preserving the pre-existing skip behavior.
fn (mut t Transformer) expand_comptime_for(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return []flat.NodeId{}
	}
	var_name, kind := comptime_for_parts(node.value)
	if kind !in ['fields', 'values'] {
		return []flat.NodeId{}
	}
	base_type := t.comptime_for_base_type(node.typ)
	body_id := t.a.child(&node, 0)
	body := t.a.nodes[int(body_id)]
	body_stmts := t.a.children_of(&body).clone()
	// A body that calls a generic function would introduce new instantiations for the concrete
	// field types (e.g. recursive `encode(field_value)`); those must be discovered by the
	// monomorphizer before calls are resolved, which the current pipeline does not do for
	// unroll-introduced calls. Fall back to the pre-existing skip behavior for such loops.
	if t.comptime_body_calls_generic_fn(body_stmts) {
		return []flat.NodeId{}
	}
	if kind == 'values' {
		return t.expand_comptime_for_values(var_name, base_type, body_stmts)
	}
	// Comptime `match field.typ { int {} ... }` (type match) and `typeof(field.$(field.name))`
	// reflection are not modelled yet; skip such loops rather than mis-lower them.
	if t.comptime_body_has_unsupported(body_stmts, var_name) {
		return []flat.NodeId{}
	}
	mut out := []flat.NodeId{}
	for fm in t.comptime_field_metas(base_type) {
		mut cloned := []flat.NodeId{cap: body_stmts.len}
		for sid in body_stmts {
			if cid := t.clone_field_subst(sid, var_name, fm) {
				cloned << cid
			}
		}
		// One block per iteration so per-field temps get their own scope.
		block := t.make_block(cloned)
		for s in t.transform_stmt(block) {
			out << s
		}
	}
	return out
}

// expand_comptime_for_values unrolls `$for value in Enum.values { ... }`. The loop variable
// exposes `value.name` / `value.value` and, used bare, materializes as `EnumData`.
fn (mut t Transformer) expand_comptime_for_values(var_name string, base_type string, body_stmts []flat.NodeId) []flat.NodeId {
	values := t.comptime_enum_members(base_type)
	mut out := []flat.NodeId{}
	for item in values {
		mut cloned := []flat.NodeId{cap: body_stmts.len}
		for sid in body_stmts {
			if cid := t.clone_value_subst(sid, var_name, item) {
				cloned << cid
			}
		}
		block := t.make_block(cloned)
		for s in t.transform_stmt(block) {
			out << s
		}
	}
	return out
}

// comptime_enum_members returns the member metadata for an enum (resolving an enum alias to its
// underlying enum).
fn (t &Transformer) comptime_enum_members(base_type string) []EnumValueMeta {
	mut names := []string{}
	mut resolved := base_type
	if got := t.enum_types[base_type] {
		names = got.clone()
	} else {
		qname := t.qualified_alias_name(base_type)
		if got := t.enum_types[qname] {
			names = got.clone()
			resolved = qname
		}
	}
	if names.len == 0 {
		return []EnumValueMeta{}
	}
	metas := t.enum_decl_value_metas(resolved)
	if metas.len > 0 {
		return metas
	}
	mut fallback := []EnumValueMeta{cap: names.len}
	for idx, name in names {
		fallback << EnumValueMeta{
			name:  name
			value: idx
		}
	}
	return fallback
}

// enum_decl_value_metas locates the enum declaration matching `enum_name` (either a bare `Enum`
// or a `module.Enum` qualified name) and evaluates each member's metadata. Explicit values are
// treated like the C backend: normal enums use the integer directly; `[flag]` enums use it as the
// bit index and materialize `1 << index`.
fn (t &Transformer) enum_decl_value_metas(enum_name string) []EnumValueMeta {
	mut cur_mod := ''
	for idx in 0 .. t.a.nodes.len {
		kind := t.a.nodes[idx].kind
		if kind == .module_decl {
			cur_mod = t.a.nodes[idx].value
			continue
		}
		if kind != .enum_decl {
			continue
		}
		node := t.a.nodes[idx]
		qualified := if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
			'${cur_mod}.${node.value}'
		} else {
			node.value
		}
		if enum_name != node.value && enum_name != qualified {
			continue
		}
		is_flag := node.typ == 'flag'
		mut values := []EnumValueMeta{}
		mut next_val := 0
		for i in 0 .. node.children_count {
			f := t.a.child_node(&node, i)
			if f.kind != .enum_field {
				continue
			}
			mut val := next_val
			if f.children_count > 0 {
				if ev := t.enum_field_int_value_in_module(t.a.child(f, 0), cur_mod) {
					val = ev
				}
			}
			values << EnumValueMeta{
				name:  f.value
				value: if is_flag { 1 << val } else { val }
				attrs: f.generic_params.clone()
			}
			next_val = val + 1
		}
		return values
	}
	return []EnumValueMeta{}
}

// enum_field_int_value evaluates an enum member's value expression using the transformer's
// current module for any const reference.
fn (t &Transformer) enum_field_int_value(id flat.NodeId) ?int {
	return t.enum_field_int_value_in_module(id, t.cur_module)
}

// enum_field_int_value_in_module evaluates an enum member's value expression (`x = 1`,
// `x = 1 << 2`, `x = SomeConst`) to an int, following the same forms as the C backend.
// `enum_module` is the enum's declaring module, so a const referenced by a member (`a = base`)
// resolves in that module rather than the caller's.
fn (t &Transformer) enum_field_int_value_in_module(id flat.NodeId, enum_module string) ?int {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.int_literal {
			return node.value.int()
		}
		.paren {
			if node.children_count == 0 {
				return none
			}
			return t.enum_field_int_value_in_module(t.a.child(&node, 0), enum_module)
		}
		.prefix {
			if node.children_count == 0 {
				return none
			}
			v := t.enum_field_int_value_in_module(t.a.child(&node, 0), enum_module)?
			return match node.op {
				.plus { v }
				.minus { -v }
				.bit_not { ~v }
				else { none }
			}
		}
		.infix {
			if node.children_count < 2 {
				return none
			}
			l := t.enum_field_int_value_in_module(t.a.child(&node, 0), enum_module)?
			r := t.enum_field_int_value_in_module(t.a.child(&node, 1), enum_module)?
			return match node.op {
				.plus {
					l + r
				}
				.minus {
					l - r
				}
				.mul {
					l * r
				}
				.div {
					if r == 0 {
						none
					} else {
						l / r
					}
				}
				.mod {
					if r == 0 {
						none
					} else {
						l % r
					}
				}
				.left_shift {
					l << r
				}
				.right_shift {
					l >> r
				}
				.amp {
					l & r
				}
				.pipe {
					l | r
				}
				.xor {
					l ^ r
				}
				else {
					none
				}
			}
		}
		.ident {
			if !isnil(t.tc) {
				lookup_module := if enum_module.len > 0 { enum_module } else { t.cur_module }
				return t.tc.const_int_value_in_module(node.value, lookup_module, []string{})
			}
			return none
		}
		else {
			return none
		}
	}
}

// clone_value_subst clones a `$for value in Enum.values` body, substituting `value.name`,
// `value.value`, `value.attrs`, and a bare `value` (an `EnumData` literal).
fn (mut t Transformer) clone_value_subst(id flat.NodeId, var_name string, item EnumValueMeta) ?flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	if comptime_for_declares_var(node, var_name) {
		return t.clone_node_preserving_children(node)
	}
	if node.kind == .ident && node.value == var_name {
		return t.make_enum_data_literal(item)
	}
	if node.kind == .selector && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind == .ident && base.value == var_name {
			match node.value {
				'name' {
					return t.make_string_literal(item.name)
				}
				'value' {
					return t.make_int_literal(item.value)
				}
				'attrs' {
					return t.make_string_array_literal(item.attrs)
				}
				else {}
			}
		}
	}
	// `$if`/`$else $if` referencing the loop variable (`value.name`, `value.value`): evaluate now
	// and keep the taken branch, mirroring the field-loop path so the guard is not left as an
	// unsupported `comptime_if` for the C backend.
	if node.kind == .comptime_if {
		cond := t.subst_value_cond(node.value, var_name, item.name, item.value)
		if !comptime_cond_has_loop_member_ref(cond, var_name) {
			if taken := t.eval_field_cond(cond) {
				branch_idx := if taken { 0 } else { 1 }
				if branch_idx >= int(node.children_count) {
					return none
				}
				return t.clone_value_subst(t.a.child(&node, branch_idx), var_name, item)
			}
		}
	}
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		if c := t.clone_value_subst(t.a.child(&node, i), var_name, item) {
			children << c
		}
	}
	start := t.a.children.len
	for c in children {
		t.a.children << c
	}
	return t.a.add_node(flat.Node{
		kind:           node.kind
		kind_id:        node.kind_id
		op:             node.op
		pos:            node.pos
		value:          node.value
		typ:            node.typ
		is_mut:         node.is_mut
		children_start: start
		children_count: flat.child_count(children.len)
	})
}

// make_enum_data_literal builds `EnumData{name: '<name>', value: <value>, attrs: [...]}`.
fn (mut t Transformer) make_enum_data_literal(item EnumValueMeta) flat.NodeId {
	name_field := t.make_named_field_init('name', t.make_string_literal(item.name), 'string')
	value_field := t.make_named_field_init('value', t.make_int_literal(item.value), 'i64')
	attrs_field := t.make_named_field_init('attrs', t.make_string_array_literal(item.attrs),
		'[]string')
	start := t.a.children.len
	t.a.children << name_field
	t.a.children << value_field
	t.a.children << attrs_field
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		value:          'EnumData'
		typ:            'EnumData'
		children_start: start
		children_count: 3
	})
}

// make_field_data_literal builds a runtime `FieldData` value for a bare `$for field` variable.
fn (mut t Transformer) make_field_data_literal(fm FieldMeta) flat.NodeId {
	fields := [
		t.make_named_field_init('name', t.make_string_literal(fm.name), 'string'),
		t.make_named_field_init('typ', t.make_int_literal(fm.typ_id), 'int'),
		t.make_named_field_init('unaliased_typ', t.make_int_literal(fm.unaliased_id), 'int'),
		t.make_named_field_init('attrs', t.make_string_array_literal(fm.attrs), '[]string'),
		t.make_named_field_init('is_pub', t.make_bool_literal(fm.is_pub), 'bool'),
		t.make_named_field_init('is_mut', t.make_bool_literal(fm.is_mut), 'bool'),
		t.make_named_field_init('is_embed', t.make_bool_literal(fm.is_embed), 'bool'),
		t.make_named_field_init('is_shared', t.make_bool_literal(fm.is_shared), 'bool'),
		t.make_named_field_init('is_atomic', t.make_bool_literal(fm.is_atomic), 'bool'),
		t.make_named_field_init('is_option', t.make_bool_literal(fm.is_option), 'bool'),
		t.make_named_field_init('is_array', t.make_bool_literal(fm.is_array), 'bool'),
		t.make_named_field_init('is_map', t.make_bool_literal(fm.is_map), 'bool'),
		t.make_named_field_init('is_chan', t.make_bool_literal(fm.is_chan), 'bool'),
		t.make_named_field_init('is_enum', t.make_bool_literal(fm.is_enum), 'bool'),
		t.make_named_field_init('is_struct', t.make_bool_literal(fm.is_struct), 'bool'),
		t.make_named_field_init('is_alias', t.make_bool_literal(fm.is_alias), 'bool'),
		t.make_named_field_init('indirections', t.make_int_literal(fm.indirections), 'u8'),
	]
	start := t.a.children.len
	for field in fields {
		t.a.children << field
	}
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		value:          'FieldData'
		typ:            'FieldData'
		children_start: start
		children_count: flat.child_count(fields.len)
	})
}

fn (mut t Transformer) make_named_field_init(field string, value flat.NodeId, typ string) flat.NodeId {
	start := t.a.children.len
	t.a.children << value
	return t.a.add_node(flat.Node{
		kind:           .field_init
		value:          field
		typ:            typ
		children_start: start
		children_count: 1
	})
}

fn (mut t Transformer) clone_node_preserving_children(node flat.Node) flat.NodeId {
	start := t.a.children.len
	for i in 0 .. node.children_count {
		t.a.children << t.a.child(&node, i)
	}
	return t.a.add_node(flat.Node{
		kind:           node.kind
		kind_id:        node.kind_id
		op:             node.op
		pos:            node.pos
		value:          node.value
		typ:            node.typ
		is_mut:         node.is_mut
		children_start: start
		children_count: node.children_count
	})
}

// comptime_body_calls_generic_fn reports whether any statement subtree calls a resolved generic
// function. A plain short-name match is not enough: a non-generic method can share a name with an
// unrelated generic helper.
fn (mut t Transformer) comptime_body_calls_generic_fn(stmts []flat.NodeId) bool {
	decls := t.cached_generic_fn_decls()
	if decls.len == 0 {
		return false
	}
	t.ensure_node_module_map()
	for sid in stmts {
		if t.subtree_calls_generic(sid, decls) {
			return true
		}
	}
	return false
}

// comptime_body_has_unsupported reports whether any statement uses a comptime construct the
// unroll cannot yet model: a `match <var>.typ` type match, or `typeof(...)` on a member of the
// loop variable.
fn (t &Transformer) comptime_body_has_unsupported(stmts []flat.NodeId, var_name string) bool {
	for sid in stmts {
		if t.subtree_has_unsupported_comptime(sid, var_name) {
			return true
		}
	}
	return false
}

fn (t &Transformer) subtree_has_unsupported_comptime(id flat.NodeId, var_name string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	// ORM (`sql db { select ... }`) mixes comptime field names with a query DSL.
	if node.kind in [.sql_expr, .select_stmt] {
		return true
	}
	if node.kind == .match_stmt && node.children_count > 0 {
		subject := t.a.child_node(&node, 0)
		if subject.kind == .selector && subject.children_count > 0
			&& subject.value in ['typ', 'unaliased_typ'] {
			sbase := t.a.child_node(subject, 0)
			if sbase.kind == .ident && sbase.value == var_name {
				return true
			}
		}
	}
	if node.kind == .typeof_expr && t.subtree_references_var(id, var_name) {
		return true
	}
	// A `field.member` / `field.$(...)` selector is a supported comptime access: check its other
	// children but not the base loop-var ident (which is not a bare use).
	if node.kind == .selector && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind == .ident && base.value == var_name {
			for i in 1 .. node.children_count {
				if t.subtree_has_unsupported_comptime(t.a.child(&node, i), var_name) {
					return true
				}
			}
			return false
		}
	}
	for i in 0 .. node.children_count {
		if t.subtree_has_unsupported_comptime(t.a.child(&node, i), var_name) {
			return true
		}
	}
	return false
}

fn (t &Transformer) subtree_references_var(id flat.NodeId, var_name string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident && node.value == var_name {
		return true
	}
	for i in 0 .. node.children_count {
		if t.subtree_references_var(t.a.child(&node, i), var_name) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) subtree_calls_generic(id flat.NodeId, decls map[string]GenericFnDecl) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .call {
		module_name := t.node_module_or(int(id), t.cur_module)
		if _ := t.generic_call_decl_key(id, node, module_name, decls) {
			return true
		}
	}
	for i in 0 .. node.children_count {
		if t.subtree_calls_generic(t.a.child(&node, i), decls) {
			return true
		}
	}
	return false
}

// FieldDeclMeta holds the per-field mutability/visibility/attributes recorded by the parser on a
// `field_decl` node, keyed by field name (see struct_field_decl_metas).
struct FieldDeclMeta {
	is_mut bool
	is_pub bool
	attrs  []string
}

// comptime_field_metas derives FieldData for every field of the concrete struct type.
fn (t &Transformer) comptime_field_metas(base_type string) []FieldMeta {
	// A monomorphized generic struct instance (`Box[int]`) is stored in the struct table under
	// its generic declaration name (`Box`), so a direct lookup misses; resolve it through the
	// generic-struct field substitution path before giving up.
	info := t.lookup_struct_info(base_type) or {
		t.generic_struct_info_for_stringify(base_type) or { return []FieldMeta{} }
	}
	decl_metas := t.struct_field_decl_metas(base_type)
	mut metas := []FieldMeta{cap: info.fields.len}
	for f in info.fields {
		// V's `field.typ` is the type as written (`MyInt`, `?[]int`); `raw_typ` preserves that,
		// while `f.typ` was already alias-resolved in the struct's declaring module during
		// collect_types. Classify using `f.typ` (and `info.module` for the alias check) so a
		// field type declared in another module is not mis-resolved against the caller's module.
		ftyp := if f.raw_typ.len > 0 { f.raw_typ } else { f.typ }
		// Fields whose declaration node we could not introspect default to the previous
		// public/immutable behaviour; found fields carry their real modifiers/attrs.
		extra := decl_metas[f.name] or {
			FieldDeclMeta{
				is_pub: true
			}
		}
		metas << t.field_meta_for(f.name, ftyp, f.typ, info.module, f.is_embedded, extra)
	}
	return metas
}

// struct_field_decl_metas scans the declaration of `base_type` (resolving a generic instance such
// as `Box[int]` to its base `Box`, module-aware like enum lookup) and returns each field's real
// `is_mut`/`is_pub`/attrs, which the parser recorded on the `field_decl` node.
fn (t &Transformer) struct_field_decl_metas(base_type string) map[string]FieldDeclMeta {
	mut out := map[string]FieldDeclMeta{}
	mut decl_name := base_type.trim_space()
	if idx := decl_name.index('[') {
		decl_name = decl_name[..idx]
	}
	mut cur_mod := ''
	for idx in 0 .. t.a.nodes.len {
		kind := t.a.nodes[idx].kind
		if kind == .module_decl {
			cur_mod = t.a.nodes[idx].value
			continue
		}
		if kind != .struct_decl {
			continue
		}
		node := t.a.nodes[idx]
		qualified := if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
			'${cur_mod}.${node.value}'
		} else {
			node.value
		}
		if decl_name != node.value && decl_name != qualified {
			continue
		}
		for i in 0 .. node.children_count {
			f := t.a.child_node(&node, i)
			if f.kind != .field_decl {
				continue
			}
			// The parser packs field metadata into generic_params: element 0 is a flag string
			// (`m` = mut, `p` = pub), the rest are attributes. An empty list is the default
			// (private, immutable, no attrs).
			mut is_mut := false
			mut is_pub := false
			mut attrs := []string{}
			if f.generic_params.len > 0 {
				flags := f.generic_params[0]
				is_mut = flags.contains('m')
				is_pub = flags.contains('p')
				attrs = f.generic_params[1..].clone()
			}
			out[f.value] = FieldDeclMeta{
				is_mut: is_mut
				is_pub: is_pub
				attrs:  attrs
			}
		}
		return out
	}
	return out
}

fn (t &Transformer) field_meta_for(name string, ftyp string, resolved_typ string, decl_module string, is_embed bool, extra FieldDeclMeta) FieldMeta {
	is_option := ftyp.starts_with('?')
	mut core := if is_option { ftyp[1..].trim_space() } else { ftyp }
	mut indir := 0
	mut is_shared := false
	mut is_atomic := false
	// A `shared T` field is stored behind a lock guard; V reports one level of indirection.
	if core.starts_with('shared ') {
		is_shared = true
		indir++
		core = core[7..].trim_space()
	} else if core.starts_with('atomic ') {
		is_atomic = true
		core = core[7..].trim_space()
	}
	for core.starts_with('&') {
		indir++
		core = core[1..]
	}
	// `resolved_typ` is already alias-resolved in the declaring module and still carries wrappers
	// like `?`, `shared`, and `&`; preserve them for `FieldData.unaliased_typ`.
	unaliased := if resolved_typ.len > 0 {
		resolved_typ.trim_space()
	} else {
		t.comptime_normalize_type_alias_chain(ftyp)
	}
	unaliased_core := comptime_strip_field_wrappers(unaliased)
	is_alias := t.field_type_is_alias(core, decl_module)
	return FieldMeta{
		name:          name
		typ:           ftyp
		unaliased_typ: unaliased
		typ_id:        t.comptime_field_type_id(ftyp, decl_module)
		unaliased_id:  t.comptime_field_type_id(unaliased, decl_module)
		is_option:     is_option
		is_embed:      is_embed
		is_array:      unaliased_core.starts_with('[]') || t.is_fixed_array_type(unaliased_core)
		is_map:        unaliased_core.starts_with('map[')
		is_chan:       unaliased_core.starts_with('chan ')
		is_struct:     unaliased_core in t.structs && !comptime_is_primitive_type(unaliased_core)
		is_enum:       unaliased_core in t.enum_types
		is_alias:      is_alias
		is_shared:     is_shared
		is_atomic:     is_atomic
		is_mut:        extra.is_mut
		is_pub:        extra.is_pub
		attrs:         extra.attrs
		indirections:  indir
	}
}

fn (t &Transformer) comptime_field_type_id(typ string, decl_module string) int {
	key := t.comptime_field_type_id_key(typ, decl_module)
	if key.len == 0 {
		return 0
	}
	return comptime_type_id_hash(key)
}

fn (t &Transformer) comptime_field_type_id_key(typ string, decl_module string) string {
	mut core := typ.trim_space()
	if core.len == 0 {
		return ''
	}
	if core.starts_with('?') {
		return '?' + t.comptime_field_type_id_key(core[1..], decl_module)
	}
	if core.starts_with('shared ') {
		return 'shared ' + t.comptime_field_type_id_key(core[7..], decl_module)
	}
	if core.starts_with('atomic ') {
		return 'atomic ' + t.comptime_field_type_id_key(core[7..], decl_module)
	}
	mut refs := ''
	for core.starts_with('&') {
		refs += '&'
		core = core[1..].trim_space()
	}
	if refs.len > 0 {
		return refs + t.comptime_field_type_id_key(core, decl_module)
	}
	if core.starts_with('[]') {
		return '[]' + t.comptime_field_type_id_key(core[2..], decl_module)
	}
	if comptime_is_primitive_type(core) || core.contains('.') || core.contains('[')
		|| core.contains(' ') || decl_module.len == 0 || decl_module in ['main', 'builtin'] {
		return core
	}
	return '${decl_module}.${core}'
}

// V3 does not have a runtime TypeInfo table yet; keep FieldData TypeIDs stable and nonzero.
fn comptime_type_id_hash(key string) int {
	mut h := u64(1469598103934665603)
	for i in 0 .. key.len {
		h = ((h ^ u64(key[i])) * 1099511628211) % 2147483647
	}
	return int(h) + 1
}

fn comptime_is_primitive_type(typ string) bool {
	return typ in ['string', 'bool', 'rune', 'char', 'i8', 'i16', 'i32', 'i64', 'int', 'isize',
		'u8', 'byte', 'u16', 'u32', 'u64', 'usize', 'f32', 'f64', 'int literal', 'float literal',
		'voidptr', 'byteptr', 'charptr', 'nil', 'void']
}

// comptime_strip_field_wrappers removes the `?` option, `shared`/`atomic`, and `&` reference
// decorations from an (already alias-resolved) field type, yielding the core type used for
// metadata classification.
fn comptime_strip_field_wrappers(typ string) string {
	mut core := typ.trim_space()
	if core.starts_with('?') {
		core = core[1..].trim_space()
	}
	if core.starts_with('shared ') {
		core = core[7..].trim_space()
	} else if core.starts_with('atomic ') {
		core = core[7..].trim_space()
	}
	for core.starts_with('&') {
		core = core[1..]
	}
	return core
}

// field_type_is_alias reports whether `core` names a `type X = Y` alias. An unqualified name is
// resolved in the struct's declaring module (`decl_module`) rather than the caller's cur_module,
// so cross-module reflection reports `is_alias` correctly.
fn (t &Transformer) field_type_is_alias(core string, decl_module string) bool {
	if isnil(t.tc) {
		return false
	}
	if core in t.tc.type_aliases {
		return true
	}
	if !core.contains('.') && decl_module.len > 0 && decl_module != 'main'
		&& decl_module != 'builtin' {
		return '${decl_module}.${core}' in t.tc.type_aliases
	}
	return false
}

fn (t &Transformer) qualified_alias_name(name string) string {
	if name.contains('.') || t.cur_module.len == 0 || t.cur_module in ['main', 'builtin'] {
		return name
	}
	return '${t.cur_module}.${name}'
}

// clone_field_subst deep-clones a body node, substituting `<var>.member` references and folding
// `$if` conditions that reference the loop variable. Returns none when a folded branch is empty.
fn (mut t Transformer) clone_field_subst(id flat.NodeId, var_name string, fm FieldMeta) ?flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	if comptime_for_declares_var(node, var_name) {
		return t.clone_node_preserving_children(node)
	}
	if node.kind == .ident && node.value == var_name {
		return t.make_field_data_literal(fm)
	}
	// `<var>.member` compile-time member access.
	if node.kind == .selector && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind == .ident && base.value == var_name {
			if v := t.field_member_value(node.value, fm) {
				return v
			}
			// Unknown FieldData member (e.g. a typo): leave the selector unresolved so it
			// surfaces as an error instead of silently becoming the field name.
			return t.clone_field_subst_children(node, var_name, fm)
		}
		// `receiver.$(<var>.name)` compile-time field selector - only fold when the name
		// expression is *this* loop variable's `.name`. A nested loop's `$(inner.name)` is left
		// untouched so its own unroll pass resolves it against the right field.
		if node.value == '$' && node.children_count >= 2
			&& t.dollar_selector_names_var(t.a.child(&node, 1), var_name) {
			receiver := t.clone_field_subst(t.a.child(&node, 0), var_name, fm) or { return none }
			return t.make_selector(receiver, fm.name, fm.typ)
		}
	}
	// `$if`/`$else $if` referencing the loop variable: evaluate now, keep the taken branch.
	if node.kind == .comptime_if {
		cond := t.subst_field_cond(node.value, var_name, fm)
		if !comptime_cond_has_loop_member_ref(cond, var_name) {
			if taken := t.eval_field_cond(cond) {
				branch_idx := if taken { 0 } else { 1 }
				if branch_idx >= int(node.children_count) {
					return none
				}
				return t.clone_field_subst(t.a.child(&node, branch_idx), var_name, fm)
			}
		}
	}
	return t.clone_field_subst_children(node, var_name, fm)
}

// dollar_selector_names_var reports whether a `$(...)` selector's name expression is the current
// field loop's `<var>.name`, so only the matching loop rewrites the selector and a nested loop's
// `$(inner.name)` is deferred to its own unroll.
fn (t &Transformer) dollar_selector_names_var(name_id flat.NodeId, var_name string) bool {
	if int(name_id) < 0 {
		return false
	}
	name_expr := t.a.nodes[int(name_id)]
	if name_expr.kind != .selector || name_expr.value != 'name' || name_expr.children_count == 0 {
		return false
	}
	base := t.a.child_node(&name_expr, 0)
	return base.kind == .ident && base.value == var_name
}

fn (mut t Transformer) clone_field_subst_children(node flat.Node, var_name string, fm FieldMeta) ?flat.NodeId {
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		if c := t.clone_field_subst(t.a.child(&node, i), var_name, fm) {
			children << c
		}
	}
	start := t.a.children.len
	for c in children {
		t.a.children << c
	}
	return t.a.add_node(flat.Node{
		kind:           node.kind
		kind_id:        node.kind_id
		op:             node.op
		pos:            node.pos
		value:          node.value
		typ:            node.typ
		is_mut:         node.is_mut
		children_start: start
		children_count: flat.child_count(children.len)
	})
}

// field_member_value replaces `<var>.member` with its concrete compile-time value. Returns none
// for an unknown member (e.g. a typo `field.nmae`) so the caller leaves it unresolved rather than
// silently substituting the field name.
fn (mut t Transformer) field_member_value(member string, fm FieldMeta) ?flat.NodeId {
	return match member {
		'name' { t.make_string_literal(fm.name) }
		'is_option', 'is_opt' { t.make_bool_literal(fm.is_option) }
		'is_embed' { t.make_bool_literal(fm.is_embed) }
		'is_array' { t.make_bool_literal(fm.is_array) }
		'is_map' { t.make_bool_literal(fm.is_map) }
		'is_chan' { t.make_bool_literal(fm.is_chan) }
		'is_struct' { t.make_bool_literal(fm.is_struct) }
		'is_enum' { t.make_bool_literal(fm.is_enum) }
		'is_alias' { t.make_bool_literal(fm.is_alias) }
		'is_shared' { t.make_bool_literal(fm.is_shared) }
		'is_atomic' { t.make_bool_literal(fm.is_atomic) }
		'is_mut' { t.make_bool_literal(fm.is_mut) }
		'is_pub' { t.make_bool_literal(fm.is_pub) }
		'indirections' { t.make_int_literal(fm.indirections) }
		'attrs' { t.make_string_array_literal(fm.attrs) }
		'typ' { t.make_string_literal(fm.typ) }
		'unaliased_typ' { t.make_string_literal(fm.unaliased_typ) }
		else { return none }
	}
}

// make_string_array_literal builds a `[]string` literal (empty when `values` is empty) for
// materializing `field.attrs`.
fn (mut t Transformer) make_string_array_literal(values []string) flat.NodeId {
	if values.len == 0 {
		return t.zero_value_for_type('[]string')
	}
	mut ids := []flat.NodeId{cap: values.len}
	for v in values {
		ids << t.make_string_literal(v)
	}
	return t.make_array_literal_typed(ids, '[]string')
}

// subst_value_cond textually substitutes `<var>.name`/`<var>.value` inside a comptime condition
// string for a `$for value in Enum.values` iteration, so `eval_field_cond` can fold it.
fn (t &Transformer) subst_value_cond(cond string, var_name string, name string, value int) string {
	mut c := cond
	c = c.replace('${var_name}.value', value.str())
	c = c.replace('${var_name}.name', "'${name}'")
	return c
}

// subst_field_cond textually substitutes `<var>.member` inside a comptime condition string.
// Longer members are replaced first so `.typ` does not clobber `.unaliased_typ`.
fn (t &Transformer) subst_field_cond(cond string, var_name string, fm FieldMeta) string {
	mut c := cond
	c = c.replace('${var_name}.unaliased_typ', fm.unaliased_typ)
	c = c.replace('${var_name}.indirections', fm.indirections.str())
	c = c.replace('${var_name}.is_option', fm.is_option.str())
	c = c.replace('${var_name}.is_opt', fm.is_option.str())
	c = c.replace('${var_name}.is_embed', fm.is_embed.str())
	c = c.replace('${var_name}.is_array', fm.is_array.str())
	c = c.replace('${var_name}.is_map', fm.is_map.str())
	c = c.replace('${var_name}.is_chan', fm.is_chan.str())
	c = c.replace('${var_name}.is_struct', fm.is_struct.str())
	c = c.replace('${var_name}.is_enum', fm.is_enum.str())
	c = c.replace('${var_name}.is_alias', fm.is_alias.str())
	c = c.replace('${var_name}.is_shared', fm.is_shared.str())
	c = c.replace('${var_name}.is_atomic', fm.is_atomic.str())
	c = c.replace('${var_name}.is_mut', fm.is_mut.str())
	c = c.replace('${var_name}.is_pub', fm.is_pub.str())
	c = c.replace('${var_name}.typ', fm.typ)
	c = c.replace('${var_name}.name', "'${fm.name}'")
	return c
}

fn comptime_cond_has_loop_member_ref(cond string, var_name string) bool {
	prefix := '${var_name}.'
	mut offset := 0
	for offset < cond.len {
		if cond[offset] == `'` || cond[offset] == `"` {
			offset = comptime_cond_skip_string(cond, offset)
			continue
		}
		if offset + prefix.len > cond.len || cond[offset..offset + prefix.len] != prefix {
			offset++
			continue
		}
		if offset > 0 && comptime_cond_name_char(cond[offset - 1]) {
			offset++
			continue
		}
		member_start := offset + prefix.len
		if member_start < cond.len && comptime_cond_name_char(cond[member_start]) {
			return true
		}
		offset = member_start
	}
	return false
}

fn comptime_cond_skip_string(cond string, start int) int {
	quote := cond[start]
	mut i := start + 1
	for i < cond.len {
		if cond[i] == `\\` {
			i += 2
			continue
		}
		if cond[i] == quote {
			return i + 1
		}
		i++
	}
	return cond.len
}

fn comptime_cond_name_char(ch u8) bool {
	return ch.is_letter() || ch.is_digit() || ch == `_`
}

// eval_field_cond evaluates a fully-substituted comptime condition (`is`/`!is`, `==`/`!=`,
// `&&`/`||`/`!`, bare bool). Returns none when it cannot be decided statically.
fn (mut t Transformer) eval_field_cond(cond string) ?bool {
	clean := comptime_condition_strip_outer_parens(cond.trim_space())
	if clean == 'true' {
		return true
	}
	if clean == 'false' {
		return false
	}
	if or_idx := comptime_top_index(clean, '||') {
		left := t.eval_field_cond(clean[..or_idx]) or { return none }
		if left {
			return true
		}
		return t.eval_field_cond(clean[or_idx + 2..])
	}
	if and_idx := comptime_top_index(clean, '&&') {
		left := t.eval_field_cond(clean[..and_idx]) or { return none }
		if !left {
			return false
		}
		return t.eval_field_cond(clean[and_idx + 2..])
	}
	for op in [' !is ', ' is '] {
		if op_idx := comptime_top_index(clean, op) {
			left := clean[..op_idx].trim_space()
			right := clean[op_idx + op.len..].trim_space()
			matches := t.comptime_type_matches(left, right) or { return none }
			return if op == ' is ' { matches } else { !matches }
		}
	}
	for op in [' != ', ' == '] {
		if op_idx := comptime_top_index(clean, op) {
			// String operands may be quoted on one side (`'txt'` from a substituted `field.name`)
			// and bare on the other (`txt` as captured in the condition); compare unquoted.
			left := comptime_unquote(clean[..op_idx].trim_space())
			right := comptime_unquote(clean[op_idx + op.len..].trim_space())
			eq := left == right
			return if op == ' == ' { eq } else { !eq }
		}
	}
	// `field.name in ['id', 'name']` membership. The serialized condition drops the space
	// between `in` and the list literal (`in[...]`), so match the operator token and accept a
	// following space, `[`, or `(`.
	for op in [' !in', ' in'] {
		if op_idx := comptime_top_index(clean, op) {
			after := op_idx + op.len
			if after < clean.len && clean[after] != ` ` && clean[after] != `[`
				&& clean[after] != `(` {
				continue
			}
			needle := comptime_unquote(clean[..op_idx].trim_space())
			found := comptime_list_contains(clean[after..].trim_space(), needle)
			return if op == ' in' { found } else { !found }
		}
	}
	// Integer ordering (e.g. `field.indirections < 2`); longer operators first.
	for op in [' <= ', ' >= ', ' < ', ' > '] {
		if op_idx := comptime_top_index(clean, op) {
			left := clean[..op_idx].trim_space()
			right := clean[op_idx + op.len..].trim_space()
			if !comptime_is_int(left) || !comptime_is_int(right) {
				return none
			}
			l := left.int()
			r := right.int()
			return match op {
				' <= ' { l <= r }
				' >= ' { l >= r }
				' < ' { l < r }
				else { l > r }
			}
		}
	}
	if clean.starts_with('!') {
		inner := t.eval_field_cond(clean[1..]) or { return none }
		return !inner
	}
	return none
}

fn comptime_list_contains(list_text string, needle string) bool {
	clean := list_text.trim_space()
	if !clean.starts_with('[') || !clean.ends_with(']') {
		return false
	}
	inner := clean[1..clean.len - 1]
	for part in inner.split(',') {
		if comptime_unquote(part.trim_space()) == needle {
			return true
		}
	}
	return false
}

fn comptime_is_int(s string) bool {
	if s.len == 0 {
		return false
	}
	start := if s[0] == `-` || s[0] == `+` { 1 } else { 0 }
	if start >= s.len {
		return false
	}
	for i in start .. s.len {
		if !s[i].is_digit() {
			return false
		}
	}
	return true
}

fn comptime_unquote(s string) string {
	if s.len >= 2 && (s[0] == `'` || s[0] == `"`) && s[s.len - 1] == s[0] {
		return s[1..s.len - 1]
	}
	return s
}

fn comptime_top_index(s string, op string) ?int {
	idx := comptime_condition_top_level_index(s, op)
	if idx >= 0 {
		return idx
	}
	return none
}
