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
	indirections  int
}

fn comptime_for_parts(value string) (string, string) {
	if idx := value.index('|') {
		return value[..idx], value[idx + 1..]
	}
	return value, 'fields'
}

// comptime_for_base_type resolves the loop source type to a concrete name. Generic `T` was already
// substituted to the concrete type in `node.typ` during monomorphization.
fn (t &Transformer) comptime_for_base_type(raw string) string {
	return t.normalize_type_alias(raw.trim_space())
}

// expand_comptime_for unrolls a `$for` loop into concrete per-field statements. Kinds other than
// `fields` (methods/variants/values/attributes) are not yet supported and expand to nothing,
// preserving the pre-existing skip behavior.
fn (mut t Transformer) expand_comptime_for(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return []flat.NodeId{}
	}
	var_name, kind := comptime_for_parts(node.value)
	if kind != 'fields' {
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

// comptime_body_calls_generic_fn reports whether any statement subtree calls a generic function
// (matched by the callee's short name against the collected generic declarations).
fn (mut t Transformer) comptime_body_calls_generic_fn(stmts []flat.NodeId) bool {
	decls := t.cached_generic_fn_decls()
	mut generic_names := map[string]bool{}
	for key, _ in decls {
		generic_names[key.all_after_last('.')] = true
	}
	for sid in stmts {
		if t.subtree_calls_generic(sid, generic_names) {
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
	// The loop variable used bare (not `field.member`/`field.$(...)`) means it is passed as a
	// `FieldData` value, which is not materialized yet.
	if node.kind == .ident && node.value == var_name {
		return true
	}
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

fn (t &Transformer) subtree_calls_generic(id flat.NodeId, generic_names map[string]bool) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .call && node.children_count > 0 {
		callee := t.a.child_node(&node, 0)
		mut name := ''
		if callee.kind == .ident {
			name = callee.value.all_after_last('.')
		} else if callee.kind == .selector {
			name = callee.value
		}
		// A nested generic call inside the body was already mangled to `<name>_T_<suffix>`
		// against the outer type args by the generic clone; match its demangled base too.
		gname := if name.contains('_T_') { name.all_before('_T_') } else { name }
		if name.len > 0 && (generic_names[name] || generic_names[gname]) {
			return true
		}
	}
	for i in 0 .. node.children_count {
		if t.subtree_calls_generic(t.a.child(&node, i), generic_names) {
			return true
		}
	}
	return false
}

// comptime_field_metas derives FieldData for every field of the concrete struct type.
fn (t &Transformer) comptime_field_metas(base_type string) []FieldMeta {
	info := t.lookup_struct_info(base_type) or { return []FieldMeta{} }
	mut metas := []FieldMeta{cap: info.fields.len}
	for f in info.fields {
		// V's `field.typ` is the type as written (`MyInt`, `?[]int`); `raw_typ` preserves that,
		// while `typ` has already been alias-resolved.
		ftyp := if f.raw_typ.len > 0 { f.raw_typ } else { f.typ }
		metas << t.field_meta_for(f.name, ftyp, f.is_embedded)
	}
	return metas
}

fn (t &Transformer) field_meta_for(name string, ftyp string, is_embed bool) FieldMeta {
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
	unaliased := t.normalize_type_alias(core)
	is_alias := !isnil(t.tc)
		&& (core in t.tc.type_aliases || t.qualified_alias_name(core) in t.tc.type_aliases)
	return FieldMeta{
		name:          name
		typ:           ftyp
		unaliased_typ: unaliased
		is_option:     is_option
		is_embed:      is_embed
		is_array:      unaliased.starts_with('[]') || t.is_fixed_array_type(unaliased)
		is_map:        unaliased.starts_with('map[')
		is_chan:       unaliased.starts_with('chan ')
		is_struct:     unaliased in t.structs && !comptime_is_primitive_type(unaliased)
		is_enum:       unaliased in t.enum_types
		is_alias:      is_alias
		is_shared:     is_shared
		is_atomic:     is_atomic
		indirections:  indir
	}
}

fn comptime_is_primitive_type(typ string) bool {
	return typ in ['string', 'bool', 'rune', 'char', 'i8', 'i16', 'i32', 'i64', 'int', 'isize',
		'u8', 'byte', 'u16', 'u32', 'u64', 'usize', 'f32', 'f64', 'int literal', 'float literal',
		'voidptr', 'byteptr', 'charptr', 'nil', 'void']
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
	// `<var>.member` compile-time member access.
	if node.kind == .selector && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind == .ident && base.value == var_name {
			return t.field_member_value(node.value, fm)
		}
		// `receiver.$(<var>.name)` compile-time field selector.
		if node.value == '$' && node.children_count >= 2 {
			receiver := t.clone_field_subst(t.a.child(&node, 0), var_name, fm) or { return none }
			return t.make_selector(receiver, fm.name, fm.typ)
		}
	}
	// `$if`/`$else $if` referencing the loop variable: evaluate now, keep the taken branch.
	if node.kind == .comptime_if {
		cond := t.subst_field_cond(node.value, var_name, fm)
		if taken := t.eval_field_cond(cond) {
			branch_idx := if taken { 0 } else { 1 }
			if branch_idx >= int(node.children_count) {
				return none
			}
			return t.clone_field_subst(t.a.child(&node, branch_idx), var_name, fm)
		}
	}
	return t.clone_field_subst_children(node, var_name, fm)
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

// field_member_value replaces `<var>.member` with its concrete compile-time value.
fn (mut t Transformer) field_member_value(member string, fm FieldMeta) flat.NodeId {
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
		'is_mut' { t.make_bool_literal(false) }
		'is_pub' { t.make_bool_literal(true) }
		'indirections' { t.make_int_literal(fm.indirections) }
		'attrs' { t.zero_value_for_type('[]string') }
		'typ', 'unaliased_typ' { t.make_string_literal(fm.typ) }
		else { t.make_string_literal(fm.name) }
	}
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
	c = c.replace('${var_name}.is_mut', 'false')
	c = c.replace('${var_name}.is_pub', 'true')
	c = c.replace('${var_name}.typ', fm.typ)
	c = c.replace('${var_name}.name', "'${fm.name}'")
	return c
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
			left := clean[..op_idx].trim_space()
			right := clean[op_idx + op.len..].trim_space()
			eq := left == right
			return if op == ' == ' { eq } else { !eq }
		}
	}
	if clean.starts_with('!') {
		inner := t.eval_field_cond(clean[1..]) or { return none }
		return !inner
	}
	return none
}

fn comptime_top_index(s string, op string) ?int {
	idx := comptime_condition_top_level_index(s, op)
	if idx >= 0 {
		return idx
	}
	return none
}
