module transform

import os
import strconv
import strings
import v.flat
import v.token
import v.types
import v.util
import v.workers

const comptime_unsupported_late_generic_call = '__v3_comptime_unsupported_late_generic_call'
const comptime_method_selector_marker = '__v3_comptime_method_selector'
const comptime_int_literal_type_id = flat.source_type_id('int literal')
const comptime_float_literal_type_id = flat.source_type_id('float literal')
const comptime_typ_member_id = flat.source_type_id('typ')
const comptime_unaliased_typ_member_id = flat.source_type_id('unaliased_typ')
const comptime_type_group_array = flat.source_type_id('\$array')
const comptime_type_group_array_dynamic = flat.source_type_id('\$array_dynamic')
const comptime_type_group_array_fixed = flat.source_type_id('\$array_fixed')
const comptime_type_group_map = flat.source_type_id('\$map')
const comptime_type_group_function = flat.source_type_id('\$function')
const comptime_type_group_option = flat.source_type_id('\$option')
const comptime_type_group_shared = flat.source_type_id('\$shared')
const comptime_type_group_pointer = flat.source_type_id('\$pointer')
const comptime_type_group_voidptr = flat.source_type_id('\$voidptr')
const comptime_type_group_int = flat.source_type_id('\$int')
const comptime_type_group_float = flat.source_type_id('\$float')
const comptime_type_group_string = flat.source_type_id('\$string')
const comptime_type_group_struct = flat.source_type_id('\$struct')
const comptime_type_group_enum = flat.source_type_id('\$enum')
const comptime_type_group_alias = flat.source_type_id('\$alias')
const comptime_type_group_sumtype = flat.source_type_id('\$sumtype')
const comptime_type_group_interface = flat.source_type_id('\$interface')
const comptime_clone_syntax_flags = flat.node_flag_freed_assignment |
	flat.node_flag_literal_interpolation_text

// vmod_root supports vmod root handling for Transformer.
fn (t &Transformer) vmod_root() string {
	mut dir := if t.cur_file.len > 0 { os.dir(t.cur_file) } else { os.getwd() }
	if dir.len == 0 {
		dir = os.getwd()
	}
	return util.nearest_vmod_root(t.cur_file) or { os.real_path(dir) }
}

// Compile-time reflection: `$for field in T.fields { ... }`.
//
// The parser emits a `comptime_for` node (value `<var>|<kind>`, typ = base type) and defers the
// loop body verbatim. Once the base type is concrete (after generic monomorphization) the loop is
// unrolled here: the body is cloned once per field with the loop variable's compile-time members
// substituted, then transformed normally. Members mirror V's `FieldData`.

// FieldMeta is the compile-time metadata exposed by the `$for field in T.fields` loop variable.
struct FieldMeta {
	name                string
	typ                 u32
	unaliased_typ       u32
	comptime_typ        u32
	comptime_unaliased  u32
	typ_id              int
	unaliased_id        int
	is_option           bool
	is_embed            bool
	is_array            bool
	is_map              bool
	is_chan             bool
	is_struct           bool
	is_enum             bool
	is_alias            bool
	is_shared           bool
	is_atomic           bool
	is_mut              bool
	is_pub              bool
	attr_kinds          u32
	attribute_module_id u32
	attrs               []string
	indirections        int
}

struct EnumValueMeta {
	name                string
	value               i64
	value_from_c        bool
	attr_kinds          u32
	attrs               []string
	enum_name           u32
	attribute_module_id u32
}

// raw_attribute_data decodes field attribute kinds only for a reflection consumer.
fn (fm &FieldMeta) raw_attribute_data() RawAttributeData {
	return RawAttributeData{
		attrs: fm.attrs
		kinds: comptime_attribute_kinds(fm.attr_kinds, fm.attrs.len)
	}
}

// attribute_metas materializes field attribute metadata only when requested.
fn (fm &FieldMeta) attribute_metas() []AttributeMeta {
	data := fm.raw_attribute_data()
	return comptime_attribute_metas_from_raw(data.attrs, data.kinds)
}

// raw_attribute_data decodes enum attribute kinds only for a reflection consumer.
fn (item &EnumValueMeta) raw_attribute_data() RawAttributeData {
	return RawAttributeData{
		attrs: item.attrs
		kinds: comptime_attribute_kinds(item.attr_kinds, item.attrs.len)
	}
}

// attribute_metas materializes enum attribute metadata only when requested.
fn (item &EnumValueMeta) attribute_metas() []AttributeMeta {
	data := item.raw_attribute_data()
	return comptime_attribute_metas_from_raw(data.attrs, data.kinds)
}

struct VariantMeta {
	typ    u32
	typ_id int
}

struct MethodMeta {
	name        string
	receiver    u32
	module_name string
	location    string
	return_type u32
	is_pub      bool
	params      []ParamMeta
	attrs       []string
	attributes  []AttributeMeta
}

struct ComptimeMethodDecl {
	id          int
	module_name string
	file_name   string
}

struct ParamMeta {
	name        string
	typ         u32
	module_name string
}

struct AttributeMeta {
	name    string
	arg     string
	has_arg bool
	// schema_narrowed is set while cloning code that a `$if attr is Schema`
	// test selected for this instance: there, schema fields win over the
	// `VAttribute` members of the same name.
	schema_narrowed bool
	kind            int
	// Legacy VAttribute pieces do not allocate typed-schema metadata. The
	// pointed-to fields and bound values are immutable, including in clones.
	schema &AttributeSchemaInstance = unsafe { nil }
}

// AttributeSchemaInstance holds the cold data for one grouped `@[Schema(...)]` use.
struct AttributeSchemaInstance {
	name   string
	module string
	fields []AttributeSchemaFieldInfo
	values []string
}

// AttributeSchemaFieldKind is the argument kind a schema field accepts. Field
// types are classified like the checker's `attribute_field_kind`.
enum AttributeSchemaFieldKind {
	string_
	integer
	boolean
	enum_
}

// AttributeSchemaFieldInfo is one field of a `@[attribute]` schema struct.
struct AttributeSchemaFieldInfo {
	name string
	kind AttributeSchemaFieldKind
	// type_id is the declared field type; for an enum field, the enum itself.
	type_id u32
}

// AttributeSchemaInfo is a struct registered with `@[attribute]`.
struct AttributeSchemaInfo {
	name   string
	module string
	fields []AttributeSchemaFieldInfo
	// supported is false when a field type is not a string, integer, bool, or
	// enum. The checker rejects such a schema, so its uses stay legacy.
	supported bool
}

// ComptimeAttributeSchemaDecl locates one struct marked with `@[attribute]`.
struct ComptimeAttributeSchemaDecl {
	node_id     int
	module_name string
}

// ComptimeAttributeSchemaState is allocated only when a worker indexes a schema.
struct ComptimeAttributeSchemaState {
mut:
	decls []ComptimeAttributeSchemaDecl
	cache []AttributeSchemaInfo
	built int
}

// AttributeCondNarrowing is what the `is` tests of a `$if` condition imply
// for one attribute instance.
struct AttributeCondNarrowing {
	// decided is set when an `is` test alone decides the condition (`value`).
	decided bool
	value   bool
mut:
	// then_branch and else_branch report the branches in which the loop
	// variable is known to be this instance's schema.
	then_branch bool
	else_branch bool
}

struct RawAttributeData {
	attrs []string
	kinds []int
}

struct EnumDeclFieldValue {
	name       string
	expr_id    flat.NodeId
	attrs      []string
	attr_kinds u32
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
fn (t &Transformer) comptime_for_base_type(raw_id u32) u32 {
	source := t.comptime_for_value_source_type(raw_id) or { raw_id }
	base := t.comptime_for_value_type_base(source)
	// Checked value types already identify their declaration, unlike source import aliases.
	if source != raw_id && t.type_name_is_declared_id(base) {
		return t.comptime_normalize_type_alias_chain(base)
	}
	return t.comptime_normalize_type_alias_chain(t.comptime_resolve_selective_import_type(base))
}

fn (t &Transformer) comptime_for_value_source_type(raw_id u32) ?u32 {
	clean_id := t.trimmed_type_id(raw_id)
	if clean_id == 0 {
		return none
	}
	parts := flat.source_type_syntax(clean_id).dotted_parts()
	if parts.len == 0 {
		return none
	}
	mut typ := t.comptime_for_var_source_type(flat.source_type_name(parts[0])) or { return none }
	for field in parts[1..] {
		typ = t.comptime_for_field_source_type(typ, flat.source_type_name(field)) or { return none }
	}
	return typ
}

fn (t &Transformer) comptime_for_var_source_type(name string) ?u32 {
	raw_typ := t.trimmed_type_id(t.raw_var_type(name))
	if raw_typ != 0 {
		return t.comptime_for_value_type_base(raw_typ)
	}
	typ := t.trimmed_type_id(t.var_type(name))
	if typ != 0 {
		return t.comptime_for_value_type_base(typ)
	}
	return none
}

fn (t &Transformer) comptime_for_field_source_type(owner_type_id u32, field_name string) ?u32 {
	base_source_id := t.comptime_normalize_type_alias_chain(t.comptime_for_value_type_base(owner_type_id))
	base_source_id_2 := base_source_id
	eprintln('DEBUG field_source: owner=${owner_type_id} field=${field_name} base_source_id=${base_source_id}')


	info := t.lookup_struct_info(base_source_id_2) or {
		t.generic_struct_info_for_stringify(base_source_id) or { return none }
	}
	for field in info.fields {
		if field.name == field_name {
			ftyp := if field.typ != 0 { field.typ } else { field.raw_typ }
			return t.comptime_for_value_type_base(ftyp)
		}
	}
	return none
}

fn (t &Transformer) comptime_for_value_type_base(raw_id u32) u32 {
	mut typ := transform_trimmed_type_id(raw_id)
	for {
		shape := flat.source_type_shape(typ)
		if shape.has(.mutable_type) || shape.has(.shared_type) || shape.has(.atomic_type)
			|| shape.has(.pointer) {
			typ = transform_trimmed_type_id(flat.source_type_syntax(typ).raw_base())
		} else {
			break
		}
	}
	return typ
}

fn (t &Transformer) comptime_resolve_selective_import_type(raw_id u32) u32 {
	clean_id := t.trimmed_type_id(raw_id)
	if clean_id == 0 || isnil(t.tc) || t.cur_file.len == 0 {
		return clean_id
	}
	if flat.source_type_shape(clean_id).has(.dotted) {
		syntax := flat.source_type_syntax(clean_id)
		if imported := t.file_import_module(t.cur_file, flat.source_type_name(syntax.first_dot_prefix())) {
			return flat.source_type_composition_two(.qualified, flat.source_type_id(imported), syntax.first_dot_suffix())
		}
		// Generic specialization already supplies canonical types. An unrelated
		// import alias from another file must not rebind `config.Cfg` to
		// `rand.config.Cfg` before its fields are reflected.
		if t.type_name_is_declared_id(clean_id) {
			return clean_id
		}
		if imported_source_id := t.resolve_imported_type_name(clean_id) {
			return imported_source_id
		}
		return clean_id
	}
	for candidate_id in t.tc.file_selective_import_ids(t.cur_file, clean_id) or { []u32{} } {
		if t.tc.type_aliases.contains(candidate_id) || candidate_id in t.tc.structs
			|| candidate_id in t.tc.interface_names || candidate_id in t.tc.flag_enums
			|| candidate_id in t.tc.enum_names || t.tc.sum_types.contains(candidate_id) {
			return candidate_id
		}
	}
	return clean_id
}

fn (t &Transformer) comptime_resolve_selective_import_reflection_source(raw string) string {
	clean := trimmed_transform_text(raw)
	if clean.len == 0 || isnil(t.tc) || t.cur_file.len == 0 {
		return clean
	}
	if local := t.comptime_local_reflection_source(clean) {
		return local
	}
	if clean.contains('.') {
		return flat.source_type_name(t.resolve_imported_type_name(flat.source_type_id(clean)) or { flat.source_type_id(clean) })
	}
	clean_id := flat.source_type_id(clean)
	resolved_type := t.comptime_resolve_selective_import_type(clean_id)
	if resolved_type != clean_id {
		return flat.source_type_name(resolved_type)
	}
	for candidate_id in t.tc.file_selective_import_ids(t.cur_file, clean_id) or { []u32{} } {
		// Function tables are keyed by declaration names.
		candidate := flat.source_type_name(candidate_id)
		if candidate in t.tc.fn_ret_types || candidate in t.tc.fn_param_types {
			return candidate
		}
	}
	return clean
}

fn (t &Transformer) comptime_local_reflection_source(name string) ?string {
	if name == '' || t.cur_module.len == 0 || t.cur_module in ['main', 'builtin'] {
		return none
	}
	qualified := '${t.cur_module}.${name}'
	if qualified in t.tc.fn_ret_types || qualified in t.tc.fn_param_types {
		return qualified
	}
	qualified_id := flat.source_type_existing_id(qualified) or { return none }
	if t.tc.type_aliases.contains(qualified_id) || qualified_id in t.tc.structs
		|| qualified_id in t.tc.interface_names || qualified_id in t.tc.flag_enums
		|| qualified_id in t.tc.enum_names || t.tc.sum_types.contains(qualified_id) {
		return qualified
	}
	return none
}

fn (mut t Transformer) cache_comptime_param_reflection_metadata() {
	if !t.has_used_fn_filter() || isnil(t.tc) || t.tc.top_level_idx.len == 0 {
		return
	}
	old_file := t.cur_file
	old_module := t.cur_module
	old_fn_name := t.cur_fn_name
	// Finding the rare `$for x in f.params` loops walks every node of every function
	// body. Record each function's node range serially, scan the ranges on the worker
	// pool, then resolve the hits here in source order, exactly as a serial walk.
	mut ranges := []ComptimeParamScanRange{cap: t.tc.top_level_idx.len}
	mut cur_file := ''
	mut cur_module := ''
	mut previous_top_level := -1
	for _, top_level_idx in t.tc.top_level_idx {
		node := t.a.nodes[top_level_idx]
		if node.kind == .file {
			cur_file = node.value
			cur_module = ''
		} else if node.kind == .module_decl {
			cur_module = node.value
		} else if node.kind == .fn_decl {
			ranges << ComptimeParamScanRange{
				lo:      previous_top_level + 1
				fn_idx:  top_level_idx
				file:    cur_file
				module:  cur_module
				fn_name: node.value
			}
		}
		previous_top_level = top_level_idx
	}
	scans := t.scan_comptime_param_ranges(ranges)
	for scan in scans {
		for hit in scan.hits {
			r := ranges[hit.range_idx]
			t.cur_file = r.file
			t.cur_module = r.module
			t.cur_fn_name = r.fn_name
			idx := hit.node_idx
			candidate := t.a.nodes[idx]
			_, kind := comptime_for_parts(candidate.value)
			if kind != 'params' {
				continue
			}
			source := t.comptime_reflection_source(candidate.typ_text(), flat.NodeId(idx))
			resolved := t.comptime_resolve_selective_import_reflection_source(source)
			if resolved != source && resolved !in t.comptime_reflected_params {
				params := t.comptime_param_metas(resolved)
				t.comptime_reflected_params[resolved] = params
			}
		}
	}
	t.cur_file = old_file
	t.cur_module = old_module
	t.cur_fn_name = old_fn_name
}

// ComptimeParamScanRange is one function's node range, (previous top-level
// declaration, fn_idx), with the context its `$for` loops resolve in.
struct ComptimeParamScanRange {
	lo      int
	fn_idx  int
	file    string
	module  string
	fn_name string
}

struct ComptimeParamScanHit {
	range_idx int
	node_idx  int
}

struct ComptimeParamScan {
	t      voidptr // &Transformer, read-only while scanning
	start  int
	end    int
	ranges voidptr // &[]ComptimeParamScanRange
mut:
	hits []ComptimeParamScanHit
}

const comptime_param_scan_max_tasks = 16

// scan_comptime_param_ranges finds the `$for` nodes of every transformed
// function's range. Each task covers a contiguous run of ranges, so the tasks'
// hits concatenated in task order are in source order.
fn (t &Transformer) scan_comptime_param_ranges(ranges []ComptimeParamScanRange) []ComptimeParamScan {
	mut task_count := 1
	if !isnil(t.a.worker_pool) {
		task_count = t.a.worker_pool.size() + 1
	}
	if task_count > comptime_param_scan_max_tasks {
		task_count = comptime_param_scan_max_tasks
	}
	if task_count > ranges.len {
		task_count = ranges.len
	}
	if task_count <= 1 {
		mut scan := ComptimeParamScan{
			t:      voidptr(t)
			start:  0
			end:    ranges.len
			ranges: unsafe { voidptr(&ranges) }
		}
		comptime_param_scan_thread(voidptr(&scan))
		return [scan]
	}
	mut total := i64(0)
	for r in ranges {
		total += i64(r.fn_idx - r.lo) + 1
	}
	mut scans := []ComptimeParamScan{cap: task_count}
	mut start := 0
	mut consumed := i64(0)
	for ti in 0 .. task_count {
		target := total * i64(ti + 1) / i64(task_count)
		mut end := start
		for end < ranges.len && (consumed < target || ti == task_count - 1) {
			consumed += i64(ranges[end].fn_idx - ranges[end].lo) + 1
			end++
		}
		scans << ComptimeParamScan{
			t:      voidptr(t)
			start:  start
			end:    end
			ranges: unsafe { voidptr(&ranges) }
		}
		start = end
	}
	mut tasks := []workers.Task{cap: scans.len}
	for i in 0 .. scans.len {
		tasks << workers.Task{
			run:        comptime_param_scan_thread
			arg:        unsafe { voidptr(&scans[i]) }
			force_sync: i == 0
		}
	}
	mut pool := t.a.worker_pool
	pool.run(tasks)
	return scans
}

fn comptime_param_scan_thread(arg voidptr) voidptr {
	mut scan := unsafe { &ComptimeParamScan(arg) }
	t := unsafe { &Transformer(scan.t) }
	ranges := unsafe { &[]ComptimeParamScanRange(scan.ranges) }
	for ri in scan.start .. scan.end {
		r := unsafe { ranges[ri] }
		if !t.should_transform_fn_in_module(t.a.nodes[r.fn_idx], r.module) {
			continue
		}
		for idx in r.lo .. r.fn_idx {
			if t.a.nodes[idx].kind == .comptime_for {
				scan.hits << ComptimeParamScanHit{
					range_idx: ri
					node_idx:  idx
				}
			}
		}
	}
	return unsafe { nil }
}

fn (t &Transformer) comptime_normalize_type_alias_chain(raw_id u32) u32 {
	mut typ := t.trimmed_type_id(raw_id)
	if typ == 0 { return typ }
	// Most names are not aliases; settle that before allocating the cycle set.
	normalized := t.trimmed_type_id(t.normalize_type_alias(typ))
	if normalized == typ { return typ }
	mut seen := map[u32]bool{}
	seen[typ] = true
	typ = normalized
	for typ != 0 && typ !in seen {
		seen[typ] = true
		next := t.trimmed_type_id(t.normalize_type_alias(typ))
		if next == typ { break }
		typ = next
	}
	return typ
}

// trimmed_text_equals reports whether `s.trim_space() == want` without
// allocating the trimmed copy.
fn trimmed_text_equals(s string, want string) bool {
	mut lo := 0
	mut hi := s.len
	for lo < hi && is_trim_space_byte(s[lo]) {
		lo++
	}
	for hi > lo && is_trim_space_byte(s[hi - 1]) {
		hi--
	}
	if hi - lo != want.len {
		return false
	}
	for i in 0 .. want.len {
		if s[lo + i] != want[i] {
			return false
		}
	}
	return true
}

// is_trim_space_byte matches the bytes String.trim_space removes.
@[inline]
fn is_trim_space_byte(c u8) bool {
	return c == ` ` || c == `\n` || c == `\t` || c == `\v` || c == `\f` || c == `\r`
}

// comptime_typeof_unaliased_type removes only aliases at the root of a reflected type.
// Aliases nested inside arrays, maps, options, and other aggregate types retain their identity.
fn (t &Transformer) comptime_typeof_unaliased_type(raw_id u32) u32 {
	clean_id := transform_trimmed_type_id(raw_id)
	if clean_id == 0 || isnil(t.tc) {
		return t.comptime_normalize_type_alias_chain(clean_id)
	}
	parsed := t.tc.parse_type(clean_id)
	if parsed is types.Unknown {
		return t.comptime_normalize_type_alias_chain(clean_id)
	}
	return types.unalias_type(parsed).source_type_id()
}

// expand_comptime_for unrolls the supported compile-time reflection loops into concrete
// statements.
fn (mut t Transformer) expand_comptime_for(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return []flat.NodeId{}
	}
	var_name, kind := comptime_for_parts(node.value)
	if kind == 'strings' && node.children_count == 2 {
		return t.expand_comptime_for_strings(id, node, var_name)
	}
	if kind !in ['fields', 'values', 'variants', 'methods', 'params', 'attributes'] {
		return []flat.NodeId{}
	}
	// Resolve the raw source only after checking for an open generic. Earlier
	// transform passes can still have a concrete type cached for the same
	// one-letter name from another function, but this template's `T` must not be
	// expanded until its specialization is cloned.
	if flat.source_type_query(node.typ).has(.generic_fn_name) && t.generic_arg_is_unresolved(node.typ) {
		return [id]
	}
	base_type := (
		if node.payload == flat.canonical_comptime_type_payload {
			// A substituted T is canonical even when this file imports a different type
			// under the same spelling. Source-written operands still resolve their aliases.
			if kind == 'methods' {
				node.typ
			} else {
				t.comptime_normalize_type_alias_chain(t.comptime_for_value_type_base(node.typ))
			}
		} else if kind == 'methods' {
			source_source_id := t.comptime_for_value_source_type(node.typ) or { node.typ }
			if source_source_id != node.typ && t.type_name_is_declared_id(t.comptime_for_value_type_base(source_source_id)) {
				source_source_id
			} else {
				t.comptime_resolve_selective_import_type(source_source_id)
			}
		} else {
			t.comptime_for_base_type(node.typ)
		}
	)
	// An open generic template must survive the pre-monomorph transform pass.
	// Its metadata and `$zero(field.typ_text())` children are needed when the concrete
	// specialization is cloned later; erasing them here leaves empty child ids.
	if t.generic_arg_is_unresolved(base_type) {
		return [id]
	}
	t.ignore_comptime_for_subtree(id)
	body_id := t.a.child(&node, 0)
	body := t.a.nodes[int(body_id)]
	body_stmts := t.a.children_of(&body).clone()
	if kind == 'values' {
		return t.expand_comptime_for_values(var_name, base_type, body_stmts)
	}
	if kind == 'variants' {
		return t.expand_comptime_for_variants(var_name, base_type, body_stmts)
	}
	if kind == 'methods' {
		return t.expand_comptime_for_methods(var_name, base_type, body_stmts)
	}
	if kind == 'params' {
		return t.expand_comptime_for_params(var_name, node.typ_text(), body_stmts, id)
	}
	if kind == 'attributes' {
		return t.expand_comptime_for_attributes(var_name, node.typ_text(), body_stmts, id)
	}
	// Comptime `match field.typ_text() { int {} ... }` (type match) is not modelled yet; skip such
	// loops rather than mis-lower them. `typeof(receiver.$(field.name))` is checked after
	// substitution, when the dynamic selector has become a concrete field selector.
	if t.comptime_body_has_unsupported(body_stmts, var_name, false) {
		return []flat.NodeId{}
	}
	mut out := []flat.NodeId{}
	for fm in t.comptime_field_metas(base_type) {
		mut cloned := []flat.NodeId{cap: body_stmts.len}
		// The clone derives types from the locals of the body, as the type of
		// `&routes`. It declares them as it goes, so that another declaration of
		// the name, such as a const, does not stand in for them. The iteration is
		// then transformed with the bindings that it started with.
		saved_vars := t.var_types.clone()
		undo_was_active := t.var_type_undo_active
		undo_start := t.var_type_undo_log.len
		was_declaring := t.declaring_comptime_field_locals
		t.var_type_undo_active = true
		t.declaring_comptime_field_locals = true
		for sid in body_stmts {
			if cid := t.clone_field_subst(sid, var_name, fm) {
				cloned << cid
			}
		}
		t.declaring_comptime_field_locals = was_declaring
		t.var_type_undo_active = undo_was_active
		t.var_type_undo_log.trim(undo_start)
		t.restore_var_types(saved_vars)
		if t.comptime_body_has_unsupported(cloned, var_name, true) {
			return []flat.NodeId{}
		}
		// One block per iteration so per-field temps get their own scope.
		old_allow_enum_int := t.allow_comptime_enum_int_assign
		t.allow_comptime_enum_int_assign = old_allow_enum_int || fm.is_enum
		transformed := t.transform_scope_stmts(cloned)
		t.allow_comptime_enum_int_assign = old_allow_enum_int
		out << t.make_block(transformed)
	}
	return out
}

fn (mut t Transformer) expand_comptime_for_attributes(var_name string, source string, body_stmts []flat.NodeId, loop_id flat.NodeId) []flat.NodeId {
	mut out := []flat.NodeId{}
	schemas := t.comptime_attribute_schemas()
	for attr in t.comptime_attribute_instances(source, loop_id, schemas) {
		mut cloned := []flat.NodeId{cap: body_stmts.len}
		for sid in body_stmts {
			cloned << t.clone_attribute_subst(sid, var_name, attr, schemas)
		}
		out << t.make_block(t.transform_scope_stmts(cloned))
	}
	return out
}

// comptime_reflected_attributes_loop_var returns the loop variable of `$for a in <var>.attributes`
// when `var` is the variable of the enclosing field or enum value loop, and '' otherwise.
fn comptime_reflected_attributes_loop_var(node flat.Node, var_name string) string {
	if node.kind != .comptime_for || node.children_count == 0 {
		return ''
	}
	loop_var, kind := comptime_for_parts(node.value)
	if kind != 'attributes' || trimmed_transform_text(node.typ_text()) != var_name {
		return ''
	}
	return loop_var
}

// unroll_reflected_attributes_loop expands `$for a in field.attributes` (or `value.attributes`)
// over the attributes of the field or enum value that is being unrolled. Its source names that
// loop variable, not a declaration, so expand_comptime_for_attributes cannot resolve it. The
// blocks are transformed with the rest of the enclosing iteration.
fn (mut t Transformer) unroll_reflected_attributes_loop(loop_id flat.NodeId, attr_var string, data RawAttributeData, decl_module string, inner_vars []string) flat.NodeId {
	t.ignore_comptime_for_subtree(loop_id)
	schemas := t.comptime_attribute_schemas()
	attrs := comptime_attribute_instances_from_raw(data, decl_module, schemas)
	loop := t.a.nodes[int(loop_id)]
	body := t.a.nodes[int(t.a.child(&loop, 0))]
	body_stmts := t.a.children_of(&body).clone()
	mut blocks := []flat.NodeId{cap: attrs.len}
	for attr in attrs {
		mut stmts := []flat.NodeId{cap: body_stmts.len}
		for sid in body_stmts {
			stmts << t.clone_attribute_subst_scoped(sid, attr_var, attr, inner_vars, schemas)
		}
		blocks << t.make_block(stmts)
	}
	return t.make_block(blocks)
}

fn (mut t Transformer) comptime_attribute_metas(source string, loop_id flat.NodeId) []AttributeMeta {
	decl_idx, _ := t.comptime_attribute_decl(source, loop_id) or { return []AttributeMeta{} }
	return t.comptime_node_attribute_metas(decl_idx)
}

// comptime_attribute_decl finds the reflected declaration of an attributes
// loop, returning its node index and module.
fn (t &Transformer) comptime_attribute_decl(source string, loop_id flat.NodeId) ?(int, string) {
	raw_name := t.comptime_reflection_source(source, loop_id)
	name := t.comptime_resolve_selective_import_reflection_source(raw_name)
	lookup_name := if name.starts_with('main.') { name['main.'.len..] } else { name }
	mut module_name := ''
	for idx in 0 .. t.a.nodes.len {
		// This read-only scan keeps the borrowed AST node valid.
		node := unsafe { &t.a.nodes[idx] }
		if node.kind == .file {
			module_name = ''
			continue
		}
		if node.kind == .module_decl {
			module_name = node.value
			continue
		}
		if node.kind !in [.fn_decl, .struct_decl, .enum_decl, .interface_decl, .type_decl] {
			continue
		}
		qualified := if module_name.len > 0 && module_name !in ['main', 'builtin'] {
			'${module_name}.${node.value}'
		} else {
			node.value
		}
		reflection_name := qualified.replace('@static@', '.')
		if reflection_name == name || reflection_name == lookup_name
			|| (module_name == t.cur_module && node.value == lookup_name) {
			return idx, module_name
		}
	}
	return none
}

// comptime_attribute_schemas returns the structs marked with `@[attribute]`
// (vlang/rfcs#35, typed materialization). The registry is built from the
// attribute directives that ensure_comptime_method_index indexes, once per
// transformer, and again only when the index finds new schema declarations.
fn (mut t Transformer) comptime_attribute_schemas() []AttributeSchemaInfo {
	t.ensure_comptime_method_index()
	if isnil(t.comptime_attribute_schema_state) {
		return []AttributeSchemaInfo{}
	}
	if t.comptime_attribute_schema_state.built != t.comptime_attribute_schema_state.decls.len {
		t.comptime_attribute_schema_state.cache = t.build_comptime_attribute_schemas()
		t.comptime_attribute_schema_state.built = t.comptime_attribute_schema_state.decls.len
	}
	return t.comptime_attribute_schema_state.cache
}

// build_comptime_attribute_schemas reads the fields of every indexed schema
// declaration from the checker, which resolved their types in the schema's
// own module.
fn (t &Transformer) build_comptime_attribute_schemas() []AttributeSchemaInfo {
	if isnil(t.comptime_attribute_schema_state) {
		return []AttributeSchemaInfo{}
	}
	mut schemas := []AttributeSchemaInfo{cap: t.comptime_attribute_schema_state.decls.len}
	if isnil(t.tc) {
		return schemas
	}
	for decl in t.comptime_attribute_schema_state.decls {
		node := t.a.nodes[decl.node_id]
		if node.kind != .struct_decl {
			continue
		}
		name := node.value.all_after_last('.')
		mod_name := if node.value.contains('.') {
			node.value.all_before_last('.')
		} else {
			decl.module_name
		}
		type_name := if mod_name in ['', 'main', 'builtin'] { name } else { '${mod_name}.${name}' }
		struct_fields := t.tc.struct_fields_for_type(flat.source_type_id(type_name))
		// A lookup miss must not turn a schema with fields into an empty one.
		mut supported := struct_fields.len > 0 || !t.a.children_of(&node).any(t.a.nodes[int(it)].kind == .field_decl)
		mut fields := []AttributeSchemaFieldInfo{cap: struct_fields.len}
		for field in struct_fields {
			if !supported {
				break
			}
			// The checker leaves embedded structs out of a schema's fields too.
			if field.is_embed {
				continue
			}
			kind, type_id := comptime_attribute_schema_field_kind(field.typ) or {
				supported = false
				break
			}
			fields << AttributeSchemaFieldInfo{
				name:    field.name
				kind:    kind
				type_id: type_id
			}
		}
		schemas << AttributeSchemaInfo{
			name:      name
			module:    mod_name
			fields:    fields
			supported: supported
		}
	}
	return schemas
}

// comptime_attribute_schema_field_kind classifies a schema field type like
// the checker's `attribute_field_kind`: aliases are followed, then strings,
// all integer types, bool, and enums are accepted. It also returns the type
// of the literal bound to the field: the declared type, or the enum.
fn comptime_attribute_schema_field_kind(typ types.Type) ?(AttributeSchemaFieldKind, u32) {
	clean := types.unalias_type(&typ)
	if clean is types.String {
		return AttributeSchemaFieldKind.string_, typ.source_type_id()
	}
	if clean.is_integer() {
		return AttributeSchemaFieldKind.integer, typ.source_type_id()
	}
	if clean.source_type_id() == flat.source_type_bool {
		return AttributeSchemaFieldKind.boolean, typ.source_type_id()
	}
	if clean is types.Enum {
		return AttributeSchemaFieldKind.enum_, clean.name_id
	}
	return none
}

// comptime_attribute_piece_named reports whether the raw attribute piece
// `raw` (`name` or `name: arg`) is named `name`, without allocating.
fn comptime_attribute_piece_named(raw string, name string) bool {
	mut end := raw.index_u8(`:`)
	if end < 0 {
		end = raw.len
	}
	mut start := 0
	for start < end && is_trim_space_byte(raw[start]) {
		start++
	}
	for end > start && is_trim_space_byte(raw[end - 1]) {
		end--
	}
	if end - start != name.len {
		return false
	}
	for i in 0 .. name.len {
		if raw[start + i] != name[i] {
			return false
		}
	}
	return true
}

// comptime_attribute_instances groups the reflected declaration's raw pieces
// into one instance per attribute use. A piece group whose base resolves to a
// registered schema becomes that schema struct; every other piece keeps the
// legacy `VAttribute` view. With no schemas registered the result matches
// `comptime_attribute_metas` exactly.
fn (mut t Transformer) comptime_attribute_instances(source string, loop_id flat.NodeId, schemas []AttributeSchemaInfo) []AttributeMeta {
	decl_idx, decl_module := t.comptime_attribute_decl(source, loop_id) or {
		return []AttributeMeta{}
	}
	data := t.comptime_node_raw_attribute_data(decl_idx)
	return comptime_attribute_instances_from_raw(data, decl_module, schemas)
}

// comptime_attribute_instances_from_raw materializes declaration, field, and enum-value
// attributes in the module where they were written, preserving their exact raw kinds.
fn comptime_attribute_instances_from_raw(data RawAttributeData, decl_module string, schemas []AttributeSchemaInfo) []AttributeMeta {
	if schemas.len == 0 {
		return comptime_attribute_metas_from_raw(data.attrs, data.kinds)
	}
	mut out := []AttributeMeta{}
	mut consumed := []bool{len: data.attrs.len}
	for i in 0 .. data.attrs.len {
		if consumed[i] {
			continue
		}
		base := data.attrs[i].all_before(':').trim_space()
		whole_string := i < data.kinds.len && data.kinds[i] >= 0
			&& data.kinds[i] & flat.attribute_kind_no_arg != 0
		if schema := comptime_attribute_use_schema(base, decl_module, schemas) {
			if whole_string {
				consumed[i] = true
				out << comptime_attribute_metas_from_raw([data.attrs[i]], [data.kinds[i]])
				continue
			}
			mut use_idx := []int{}
			for j in i .. data.attrs.len {
				if consumed[j] || (j < data.kinds.len && data.kinds[j] >= 0
					&& data.kinds[j] & flat.attribute_kind_no_arg != 0) {
					continue
				}
				piece_base := data.attrs[j].all_before(':').trim_space()
				if piece_base == base || piece_base.starts_with(base + '_') {
					use_idx << j
				}
			}
			if inst := comptime_attribute_schema_instance(base, schema, data, use_idx) {
				for j in use_idx {
					consumed[j] = true
				}
				out << inst
				continue
			}
			// A use the checker rejects (unknown field, arity) stays legacy:
			// the checker reports it; the transform must not crash on it.
		}
		consumed[i] = true
		single_attrs := [data.attrs[i]]
		single_kinds := if i < data.kinds.len { [data.kinds[i]] } else { []int{} }
		for meta in comptime_attribute_metas_from_raw(single_attrs, single_kinds) {
			out << meta
		}
	}
	return out
}

// comptime_attribute_use_schema resolves an attribute base to its schema,
// V-strict: a bare `Name` sees the declaration's module only, a dotted
// `mod.Name` sees module `mod` (vlang/rfcs#35, name resolution).
fn comptime_attribute_use_schema(base string, decl_module string, schemas []AttributeSchemaInfo) ?AttributeSchemaInfo {
	mut qualifier := ''
	short := if idx := base.last_index('.') {
		qualifier = base[..idx]
		base[idx + 1..]
	} else {
		base
	}
	for schema in schemas {
		if schema.name != short {
			continue
		}
		if qualifier.len > 0 {
			if schema.module == qualifier {
				return schema
			}
			continue
		}
		if comptime_attribute_modules_compatible(schema.module, decl_module) {
			return schema
		}
	}
	return none
}

// comptime_attribute_modules_compatible mirrors the checker's V-strict module
// rule: a bare name sees the current module only.
fn comptime_attribute_modules_compatible(schema_module string, use_module string) bool {
	if schema_module == use_module {
		return true
	}
	return schema_module in ['', 'main'] && use_module in ['', 'main']
}

// comptime_attribute_schema_instance binds one schema use's flattened pieces
// to the schema fields, positionally then by name (mirroring the checker's
// `check_attribute_use_args` grouping, including the dot-enum split). It
// returns none when the pieces do not bind, leaving them legacy.
fn comptime_attribute_schema_instance(base string, schema AttributeSchemaInfo, data RawAttributeData, use_idx []int) ?AttributeMeta {
	if !schema.supported {
		return none
	}
	mut names := []string{}
	mut values := []string{}
	mut kinds := []int{}
	for j in use_idx {
		raw := data.attrs[j]
		name := raw.all_before(':').trim_space()
		value := if raw.contains(':') { raw.all_after(':').trim_space() } else { '' }
		recorded_kind := if j < data.kinds.len { data.kinds[j] } else { 0 }
		kind := if recorded_kind >= 0 {
			recorded_kind & flat.attribute_kind_mask
		} else {
			recorded_kind
		}
		names << name
		values << value
		kinds << kind
	}
	mut positional := []string{}
	mut positional_kinds := []int{}
	mut named_order := []string{}
	mut named := map[string][]string{}
	mut named_kinds := map[string][]int{}
	mut idx := 0
	for idx < names.len {
		name := names[idx]
		value := values[idx]
		kind := kinds[idx]
		if merged := comptime_attribute_dot_enum_value(base, names, values, idx) {
			if name == base || comptime_attribute_is_numbered_suffix(base, name) {
				positional << merged
				positional_kinds << 0
			} else {
				key := name[base.len + 1..]
				if key !in named {
					named_order << key
				}
				named[key] << merged
				named_kinds[key] << 0
			}
			idx += 2
			continue
		}
		if name == base {
			if value.len > 0 {
				positional << value
				positional_kinds << kind
			}
			idx++
			continue
		}
		if comptime_attribute_is_numbered_suffix(base, name) {
			positional << value
			positional_kinds << kind
			idx++
			continue
		}
		key := name[base.len + 1..]
		if key !in named {
			named_order << key
		}
		named[key] << value
		named_kinds[key] << kind
		idx++
	}
	if positional.len > schema.fields.len {
		return none
	}
	mut bound_values := []string{len: schema.fields.len}
	mut bound := []bool{len: schema.fields.len}
	for positional_idx, value in positional {
		if !comptime_attribute_value_matches(schema.fields[positional_idx].kind, value,
			positional_kinds[positional_idx]) {
			return none
		}
		bound_values[positional_idx] = value
		bound[positional_idx] = true
	}
	for key in named_order {
		mut field_idx := -1
		for f_idx, field in schema.fields {
			if field.name == key {
				field_idx = f_idx
				break
			}
		}
		if field_idx < 0 || bound[field_idx] || named[key].len > 1 {
			return none
		}
		kind := if named_kinds[key].len > 0 { named_kinds[key][0] } else { 0 }
		if !comptime_attribute_value_matches(schema.fields[field_idx].kind, named[key][0], kind) {
			return none
		}
		bound_values[field_idx] = named[key][0]
		bound[field_idx] = true
	}
	for b in bound {
		if !b {
			return none
		}
	}
	base_idx := use_idx[0]
	base_kind := if base_idx < data.kinds.len { data.kinds[base_idx] } else { 0 }
	metadata := comptime_attribute_metas_from_raw([data.attrs[base_idx]], [base_kind])[0]
	return AttributeMeta{
		name:    base
		arg:     metadata.arg
		has_arg: metadata.has_arg
		kind:    metadata.kind
		schema:  &AttributeSchemaInstance{
			name:   schema.name
			module: schema.module
			fields: schema.fields
			values: bound_values
		}
	}
}

// comptime_attribute_dot_enum_value reassembles a dot-prefixed enum value the
// legacy representation splits into an empty piece plus a numbered positional
// holding the bare ident (`Name_key: ` + `Name_1: get` becomes `.get`).
fn comptime_attribute_dot_enum_value(base string, names []string, values []string, idx int) ?string {
	if values[idx].len > 0 || idx + 1 >= names.len {
		return none
	}
	if !comptime_attribute_is_numbered_suffix(base, names[idx + 1])
		|| !comptime_attribute_is_bare_ident(values[idx + 1]) {
		return none
	}
	return '.' + values[idx + 1]
}

// comptime_attribute_is_numbered_suffix reports whether `name` is a numbered
// positional piece (`Base_1`, ...) of the attribute `base`.
fn comptime_attribute_is_numbered_suffix(base string, name string) bool {
	if !name.starts_with(base + '_') {
		return false
	}
	suffix := name[base.len + 1..]
	if suffix.len == 0 {
		return false
	}
	for c in suffix.bytes() {
		if c < `0` || c > `9` {
			return false
		}
	}
	return true
}

// comptime_attribute_value_matches reports whether a raw argument value of
// the recorded piece kind may bind to a schema field of `field_kind`
// (vlang/rfcs#35, argument binding).
fn comptime_attribute_value_matches(field_kind AttributeSchemaFieldKind, value string, kind int) bool {
	return match field_kind {
		.string_ { kind == 1 && comptime_attr_is_string_literal(value) }
		.boolean { kind == 3 && (value == 'true' || value == 'false') }
		.integer { kind == 2 && comptime_attribute_is_integer_literal(value) }
		.enum_ { value.len > 1 && value[0] == `.` && comptime_attribute_is_bare_ident(value[1..]) }
	}
}

// comptime_attribute_is_integer_literal mirrors the checker's decimal, hex,
// octal, and binary forms with optional `-` and `_` separators.
fn comptime_attribute_is_integer_literal(value string) bool {
	mut text := value.replace('_', '')
	if text.starts_with('-') {
		text = text[1..]
	}
	if text.starts_with('0x') || text.starts_with('0X') {
		if text.len <= 2 {
			return false
		}
		for c in text[2..] {
			if !c.is_hex_digit() {
				return false
			}
		}
		return true
	}
	if text.starts_with('0o') || text.starts_with('0O') {
		if text.len <= 2 {
			return false
		}
		for c in text[2..] {
			if c < `0` || c > `7` {
				return false
			}
		}
		return true
	}
	if text.starts_with('0b') || text.starts_with('0B') {
		if text.len <= 2 {
			return false
		}
		for c in text[2..] {
			if c != `0` && c != `1` {
				return false
			}
		}
		return true
	}
	if text.len == 0 {
		return false
	}
	for c in text.bytes() {
		if c < `0` || c > `9` {
			return false
		}
	}
	return true
}

// comptime_attribute_is_bare_ident reports whether `s` is a plain identifier.
fn comptime_attribute_is_bare_ident(s string) bool {
	if s.len == 0 || !(s[0].is_letter() || s[0] == `_`) {
		return false
	}
	for c in s[1..] {
		if !(c.is_alnum() || c == `_`) {
			return false
		}
	}
	return true
}

fn (t &Transformer) comptime_reflection_source(source string, loop_id flat.NodeId) string {
	clean := trimmed_transform_text(source)
	rhs_id := t.comptime_reflection_local_rhs(loop_id, clean) or { return clean }
	// This helper only reads AST nodes while the borrow is live.
	rhs := unsafe { &t.a.nodes[int(rhs_id)] }
	if rhs.kind == .ident {
		return rhs.value
	}
	if rhs.kind == .selector && rhs.children_count > 0 {
		base := t.a.child_node(rhs, 0)
		if base.kind == .ident {
			return '${base.value}.${rhs.value}'
		}
		mut found := ''
		for candidate_id in 0 .. t.a.nodes.len {
			// This read-only scan keeps the borrowed AST node valid.
			candidate := unsafe { &t.a.nodes[candidate_id] }
			if candidate.kind == .fn_decl && candidate.value.contains('.')
				&& candidate.value.all_after_last('.') == rhs.value {
				if found != '' {
					return clean
				}
				found = candidate.value
			}
		}
		if found.len > 0 {
			return found
		}
	}
	return clean
}

fn (t &Transformer) comptime_reflection_local_rhs(loop_id flat.NodeId, name string) ?flat.NodeId {
	if int(loop_id) < 0 || int(loop_id) >= t.a.nodes.len {
		return none
	}
	for idx in 0 .. t.a.nodes.len {
		// This read-only scan keeps the borrowed AST node valid.
		node := unsafe { &t.a.nodes[idx] }
		if node.kind != .fn_decl || (t.cur_fn_name.len > 0 && node.value != t.cur_fn_name) {
			continue
		}
		mut path := []flat.NodeId{}
		if !t.comptime_reflection_node_path(flat.NodeId(idx), loop_id, mut path) {
			continue
		}
		mut rhs_id := flat.empty_node
		for depth in 0 .. path.len - 1 {
			scope := t.a.nodes[int(path[depth])]
			if scope.kind !in [.fn_decl, .block, .for_stmt, .for_in_stmt, .match_branch,
				.select_branch] {
				continue
			}
			child_on_path := path[depth + 1]
			for i in 0 .. scope.children_count {
				child_id := t.a.child(&scope, i)
				if child_id == child_on_path {
					break
				}
				if candidate := t.comptime_reflection_decl_rhs(child_id, name) {
					rhs_id = candidate
				}
			}
		}
		if rhs_id != flat.empty_node {
			return rhs_id
		}
		return none
	}
	return none
}

fn (t &Transformer) comptime_reflection_node_path(id flat.NodeId, target flat.NodeId, mut path []flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	path << id
	if id == target {
		return true
	}
	// This helper only reads AST nodes while the borrow is live.
	node := unsafe { &t.a.nodes[int(id)] }
	for i in 0 .. node.children_count {
		if t.comptime_reflection_node_path(t.a.child(node, i), target, mut path) {
			return true
		}
	}
	path.pop()
	return false
}

fn (t &Transformer) comptime_reflection_decl_rhs(id flat.NodeId, name string) ?flat.NodeId {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	// This helper only reads AST nodes while the borrow is live.
	node := unsafe { &t.a.nodes[int(id)] }
	if node.kind != .decl_assign || node.children_count < 2 {
		return none
	}
	for i := 0; i + 1 < int(node.children_count); i += 2 {
		lhs := t.a.child_node(node, i)
		if lhs.kind == .ident && lhs.value == name {
			return t.a.child(node, i + 1)
		}
	}
	return none
}

fn (mut t Transformer) comptime_node_raw_attributes(node_id int) []string {
	return t.comptime_node_raw_attribute_data(node_id).attrs
}

// Reflection indexes store node ids, so AST growth cannot invalidate their entries.
// Each worker owns its indexes and metadata; scoped workers must not retain another
// worker's scratch allocations. Generic signatures are finalized before body
// reflection, and source attribute directives are immutable during lowering.
fn (mut t Transformer) ensure_comptime_method_index() {
	if t.comptime_method_index_nodes > t.a.nodes.len {
		t.comptime_method_decls.clear()
		t.comptime_raw_attribute_nodes.clear()
		t.comptime_method_metas_cache.clear()
		t.comptime_method_index_nodes = 0
		t.comptime_method_index_module = ''
		t.comptime_method_index_file = ''
		t.comptime_attribute_schema_state = unsafe { nil }
	}
	mut changed := false
	for node_id in t.comptime_method_index_nodes .. t.a.nodes.len {
		// Index construction only reads the AST and cannot invalidate this borrow.
		node := unsafe { &t.a.nodes[node_id] }
		if node.kind == .file {
			t.comptime_method_index_module = ''
			t.comptime_method_index_file = node.value
		} else if node.kind == .module_decl {
			t.comptime_method_index_module = node.value
		} else if node.kind == .directive && node.value.starts_with('@attributes:') {
			target := node.value['@attributes:'.len..].int()
			if node.value == '@attributes:${target}' && target !in t.comptime_raw_attribute_nodes {
				t.comptime_raw_attribute_nodes[target] = node_id
				changed = true
				if target >= 0 && target < t.a.nodes.len && t.a.nodes[target].kind == .struct_decl {
					for attr in node.generic_params() {
						if !comptime_attribute_piece_named(attr, 'attribute') {
							continue
						}
						if isnil(t.comptime_attribute_schema_state) {
							t.comptime_attribute_schema_state = &ComptimeAttributeSchemaState{}
						}
						t.comptime_attribute_schema_state.decls << ComptimeAttributeSchemaDecl{
							node_id:     target
							module_name: t.comptime_method_index_module
						}
						break
					}
				}
			}
		} else if node.kind == .fn_decl && node.value.contains('.') && node.children_count > 0 {
			first := t.a.child_node(node, 0)
			if first.kind == .param && first.op == .dot && first.value.len > 0 {
				receiver := comptime_method_receiver_name(first.typ, t.comptime_method_index_module)
				t.comptime_method_decls[receiver] << ComptimeMethodDecl{
					id:          node_id
					module_name: t.comptime_method_index_module
					file_name:   t.comptime_method_index_file
				}
				changed = true
			}
		}
	}
	t.comptime_method_index_nodes = t.a.nodes.len
	if changed {
		t.comptime_method_metas_cache.clear()
	}
}

fn (mut t Transformer) comptime_node_raw_attribute_data(node_id int) RawAttributeData {
	t.ensure_comptime_method_index()
	attribute_id := t.comptime_raw_attribute_nodes[node_id] or { return RawAttributeData{} }
	node := t.a.node(flat.NodeId(attribute_id))
	return RawAttributeData{
		attrs: node.generic_params().clone()
		kinds: if node.typ != 0 {
			node.typ_text().split(',').map(it.int())
		} else {
			[]int{}
		}
	}
}

fn comptime_attribute_metas_from_raw(raw_attrs []string, raw_kinds []int) []AttributeMeta {
	mut attrs := []AttributeMeta{}
	for attr_idx, raw in raw_attrs {
		clean := trimmed_transform_text(raw)
		if clean.len == 0 {
			continue
		}
		recorded_kind := if attr_idx < raw_kinds.len { raw_kinds[attr_idx] } else { -1 }
		whole_string := recorded_kind >= 0 && recorded_kind & flat.attribute_kind_no_arg != 0
		value_kind := if recorded_kind >= 0 { recorded_kind & flat.attribute_kind_mask } else { -1 }
		colon := clean.index_u8(`:`)
		has_arg := colon >= 0 && !whole_string
		if has_arg {
			idx := colon
			name := trimmed_transform_text(clean[..idx])
			raw_arg := trimmed_transform_text(clean[idx + 1..])
			arg := comptime_attr_unquote(raw_arg)
			kind := if value_kind >= 0 {
				value_kind
			} else if raw_arg.len >= 2 && raw_arg[0] in [`'`, `\"`] {
				1
			} else if raw_arg == 'true' || raw_arg == 'false' {
				3
			} else if comptime_is_int(raw_arg) {
				2
			} else {
				0
			}
			attrs << AttributeMeta{
				name:    name
				arg:     arg
				has_arg: true
				kind:    kind
			}
		} else {
			name := if whole_string {
				if recorded_kind & flat.attribute_kind_raw_string != 0 {
					comptime_attr_unquote(clean)
				} else {
					comptime_cond_unescape(clean)
				}
			} else if value_kind == 1 {
				if comptime_attr_is_string_literal(clean) {
					comptime_attr_unquote(clean)
				} else {
					comptime_cond_unescape(clean)
				}
			} else {
				clean
			}
			attrs << AttributeMeta{
				name: name
				kind: if value_kind >= 0 { value_kind } else { 0 }
			}
		}
	}
	return attrs
}

fn comptime_attr_is_string_literal(raw string) bool {
	return (raw.len >= 2 && raw[0] in [`'`, `"`] && raw[raw.len - 1] == raw[0])
		|| (raw.len >= 3 && raw[0] == `r` && raw[1] in [`'`, `"`] && raw[raw.len - 1] == raw[1])
}

fn (mut t Transformer) comptime_node_attribute_metas(node_id int) []AttributeMeta {
	data := t.comptime_node_raw_attribute_data(node_id)
	return comptime_attribute_metas_from_raw(data.attrs, data.kinds)
}

fn comptime_attr_unquote(s string) string {
	if s.len >= 3 && s[0] == `r` && s[1] in [`'`, `\"`] && s[s.len - 1] == s[1] {
		return s[2..s.len - 1]
	}
	if s.len >= 2 && s[0] in [`'`, `\"`] && s[s.len - 1] == s[0] {
		return comptime_cond_unescape(s[1..s.len - 1])
	}
	return s
}

fn comptime_attr_display(raw string) string {
	clean := trimmed_transform_text(raw)
	colon := clean.index_u8(`:`)
	if colon < 0 {
		// Whole-string attributes are stored without their quotes. Decode their source spelling
		// before exposing it through the legacy []string metadata.
		return comptime_cond_unescape(clean)
	}
	raw_arg := trimmed_transform_text(clean[colon + 1..])
	if !comptime_attr_is_string_literal(raw_arg) || raw_arg[0] == `r` {
		return clean
	}
	quote := raw_arg[0].ascii_str()
	decoded := comptime_attr_unquote(raw_arg)
	return '${clean[..colon + 1]} ${quote}${decoded}${quote}'
}

fn (mut t Transformer) clone_attribute_subst(id flat.NodeId, var_name string, attr AttributeMeta, schemas []AttributeSchemaInfo) flat.NodeId {
	return t.clone_attribute_subst_scoped(id, var_name, attr, []string{}, schemas)
}

fn (mut t Transformer) clone_attribute_subst_scoped(id flat.NodeId, var_name string, attr AttributeMeta, inner_vars []string, schemas []AttributeSchemaInfo) flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	if comptime_for_declares_var(node, var_name) {
		return t.clone_node_preserving_children(node)
	}
	if node.kind == .comptime_if {
		narrowing := t.comptime_attribute_cond_narrowing(node.value, var_name, attr, schemas)
		cond := if narrowing.decided {
			narrowing.value.str()
		} else if narrowing.then_branch || narrowing.else_branch {
			// The `is` test that narrows the instance also guards the rest of
			// the condition (`attr is Route && attr.path == '/'`).
			t.subst_attribute_cond(node.value, var_name, attr.narrowed(), schemas)
		} else {
			t.subst_attribute_cond(node.value, var_name, attr, schemas)
		}
		if comptime_cond_references_ident(node.value, var_name)
			&& !comptime_cond_has_any_loop_member_ref(cond, inner_vars) {
			if !comptime_cond_has_loop_member_ref(cond, var_name) {
				if taken := t.eval_reflected_string_condition(cond, node) {
					branch_idx := if taken { 0 } else { 1 }
					if branch_idx >= int(node.children_count) {
						return t.make_block([]flat.NodeId{})
					}
					narrowed := if taken { narrowing.then_branch } else { narrowing.else_branch }
					branch_attr := if narrowed { attr.narrowed() } else { attr }
					return t.clone_attribute_subst_scoped(t.a.child(&node, branch_idx), var_name, branch_attr, inner_vars, schemas)
				}
			}
			if !t.comptime_condition_needs_local_value(cond, node) {
				t.reject_unevaluated_comptime_if(id, node, cond)
			}
		}
		return t.clone_attribute_subst_children_with_value(node, var_name, attr, inner_vars, cond, schemas)
	}
	if node.kind == .ident && node.value == var_name {
		// The checker types the loop variable as `VAttribute`, so a value use
		// (`r := attr`, `f(attr)`, `a << attr`) is that view even for a schema
		// use; schema fields are read through `attr.field`.
		return t.make_attribute_literal(attr)
	}
	if node.kind == .selector && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind == .ident && base.value == var_name {
			// Inside `$if attr is Schema`, the schema's own fields win over the
			// `VAttribute` members of the same name.
			if attr.schema_narrowed {
				if field_id := t.clone_attribute_schema_field(node.value, attr) {
					return field_id
				}
			}
			return match node.value {
				'name' { t.make_string_literal(attr.name) }
				'arg' { t.make_string_literal(attr.arg) }
				'has_arg' { t.make_bool_literal(attr.has_arg) }
				'kind' {
					t.make_int_literal_typed(attr.kind.str(), flat.source_type_id('AttributeKind'))
				}
				else {
					if field_id := t.clone_attribute_schema_field(node.value, attr) {
						field_id
					} else {
						t.clone_attribute_subst_children(node, var_name, attr, inner_vars, schemas)
					}
				}
			}
		}
	}
	return t.clone_attribute_subst_children(node, var_name, attr, inner_vars, schemas)
}

fn (t &Transformer) subst_attribute_cond(cond string, var_name string, attr AttributeMeta, schemas []AttributeSchemaInfo) string {
	mut result := subst_attribute_schema_field_cond(cond, var_name, attr)
	result = comptime_cond_replace_unquoted(result, '${var_name}.has_arg', attr.has_arg.str())
	result = comptime_cond_replace_unquoted(result, '${var_name}.name', comptime_cond_string_literal(attr.name))
	result = comptime_cond_replace_unquoted(result, '${var_name}.arg', comptime_cond_string_literal(attr.arg))
	kind_value := comptime_attribute_kind_cond_value(attr.kind)
	result = comptime_cond_replace_unquoted(result, '${var_name}.kind ==.', '${kind_value} == .')
	result = comptime_cond_replace_unquoted(result, '${var_name}.kind !=.', '${kind_value} != .')
	result = comptime_cond_replace_unquoted(result, '${var_name}.kind', kind_value)
	if schemas.len > 0 {
		result = t.subst_attribute_is_cond(result, var_name, attr, schemas)
	}
	return result
}

fn comptime_cond_string_literal(value string) string {
	mut out := strings.new_builder(value.len + 2)
	out.write_u8(`'`)
	for i := 0; i < value.len; i++ {
		if value[i] == `\\` || value[i] == `'` {
			out.write_u8(`\\`)
		}
		out.write_u8(value[i])
	}
	out.write_u8(`'`)
	return out.str()
}

fn comptime_attribute_kind_cond_value(kind int) string {
	return match kind {
		1 { '.string' }
		2 { '.number' }
		3 { '.bool' }
		4 { '.comptime_define' }
		else { '.plain' }
	}
}

// subst_attribute_is_cond folds per-iteration `$if attr is Schema` tests: the
// instance's own schema decides, so each unrolled copy keeps exactly its taken
// branch. A test against an unregistered name is left for the normal type
// evaluation, preserving today's behavior for non-schema types.
fn (t &Transformer) subst_attribute_is_cond(cond string, var_name string, attr AttributeMeta, schemas []AttributeSchemaInfo) string {
	if var_name == '' || !cond.contains(var_name) {
		return cond
	}
	mut out := strings.new_builder(cond.len)
	mut offset := 0
	for offset < cond.len {
		if cond[offset] == `'` || cond[offset] == `"` {
			end := comptime_cond_skip_string(cond, offset)
			out.write_string(cond[offset..end])
			offset = end
			continue
		}
		start := offset
		if start + var_name.len <= cond.len && cond[start..start + var_name.len] == var_name
			&& (start == 0 || !comptime_cond_name_char(cond[start - 1])) {
			after_var := start + var_name.len
			if after_var >= cond.len || (!comptime_cond_name_char(cond[after_var]) && cond[after_var] != `.`) {
				mut i := after_var
				for i < cond.len && cond[i] == ` ` {
					i++
				}
				mut negated := false
				if i < cond.len && cond[i] == `!` {
					negated = true
					i++
					for i < cond.len && cond[i] == ` ` {
						i++
					}
				}
				if cond[i..].starts_with('is')
					&& (i + 2 >= cond.len || (!comptime_cond_name_char(cond[i + 2]) && cond[i + 2] != `.`)) {
					i += 2
					for i < cond.len && cond[i] == ` ` {
						i++
					}
					rhs_start := i
					for i < cond.len && (comptime_cond_name_char(cond[i]) || cond[i] == `.`) {
						i++
					}
					if rhs_start < i
						&& (i >= cond.len || (!comptime_cond_name_char(cond[i]) && cond[i] != `.`)) {
						rhs := cond[rhs_start..i]
						if value := t.comptime_attribute_is_test_value(rhs, attr, schemas) {
							out.write_string(if negated == !value { 'true' } else { 'false' })
							offset = i
							continue
						}
					}
				}
			}
		}
		out.write_u8(cond[offset])
		offset++
	}
	return out.str()
}

// comptime_attribute_is_test_value decides `$if attr is Schema` for one
// attribute instance: true when the instance groups that schema, false when
// the name is a registered schema it does not group. It returns none for
// names no schema declares, so ordinary type tests keep working.
fn (t &Transformer) comptime_attribute_is_test_value(rhs string, attr AttributeMeta, schemas []AttributeSchemaInfo) ?bool {
	clean := rhs.trim_space()
	if clean.len == 0 || clean[0] in [`?`, `&`, `[`] {
		return none
	}
	for c in clean.bytes() {
		if !(c.is_letter() || c.is_digit() || c == `_` || c == `.`) {
			return none
		}
	}
	short := clean.all_after_last('.')
	qualifier := if clean.contains('.') { clean.all_before_last('.') } else { '' }
	mut target_module := ''
	mut known := false
	for schema in schemas {
		if schema.name != short {
			continue
		}
		if qualifier.len > 0 {
			if schema.module == qualifier {
				known = true
				target_module = schema.module
			}
			continue
		}
		if comptime_attribute_modules_compatible(schema.module, t.cur_module) {
			known = true
			if target_module == '' {
				target_module = schema.module
			}
		}
	}
	if !known {
		return none
	}
	if isnil(attr.schema) {
		return false
	}
	if attr.schema.name != short {
		return false
	}
	if qualifier.len > 0 {
		return attr.schema.module == target_module
	}
	return comptime_attribute_modules_compatible(attr.schema.module, t.cur_module)
}

// clone_attribute_schema_field lowers `attr.field` for a grouped schema use to
// the bound argument literal. It returns none for legacy pieces and unknown
// members, preserving the `VAttribute` path.
fn (mut t Transformer) clone_attribute_schema_field(member string, attr AttributeMeta) ?flat.NodeId {
	if isnil(attr.schema) {
		return none
	}
	for i, field in attr.schema.fields {
		if field.name == member {
			return t.make_attribute_schema_field_literal(field, attr.schema.values[i])
		}
	}
	return none
}

// make_attribute_schema_field_literal builds the value bound to one schema
// field: strings unquote, integers keep their spelling, bools fold, and enum
// shorthands (`.get`) become values of the field's enum. Strings and integers
// are cast to the field's declared type (`uint`, `rune`, an alias), so that
// `x := attr.field` has the type of the field.
fn (mut t Transformer) make_attribute_schema_field_literal(field AttributeSchemaFieldInfo, raw string) flat.NodeId {
	return match field.kind {
		.string_ {
			literal := t.make_string_literal(comptime_attr_unquote(raw))
			if field.type_id == flat.source_type_string {
				literal
			} else {
				t.make_cast(field.type_id, literal, field.type_id)
			}
		}
		.boolean {
			literal := t.make_bool_literal(raw == 'true')
			if field.type_id == flat.source_type_bool {
				literal
			} else {
				t.make_cast(field.type_id, literal, field.type_id)
			}
		}
		.integer {
			literal := t.make_int_literal_typed(raw, field.type_id)
			if field.type_id == flat.source_type_int {
				literal
			} else {
				t.make_cast(field.type_id, literal, field.type_id)
			}
		}
		.enum_ {
			t.a.add_node(flat.Node{
				kind:  .enum_val
				value: '${flat.source_type_name(field.type_id)}.${raw[1..]}'
				typ:   field.type_id
			})
		}
	}
}

// subst_attribute_schema_field_cond replaces the bound fields of a schema use
// in a `$if` condition. A field named like a `VAttribute` member (`name`,
// `arg`, `has_arg`, `kind`) is replaced only where the use is narrowed;
// elsewhere that name keeps meaning the `VAttribute` member.
fn subst_attribute_schema_field_cond(cond string, var_name string, attr AttributeMeta) string {
	if isnil(attr.schema) || !cond.contains('${var_name}.') {
		return cond
	}
	mut result := cond
	for i, field in attr.schema.fields {
		if !attr.schema_narrowed && field.name in ['name', 'arg', 'has_arg', 'kind'] {
			continue
		}
		member := '${var_name}.${field.name}'
		value := comptime_attribute_schema_field_cond_value(field, attr.schema.values[i])
		if field.kind == .enum_ {
			// Conditions are stored without the space before an enum shorthand.
			result = comptime_cond_replace_unquoted(result, '${member} ==.', '${value} == .')
			result = comptime_cond_replace_unquoted(result, '${member} !=.', '${value} != .')
		}
		result = comptime_cond_replace_unquoted(result, member, value)
	}
	return result
}

// comptime_attribute_schema_field_cond_value is the condition text of a bound
// schema field value. Integers are written in decimal, so that `0x10` matches
// a `16` in the condition.
fn comptime_attribute_schema_field_cond_value(field AttributeSchemaFieldInfo, raw string) string {
	return match field.kind {
		.string_ {
			comptime_cond_string_literal(comptime_attr_unquote(raw))
		}
		.integer {
			if value := strconv.parse_int(raw, 0, 64) {
				value.str()
			} else if value := strconv.parse_uint(raw, 0, 64) {
				value.str()
			} else {
				raw
			}
		}
		.boolean, .enum_ {
			raw
		}
	}
}

// narrowed returns `attr` as seen inside a `$if attr is Schema` branch that
// was taken for it.
fn (attr AttributeMeta) narrowed() AttributeMeta {
	return AttributeMeta{
	...attr
		schema_narrowed: !isnil(attr.schema)
	}
}

// comptime_attribute_cond_narrowing reads the `is` tests among the top-level
// `&&` terms (or `||` terms) of a `$if` condition for one attribute instance.
// A false `is` conjunct decides the condition, as a true `!is` disjunct does,
// so later terms that read schema fields the instance lacks are never
// evaluated. A true `is` conjunct narrows the `$if` branch to the instance's
// schema; a false `!is` disjunct narrows the `$else` branch.
fn (t &Transformer) comptime_attribute_cond_narrowing(cond string, var_name string, attr AttributeMeta, schemas []AttributeSchemaInfo) AttributeCondNarrowing {
	if schemas.len == 0 || !cond.contains('is') {
		return AttributeCondNarrowing{}
	}
	clean := comptime_condition_strip_outer_parens(cond)
	is_disjunction := comptime_condition_top_level_index(clean, '||') >= 0
	terms := comptime_condition_top_level_terms(clean, if is_disjunction { '||' } else { '&&' })
	mut result := AttributeCondNarrowing{}
	for term in terms {
		negated, rhs := comptime_attribute_is_term(term, var_name) or { continue }
		is_schema := t.comptime_attribute_is_test_value(rhs, attr, schemas) or { continue }
		term_value := is_schema != negated
		if is_disjunction {
			if term_value {
				return AttributeCondNarrowing{
					decided:     true
					value:       true
					then_branch: !negated
				}
			}
			if negated {
				result.else_branch = true
			}
		} else {
			if !term_value {
				return AttributeCondNarrowing{
					decided:     true
					value:       false
					else_branch: terms.len == 1 && negated
				}
			}
			if !negated {
				result.then_branch = true
			}
		}
	}
	return result
}

// comptime_condition_top_level_terms splits `cond` at each top-level `op`.
fn comptime_condition_top_level_terms(cond string, op string) []string {
	mut terms := []string{}
	mut rest := cond
	for {
		idx := comptime_condition_top_level_index(rest, op)
		if idx < 0 {
			break
		}
		terms << rest[..idx]
		rest = rest[idx + op.len..]
	}
	terms << rest
	return terms
}

// comptime_attribute_is_term parses a whole condition term `var is Name` or
// `var !is Name`, returning whether it is negated and the tested name.
fn comptime_attribute_is_term(term string, var_name string) ?(bool, string) {
	clean := comptime_condition_strip_outer_parens(term)
	if var_name == '' || !clean.starts_with(var_name) {
		return none
	}
	mut i := var_name.len
	if i < clean.len && (comptime_cond_name_char(clean[i]) || clean[i] == `.`) {
		return none
	}
	for i < clean.len && clean[i] == ` ` {
		i++
	}
	mut negated := false
	if i < clean.len && clean[i] == `!` {
		negated = true
		i++
	}
	if !clean[i..].starts_with('is ') {
		return none
	}
	rhs := clean[i + 3..].trim_space()
	if rhs.len == 0 {
		return none
	}
	for c in rhs {
		if !comptime_cond_name_char(c) && c != `.` {
			return none
		}
	}
	return negated, rhs
}

fn (mut t Transformer) clone_attribute_subst_children(node flat.Node, var_name string, attr AttributeMeta, inner_vars []string, schemas []AttributeSchemaInfo) flat.NodeId {
	return t.clone_attribute_subst_children_with_value(node, var_name, attr, inner_vars, node.value, schemas)
}

fn (mut t Transformer) clone_attribute_subst_children_with_value(node flat.Node, var_name string, attr AttributeMeta, inner_vars []string, value string, schemas []AttributeSchemaInfo) flat.NodeId {
	child_inner_vars := comptime_nested_loop_vars(node, var_name, inner_vars)
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if node.kind == .call && i == 0 {
			child := t.a.nodes[int(child_id)]
			if child.kind == .selector && child.children_count > 0 {
				receiver := t.a.child_node(&child, 0)
				if receiver.kind == .ident && receiver.value == var_name {
					// A method callee keeps the VAttribute receiver even when a
					// schema field has the same name as the method.
					children << t.clone_attribute_subst_children(child, var_name, attr, child_inner_vars, schemas)
					continue
				}
			}
		}
		children << t.clone_attribute_subst_scoped(child_id, var_name, attr, child_inner_vars, schemas)
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	result := t.a.add_node(flat.Node{
		kind:           node.kind
		op:             node.op
		pos:            node.pos
		value:          value
		source_value:   node.source_value
		typ:            node.typ
		payload:        if node.payload != 0 { node.clone_owned().payload } else { u32(0) }
		is_mut:         node.is_mut
		flags:          node.flags & comptime_clone_syntax_flags
		children_start: start
		children_count: flat.child_count(children.len)
	})
	if t.specialization_node_start >= 0 && node.kind == .decl_assign && children.len >= 2 {
		lhs := t.a.nodes[int(children[0])]
		if lhs.kind == .ident && lhs.value.len > 0 {
			t.specialization_decl_nodes_by_name[lhs.value] << int(result)
		}
	}
	return result
}

fn (mut t Transformer) make_attribute_literal(attr AttributeMeta) flat.NodeId {
	fields := [
		t.make_named_field_init('name', t.make_string_literal(attr.name), flat.source_type_string),
		t.make_named_field_init('has_arg', t.make_bool_literal(attr.has_arg), flat.source_type_bool),
		t.make_named_field_init('arg', t.make_string_literal(attr.arg), flat.source_type_string),
		t.make_named_field_init('kind', t.make_int_literal_typed(attr.kind.str(), flat.source_type_id('AttributeKind')), flat.source_type_id('AttributeKind')),
	]
	start := t.a.children.len
	for field in fields {
		t.a.children << field
	}
	literal_type_id := flat.source_type_id('VAttribute')
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		source_value:   literal_type_id
		typ:            literal_type_id
		children_start: start
		children_count: flat.child_count(fields.len)
	})
}

fn (mut t Transformer) expand_comptime_for_params(var_name string, fn_name string, body_stmts []flat.NodeId, loop_id flat.NodeId) []flat.NodeId {
	mut out := []flat.NodeId{}
	source := t.comptime_reflection_source(fn_name, loop_id)
	for param in t.comptime_param_metas(source) {
		mut cloned := []flat.NodeId{cap: body_stmts.len}
		for sid in body_stmts {
			if cid := t.clone_param_subst(sid, var_name, param) {
				cloned << cid
			}
		}
		out << t.make_block(t.transform_scope_stmts(cloned))
	}
	return out
}

fn (t &Transformer) comptime_param_metas(fn_name string) []ParamMeta {
	raw_wanted := trimmed_transform_text(fn_name)
	wanted := t.comptime_resolve_selective_import_reflection_source(raw_wanted)
	wanted_syntax := flat.source_type_syntax(flat.source_type_id(wanted))
	if params := t.comptime_reflected_params[wanted] {
		return params.clone()
	}
	mut module_name := ''
	mut file_name := ''
	mut signature_fallback := []ParamMeta{}
	for node_id in 0 .. t.a.nodes.len {
		// This read-only scan keeps the borrowed AST node valid.
		node := unsafe { &t.a.nodes[node_id] }
		if node.kind == .file {
			module_name = ''
			file_name = node.value
			continue
		}
		if node.kind == .module_decl {
			module_name = node.value
			continue
		}
		if node.kind != .fn_decl {
			continue
		}
		qualified := if module_name.len > 0 && module_name !in ['main', 'builtin'] {
			'${module_name}.${node.value}'
		} else {
			node.value
		}
		is_scoped_match := qualified == wanted
			|| (module_name == t.cur_module && node.value == wanted)
		mut params := []ParamMeta{}
		for i in 0 .. node.children_count {
			param := t.a.child_node(node, i)
			if param.kind != .param {
				if t.prefix_param_scan {
					break
				}
				continue
			}
			if i == 0 && param.op == .dot {
				continue
			}
			params << ParamMeta{
				name:        param.value
				typ:         param.typ
				module_name: module_name
			}
		}
		if is_scoped_match {
			return params
		}
		if wanted_syntax.function_closed() {
			if !comptime_params_match_signature(params, node.typ, wanted_syntax.function_params(), wanted_syntax.function_result()) {
				continue
			}
			if file_name != t.cur_file {
				if signature_fallback.len == 0 {
					signature_fallback = params.clone()
				}
				continue
			}
		} else {
			continue
		}
		return params
	}
	return signature_fallback
}

fn comptime_params_match_signature(params []ParamMeta, return_type_id u32, wanted_params []u32, wanted_ret u32) bool {
	if params.len != wanted_params.len { return false }
	for i, param in params {
		if transform_trimmed_type_id(param.typ) != flat.source_type_syntax(wanted_params[i]).generic_parameter_payload() {
			return false
		}
	}
	actual_ret := if return_type_id != 0 { return_type_id } else { flat.source_type_void }
	expected_ret := if wanted_ret != 0 { wanted_ret } else { flat.source_type_void }
	return actual_ret == expected_ret
}

fn (mut t Transformer) clone_param_subst(id flat.NodeId, var_name string, param ParamMeta) ?flat.NodeId {
	return t.clone_param_subst_scoped(id, var_name, param, []string{})
}

fn (mut t Transformer) clone_param_subst_scoped(id flat.NodeId, var_name string, param ParamMeta, inner_vars []string) ?flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	if comptime_for_declares_var(node, var_name) {
		return t.clone_node_preserving_children(node)
	}
	if node.kind == .ident && node.value == var_name {
		return t.make_param_data_literal(param)
	}
	if node.kind == .selector && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind == .ident && base.value == var_name {
			return match node.value {
				'name' {
					t.make_string_literal(param.name)
				}
				'typ' {
					t.make_int_literal(t.type_index(param.typ, param.module_name))
				}
				else {
					t.clone_param_subst_children(node, var_name, param, inner_vars)
				}
			}
		}
		if base.kind == .typeof_expr && t.typeof_arg_is_param_typ(t.a.child(&node, 0), var_name) {
			return match node.value {
				'name' {
					t.make_string_literal(param.typ_text())
				}
				'idx' {
					t.make_int_literal(t.type_index(param.typ, param.module_name))
				}
				else {
					t.clone_param_subst_children(node, var_name, param, inner_vars)
				}
			}
		}
	}
	if node.kind == .typeof_expr && t.typeof_arg_is_param_typ(id, var_name) {
		return t.make_string_literal(param.typ_text())
	}
	if node.kind == .comptime_if {
		param_typ := if param.typ == transform_identity_void_ref {
			'voidptr'
		} else {
			flat.source_type_name(t.comptime_field_type_id_key(param.typ, param.module_name))
		}
		mut cond := comptime_cond_replace_int_compared(node.value, '${var_name}.typ', t.type_index(param.typ, param.module_name).str())
		cond = comptime_cond_replace_unquoted(cond, '${var_name}.typ', param_typ)
		cond = comptime_cond_replace_unquoted(cond, '${var_name}.name', "'${param.name}'")
		cond = comptime_cond_replace_unquoted(cond, ' is &void', ' is voidptr')
		cond = comptime_cond_replace_unquoted(cond, ' !is &void', ' !is voidptr')
		if comptime_cond_references_ident(node.value, var_name)
			&& !comptime_cond_has_any_loop_member_ref(cond, inner_vars) {
			if !comptime_cond_has_loop_member_ref(cond, var_name) {
				if taken := t.eval_reflected_string_condition(cond, node) {
					branch_idx := if taken { 0 } else { 1 }
					if branch_idx >= int(node.children_count) {
						return none
					}
					return t.clone_param_subst_scoped(t.a.child(&node, branch_idx), var_name, param, inner_vars)
				}
			}
			if !t.comptime_condition_needs_local_value(cond, node) {
				t.reject_unevaluated_comptime_if(id, node, cond)
			}
		}
		return t.clone_param_subst_children_with_value(node, var_name, param, inner_vars, cond)
	}
	return t.clone_param_subst_children(node, var_name, param, inner_vars)
}

fn (t &Transformer) typeof_arg_is_param_typ(id flat.NodeId, var_name string) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.node(id)
	if node.kind != .typeof_expr || node.children_count == 0 {
		return false
	}
	return t.param_typ_expr_references(t.a.child(node, 0), var_name)
}

fn (t &Transformer) param_typ_expr_references(id flat.NodeId, var_name string) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.node(id)
	if node.kind == .selector && node.value == 'typ' && node.children_count > 0 {
		base := t.a.child_node(node, 0)
		if base.kind == .ident && base.value == var_name {
			return true
		}
	}
	for i in 0 .. node.children_count {
		if t.param_typ_expr_references(t.a.child(node, i), var_name) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) clone_param_subst_children(node flat.Node, var_name string, param ParamMeta, inner_vars []string) flat.NodeId {
	return t.clone_param_subst_children_with_value(node, var_name, param, inner_vars, node.value)
}

fn (mut t Transformer) clone_param_subst_children_with_value(node flat.Node, var_name string, param ParamMeta, inner_vars []string, value string) flat.NodeId {
	child_inner_vars := comptime_nested_loop_vars(node, var_name, inner_vars)
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		if child := t.clone_param_subst_scoped(t.a.child(&node, i), var_name, param, child_inner_vars) {
			children << child
		}
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	return t.a.add_node(flat.Node{
		kind:           node.kind
		op:             node.op
		pos:            node.pos
		value:          value
		source_value:   node.source_value
		typ:            node.typ
		payload:        if node.payload != 0 { node.clone_owned().payload } else { u32(0) }
		is_mut:         node.is_mut
		flags:          node.flags & comptime_clone_syntax_flags
		children_start: start
		children_count: flat.child_count(children.len)
	})
}

fn (mut t Transformer) make_param_data_literal(param ParamMeta) flat.NodeId {
	return t.make_param_data_literal_in_module(param, param.module_name)
}

fn (mut t Transformer) make_param_data_literal_in_module(param ParamMeta, module_name string) flat.NodeId {
	name_field := t.make_named_field_init('name', t.make_string_literal(param.name), flat.source_type_string)
	typ_field := t.make_named_field_init('typ', t.make_int_literal(t.type_index(param.typ, module_name)), flat.source_type_int)
	start := t.a.children.len
	t.a.children << name_field
	t.a.children << typ_field
	literal_type_id := flat.source_type_id('FunctionParam')
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		source_value:   literal_type_id
		typ:            literal_type_id
		children_start: start
		children_count: 2
	})
}

// make_method_data_literal builds a runtime `FunctionData` value for a bare methods loop variable.
fn (mut t Transformer) make_method_data_literal(method MethodMeta) flat.NodeId {
	fields := [
		t.make_named_field_init('name', t.make_string_literal(method.name), flat.source_type_string),
		t.make_named_field_init('location', t.make_string_literal(method.location), flat.source_type_string),
		t.make_named_field_init('attrs', t.make_string_array_literal(method.attrs), flat.source_type_id('[]string')),
		t.make_named_field_init('attributes', t.make_attribute_array_literal(method.attributes), flat.source_type_id('[]VAttribute')),
		t.make_named_field_init('args', t.make_param_array_literal(method.params, method.module_name), flat.source_type_id('[]FunctionParam')),
		t.make_named_field_init('return_type', t.make_int_literal(t.type_index(method.return_type, method.module_name)), flat.source_type_int),
		t.make_named_field_init('typ', t.make_int_literal(t.comptime_method_type_id(method)), flat.source_type_int),
	]
	start := t.a.children.len
	for field in fields {
		t.a.children << field
	}
	literal_type_id := flat.source_type_id('FunctionData')
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		source_value:   literal_type_id
		typ:            literal_type_id
		children_start: start
		children_count: flat.child_count(fields.len)
	})
}

// comptime_method_escape_stmts gives the escape prepass the concrete calls that
// method reflection will emit, without lowering statements or changing the source loop.
fn (mut t Transformer) comptime_method_escape_stmts(node flat.Node) ?[]flat.NodeId {
	var_name, kind := comptime_for_parts(node.value)
	if kind != 'methods' || node.children_count == 0 || t.generic_arg_is_unresolved(node.typ) {
		return none
	}
	base_type_id := if node.payload == flat.canonical_comptime_type_payload {
		node.typ
	} else {
		source_id := t.comptime_for_value_source_type(node.typ) or { node.typ }
		if source_id != node.typ && t.type_name_is_declared_id(t.comptime_for_value_type_base(source_id)) {
			source_id
		} else {
			t.comptime_resolve_selective_import_type(source_id)
		}
	}
	if t.generic_arg_is_unresolved(base_type_id) {
		return none
	}
	body := *t.a.child_node(&node, 0)
	body_stmts := t.a.children_of(&body).clone()
	mut result := []flat.NodeId{}
	for method in t.comptime_method_metas(base_type_id) {
		mut cloned := []flat.NodeId{cap: body_stmts.len}
		for sid in body_stmts {
			if cid := t.clone_method_subst(sid, var_name, method) {
				cloned << cid
			}
		}
		result << t.make_block(cloned)
	}
	return result
}

fn (mut t Transformer) expand_comptime_for_methods(var_name string, base_type_id u32, body_stmts []flat.NodeId) []flat.NodeId {
	mut out := []flat.NodeId{}
	for method in t.comptime_method_metas(base_type_id) {
		mut cloned := []flat.NodeId{cap: body_stmts.len}
		for sid in body_stmts {
			if cid := t.clone_method_subst(sid, var_name, method) {
				cloned << cid
			}
		}
		out << t.make_block(t.transform_scope_stmts(cloned))
	}
	return out
}

fn comptime_method_receiver_base(raw_id u32) u32 {
	name_id := comptime_method_receiver_type(raw_id)
	return flat.source_type_syntax(name_id).generic_base()
}

fn comptime_method_receiver_type(raw_id u32) u32 {
	mut name_id := transform_trimmed_type_id(raw_id)
	if flat.source_type_shape(name_id).has(.mutable_type) {
		name_id = transform_trimmed_type_id(flat.source_type_syntax(name_id).raw_base())
	}
	return flat.source_type_syntax(name_id).pointer_base()
}

fn comptime_method_receiver_name(raw_id u32, module_name string) u32 {
	name_id := comptime_method_receiver_base(raw_id)
	if name_id == 0 { return 0 }
	shape := flat.source_type_shape(name_id)
	if !shape.has(.dotted) && module_name in ['', 'main', 'builtin'] { return name_id }
	if flat.source_type_query(name_id).has(.main_prefix) {
		return flat.source_type_syntax(name_id).main_base()
	}
	if shape.has(.dotted) || module_name in ['', 'main', 'builtin'] { return name_id }
	return flat.source_type_composition_two(.qualified, flat.source_type_id(module_name), name_id)
}

fn comptime_source_line_offsets(path string) []int {
	source := os.read_file(path) or { return []int{} }
	mut offsets := []int{cap: source.len / 40 + 1}
	offsets << 0
	for i, ch in source {
		if ch == `\n` {
			offsets << i + 1
		}
	}
	return offsets
}

// comptime_source_location returns the `file:line:column` of a declaration, following
// the `#line` directives of its source `file`, when it has any.
fn comptime_source_location(path string, encoded_offset int, line_offsets []int, file &token.File) string {
	if path == '' || encoded_offset <= 0 || line_offsets.len == 0 {
		return ''
	}
	offset := encoded_offset - 1
	mut lo := 0
	mut hi := line_offsets.len
	for lo < hi {
		mid := (lo + hi) / 2
		if line_offsets[mid] <= offset {
			lo = mid + 1
		} else {
			hi = mid
		}
	}
	line_index := if lo > 0 { lo - 1 } else { 0 }
	column := offset - line_offsets[line_index] + 1
	if !isnil(file) && file.has_line_directives() {
		logical_file, logical_line := file.logical_line(line_index + 1)
		return '${logical_file}:${logical_line}:${column}'
	}
	return '${path}:${line_index + 1}:${column}'
}

fn comptime_method_receiver_matches(receiver_id u32, requested_id u32, normalized_id u32, receiver_module string, requested_module string) bool {
	receiver_name := comptime_method_receiver_name(receiver_id, receiver_module)
	for candidate_id in [requested_id, normalized_id] {
		if receiver_name == comptime_method_receiver_name(candidate_id, requested_module) {
			return true
		}
	}
	return false
}

fn (t &Transformer) comptime_method_source_type(raw_id u32) u32 {
	clean_id := comptime_method_receiver_type(raw_id)
	if source_id := t.generic_specialized_source_type_name(clean_id) {
		return source_id
	}
	if current_args_ids := t.recorded_generic_specialization_args(t.cur_fn_name) {
		for arg_id in current_args_ids {
			syntax := flat.source_type_syntax(comptime_method_receiver_type(arg_id))
			if syntax.generic_app_valid() && generic_specialized_type_matches_flat_name(clean_id, flat.source_type_name(syntax.generic_app_base()), syntax.generic_app_args()) {
				return arg_id
			}
		}
	}
	return raw_id
}

fn (mut t Transformer) comptime_method_metas(base_type_id u32) []MethodMeta {
	requested_source_id := t.comptime_method_source_type(base_type_id)
	normalized_id := t.comptime_normalize_type_alias_chain(requested_source_id)
	requested_module := t.comptime_method_requested_module(requested_source_id)
	t.ensure_comptime_method_index()
	cache_key := (u64(requested_source_id) << 32) | u64(normalized_id)
	context := '${t.cur_module}\x00${requested_module}'
	if contexts := t.comptime_method_metas_cache[cache_key] {
		if cached := contexts[context] {
			return cached
		}
	}
	mut decls := []ComptimeMethodDecl{}
	requested_receiver := comptime_method_receiver_name(requested_source_id, requested_module)
	normalized_receiver := comptime_method_receiver_name(normalized_id, requested_module)
	decls << t.comptime_method_decls[requested_receiver] or { []ComptimeMethodDecl{} }
	if normalized_receiver != requested_receiver {
		decls << t.comptime_method_decls[normalized_receiver] or { []ComptimeMethodDecl{} }
		// Alias and base methods retain their original declaration order.
		decls.sort(a.id < b.id)
	}
	mut methods := []MethodMeta{cap: decls.len}
	mut seen := map[string]bool{}
	for decl in decls {
		node := t.a.node(flat.NodeId(decl.id))
		if node.kind != .fn_decl || !node.value.contains('.') || node.children_count == 0 {
			continue
		}
		first := t.a.child_node(node, 0)
		if first.kind != .param || first.op != .dot || first.value.len == 0 {
			continue
		}
		name := node.value.all_after_last('.')
		if name.len == 0 || name in seen {
			continue
		}
		seen[name] = true
		generic_args_source_ids, generic_params := t.comptime_method_receiver_generic_args(first.typ, requested_source_id, normalized_id)

		mut params := []ParamMeta{}
		for i in 1 .. node.children_count {
			param := t.a.child_node(node, i)
			if param.kind != .param {
				if t.prefix_param_scan {
					break
				}
				continue
			}
			params << ParamMeta{
				name:        param.value
				typ:         substitute_generic_type_text_with_params(param.typ, generic_args_source_ids, generic_params)
				module_name: decl.module_name
			}
		}
		return_type := substitute_generic_type_text_with_params(if node.typ != 0 {
			node.typ
		} else {
			flat.source_type_void
		}, generic_args_source_ids, generic_params)
		raw_attr_data := t.comptime_node_raw_attribute_data(decl.id)
		if decl.file_name !in t.comptime_method_line_offsets {
			t.comptime_method_line_offsets[decl.file_name] = comptime_source_line_offsets(decl.file_name)
		}
		methods << MethodMeta{
			name:        name
			receiver:    first.typ
			module_name: decl.module_name
			location:    comptime_source_location(decl.file_name, node.pos.offset, t.comptime_method_line_offsets[decl.file_name],
				t.a.source_files[node.pos.id] or { unsafe { nil } })
			return_type: return_type
			is_pub:      node.op == .arrow
			params:      params
			attrs:       raw_attr_data.attrs
			attributes:  comptime_attribute_metas_from_raw(raw_attr_data.attrs, raw_attr_data.kinds)
		}
	}
	t.comptime_method_metas_cache[cache_key][context] = methods
	return methods
}

fn (t &Transformer) generic_checker_struct_name_known(name_id u32) bool {
	if isnil(t.tc) { return false }
	if t.generic_type_indexes_ready && t.generic_checker_struct_index_count == t.tc.structs.len {
		return t.generic_checker_struct_names[name_id]
	}
	return name_id in t.tc.structs
}

fn (t &Transformer) comptime_method_requested_module(raw_id u32) string {
	name_id := comptime_method_receiver_base(raw_id)
	if flat.source_type_query(name_id).has(.main_prefix) { return 'main' }
	if flat.source_type_shape(name_id).has(.dotted) {
		return flat.source_type_name(flat.source_type_syntax(name_id).qualifier())
	}
	// Specializations retain caller-owned main types passed into imported bodies.
	if t.active_specialization_main_types[name_id] { return 'main' }
	if !isnil(t.tc) {
		local := if t.cur_module in ['', 'main', 'builtin'] {
			name_id
		} else {
			flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), name_id)
		}
		if t.generic_checker_struct_name_known(local) || t.generic_checker_sum_name_known(local) {
			return t.cur_module
		}
		if t.generic_checker_struct_name_known(name_id) || t.generic_checker_sum_name_known(name_id) {
			return 'main'
		}
	}
	return t.cur_module
}

fn (t &Transformer) comptime_method_receiver_generic_args(receiver_id u32, requested_id u32, normalized_id u32) ([]u32, []string) {
	receiver := flat.source_type_syntax(comptime_method_receiver_type(receiver_id))
	if !receiver.generic_app_valid() || receiver.generic_app_args().len == 0 {
		return []u32{}, []string{}
	}
	mut candidates := [requested_id, normalized_id]
	for candidate_id in [requested_id, normalized_id] {
		if source_id := t.generic_specialized_source_type_name(comptime_method_receiver_type(candidate_id)) {
			candidates << source_id
		}
	}
	for candidate_id in candidates {
		syntax := flat.source_type_syntax(comptime_method_receiver_type(candidate_id))
		if syntax.generic_app_valid() && syntax.generic_app_args().len == receiver.generic_app_args().len {
			if !t.generic_args_have_placeholders(syntax.generic_app_args()) {
				// These are declaration parameter identifiers consumed by substitution.
				return syntax.generic_app_args(), flat.TypeTextList{ ids: receiver.generic_app_args() }.names()
			}
		}
	}
	return []u32{}, []string{}
}

fn (mut t Transformer) clone_method_subst(id flat.NodeId, var_name string, method MethodMeta) ?flat.NodeId {
	return t.clone_method_subst_scoped(id, var_name, method, []string{})
}

fn (t &Transformer) comptime_method_call_arity_matches(node flat.Node, method MethodMeta) bool {
	args, has_spread := t.logical_call_arg_ids(node)
	if has_spread {
		return true
	}
	actual_count := args.len
	hidden_ctx_count := if t.method_has_implicit_veb_ctx(method) { 1 } else { 0 }
	max_count := method.params.len + hidden_ctx_count
	min_count := t.comptime_method_min_required_arg_count(method)
	if actual_count < min_count {
		return false
	}
	return (method.params.len > 0 && flat.source_type_shape(method.params[method.params.len - 1].typ).has(.variadic))
		|| actual_count <= max_count
}

fn (mut t Transformer) comptime_method_call_matches(node flat.Node, method MethodMeta) bool {
	if !t.comptime_method_call_arity_matches(node, method) {
		return false
	}
	if !t.method_has_implicit_veb_ctx(method) {
		return true
	}
	args, has_spread := t.logical_call_arg_ids(node)
	if has_spread {
		return true
	}
	// Match the checker's omission rule first: while the call does not exceed the
	// declared route arity, its first argument belongs to the first route param.
	ctx_omitted := args.len <= method.params.len
	if t.comptime_method_call_args_match_with_ctx(node, args, method, !ctx_omitted) {
		return true
	}
	// An explicit ctx can still be followed only by omittable route params. This
	// is the second valid interpretation of an otherwise incompatible omitted-ctx
	// binding, not a type-based override of the checker's arity decision.
	return ctx_omitted && t.comptime_method_call_args_match_with_ctx(node, args, method, true)
}

fn (mut t Transformer) comptime_method_call_args_match_with_ctx(node flat.Node, args []flat.NodeId, method MethodMeta, explicit_ctx bool) bool {
	mut route_start := 0
	if explicit_ctx {
		if args.len == 0 || !t.comptime_method_call_arg_matches_hidden_veb_ctx(args[0], method) {
			return false
		}
		route_start = 1
	}
	route_count := args.len - route_start
	min_count := t.comptime_method_min_required_arg_count(method)
	is_variadic := method.params.len > 0
		&& flat.source_type_shape(method.params[method.params.len - 1].typ).has(.variadic)
	if route_count < min_count || (!is_variadic && route_count > method.params.len) {
		return false
	}
	for route_idx in 0 .. route_count {
		param_idx := if route_idx < method.params.len { route_idx } else { method.params.len - 1 }
		if param_idx < 0 {
			continue
		}
		param := method.params[param_idx]
		decl_module := if param.module_name.len > 0 {
			param.module_name
		} else {
			method.module_name
		}
		mut expected := param.typ
		if is_variadic && param_idx == method.params.len - 1 && flat.source_type_shape(expected).has(.variadic) {
			expected = flat.source_type_syntax(expected).raw_base()
		}
		expected = t.qualify_generic_arg_for_decl_module(expected, decl_module)
		arg_id := args[route_start + route_idx]
		if t.a.node(arg_id).kind == .field_init {
			if !t.comptime_method_field_group_matches(node, arg_id, expected) {
				return false
			}
			continue
		}
		mut actual := t.specialized_expr_type_name(arg_id)
		if actual == flat.source_type_unknown {
			actual = t.a.node(arg_id).typ
		}
		if decl_module in ['', 'main'] {
			expected = type_text_without_main_locks(expected)
			actual = type_text_without_main_locks(actual)
		}
		if !t.resolved_receiver_arg_compatible(arg_id, actual, expected) {
			return false
		}
	}
	return true
}

fn (mut t Transformer) comptime_method_field_group_matches(node flat.Node, first_field_id flat.NodeId, expected_id u32) bool {
	struct_type_source_id := t.params_struct_type_name(expected_id) or {
		t.struct_arg_type_name(expected_id) or { return false }
	}

	mut field_start := -1
	for i in 1 .. node.children_count {
		if t.a.child(&node, i) == first_field_id {
			field_start = i
			break
		}
	}
	if field_start < 0 {
		return false
	}
	return t.specialized_struct_field_args_match(node, field_start, struct_type_source_id, false)
}

fn (t &Transformer) comptime_method_call_arg_matches_hidden_veb_ctx(arg_id flat.NodeId, method MethodMeta) bool {
	receiver_name := flat.source_type_name(comptime_method_receiver_name(method.receiver, method.module_name))
	if receiver_name.len == 0 {
		return false
	}
	params := t.implicit_veb_call_param_types('${receiver_name}.${method.name}') or {
		return false
	}
	// Method ABI parameters start with the receiver, followed by the inserted ctx.
	if params.len < 2 {
		return false
	}
	return t.call_arg_matches_abi_type(arg_id, params[1], method.module_name)
}

fn (t &Transformer) comptime_method_min_required_arg_count(method MethodMeta) int {
	if method.params.len > 0 && flat.source_type_shape(method.params[method.params.len - 1].typ).has(.variadic) {
		return method.params.len - 1
	}
	mut count := method.params.len
	for count > 0 {
		param := method.params[count - 1]
		if flat.source_type_shape(param.typ).has(.option_type) {
			count--
			continue
		}
		if _ := t.params_struct_type_name(param.typ) {
			count--
			continue
		}
		qualified_source_id := t.qualify_generic_arg_for_decl_module(param.typ, param.module_name)
		if qualified_source_id != param.typ {
			if _ := t.params_struct_type_name(qualified_source_id) {
				count--
				continue
			}
		}
		break
	}
	return count
}

// method_has_implicit_veb_ctx reports whether `method` is a veb route handler that
// declares no `ctx` parameter and therefore had one inserted into its signature.
fn (t &Transformer) method_has_implicit_veb_ctx(method MethodMeta) bool {
	if isnil(t.tc) || t.tc.fn_implicit_veb_ctx.len == 0 || method.name.len == 0 {
		return false
	}
	receiver_name := flat.source_type_name(comptime_method_receiver_name(method.receiver, method.module_name))
	if receiver_name.len == 0 {
		return false
	}
	key := '${receiver_name}.${method.name}'
	for name in [key, c_name(key)] {
		if t.tc.fn_implicit_veb_ctx[name] {
			return true
		}
	}
	return false
}

fn (mut t Transformer) clone_method_subst_scoped(id flat.NodeId, var_name string, method MethodMeta, inner_vars []string) ?flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	if comptime_for_declares_var(node, var_name) {
		return t.clone_node_preserving_children(node)
	}
	if node.kind == .ident && node.value == var_name {
		return t.make_method_data_literal(method)
	}
	if idx := t.comptime_method_param_index(id, var_name) {
		if idx >= 0 && idx < method.params.len {
			return t.make_param_data_literal_in_module(method.params[idx], method.module_name)
		}
	}
	if node.kind == .call && node.children_count > 0 {
		callee := t.a.child_node(&node, 0)
		if callee.kind == .selector && callee.value == '\$' && callee.children_count >= 2
			&& t.comptime_method_name_expr_matches(t.a.child(callee, 1), var_name)
			&& !t.comptime_method_call_matches(node, method) {
			// A `$method` call can appear in the runtime branch paired with a
			// method-metadata condition. V1 leaves that branch in place but skips
			// specializations whose argument list cannot call the current method.
			return none
		}
	}
	// Method attributes are compile-time metadata: fold what is already known
	// for this unrolled method instead of materializing a runtime array (one
	// heap allocation per pass of the enclosing `$for`).
	if node.kind == .for_in_stmt && node.children_count >= 3
		&& t.direct_reflected_field_attrs_selector(t.a.child(&node, 2), var_name) {
		if method.attrs.len == 0 {
			return none
		}
		if unrolled := t.unroll_method_attrs_loop(node, var_name, method, inner_vars) {
			return unrolled
		}
	}
	if node.kind == .call {
		if contains := t.comptime_attrs_condition(id, var_name, method.attrs) {
			return t.make_bool_literal(contains)
		}
	}
	if node.kind == .selector && node.value == 'len' && node.children_count > 0
		&& t.direct_reflected_field_attrs_selector(t.a.child(&node, 0), var_name) {
		return t.make_int_literal(method.attrs.len)
	}
	if node.kind == .selector && node.value == '\$' && node.children_count >= 2
		&& t.comptime_method_name_expr_matches(t.a.child(&node, 1), var_name) {
		source_receiver := t.a.child(&node, 0)
		receiver_names_type := t.comptime_method_receiver_names_type(source_receiver)
		receiver := t.clone_method_subst_scoped(source_receiver, var_name, method, inner_vars) or {
			return none
		}
		receiver_name := flat.source_type_name(comptime_method_receiver_name(method.receiver, method.module_name))
		method_key := '${receiver_name}.${method.name}'
		t.mark_fn_used(method_key)
		if receiver_names_type {
			same_main_module := method.module_name in ['', 'main']
				&& t.cur_module in ['', 'main']
			if !method.is_pub && method.module_name != t.cur_module && !same_main_module
				&& !t.cur_file.ends_with('_test.v') && !t.cur_file.ends_with('_test.vv') {
				t.record_monomorph_error('method `${method_key}` is private')
			}
			value := t.make_ident(method_key)
			fn_type := t.tc.resolve_type(value)
			if fn_type is types.FnType {
				t.set_node_typ(int(value), t.semantic_type_name(fn_type))
				t.tc.register_synth_type(value, fn_type)
				return value
			}
		}
		return t.make_comptime_method_selector(receiver, method, receiver_names_type)
	}
	if node.kind == .selector && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind == .ident && base.value == var_name {
			return match node.value {
				'name' {
					t.make_string_literal(method.name)
				}
				'location' {
					t.make_string_literal(method.location)
				}
				'is_pub' {
					t.make_bool_literal(method.is_pub)
				}
				'return_type' {
					t.make_int_literal(t.type_index(method.return_type, method.module_name))
				}
				'typ' {
					t.make_int_literal(t.comptime_method_type_id(method))
				}
				'args', 'params' {
					t.make_param_array_literal(method.params, method.module_name)
				}
				'attrs' {
					t.make_string_array_literal(method.attrs)
				}
				'attributes' {
					t.make_attribute_array_literal(method.attributes)
				}
				else {
					t.clone_method_subst_children(node, var_name, method, inner_vars)
				}
			}
		}
	}
	if node.kind == .comptime_if {
		cond := t.subst_method_cond(node.value, var_name, method)
		if comptime_cond_references_ident(node.value, var_name)
			&& !comptime_cond_has_any_loop_member_ref(cond, inner_vars) {
			if !comptime_cond_has_loop_member_ref(cond, var_name) {
				if taken := t.eval_reflected_string_condition(cond, node) {
					branch_idx := if taken { 0 } else { 1 }
					if branch_idx >= int(node.children_count) {
						return none
					}
					return t.clone_method_subst_scoped(t.a.child(&node, branch_idx), var_name, method, inner_vars)
				}
			}
			if !t.comptime_condition_needs_local_value(cond, node) {
				t.reject_unevaluated_comptime_if(id, node, cond)
			}
		}
		return t.clone_method_subst_children_with_value(node, var_name, method, inner_vars, cond)
	}
	if node.kind == .if_expr && node.children_count >= 2 {
		if taken := t.method_if_condition_value(t.a.child(&node, 0), var_name, method) {
			branch_idx := if taken { 1 } else { 2 }
			if branch_idx >= int(node.children_count) {
				return none
			}
			return t.clone_method_subst_scoped(t.a.child(&node, branch_idx), var_name, method, inner_vars)
		}
	}
	clone := t.clone_method_subst_children(node, var_name, method, inner_vars)
	if node.kind == .ident {
		semantic_type_source_id := t.node_type(id)

		if semantic_type_source_id != 0 && semantic_type_source_id != flat.source_type_unknown {
			t.set_node_typ(int(clone), semantic_type_source_id)
		}
	}
	return clone
}

fn (t &Transformer) method_if_condition_value(id flat.NodeId, var_name string, method MethodMeta) ?bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.node(id)
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return t.method_if_condition_value(t.a.child(node, 0), var_name, method)
	}
	if node.kind == .prefix && node.op == .not && node.children_count > 0 {
		value := t.method_if_condition_value(t.a.child(node, 0), var_name, method) or {
			return none
		}
		return !value
	}
	if node.kind != .infix || node.children_count != 2 {
		return none
	}
	if node.op == .logical_and {
		left := t.method_if_condition_value(t.a.child(node, 0), var_name, method) or {
			return none
		}
		if !left {
			return false
		}
		return t.method_if_condition_value(t.a.child(node, 1), var_name, method)
	}
	if node.op == .logical_or {
		left := t.method_if_condition_value(t.a.child(node, 0), var_name, method) or {
			return none
		}
		if left {
			return true
		}
		return t.method_if_condition_value(t.a.child(node, 1), var_name, method)
	}
	if node.op in [.eq, .ne, .lt, .gt, .le, .ge] {
		if left := t.method_const_int_value(t.a.child(node, 0), var_name, method) {
			if right := t.method_const_int_value(t.a.child(node, 1), var_name, method) {
				return match node.op {
					.eq { left == right }
					.ne { left != right }
					.lt { left < right }
					.gt { left > right }
					.le { left <= right }
					.ge { left >= right }
					else { false }
				}
			}
		}
	}
	if node.op !in [.eq, .ne] {
		return none
	}
	left := t.method_const_string_value(t.a.child(node, 0), var_name, method) or { return none }
	right := t.method_const_string_value(t.a.child(node, 1), var_name, method) or { return none }
	return if node.op == .eq { left == right } else { left != right }
}

fn (t &Transformer) method_const_int_value(id flat.NodeId, var_name string, method MethodMeta) ?int {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.node(id)
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return t.method_const_int_value(t.a.child(node, 0), var_name, method)
	}
	if node.kind == .int_literal {
		return node.value.int()
	}
	if node.kind != .selector || node.value != 'len' || node.children_count == 0 {
		return none
	}
	params := t.a.child_node(node, 0)
	if params.kind != .selector || params.value !in ['args', 'params'] || params.children_count == 0 {
		return none
	}
	owner := t.a.child_node(params, 0)
	if owner.kind == .ident && owner.value == var_name {
		return method.params.len
	}
	return none
}

fn (t &Transformer) method_const_string_value(id flat.NodeId, var_name string, method MethodMeta) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.node(id)
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return t.method_const_string_value(t.a.child(node, 0), var_name, method)
	}
	if node.kind == .string_literal {
		return node.value
	}
	if node.kind == .selector && node.value == 'name' && node.children_count > 0 {
		base := t.a.child_node(node, 0)
		if base.kind == .ident && base.value == var_name {
			return method.name
		}
	}
	return none
}

fn (mut t Transformer) make_attribute_array_literal(attrs []AttributeMeta) flat.NodeId {
	if attrs.len == 0 {
		return t.zero_value_for_type(flat.source_type_id('[]VAttribute'))
	}
	mut ids := []flat.NodeId{cap: attrs.len}
	for attr in attrs {
		ids << t.make_attribute_literal(attr)
	}
	return t.make_array_literal_typed(ids, flat.source_type_id('[]VAttribute'))
}

fn (mut t Transformer) make_param_array_literal(params []ParamMeta, module_name string) flat.NodeId {
	if params.len == 0 {
		return t.zero_value_for_type(flat.source_type_id('[]FunctionParam'))
	}
	mut ids := []flat.NodeId{cap: params.len}
	for param in params {
		ids << t.make_param_data_literal_in_module(param, module_name)
	}
	return t.make_array_literal_typed(ids, flat.source_type_id('[]FunctionParam'))
}

fn (t &Transformer) comptime_method_receiver_names_type(receiver flat.NodeId) bool {
	if int(receiver) < 0 {
		return false
	}
	node := t.a.node(receiver)
	if node.kind != .ident || node.value.len == 0 || t.static_assoc_ident_is_value(receiver) {
		return false
	}
	// A cloned local may not be registered until its declaration is lowered.
	// Decide on the source node while its lexical binding and checked type remain available.
	namespace_source_id := t.comptime_resolve_selective_import_type(flat.source_type_id(node.value))
	namespace_spelling := flat.source_type_name(namespace_source_id)
	if !node.value[0].is_capital() && !types.is_builtin_type_name(namespace_spelling)
		&& !t.is_known_type_name(namespace_source_id) {
		return false
	}
	if _ := t.local_binding_before(node.value, receiver) {
		return false
	}
	if !isnil(t.tc)
		&& (types.is_builtin_type_name(namespace_spelling) || t.is_known_type_name(namespace_source_id)) {
		mut checked_name := node.typ
		if typ := t.tc.expr_type(receiver) {
			checked_name = t.tc.type_name(typ)
		}
		if decl_type_is_usable(checked_name) && checked_name != flat.source_type_void {
			raw_namespace_source_id := t.normalize_type_in_module(namespace_source_id, t.cur_module)
			parsed_namespace_source_id := t.tc.type_name(t.tc.parse_resolution_type(raw_namespace_source_id))
			namespace_name := if decl_type_is_usable(parsed_namespace_source_id) && parsed_namespace_source_id != flat.source_type_void {
				parsed_namespace_source_id
			} else {
				raw_namespace_source_id
			}
			if type_text_without_main_locks(t.normalize_type_alias(checked_name)) !=
				type_text_without_main_locks(t.normalize_type_alias(namespace_name)) {
				return false
			}
		}
	}
	return true
}

fn (mut t Transformer) make_comptime_method_selector(receiver flat.NodeId, method MethodMeta, receiver_names_type bool) flat.NodeId {
	if receiver_names_type {
		receiver_name := flat.source_type_name(comptime_method_receiver_name(method.receiver, method.module_name))
		method_key := '${receiver_name}.${method.name}'
		same_main_module := method.module_name in ['', 'main'] && t.cur_module in ['', 'main']
		if !method.is_pub && method.module_name != t.cur_module && !same_main_module
			&& !t.cur_file.ends_with('_test.v') && !t.cur_file.ends_with('_test.vv') {
			t.record_monomorph_error('method `${method_key}` is private')
		}
		if signature := t.tc.fn_type_for_name(method_key) {
			fn_type := types.Type(signature)
			value := t.a.add_node(flat.Node{
				kind:  .ident
				value: method_key
				typ:   fn_type.source_type_id()
			})
			t.set_resolved_fn_value_entry(int(value), method_key)
			t.set_node_typ(int(value), fn_type.source_type_id())
			t.tc.register_synth_type(value, fn_type)
			return value
		}
	}
	start := t.a.children.len
	t.a.children << receiver
	fn_type := fn_literal_value_type_text_from_text(method.params.map(it.typ), method.return_type)
	return t.a.add_node(flat.Node{
		kind:           .selector
		children_start: start
		children_count: 1
		value:          method.name
		typ:            method.return_type
		payload:        flat.node_payload_with_fn_value_type([comptime_method_selector_marker], fn_type)
	})
}

fn (t &Transformer) comptime_method_param_index(id flat.NodeId, var_name string) ?int {
	if int(id) < 0 {
		return none
	}
	node := t.a.node(id)
	if node.kind == .paren && node.children_count > 0 {
		return t.comptime_method_param_index(t.a.child(node, 0), var_name)
	}
	if node.kind != .index || node.value == 'range' || node.children_count < 2 {
		return none
	}
	base := t.a.child_node(node, 0)
	index := t.a.child_node(node, 1)
	if base.kind != .selector || base.value !in ['args', 'params'] || base.children_count == 0
		|| index.kind != .int_literal
		|| !t.comptime_method_param_owner_matches(t.a.child(base, 0), var_name) {
		return none
	}
	return index.value.int()
}

fn (t &Transformer) comptime_method_param_owner_matches(id flat.NodeId, var_name string) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.node(id)
	if node.kind == .paren && node.children_count > 0 {
		return t.comptime_method_param_owner_matches(t.a.child(node, 0), var_name)
	}
	return node.kind == .ident && node.value == var_name
}

fn (t &Transformer) comptime_method_name_expr_matches(id flat.NodeId, var_name string) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.node(id)
	if node.kind == .ident {
		return node.value == var_name
	}
	if node.kind == .selector && node.value == 'name' && node.children_count > 0 {
		base := t.a.child_node(node, 0)
		return base.kind == .ident && base.value == var_name
	}
	return false
}

fn (mut t Transformer) clone_method_subst_children(node flat.Node, var_name string, method MethodMeta, inner_vars []string) flat.NodeId {
	return t.clone_method_subst_children_with_value(node, var_name, method, inner_vars, node.value)
}

// transform_comptime_method_embedded_arg projects an argument to the embedded
// struct expected by a reflected method. Comptime method calls are checked before
// generic specialization, so their argument can still be `T` then and become a
// concrete embedding struct only while the call is cloned.
fn (mut t Transformer) transform_comptime_method_embedded_arg(arg_id flat.NodeId, param_type_id u32, param_module string) ?flat.NodeId {
	param_type_shape := flat.source_type_shape(param_type_id)
	if int(arg_id) < 0 || !param_type_shape.has(.pointer) {
		return none
	}
	expected_type_source_id := t.trim_pointer_type(param_type_id)
	mut actual_type := t.raw_var_type_for_expr(arg_id) or { t.node_type(arg_id) }
	if actual_type == 0 {
		return none
	}
	arg := t.a.nodes[int(arg_id)]
	if arg.kind == .ident && t.mut_param_values[arg.value] && !flat.source_type_shape(actual_type).has(.pointer) {
		actual_type = flat.source_type_composition_one(.pointer, actual_type)
	}
	actual_base_source_id := t.trim_pointer_type(actual_type)

	actual_identity := type_text_without_main_locks(t.normalize_type_in_module(actual_base_source_id,
		t.cur_module))
	expected_identity := if param_module in ['', 'main'] {
		type_text_without_main_locks(expected_type_source_id)
	} else {
		type_text_without_main_locks(t.normalize_type_in_module(expected_type_source_id, param_module))
	}
	if actual_identity == expected_identity {
		return none
	}
	_ := t.embedded_receiver_path(actual_base_source_id, expected_type_source_id) or { return none }
	base := if flat.source_type_shape(actual_type).has(.pointer) && arg.kind == .ident {
		t.transform_expr_preserving_pointer_value(arg_id)
	} else {
		t.transform_expr(arg_id)
	}
	embedded := t.embedded_receiver_base_for_type(base, actual_type, expected_type_source_id) or {
		return none
	}
	if flat.source_type_shape(t.node_type(embedded)).has(.pointer) {
		t.set_node_typ(int(embedded), param_type_id)
		return embedded
	}
	address := t.make_prefix(.amp, embedded)
	t.set_node_typ(int(address), param_type_id)
	return address
}

fn (mut t Transformer) clone_method_subst_children_with_value(node flat.Node, var_name string, method MethodMeta, inner_vars []string, value string) flat.NodeId {
	child_inner_vars := comptime_nested_loop_vars(node, var_name, inner_vars)
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child_node := t.a.node(child_id)
		if node.kind in [.fn_literal, .lambda_expr] && child_node.kind == .ident
			&& child_node.value == var_name {
			continue
		}
		if child := t.clone_method_subst_scoped(child_id, var_name, method, child_inner_vars) {
			children << child
		}
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	if node.kind == .call && children.len > 1 && method.params.len > 0
		&& !t.method_has_implicit_veb_ctx(method) {
		callee := t.a.node(children[0])
		if callee.kind == .selector && comptime_method_selector_marker in callee.generic_params() {
			if embedded_ctx := t.transform_comptime_method_embedded_arg(children[1],
				method.params[0].typ, method.params[0].module_name)
			{
				children[1] = embedded_ctx
				t.a.children[start + 1] = embedded_ctx
			}
		}
	}
	mut typ := node.typ
	if node.kind == .index && node.value == 'range' && children.len > 0 {
		// `method.args[lo..hi]` is cloned after checking, so the new slice has no
		// checker-side type entry. Keep the materialized metadata array type for
		// for-in inference instead of letting the loop element degrade to `int`.
		base_type_source_id := t.node_type(children[0])
		base_type_shape := flat.source_type_shape(base_type_source_id)
		if base_type_shape.has(.array_type) {
			typ = base_type_source_id
		}
	}
	if node.kind == .call && children.len > 0 {
		callee := t.a.node(children[0])
		if callee.kind == .selector && comptime_method_selector_marker in callee.generic_params() {
			typ = callee.typ
		}
	}
	if node.kind == .comptime_for {
		receiver_name := flat.source_type_name(comptime_method_receiver_name(method.receiver, method.module_name))
		typ = flat.source_type_id(comptime_cond_replace_bare_ident(flat.source_type_name(typ), var_name, '${receiver_name}.${method.name}'))
	}
	return t.a.add_node(flat.Node{
		kind:           node.kind
		op:             node.op
		pos:            node.pos
		value:          value
		source_value:   node.source_value
		typ:            typ
		payload:        if node.payload != 0 { node.clone_owned().payload } else { u32(0) }
		is_mut:         node.is_mut
		flags:          node.flags & comptime_clone_syntax_flags
		children_start: start
		children_count: flat.child_count(children.len)
	})
}

fn (t &Transformer) subst_method_cond(cond string, var_name string, method MethodMeta) string {
	mut result := t.subst_method_param_cond(cond, var_name, method)
	result = comptime_cond_replace_unquoted(result, '${var_name}.args.len', method.params.len.str())
	result = comptime_cond_replace_unquoted(result, '${var_name}.params.len', method.params.len.str())
	// `$if method.attrs.len > 0`, `$if 'GET /x' in method.attrs`
	result = subst_method_attrs_access_cond(result, var_name, method)
	result = comptime_cond_replace_unquoted(result, '${var_name}.attrs.len', method.attrs.len.str())
	if result.contains('${var_name}.attrs') {
		result = comptime_cond_replace_unquoted(result, '${var_name}.attrs', comptime_method_attrs_cond_array(method))
	}
	method_type := flat.source_type_name(t.comptime_method_type_text(method))
	result = comptime_cond_replace_unquoted(result, '${var_name}.location', comptime_cond_string_literal(method.location))
	result = comptime_cond_replace_int_compared(result, '${var_name}.return_type', t.type_index(method.return_type, method.module_name).str())
	result = comptime_cond_replace_unquoted(result, '${var_name}.return_type', flat.source_type_name(t.comptime_field_type_id_key(method.return_type, method.module_name)))
	result = comptime_cond_replace_int_compared(result, '${var_name}.typ', t.comptime_method_type_id(method).str())
	result = comptime_cond_replace_unquoted(result, '${var_name}.typ', method_type)
	result = comptime_cond_replace_unquoted(result, '${var_name}.is_pub', method.is_pub.str())
	result = comptime_cond_replace_unquoted(result, '${var_name}.name', "'${method.name}'")
	result = comptime_cond_replace_bare_ident(result, var_name, method_type)
	for op in [' !is ', ' is '] {
		if idx := comptime_top_index(result, op) {
			expected := trimmed_transform_text(result[idx + op.len..])
			normalized := if !isnil(t.tc)
				&& (expected.starts_with('fn(') || expected.starts_with('fn (')) {
				flat.source_type_name(t.comptime_field_type_id_key(flat.source_type_id(expected), t.cur_module))
			} else {
				flat.source_type_name(t.comptime_normalize_type_alias_chain(flat.source_type_id(expected)))
			}
			if normalized != expected {
				result = result[..idx + op.len] + normalized
			}
			break
		}
	}
	return result
}

fn (t &Transformer) comptime_method_type_text(method MethodMeta) u32 {
	mut parts := []u32{cap: method.params.len + 1}
	for param in method.params {
		parts << t.comptime_field_type_id_key(param.typ, method.module_name)
	}
	if method.return_type != 0 && method.return_type != flat.source_type_void {
		parts << t.comptime_field_type_id_key(method.return_type, method.module_name)
		return flat.source_type_composition(.function_type, parts)
	}
	return flat.source_type_composition(.void_function_type, parts)
}

fn (t &Transformer) comptime_method_type_id(method MethodMeta) int {
	return t.type_index(t.comptime_method_type_text(method), '')
}

// subst_method_attrs_access_cond materializes `method.attrs[i]` (the attribute, or '' past the
// last one, like a missing param name) and `method.attrs.contains(x)` (as `x in [...]`) in
// serialized `$if` guards; left as they are, neither could be evaluated.
fn subst_method_attrs_access_cond(cond string, var_name string, method MethodMeta) string {
	prefix := '${var_name}.attrs'
	if !cond.contains(prefix) {
		return cond
	}
	mut result := cond
	mut offset := 0
	for offset < result.len {
		if result[offset] == `'` || result[offset] == `"` {
			offset = comptime_cond_skip_string(result, offset)
			continue
		}
		if !result[offset..].starts_with(prefix)
			|| (offset > 0 && comptime_cond_name_char(result[offset - 1])) {
			offset++
			continue
		}
		start := offset
		mut pos := start + prefix.len
		if pos < result.len && result[pos] == `[` {
			rel_end := result[pos + 1..].index_u8(`]`)
			if rel_end < 0 {
				break
			}
			index_text := trimmed_transform_text(result[pos + 1..pos + 1 + rel_end])
			end := pos + 1 + rel_end + 1
			if !comptime_is_int(index_text) || index_text.starts_with('-') {
				offset = end
				continue
			}
			index := index_text.int()
			mut replacement := if index < method.attrs.len {
				comptime_cond_string_literal(comptime_attr_display(method.attrs[index]))
			} else {
				"''"
			}
			// the guard is serialized as `method.attrs[0]== 'x'`; comparisons need ` == `,
			// while a method call (`.starts_with(...)`) or a closing paren must stay attached
			if end < result.len && result[end] !in [` `, `.`, `)`] {
				replacement += ' '
			}
			result = result[..start] + replacement + result[end..]
			offset = start + replacement.len
			continue
		}
		if !result[pos..].starts_with('.contains')
			|| (pos + 9 < result.len && comptime_cond_name_char(result[pos + 9])) {
			offset = pos
			continue
		}
		pos += '.contains'.len
		for pos < result.len && result[pos] == ` ` {
			pos++
		}
		if pos >= result.len || result[pos] != `(` {
			offset = pos
			continue
		}
		// the argument runs to the matching `)`; parens inside strings do not count
		arg_start := pos + 1
		mut depth := 1
		pos = arg_start
		for pos < result.len && depth > 0 {
			ch := result[pos]
			if ch == `'` || ch == `"` {
				pos = comptime_cond_skip_string(result, pos)
				continue
			}
			if ch == `(` {
				depth++
			} else if ch == `)` {
				depth--
			}
			pos++
		}
		if depth != 0 {
			break
		}
		arg := comptime_condition_strip_outer_parens(trimmed_transform_text(result[arg_start..pos - 1]))
		replacement := '(${arg} in ${comptime_method_attrs_cond_array(method)})'
		result = result[..start] + replacement + result[pos..]
		offset = start + replacement.len
	}
	return result
}

// comptime_method_attrs_cond_array renders the method's attributes as a `$if` array literal.
fn comptime_method_attrs_cond_array(method MethodMeta) string {
	attrs := method.attrs.map(comptime_cond_string_literal(comptime_attr_display(it)))
	return '[${attrs.join(', ')}]'
}

// subst_method_param_cond materializes indexed FunctionParam members in serialized `$if` guards.
fn (t &Transformer) subst_method_param_cond(cond string, var_name string, method MethodMeta) string {
	mut result := cond
	for collection in ['args', 'params'] {
		prefix := '${var_name}.${collection}['
		mut offset := 0
		for offset < result.len {
			if result[offset] == `'` || result[offset] == `"` {
				offset = comptime_cond_skip_string(result, offset)
				continue
			}
			if !result[offset..].starts_with(prefix)
				|| (offset > 0 && comptime_cond_name_char(result[offset - 1])) {
				offset++
				continue
			}
			start := offset
			index_start := start + prefix.len
			rel_end := result[index_start..].index_u8(`]`)
			if rel_end < 0 {
				break
			}
			index_end := index_start + rel_end
			member_start := index_end + 1
			member := if result[member_start..].starts_with('.typ') {
				'typ'
			} else if result[member_start..].starts_with('.name') {
				'name'
			} else {
				''
			}
			member_end := member_start + member.len + 1
			if member.len == 0
				|| (member_end < result.len && comptime_cond_name_char(result[member_end])) {
				offset = member_start
				continue
			}
			index_text := trimmed_transform_text(result[index_start..index_end])
			if !comptime_is_int(index_text) || index_text.starts_with('-') {
				offset = member_end
				continue
			}
			index := index_text.int()
			replacement := if index >= 0 && index < method.params.len {
				if member == 'name' {
					"'${method.params[index].name}'"
				} else if comptime_cond_int_compared(result, start, member_end) {
					t.type_index(method.params[index].typ, method.module_name).str()
				} else if method.params[index].typ == transform_identity_void_ref {
					'voidptr'
				} else {
					flat.source_type_name(t.comptime_field_type_id_key(method.params[index].typ, method.module_name))
				}
			} else if member == 'name' {
				"''"
			} else {
				// Missing slots must not accidentally match a real type such as `void`.
				'__v3_missing_method_param_type'
			}
			result = result[..start] + replacement + result[member_end..]
			offset = start + replacement.len
		}
	}
	return result
}

// expand_comptime_for_values unrolls `$for value in Enum.values { ... }`. The loop variable
// exposes `value.name` / `value.value` and, used bare, materializes as `EnumData`.
fn (mut t Transformer) expand_comptime_for_values(var_name string, base_type_id u32, body_stmts []flat.NodeId) []flat.NodeId {
	values := t.comptime_enum_members(base_type_id)
	mut out := []flat.NodeId{}
	for item in values {
		mut cloned := []flat.NodeId{cap: body_stmts.len}
		for sid in body_stmts {
			if cid := t.clone_value_subst(sid, var_name, item) {
				cloned << cid
			}
		}
		out << t.make_block(t.transform_scope_stmts(cloned))
	}
	return out
}

// expand_comptime_for_variants unrolls `$for variant in Sum.variants`. The loop variable is a
// VariantData value at runtime, while its `.typ` member also carries the concrete compile-time
// type used by `is` checks and `typeof(variant.typ)`.
fn (mut t Transformer) expand_comptime_for_variants(var_name string, base_type_id u32, body_stmts []flat.NodeId) []flat.NodeId {
	mut out := []flat.NodeId{}
	for item in t.comptime_sum_variants(base_type_id) {
		mut cloned := []flat.NodeId{cap: body_stmts.len}
		for sid in body_stmts {
			if cid := t.clone_variant_subst(sid, var_name, item) {
				cloned << cid
			}
		}
		out << t.make_block(t.transform_scope_stmts(cloned))
	}
	return out
}

fn (t &Transformer) comptime_sum_variants(base_type_id u32) []VariantMeta {
	resolved := t.comptime_resolve_sum_type_name(base_type_id)
	mut variants := if !flat.source_type_shape(base_type_id).has(.dotted) {
		t.comptime_local_sum_variants(t.trimmed_type_id(base_type_id)) or { []u32{} }
	} else if isnil(t.tc) {
		[]u32{}
	} else {
		t.tc.sum_types.type_ids(resolved) or { []u32{} }
	}
	if variants.len == 0 && !isnil(t.tc) {
		variants = t.tc.sum_types.type_ids(resolved) or { []u32{} }
	}
	if variants.len == 0 {
		variants = t.sum_types.type_ids(resolved) or { []u32{} }
	}
	mut metas := []VariantMeta{cap: variants.len}
	for variant in variants {
		metas << VariantMeta{ typ: variant, typ_id: t.type_index(variant, t.cur_module) }
	}
	return metas
}

fn (t &Transformer) comptime_resolve_sum_type_name(base_type_id u32) u32 {
	base := t.trimmed_type_id(base_type_id)
	if base == 0 { return base }
	// Main-module locks use bare declaration storage keys.
	if flat.source_type_query(base).has(.main_prefix) {
		storage := flat.source_type_syntax(base).main_base()
		if t.sum_types.contains(storage) || t.generic_checker_sum_name_known(storage) {
			return storage
		}
	}
	if isnil(t.tc) { return base }
	if flat.source_type_shape(base).has(.dotted) {
		return t.resolve_imported_type_name(base) or { base }
	}
	local := if t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), base)
	} else {
		base
	}
	if t.generic_checker_sum_name_known(local) { return local }
	// Bare caller types take precedence over the imported declaration's module.
	if t.generic_checker_sum_name_known(base) { return base }
	imported := t.comptime_resolve_selective_import_type(base)
	if imported != base { return imported }
	return local
}

// collect_types keeps legacy short-name entries for imported sums, so locate a bare sum's
// declaration in the current module before consulting that ambiguous cache.
fn (t &Transformer) comptime_local_sum_variants(name_id u32) ?[]u32 {
	if name_id == 0 || flat.source_type_shape(name_id).has(.dotted) { return none }
	mut module_name := ''
	for node_id in 0 .. t.a.nodes.len {
		// This read-only scan keeps the borrowed AST node valid.
		node := unsafe { &t.a.nodes[node_id] }
		if node.kind == .file {
			module_name = ''
			continue
		}
		if node.kind == .module_decl {
			module_name = node.value
			continue
		}
		if node.kind != .type_decl || flat.source_type_id(node.value) != name_id || module_name != t.cur_module
			|| node.children_count == 0 {
			continue
		}
		mut variants := []u32{cap: int(node.children_count)}
		for i in 0 .. node.children_count {
			variant := t.a.child_node(node, i)
			variants << t.normalize_sum_variant_type(variant.source_value_id(), module_name, node.generic_params())
		}
		return variants
	}
	return none
}

// comptime_enum_members returns the member metadata for an enum (resolving an enum alias to its
// underlying enum).
fn (t &Transformer) comptime_enum_members(base_type_id u32) []EnumValueMeta {
	mut names := []string{}
	storage_name_id := if flat.source_type_query(base_type_id).has(.main_prefix) {
		flat.source_type_syntax(base_type_id).main_base()
	} else {
		base_type_id
	}
	storage_name := storage_name_id
	mut resolved := storage_name_id
	if got := t.enum_types[storage_name] {
		names = got.clone()
	} else {
		qname := t.qualified_alias_name(storage_name_id)
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
			name:      name
			value:     i64(idx)
			enum_name: resolved
		}
	}
	return fallback
}

// enum_decl_value_metas locates the enum declaration matching `enum_name` (either a bare `Enum`
// or a `module.Enum` qualified name) and evaluates each member's metadata. Explicit values are
// treated like the C backend: normal enums use the integer directly; `[flag]` enums use it as the
// bit index and materialize `1 << index`.
fn (t &Transformer) enum_decl_value_metas(enum_name_id u32) []EnumValueMeta {
	checked_values := if isnil(t.tc) {
		map[string]i64{}
	} else {
		t.tc.comptime_enum_decl_field_values(enum_name_id)
	}
	enum_name := flat.source_type_name(enum_name_id)
	mut cur_mod := ''
	for idx in 0 .. t.a.nodes.len {
		kind := t.a.nodes[idx].kind
		if kind == .file {
			cur_mod = ''
			continue
		}
		if kind == .module_decl {
			cur_mod = t.a.nodes[idx].value
			continue
		}
		if kind != .enum_decl {
			continue
		}
		// This helper only reads AST nodes while the borrow is live.
		node := unsafe { &t.a.nodes[idx] }
		qualified := if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
			'${cur_mod}.${node.value}'
		} else {
			node.value
		}
		if enum_name != node.value && enum_name != qualified {
			continue
		}
		qualified_id := flat.source_type_id(qualified)
		is_flag := node.typ_text() == 'flag'
		mut fields := []EnumDeclFieldValue{}
		mut field_exprs := map[string]flat.NodeId{}
		for i in 0 .. node.children_count {
			f := t.a.child_node(node, i)
			if f.kind != .enum_field {
				continue
			}
			expr_id := if f.children_count > 0 { t.a.child(f, 0) } else { flat.NodeId(-1) }
			fields << EnumDeclFieldValue{
				name:       f.value
				expr_id:    expr_id
				attrs:      f.generic_params().clone()
				attr_kinds: f.attribute_kinds_id()
			}
			if int(expr_id) >= 0 {
				field_exprs[f.value] = expr_id
			}
		}
		mut values := []EnumValueMeta{}
		mut field_values := map[string]i64{}
		mut resolving := map[string]bool{}
		mut next_val := i64(0)
		mut next_from_c := false
		for f in fields {
			mut val := next_val
			mut from_c := next_from_c
			mut materialized_value := i64(0)
			mut has_materialized_value := false
			if checked_value := checked_values[f.name] {
				materialized_value = i64(checked_value)
				has_materialized_value = true
				from_c = false
				if is_flag {
					val = enum_flag_value_index(u64(checked_value)) or { next_val }
				} else {
					val = materialized_value
				}
			} else if !isnil(t.tc) {
				from_c = true
			} else if int(f.expr_id) >= 0 {
				if ev := t.enum_field_int_value_with_enum(f.expr_id, cur_mod, qualified_id, mut field_values, field_exprs, mut resolving) {
					val = ev
					from_c = false
				} else {
					from_c = true
				}
			}
			if !from_c {
				field_values[f.name] = val
			}
			values << EnumValueMeta{
				name:                f.name
				value_from_c:        from_c
				value:               if has_materialized_value {
					materialized_value
				} else if is_flag {
					i64(u64(1) << u64(val))
				} else {
					val
				}
				attrs:               f.attrs.clone()
				attr_kinds:          f.attr_kinds
				attribute_module_id: if f.attrs.len > 0 {
					flat.source_type_id(cur_mod)
				} else {
					u32(0)
				}
				enum_name:           qualified_id
			}
			next_val = val + 1
			next_from_c = from_c
		}
		return values
	}
	return []EnumValueMeta{}
}

fn enum_flag_value_index(value u64) ?i64 {
	if value == 0 || (value & (value - 1)) != 0 {
		return none
	}
	mut index := i64(0)
	mut remaining := value
	for remaining > 1 {
		remaining >>= 1
		index++
	}
	return index
}

// enum_field_int_value evaluates an enum member's value expression using the transformer's
// current module for any const reference.
fn (t &Transformer) enum_field_int_value(id flat.NodeId) ?i64 {
	return t.enum_field_int_value_in_module(id, t.cur_module)
}

// enum_field_int_value_in_module evaluates an enum member's value expression (`x = 1`,
// `x = 1 << 2`, `x = SomeConst`) to an i64, following the same forms as the C backend.
// `enum_module` is the enum's declaring module, so a const referenced by a member (`a = base`)
// resolves in that module rather than the caller's.
fn (t &Transformer) enum_field_int_value_in_module(id flat.NodeId, enum_module string) ?i64 {
	mut field_values := map[string]i64{}
	field_exprs := map[string]flat.NodeId{}
	mut resolving := map[string]bool{}
	return t.enum_field_int_value_with_enum(id, enum_module, u32(0), mut field_values, field_exprs, mut resolving)
}

fn (t &Transformer) enum_field_int_value_with_enum(id flat.NodeId, enum_module string, enum_name u32, mut field_values map[string]i64, field_exprs map[string]flat.NodeId, mut resolving map[string]bool) ?i64 {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.int_literal {
			clean := node.value.replace('_', '')
			parsed := strconv.common_parse_int(clean, util.v_literal_parse_base(clean), 64, true, true) or { return none }
			return parsed
		}
		.paren, .cast_expr {
			if node.children_count == 0 {
				return none
			}
			return t.enum_field_int_value_with_enum(t.a.child(&node, 0), enum_module, enum_name, mut field_values, field_exprs, mut resolving)
		}
		.prefix {
			if node.children_count == 0 {
				return none
			}
			v := t.enum_field_int_value_with_enum(t.a.child(&node, 0), enum_module, enum_name, mut field_values, field_exprs, mut resolving)?
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
			l := t.enum_field_int_value_with_enum(t.a.child(&node, 0), enum_module, enum_name, mut field_values, field_exprs, mut resolving)?
			r := t.enum_field_int_value_with_enum(t.a.child(&node, 1), enum_module, enum_name, mut field_values, field_exprs, mut resolving)?
			if node.op in [.left_shift, .right_shift, .right_shift_unsigned] && (r < 0 || r >= 64) {
				return none
			}
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
				.power {
					enum_metadata_int_power(l, r)
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
					i64(u64(l) << u64(r))
				}
				.right_shift {
					l >> r
				}
				.right_shift_unsigned {
					i64(u64(l) >> u64(r))
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
		.enum_val {
			return t.enum_decl_field_ref_value(node.value, enum_module, enum_name, mut field_values, field_exprs, mut resolving)
		}
		.selector {
			if field := t.enum_decl_selector_ref_field(id, enum_module, enum_name) {
				return t.enum_decl_field_ref_value(field, enum_module, enum_name, mut field_values, field_exprs, mut resolving)
			}
			return none
		}
		.ident {
			if ev := t.enum_decl_field_ref_value(node.value, enum_module, enum_name, mut field_values, field_exprs, mut resolving) {
				return ev
			}
			if !isnil(t.tc) {
				lookup_module := if enum_module != '' { enum_module } else { t.cur_module }
				return i64(t.tc.const_int_value_in_module(node.value, lookup_module, []string{})?)
			}
			return none
		}
		else {
			return none
		}
	}
}

@[ignore_overflow]
fn enum_metadata_int_power(base i64, exponent i64) i64 {
	mut exp := exponent
	mut power := base
	mut value := i64(1)
	if exp < 0 {
		if base == 0 {
			return -1
		}
		if base != 1 && base != -1 {
			return 0
		}
		return if exp & 1 != 0 { base } else { 1 }
	}
	for exp > 0 {
		if exp & 1 != 0 {
			value *= power
		}
		power *= power
		exp >>= 1
	}
	return value
}

fn (t &Transformer) enum_decl_field_ref_value(field_name string, enum_module string, enum_name u32, mut field_values map[string]i64, field_exprs map[string]flat.NodeId, mut resolving map[string]bool) ?i64 {
	if val := field_values[field_name] {
		return val
	}
	expr_id := field_exprs[field_name] or { return none }
	if resolving[field_name] {
		return none
	}
	resolving[field_name] = true
	maybe_val := t.enum_field_int_value_with_enum(expr_id, enum_module, enum_name, mut field_values, field_exprs, mut resolving)
	resolving.delete(field_name)
	val := maybe_val?
	field_values[field_name] = val
	return val
}

fn (t &Transformer) enum_decl_selector_ref_field(id flat.NodeId, enum_module string, enum_name u32) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	prefix := t.enum_decl_selector_base_text(t.a.child(&node, 0))
	if !enum_ref_prefix_matches(prefix, enum_module, flat.source_type_name(enum_name)) {
		return none
	}
	return node.value
}

fn (t &Transformer) enum_decl_selector_base_text(id flat.NodeId) string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return ''
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value
		}
		.selector {
			if node.children_count == 0 {
				return node.value
			}
			base := t.enum_decl_selector_base_text(t.a.child(&node, 0))
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
	if prefix == '' || enum_name == '' {
		return false
	}
	short := enum_name.all_after_last('.')
	if prefix == enum_name || prefix == short {
		return true
	}
	if enum_module != '' && prefix == '${enum_module}.${short}' {
		return true
	}
	return false
}

// clone_value_subst clones a `$for value in Enum.values` body, substituting `value.name`,
// `value.value`, `value.attrs`, and a bare `value` (an `EnumData` literal).
fn (mut t Transformer) clone_value_subst(id flat.NodeId, var_name string, item EnumValueMeta) ?flat.NodeId {
	return t.clone_value_subst_scoped(id, var_name, item, []string{})
}

fn (mut t Transformer) clone_value_subst_scoped(id flat.NodeId, var_name string, item EnumValueMeta, inner_vars []string) ?flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	attr_var := comptime_reflected_attributes_loop_var(node, var_name)
	if attr_var == var_name {
		// `$for value in value.attributes` shadows the enum value in its body.
		return t.unroll_reflected_attributes_loop(id, attr_var, item.raw_attribute_data(), flat.source_type_name(item.attribute_module_id), inner_vars)
	}
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
					return t.make_comptime_enum_value(item)
				}
				'attrs' {
					return t.make_string_array_literal(item.attrs)
				}
				'attributes' {
					return t.make_attribute_array_literal(item.attribute_metas())
				}
				else {}
			}
		}
	}
	// `$if`/`$else $if` referencing the loop variable (`value.name`, `value.value`): evaluate now
	// and keep the taken branch, mirroring the field-loop path so the guard is not left as an
	// unsupported `comptime_if` for the C backend.
	mut value := node.value
	if node.kind == .comptime_if && comptime_cond_references_ident(node.value, var_name) {
		cond := t.subst_value_cond(node.value, var_name, item)
		if !comptime_cond_has_any_loop_member_ref(cond, inner_vars) {
			if !comptime_cond_has_loop_member_ref(cond, var_name) {
				if taken := t.eval_reflected_string_condition(cond, node) {
					branch_idx := if taken { 0 } else { 1 }
					if branch_idx >= int(node.children_count) {
						return none
					}
					return t.clone_value_subst_scoped(t.a.child(&node, branch_idx), var_name, item, inner_vars)
				}
			}
			if !t.comptime_condition_needs_local_value(cond, node) {
				t.reject_unevaluated_comptime_if(id, node, cond)
			}
		}
		// A nested `$for` decides the rest, with this loop's variable already substituted.
		value = cond
	}
	child_vars := comptime_nested_loop_vars(node, var_name, inner_vars)
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if c := t.clone_value_subst_scoped(child_id, var_name, item, child_vars) {
			children << c
		}
	}
	start := t.a.children.len
	for c in children {
		t.a.children << c
	}
	clone_id := t.a.add_node(flat.Node{
		kind:           node.kind
		op:             node.op
		pos:            node.pos
		value:          value
		source_value:   node.source_value
		payload:        if node.payload != 0 { node.clone_owned().payload } else { u32(0) }
		typ:            node.typ
		is_mut:         node.is_mut
		flags:          node.flags & comptime_clone_syntax_flags
		children_start: start
		children_count: flat.child_count(children.len)
	})
	if attr_var.len > 0 {
		return t.unroll_reflected_attributes_loop(clone_id, attr_var, item.raw_attribute_data(), flat.source_type_name(item.attribute_module_id), inner_vars)
	}
	return clone_id
}

fn (mut t Transformer) comptime_field_call_generic_args(node flat.Node, mut children []flat.NodeId, fm FieldMeta) (string, []u32) {
	if node.kind != .call || children.len < 2 {
		return node.value, node.call_type_args()
	}
	// Calls also store checker-inferred generic argument IDs. A reflected
	// field loop must infer those again for each unrolled field; otherwise the
	// first field's specialization can leak into later fields. Preserve only a
	// source-level `fn[Type](...)` selection here.
	if t.call_has_source_generic_args(node) {
		return node.value, node.call_type_args()
	}
	callee := t.a.nodes[int(children[0])]
	if callee.kind !in [.ident, .selector] {
		return node.value, node.call_type_args()
	}
	decls := t.cached_generic_fn_decls()
	mut decl := GenericFnDecl{}
	mut found := false
	if callee.kind == .ident {
		for candidate in t.generic_plain_call_candidates(callee.value, t.cur_module) {
			if got := decls[candidate] {
				decl = got
				found = true
				break
			}
		}
		// The checker can already lower an inferred generic receiver call to a
		// concrete plain callee (`Decoder_string.decode_value`). The reflected
		// clone still has to recover the receiver-method template so each field
		// can select its own specialization.
		if !found && t.generic_callee_is_specialization(callee.value) {
			method_name := callee.value.all_after_last('.').all_after_last('__')
			for key in t.generic_receiver_methods_by_name[method_name] {
				candidate := decls[key] or { continue }
				if candidate.module == t.cur_module {
					decl = candidate
					found = true
					break
				}
			}
		}
	} else {
		for key in t.generic_receiver_methods_by_name[callee.value] {
			candidate := decls[key] or { continue }
			if candidate.module == t.cur_module {
				decl = candidate
				found = true
				break
			}
		}
	}
	if !found {
		return node.value, node.call_type_args()
	}
	param_names := t.generic_fn_param_names(decl.node, decl.module)
	if param_names.len == 0 {
		return comptime_unsupported_late_generic_call, []u32{}
	}
	mut inferred := map[string]u32{}
	mut param_idx := 0
	is_receiver := callee.kind == .selector && t.generic_decl_is_receiver_method(decl.node)
	for i in 0 .. decl.node.children_count {
		param := t.a.child_node(&decl.node, i)
		if param.kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		arg_id := if is_receiver && param_idx == 0 {
			if callee.children_count == 0 { flat.empty_node } else { t.a.child(&callee, 0) }
		} else {
			arg_pos := if is_receiver { param_idx } else { param_idx + 1 }
			if arg_pos < children.len {
				children[arg_pos]
			} else {
				flat.empty_node
			}
		}
		if int(arg_id) < 0 {
			break
		}
		mut inference_param_type := param.typ
		mut value_id := arg_id
		if generic_param_text_is_variadic(param.typ) {
			inference_param_type, value_id = t.generic_variadic_arg_for_inference(param.typ,
				generic_variadic_elem_param_text(param.typ), arg_id)
		}
		arg := t.a.nodes[int(value_id)]
		mut arg_type := t.comptime_field_generic_arg_type(value_id, fm)
		if arg.kind == .ident {
			if payload_source_id := t.comptime_option_unwrapped_local_type(arg.value, node, fm) {
				arg_type = payload_source_id
			}
		}
		if arg_type in [u32(0), flat.source_type_unknown, flat.source_type_generic]
			|| arg_type in [expr_array_type_id, expr_map_type_id]
			|| flat.source_type_query(arg_type).has(.generic_fn_name) {
			arg_type = fm.comptime_typ
		}
		mut inference_type := arg_type
		if inference_type == 0 {
			inference_type = fm.comptime_unaliased
		}
		infer_generic_type_args(inference_param_type, inference_type, mut inferred)
		param_idx++
	}
	mut inferred_args := []u32{cap: param_names.len}
	for name in param_names {
		inferred_args << inferred[name] or { return comptime_unsupported_late_generic_call, []u32{} }
	}
	args_source_ids := t.canonical_generic_specialization_args(inferred_args)

	if args_source_ids.len != param_names.len || t.generic_args_have_placeholders(args_source_ids) {
		return comptime_unsupported_late_generic_call, []u32{}
	}
	mut symbol_args := []u32{cap: args_source_ids.len}
	for arg_source_id in args_source_ids {
		arg := flat.source_type_name(arg_source_id)

		symbol_args << flat.source_type_id(generic_type_name_display(arg))
	}
	spec_value := specialized_generic_fn_value(decl.node.value, symbol_args)
	spec_name := transform_qualified_fn_name(decl.module, spec_value)
	t.record_generic_specialization_args_for_names([
		spec_value,
		spec_name,
		c_name(spec_value),
		c_name(spec_name),
	], args_source_ids)
	if t.defer_nested_generic_emissions || t.parallel_monomorph_worker {
		t.request_generic_fn_specialization(decl, args_source_ids)
	} else {
		clone_id := t.emit_generic_fn_specialization(decl, args_source_ids)
		t.generated_fn_used_names(decl, clone_id, args_source_ids)
	}
	if is_receiver {
		receiver := t.a.child(&callee, 0)
		children[0] = t.make_ident(spec_name)
		children.insert(1, receiver)
	} else {
		t.set_node_value(int(children[0]), spec_name)
	}
	// The callee now carries the exact specialization, so no inferred type arguments remain.
	return node.value, []u32{}
}

fn (mut t Transformer) comptime_field_generic_arg_type(arg_id flat.NodeId, fm FieldMeta) u32 {
	arg := t.a.nodes[int(arg_id)]
	if arg.kind == .ident {
		return t.local_decl_type_before(arg.value, arg_id) or {
			t.comptime_reflected_for_in_local_type(arg.value, fm) or {
				t.generic_call_arg_type_for_inference(arg_id)
			}
		}
	}
	if arg.kind == .prefix && arg.children_count > 0 {
		child_id := t.a.child(&arg, 0)
		child := t.a.nodes[int(child_id)]
		if child.kind == .ident {
			if local_type_source_id := t.local_decl_type_before(child.value, child_id) {
				if arg.op == .amp {
					return flat.source_type_composition_one(.pointer, local_type_source_id)
				}
				if arg.op == .mul && flat.source_type_shape(local_type_source_id).has(.pointer) {
					return flat.source_type_syntax(local_type_source_id).raw_base()
				}
			}
		}
	}
	return t.generic_call_arg_type_for_inference(arg_id)
}

fn (t &Transformer) comptime_option_unwrapped_local_type(name string, call flat.Node, fm FieldMeta) ?u32 {
	if !fm.is_option || !flat.source_type_shape(fm.comptime_typ).has(.option_type) || name == '' || !call.pos.is_valid() {
		return none
	}
	mut source_name := name
	for _ in 0 .. 4 {
		mut found_offset := -1
		mut next_name := ''
		for candidate_id in t.local_decl_nodes_by_name[source_name] {
			// This helper only reads AST nodes while the borrow is live.
			candidate := unsafe { &t.a.nodes[candidate_id] }
			if candidate.kind != .decl_assign || candidate.children_count < 2
				|| !candidate.pos.is_valid() || candidate.pos.id != call.pos.id
				|| candidate.pos.offset >= call.pos.offset || candidate.pos.offset <= found_offset {
				continue
			}
			lhs := t.a.child_node(candidate, 0)
			rhs := t.a.child_node(candidate, 1)
			if lhs.kind == .ident && lhs.value == source_name && rhs.kind == .ident {
				found_offset = candidate.pos.offset
				next_name = rhs.value
			}
		}
		if next_name.len == 0 || next_name == source_name {
			break
		}
		source_name = next_name
	}
	if t.source_parent_ids.len > 0 {
		for candidate_id in t.if_expr_nodes_by_file[call.pos.id] {
			if t.comptime_option_guard_unwraps(unsafe { &t.a.nodes[candidate_id] }, source_name, &call) {
				return t.trimmed_type_id(flat.source_type_syntax(fm.comptime_typ).raw_base())
			}
		}
	} else {
		// Hand-built transform tests may invoke this helper without prepare(), so
		// retain the complete scan when the immutable source indexes are unavailable.
		for candidate_id in 0 .. t.a.nodes.len {
			// This read-only scan keeps the borrowed AST node valid.
			candidate := unsafe { &t.a.nodes[candidate_id] }
			if t.comptime_option_guard_unwraps(candidate, source_name, &call) {
				return t.trimmed_type_id(flat.source_type_syntax(fm.comptime_typ).raw_base())
			}
		}
	}
	return none
}

fn (t &Transformer) comptime_option_guard_unwraps(candidate &flat.Node, source_name string, call &flat.Node) bool {
	if candidate.kind != .if_expr || candidate.children_count < 2 {
		return false
	}
	body := t.a.child_node(candidate, 1)
	if !body.pos.is_valid() || body.pos.id != call.pos.id || call.pos.offset < body.pos.offset
		|| call.pos.offset > body.pos.end {
		return false
	}
	condition := t.a.child_node(candidate, 0)
	if condition.kind != .infix || condition.op != .ne || condition.children_count < 2 {
		return false
	}
	left := t.a.child_node(condition, 0)
	right := t.a.child_node(condition, 1)
	return (left.kind == .ident && left.value == source_name && right.kind == .none_expr)
		|| (right.kind == .ident && right.value == source_name && left.kind == .none_expr)
}

fn (mut t Transformer) comptime_reflected_for_in_local_type(name string, fm FieldMeta) ?u32 {
	if name == '' {
		return none
	}
	iter_type_source_id := t.comptime_normalize_type_alias_chain(fm.comptime_typ)
	iter_shape := flat.source_type_shape(iter_type_source_id)
	if !iter_shape.has(.map_type) && !iter_shape.has(.array_type) {
		return none
	}
	t.prepare_comptime_reflected_for_roles()
	role := t.comptime_reflected_for_roles[name] or { return none }
	if iter_shape.has(.map_type) {
		if role in [u8(1), 3] {
			return t.map_key_type(iter_type_source_id)
		}
		return t.map_value_type(iter_type_source_id)
	}
	if role == 1 {
		return flat.source_type_int
	}
	return flat.source_type_syntax(iter_type_source_id).raw_base()
}

// prepare_comptime_reflected_for_roles indexes the first reflected-field loop role for each
// local name. Generic JSON decoders query this while cloning every reflected field; rescanning
// the complete, growing AST for each query made monomorphization quadratic on large programs.
fn (mut t Transformer) prepare_comptime_reflected_for_roles() {
	if t.comptime_reflected_for_ready {
		return
	}
	t.comptime_reflected_for_ready = true
	mut reflected_locals := map[string]bool{}
	for node_id in 0 .. t.a.nodes.len {
		// This read-only scan keeps the borrowed AST node valid.
		node := unsafe { &t.a.nodes[node_id] }
		if node.kind != .decl_assign || node.children_count < 2 {
			continue
		}
		lhs := t.a.child_node(node, 0)
		if lhs.kind == .ident && lhs.value.len > 0
			&& t.subtree_has_comptime_field_selector(t.a.child(node, 1)) {
			reflected_locals[lhs.value] = true
		}
	}
	mut roles := map[string]u8{}
	for node_id in 0 .. t.a.nodes.len {
		// This read-only scan keeps the borrowed AST node valid.
		node := unsafe { &t.a.nodes[node_id] }
		if node.kind != .for_in_stmt || node.children_count < 3 {
			continue
		}
		container_id := t.a.child(node, 2)
		// This helper only reads AST nodes while the borrow is live.
		container := unsafe { &t.a.nodes[int(container_id)] }
		if !t.subtree_has_comptime_field_selector(container_id) && !(container.kind == .ident
			&& reflected_locals[container.value]) {
			continue
		}
		key := t.a.child_node(node, 0)
		value := t.a.child_node(node, 1)
		has_index := value.kind == .ident && value.value.len > 0
		if key.kind == .ident && key.value.len > 0 && key.value !in roles {
			roles[key.value] = if has_index { u8(1) } else { u8(3) }
		}
		if has_index && value.value !in roles {
			roles[value.value] = 2
		}
	}
	t.comptime_reflected_for_roles = roles.move()
}

fn (t &Transformer) comptime_for_container_uses_reflected_field(container_id flat.NodeId) bool {
	if t.subtree_has_comptime_field_selector(container_id) {
		return true
	}
	if int(container_id) < 0 || int(container_id) >= t.a.nodes.len {
		return false
	}
	// This helper only reads AST nodes while the borrow is live.
	container := unsafe { &t.a.nodes[int(container_id)] }
	if container.kind != .ident || container.value.len == 0 {
		return false
	}
	// A common reflection pattern first saves `value := object.$(field.name)`
	// and then iterates `value`. Find that source declaration in the template;
	// its cloned concrete declaration no longer contains the `$` selector.
	for candidate_id in 0 .. t.a.nodes.len {
		// This read-only scan keeps the borrowed AST node valid.
		candidate := unsafe { &t.a.nodes[candidate_id] }
		if candidate.kind != .decl_assign || candidate.children_count < 2 {
			continue
		}
		lhs := t.a.child_node(candidate, 0)
		if lhs.kind != .ident || lhs.value != container.value {
			continue
		}
		if t.subtree_has_comptime_field_selector(t.a.child(candidate, 1)) {
			return true
		}
	}
	return false
}

fn (t &Transformer) subtree_has_comptime_field_selector(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	// This helper only reads AST nodes while the borrow is live.
	node := unsafe { &t.a.nodes[int(id)] }
	if node.kind == .selector && node.value == '\$' {
		return true
	}
	for i in 0 .. node.children_count {
		if t.subtree_has_comptime_field_selector(t.a.child(node, i)) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) make_comptime_enum_value(item EnumValueMeta) flat.NodeId {
	if item.value_from_c {
		return t.a.add_node(flat.Node{
			kind:  .enum_val
			value: '${item.enum_name_text()}.${item.name}'
			typ:   item.enum_name
		})
	}
	literal := t.make_int_literal_typed(item.value.str(), flat.source_type_i64)
	return t.make_cast(item.enum_name, literal, item.enum_name)
}

// clone_variant_subst clones a `$for variant in Sum.variants` body and gives the variant loop
// variable its dual meaning: a VariantData value in ordinary expressions and a concrete type in
// `is`/`$if`/`typeof(variant.typ)` compile-time positions.
fn (mut t Transformer) clone_variant_subst(id flat.NodeId, var_name string, item VariantMeta) ?flat.NodeId {
	return t.clone_variant_subst_with_smartcast(id, var_name, item, '', u32(0), []string{})
}

fn (mut t Transformer) clone_variant_subst_with_smartcast(id flat.NodeId, var_name string, item VariantMeta, smartcast_name string, return_context_id u32, inner_vars []string) ?flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	if node.kind == .string_literal && node.children_count > 0
		&& node.value in ['__v3_comptime_zero', '__v3_comptime_new'] {
		// The marker can share its loop-variable leaf with the discarded generic
		// template. The marker itself is sufficient to recover the variant type
		// after that leaf has been pruned.
		if member := t.variant_type_member(t.a.child(&node, 0), var_name, true) {
			target_source_id_2 := t.variant_member_type(member, item)

			return if node.value == '__v3_comptime_new' {
				t.comptime_new_value(target_source_id_2)
			} else {
				t.comptime_zero_value(target_source_id_2)
			}
		}
	}
	if comptime_for_declares_var(node, var_name) {
		return t.clone_node_preserving_children(node)
	}
	if node.kind == .ident && node.value == var_name {
		return t.make_variant_data_literal(item)
	}
	if node.kind == .cast_expr && node.children_count > 0 {
		operand := t.a.child_node(&node, 0)
		if operand.kind == .ident && operand.value == var_name {
			// `T(v)` wraps a zero value of the variant's type in the sum type
			// (the VariantData literal is only the loop var's runtime carrier).
			mut zero := t.comptime_zero_value(item.typ)
			if t.comptime_typeof_unaliased_type(item.typ) != item.typ {
				// An alias variant's zero value has its base type; keep the alias, so
				// `Foo | Alias` selects `Alias` rather than `Foo`.
				zero = t.make_cast(item.typ, zero, item.typ)
			}
			return t.make_cast(node.source_value_id(), zero, node.source_value_id())
		}
	}
	if node.kind == .selector && node.children_count > 0 {
		if member := t.typeof_arg_variant_member(t.a.child(&node, 0), var_name) {
			match node.value {
				'name' {
					return t.make_string_literal(flat.demangle_named_variants(flat.source_type_name(t.variant_member_type(member,
						item))))
				}
				'idx' {
					if member == 'typ' {
						return t.make_int_literal(item.typ_id)
					}
				}
				else {}
			}
		}
	}
	if node.kind == .typeof_expr {
		if member := t.typeof_arg_variant_member(id, var_name) {
			return t.make_string_literal(flat.demangle_named_variants(flat.source_type_name(t.variant_member_type(member,
				item))))
		}
	}
	if node.kind == .selector && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind == .ident && base.value == var_name && node.value == 'typ' {
			return t.make_int_literal(item.typ_id)
		}
	}
	mut comptime_cond := ''
	if node.kind == .comptime_if && (comptime_cond_references_ident(node.value, var_name)
		|| (smartcast_name != '' && comptime_cond_references_ident(node.value, smartcast_name))) {
		mut cond := t.subst_variant_cond(node.value, var_name, item)
		if smartcast_name != '' {
			cond = comptime_cond_replace_bare_ident(cond, smartcast_name, item.typ_text())
		}
		if !comptime_cond_has_any_loop_member_ref(cond, inner_vars) {
			if !comptime_cond_has_loop_member_ref(cond, var_name) {
				if taken := t.eval_reflected_string_condition(cond, node) {
					branch_idx := if taken { 0 } else { 1 }
					if branch_idx >= int(node.children_count) {
						return none
					}
					return t.clone_variant_subst_with_smartcast(t.a.child(&node, branch_idx), var_name, item, smartcast_name, return_context_id, inner_vars)
				}
			}
			if !t.comptime_condition_needs_local_value(cond, node) {
				t.reject_unevaluated_comptime_if(id, node, cond)
			}
		}
		// A nested `$for` decides the rest, with this loop's variable already substituted.
		comptime_cond = cond
	}
	mut branch_smartcast := ''
	if node.kind == .if_expr && node.children_count >= 2 {
		cond := t.a.child_node(&node, 0)
		if cond.kind == .is_expr && cond.source_value_id() in [
			flat.source_type_id(var_name),
			item.typ,
		] && cond.children_count > 0 {
			base := t.a.child_node(cond, 0)
			if base.kind == .ident {
				branch_smartcast = base.value
			}
		}
	}
	argument_types_source_ids := t.generic_clone_call_param_types(node, t.active_specialization_args.ids, return_context_id)

	child_vars := comptime_nested_loop_vars(node, var_name, inner_vars)
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_smartcast := if i == 1 && branch_smartcast.len > 0 {
			branch_smartcast
		} else {
			smartcast_name
		}
		if child := t.clone_variant_subst_with_smartcast(t.a.child(&node, i), var_name, item, child_smartcast, t.generic_clone_child_return_context(node, i, return_context_id, argument_types_source_ids.clone()), child_vars) {
			children << child
		}
	}
	if node.kind == .cast_expr && children.len == 1 {
		target_sum_source_id := t.resolve_sum_name(node.source_value_id())
		target_sum := target_sum_source_id
		child := t.a.nodes[int(children[0])]
		child_type_source_id := t.node_type(children[0])

		if t.sum_types.contains(target_sum) && child.kind == .ident
			&& t.sum_target_accepts_variant_type(target_sum_source_id, item.typ)
			&& (child_type_source_id == 0 || t.resolve_sum_name(child_type_source_id) == target_sum_source_id
				|| t.normalize_type_alias(child_type_source_id) == t.normalize_type_alias(item.typ)) {
			t.set_node_typ(int(children[0]), item.typ)
			return t.make_sum_literal(target_sum_source_id, item.typ, children[0])
		}
	}
	mut call_node := node
	if node.kind == .call && smartcast_name != '' && !t.call_has_source_generic_args(node) {
		// The checker can leave an inferred generic argument from the sum alias.
		// Each reflected variant needs inference from its narrowed argument instead.
		if call_node.call_type_args().len > 0 {
			call_node.set_call_type_args([]u32{})
		}
	}
	retargeted_call_type := if node.kind == .call && smartcast_name != '' {
		t.retarget_cloned_generic_call(call_node, mut children, t.active_specialization_args.ids, return_context_id)
	} else {
		u32(0)
	}
	mut typ := (if retargeted_call_type != 0 {
		retargeted_call_type
	} else {
		node.typ
	})
	if node.kind == .ident && smartcast_name != '' && node.value == smartcast_name {
		typ = item.typ
	} else if node.kind == .ident && t.mut_param_values[node.value] {
		storage_type_id := t.var_type(node.value)
		if flat.source_type_shape(storage_type_id).has(.pointer) {
			typ = flat.source_type_syntax(storage_type_id).raw_base()
		} else if flat.source_type_shape(typ).has(.pointer) {
			typ = flat.source_type_syntax(typ).raw_base()
		}
	} else if node.kind == .ident && node.value.len > 0 {
		local_type_source_id := t.raw_var_type(node.value)
		local_type_source_id_2 := local_type_source_id

		if local_type_source_id != 0 && !t.generic_arg_is_unresolved(local_type_source_id) {
			typ = local_type_source_id_2
		}
	}
	if t.specialization_node_start >= 0 && node.kind == .decl_assign && children.len >= 2 {
		rhs := t.a.nodes[int(children[1])]
		rhs_typ := (if rhs.kind == .ident && smartcast_name != '' && rhs.value == smartcast_name
			&& rhs.typ != 0 {
			rhs.typ
		} else {
			t.node_type(children[1])
		})
		if rhs_typ != 0 && rhs_typ !in [flat.source_type_unknown, flat.source_type_generic] {
			typ = rhs_typ
			t.set_node_typ(int(children[0]), rhs_typ)
			lhs := t.a.nodes[int(children[0])]
			if lhs.kind == .ident && lhs.value.len > 0 {
				t.set_var_type(lhs.value, rhs_typ)
			}
		}
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	mut cloned_metadata := flat.Node{
		payload: if node.payload != 0 { node.clone_owned().payload } else { u32(0) }
	}
	if node.kind == .call && retargeted_call_type != 0 && node.call_type_args().len > 0 {
		cloned_metadata.set_call_type_args([]u32{})
	}
	clone_id := t.a.add_node(flat.Node{
		kind:           node.kind
		op:             node.op
		pos:            node.pos
		value:          if node.kind == .is_expr && node.source_value_id() == flat.source_type_id(var_name) {
			''
		} else if comptime_cond.len > 0 {
			comptime_cond
		} else {
			node.value
		}
		source_value:   if node.kind == .is_expr && node.source_value_id() == flat.source_type_id(var_name) {
			item.typ
		} else {
			node.source_value
		}
		payload:        cloned_metadata.payload
		typ:            typ
		is_mut:         node.is_mut
		flags:          node.flags & comptime_clone_syntax_flags
		children_start: start
		children_count: flat.child_count(children.len)
	})
	if node.kind == .ident && smartcast_name != '' && node.value == smartcast_name {
		// Keep the loop variant refinement separate from the function-scope type.
		// Later generic-call inference runs after the smartcast stack has unwound.
		t.record_refined_node_type(int(clone_id), item.typ)
	}
	if node.kind == .ident && t.mut_param_values[node.value] {
		t.mut_value_ident_nodes[int(clone_id)] = true
	}
	return clone_id
}

// typeof_arg_variant_member returns `typ` for `typeof(v.typ_text())` and `unaliased_typ` for
// `typeof(v.typ_text().unaliased_typ_text())`, where `v` is the variant loop variable `var_name`.
fn (t &Transformer) typeof_arg_variant_member(id flat.NodeId, var_name string) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.node(id)
	if node.kind != .typeof_expr || node.children_count == 0 {
		return none
	}
	return t.variant_type_member(t.a.child(node, 0), var_name, false)
}

// variant_type_member returns `typ` for `v.typ_text()` and `unaliased_typ` for
// `v.typ_text().unaliased_typ_text()`, where `v` is the variant loop variable `var_name`. With
// `allow_pruned`, an emptied `v` leaf is accepted too.
fn (t &Transformer) variant_type_member(id flat.NodeId, var_name string, allow_pruned bool) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	mut sel := t.a.node(id)
	if sel.kind != .selector || sel.children_count == 0 {
		return none
	}
	member := sel.value
	if member == 'unaliased_typ' {
		sel = t.a.child_node(sel, 0)
		if sel.kind != .selector || sel.children_count == 0 {
			return none
		}
	} else if member != 'typ' {
		return none
	}
	if sel.value != 'typ' {
		return none
	}
	base := t.a.child_node(sel, 0)
	if (allow_pruned && base.kind == .empty) || (base.kind == .ident && base.value == var_name) {
		return member
	}
	return none
}

// variant_member_type is the type named by `v.typ_text()` or `v.typ_text().unaliased_typ_text()` for the
// variant `item`: an alias variant keeps its name for `typ` and is unwrapped for
// `unaliased_typ`.
fn (t &Transformer) variant_member_type(member string, item VariantMeta) u32 {
	return if member == 'unaliased_typ' {
		t.comptime_typeof_unaliased_type(item.typ)
	} else {
		item.typ
	}
}

fn (t &Transformer) subst_variant_cond(cond string, var_name string, item VariantMeta) string {
	mut result := comptime_cond_replace_int_compared(cond, '${var_name}.typ', item.typ_id.str())
	result = result.replace('${var_name}.typ', item.typ_text())
	result = comptime_cond_replace_bare_ident(result, var_name, item.typ_text())
	return result
}

// make_enum_data_literal builds `EnumData{name: '<name>', value: <value>, attrs: [...]}`.
fn (mut t Transformer) make_enum_data_literal(item EnumValueMeta) flat.NodeId {
	name_field := t.make_named_field_init('name', t.make_string_literal(item.name), flat.source_type_string)
	value := if item.value_from_c {
		t.make_cast(flat.source_type_i64, t.make_comptime_enum_value(item), flat.source_type_i64)
	} else {
		t.make_int_literal_typed(item.value.str(), flat.source_type_i64)
	}
	value_field := t.make_named_field_init('value', value, flat.source_type_i64)
	attrs_field := t.make_named_field_init('attrs', t.make_string_array_literal(item.attrs), flat.source_type_id('[]string'))
	attrs_attributes_field := t.make_named_field_init('attributes', t.make_attribute_array_literal(item.attribute_metas()), flat.source_type_id('[]VAttribute'))
	start := t.a.children.len
	t.a.children << name_field
	t.a.children << value_field
	t.a.children << attrs_field
	t.a.children << attrs_attributes_field
	literal_type_id := flat.source_type_id('EnumData')
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		source_value:   literal_type_id
		typ:            literal_type_id
		children_start: start
		children_count: 4
	})
}

fn (mut t Transformer) make_variant_data_literal(item VariantMeta) flat.NodeId {
	typ_field := t.make_named_field_init('typ', t.make_int_literal(item.typ_id), flat.source_type_int)
	start := t.a.children.len
	t.a.children << typ_field
	literal_type_id := flat.source_type_id('VariantData')
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		source_value:   literal_type_id
		typ:            literal_type_id
		children_start: start
		children_count: 1
	})
}

// make_field_data_literal builds a runtime `FieldData` value for a bare `$for field` variable.
fn (mut t Transformer) make_field_data_literal(fm FieldMeta) flat.NodeId {
	fields := [
		t.make_named_field_init('name', t.make_string_literal(fm.name), flat.source_type_string),
		t.make_named_field_init('typ', t.make_int_literal(fm.typ_id), flat.source_type_int),
		t.make_named_field_init('unaliased_typ', t.make_int_literal(fm.unaliased_id), flat.source_type_int),
		t.make_named_field_init('attrs', t.make_string_array_literal(fm.attrs), flat.source_type_id('[]string')),
		t.make_named_field_init('attributes', t.make_attribute_array_literal(fm.attribute_metas()), flat.source_type_id('[]VAttribute')),
		t.make_named_field_init('is_pub', t.make_bool_literal(fm.is_pub), flat.source_type_bool),
		t.make_named_field_init('is_mut', t.make_bool_literal(fm.is_mut), flat.source_type_bool),
		t.make_named_field_init('is_embed', t.make_bool_literal(fm.is_embed), flat.source_type_bool),
		t.make_named_field_init('is_shared', t.make_bool_literal(fm.is_shared), flat.source_type_bool),
		t.make_named_field_init('is_atomic', t.make_bool_literal(fm.is_atomic), flat.source_type_bool),
		t.make_named_field_init('is_option', t.make_bool_literal(fm.is_option), flat.source_type_bool),
		t.make_named_field_init('is_array', t.make_bool_literal(fm.is_array), flat.source_type_bool),
		t.make_named_field_init('is_map', t.make_bool_literal(fm.is_map), flat.source_type_bool),
		t.make_named_field_init('is_chan', t.make_bool_literal(fm.is_chan), flat.source_type_bool),
		t.make_named_field_init('is_enum', t.make_bool_literal(fm.is_enum), flat.source_type_bool),
		t.make_named_field_init('is_struct', t.make_bool_literal(fm.is_struct), flat.source_type_bool),
		t.make_named_field_init('is_alias', t.make_bool_literal(fm.is_alias), flat.source_type_bool),
		t.make_named_field_init('indirections', t.make_int_literal(fm.indirections), flat.source_type_u8),
	]
	start := t.a.children.len
	for field in fields {
		t.a.children << field
	}
	literal_type_id := flat.source_type_id('FieldData')
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		source_value:   literal_type_id
		typ:            literal_type_id
		children_start: start
		children_count: flat.child_count(fields.len)
	})
}

fn (mut t Transformer) make_named_field_init(field string, value flat.NodeId, typ_id u32) flat.NodeId {
	start := t.a.children.len
	t.a.children << value
	return t.a.add_node(flat.Node{
		kind:           .field_init
		value:          field
		typ:            typ_id
		children_start: start
		children_count: 1
	})
}

// The clones a comptime `$for` makes keep an assignment's `@[freed]`, which
// -warn-about-allocs reads off the declaration it expanded.
fn (mut t Transformer) clone_node_preserving_children(node flat.Node) flat.NodeId {
	return t.clone_node_preserving_children_with_type(node, node.typ)
}

fn (mut t Transformer) clone_node_preserving_children_with_type(node flat.Node, typ_id u32) flat.NodeId {
	start := t.a.children.len
	for i in 0 .. node.children_count {
		t.a.children << t.a.child(&node, i)
	}
	return t.a.add_node(flat.Node{
		kind:           node.kind
		op:             node.op
		pos:            node.pos
		value:          node.value
		source_value:   node.source_value
		payload:        if node.payload != 0 { node.clone_owned().payload } else { u32(0) }
		typ:            typ_id
		is_mut:         node.is_mut
		flags:          node.flags & comptime_clone_syntax_flags
		children_start: start
		children_count: node.children_count
	})
}

// comptime_body_has_unsupported_late_generic reports whether inference marked a generic call
// that could not be specialized after the normal monomorphization pass.
fn (t &Transformer) comptime_body_has_unsupported_late_generic(stmts []flat.NodeId) bool {
	for sid in stmts {
		if t.subtree_has_unsupported_late_generic(sid) {
			return true
		}
	}
	return false
}

// comptime_body_has_unsupported reports whether any statement uses a comptime construct the unroll
// cannot yet model: a `match <var>.typ_text()` type match, or (after substitution) `typeof(...)` that
// still references the loop variable.
fn (t &Transformer) comptime_body_has_unsupported(stmts []flat.NodeId, var_name string, reject_typeof bool) bool {
	for sid in stmts {
		if t.subtree_has_unsupported_comptime(sid, var_name, reject_typeof) {
			return true
		}
	}
	return false
}

fn (t &Transformer) subtree_has_unsupported_comptime(id flat.NodeId, var_name string, reject_typeof bool) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.node(id)
	// ORM (`sql db { select ... }`) mixes comptime field names with a query DSL.
	if node.kind in [.sql_expr, .select_stmt] {
		return true
	}
	if reject_typeof && node.kind == .typeof_expr && t.subtree_references_var(id, var_name) {
		return true
	}
	// A `field.member` / `field.$(...)` selector is a supported comptime access: check its other
	// children but not the base loop-var ident (which is not a bare use).
	if node.kind == .selector && node.children_count > 0 {
		base := t.a.child_node(node, 0)
		if base.kind == .ident && base.value == var_name {
			for i in 1 .. node.children_count {
				if t.subtree_has_unsupported_comptime(t.a.child(node, i), var_name, reject_typeof) {
					return true
				}
			}
			return false
		}
	}
	for i in 0 .. node.children_count {
		if t.subtree_has_unsupported_comptime(t.a.child(node, i), var_name, reject_typeof) {
			return true
		}
	}
	return false
}

fn (t &Transformer) subtree_references_var(id flat.NodeId, var_name string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.node(id)
	if node.kind == .ident && node.value == var_name {
		return true
	}
	for i in 0 .. node.children_count {
		if t.subtree_references_var(t.a.child(node, i), var_name) {
			return true
		}
	}
	return false
}

fn (t &Transformer) subtree_has_unsupported_late_generic(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.node(id)
	if node.kind == .call && node.value == comptime_unsupported_late_generic_call {
		return true
	}
	for i in 0 .. node.children_count {
		if t.subtree_has_unsupported_late_generic(t.a.child(node, i)) {
			return true
		}
	}
	return false
}

// FieldDeclMeta holds the per-field mutability/visibility/attributes recorded by the parser on a
// `field_decl` node, keyed by field name (see struct_field_decl_metas).
struct FieldDeclMeta {
	is_mut     bool
	is_pub     bool
	attr_kinds u32 // see flat.Node.attribute_kinds_id
	attrs      []string
}

// comptime_field_metas derives FieldData for every field of the concrete struct type.
fn (mut t Transformer) comptime_field_metas(base_type_id u32) []FieldMeta {
	if interface_metas := t.comptime_interface_field_metas(base_type_id) {
		return interface_metas
	}
	clean_base_id := t.trimmed_type_id(base_type_id)
	clean_shape := flat.source_type_shape(clean_base_id)
	// Builtin runtime representations such as `string` and `array` are structs
	// in the generated C ABI, but they are not language-level structs and must
	// not expose those implementation fields through `T.fields`.
	if comptime_is_primitive_type_id(clean_base_id) || clean_shape.has(.array_type)
		|| clean_shape.has(.map_type) || clean_shape.has(.channel_type)
		|| flat.source_type_syntax(clean_base_id).function_space_prefix()
		|| clean_shape.has(.leading_bracket) {
		return []FieldMeta{}
	}
	// A monomorphized generic struct instance (`Box[int]`) is stored in the struct table under
	// its generic declaration name (`Box`), so a direct lookup misses; resolve it through the
	// generic-struct field substitution path before giving up.
	info := t.lookup_struct_info(base_type_id) or {
		t.generic_struct_info_for_stringify(base_type_id) or { return []FieldMeta{} }
	}
	cache_key := comptime_struct_info_cache_key(info)
	if cached := t.comptime_field_metas_cache[cache_key] {
		return cached
	}
	decl_metas := t.struct_field_decl_metas_in_module(cache_key, info.module)
	mut metas := []FieldMeta{cap: info.fields.len}
	for f in info.fields {
		// V's `field.typ_text()` is the type as written (`MyInt`, `?[]int`); `raw_typ` preserves that,
		// while `f.typ_text()` was already alias-resolved in the struct's declaring module during
		// collect_types. Classify using `f.typ_text()` (and `info.module` for the alias check) so a
		// field type declared in another module is not mis-resolved against the caller's module.
		ftyp := if f.raw_typ != 0 { f.raw_typ } else { f.typ }
		// Fields whose declaration node we could not introspect default to the previous
		// public/immutable behaviour; found fields carry their real modifiers/attrs.
		extra := decl_metas[f.name] or {
			FieldDeclMeta{
				is_pub: true
			}
		}
		field_name := comptime_reflected_field_name(f.name, ftyp, f.is_embedded)
		metas << t.field_meta_for(field_name, ftyp, f.typ, info.module, f.is_embedded, extra)
	}
	t.comptime_field_metas_cache[cache_key] = metas
	return metas
}

fn (mut t Transformer) comptime_interface_field_metas(base_type_id u32) ?[]FieldMeta {
	if isnil(t.tc) {
		return none
	}
	mut clean := t.trimmed_type_id(base_type_id)
	for flat.source_type_shape(clean).has(.pointer) || flat.source_type_shape(clean).has(.optional) {
		clean = flat.source_type_syntax(clean).raw_base()
	}
	mut iface_name := clean
	if !t.generic_checker_interface_name_known(iface_name) {
		// Qualification consults declaration and import names at their resolution boundary.
		qualified := t.tc.qualify_type_name(clean)
		if !t.generic_checker_interface_name_known(qualified) {
			return none
		}
		iface_name = qualified
	}
	mut seen_ifaces := map[u32]bool{}
	mut seen_fields := map[string]bool{}
	mut metas := []FieldMeta{}
	t.collect_comptime_interface_field_metas(iface_name, mut seen_ifaces, mut seen_fields, mut metas)
	return metas
}

fn (mut t Transformer) collect_comptime_interface_field_metas(iface_name_id u32, mut seen_ifaces map[u32]bool, mut seen_fields map[string]bool, mut metas []FieldMeta) {
	if iface_name_id in seen_ifaces {
		return
	}
	seen_ifaces[iface_name_id] = true
	for embedded_source_id in t.tc.interface_embeds.type_ids(iface_name_id) or { []u32{} } {
		t.collect_comptime_interface_field_metas(embedded_source_id, mut seen_ifaces, mut seen_fields, mut metas)
	}
	decl_module := flat.source_type_name(if flat.source_type_shape(iface_name_id).has(.dotted) {
		flat.source_type_syntax(iface_name_id).qualifier()
	} else {
		iface_name_id
	})
	for field in t.tc.interface_fields[iface_name_id] or { []types.StructField{} } {
		if field.name in seen_fields {
			continue
		}
		seen_fields[field.name] = true
		field_type := field.typ.source_type_id()
		metas << t.field_meta_for(field.name, field_type, field_type, decl_module, false, FieldDeclMeta{
			is_mut: field.is_mut
			is_pub: true
		})
	}
}

fn comptime_struct_info_cache_key(info StructInfo) u32 {
	if info.module.len == 0 {
		return info.name
	}
	name := info.name_text()
	if name.starts_with('${info.module}.')
		|| name.starts_with('${c_name(info.module)}__') {
		return info.name
	}
	return flat.source_type_id('${info.module}.${name}')
}

fn (mut t Transformer) build_struct_field_decl_metas_cache() {
	mut cache := map[u32]map[string]FieldDeclMeta{}
	mut cur_mod := ''
	mut cur_file_id := 0
	// The checker's top-level index lists the declarations of the nodes it
	// covers, with their file markers; the nodes appended behind it, by lowering,
	// are read one by one, their module following their position.
	indexed_count, indexed_nodes := t.top_level_scan_prefix()
	count := indexed_count + t.a.nodes.len - indexed_nodes
	for ii in 0 .. count {
		use_idx := ii < indexed_count
		node_idx := if use_idx {
			int(t.tc.top_level_idx[ii])
		} else {
			indexed_nodes + ii - indexed_count
		}
		// The appended nodes are visited one by one; read one in place, and copy
		// only a struct declaration.
		scanned := unsafe { &t.a.nodes[node_idx] }
		if use_idx && scanned.kind == .file {
			cur_mod = t.tc.file_modules[scanned.value] or { 'main' }
			continue
		}
		if !use_idx && scanned.pos.id > 0 && scanned.pos.id != cur_file_id {
			cur_file_id = scanned.pos.id
			// Files without an explicit module declaration belong to main.
			cur_mod = 'main'
		}
		if scanned.kind == .module_decl {
			cur_mod = scanned.value
			continue
		}
		if scanned.kind != .struct_decl {
			continue
		}
		node := *scanned
		node_ref := t.a.node(flat.NodeId(node_idx))
		mut fields := map[string]FieldDeclMeta{}
		for i in 0 .. node.children_count {
			field := t.a.child_node(node_ref, i)
			if field.kind == .field_decl {
				fields[field.value] = field_decl_meta(field)
			}
		}
		name_id := node.source_value_id()
		if cur_mod.len > 0 {
			cache[flat.source_type_id('${cur_mod}.${node.value}')] = fields.clone()
		}
		// Bare names are used for unqualified/main lookups. Builtin is parsed before
		// main, so let a main declaration replace an earlier builtin homonym.
		if cur_mod == 'main' || name_id !in cache {
			cache[name_id] = fields.clone()
		}
	}
	t.struct_field_decl_metas_cache = cache.move()
}

fn field_decl_meta(field flat.Node) FieldDeclMeta {
	params := field.generic_params()
	if params.len == 0 {
		return FieldDeclMeta{}
	}
	flags := params[0]
	return FieldDeclMeta{
		is_mut:     flags.contains('m')
		is_pub:     flags.contains('p')
		attrs:      params[1..].clone()
		attr_kinds: field.attribute_kinds_id()
	}
}

// comptime_attribute_kinds decodes the attribute kinds the parser recorded for a field or enum
// value. It returns no kinds when they were not recorded or do not match the attributes; the
// kinds are then derived from the attribute text.
fn comptime_attribute_kinds(kinds_id u32, attr_count int) []int {
	if kinds_id == 0 || attr_count == 0 {
		return []int{}
	}
	kinds := flat.source_type_name(kinds_id).split(',').map(it.int())
	if kinds.len != attr_count {
		return []int{}
	}
	return kinds
}

// struct_field_decl_metas returns parser metadata for the declaration of `base_type`, resolving a
// generic instance such as `Box[int]` to its base `Box`.
fn (t &Transformer) struct_field_decl_metas(base_type_id u32) map[string]FieldDeclMeta {
	return t.struct_field_decl_metas_in_module(base_type_id, '')
}

fn (t &Transformer) struct_field_decl_metas_in_module(base_type_id u32, decl_module string) map[string]FieldDeclMeta {
	decl_name := flat.source_type_syntax(t.trimmed_type_id(base_type_id)).generic_base()
	mut short_name := decl_name
	if decl_module != '' {
		syntax := flat.source_type_syntax(decl_name)
		module_id := flat.source_type_id(decl_module)
		mut found_module := false
		for i, prefix in syntax.qualifier_prefixes() {
			if prefix == module_id {
				short_name = syntax.qualifier_suffixes()[i]
				found_module = true
				break
			}
		}
		if !found_module {
			// Module declarations supply the serialized C name prefix here.
			encoded_module_prefix := flat.source_type_id(c_name(decl_module) + '_')
			for i, prefix in syntax.underscore_prefixes() {
				if prefix == encoded_module_prefix {
					short_name = syntax.underscore_suffixes()[i]
					break
				}
			}
		}
	}
	if decl_module != '' {
		qualified := flat.source_type_composition_two(.qualified, flat.source_type_id(decl_module), short_name)
		if cached := t.struct_field_decl_metas_cache[qualified] {
			return cached
		}
	}
	lookup_name := if decl_module in ['main', 'builtin'] { short_name } else { decl_name }
	if cached := t.struct_field_decl_metas_cache[lookup_name] {
		if decl_module == '' || decl_module in ['main', 'builtin'] {
			return cached
		}
	}
	// Materialized generic instances can reach comptime expansion under their C
	// name (`Box_int`, `mod__Box_int`) rather than bracket syntax. The cache is
	// keyed by source declarations, so recover the generic declaration prefix.
	mut cur_mod := ''
	mut cur_file_id := 0
	for node_id in 0 .. t.a.nodes.len {
		// This read-only scan keeps the borrowed AST node valid.
		node := unsafe { &t.a.nodes[node_id] }
		if node.pos.id > 0 && node.pos.id != cur_file_id {
			cur_file_id = node.pos.id
			cur_mod = 'main'
		}
		if node.kind == .module_decl {
			cur_mod = node.value
			continue
		}
		if node.kind != .struct_decl || node.generic_params().len == 0 {
			continue
		}
		if decl_module != '' && cur_mod != decl_module {
			continue
		}
		qualified := if cur_mod.len > 0 {
			flat.source_type_composition_two(.qualified, flat.source_type_id(cur_mod), node.source_value_id())
		} else {
			node.source_value_id()
		}
		// These declaration identifiers enter the emitted-name codec once;
		// matching an already interned type uses its cached separator positions.
		for prefix in [node.source_value_id(), qualified, flat.source_type_id(c_name(node.value)),
			flat.source_type_id(c_name(flat.source_type_name(qualified)))] {
			if prefix == 0 || prefix !in flat.source_type_syntax(lookup_name).underscore_prefixes() {
				continue
			}
			if decl_module == '' {
				if cached := t.struct_field_decl_metas_cache[node.source_value_id()] {
					return cached
				}
			}
			if cached := t.struct_field_decl_metas_cache[qualified] {
				return cached
			}
		}
	}
	return map[string]FieldDeclMeta{}
}

fn (t &Transformer) field_meta_for(name string, ftyp_id u32, resolved_typ_id u32, decl_module string, is_embed bool, extra FieldDeclMeta) FieldMeta {
	is_option := flat.source_type_shape(ftyp_id).has(.option_type)
	mut core_id := if is_option {
		t.trimmed_type_id(flat.source_type_syntax(ftyp_id).raw_base())
	} else {
		ftyp_id
	}
	mut indir := 0
	mut is_shared := false
	mut is_atomic := false
	// A `shared T` field is stored behind a lock guard; V reports one level of indirection.
	if flat.source_type_shape(core_id).has(.shared_type) {
		is_shared = true
		indir++
		core_id = t.trimmed_type_id(flat.source_type_syntax(core_id).raw_base())
	} else if flat.source_type_shape(core_id).has(.atomic_type) {
		is_atomic = true
		core_id = t.trimmed_type_id(flat.source_type_syntax(core_id).raw_base())
	}
	for flat.source_type_shape(core_id).has(.pointer) {
		indir++
		core_id = flat.source_type_syntax(core_id).raw_base()
	}
	unaliased_source_id := if resolved_typ_id != 0 {
		t.trimmed_type_id(resolved_typ_id)
	} else {
		t.comptime_normalize_type_alias_chain(ftyp_id)
	}
	unaliased_core_id := comptime_strip_field_wrappers(unaliased_source_id)
	unaliased_shape := flat.source_type_shape(unaliased_core_id)
	is_alias := t.field_type_is_alias(core_id, decl_module)
	return FieldMeta{
		name:                name
		typ:                 ftyp_id
		unaliased_typ:       unaliased_source_id
		comptime_typ:        t.comptime_field_type_id_key(ftyp_id, decl_module)
		comptime_unaliased:  t.comptime_field_type_id_key(unaliased_source_id, decl_module)
		typ_id:              t.type_index(ftyp_id, decl_module)
		unaliased_id:        t.type_index(unaliased_source_id, decl_module)
		is_option:           is_option
		is_embed:            is_embed
		is_array:            unaliased_shape.has(.array_type) || t.is_fixed_array_type(unaliased_core_id)
		is_map:              unaliased_shape.has(.map_type)
		is_chan:             unaliased_shape.has(.channel_type)
		is_struct:           t.comptime_field_type_is_struct(unaliased_core_id)
		is_enum:             t.comptime_enum_type_known(unaliased_core_id)
		is_alias:            is_alias
		is_shared:           is_shared
		is_atomic:           is_atomic
		is_mut:              extra.is_mut
		is_pub:              extra.is_pub
		attrs:               extra.attrs
		attr_kinds:          extra.attr_kinds
		attribute_module_id: if extra.attrs.len > 0 {
			flat.source_type_id(decl_module)
		} else {
			u32(0)
		}
		indirections:        indir
	}
}

fn (t &Transformer) comptime_enum_type_known(raw_id u32) bool {
	clean_id := t.trimmed_type_id(raw_id)
	if t.generic_enum_type_name_known(clean_id) { return true }
	// Reflected main types use explicit qualification; imported modules remain distinct.
	return flat.source_type_query(clean_id).has(.main_prefix)
		&& t.generic_enum_type_name_known(flat.source_type_syntax(clean_id).main_base())
}

fn (t &Transformer) comptime_field_type_is_struct(typ_id u32) bool {
	clean_id := transform_trimmed_type_id(typ_id)
	if clean_id == 0 || comptime_is_primitive_type_id(clean_id) { return false }
	return t.comptime_struct_type_known(clean_id)
}

fn comptime_generic_type_base(typ_id u32) u32 {
	clean_id := transform_trimmed_type_id(typ_id)
	shape := flat.source_type_shape(clean_id)
	syntax := flat.source_type_syntax(clean_id)
	if shape.has(.leading_bracket) || !shape.has(.bracketed) { return 0 }
	return transform_trimmed_type_id(syntax.generic_base())
}

fn parse_comptime_generic_type_base(typ string) string {
	return flat.source_type_name(comptime_generic_type_base(flat.source_type_id(typ)))
}

fn (t &Transformer) type_index(typ_id u32, decl_module string) int {
	mut key_id := t.comptime_field_type_id_key(typ_id, decl_module)
	// Comptime metadata exposes the wrapped type's index; runtime reflection
	// carries option/result flags separately from that index.
	for flat.source_type_shape(key_id).has(.optional) {
		key_id = transform_trimmed_type_id(flat.source_type_syntax(key_id).raw_base())
	}
	return t.tc.type_index(key_id)
}

fn (t &Transformer) comptime_field_type_id_key(typ_id u32, decl_module string) u32 {
	core_id := transform_trimmed_type_id(typ_id)
	if core_id == 0 { return 0 }
	syntax := flat.source_type_syntax(core_id)
	shape := flat.source_type_shape(core_id)
	if shape.has(.optional) {
		kind := if shape.has(.option_type) {
			flat.SourceTypeComposition.option
		} else {
			flat.SourceTypeComposition.result
		}
		return flat.source_type_composition_one(kind,
			t.comptime_field_type_id_key(syntax.raw_base(), decl_module))
	}
	if shape.has(.mutable_type) {
		return flat.source_type_composition_one(.mutable_parameter,
			t.comptime_field_type_id_key(syntax.raw_base(), decl_module))
	}
	if shape.has(.shared_type) {
		return flat.source_type_composition_one(.shared_type,
			t.comptime_field_type_id_key(syntax.raw_base(), decl_module))
	}
	if shape.has(.atomic_type) {
		return flat.source_type_composition_one(.atomic_type,
			t.comptime_field_type_id_key(syntax.raw_base(), decl_module))
	}
	if shape.has(.pointer) {
		mut out_id := t.comptime_field_type_id_key(syntax.pointer_base(), decl_module)
		for _ in 0 .. syntax.pointer_depth() {
			out_id = flat.source_type_composition_one(.pointer, out_id)
		}
		return out_id
	}
	if shape.has(.array_type) {
		return flat.source_type_composition_one(.array,
			t.comptime_field_type_id_key(syntax.raw_base(), decl_module))
	}
	if shape.has(.map_type) && syntax.bracket_closed() && syntax.raw_base() != 0 {
		return flat.source_type_composition_two(.map,
			t.comptime_field_type_id_key(syntax.bracket_inner(), decl_module),
			t.comptime_field_type_id_key(syntax.raw_base(), decl_module))
	}
	if shape.has(.leading_bracket) && syntax.bracket_closed() && syntax.raw_base() != 0 {
		return flat.source_type_composition_two(.fixed_array, syntax.bracket_raw_inner(),
			t.comptime_field_type_id_key(syntax.raw_base(), decl_module))
	}
	if t.is_fixed_array_type(core_id) && syntax.postfix_base() != 0 && syntax.postfix_dims().len > 0 {
		mut out_id := t.comptime_field_type_id_key(syntax.postfix_base(), decl_module)
		for dim_id in syntax.postfix_dims() {
			out_id = flat.source_type_composition_two(.fixed_array_suffix, dim_id, out_id)
		}
		return out_id
	}
	if syntax.tuple_like() && syntax.tuple_components().len > 1 {
		mut parts := []u32{cap: syntax.tuple_components().len}
		for part_id in syntax.tuple_components() {
			parts << t.comptime_field_type_id_key(part_id, decl_module)
		}
		return flat.source_type_composition(.multi_return, parts)
	}
	if shape.has(.function_compact) || shape.has(.function_spaced) {
		if !syntax.function_closed() { return core_id }
		mut parts := []u32{cap: syntax.function_params().len + 1}
		for param_id in syntax.function_params() {
			parts << t.comptime_field_type_id_key(param_id, decl_module)
		}
		qualified_ret_id := t.comptime_field_type_id_key(syntax.function_result(), decl_module)
		if qualified_ret_id != 0 {
			parts << qualified_ret_id
			return flat.source_type_composition(.function_type, parts)
		}
		return flat.source_type_composition(.void_function_type, parts)
	}
	if syntax.generic_app_valid() {
		mut parts := []u32{cap: syntax.generic_app_args().len + 1}
		parts << t.comptime_field_type_id_key(syntax.generic_app_base(), decl_module)
		for arg_id in syntax.generic_app_args() {
			parts << t.comptime_field_type_id_key(arg_id, decl_module)
		}
		return flat.source_type_composition(.generic, parts)
	}
	if flat.source_type_query(core_id).has(.generic_fn_name) || comptime_is_primitive_type_id(core_id) {
		return core_id
	}
	// A bare type substituted into an imported generic still belongs to the
	// caller's main module. Keep that provenance when producing stable type ids;
	// otherwise `typeof[T]().idx` is hashed as though the type were declared by
	// the generic function's module.
	if t.active_specialization_main_types[core_id] {
		return flat.source_type_composition_two(.qualified, generic_main_module_id, core_id)
	}
	if shape.has(.dotted) || shape.has(.bracketed) || syntax.spaced() || decl_module == 'builtin' {
		return core_id
	}
	if decl_module in ['', 'main'] {
		return if t.type_name_is_declared_id(core_id) {
			flat.source_type_composition_two(.qualified, generic_main_module_id, core_id)
		} else {
			core_id
		}
	}
	return flat.source_type_composition_two(.qualified, flat.source_type_id(decl_module), core_id)
}

fn comptime_is_primitive_type(typ string) bool {
	return comptime_is_primitive_type_id(flat.source_type_id(typ))
}

fn comptime_is_primitive_type_id(typ_id u32) bool {
	return typ_id in [flat.source_type_string, flat.source_type_bool, flat.source_type_rune,
		flat.source_type_char, flat.source_type_i8, flat.source_type_i16, flat.source_type_i32,
		flat.source_type_i64, flat.source_type_int, flat.source_type_isize, flat.source_type_u8,
		flat.source_type_u16, flat.source_type_u32, flat.source_type_u64, flat.source_type_uint,
		flat.source_type_f32, flat.source_type_f64, comptime_int_literal_type_id,
		comptime_float_literal_type_id, flat.source_type_voidptr, flat.source_type_byteptr,
		flat.source_type_charptr, flat.source_type_nil, flat.source_type_void]
}

// comptime_strip_field_wrappers removes the `?` option, `shared`/`atomic`, and `&` reference
// decorations from an (already alias-resolved) field type, yielding the core type used for
// metadata classification.
fn comptime_strip_field_wrappers(typ_id u32) u32 {
	mut core_id := transform_trimmed_type_id(typ_id)
	if flat.source_type_shape(core_id).has(.option_type) {
		core_id = transform_trimmed_type_id(flat.source_type_syntax(core_id).raw_base())
	}
	shape := flat.source_type_shape(core_id)
	if shape.has(.shared_type) || shape.has(.atomic_type) {
		core_id = transform_trimmed_type_id(flat.source_type_syntax(core_id).raw_base())
	}
	for flat.source_type_shape(core_id).has(.pointer) {
		core_id = flat.source_type_syntax(core_id).raw_base()
	}
	return core_id
}

// comptime_reflected_field_name returns V's source-level name for an embedded field. Generic
// embedded fields are stored internally under their full type application, but reflection exposes
// only the base type's last name segment (for example `veb.Middleware[Context]` as `Middleware`).
fn comptime_reflected_field_name(name string, typ_id u32, is_embed bool) string {
	if !is_embed { return name }
	mut core_id := comptime_strip_field_wrappers(typ_id)
	shape := flat.source_type_shape(core_id)
	if shape.has(.bracketed) && !shape.has(.leading_bracket) {
		core_id = flat.source_type_syntax(core_id).generic_base()
	}
	return flat.source_type_name(flat.source_type_syntax(core_id).unqualified())
}

// field_type_is_alias reports whether `core` names a `type X = Y` alias. An unqualified name is
// resolved in the struct's declaring module (`decl_module`) rather than the caller's cur_module,
// so cross-module reflection reports `is_alias` correctly.
fn (t &Transformer) field_type_is_alias(core_id u32, decl_module string) bool {
	if t.generic_exact_alias_name_known(core_id) { return true }
	if !flat.source_type_shape(core_id).has(.dotted) && decl_module !in ['', 'main', 'builtin'] {
		return t.generic_exact_alias_name_known(flat.source_type_composition_two(.qualified,
			flat.source_type_id(decl_module), core_id))
	}
	return false
}

// comptime_zero_value is the value of `$zero(typ)`, and the payload of `T(v)`: the
// zero value of `typ`. A literal zero has its default type (`0` is an `int`, `''` a
// `string`), so it is cast to `typ` whenever that differs: an `i32`, float, enum or
// alias type must keep its identity, or `T(v)` wraps it as another sum variant, or
// as none at all.
fn (mut t Transformer) comptime_zero_value(typ_id u32) flat.NodeId {
	mut zero_type := typ_id
	// In an imported specialization a program alias is locked as `main.Props`, but
	// program aliases are registered bare. Resolve it through its bare name, or its
	// zero value is a zeroed struct (`(map){0}` for a map alias, which crashes on use).
	if flat.source_type_query(typ_id).has(.main_prefix) && !t.ident_is_import_alias('main') && !isnil(t.tc) {
		if target_source_id := t.tc.type_aliases.type_id(flat.source_type_syntax(typ_id).main_base()) {
			zero_type = t.lock_colliding_main_generic_type_text(target_source_id, t.cur_module)
		}
	}
	zero_id := t.zero_value_for_type(zero_type)
	if t.generic_type_text_contains_alias(typ_id, t.cur_module) {
		return t.make_cast(typ_id, zero_id, typ_id)
	}
	// `nil` is a `voidptr` on its own, so a pointer zero is cast too: a generic call
	// then infers `&&int` from `$zero(E.pointee_type)`, not `voidptr`.
	if typ_id !in [u32(0), flat.source_type_void, flat.source_type_int, flat.source_type_f64,
		flat.source_type_string, flat.source_type_bool]
		&& t.a.node(zero_id).kind in [.int_literal, .float_literal, .string_literal, .bool_literal,
			.nil_literal] {
		return t.make_cast(typ_id, zero_id, typ_id)
	}
	return zero_id
}

fn (t &Transformer) qualified_alias_name(name_id u32) u32 {
	if flat.source_type_shape(name_id).has(.dotted) || t.cur_module.len == 0 || t.cur_module in [
		'main',
		'builtin',
	] {
		return name_id
	}
	return flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), name_id)
}

// clone_field_subst deep-clones a body node, substituting `<var>.member` references and folding
// `$if` conditions that reference the loop variable. Returns none when a folded branch is empty.
fn (mut t Transformer) clone_field_subst(id flat.NodeId, var_name string, fm FieldMeta) ?flat.NodeId {
	return t.clone_field_subst_scoped(id, var_name, fm, []string{})
}

@[direct_array_access]
fn (mut t Transformer) clone_field_subst_scoped(id flat.NodeId, var_name string, fm FieldMeta, inner_vars []string) ?flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	if node.kind == .string_literal && node.children_count > 0
		&& node.value in ['__v3_comptime_zero', '__v3_comptime_new'] {
		target := t.comptime_field_type_accessor(t.a.child(&node, 0), var_name, fm) or {
			return t.clone_field_subst_children(node, var_name, fm, inner_vars)
		}
		return if node.value == '__v3_comptime_new' {
			t.comptime_new_value(target)
		} else {
			t.comptime_zero_value(target)
		}
	}
	attr_var := comptime_reflected_attributes_loop_var(node, var_name)
	if attr_var.len > 0 {
		// In `$for field in field.attributes` the attribute shadows the field in the body.
		loop_id := if attr_var == var_name {
			id
		} else {
			t.clone_field_subst_children(node, var_name, fm, inner_vars) or { return none }
		}
		return t.unroll_reflected_attributes_loop(loop_id, attr_var, fm.raw_attribute_data(), flat.source_type_name(fm.attribute_module_id), inner_vars)
	}
	if comptime_for_declares_var(node, var_name) {
		return t.clone_node_preserving_children_with_type(node, t.clone_field_subst_type(node, var_name, fm))
	}
	if node.kind == .or_expr && node.value == '?' && node.children_count > 0 && !fm.is_option
		&& t.direct_reflected_field_selector(t.a.child(&node, 0), var_name) {
		source_id := t.a.child(&node, 0)
		t.tc.record_cgen_error_at('cannot use `?` on non-option expression', id, source_id, '?')
		return t.clone_field_subst_children(node, var_name, fm, inner_vars)
	}
	if node.kind == .sizeof_expr && t.sizeof_operand_name(node) == var_name {
		return t.make_sizeof_type(fm.comptime_typ)
	}
	// `isreftype(field)` does carry `field` as an expression. Preserve its reflected type
	// instead of materializing the FieldData value used by ordinary bare `field` access.
	if node.kind == .call && node.children_count == 2 {
		callee := t.a.child_node(&node, 0)
		arg := t.a.child_node(&node, 1)
		if callee.kind == .ident && callee.value == '__v3_isreftype' && arg.kind == .ident
			&& arg.value == var_name {
			return t.make_call('__v3_isreftype', [t.make_sizeof_type(fm.comptime_typ)])
		}
	}
	if node.kind == .ident && node.value == var_name {
		return t.make_field_data_literal(fm)
	}
	if node.children_count == 0 {
		return t.a.add_node(node)
	}
	// Field attributes are compile-time metadata. Avoid materializing and then
	// lowering runtime array work for operations whose result is already known
	// for this unrolled field.
	if node.kind == .call {
		if contains := t.comptime_attrs_condition(id, var_name, fm.attrs) {
			return t.make_bool_literal(contains)
		}
	}
	if node.kind == .for_in_stmt && node.children_count >= 3 && fm.attrs.len == 0
		&& t.direct_reflected_field_attrs_selector(t.a.child(&node, 2), var_name) {
		return none
	}
	if node.kind == .match_stmt && node.children_count > 1 {
		subject := t.a.child_node(&node, 0)
		if subject.kind == .selector && subject.value == 'typ' && subject.children_count > 0 {
			base := t.a.child_node(subject, 0)
			if base.kind == .ident && base.value == var_name {
				mut else_branch := flat.empty_node
				for i in 1 .. node.children_count {
					branch_id := t.a.child(&node, i)
					branch := t.a.nodes[int(branch_id)]
					if branch.kind != .match_branch {
						continue
					}
					if branch.value == 'else' {
						else_branch = branch_id
						continue
					}
					n_conds := t.count_conds(branch)
					mut selected := false
					for j in 0 .. n_conds {
						pattern_source_id := t.type_pattern_name(t.a.child(&branch, j))
						matches := t.comptime_type_matches_id(fm.comptime_typ, pattern_source_id) or { false }
						if pattern_source_id != 0 && matches {
							selected = true
							break
						}
					}
					if selected {
						return t.clone_field_match_branch_body(branch, n_conds, var_name, fm, inner_vars)
					}
				}
				if int(else_branch) >= 0 {
					branch := t.a.nodes[int(else_branch)]
					return t.clone_field_match_branch_body(branch, 0, var_name, fm, inner_vars)
				}
				return none
			}
		}
	}
	if node.kind == .prefix && node.op == .amp && node.children_count > 0
		&& t.subtree_has_reflected_typeof_idx(t.a.child(&node, 0), var_name) {
		mut pointee := t.trimmed_type_id(fm.comptime_typ)
		if flat.source_type_shape(pointee).has(.pointer) {
			pointee = t.trimmed_type_id(flat.source_type_syntax(pointee).raw_base())
		}
		zero := t.zero_value_for_type(pointee)
		start := t.a.children.len
		t.a.children << zero
		return t.a.add_node(flat.Node{
			kind:           .prefix
			op:             .amp
			children_start: start
			children_count: 1
		})
	}
	if node.kind == .for_in_stmt && node.children_count >= 3 && fm.is_option
		&& flat.source_type_shape(fm.comptime_typ).has(.option_type)
		&& t.direct_reflected_field_selector(t.a.child(&node, 2), var_name) {
		mut children := []flat.NodeId{cap: int(node.children_count)}
		scope := t.open_comptime_field_scope()
		for i in 0 .. node.children_count {
			child := t.clone_field_subst_scoped(t.a.child(&node, i), var_name, fm, inner_vars) or {
				flat.empty_node
			}
			if i == 2 {
				children << t.make_selector(child, 'value', flat.source_type_syntax(fm.comptime_typ).raw_base())
			} else {
				children << child
			}
		}
		t.close_comptime_field_scope(scope)
		start := t.a.children.len
		for child in children {
			t.a.children << child
		}
		return t.a.add_node(flat.Node{
			kind:           node.kind
			op:             node.op
			pos:            node.pos
			value:          node.value
			source_value:   node.source_value
			payload:        if node.payload != 0 { node.clone_owned().payload } else { u32(0) }
			typ:            node.typ
			is_mut:         node.is_mut
			flags:          node.flags & comptime_clone_syntax_flags
			children_start: start
			children_count: flat.child_count(children.len)
		})
	}
	if node.kind == .call && node.children_count > 0 {
		callee := t.a.child_node(&node, 0)
		if callee.kind == .selector && callee.value == 'str' && callee.children_count > 0 {
			base := t.a.child_node(callee, 0)
			if base.kind == .ident && base.value == var_name {
				return t.make_string_literal('FieldData{name: ${fm.name}, typ: ${fm.typ_text()}}')
			}
		}
	}
	// `typeof(<var>)` / `typeof(<var>).name` / `typeof(<var>).idx`: the field's own type,
	// not the FieldData metadata struct.
	if node.kind == .typeof_expr && t.typeof_arg_is_field_typ(id, var_name) {
		return t.make_string_literal(fm.typ_text())
	}
	if node.kind == .typeof_expr && node.children_count > 0
		&& t.reflected_field_value_selector(t.a.child(&node, 0), var_name) {
		return t.make_string_literal(fm.typ_text())
	}
	if node.kind == .selector && node.children_count > 0
		&& t.typeof_arg_is_field_typ(t.a.child(&node, 0), var_name) {
		match node.value {
			'name' {
				return t.make_string_literal(fm.typ_text())
			}
			'idx' {
				return t.make_int_literal(fm.typ_id)
			}
			else {}
		}
	}
	if node.kind == .selector && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind == .typeof_expr && base.children_count > 0
			&& t.reflected_field_value_selector(t.a.child(base, 0), var_name) {
			return match node.value {
				'name' { t.make_string_literal(fm.typ_text()) }
				'idx' { t.make_int_literal(fm.typ_id) }
				else { t.clone_field_subst_children(node, var_name, fm, inner_vars) }
			}
		}
	}
	if node.kind == .typeof_expr && t.typeof_arg_is_var(id, var_name) {
		return t.make_string_literal(fm.typ_text())
	}
	if node.kind == .selector && node.children_count > 0
		&& t.typeof_arg_is_var(t.a.child(&node, 0), var_name) {
		match node.value {
			'name' {
				return t.make_string_literal(fm.typ_text())
			}
			'unaliased_typ' {
				return t.make_string_literal(fm.unaliased_typ_text())
			}
			'idx' {
				return t.make_int_literal(fm.typ_id)
			}
			else {}
		}
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
			return t.clone_field_subst_children(node, var_name, fm, inner_vars)
		}
		// `receiver.$(<var>.name)` compile-time field selector - only fold when the name
		// expression is *this* loop variable's `.name`. A nested loop's `$(inner.name)` is left
		// untouched so its own unroll pass resolves it against the right field.
		if node.value == '\$' && node.children_count >= 2
			&& t.dollar_selector_names_var(t.a.child(&node, 1), var_name) {
			receiver := t.clone_field_subst_scoped(t.a.child(&node, 0), var_name, fm, inner_vars) or {
				return none
			}
								return t.make_selector(receiver, fm.name, fm.comptime_typ)
		}
	}
	// A normal `if !field.attrs.contains('skip')` inside a reflected field loop is
	// compile-time data even though V exposes `attrs` as an array. Fold it before
	// generic discovery so calls in a skipped field's branch do not instantiate
	// serializers for types that can never be visited at runtime.
	if node.kind == .if_expr && node.children_count >= 2 {
		cond_id := t.a.child(&node, 0)
		if taken := t.comptime_attrs_condition(cond_id, var_name, fm.attrs) {
			branch_idx := if taken { 1 } else { 2 }
			if branch_idx >= int(node.children_count) {
				return none
			}
			return t.clone_field_subst_scoped(t.a.child(&node, branch_idx), var_name, fm, inner_vars)
		}
	}
	// `$if`/`$else $if` referencing the loop variable: evaluate now, keep the taken branch.
	if node.kind == .comptime_if {
		substituted := t.subst_field_cond(node.value, var_name, fm)
		cond := t.subst_reflected_field_selector_cond(node.value, substituted, var_name, fm)
		references_var := comptime_cond_references_ident(node.value, var_name)
		if !comptime_cond_has_any_loop_member_ref(cond, inner_vars) {
			if (references_var || cond != node.value || comptime_cond_is_static_literal_expr(cond))
				&& !comptime_cond_has_loop_member_ref(cond, var_name) {
				if taken := t.eval_reflected_string_condition(cond, node) {
					branch_idx := if taken { 0 } else { 1 }
					if branch_idx >= int(node.children_count) {
						return none
					}
					return t.clone_field_subst_scoped(t.a.child(&node, branch_idx), var_name, fm, inner_vars)
				}
			}
			if references_var {
				if !t.comptime_condition_needs_local_value(cond, node) {
					t.reject_unevaluated_comptime_if(id, node, cond)
				}
			}
		}
		return t.clone_field_subst_children_with_value(node, var_name, fm, inner_vars, cond)
	}
	return t.clone_field_subst_children(node, var_name, fm, inner_vars)
}

fn (t &Transformer) reflected_field_value_selector(id flat.NodeId, var_name string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.node(id)
	if node.kind != .selector || node.value != '\$' || node.children_count < 2 {
		return false
	}
	receiver := t.a.child_node(node, 0)
	return receiver.kind == .ident && receiver.value == var_name
		&& t.dollar_selector_names_var(t.a.child(node, 1), var_name)
}

fn (t &Transformer) direct_reflected_field_attrs_selector(id flat.NodeId, var_name string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.node(id)
	if node.kind != .selector || node.value != 'attrs' || node.children_count == 0 {
		return false
	}
	base := t.a.child_node(node, 0)
	return base.kind == .ident && base.value == var_name
}

// unroll_method_attrs_loop expands `for [i,] attr in <var>.attrs { body }` into one block
// per attribute of the unrolled method, binding `attr` (and `i`) to that attribute's
// literal value. The attributes are compile-time data, so the runtime array the loop
// would iterate is pure overhead: one heap allocation on every pass of the enclosing
// `$for`, i.e. per call for code like a request router. A body that can `break` or
// `continue` keeps the array form.
fn (mut t Transformer) unroll_method_attrs_loop(node flat.Node, var_name string, method MethodMeta, inner_vars []string) ?flat.NodeId {
	if node.value != '3' {
		return none
	}
	key := t.a.child_node(&node, 0)
	val_id := t.a.child(&node, 1)
	idx_name := if int(val_id) < 0 { '' } else { key.value }
	attr_name := if int(val_id) < 0 { key.value } else { t.a.node(val_id).value }
	for i in 3 .. node.children_count {
		if t.node_has_loop_jump(t.a.child(&node, i)) {
			return none
		}
	}
	mut blocks := []flat.NodeId{cap: method.attrs.len}
	for i, raw in method.attrs {
		mut stmts := []flat.NodeId{}
		if attr_name.len > 0 && attr_name != '_' {
			stmts << t.make_decl_assign_typed(attr_name, t.make_string_literal(comptime_attr_display(raw)),
				flat.source_type_string)
		}
		if idx_name.len > 0 && idx_name != '_' {
			stmts << t.make_decl_assign_typed(idx_name, t.make_int_literal(i), flat.source_type_int)
		}
		for j in 3 .. node.children_count {
			if stmt := t.clone_method_subst_scoped(t.a.child(&node, j), var_name, method, inner_vars) {
				stmts << stmt
			}
		}
		blocks << t.make_block(stmts)
	}
	return t.make_block(blocks)
}

// node_has_loop_jump reports whether a `break` or `continue` occurs anywhere under `id`.
fn (t &Transformer) node_has_loop_jump(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.node(id)
	if node.kind in [.break_stmt, .continue_stmt] {
		return true
	}
	for i in 0 .. node.children_count {
		if t.node_has_loop_jump(t.a.child(node, i)) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) clone_field_match_branch_body(branch flat.Node, body_start int, var_name string, fm FieldMeta, inner_vars []string) flat.NodeId {
	mut body := []flat.NodeId{}
	scope := t.open_comptime_field_scope()
	for i in body_start .. branch.children_count {
		if child := t.clone_field_subst_scoped(t.a.child(&branch, i), var_name, fm, inner_vars) {
			body << child
		}
	}
	t.close_comptime_field_scope(scope)
	return t.make_block(body)
}

// comptime_attrs_condition folds `[!]<var>.attrs.contains('lit')` for a field or method
// whose attributes (`known`, in source spelling) are fixed at this unrolled iteration.
fn (t &Transformer) comptime_attrs_condition(id flat.NodeId, var_name string, known []string) ?bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.node(id)
	if node.kind == .prefix && node.op == .not && node.children_count == 1 {
		value := t.comptime_attrs_condition(t.a.child(node, 0), var_name, known) or {
			return none
		}
		return !value
	}
	if node.kind != .call || node.children_count != 2 {
		return none
	}
	callee := t.a.child_node(node, 0)
	if callee.kind != .selector || callee.value != 'contains' || callee.children_count == 0 {
		return none
	}
	attrs := t.a.child_node(callee, 0)
	if attrs.kind != .selector || attrs.value != 'attrs' || attrs.children_count == 0 {
		return none
	}
	base := t.a.child_node(attrs, 0)
	needle := t.a.child_node(node, 1)
	if base.kind != .ident || base.value != var_name || needle.kind != .string_literal {
		return none
	}
	// compare decoded values, as the runtime `attrs` array holds them (`it\'s` -> `it's`)
	return known.any(comptime_attr_display(it) == needle.value)
}

// direct_reflected_field_selector reports whether an iterable is exactly
// `receiver.$(<var>.name)` (with optional parentheses). Expressions that merely contain that
// selector, such as `receiver.$(<var>.name) or { fallback }`, already produce an unwrapped value.
fn (t &Transformer) direct_reflected_field_selector(id flat.NodeId, var_name string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.node(id)
	if node.kind == .paren && node.children_count == 1 {
		return t.direct_reflected_field_selector(t.a.child(node, 0), var_name)
	}
	return node.kind == .selector && node.value == '\$' && node.children_count >= 2
		&& t.dollar_selector_names_var(t.a.child(node, 1), var_name)
}

fn (t &Transformer) subtree_has_reflected_typeof_idx(id flat.NodeId, var_name string) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.node(id)
	if node.kind == .selector && node.value == 'idx' && node.children_count > 0 {
		base := t.a.child_node(node, 0)
		if base.kind == .typeof_expr && t.subtree_references_var(t.a.child(node, 0), var_name) {
			return true
		}
	}
	for i in 0 .. node.children_count {
		if t.subtree_has_reflected_typeof_idx(t.a.child(node, i), var_name) {
			return true
		}
	}
	return false
}

// subst_reflected_field_selector_cond resolves type membership for an actual dynamic reflected
// selector. For the legacy static form (`val.test in [...]` inside the `test` iteration), it
// resolves the receiver's field type instead of assuming that every `.test` belongs to `fm`.
fn (t &Transformer) subst_reflected_field_selector_cond(original string, substituted string, var_name string, fm FieldMeta) string {
	clean := trimmed_transform_text(original)
	for op in [' !in', ' in'] {
		if op_idx := comptime_top_index(clean, op) {
			left := trimmed_transform_text(clean[..op_idx])
			mut selector_type := ''
			if comptime_reflected_selector_left(left, var_name) {
				selector_type = fm.comptime_typ_text()
			} else {
				static_suffix := '.${fm.name}'
				if !left.ends_with(static_suffix) {
					continue
				}
				receiver := trimmed_transform_text(left[..left.len - static_suffix.len])
				if !comptime_plain_ident(receiver) {
					continue
				}
				receiver_type := t.trim_all_pointer_type(t.trimmed_type_id(t.var_type(receiver)))
				selector_type = flat.source_type_name(t.lookup_struct_field_type(receiver_type, fm.name) or { continue })
			}
			resolved := trimmed_transform_text(substituted)
			if resolved_op_idx := comptime_top_index(resolved, op) {
				return selector_type + resolved[resolved_op_idx..]
			}
		}
	}
	return substituted
}

fn comptime_reflected_selector_left(left string, var_name string) bool {
	if !left.ends_with(')') {
		return false
	}
	open_idx := left.last_index('.\$(') or { return false }
	return comptime_cond_references_ident(left, var_name)
		&& trimmed_transform_text(left[open_idx + 3..left.len - 1]) == '${var_name}.name'
}

fn comptime_plain_ident(value string) bool {
	if value == '' || !(value[0].is_letter() || value[0] == `_`) {
		return false
	}
	for c in value {
		if !(c.is_letter() || c.is_digit() || c == `_`) {
			return false
		}
	}
	return true
}

// typeof_arg_is_var reports whether `id` is a `typeof(<var_name>)` expression over the
// comptime loop variable.
fn (t &Transformer) typeof_arg_is_var(id flat.NodeId, var_name string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.node(id)
	if node.kind != .typeof_expr || node.children_count == 0 {
		return false
	}
	arg := t.a.child_node(node, 0)
	return arg.kind == .ident && arg.value == var_name
}

fn (t &Transformer) typeof_arg_is_field_typ(id flat.NodeId, var_name string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.node(id)
	if node.kind != .typeof_expr || node.children_count == 0 {
		return false
	}
	arg := t.a.child_node(node, 0)
	if arg.kind != .selector || arg.value != 'typ' || arg.children_count == 0 {
		return false
	}
	base := t.a.child_node(arg, 0)
	return base.kind == .ident && base.value == var_name
}

// dollar_selector_names_var reports whether a `$(...)` selector's name expression is the current
// field loop's `<var>.name`, so only the matching loop rewrites the selector and a nested loop's
// `$(inner.name)` is deferred to its own unroll.
fn (t &Transformer) dollar_selector_names_var(name_id flat.NodeId, var_name string) bool {
	if int(name_id) < 0 {
		return false
	}
	name_expr := t.a.node(name_id)
	if name_expr.kind != .selector || name_expr.value != 'name' || name_expr.children_count == 0 {
		return false
	}
	base := t.a.child_node(name_expr, 0)
	return base.kind == .ident && base.value == var_name
}

fn (mut t Transformer) clone_field_subst_children(node flat.Node, var_name string, fm FieldMeta, inner_vars []string) ?flat.NodeId {
	return t.clone_field_subst_children_with_value(node, var_name, fm, inner_vars, node.value)
}

// open_comptime_field_scope opens the scope of the children of a node that the
// clone of a `$for` body copies: a local that one child declares is seen by the
// later children only.
@[inline]
fn (t &Transformer) open_comptime_field_scope() VarTypeScopeMark {
	return t.mark_var_type_scope()
}

// close_comptime_field_scope drops the locals declared since `mark`.
fn (mut t Transformer) close_comptime_field_scope(mark VarTypeScopeMark) {
	if t.declaring_comptime_field_locals
		&& (t.var_types.len != mark.bindings || t.var_type_undo_log.len != mark.undo) {
		t.restore_var_type_scope(mark)
	}
}

// declare_comptime_field_local declares the local of a `name := value` that the
// clone of a `$for` body copied, with the type that the clone gave it.
fn (mut t Transformer) declare_comptime_field_local(lhs_id flat.NodeId, typ u32) {
	if !t.declaring_comptime_field_locals || typ == 0
		|| typ in [flat.source_type_unknown, flat.source_type_generic] {
		return
	}
	if t.generic_arg_is_unresolved(typ) {
		return
	}
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind == .ident && lhs.value.len > 0 && lhs.value != '_' {
		t.set_var_type(lhs.value, typ)
	}
}

@[direct_array_access]
fn (mut t Transformer) clone_field_subst_children_with_value(node flat.Node, var_name string, fm FieldMeta, inner_vars []string, value string) ?flat.NodeId {
	child_inner_vars := comptime_nested_loop_vars(node, var_name, inner_vars)
	mut children := []flat.NodeId{cap: int(node.children_count)}
	scope := t.open_comptime_field_scope()
	for i in 0 .. node.children_count {
		if c := t.clone_field_subst_scoped(t.a.child(&node, i), var_name, fm, child_inner_vars) {
			children << c
		}
	}
	t.close_comptime_field_scope(scope)
	mut cloned_value := value
	mut cloned_call_type_args := node.call_type_args()
	if node.kind != .comptime_if {
		cloned_value, cloned_call_type_args = t.comptime_field_call_generic_args(node, mut children, fm)
	}
	mut cloned_metadata := flat.Node{
		payload: if node.payload != 0 { node.clone_owned().payload } else { u32(0) }
	}
	if node.kind == .call && cloned_call_type_args != node.call_type_args() {
		cloned_metadata.set_call_type_args(cloned_call_type_args)
	}
	mut typ := t.clone_field_subst_type(node, var_name, fm)
	if node.kind == .prefix && children.len == 1 {
		child_type := t.node_type(children[0])
		if child_type != 0 && child_type !in [flat.source_type_unknown, flat.source_type_generic] {
			if node.op == .amp {
				typ = flat.source_type_composition_one(.pointer, child_type)
			} else if node.op == .mul && flat.source_type_shape(child_type).has(.pointer) {
				typ = t.trim_pointer_type(child_type)
			}
		}
	}
	if node.kind == .call && children.len > 0 {
		callee := t.a.nodes[int(children[0])]
		if callee.kind == .ident {
			if ret_source_id := t.fn_ret_types.type_id(callee.value) {
				typ = ret_source_id
			} else if !isnil(t.tc) {
				if ret := t.tc.fn_ret_types[callee.value] {
					typ = ret.source_type_id()
				}
			}
		}
	}
	if node.kind == .decl_assign && children.len >= 2 {
		rhs := t.a.nodes[int(children[1])]
		unwrapped_rhs_typ := if rhs.kind == .ident {
			t.comptime_option_unwrapped_local_type(rhs.value, node, fm) or { u32(0) }
		} else {
			u32(0)
		}
		reflected_rhs_typ := (if unwrapped_rhs_typ != 0 {
			unwrapped_rhs_typ
		} else if rhs.kind == .ident {
			(t.local_decl_type_before(rhs.value, children[1]) or { u32(0) })
		} else if rhs.kind == .or_expr && rhs.value == '?' && fm.is_option
			&& flat.source_type_shape(fm.comptime_typ).has(.option_type) {
			t.trimmed_type_id(flat.source_type_syntax(fm.comptime_typ).raw_base())
		} else {
			u32(0)
		})
		if reflected_rhs_typ != 0 {
			t.set_node_typ(int(children[1]), reflected_rhs_typ)
			t.record_refined_node_type(int(children[1]), reflected_rhs_typ)
		}
		// A reflected selector carries the field's qualified type on the cloned
		// node. Keep that spelling so a bare user type is not mistaken for an
		// unresolved generic placeholder during a later call in this branch.
		rhs_typ := if reflected_rhs_typ != 0 {
			reflected_rhs_typ
		} else if rhs.typ != 0 && (rhs.typ) !in [flat.source_type_unknown, flat.source_type_generic]
			&& !t.generic_arg_is_unresolved(rhs.typ) {
			rhs.typ
		} else {
			t.node_type(children[1])
		}
		if rhs_typ != 0 && rhs_typ !in [flat.source_type_unknown, flat.source_type_generic] {
			typ = rhs_typ
			t.set_node_typ(int(children[0]), typ)
			if children.len == 2 {
				t.declare_comptime_field_local(children[0], typ)
			}
		}
	}
	start := t.a.children.len
	for c in children {
		t.a.children << c
	}
	result := t.a.add_node(flat.Node{
		kind:           node.kind
		op:             node.op
		pos:            node.pos
		value:          cloned_value
		source_value:   node.source_value
		payload:        cloned_metadata.payload
		typ:            typ
		is_mut:         node.is_mut
		flags:          node.flags & comptime_clone_syntax_flags
		children_start: start
		children_count: flat.child_count(children.len)
	})
	if t.specialization_node_start >= 0 && node.kind == .decl_assign && children.len >= 2 {
		lhs := t.a.nodes[int(children[0])]
		if lhs.kind == .ident && lhs.value.len > 0 {
			t.specialization_decl_nodes_by_name[lhs.value] << int(result)
		}
	}
	return result
}

fn (mut t Transformer) comptime_field_type_accessor(id flat.NodeId, var_name string, fm FieldMeta) ?u32 {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident && node.value == var_name {
		return fm.comptime_typ
	}
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	base := t.a.child_node(&node, 0)
	if base.kind == .ident && base.value == var_name {
		return match node.value {
			'typ' { fm.comptime_typ }
			'unaliased_typ' { fm.comptime_unaliased }
			else { none }
		}
	}
	inner := t.comptime_field_type_accessor(t.a.child(&node, 0), var_name, fm) or { return none }
	return match node.value {
		'payload_type' {
			if flat.source_type_shape(inner).has(.optional) {
				flat.source_type_syntax(inner).raw_base()
			} else {
				inner
			}
		}
		'pointee_type' {
			t.trim_all_pointer_type(inner)
		}
		else {
			none
		}
	}
}

fn (mut t Transformer) comptime_new_value(typ_id u32) flat.NodeId {
	value := t.transform_expr(t.zero_value_for_type(typ_id))
	tmp_name := t.new_temp('new')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, value, typ_id)
	addr := t.make_prefix(.amp, t.make_ident(tmp_name))
	pointer := flat.source_type_composition_one(.pointer, typ_id)
	t.set_node_typ(int(addr), pointer)
	allocation := t.make_memdup_call_for_type(addr, typ_id)
	return t.make_cast(pointer, allocation, pointer)
}

fn (t &Transformer) clone_field_subst_type(node flat.Node, var_name string, fm FieldMeta) u32 {
	if node.kind != .comptime_for {
		return node.typ
	}
	mut typ := node.typ_text()
	typ = typ.replace('${var_name}.unaliased_typ', fm.comptime_unaliased_text())
	typ = typ.replace('${var_name}.typ', fm.comptime_typ_text())
	return flat.source_type_id(comptime_cond_replace_bare_ident(typ, var_name, fm.comptime_typ_text()))
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
		'attributes' { t.make_attribute_array_literal(fm.attribute_metas()) }
		'typ' { t.make_int_literal(fm.typ_id) }
		'unaliased_typ' { t.make_int_literal(fm.unaliased_id) }
		else {
			return none
		}
	}
}

// make_string_array_literal builds a `[]string` literal (empty when `values` is empty) for
// materializing `field.attrs`.
fn (mut t Transformer) make_string_array_literal(values []string) flat.NodeId {
	if values.len == 0 {
		return t.zero_value_for_type(flat.source_type_id('[]string'))
	}
	mut ids := []flat.NodeId{cap: values.len}
	for v in values {
		ids << t.make_string_literal(comptime_attr_display(v))
	}
	return t.make_array_literal_typed(ids, flat.source_type_id('[]string'))
}

// subst_value_cond textually substitutes `<var>.name`/`<var>.value` inside a comptime condition
// string for a `$for value in Enum.values` iteration, so `eval_field_cond` can fold it.
fn (t &Transformer) subst_value_cond(cond string, var_name string, item EnumValueMeta) string {
	mut c := cond
	if !item.value_from_c {
		c = comptime_cond_replace_unquoted(c, '${var_name}.value', item.value.str())
	}
	c = comptime_cond_replace_unquoted(c, '${var_name}.name', "'${item.name}'")
	return c
}

// subst_field_cond textually substitutes `<var>.member` and bare `<var>` type-guard shorthand
// inside a comptime condition string. Longer members are replaced first so `.typ_text()` does not
// clobber `.unaliased_typ_text()`.
fn (t &Transformer) subst_field_cond(cond string, var_name string, fm FieldMeta) string {
	if !cond.contains("'") && !cond.contains('"') {
		return t.subst_unquoted_field_cond(cond, var_name, fm)
	}
	mut result := ''
	mut offset := 0
	for offset < cond.len {
		if cond[offset] == `'` || cond[offset] == `"` {
			end := comptime_cond_skip_string(cond, offset)
			result += cond[offset..end]
			offset = end
			continue
		}
		start := offset
		for offset < cond.len && cond[offset] !in [`'`, `"`] {
			offset++
		}
		result += t.subst_unquoted_field_cond(cond[start..offset], var_name, fm)
	}
	return result
}

fn (t &Transformer) subst_unquoted_field_cond(cond string, var_name string, fm FieldMeta) string {
	mut c := cond
	if t.cur_module.len > 0 {
		c = comptime_cond_replace_unquoted(c, '${t.cur_module}.${var_name}', var_name)
	}
	c = comptime_cond_replace_int_compared(c, '${var_name}.unaliased_typ', fm.unaliased_id.str())
	c = comptime_cond_replace_unquoted(c, '${var_name}.unaliased_typ', fm.comptime_unaliased_text())
	c = comptime_cond_replace_unquoted(c, '${var_name}.indirections', fm.indirections.str())
	c = comptime_cond_replace_unquoted(c, '${var_name}.is_option', fm.is_option.str())
	c = comptime_cond_replace_unquoted(c, '${var_name}.is_opt', fm.is_option.str())
	c = comptime_cond_replace_unquoted(c, '${var_name}.is_embed', fm.is_embed.str())
	c = comptime_cond_replace_unquoted(c, '${var_name}.is_array', fm.is_array.str())
	c = comptime_cond_replace_unquoted(c, '${var_name}.is_map', fm.is_map.str())
	c = comptime_cond_replace_unquoted(c, '${var_name}.is_chan', fm.is_chan.str())
	c = comptime_cond_replace_unquoted(c, '${var_name}.is_struct', fm.is_struct.str())
	c = comptime_cond_replace_unquoted(c, '${var_name}.is_enum', fm.is_enum.str())
	c = comptime_cond_replace_unquoted(c, '${var_name}.is_alias', fm.is_alias.str())
	c = comptime_cond_replace_unquoted(c, '${var_name}.is_shared', fm.is_shared.str())
	c = comptime_cond_replace_unquoted(c, '${var_name}.is_atomic', fm.is_atomic.str())
	c = comptime_cond_replace_unquoted(c, '${var_name}.is_mut', fm.is_mut.str())
	c = comptime_cond_replace_unquoted(c, '${var_name}.is_pub', fm.is_pub.str())
	// Preserve the metadata selector marker until type matching. It distinguishes
	// an alias-valued `field.typ_text()` from `field.unaliased_typ_text()` after substitution.
	c = comptime_cond_replace_int_compared(c, '${var_name}.typ', fm.typ_id.str())
	c = comptime_cond_replace_unquoted(c, '${var_name}.typ', '${fm.comptime_typ_text()}.typ')
	c = comptime_cond_replace_unquoted(c, '${var_name}.name', "'${fm.name}'")
	c = comptime_cond_replace_bare_ident(c, var_name, fm.comptime_typ_text())
	return c
}

fn comptime_cond_replace_unquoted(cond string, needle string, replacement string) string {
	if needle == '' || !cond.contains(needle) {
		return cond
	}
	mut out := ''
	mut offset := 0
	for offset < cond.len {
		if cond[offset] == `'` || cond[offset] == `"` {
			end := comptime_cond_skip_string(cond, offset)
			out += cond[offset..end]
			offset = end
			continue
		}
		if offset + needle.len <= cond.len && cond[offset..offset + needle.len] == needle {
			before_ok := !comptime_cond_name_char(needle[0]) || offset == 0
				|| !comptime_cond_name_char(cond[offset - 1])
			after := offset + needle.len
			after_ok := !comptime_cond_name_char(needle[needle.len - 1]) || after >= cond.len
				|| !comptime_cond_name_char(cond[after])
			if before_ok && after_ok {
				out += replacement
				offset = after
				continue
			}
		}
		out += cond[offset].ascii_str()
		offset++
	}
	return out
}

// comptime_cond_replace_int_compared replaces the unquoted `needle` with `replacement` where
// it is compared with an integer literal (see comptime_cond_int_compared). Type members such
// as `method.return_type_text()` hold type ids at runtime, so `$if method.return_type_text() == 1` needs the
// id, while `is`, `in` and a comparison with a type (`== void`) need the type text.
fn comptime_cond_replace_int_compared(cond string, needle string, replacement string) string {
	if needle == '' || !cond.contains(needle) {
		return cond
	}
	mut out := ''
	mut offset := 0
	for offset < cond.len {
		if cond[offset] == `'` || cond[offset] == `"` {
			end := comptime_cond_skip_string(cond, offset)
			out += cond[offset..end]
			offset = end
			continue
		}
		after := offset + needle.len
		if after <= cond.len && cond[offset..after] == needle
			&& !comptime_cond_operand_char(cond, offset - 1)
			&& !comptime_cond_operand_char(cond, after)
			&& comptime_cond_int_compared(cond, offset, after) {
			out += replacement
			offset = after
			continue
		}
		out += cond[offset].ascii_str()
		offset++
	}
	return out
}

// comptime_cond_int_compared reports whether the operand `cond[start..end]` is compared with
// an integer literal by `==`, `!=`, `<`, `>`, `<=` or `>=`, on either side.
fn comptime_cond_int_compared(cond string, start int, end int) bool {
	// `operand == 1`
	mut i := end
	for i < cond.len && cond[i] == ` ` {
		i++
	}
	op_len := if cond[i..].starts_with('==') || cond[i..].starts_with('!=')
		|| cond[i..].starts_with('<=') || cond[i..].starts_with('>=') {
		2
	} else if i < cond.len && cond[i] in [`<`, `>`] {
		1
	} else {
		0
	}
	if op_len > 0 {
		i += op_len
		for i < cond.len && cond[i] == ` ` {
			i++
		}
		mut j := i
		for j < cond.len && cond[j].is_digit() {
			j++
		}
		if j > i && !comptime_cond_operand_char(cond, j) {
			return true
		}
	}
	// `1 == operand`
	mut k := start
	for k > 0 && cond[k - 1] == ` ` {
		k--
	}
	if k >= 2 && cond[k - 2..k] in ['==', '!=', '<=', '>='] {
		k -= 2
	} else if k >= 1 && cond[k - 1] in [`<`, `>`] && (k < 2 || cond[k - 2] !in [`<`, `>`]) {
		k--
	} else {
		return false
	}
	for k > 0 && cond[k - 1] == ` ` {
		k--
	}
	mut j := k
	for j > 0 && cond[j - 1].is_digit() {
		j--
	}
	return j < k && !comptime_cond_operand_char(cond, j - 1)
}

// comptime_cond_operand_char reports whether `cond[i]` exists and continues an operand such
// as `field.typ_text()` or `12`: a name character or a `.`.
fn comptime_cond_operand_char(cond string, i int) bool {
	return i >= 0 && i < cond.len && (comptime_cond_name_char(cond[i]) || cond[i] == `.`)
}

fn comptime_cond_replace_bare_ident(cond string, ident string, replacement string) string {
	if ident == '' {
		return cond
	}
	mut out := ''
	mut offset := 0
	for offset < cond.len {
		if cond[offset] == `'` || cond[offset] == `"` {
			end := comptime_cond_skip_string(cond, offset)
			out += cond[offset..end]
			offset = end
			continue
		}
		if offset + ident.len <= cond.len && cond[offset..offset + ident.len] == ident {
			before_ok := offset == 0 || !comptime_cond_name_char(cond[offset - 1])
			after := offset + ident.len
			after_ok := after >= cond.len
				|| (!comptime_cond_name_char(cond[after]) && cond[after] != `.`)
			if before_ok && after_ok {
				out += replacement
				offset = after
				continue
			}
		}
		out += cond[offset..offset + 1]
		offset++
	}
	return out
}

fn comptime_cond_references_ident(cond string, ident string) bool {
	if ident == '' {
		return false
	}
	mut offset := 0
	for offset < cond.len {
		if cond[offset] == `'` || cond[offset] == `"` {
			offset = comptime_cond_skip_string(cond, offset)
			continue
		}
		if offset + ident.len <= cond.len && cond[offset..offset + ident.len] == ident {
			before_ok := offset == 0 || !comptime_cond_name_char(cond[offset - 1])
			after := offset + ident.len
			after_ok := after >= cond.len || !comptime_cond_name_char(cond[after])
			if before_ok && after_ok {
				return true
			}
		}
		offset++
	}
	return false
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

fn comptime_cond_has_any_loop_member_ref(cond string, var_names []string) bool {
	for var_name in var_names {
		if comptime_cond_has_loop_member_ref(cond, var_name) {
			return true
		}
	}
	return false
}

fn comptime_nested_loop_vars(node flat.Node, var_name string, inner_vars []string) []string {
	if node.kind != .comptime_for {
		return inner_vars
	}
	loop_var, _ := comptime_for_parts(node.value)
	if loop_var == var_name || loop_var in inner_vars {
		return inner_vars
	}
	mut scoped_vars := inner_vars.clone()
	scoped_vars << loop_var
	return scoped_vars
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

fn comptime_cond_is_static_literal_expr(cond string) bool {
	clean := comptime_condition_strip_outer_parens(trimmed_transform_text(cond))
	for op in ['||', '&&'] {
		if op_idx := comptime_top_index(clean, op) {
			return comptime_cond_is_static_literal_expr(clean[..op_idx])
				&& comptime_cond_is_static_literal_expr(clean[op_idx + op.len..])
		}
	}
	if clean.starts_with('!') {
		return comptime_cond_is_static_literal_expr(clean[1..])
	}
	if clean in ['true', 'false'] {
		return true
	}
	for op in ['!=', '=='] {
		if op_idx := comptime_top_index(clean, op) {
			return comptime_cond_is_quoted_literal(clean[..op_idx])
				&& comptime_cond_is_quoted_literal(clean[op_idx + op.len..])
		}
	}
	return false
}

fn comptime_cond_is_quoted_literal(value string) bool {
	clean := trimmed_transform_text(value)
	return clean.len >= 2 && clean[0] in [`'`, `"`, `\``] && clean[clean.len - 1] == clean[0]
}

// UnevaluatedComptimeIf is a `$if` in a `$for` body that names the loop variable, but
// whose condition is still undecided once the loop variable is substituted.
struct UnevaluatedComptimeIf {
	node flat.NodeId
	pos  token.Pos
}

// reject_unevaluated_comptime_if records the `$if` `node` (`id`), which names a `$for`
// loop variable, when its substituted condition `cond` cannot be decided. Code generation
// would otherwise drop it as if it were false. Workers keep their own records; the
// transform reports them once they are merged, see report_unevaluated_comptime_ifs.
fn (mut t Transformer) reject_unevaluated_comptime_if(id flat.NodeId, node flat.Node, cond string) {
	// A generic template is expanded again for each specialization, once its type
	// parameters are known. Portable output (`-os cross`) keeps target conditions for
	// the C preprocessor.
	if t.cur_fn_is_generic || comptime_cond_has_target_flag(cond) {
		return
	}
	// The forms that the statement transform still folds after the substitution.
	if _ := t.comptime_type_condition_value(cond) {
		return
	}
	t.unevaluated_comptime_ifs << UnevaluatedComptimeIf{
		node: id
		pos:  node.pos
	}
}

// report_unevaluated_comptime_ifs passes what reject_unevaluated_comptime_if recorded to
// the checker as errors, once per `$if`. Called on the main thread.
fn (mut t Transformer) report_unevaluated_comptime_ifs() {
	// Workers and loop iterations meet them in any order; report in source order.
	t.unevaluated_comptime_ifs.sort_with_compare(compare_unevaluated_comptime_ifs)
	mut reported := map[string]bool{}
	for item in t.unevaluated_comptime_ifs {
		key := '${item.pos.id}:${item.pos.offset}'
		if reported[key] {
			continue
		}
		reported[key] = true
		mut cond := if int(item.node) >= 0 && int(item.node) < t.a.nodes.len {
			t.a.nodes[int(item.node)].value
		} else {
			''
		}
		mut pos := item.pos
		if source_cond, source_pos := t.comptime_if_source_cond(item.pos) {
			cond = source_cond
			pos = source_pos
		}
		t.tc.record_transform_error(item.node, pos, 'cannot evaluate `\$if` condition `${cond}` at compile time; use a runtime `if` instead')
	}
	t.unevaluated_comptime_ifs.clear()
}

fn compare_unevaluated_comptime_ifs(a &UnevaluatedComptimeIf, b &UnevaluatedComptimeIf) int {
	if a.pos.id != b.pos.id {
		return if a.pos.id < b.pos.id { -1 } else { 1 }
	}
	if a.pos.offset != b.pos.offset {
		return if a.pos.offset < b.pos.offset { -1 } else { 1 }
	}
	return 0
}

// comptime_if_source_cond returns the condition of the `$if` at `pos` as it is written in
// the source (on one line), and the span that it covers there. The stored condition is
// normalized and, inside a `$for` body, already substituted.
fn (t &Transformer) comptime_if_source_cond(pos token.Pos) ?(string, token.Pos) {
	file := t.a.source_files[pos.id] or { return none }
	source := os.read_file(file.name) or { return none }
	mut i := int(pos.offset)
	if i < 0 || i >= source.len || source[i] != `$` {
		return none
	}
	i++
	for i < source.len && source[i].is_space() {
		i++
	}
	if !source[i..].starts_with('if') {
		return none
	}
	i += 2
	for i < source.len && source[i].is_space() {
		i++
	}
	start := i
	mut depth := 0
	for i < source.len {
		ch := source[i]
		if ch in [`'`, `"`, `\``] {
			i = comptime_cond_skip_string(source, i)
			continue
		}
		if ch in [`(`, `[`] {
			depth++
		} else if ch in [`)`, `]`] {
			depth--
		} else if ch == `{` && depth <= 0 {
			break
		}
		i++
	}
	end := start + source[start..int_min(i, source.len)].trim_right(' \t\r\n').len
	if end <= start || i >= source.len {
		return none
	}
	mut text := source[start..end]
	if text.contains('\n') {
		text = text.fields().join(' ')
	}
	return text, token.new_span(int(pos.id), start, end)
}

// eval_field_cond evaluates a fully-substituted comptime condition (`is`/`!is`, `in`/`!in`,
// `==`/`!=`, integer ordering, `&&`/`||`/`!`, bare bool, and the string literal members of
// comptime_cond_string_member). Returns none when it cannot be decided statically.
fn (mut t Transformer) eval_field_cond(cond string) ?bool {
	clean := comptime_condition_strip_outer_parens(trimmed_transform_text(cond))
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
			left := trimmed_transform_text(clean[..op_idx])
			right := trimmed_transform_text(clean[op_idx + op.len..])
			matches := if left.starts_with('\$') && !right.starts_with('\$') {
				t.comptime_type_matches(right, left) or { return none }
			} else {
				t.comptime_type_matches(left, right) or { return none }
			}
			return if op == ' is ' { matches } else { !matches }
		}
	}
	for op in [' != ', ' == '] {
		if op_idx := comptime_top_index(clean, op) {
			// String operands may be quoted on one side (`'txt'` from a substituted `field.name`)
			// and bare on the other (`txt` as captured in the condition); compare unquoted.
			left := comptime_cond_operand(clean[..op_idx]) or { return none }
			right := comptime_cond_operand(clean[op_idx + op.len..]) or { return none }
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
			needle_text := trimmed_transform_text(clean[..op_idx])
			list := trimmed_transform_text(clean[after..])
			mut found := false
			if needle_text.ends_with('.typ') || needle_text.ends_with('.unaliased_typ') {
				if !list.starts_with('[') || !list.ends_with(']') {
					return none
				}
				for expected in split_generic_args(list[1..list.len - 1]) {
					if t.comptime_type_matches(needle_text, expected) or { false } {
						found = true
						break
					}
				}
			} else {
				needle := comptime_cond_operand(needle_text) or { return none }
				if list.starts_with('[') && list.ends_with(']') {
					mut rest := trimmed_transform_text(list[1..list.len - 1])
					for rest.len > 0 {
						comma := comptime_condition_top_level_index(rest, ',')
						item := if comma >= 0 { rest[..comma] } else { rest }
						value := comptime_cond_operand(item) or { return none }
						found = found || value == needle
						if comma < 0 { break }
						rest = trimmed_transform_text(rest[comma + 1..])
					}
				} else {
					value := comptime_cond_string_member(list) or { list }
					if !comptime_cond_is_quoted_literal(value) { return none }
					found = comptime_unquote(value).contains(needle)
				}
			}
			return if op == ' in' { found } else { !found }
		}
	}
	// Integer ordering (e.g. `field.indirections < 2`); longer operators first.
	for op in [' <= ', ' >= ', ' < ', ' > '] {
		if op_idx := comptime_top_index(clean, op) {
			left := comptime_cond_operand(clean[..op_idx]) or { return none }
			right := comptime_cond_operand(clean[op_idx + op.len..]) or { return none }
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
	if value := comptime_cond_string_member(clean) {
		return match value {
			'true' { true }
			'false' { false }
			else { none }
		}
	}
	if clean.starts_with('!') {
		inner := t.eval_field_cond(clean[1..]) or { return none }
		return !inner
	}
	return none
}

// comptime_cond_operand returns a comparison operand as the text to compare: a string
// literal without its quotes, the value of a string literal member (see
// comptime_cond_string_member), or a plain operand (a number, name or enum value) as it
// is. Any unsupported expression, such as `'name'.repeat(2)` or `'name'[0]`, is not its own
// value, so it cannot be compared as text: none.
fn comptime_cond_operand(operand string) ?string {
	clean := comptime_condition_strip_outer_parens(trimmed_transform_text(operand))
	if value := comptime_cond_string_member(clean) {
		return comptime_unquote(value)
	}
	if clean.len > 0 && clean[0] in [`'`, `"`, `\``] {
		if comptime_cond_skip_string(clean, 0) != clean.len {
			return none
		}
		return comptime_unquote(clean)
	}
	if clean.contains_any('\'"`([') {
		return none
	}
	return clean
}

// ComptimeStringScalar is the literal result of a pure string operation.
struct ComptimeStringScalar {
	typ   u32
	value string
}

fn comptime_string_scalar(receiver string, method string, args []string) ?ComptimeStringScalar {
	value := util.comptime_string_scalar(receiver, method, args)?
	return ComptimeStringScalar{value.typ, value.value}
}

fn comptime_string_repeat(receiver string, count_text string) ?ComptimeStringScalar {
	value := util.comptime_string_repeat(receiver, count_text)?
	return ComptimeStringScalar{value.typ, value.value}
}

fn comptime_string_index(receiver string, needle string) (ComptimeStringScalar, bool) {
	value, found := util.comptime_string_index(receiver, needle)
	return ComptimeStringScalar{value.typ, value.value}, found
}

fn comptime_string_split(receiver string, delim string) []string {
	return util.comptime_string_split(receiver, delim)
}

// comptime_cond_string_member folds literal string method chains to literal condition text.
fn comptime_cond_string_member(expr string) ?string {
	clean := comptime_condition_strip_outer_parens(trimmed_transform_text(expr))
	if clean.len < 2 || clean[0] !in [`'`, `"`] {
		return none
	}
	receiver_end := comptime_cond_skip_string(clean, 0)
	if receiver_end >= clean.len || clean[receiver_end] !in [`.`, `[`] {
		return none
	}
	mut receiver := comptime_unquote(clean[..receiver_end])
	mut rest := trimmed_transform_text(clean[receiver_end..])
	for rest.len > 0 {
		if rest.starts_with('[') {
			end := rest.index_u8(`]`)
			if end < 0 {
				return none
			}
			bounds := rest[1..end].split('..')
			if bounds.len != 2 {
				return none
			}
			low_text := trimmed_transform_text(bounds[0])
			high_text := trimmed_transform_text(bounds[1])
			low := if low_text == '' {
				0
			} else {
				util.comptime_string_bound(low_text) or { return none }
			}
			high := if high_text == '' {
				receiver.len
			} else {
				util.comptime_string_bound(high_text) or { return none }
			}
			if low < 0 || high < low || high > receiver.len {
				return none
			}
			receiver = receiver[low..high]
			rest = trimmed_transform_text(rest[end + 1..])
			if rest.len == 0 {
				return comptime_cond_string_literal(receiver)
			}
			continue
		}
		if !rest.starts_with('.') {
			return none
		}
		member := trimmed_transform_text(rest[1..])
		if member == 'len' {
			return receiver.len.str()
		}
		paren := member.index_u8(`(`)
		if paren < 0 {
			return none
		}
		end := comptime_condition_matching_paren(member, paren)
		if end < 0 {
			return none
		}
		mut args := []string{}
		mut arg_text := trimmed_transform_text(member[paren + 1..end])
		for arg_text.len > 0 {
			comma := comptime_condition_top_level_index(arg_text, ',')
			raw_arg := trimmed_transform_text((if comma < 0 { arg_text } else { arg_text[..comma] }))
			arg := comptime_cond_string_member(raw_arg) or { raw_arg }
			if arg.len < 2 || arg[0] !in [`'`, `"`] || comptime_cond_skip_string(arg, 0) != arg.len {
				return none
			}
			args << comptime_unquote(arg)
			if comma < 0 {
				break
			}
			arg_text = trimmed_transform_text(arg_text[comma + 1..])
			if arg_text.len == 0 {
				return none
			}
		}
		value := comptime_string_scalar(receiver, trimmed_transform_text(member[..paren]), args) or { return none }
		rest = trimmed_transform_text(member[end + 1..])
		if rest.len == 0 {
			return if value.typ == flat.source_type_string {
				comptime_cond_string_literal(value.value)
			} else {
				value.value
			}
		}
		if value.typ != flat.source_type_string {
			return none
		}
		receiver = value.value
	}
	return none
}

fn comptime_is_int(s string) bool {
	if s == '' {
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
		return comptime_cond_unescape(s[1..s.len - 1])
	}
	return s
}

fn comptime_cond_unescape(value string) string {
	if !value.contains('\\') {
		return value
	}
	mut out := strings.new_builder(value.len)
	mut i := 0
	for i < value.len {
		if value[i] != `\\` || i + 1 >= value.len {
			out.write_u8(value[i])
			i++
			continue
		}
		next := value[i + 1]
		if next == `\n` {
			i += 2
			for i < value.len && value[i] in [` `, `\t`, `\r`] {
				i++
			}
			continue
		}
		if next == `\r` && i + 2 < value.len && value[i + 2] == `\n` {
			i += 3
			for i < value.len && value[i] in [` `, `\t`] {
				i++
			}
			continue
		}
		if next == `x` && i + 3 < value.len {
			if code := comptime_cond_fixed_hex(value, i + 2, 2) {
				out.write_u8(u8(code))
				i += 4
				continue
			}
		}
		if next == `u` && i + 5 < value.len {
			if code := comptime_cond_fixed_hex(value, i + 2, 4) {
				out.write_rune(rune(code))
				i += 6
				continue
			}
		}
		if next == `U` && i + 9 < value.len {
			if code := comptime_cond_fixed_hex(value, i + 2, 8) {
				out.write_rune(rune(code))
				i += 10
				continue
			}
		}
		decoded := match next {
			`n` { int(`\n`) }
			`t` { int(`\t`) }
			`r` { int(`\r`) }
			`\\` { int(`\\`) }
			`'` { int(`'`) }
			`"` { int(`"`) }
			`$` { int(`$`) }
			`0` { 0 }
			`a` { 7 }
			`b` { 8 }
			`f` { 12 }
			`v` { 11 }
			else { -1 }
		}

		if decoded >= 0 {
			out.write_u8(u8(decoded))
		} else {
			out.write_u8(`\\`)
			out.write_u8(next)
		}
		i += 2
	}
	return out.str()
}

fn comptime_cond_fixed_hex(value string, start int, count int) ?u32 {
	mut code := u32(0)
	for i in 0 .. count {
		if start + i >= value.len {
			return none
		}
		digit := comptime_cond_hex_digit(value[start + i]) or { return none }
		code = (code << 4) | digit
	}
	return code
}

fn comptime_cond_hex_digit(c u8) ?u32 {
	if c >= `0` && c <= `9` {
		return u32(c - `0`)
	}
	if c >= `a` && c <= `f` {
		return u32(c - `a` + 10)
	}
	if c >= `A` && c <= `F` {
		return u32(c - `A` + 10)
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

// typ_text resolves the indexed source type spelling.
@[inline]
fn (value &ComptimeStringScalar) typ_text() string {
	return flat.source_type_name(value.typ)
}

// typ_text resolves the indexed source type spelling.
@[inline]
fn (value &FieldMeta) typ_text() string {
	return flat.source_type_name(value.typ)
}

// typ_text resolves the indexed source type spelling.
@[inline]
fn (value &VariantMeta) typ_text() string {
	return flat.source_type_name(value.typ)
}

// typ_text resolves the indexed source type spelling.
@[inline]
fn (value &ParamMeta) typ_text() string {
	return flat.source_type_name(value.typ)
}

// comptime_typ_text resolves the indexed source type spelling.
@[inline]
fn (value &FieldMeta) comptime_typ_text() string {
	return flat.source_type_name(value.comptime_typ)
}

// comptime_unaliased_text resolves the indexed source type spelling.
@[inline]
fn (value &FieldMeta) comptime_unaliased_text() string {
	return flat.source_type_name(value.comptime_unaliased)
}

// unaliased_typ_text resolves the indexed source type spelling.
@[inline]
fn (value &FieldMeta) unaliased_typ_text() string {
	return flat.source_type_name(value.unaliased_typ)
}

// return_type_text resolves the indexed source type spelling.
@[inline]
fn (value &MethodMeta) return_type_text() string {
	return flat.source_type_name(value.return_type)
}

// enum_name_text resolves the enum source type spelling.
@[inline]
fn (value &EnumValueMeta) enum_name_text() string {
	return flat.source_type_name(value.enum_name)
}
