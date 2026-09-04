module c

import strconv
import v3.flat
import v3.gen.c.naming
import v3.types

fn enum_decl_is_flag(node flat.Node) bool {
	return node.typ == 'flag'
}

fn enum_decl_backing_type(node flat.Node) ?string {
	if node.generic_params().len > 0 && node.generic_params()[0].len > 0 {
		return node.generic_params()[0]
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
		c_name: g.cname(enum_name)
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
	clean_type := cgen_unalias_type(t)
	if clean_type is types.Pointer {
		if clean_type.base_type is types.OptionType || clean_type.base_type is types.ResultType {
			return g.optional_type_name(clean_type.base_type) + '*'
		}
	}
	mut base_type := types.Type(types.void_)
	if clean_type is types.OptionType {
		base_type = clean_type.base_type
	} else if clean_type is types.ResultType {
		base_type = clean_type.base_type
	} else {
		if clean_type is types.MultiReturn {
			// The checker-level name spells fn-type parts as `fn_ptr_void_void`;
			// the emitted typedef uses the resolved `_fn_ptr_<hash>` form.
			return g.multi_return_c_type_name(clean_type)
		}
		return g.tc.c_type(clean_type)
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

fn optional_result_unalias_type(t types.Type) types.Type {
	if t is types.Alias {
		base := optional_result_unalias_type(t.base_type)
		if base is types.OptionType || base is types.ResultType {
			return base
		}
	}
	return t
}

fn (mut g FlatGen) optional_type_name_for_context(t types.Type, concrete_optional bool) string {
	if concrete_optional && type_is_optional_result(t) {
		return g.concrete_optional_type_name(t)
	}
	return g.optional_type_name(t)
}

fn (mut g FlatGen) current_fn_optional_type_name(t types.Type) string {
	return g.optional_type_name_for_context(t, g.cur_fn_is_specialized)
}

fn (mut g FlatGen) value_c_type(t types.Type) string {
	if shared_alias_ptr := g.shared_alias_pointer_type(t) {
		return g.tc.c_type(shared_alias_ptr)
	}
	clean_type := g.value_unalias_type(t)
	if clean_type is types.OptionType || clean_type is types.ResultType {
		return g.optional_type_name(clean_type)
	}
	if clean_type is types.Pointer {
		if clean_type.base_type is types.OptionType || clean_type.base_type is types.ResultType {
			return g.optional_type_name(clean_type.base_type) + '*'
		}
		if fn_type := fn_type_from(clean_type.base_type) {
			// `fn_ptr:void|void*` is ambiguous: it can mean `&fn ()` or
			// `fn (voidptr)`. Resolve the function itself first, then add the
			// pointer declarator explicitly.
			return g.resolve_fn_ptr_type(g.tc.c_type(fn_type)) + '*'
		}
	}
	if clean_type is types.MultiReturn {
		return g.multi_return_c_type_name(clean_type)
	}
	if clean_type is types.Enum {
		return g.enum_value_c_type(clean_type)
	}
	if clean_type is types.ArrayFixed {
		return g.fixed_array_c_type(clean_type)
	}
	if clean_type is types.Channel {
		return 'chan'
	}
	mut ct := g.tc.c_type(clean_type)
	if ct.starts_with('fn_ptr:') {
		ct = g.resolve_fn_ptr_type(ct)
	}
	mut bare_ct := ct
	mut pointer_suffix := ''
	for bare_ct.ends_with('*') {
		bare_ct = bare_ct[..bare_ct.len - 1]
		pointer_suffix += '*'
	}
	// Specialized generic bodies can retain a stale bare concrete type spelling.
	if optional_payload_is_bare_struct(clean_type) {
		if qualified := g.unique_qualified_struct_c_type(bare_ct) {
			return qualified + pointer_suffix
		}
	}
	for candidate in [ct, 'main.${ct}'] {
		if target := g.tc.type_aliases[candidate] {
			return g.tc.c_type(cgen_unalias_type(g.tc.parse_type(target)))
		}
	}
	return ct
}

fn (mut g FlatGen) value_unalias_type(typ types.Type) types.Type {
	clean_type := cgen_unalias_type(typ)
	if clean_type is types.Struct {
		// Generic substitution can preserve a caller alias only as its type name
		// after the specialized body has moved into the generic function's module.
		// Recover the registered alias before selecting the C storage type.
		for candidate in [clean_type.name, 'main.${clean_type.name}'] {
			if target := g.tc.type_aliases[candidate] {
				return cgen_unalias_type(g.tc.parse_type(target))
			}
		}
	}
	return clean_type
}

fn cgen_unalias_type(typ types.Type) types.Type {
	mut current := typ
	for _ in 0 .. 1000 {
		if current is types.Alias {
			current = current.base_type
			continue
		}
		return current
	}
	return current
}

fn cgen_types_equal_after_alias_erasure(left types.Type, right types.Type) bool {
	l := cgen_unalias_type(left)
	r := cgen_unalias_type(right)
	if l is types.OptionType {
		return r is types.OptionType
			&& cgen_types_equal_after_alias_erasure(l.base_type, r.base_type)
	}
	if l is types.ResultType {
		return r is types.ResultType
			&& cgen_types_equal_after_alias_erasure(l.base_type, r.base_type)
	}
	if l is types.Pointer {
		return r is types.Pointer && cgen_types_equal_after_alias_erasure(l.base_type, r.base_type)
	}
	if l is types.Array {
		return r is types.Array && cgen_types_equal_after_alias_erasure(l.elem_type, r.elem_type)
	}
	if l is types.ArrayFixed {
		return r is types.ArrayFixed && l.len == r.len
			&& cgen_types_equal_after_alias_erasure(l.elem_type, r.elem_type)
	}
	if l is types.Channel {
		return r is types.Channel && cgen_types_equal_after_alias_erasure(l.elem_type, r.elem_type)
	}
	if l is types.Map {
		return r is types.Map && cgen_types_equal_after_alias_erasure(l.key_type, r.key_type)
			&& cgen_types_equal_after_alias_erasure(l.value_type, r.value_type)
	}
	if l is types.FnType {
		if r !is types.FnType {
			return false
		}
		if l.params.len != r.params.len
			|| !cgen_types_equal_after_alias_erasure(l.return_type, r.return_type) {
			return false
		}
		for i, param in l.params {
			if !cgen_types_equal_after_alias_erasure(param, r.params[i]) {
				return false
			}
		}
		return true
	}
	if l is types.MultiReturn {
		if r !is types.MultiReturn {
			return false
		}
		if l.types.len != r.types.len {
			return false
		}
		for i, item in l.types {
			if !cgen_types_equal_after_alias_erasure(item, r.types[i]) {
				return false
			}
		}
		return true
	}
	return l.name() == r.name()
}

fn (mut g FlatGen) multi_return_c_type_name(t types.MultiReturn) string {
	mut parts := []string{cap: t.types.len}
	for item in t.types {
		parts << naming.type_name_part(g.multi_return_field_name_type(item))
	}
	return 'multi_return_${parts.join('_')}'
}

fn (mut g FlatGen) multi_return_field_name_type(t types.Type) string {
	if t is types.FnType {
		return g.tc.c_type(t)
	}
	if t is types.Alias && t.base_type is types.FnType {
		return g.tc.c_type(t.base_type)
	}
	return g.multi_return_field_c_type(t)
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
	clean_t := optional_result_unalias_type(t)
	if clean_t is types.OptionType {
		if clean_t.base_type is types.Void {
			return 'int', types.Type(types.int_)
		}
		return g.optional_payload_c_type(clean_t.base_type), clean_t.base_type
	} else if clean_t is types.ResultType {
		if clean_t.base_type is types.Void {
			return 'int', types.Type(types.int_)
		}
		return g.optional_payload_c_type(clean_t.base_type), clean_t.base_type
	}
	return 'int', types.Type(types.int_)
}

fn (mut g FlatGen) optional_value_info(t types.Type, opt_ct string) (string, types.Type) {
	val_ct0, mut val_type := g.optional_value_ct(t)
	mut val_ct := if val_type is types.MultiReturn {
		g.optional_payload_c_type(val_type)
	} else {
		val_ct0
	}
	val_ct = g.optional_payload_c_type_for_optional_ct(opt_ct, val_ct)
	if opt_ct.starts_with('Optional_') && opt_ct.ends_with('ptr') {
		val_ct = '${opt_ct['Optional_'.len..opt_ct.len - 3]}*'
	}
	semantic_ct := g.optional_payload_c_type(val_type)
	if val_ct.ends_with('*') && !semantic_ct.ends_with('*') {
		val_type = types.Type(types.Pointer{
			base_type: val_type
		})
	}
	return val_ct, val_type
}

fn (mut g FlatGen) optional_payload_c_type(t types.Type) string {
	if t is types.ArrayFixed {
		return g.fixed_array_c_type(t)
	}
	mut ct := g.value_c_type(t)
	mut pointer_suffix := ''
	for ct.ends_with('*') {
		ct = ct[..ct.len - 1]
		pointer_suffix += '*'
	}
	if source_name := optional_payload_struct_name(t) {
		canonical_name := g.canonical_import_alias_type_text(source_name)
		lookup_name := if canonical_name.starts_with('main.') {
			canonical_name['main.'.len..]
		} else {
			canonical_name
		}
		if canonical_name != source_name && lookup_name in g.tc.structs {
			return g.struct_cname(canonical_name) + pointer_suffix
		}
	}
	// A concrete generic type can reach this collector through a stale bare
	// specialization spelling while its declaration is module-qualified
	// (`StructKeyDecodeResult_T` vs `json2__StructKeyDecodeResult_T`). Resolve
	// the unique declaration here so we neither emit an unusable phantom
	// Optional typedef nor duplicate the correctly qualified one.
	if optional_payload_is_bare_struct(t) {
		if qualified := g.unique_qualified_struct_c_type(ct) {
			return qualified + pointer_suffix
		}
	}
	// A stale declaration spelling can also lose the nominal kind and represent
	// an interface as a bare struct. Only consult the interface registry for
	// interface-like semantic types; concrete qualified types such as `C.Value`
	// can legitimately share their bare C spelling with a V interface.
	if optional_payload_may_have_stale_interface_spelling(t) {
		if qualified := g.unique_qualified_interface_c_type(ct) {
			return qualified + pointer_suffix
		}
	}
	return ct + pointer_suffix
}

fn optional_payload_may_have_stale_interface_spelling(t types.Type) bool {
	mut clean := cgen_unalias_type(t)
	for clean is types.Pointer {
		clean = cgen_unalias_type(clean.base_type)
	}
	return clean is types.Interface || (clean is types.Struct && !clean.name.contains('.'))
}

fn optional_payload_struct_name(t types.Type) ?string {
	mut clean := t
	for clean is types.Pointer {
		clean = clean.base_type
	}
	if clean is types.Struct {
		return clean.name
	}
	return none
}

fn (g &FlatGen) canonical_import_alias_type_text(typ string) string {
	clean := typ.trim_space()
	for prefix in ['&', '?', '!', '[]'] {
		if clean.starts_with(prefix) {
			return prefix + g.canonical_import_alias_type_text(clean[prefix.len..])
		}
	}
	if clean.starts_with('map[') {
		bracket_end := shared_generic_matching_bracket(clean, 3)
		if bracket_end < clean.len - 1 {
			key := g.canonical_import_alias_type_text(clean[4..bracket_end])
			value := g.canonical_import_alias_type_text(clean[bracket_end + 1..])
			return 'map[${key}]${value}'
		}
	}
	base, args, ok := parse_shared_generic_app_parts(clean)
	if ok {
		mut canonical_args := []string{cap: args.len}
		for arg in args {
			canonical_args << g.canonical_import_alias_type_text(arg)
		}
		canonical_base := g.canonical_import_alias_type_text(base)
		return '${canonical_base}[${canonical_args.join(', ')}]'
	}
	if clean.contains('.') {
		alias := clean.all_before('.')
		if module_name := g.current_file_import_alias_module(alias) {
			return module_name + clean[alias.len..]
		}
	}
	return clean
}

fn (g &FlatGen) current_file_import_alias_module(alias string) ?string {
	if g.tc.cur_file.len == 0 {
		return none
	}
	return g.tc.file_imports['${g.tc.cur_file}\n${alias}'] or { none }
}

fn optional_payload_is_bare_struct(t types.Type) bool {
	mut clean := t
	for clean is types.Pointer {
		clean = clean.base_type
	}
	return clean is types.Struct && !clean.name.contains('.')
}

// optional_typedefs supports optional typedefs handling for FlatGen.
fn (mut g FlatGen) optional_typedefs() {
	g.collect_optional_typedefs()
	mut wrote := false
	mut names := g.needed_optional_types.keys()
	names.sort()
	for opt_name in names {
		val_type := g.needed_optional_types[opt_name]
		if g.emit_optional_typedef(opt_name, val_type) {
			wrote = true
		}
	}
	if wrote {
		g.writeln('')
	}
}

fn (mut g FlatGen) collect_optional_typedefs() {
	if g.optional_types_ready {
		return
	}
	g.collect_declaration_signature_types()
	// Calls without a resolved expression type are the only optional-type source
	// not covered by the shared declaration-signature scan.
	mut seen_type_ids := []bool{len: 65536}
	mut seen_type_texts := map[string]bool{}
	for idx, node in g.a.nodes {
		if node.kind != .call || (idx < g.tc.expr_type_set.len && g.tc.expr_type_set[idx]) {
			continue
		}
		if idx < g.tc.resolved_call_set.len && g.tc.resolved_call_set[idx] {
			name := g.tc.resolved_call_names[idx]
			if name in g.tc.fn_ret_types {
				// collect_declaration_signature_types() already processed this exact
				// return entry; only calls without checker return metadata need their
				// transformed node spelling inspected below.
				continue
			}
		}
		if node.typ.len > 0 && node.typ !in ['int', 'array', 'map', 'unknown'] && cgen_type_text_is_complete(node.typ) {
			type_id := node.type_text_id()
			if type_id != 0 {
				if seen_type_ids[int(type_id)] {
					continue
				}
				seen_type_ids[int(type_id)] = true
			} else {
				if node.typ in seen_type_texts {
					continue
				}
				seen_type_texts[node.typ] = true
			}
			g.collect_optional_typedef_type(g.parse_node_type(&node))
		}
	}
	g.optional_types_ready = true
}

fn cgen_type_text_is_complete(text string) bool {
	mut parens := 0
	mut brackets := 0
	for ch in text {
		match ch {
			`(` {
				parens++
			}
			`)` {
				if parens == 0 {
					return false
				}
				parens--
			}
			`[` {
				brackets++
			}
			`]` {
				if brackets == 0 {
					return false
				}
				brackets--
			}
			else {}
		}
	}
	return parens == 0 && brackets == 0
}

fn (mut g FlatGen) collect_declaration_signature_types() {
	if g.decl_types_ready {
		return
	}
	// Specialized generic-interface wrappers are synthesized from the interface
	// declaration instead of appearing as standalone AST declarations. Discover
	// their applications here as well so tuple and option return ABIs are defined
	// before the wrapper forward declarations use them.
	g.register_specialized_interface_applications()
	for iface_name, methods in g.interfaces {
		_, _, is_specialized := parse_shared_generic_app_parts(iface_name)
		if !is_specialized {
			continue
		}
		for method in methods {
			decl_key := g.interface_method_signature_key(iface_name, method) or { continue }
			params, ret := g.tc.specialized_interface_method_signature(iface_name, decl_key)
			g.collect_declaration_signature_type_for_context(ret, true)
			for param in params {
				g.collect_declaration_signature_type_for_context(param, true)
			}
		}
	}
	old_module := g.tc.cur_module
	old_file := g.tc.cur_file
	defer {
		g.tc.cur_module = old_module
		g.tc.cur_file = old_file
	}
	// Parallel monomorph workers can append concrete declarations before their
	// checker signature maps are merged. Read declaration nodes directly too, so
	// an option/result used only by a worker specialization still gets a typedef.
	mut cur_module := ''
	mut cur_file := ''
	for idx in g.top_level_nodes() {
		node := g.a.nodes[idx]
		if node.kind == .file {
			cur_file = node.value
			cur_module = g.tc.file_modules[cur_file] or { '' }
			continue
		}
		if node.kind == .module_decl {
			cur_module = node.value
			continue
		}
		if node.kind != .fn_decl {
			continue
		}
		concrete_optional := g.a.specialized_fn_nodes[idx]
			|| g.name_uses_specialized_generic_abi(node.value)
		if !concrete_optional {
			continue
		}
		g.tc.cur_module = g.a.specialized_fn_modules[idx] or { cur_module }
		g.tc.cur_file = g.a.specialized_fn_files[idx] or {
			g.tc.fn_type_files[node.value] or { cur_file }
		}
		if node.typ.len > 0 {
			g.collect_declaration_signature_type_for_context(g.tc.parse_type(node.typ), concrete_optional)
		}
		for i in 0 .. node.children_count {
			param := g.a.child_node(&node, i)
			if param.kind != .param {
				break
			}
			g.collect_declaration_signature_type_for_context(g.tc.parse_type(param.typ), concrete_optional)
		}
	}
	mut seen := &PreseedTypeSeen{}
	for name, ret in g.tc.fn_ret_types {
		// Generic template signatures keep unspecialized placeholder types
		// (`!&Tls[T]`); their typedefs would reference C types that are never
		// emitted. Specializations register concrete signatures under their
		// own suffixed names.
		if name in g.tc.fn_generic_params {
			continue
		}
		if !cgen_type_first_seen(ret, mut seen) {
			continue
		}
		g.collect_declaration_signature_type(ret)
	}
	for name, params in g.tc.fn_param_types {
		if name in g.tc.fn_generic_params {
			continue
		}
		for param in params {
			if !cgen_type_first_seen(param, mut seen) {
				continue
			}
			g.collect_declaration_signature_type(param)
		}
	}
	for _, fields in g.tc.structs {
		for field in fields {
			if !cgen_type_first_seen(field.typ, mut seen) {
				continue
			}
			g.collect_declaration_signature_type(field.typ)
		}
	}
	for _, fields in g.tc.interface_fields {
		for field in fields {
			if !cgen_type_first_seen(field.typ, mut seen) {
				continue
			}
			g.collect_declaration_signature_type(field.typ)
		}
	}
	for _, typ in g.tc.c_globals {
		if !cgen_type_first_seen(typ, mut seen) {
			continue
		}
		g.collect_declaration_signature_type(typ)
	}
	for _, typ in g.tc.const_types {
		if !cgen_type_first_seen(typ, mut seen) {
			continue
		}
		g.collect_declaration_signature_type(typ)
	}
	for idx, is_set in g.tc.expr_type_set {
		if !is_set || idx >= g.tc.expr_type_values.len {
			continue
		}
		typ := g.tc.expr_type_values[idx]
		if !cgen_type_first_seen(typ, mut seen) {
			continue
		}
		g.collect_declaration_signature_type(typ)
	}
	g.decl_types_ready = true
	g.multi_return_types_ready = true
}

@[inline]
fn cgen_type_first_seen(typ &types.Type, mut seen PreseedTypeSeen) bool {
	words := unsafe { &u64(voidptr(typ)) }
	w0 := unsafe { words[0] }
	w1 := unsafe { words[1] }
	slot := int((w0 >> 4 ^ w1) & 4095)
	if seen.seen[slot] && seen.w0[slot] == w0 && seen.w1[slot] == w1 {
		return false
	}
	seen.w0[slot] = w0
	seen.w1[slot] = w1
	seen.seen[slot] = true
	return true
}

fn (mut g FlatGen) collect_declaration_signature_type(t types.Type) {
	g.collect_declaration_signature_type_for_context(t, false)
}

fn (mut g FlatGen) collect_declaration_signature_type_for_context(t types.Type, concrete_optional bool) {
	// Erased-template signatures keep their placeholder spellings in the
	// checker tables even when the program itself uses no generics
	// (skip_generics); force the placeholder check so an unused template's
	// `!&Tls[T]` return cannot leave an Optional_..._T typedef referencing a
	// C type that is never emitted.
	g.placeholder_check_forced = true
	skip := g.type_contains_generic_placeholder(t)
	g.placeholder_check_forced = false
	if skip {
		return
	}
	g.collect_concrete_optional_typedef_type_for_context(t, concrete_optional)
	g.collect_known_concrete_multi_return_type(t)
}

fn (mut g FlatGen) collect_optional_typedef_type(t types.Type) {
	if g.type_contains_generic_placeholder(t) {
		return
	}
	g.collect_concrete_optional_typedef_type(t)
}

fn (mut g FlatGen) collect_concrete_optional_typedef_type(t types.Type) {
	g.collect_concrete_optional_typedef_type_for_context(t, false)
}

fn (mut g FlatGen) collect_concrete_optional_typedef_type_for_context(t types.Type, concrete_optional bool) {
	match t {
		types.OptionType {
			if concrete_optional {
				g.concrete_optional_type_name(t)
			} else {
				g.optional_type_name(t)
			}
			g.collect_concrete_optional_typedef_type_for_context(t.base_type, concrete_optional)
		}
		types.ResultType {
			if concrete_optional {
				g.concrete_optional_type_name(t)
			} else {
				g.optional_type_name(t)
			}
			g.collect_concrete_optional_typedef_type_for_context(t.base_type, concrete_optional)
		}
		types.Array {
			g.collect_concrete_optional_typedef_type_for_context(t.elem_type, concrete_optional)
		}
		types.ArrayFixed {
			g.collect_concrete_optional_typedef_type_for_context(t.elem_type, concrete_optional)
		}
		types.Channel {
			g.collect_concrete_optional_typedef_type_for_context(t.elem_type, concrete_optional)
		}
		types.Map {
			g.collect_concrete_optional_typedef_type_for_context(t.key_type, concrete_optional)
			g.collect_concrete_optional_typedef_type_for_context(t.value_type, concrete_optional)
		}
		types.Pointer {
			g.collect_concrete_optional_typedef_type_for_context(t.base_type, concrete_optional)
		}
		types.FnType {
			for param in t.params {
				g.collect_concrete_optional_typedef_type_for_context(param, concrete_optional)
			}
			g.collect_concrete_optional_typedef_type_for_context(t.return_type, concrete_optional)
		}
		types.Alias {
			g.collect_concrete_optional_typedef_type_for_context(t.base_type, concrete_optional)
		}
		types.MultiReturn {
			for typ in t.types {
				g.collect_concrete_optional_typedef_type_for_context(typ, concrete_optional)
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
			// A stale generic-call annotation can carry the concrete C function name
			// as a nominal type. It is not a payload type and must not create an
			// `Optional_<function>` typedef in the program prefix.
			if t.name.contains('_T_') && !g.type_name_known(t.name) {
				return true
			}
			if type_name_is_unbound_generic_decl(t.name, g.struct_generic_params_for_name(t.name), t.name in g.tc.structs || g.tc.qualify_name(t.name) in g.tc.structs) {
				return true
			}
			return g.type_name_contains_generic_placeholder(t.name)
		}
		types.Interface {
			return g.type_name_contains_generic_placeholder(t.name)
		}
		types.Enum {
			return g.type_name_contains_generic_placeholder(t.name)
		}
		types.SumType {
			if type_name_is_unbound_generic_decl(t.name, g.sum_generic_params_for_name(t.name), t.name in g.tc.sum_types || g.tc.qualify_name(t.name) in g.tc.sum_types) {
				return true
			}
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

fn (g &FlatGen) struct_generic_params_for_name(name string) []string {
	base, _, ok := g.shared_generic_app_parts(name)
	if !ok {
		return []string{}
	}
	return g.tc.struct_generic_params[base] or {
		g.tc.struct_generic_params[base.all_after_last('.')] or { []string{} }
	}
}

fn (g &FlatGen) sum_generic_params_for_name(name string) []string {
	base, _, ok := g.shared_generic_app_parts(name)
	if !ok {
		return []string{}
	}
	return g.tc.sum_generic_params[base] or {
		g.tc.sum_generic_params[base.all_after_last('.')] or { []string{} }
	}
}

fn type_name_is_unbound_generic_decl(name string, params []string, materialized bool) bool {
	_, args, ok := parse_shared_generic_app_parts(name)
	if !ok || materialized || params.len == 0 {
		return false
	}
	for arg in args {
		if shared_type_text_uses_generic_params(arg, params) {
			return true
		}
	}
	return false
}

fn (g &FlatGen) type_name_contains_generic_placeholder(name string) bool {
	if g.skip_generics && !g.placeholder_check_forced {
		return false
	}
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
	if g.cached_support_identifiers[opt_name] {
		g.emitted_optional_types[opt_name] = true
		return false
	}
	bare_val_type := val_type.trim_right('*')
	// Multi-return names can contain a module-qualified field component, but the
	// payload is the generated tuple struct rather than a stale source struct.
	if !bare_val_type.starts_with('multi_return_')
		&& (g.stale_ambiguous_qualified_struct_c_type(bare_val_type)
			|| g.stale_missing_qualified_struct_c_type(bare_val_type)) {
		// Stale generic annotations can lose the declaration module or inherit the
		// generic helper's module. The correctly qualified specialization registers
		// the usable optional; do not emit a phantom payload type here.
		g.emitted_optional_types[opt_name] = true
		return false
	}
	if !bare_val_type.contains('__') && g.stale_ambiguous_qualified_interface_c_type(bare_val_type) {
		// A stale unqualified signature cannot identify which imported interface it
		// belongs to. Its concrete, module-qualified signature registers the usable
		// typedef; do not emit an invalid C type for the ambiguous collector entry.
		g.emitted_optional_types[opt_name] = true
		return false
	}
	// A `?fn (...)` payload names a `_fn_ptr_<hash>` typedef. That typedef may only
	// have been registered (name reserved) without being emitted — e.g. discovered
	// via an unused declaration whose param resolves to a different C spelling than a
	// resolved use (`fn (int)` keyed as `fn_ptr:void|int` vs the emitted
	// `fn_ptr:void|i64` once `int` widens to i64). Emit the referenced fn-ptr typedef
	// now so this wrapper never references an undefined type.
	if bare_val_type.starts_with('_fn_ptr_') {
		g.ensure_fn_ptr_typedef_by_name(bare_val_type)
	}
	err_field := if g.has_ierror_interface() { 'IError err; ' } else { '' }
	g.writeln('typedef struct ${opt_name} { bool ok; ${err_field}${val_type} value; } ${opt_name};')
	g.emitted_optional_types[opt_name] = true
	return true
}

// ensure_fn_ptr_typedef_by_name emits the `_fn_ptr_<hash>` typedef registered under
// `name` when it has not been emitted yet. Used when an emitted type references a
// fn-ptr typedef that was only registered (name reserved) but never resolved/used.
fn (mut g FlatGen) ensure_fn_ptr_typedef_by_name(name string) {
	for encoded, registered_name in g.fn_ptr_types {
		if registered_name == name {
			g.emit_fn_ptr_typedef(encoded, name, mut g.emitted_fn_ptr_typedefs)
			return
		}
	}
}

// enum_decls supports enum decls handling for FlatGen.
fn (mut g FlatGen) enum_decls() {
	old_file := g.tc.cur_file
	old_module := g.tc.cur_module
	defer {
		g.tc.cur_file = old_file
		g.tc.cur_module = old_module
	}
	mut cur_module := ''
	mut emitted := map[string]bool{}
	for node_idx in g.top_level_nodes() {
		node := g.a.nodes[node_idx]
		node_ref := g.a.node(flat.NodeId(node_idx))
		match node.kind {
			.file {
				cur_module = g.tc.file_modules[node.value] or { '' }
				g.tc.cur_file = node.value
				g.tc.cur_module = cur_module
			}
			.module_decl {
				cur_module = node.value
				g.tc.cur_module = node.value
			}
			.enum_decl {
				name := g.enum_decl_type_name(node, cur_module)
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
							f := g.a.child_node(node_ref, i)
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
							f := g.a.child_node(node_ref, i)
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
							f := g.a.child_node(node_ref, i)
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
								if enum_val := g.enum_field_expr_value_with_enum(expr_id, cur_module, node.value, mut field_values, field_exprs, mut resolving) {
									value = enum_val
									value_known = true
									value_expr = enum_val.str()
								} else {
									// Preserve expressions outside V's 32-bit `int` range so the C
									// storage type can represent wide backed enum values without truncation.
									value_known = false
									value_expr = g.enum_field_expr_to_string_with_enum(expr_id, cur_module, node.value, cn, field_names) or {
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
				mut field_values := map[string]i64{}
				mut field_exprs := map[string]flat.NodeId{}
				mut field_names := map[string]bool{}
				for i in 0 .. node.children_count {
					f := g.a.child_node(node_ref, i)
					field_names[f.value] = true
					if f.children_count > 0 {
						field_exprs[f.value] = g.a.child(f, 0)
					}
				}
				if is_flag {
					mut val := 0
					for i in 0 .. node.children_count {
						f := g.a.child_node(node_ref, i)
						if f.children_count > 0 {
							mut resolving := map[string]bool{}
							if enum_val := g.enum_field_expr_value_with_enum(g.a.child(f, 0), cur_module, node.value, mut field_values, field_exprs, mut resolving) {
								val = int(enum_val)
							}
						}
						field_values[f.value] = i64(val)
						cfield := g.cname(f.value)
						g.writeln('\t${cn}__${cfield} = ${1 << val},')
						val++
					}
				} else {
					mut next_value := i64(0)
					mut next_value_known := true
					mut next_value_expr := '0'
					for i in 0 .. node.children_count {
						f := g.a.child_node(node_ref, i)
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
							if enum_val := g.enum_field_expr_value_with_enum(expr_id, cur_module, node.value, mut field_values, field_exprs, mut resolving) {
								value = enum_val
								value_known = true
								value_expr = enum_val.str()
							} else {
								value_known = false
								value_expr = g.enum_field_expr_to_string_with_enum(expr_id, cur_module, node.value, cn, field_names) or {
									g.expr_to_string(expr_id)
								}
							}
						}
						if value_known {
							field_values[f.value] = value
						}
						g.writeln('\t${cn}__${g.cname(f.value)} = ${value_expr},')
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
	for node_idx in g.top_level_nodes() {
		node := g.a.nodes[node_idx]
		match node.kind {
			.file {
				cur_module = g.tc.file_modules[node.value] or { '' }
			}
			.module_decl {
				cur_module = node.value
			}
			.enum_decl {
				name := g.enum_decl_type_name(node, cur_module)
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
	for node_idx in g.top_level_nodes() {
		node := g.a.nodes[node_idx]
		match node.kind {
			.file {
				cur_module = g.tc.file_modules[node.value] or { '' }
			}
			.module_decl {
				cur_module = node.value
			}
			.enum_decl {
				name := g.enum_decl_type_name(node, cur_module)
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
						raw_fname := f.value
						fname := enum_field_display_name(raw_fname)
						cfield := g.cname(raw_fname)
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
						raw_fname := f.value
						fname := enum_field_display_name(raw_fname)
						cfield := g.cname(raw_fname)
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

fn (g &FlatGen) enum_decl_type_name(node flat.Node, module_name string) string {
	if node.value.contains('.') {
		return node.value
	}
	candidate := if module_name.len > 0 && module_name !in ['main', 'builtin'] {
		'${module_name}.${node.value}'
	} else {
		node.value
	}
	if candidate in g.tc.enum_names {
		return candidate
	}
	mut resolved := ''
	for name in g.tc.enum_names.keys() {
		if name.all_after_last('.') != node.value {
			continue
		}
		if resolved.len > 0 && resolved != name {
			return candidate
		}
		resolved = name
	}
	return if resolved.len > 0 { resolved } else { candidate }
}

fn (g &FlatGen) enum_autostr_c_name(type_name string) string {
	mut name := type_name
	if name.starts_with('main.') {
		name = name['main.'.len..]
	}
	if name in g.tc.enum_names {
		return g.cname(name)
	}
	if !name.contains('.') && g.tc.cur_module.len > 0 {
		qualified := '${g.tc.cur_module}.${name}'
		if qualified in g.tc.enum_names {
			return g.cname(qualified)
		}
	}
	short_name := name.all_after_last('.')
	if short_name in g.tc.enum_names {
		return g.cname(short_name)
	}
	if !name.contains('.') {
		suffix := '.${name}'
		mut match_name := ''
		mut matches := 0
		for enum_name, _ in g.tc.enum_names {
			if enum_name.ends_with(suffix) {
				match_name = enum_name
				matches++
				if matches > 1 {
					break
				}
			}
		}
		if matches == 1 {
			return g.cname(match_name)
		}
	}
	return g.cname(name)
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
			if enum_val := g.enum_field_expr_value_with_enum(g.a.child(f, 0), enum_module, node.value, mut field_values, field_exprs, mut resolving) {
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
		raw_fname := f.value
		fname := enum_field_display_name(raw_fname)
		cfield := g.cname(raw_fname)
		field_expr := '${cn}__${cfield}'
		g.writeln('\tif (${field_expr} != 0 && (__fe_v & (${storage_ct})${field_expr}) == (${storage_ct})${field_expr}) {')
		g.writeln('\t\tif (!__fe_first) { __fe_res = string__plus(__fe_res, (string){.str = (u8*)" | ", .len = 3, .is_lit = 1}); }')
		g.writeln('\t\t__fe_res = string__plus(__fe_res, (string){.str = (u8*)".${fname}", .len = ${fname.len + 1}, .is_lit = 1});')
		g.writeln('\t\t__fe_first = false;')
		g.writeln('\t}')
	}
	g.writeln('\t__fe_res = string__plus(__fe_res, (string){.str = (u8*)"}", .len = 1, .is_lit = 1});')
	g.writeln('\treturn __fe_res;')
	g.writeln('}')
	g.writeln('')
}

fn enum_field_display_name(name string) string {
	return if name.starts_with('@') { name[1..] } else { name }
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
				.power {
					int(enum_foldable_int_power(i64(left), i64(right)))
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
			if ev := g.enum_decl_field_ref_value(node.value, enum_module, enum_name, mut field_values, field_exprs, mut resolving) {
				return ev
			}
			lookup_module := if enum_module.len > 0 { enum_module } else { g.tc.cur_module }
			return i64(g.tc.const_int_value_in_module(node.value, lookup_module, []string{})?)
		}
		.paren {
			if node.children_count == 0 {
				return none
			}
			return g.enum_field_expr_value_with_enum(g.a.child(&node, 0), enum_module, enum_name, mut field_values, field_exprs, mut resolving)
		}
		.cast_expr {
			if node.children_count == 0 {
				return none
			}
			return g.enum_field_expr_value_with_enum(g.a.child(&node, 0), enum_module, enum_name, mut field_values, field_exprs, mut resolving)
		}
		.call {
			return g.enum_comptime_call_value(id, enum_module, enum_name, mut field_values, field_exprs, mut resolving)
		}
		.prefix {
			if node.children_count == 0 {
				return none
			}
			value := g.enum_field_expr_value_with_enum(g.a.child(&node, 0), enum_module, enum_name, mut field_values, field_exprs, mut resolving)?
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
			left := g.enum_field_expr_value_with_enum(g.a.child(&node, 0), enum_module, enum_name, mut field_values, field_exprs, mut resolving)?
			right := g.enum_field_expr_value_with_enum(g.a.child(&node, 1), enum_module, enum_name, mut field_values, field_exprs, mut resolving)?
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
				.power { enum_foldable_int_power(left, right) }
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
				return g.enum_decl_field_ref_value(field, enum_module, enum_name, mut field_values, field_exprs, mut resolving)
			}
			prefix := g.enum_decl_selector_base_text(g.a.child(&node, 0))
			if enum_type := g.enum_selector_base_name(prefix) {
				if value := g.enum_value_for_type(enum_type, node.value) {
					return i64(value)
				}
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
	for candidate_idx in g.top_level_nodes() {
		candidate := g.a.nodes[candidate_idx]
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
		locals[param.value] = g.enum_field_expr_value_with_enum(arg_id, enum_module, enum_name, mut field_values, field_exprs, mut resolving)?
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
				.power { enum_foldable_int_power(left, right) }
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

@[ignore_overflow]
fn enum_foldable_int_power(base i64, exponent i64) i64 {
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
			inner := g.enum_field_expr_to_string_with_enum(g.a.child(&node, 0), enum_module, enum_name, enum_c_name, field_names)?
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
			inner := g.enum_field_expr_to_string_with_enum(g.a.child(&node, 0), enum_module, enum_name, enum_c_name, field_names)?
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
			inner := g.enum_field_expr_to_string_with_enum(g.a.child(&node, 0), enum_module, enum_name, enum_c_name, field_names)?
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
			left := g.enum_field_expr_to_string_with_enum(g.a.child(&node, 0), enum_module, enum_name, enum_c_name, field_names)?
			right := g.enum_field_expr_to_string_with_enum(g.a.child(&node, 1), enum_module, enum_name, enum_c_name, field_names)?
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
	maybe_val := g.enum_field_expr_value_with_enum(expr_id, enum_module, enum_name, mut field_values, field_exprs, mut resolving)
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
fn (mut g FlatGen) type_alias_decls(emit_fn_ptr_aliases bool) {
	mut emitted := false
	mut main_aliases := map[string]bool{}
	if g.tc.autofree_mode {
		mut cur_module := ''
		for node_idx in g.top_level_nodes() {
			node := g.a.nodes[node_idx]
			match node.kind {
				.file {
					cur_module = g.tc.file_modules[node.value] or { '' }
				}
				.module_decl {
					cur_module = node.value
				}
				.type_decl {
					if cur_module in ['', 'main'] && node.children_count == 0 {
						main_aliases[node.value] = true
					}
				}
				else {}
			}
		}
	}
	for name, target in g.tc.type_aliases {
		if target.starts_with('fn_ptr:') || target.starts_with('C.') {
			continue
		}
		if g.has_builtins && !g.tc.autofree_mode {
			continue
		}
		if g.tc.autofree_mode && !main_aliases[name] {
			continue
		}
		mut ct := g.tc.c_type(g.tc.parse_type(target))
		is_fn_ptr_alias := ct.starts_with('fn_ptr:')
		if is_fn_ptr_alias {
			ct = g.resolve_fn_ptr_type(ct)
		}
		if is_fn_ptr_alias != emit_fn_ptr_aliases {
			continue
		}
		alias_cname := if g.tc.autofree_mode { g.cname('main.${name}') } else { g.cname(name) }
		if ct == 'void' || ct == alias_cname {
			continue
		}
		g.writeln('typedef ${ct} ${alias_cname};')
		emitted = true
	}
	if emitted {
		g.writeln('')
	}
}
