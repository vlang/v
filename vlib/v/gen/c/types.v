module c

import v.flat
import v.gen.c.naming
import v.types { unalias_type }

fn enum_decl_is_flag(node flat.Node) bool {
	return node.typ_text() == 'flag'
}

fn enum_decl_backing_type(node flat.Node) ?u32 {
	backing := node.enum_backing_type_id()
	if backing != 0 { return backing }
	return none
}

fn (g &FlatGen) enum_backing_storage_c_type(backing u32) string {
	return g.tc.c_type(g.tc.parse_type(backing))
}

fn (g &FlatGen) enum_emit_storage_c_type(enum_name u32, backing u32) string {
	if info := g.enum_backing_info(enum_name) {
		return info.storage_c_type_text()
	}
	return g.enum_backing_storage_c_type(backing)
}

fn enum_storage_c_type_is_unsigned(storage_ct string) bool {
	return storage_ct in ['u8', 'u16', 'u32', 'u64', 'size_t']
}

fn (mut g FlatGen) register_enum_backing_info(enum_name u32, backing u32) {
	info := EnumBackingInfo{
		c_name:         g.enum_typedef_c_name(flat.source_type_name(enum_name))
		storage_c_type: flat.source_type_id(g.enum_backing_storage_c_type(backing))
	}
	g.enum_backing_infos[enum_name] = info
	short := flat.source_type_syntax(enum_name).unqualified
	if short !in g.enum_backing_infos {
		g.enum_backing_infos[short] = info
	}
}

fn (g &FlatGen) enum_backing_info(enum_name u32) ?EnumBackingInfo {
	if info := g.enum_backing_infos[enum_name] {
		return info
	}
	short := flat.source_type_syntax(enum_name).unqualified
	if info := g.enum_backing_infos[short] {
		return info
	}
	return none
}

// wide_enum_signature_c_type returns the C type that function parameters and return
// values need for an enum with a backing type (`enum E as u64`). Those pass other
// enums as `int`. A narrower value round-trips through `int`, but a 64 bit value
// would be truncated, and a pointer (`mut e E`) must address the real storage: an
// `int*` to a `u8` field would overwrite the bytes after it.
fn (g &FlatGen) wide_enum_signature_c_type(t &types.Type) ?string {
	if t is types.Pointer {
		if t.base_type is types.Enum {
			if info := g.enum_backing_info(t.base_type.source_type_id()) {
				if info.storage_c_type !in [flat.source_type_int, flat.source_type_i32] {
					return info.c_name + '*'
				}
			}
			return none
		}
		base := g.wide_enum_signature_c_type(t.base_type)?
		return base + '*'
	}
	if t is types.Enum {
		if info := g.enum_backing_info(t.source_type_id()) {
			if info.storage_c_type in [flat.source_type_i64, flat.source_type_u64] {
				return info.c_name
			}
		}
	}
	return none
}

fn (g &FlatGen) enum_value_c_type(enum_type types.Enum) string {
	if info := g.enum_backing_info(enum_type.source_type_id()) {
		return info.c_name
	}
	return g.tc.c_type(types.Type(enum_type))
}

fn (g &FlatGen) enum_storage_c_type(enum_type types.Enum) string {
	if info := g.enum_backing_info(enum_type.source_type_id()) {
		return info.storage_c_type_text()
	}
	return g.tc.c_type(types.Type(enum_type))
}

// optional_type_name supports optional type name handling for FlatGen.
fn (mut g FlatGen) optional_type_name(t &types.Type) string {
	clean_type := unalias_type(t)
	if clean_type is types.Pointer {
		if clean_type.base_type is types.OptionType || clean_type.base_type is types.ResultType {
			return g.optional_type_name(clean_type.base_type) + '*'
		}
	}
	mut base_type := cgen_void_type
	if clean_type is types.OptionType {
		base_type = clean_type.base_type
	} else if clean_type is types.ResultType {
		base_type = clean_type.base_type
	} else {
		if clean_type is types.FnType {
			return g.fn_ptr_type_key(clean_type)
		}
		if clean_type is types.MultiReturn {
			// The checker-level name spells fn-type parts as `fn_ptr_void_void`;
			// the emitted typedef uses the resolved `_fn_ptr_<hash>` form.
			return g.multi_return_c_type_name(clean_type)
		}
		return g.tc.c_type(clean_type)
	}

	prefix := if clean_type is types.ResultType { '__v_result' } else { '__v_option' }
	if base_type is types.Void {
		return prefix
	}
	if g.type_contains_generic_placeholder(base_type) {
		return prefix
	}
	mut inner_ct := g.optional_payload_c_type(base_type)
	if inner_ct.starts_with('fn_ptr:') {
		inner_ct = g.resolve_fn_ptr_type(inner_ct)
	}
	if inner_ct == 'int' {
		return prefix
	}
	safe_name := inner_ct.replace('*', 'ptr').replace(' ', '_')
	opt_name := '${prefix}_${safe_name}'
	g.needed_optional_types.set_id(opt_name, flat.source_type_id(inner_ct))
	mut pointee := cgen_unalias_type(base_type)
	if pointee is types.Pointer {
		for pointee is types.Pointer {
			pointee = cgen_unalias_type(pointee.base_type)
		}
		if pointee is types.Struct && !flat.source_type_query(pointee.source_type_id()).has(.c_prefix)
			&& !types.is_builtin_type_name_id(pointee.source_type_id()) && !type_references_thread(pointee) {
			pointee_ct := g.optional_payload_c_type(pointee)
			g.optional_pointer_struct_tags[pointee_ct] = if pointee.name() in g.tc.unions {
				'union'
			} else {
				'struct'
			}
		}
	}
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

fn (mut g FlatGen) value_c_type(t &types.Type) string {
	g.note_thread_type_usage(t)
	if c_type := c_alias_value_c_type(t) {
		return c_type
	}
	if shared_alias_ptr := g.shared_alias_pointer_type(t) {
		return g.tc.c_type(shared_alias_ptr)
	}
	clean_type := g.value_unalias_type(t)
	if slot_type := g.sum_pointer_slot_c_type(clean_type) {
		return slot_type
	}
	if clean_type is types.OptionType || clean_type is types.ResultType {
		return g.optional_type_name(clean_type)
	}
	if clean_type is types.Pointer {
		if _ := array_fixed_type(types.unwrap_all_pointers(clean_type.base_type)) {
			return g.value_c_type(clean_type.base_type) + '*'
		}
		if clean_type.base_type is types.OptionType || clean_type.base_type is types.ResultType {
			return g.optional_type_name(clean_type.base_type) + '*'
		}
		if fn_type := fn_type_from(clean_type.base_type) {
			// `fn_ptr:void|void*` is ambiguous: it can mean `&fn ()` or
			// `fn (voidptr)`. Resolve the function itself first, then add the
			// pointer declarator explicitly.
			return g.resolve_fn_ptr_type(g.fn_ptr_type_key(fn_type)) + '*'
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
	if clean_type is types.FnType {
		return g.resolve_fn_ptr_type(g.fn_ptr_type_key(clean_type))
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
	// Both alias keys have the same final component, including C spellings.
	if g.name_may_be_type_alias(ct) {
		if target_source_id := g.tc.type_aliases.type_id(ct) {
			return g.tc.c_type(unalias_type(g.tc.parse_type(target_source_id)))
		}
		if target_source_id := main_type_alias_id(g.tc.type_aliases, ct) {
			return g.tc.c_type(unalias_type(g.tc.parse_type(target_source_id)))
		}
	}
	return ct
}

fn type_references_thread(typ &types.Type) bool {
	return match typ {
		types.Struct {
			name := flat.source_type_syntax(typ.source_type_id()).trimmed
			short_name := flat.source_type_syntax(name).unqualified
			name == cgen_thread_source_type || flat.source_type_shape(name).has(.thread_type)
				|| flat.source_type_syntax(name).thread_bracket_prefix
				|| short_name == cgen_thread_source_type || flat.source_type_shape(short_name).has(.thread_type)
				|| flat.source_type_syntax(short_name).thread_bracket_prefix
		}
		types.Array { type_references_thread(typ.elem_type) }
		types.ArrayFixed { type_references_thread(typ.elem_type) }
		types.Channel { type_references_thread(typ.elem_type) }
		types.Map {
			type_references_thread(typ.key_type) || type_references_thread(typ.value_type)
		}
		types.Pointer { type_references_thread(typ.base_type) }
		types.FnType {
			type_references_thread(typ.return_type) || typ.params.any(type_references_thread(it.typ))
		}
		types.OptionType { type_references_thread(typ.base_type) }
		types.ResultType { type_references_thread(typ.base_type) }
		types.Alias { type_references_thread(typ.base_type) }
		types.MultiReturn { typ.types.any(type_references_thread(it)) }
		else { false }
	}
}

fn pthread_source_type_name(name_id u32) bool {
	name := flat.source_type_syntax(name_id).trimmed
	if suffix := flat.source_type_qualified_suffix(name, flat.source_type_id('C')) {
		if flat.source_type_syntax(suffix).pthread_prefix { return true }
	}
	return flat.source_type_syntax(flat.source_type_syntax(name).unqualified).pthread_prefix
}

fn type_references_pthread(typ &types.Type) bool {
	return match typ {
		types.Struct {
			pthread_source_type_name(typ.source_type_id())
		}
		types.Array { type_references_pthread(typ.elem_type) }
		types.ArrayFixed { type_references_pthread(typ.elem_type) }
		types.Channel { type_references_pthread(typ.elem_type) }
		types.Map {
			type_references_pthread(typ.key_type) || type_references_pthread(typ.value_type)
		}
		types.Pointer { type_references_pthread(typ.base_type) }
		types.FnType {
			type_references_pthread(typ.return_type) || typ.params.any(type_references_pthread(it.typ))
		}
		types.OptionType { type_references_pthread(typ.base_type) }
		types.ResultType { type_references_pthread(typ.base_type) }
		types.Alias {
			pthread_source_type_name(typ.source_type_id())
				|| type_references_pthread(typ.base_type)
		}
		types.MultiReturn { typ.types.any(type_references_pthread(it)) }
		else { false }
	}
}

fn (mut g FlatGen) note_thread_type_usage(typ &types.Type) {
	if !g.target_libc_headers {
		return
	}
	if !g.needs_thread_type && type_references_thread(typ) {
		g.needs_thread_type = true
	}
	if !g.needs_pthread_header && type_references_pthread(typ) {
		g.needs_pthread_header = true
	}
}

fn (mut g FlatGen) precompute_thread_type_usage() {
	for _, fields in g.tc.structs {
		for field in fields {
			g.note_thread_type_usage(field.typ)
		}
	}
	for _, fields in g.tc.interface_fields {
		for field in fields {
			g.note_thread_type_usage(field.typ)
		}
	}
	for _, typ in g.global_types {
		g.note_thread_type_usage(typ)
	}
	for _, params in g.fn_decl_param_types {
		for typ in params {
			g.note_thread_type_usage(typ)
		}
	}
	for _, typ in g.fn_decl_ret_types {
		g.note_thread_type_usage(typ)
	}
	for _, variants_id in g.tc.sum_types.ids {
		variants := variants_id.map(flat.source_type_name(it))
		for variant in variants {
			g.note_thread_type_usage(g.tc.parse_type(flat.source_type_id(variant)))
		}
	}
}

fn c_alias_value_c_type(typ &types.Type) ?string {
	if typ is types.Alias && flat.source_type_query(typ.source_type_id()).has(.c_prefix) {
		return typ.name()['C.'.len..]
	}
	if typ is types.Pointer {
		if base := c_alias_value_c_type(typ.base_type) {
			return '${base}*'
		}
	}
	return none
}

fn (mut g FlatGen) value_unalias_type(typ &types.Type) types.Type {
	clean_type := unalias_type(typ)
	if clean_type is types.Struct {
		// Generic substitution can preserve a caller alias only as its type name
		// after the specialized body has moved into the generic function's module.
		// Recover the registered alias before selecting the C storage type.
		name := clean_type.name()
		if target_source_id := g.tc.type_aliases.type_id(name) {
			return *unalias_type(g.tc.parse_type(target_source_id))
		}
		if target_source_id := main_type_alias_id(g.tc.type_aliases, name) {
			return *unalias_type(g.tc.parse_type(target_source_id))
		}
	}
	return *clean_type
}

// known_type_id returns `id` when it is set, else interns `text`.
@[inline]
fn known_type_id(id u32, text string) u32 {
	return if id != 0 { id } else { flat.source_type_id(text) }
}

// main_type_alias_id reads the `main.<name>` alias. Struct values reach this for
// every C type query; join the key on the stack, as map reads do not keep it.
@[direct_array_access]
fn main_type_alias_id(aliases flat.TypeTextBindings, name string) ?u32 {
	key_len := name.len + 5
	if key_len > module_key_view_cap {
		return aliases.type_id('main.${name}')
	}
	mut buf := [module_key_view_cap]u8{}
	// Declarations stay outside the `unsafe` blocks: the FastC parser, which builds
	// the compiler with `-b fastc`, rejects `:=` inside a value block.
	p := unsafe { &u8(&buf[0]) }
	unsafe {
		vmemcpy(p, c'main.', 5)
		if name.len > 0 {
			vmemcpy(p + 5, name.str, name.len)
		}
	}
	key := unsafe { tos(p, key_len) }
	return aliases.type_id(key)
}

// cgen_type_is_map reports whether typ is a map, looking through any alias
// chain. The map header no longer carries a `len` field -- the count lives in
// `data->count` -- so a `.len` selector on an *alias* of a map has to take the
// map branch as well. Matching only the bare map type left the alias falling
// through to a plain field access, and the generated C did not compile:
// `error: no member named 'len' in 'struct map'`.
fn cgen_type_is_map(typ &types.Type) bool {
	clean := unalias_type(typ)
	return clean is types.Map || (clean is types.Struct && clean.source_type_id() == cgen_map_source_type)
}

fn cgen_types_equal_after_alias_erasure(left &types.Type, right &types.Type) bool {
	l := types.Type(*unalias_type(left))
	r := types.Type(*unalias_type(right))
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
			if !cgen_types_equal_after_alias_erasure(param.typ, r.params[i].typ) {
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
	return l.source_type_id() == r.source_type_id()
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
	if slot_type := g.sum_pointer_slot_c_type(t) { return slot_type }
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
	semantic_ct := g.optional_payload_c_type(val_type)
	if val_ct.ends_with('*') && !semantic_ct.ends_with('*') {
		val_type = types.Type(types.Pointer{
			base_type: types.new_type_reference(types.Type(val_type))
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
		canonical_name_id := g.canonical_import_alias_type_text(source_name)
		canonical_name := flat.source_type_name(canonical_name_id)
		lookup_name := if flat.source_type_query(canonical_name_id).has(.main_prefix) {
			canonical_name['main.'.len..]
		} else {
			canonical_name
		}
		if canonical_name_id != source_name && lookup_name in g.tc.structs {
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

fn optional_payload_may_have_stale_interface_spelling(t &types.Type) bool {
	mut clean := unalias_type(t)
	for clean is types.Pointer {
		clean = unalias_type(clean.base_type)
	}
	return clean is types.Interface || (clean is types.Struct && !flat.source_type_shape(clean.source_type_id()).has(.dotted))
}

fn optional_payload_struct_name(t &types.Type) ?u32 {
	mut clean := *t
	for clean is types.Pointer {
		clean = clean.base_type
	}
	if clean is types.Struct {
		return clean.source_type_id()
	}
	return none
}

fn (g &FlatGen) canonical_import_alias_type_text(type_id u32) u32 {
	return g.canonical_import_alias_type_text_in_file(type_id, g.tc.cur_file)
}

@[heap]
struct ImportFileTypeCache {
mut:
	texts  TypeNameLookupCache
	parsed map[u32]types.Type
}

@[heap]
struct ImportTypeCache {
mut:
	unqualified_texts TypeNameLookupCache
	by_file           map[string]&ImportFileTypeCache
	last_file         string
	last              &ImportFileTypeCache = unsafe { nil }
}

fn (mut cache ImportTypeCache) for_file(file string) &ImportFileTypeCache {
	if !isnil(cache.last) && cache.last_file == file {
		return cache.last
	}
	entry := cache.by_file[file] or {
		created := &ImportFileTypeCache{}
		cache.by_file[file] = created
		created
	}
	cache.last_file = file
	cache.last = entry
	return entry
}

@[direct_array_access]
fn (g &FlatGen) canonical_import_alias_type_in_file(type_id u32, file string) types.Type {
	mut cache := g.import_type_cache
	if isnil(cache) || flat.source_type_shape(type_id).has(.typeof_name) {
		return g.canonical_import_alias_type_in_file_uncached(type_id, file)
	}
	if !g.skip_generics {
		// Concrete annotations recur in generic bodies, but their resolutions
		// must not survive a specialization or a temporary declaration context.
		mut memo := g.usable_expr_type_memo
		if isnil(memo) || !memo.active || memo.file != g.tc.cur_file
			|| memo.module != g.tc.cur_module || g.const_init_depth > 0
			|| g.struct_default_module.len > 0 {
			return g.canonical_import_alias_type_in_file_uncached(type_id, file)
		}
		slot := int(type_id & u32(body_import_type_query_slots - 1))
		// The masked slot is in bounds; borrow the entry without copying its type.
		entry := unsafe { &memo.import_queries[slot] }
		if entry.generation == memo.generation && entry.type_id == type_id && entry.file == file {
			return entry.value
		}
		result := g.canonical_import_alias_type_in_file_uncached(type_id, file)
		if result !is types.Unknown && result !is types.Void
			&& !g.type_contains_generic_placeholder(result) {
			memo.import_queries[slot] = BodyImportTypeQuery{
				type_id:    type_id
				file:       file
				value:      result
				generation: memo.generation
			}
		}
		return result
	}
	mut entry := cache.for_file(file)
	if result := entry.parsed[type_id] { return result }
	result := g.canonical_import_alias_type_in_file_uncached(type_id, file)
	entry.parsed[type_id] = result
	return result
}

fn (g &FlatGen) canonical_import_alias_type_in_file_uncached(type_id u32, file string) types.Type {
	canonical := g.canonical_import_alias_type_text_in_file(type_id, file)
	if canonical != type_id {
		if exact := g.exact_known_import_type_text(canonical) {
			return exact
		}
	}
	return g.tc.parse_resolution_type_in_file(type_id, file)
}

fn (g &FlatGen) canonical_import_alias_type_for_node(typ &types.Type, node &flat.Node) types.Type {
	if !type_has_import_alias_text(typ) {
		return *typ
	}
	// A nominal type carries the ID of its name; other types intern theirs once.
	source_id := typ.source_type_id()
	// This function receives a semantic Type, not raw source text. Preserve an
	// exact registered name before repairing stale alias-qualified spellings;
	// otherwise a current file import can retarget a canonical type from a call.
	if exact := g.exact_known_import_type_text(source_id) {
		return exact
	}
	// Only dotted names can reference an import alias. Primitive and local
	// type spellings do not need a walk through synthesized child nodes.
	file := if flat.source_type_shape(source_id).has(.dotted) {
		g.node_source_file(node)
	} else {
		''
	}
	canonical_source_id := g.canonical_import_alias_type_text_in_file(source_id, file)
	if canonical_source_id != source_id {
		if exact := g.exact_known_import_type_text(canonical_source_id) {
			return exact
		}
	}
	return *typ
}

// Primitive containers already have canonical names. Inspect their leaves
// directly instead of allocating type text merely to normalize it unchanged.
fn type_has_import_alias_text(typ &types.Type) bool {
	return match typ {
		types.Struct, types.Interface, types.Enum, types.SumType, types.Alias, types.FnType, types.MultiReturn,
		types.Channel {
			true
		}
		types.Pointer, types.OptionType, types.ResultType {
			type_has_import_alias_text(typ.base_type)
		}
		types.Array {
			type_has_import_alias_text(typ.elem_type)
		}
		types.ArrayFixed {
			typ.len_expr.len > 0 || type_has_import_alias_text(typ.elem_type)
		}
		types.Map {
			type_has_import_alias_text(typ.key_type) || type_has_import_alias_text(typ.value_type)
		}
		else { false }
	}
}

fn (mut g FlatGen) sizeof_target_in_file(value_id u32, file string) string {
	target_id := g.generic_default_type_id(value_id)
	canonical_source_id := g.canonical_import_alias_type_text_in_file(target_id, file)
	if canonical_source_id != target_id {
		if exact := g.exact_known_import_type_text(canonical_source_id) {
			return g.value_sizeof_target(exact)
		}
	}
	return g.sizeof_target(target_id)
}

fn (mut g FlatGen) import_alias_sizeof_target_in_file(value_id u32, file string) ?string {
	canonical_source_id := g.canonical_import_alias_type_text_in_file(value_id, file)
	if canonical_source_id == value_id {
		return none
	}
	return g.value_sizeof_target(g.exact_known_import_type_text(canonical_source_id)?)
}

fn (g &FlatGen) exact_known_import_type_text(typ_id u32) ?types.Type {
	clean_id := flat.source_type_syntax(typ_id).trimmed
	shape := flat.source_type_shape(clean_id)
	syntax := flat.source_type_syntax(clean_id)
	if shape.has(.pointer) {
		return types.Type(types.Pointer{
			base_type: types.new_type_reference(types.Type(g.exact_known_import_type_text(syntax.raw_base)?))
		})
	}
	if shape.has(.option_type) {
		return types.Type(types.OptionType{
			base_type: types.new_type_reference(types.Type(g.exact_known_import_type_text(syntax.raw_base)?))
		})
	}
	if shape.has(.result_type) {
		return types.Type(types.ResultType{
			base_type: types.new_type_reference(types.Type(g.exact_known_import_type_text(syntax.raw_base)?))
		})
	}
	if shape.has(.array_type) {
		return types.Type(types.Array{
			elem_type: types.new_type_reference(types.Type(g.exact_known_import_type_text(syntax.raw_base)?))
		})
	}
	// These indexes are keyed by declaration names, so spelling access remains
	// at the lookup boundary without inspecting the name's bytes.
	clean := flat.source_type_name(clean_id)
	if g.tc.type_aliases.contains(clean) { return *g.tc.parse_canonical_type_cached(clean_id) }
	if clean in g.tc.structs { return types.Type(types.Struct{ name_id: clean_id }) }
	if shape.has(.dotted) {
		module_name := flat.source_type_name(syntax.qualifier)
		short_name := flat.source_type_name(syntax.unqualified)
		key := if module_name in ['', 'main', 'builtin'] { short_name } else { clean }
		if info := g.struct_decl_infos[key] {
			if info.module == module_name && info.node.source_value_id() == syntax.unqualified {
				return types.Type(types.Struct{ name_id: clean_id })
			}
		}
	}
	if clean in g.tc.interface_names { return types.Type(types.Interface{ name_id: clean_id }) }
	if g.tc.sum_types.contains(clean) { return types.Type(types.SumType{ name_id: clean_id }) }
	if clean in g.tc.enum_names {
		return types.Type(types.Enum{ name_id: clean_id, is_flag: clean in g.tc.flag_enums })
	}
	return none
}

fn (g &FlatGen) canonical_import_alias_type_text_in_file(type_id u32, file string) u32 {
	shape := flat.source_type_shape(type_id)
	// Bare, unpadded spellings have no import prefix or nested type to rewrite.
	// Padding must still reach the parser: trimming can expose wrapper syntax.
	if !shape.has(.dotted) && !shape.has(.bracketed) && !shape.has(.wrapped) && !shape.has(.padded) {
		return type_id
	}
	mut cache := g.import_type_cache
	if isnil(cache) {
		return g.canonical_import_alias_type_text_in_file_uncached(type_id, file)
	}
	if !shape.has(.dotted) {
		if !shape.has(.bracketed) && !shape.has(.wrapped) {
			return g.canonical_import_alias_type_text_in_file_uncached(type_id, file)
		}
		if result := cache.unqualified_texts.get_id(type_id) { return result }
		result := g.canonical_import_alias_type_text_in_file_uncached(type_id, file)
		cache.unqualified_texts.put_id(type_id, result)
		return result
	}
	mut entry := cache.for_file(file)
	if result := entry.texts.get_id(type_id) { return result }
	result := g.canonical_import_alias_type_text_in_file_uncached(type_id, file)
	entry.texts.put_id(type_id, result)
	return result
}

fn (g &FlatGen) canonical_import_alias_type_text_in_file_uncached(type_id u32, file string) u32 {
	clean := flat.source_type_syntax(type_id).trimmed
	shape := flat.source_type_shape(clean)
	syntax := flat.source_type_syntax(clean)
	if shape.has(.pointer) || shape.has(.option_type) || shape.has(.result_type) || shape.has(.array_type) {
		inner := syntax.raw_base
		canonical := g.canonical_import_alias_type_text_in_file(inner, file)
		if canonical == inner { return clean }
		kind := if shape.has(.pointer) {
			flat.SourceTypeComposition.pointer
		} else if shape.has(.option_type) {
			flat.SourceTypeComposition.option
		} else if shape.has(.result_type) {
			flat.SourceTypeComposition.result
		} else {
			flat.SourceTypeComposition.array
		}
		return flat.source_type_composition_one(kind, canonical)
	}
	if !shape.has(.dotted) && !shape.has(.bracketed) { return clean }
	if shape.has(.map_type) && syntax.bracket_closed && syntax.raw_base != 0 {
		key := syntax.bracket_raw_inner
		value := syntax.raw_base
		canonical_key := g.canonical_import_alias_type_text_in_file(key, file)
		canonical_value := g.canonical_import_alias_type_text_in_file(value, file)
		if key == canonical_key && value == canonical_value { return clean }
		return flat.source_type_composition_two(.map, canonical_key, canonical_value)
	}
	if syntax.cgen_app_valid {
		mut parts := []u32{cap: syntax.cgen_app_args.len + 1}
		parts << g.canonical_import_alias_type_text_in_file(syntax.cgen_app_base, file)
		for arg in syntax.cgen_app_args {
			parts << g.canonical_import_alias_type_text_in_file(arg, file)
		}
		return flat.source_type_composition(.generic, parts)
	}
	if shape.has(.dotted) {
		alias := flat.source_type_name(syntax.first_dot_prefix)
		if module_name := g.cached_file_import(file, alias) {
			module_id := flat.source_type_id(module_name)
			// Canonical module paths may themselves start with their import alias.
			if flat.source_type_has_qualified_prefix(clean, module_id) { return clean }
			return flat.source_type_composition_two(.qualified, module_id, syntax.first_dot_suffix)
		}
	}
	return clean
}

fn (g &FlatGen) node_source_file(node &flat.Node) string {
	// A synthesized node (a transform-created `sizeof`, a temporary, ...) carries no
	// position, and `source_files` is keyed by file id where 0 is a real file. An
	// unchecked lookup therefore resolves such a node against whichever file happens
	// to be first, which qualified a bare `Type` against that file's imports (giving
	// `io__Type` instead of `types__Type`). Fall through to the enclosing function's
	// file instead.
	if node.pos.is_valid() {
		if source_file := g.a.source_files[node.pos.id] {
			return source_file.name
		}
	}
	mut pending := []flat.Node{cap: node.children_count}
	for i in 0 .. node.children_count {
		pending << g.a.child_node(node, i)
	}
	for pending.len > 0 {
		child := pending.pop()
		if child.pos.is_valid() {
			if source_file := g.a.source_files[child.pos.id] {
				return source_file.name
			}
		}
		for i in 0 .. child.children_count {
			pending << g.a.node(g.a.child(&child, i))
		}
	}
	if g.cur_fn_source_file.len > 0 {
		return g.cur_fn_source_file
	}
	return g.tc.cur_file
}

fn (g &FlatGen) current_file_import_alias_module(alias string) ?string {
	if g.tc.cur_file.len == 0 {
		return none
	}
	return g.cached_file_import(g.tc.cur_file, alias) or { none }
}

fn optional_payload_is_bare_struct(t &types.Type) bool {
	mut clean := t
	for clean is types.Pointer {
		clean = clean.base_type
	}
	return clean is types.Struct && !flat.source_type_shape(clean.source_type_id()).has(.dotted)
}

// optional_typedefs supports optional typedefs handling for FlatGen.
fn (mut g FlatGen) optional_typedefs() {
	g.collect_optional_typedefs()
	// Declaration collection can have completed before the final selected-function
	// list is available. Revisit that concrete list immediately before emission so
	// every wrapper used by forward_decls() is already defined.
	g.collect_selected_declaration_signature_types()
	g.optional_pointer_struct_forward_decls()
	mut wrote := false
	mut names := g.needed_optional_types.keys()
	names.sort()
	for opt_name in names {
		val_type := flat.source_type_name(g.needed_optional_types.value_id(opt_name))
		if g.emit_optional_typedef(opt_name, val_type) {
			wrote = true
		}
	}
	if wrote {
		g.writeln('')
	}
}

// A signature can retain a pointer Result/Option after its unused generic
// specialization is discarded. Its pointee needs only an opaque declaration.
fn (mut g FlatGen) optional_pointer_struct_forward_decls() {
	mut needed := map[string]bool{}
	for _, payload_id in g.needed_optional_types.ids {
		mut base_id := payload_id
		for flat.source_type_query(base_id).has(.c_pointer_suffix) {
			base_id = flat.source_type_syntax(base_id).c_pointer_base
		}
		needed[flat.source_type_name(base_id)] = true
	}
	mut declared := map[string]bool{}
	for name in g.c_struct_decl_names() {
		declared[g.struct_cname(name)] = true
	}
	for name in g.c_sum_decl_names() {
		declared[g.cname(name)] = true
	}
	for name, _ in g.interfaces {
		declared[g.cname(name)] = true
	}
	mut names := g.optional_pointer_struct_tags.keys()
	names.sort()
	for name in names {
		if !needed[name] || name in declared || g.cached_support_has_c_type(name) {
			continue
		}
		tag := g.optional_pointer_struct_tags[name]
		g.writeln('typedef ${tag} ${name} ${name};')
	}
}

fn (mut g FlatGen) collect_optional_typedefs() {
	if g.optional_types_ready {
		return
	}
	g.collect_declaration_signature_types()
	g.collect_unresolved_call_optional_types()
	g.optional_types_ready = true
}

// collect_sizeof_optional_types collects `sizeof(?T)` and `sizeof(!T)` operands,
// which can be the only place a program names an option or result type.
fn (mut g FlatGen) collect_sizeof_optional_types() {
	old_file := g.tc.cur_file
	old_module := g.tc.cur_module
	for idx in g.type_metadata_nodes() {
		node := g.a.nodes[idx]
		if node.kind == .file {
			g.tc.cur_file = node.value
			g.tc.cur_module = g.tc.file_modules[node.value] or { old_module }
		} else if node.kind == .module_decl {
			g.tc.cur_module = node.value
		} else if is_optional_sizeof_node(&node) {
			g.collect_optional_typedef_type(g.tc.parse_type(node.source_value_id()))
		}
	}
	g.tc.cur_file = old_file
	g.tc.cur_module = old_module
}

fn (mut g FlatGen) collect_unresolved_call_optional_types() {
	// Calls without a resolved expression type and `sizeof` operands are the only
	// optional-type sources not covered by the shared declaration-signature scan.
	mut seen_type_ids := map[u32]bool{}
	for idx in g.type_metadata_nodes() {
		// This scan only publishes type metadata; keep source nodes borrowed.
		node := unsafe { &g.a.nodes[idx] }
		if node.kind != .call {
			continue
		}
		if node.typ == 0 || (idx < g.tc.expr_type_set.len && g.tc.expr_type_set[idx]) {
			continue
		}
		// Only a complete type spelling that was not collected yet can add a
		// typedef. Rule the rest out before the checker metadata below, which
		// costs a map lookup per resolved call.
		if node.typ in [flat.source_type_int, cgen_array_source_type, cgen_map_source_type,
			flat.source_type_unknown]
			|| !cgen_type_text_is_complete(node.typ) {
			continue
		}
		type_id := node.type_text_id()
		if seen_type_ids[type_id] {
			continue
		}
		if idx < g.tc.resolved_call_set.len && g.tc.resolved_call_set[idx] {
			name := g.tc.resolved_call_names[idx].value
			if name in g.tc.fn_ret_types {
				// collect_declaration_signature_types() already processed this exact
				// return entry; only calls without checker return metadata need their
				// transformed node spelling inspected below.
				continue
			}
		}
		seen_type_ids[type_id] = true
		g.collect_optional_typedef_type(g.parse_node_type(node))
	}
	g.collect_sizeof_optional_types()
}

fn cgen_type_text_is_complete(text_id u32) bool {
	return flat.source_type_query(text_id).has(.complete)
}

fn (mut g FlatGen) collect_declaration_signature_types() {
	if g.decl_types_ready {
		return
	}
	old_module := g.tc.cur_module
	old_file := g.tc.cur_file
	defer {
		g.tc.cur_module = old_module
		g.tc.cur_file = old_file
	}
	g.collect_specialized_declaration_signature_types()
	g.finish_declaration_signature_types()
}

fn (mut g FlatGen) collect_specialized_declaration_signature_types() {
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
			decl_key := g.interface_method_signature_key(flat.source_type_id(iface_name), method) or { continue }
			params, ret := g.tc.specialized_interface_method_signature(flat.source_type_id(iface_name), decl_key)
			g.collect_declaration_signature_type(ret)
			for param in params {
				g.collect_declaration_signature_type(param)
			}
		}
	}
	// Parallel monomorph workers can append concrete declarations before their
	// checker signature maps are merged. Read those declaration nodes directly too,
	// so an option/result used only by a worker specialization still gets a typedef.
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
		g.tc.cur_module = g.a.specialized_fn_modules[idx] or { cur_module }
		g.tc.cur_file = g.a.specialized_fn_files[idx] or {
			g.tc.fn_type_files[node.value] or { cur_file }
		}
		concrete_optional := g.is_program_specialization_fn_node(node, idx, g.tc.cur_module)
		if !concrete_optional {
			continue
		}
		return_type := g.fn_node_return_type(node, g.tc.cur_module)
		g.collect_declaration_signature_type(return_type)
		typed_params := g.fn_node_param_types(node, g.tc.cur_module)
		mut param_idx := 0
		for i in 0 .. node.children_count {
			param := g.a.child_node(&node, i)
			if param.kind != .param {
				break
			}
			raw_type := if param_idx < typed_params.len {
				typed_params[param_idx]
			} else {
				g.tc.parse_resolution_type(param.typ)
			}
			param_idx++
			g.collect_declaration_signature_type(g.fn_node_effective_param_type(param, raw_type))
		}
	}
}

fn (mut g FlatGen) finish_declaration_signature_types() {
	// The selected function list excludes open generic templates and carries the
	// exact module/file context later used by forward_decls(). Collect those known
	// concrete signatures without the conservative placeholder-name filter: in a
	// large program it can mistake short nominal payloads such as `Token` or `Type`
	// for generic parameters and omit their optional wrapper typedefs.
	g.collect_selected_declaration_signature_types()
	g.collect_checker_declaration_signature_types()
}

fn (mut g FlatGen) collect_checker_declaration_signature_types() {
	mut seen := &PreseedTypeSeen{}
	for name, ret in g.tc.fn_ret_types {
		if g.cache_decl_demand && !g.cache_function_declaration_needed(name) {
			continue
		}
		// Generic template signatures keep unspecialized placeholder types
		// (`!&Tls[T]`); their typedefs would reference C types that are never
		// emitted. Specializations register concrete signatures under their
		// own suffixed names.
		if name in g.tc.fn_generic_params {
			continue
		}
		if !seen.insert(ret) {
			continue
		}
		g.collect_declaration_signature_type(ret)
	}
	for name, params in g.tc.fn_param_types {
		if g.cache_decl_demand && !g.cache_function_declaration_needed(name) {
			continue
		}
		if name in g.tc.fn_generic_params {
			continue
		}
		for param in params {
			if !seen.insert(param) {
				continue
			}
			g.collect_declaration_signature_type(param)
		}
	}
	for name, fields in g.tc.structs {
		if g.cache_decl_demand && !g.cache_struct_declaration_needed(name) {
			continue
		}
		if name.starts_with('C.') {
			g.ensure_referenced_c_struct_ids()
			if !g.referenced_c_struct_ids[flat.source_type_id(name)] {
				continue
			}
		}
		for field in fields {
			if !seen.insert(field.typ) {
				continue
			}
			g.collect_declaration_signature_type(field.typ)
		}
	}
	for _, fields in g.tc.interface_fields {
		for field in fields {
			if !seen.insert(field.typ) {
				continue
			}
			g.collect_declaration_signature_type(field.typ)
		}
	}
	for _, typ in g.tc.c_globals {
		if !seen.insert(typ) {
			continue
		}
		g.collect_declaration_signature_type(typ)
	}
	for name, typ in g.tc.const_types {
		if g.cache_decl_demand && !g.cache_decl_refs[g.const_ident_c_name(name)] {
			continue
		}
		if !seen.insert(typ) {
			continue
		}
		g.collect_declaration_signature_type(typ)
	}
	for idx, is_set in g.tc.expr_type_set {
		if !is_set || idx >= g.tc.expr_type_values.len {
			continue
		}
		typ := g.tc.expr_type_values[idx]
		if !seen.insert(typ) {
			continue
		}
		g.collect_declaration_signature_type(typ)
	}
	g.decl_types_ready = true
	g.multi_return_types_ready = true
}

fn (mut g FlatGen) collect_selected_declaration_signature_types() {
	if g.selected_decl_types_ready {
		return
	}
	old_module := g.tc.cur_module
	old_file := g.tc.cur_file
	defer {
		g.tc.cur_module = old_module
		g.tc.cur_file = old_file
	}
	for item in g.ensure_fn_gen_items() {
		node := g.a.nodes[int(item.node_id)]
		g.tc.cur_module = item.module
		g.tc.cur_file = item.file
		return_type := g.fn_node_return_type(node, item.module)
		g.collect_known_declaration_signature_type(return_type)
		typed_params := g.fn_node_param_types(node, item.module)
		mut param_idx := 0
		for i in 0 .. node.children_count {
			param := g.a.child_node(&node, i)
			if param.kind != .param {
				break
			}
			raw_type := if param_idx < typed_params.len {
				typed_params[param_idx]
			} else {
				g.tc.parse_resolution_type(param.typ)
			}
			param_idx++
			g.collect_known_declaration_signature_type(g.fn_node_effective_param_type(param, raw_type))
		}
	}
	g.selected_decl_types_ready = true
}

// FnDeclSignatureTypes locates the signature types of one top-level fn
// declaration in FnDeclSignatureTable.types, resolved in `module`/`file`.
struct FnDeclSignatureTypes {
	module string
	file   string
	start  int
	count  int
}

// FnDeclSignatureTable holds what collect_selected_declaration_signature_types
// reads from each selected function, computed before the selection is known.
struct FnDeclSignatureTable {
mut:
	by_node   map[int]FnDeclSignatureTypes
	sig_types []types.Type
}

// precompute_fn_decl_signature_types resolves the return and parameter types of
// every top-level fn declaration, in the module and file that its selection item
// would carry (see collect_fn_gen_candidates_range). The optional-support lane
// runs it while the master is still selecting the functions.
fn (mut g FlatGen) precompute_fn_decl_signature_types() FnDeclSignatureTable {
	mut table := FnDeclSignatureTable{}
	old_module := g.tc.cur_module
	old_file := g.tc.cur_file
	defer {
		g.tc.cur_module = old_module
		g.tc.cur_file = old_file
	}
	nodes := g.top_level_nodes()
	mut cur_module := ''
	mut cur_file := ''
	for pos in 0 .. nodes.len {
		i := nodes[pos]
		node := g.a.nodes[i]
		kind_id := node_kind_id(node)
		if kind_id == 77 {
			cur_file = node.value
			cur_module = ''
			continue
		}
		if kind_id == 73 {
			cur_module = node.value
			continue
		}
		if kind_id != 61 {
			continue
		}
		is_specialization := g.a.specialized_fn_nodes[i]
		item_module := g.a.specialized_fn_modules[i] or { cur_module }
		item_file := g.a.specialized_fn_files[i] or {
			if is_specialization {
				g.tc.fn_type_files[node.value] or { cur_file }
			} else {
				cur_file
			}
		}
		start := table.sig_types.len
		g.tc.cur_module = item_module
		g.tc.cur_file = item_file
		g.append_fn_decl_signature_types(mut table.sig_types, node, item_module)
		table.by_node[int(i)] = FnDeclSignatureTypes{
			module: item_module
			file:   item_file
			start:  start
			count:  table.sig_types.len - start
		}
	}
	return table
}

// append_fn_decl_signature_types appends the declaration signature types that
// collect_selected_declaration_signature_types registers for `node`.
fn (mut g FlatGen) append_fn_decl_signature_types(mut out []types.Type, node flat.Node, module_name string) {
	out << g.fn_node_return_type(node, module_name)
	typed_params := g.fn_node_param_types(node, module_name)
	mut param_idx := 0
	for i in 0 .. node.children_count {
		param := g.a.child_node(&node, i)
		if param.kind != .param {
			break
		}
		raw_type := if param_idx < typed_params.len {
			typed_params[param_idx]
		} else {
			g.tc.parse_resolution_type(param.typ)
		}
		param_idx++
		out << g.fn_node_effective_param_type(param, raw_type)
	}
}

// collect_selected_declaration_signature_types_from is
// collect_selected_declaration_signature_types over precomputed signatures. An
// item whose context differs from the precomputed one is resolved again.
fn (mut g FlatGen) collect_selected_declaration_signature_types_from(table &FnDeclSignatureTable) {
	if g.selected_decl_types_ready {
		return
	}
	old_module := g.tc.cur_module
	old_file := g.tc.cur_file
	defer {
		g.tc.cur_module = old_module
		g.tc.cur_file = old_file
	}
	mut resolved := []types.Type{}
	for item in g.ensure_fn_gen_items() {
		g.tc.cur_module = item.module
		g.tc.cur_file = item.file
		if sig := table.by_node[int(item.node_id)] {
			if sig.module == item.module && sig.file == item.file {
				for k in sig.start .. sig.start + sig.count {
					g.collect_known_declaration_signature_type(table.sig_types[k])
				}
				continue
			}
		}
		resolved.clear()
		g.append_fn_decl_signature_types(mut resolved, g.a.nodes[int(item.node_id)],
			item.module)
		for typ in resolved {
			g.collect_known_declaration_signature_type(typ)
		}
	}
	g.selected_decl_types_ready = true
}

fn (mut g FlatGen) collect_declaration_signature_type(t types.Type) {
	// Erased-template signatures keep their placeholder spellings in the
	// checker tables even when the program itself uses no generics
	// (skip_generics); force the placeholder check so an unused template's
	// `!&Tls[T]` return cannot leave an __v_option_..._T typedef referencing a
	// C type that is never emitted.
	g.placeholder_check_forced = true
	skip := g.type_contains_generic_placeholder(t)
	g.placeholder_check_forced = false
	if skip {
		return
	}
	g.collect_known_declaration_signature_type(t)
}

fn (mut g FlatGen) collect_known_declaration_signature_type(t types.Type) {
	g.collect_concrete_optional_typedef_type(t)
	g.collect_known_concrete_multi_return_type(t)
}

fn (mut g FlatGen) collect_optional_typedef_type(t types.Type) {
	if g.type_contains_generic_placeholder(t) {
		return
	}
	g.collect_concrete_optional_typedef_type(t)
}

fn (mut g FlatGen) collect_concrete_optional_typedef_type(t types.Type) {
	match t {
		types.OptionType {
			g.optional_type_name(t)
			g.collect_concrete_optional_typedef_type(t.base_type)
		}
		types.ResultType {
			g.optional_type_name(t)
			g.collect_concrete_optional_typedef_type(t.base_type)
		}
		types.Array {
			g.collect_concrete_optional_typedef_type(t.elem_type)
		}
		types.ArrayFixed {
			g.collect_concrete_optional_typedef_type(t.elem_type)
		}
		types.Channel {
			g.collect_concrete_optional_typedef_type(t.elem_type)
		}
		types.Map {
			g.collect_concrete_optional_typedef_type(t.key_type)
			g.collect_concrete_optional_typedef_type(t.value_type)
		}
		types.Pointer {
			g.collect_concrete_optional_typedef_type(t.base_type)
		}
		types.FnType {
			for parameter in t.params {
				param := parameter.typ
				g.collect_concrete_optional_typedef_type(param)
			}
			g.collect_concrete_optional_typedef_type(t.return_type)
		}
		types.Alias {
			g.collect_concrete_optional_typedef_type(t.base_type)
		}
		types.MultiReturn {
			for typ in t.types {
				g.collect_concrete_optional_typedef_type(typ)
			}
		}
		else {}
	}
}

fn (g &FlatGen) type_contains_generic_placeholder(t &types.Type) bool {
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
			for parameter in t.params {
				param := parameter.typ
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
			// Inferred anonymous literals are materialized during transform. Their
			// earlier checker entry has no nominal storage type; prefer the concrete
			// lowered annotation instead of emitting `v_struct` for its container.
			shape := flat.source_type_shape(t.name_id)
			if shape.has(.anonymous_struct) {
				return true
			}
			// A stale generic-call annotation can carry the concrete C function name
			// as a nominal type. It is not a payload type and must not create an
			// `__v_option_<function>` typedef in the program prefix.
			if shape.has(.generic_c_name) && !g.type_name_known(t.name_id) {
				return true
			}
			if !g.skip_generics && shape.has(.bracketed)
				&& g.type_name_is_unbound_generic_decl(t.source_type_id(), g.struct_generic_params_for_name(t.source_type_id()), t.name() in g.tc.structs || flat.source_type_name(g.tc.qualify_type_name(t.source_type_id())) in g.tc.structs) {
				return true
			}
			return g.type_name_contains_generic_placeholder(t.source_type_id())
		}
		types.Interface {
			return g.type_name_contains_generic_placeholder(t.source_type_id())
		}
		types.Enum {
			return g.type_name_contains_generic_placeholder(t.source_type_id())
		}
		types.SumType {
			if !g.skip_generics && flat.source_type_shape(t.name_id).has(.bracketed)
				&& g.type_name_is_unbound_generic_decl(t.source_type_id(), g.sum_generic_params_for_name(t.source_type_id()), t.name() in g.tc.sum_types.ids || flat.source_type_name(g.tc.qualify_type_name(t.source_type_id())) in g.tc.sum_types.ids) {
				return true
			}
			return g.type_name_contains_generic_placeholder(t.source_type_id())
		}
		types.Alias {
			return g.type_name_contains_generic_placeholder(t.source_type_id())
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

// struct_generic_params_for_name returns declaration parameter names for a struct source spelling ID.
fn (g &FlatGen) struct_generic_params_for_name(name_id u32) []string {
	base_source_id, _, ok := g.shared_generic_app_parts(name_id)
	base := flat.source_type_name(base_source_id)
	if !ok {
		return []string{}
	}
	return g.tc.struct_generic_params[base] or {
		g.tc.struct_generic_params[flat.source_type_name(flat.source_type_syntax(base_source_id).unqualified)] or { []string{} }
	}
}

// sum_generic_params_for_name returns declaration parameter names for a sum source spelling ID.
fn (g &FlatGen) sum_generic_params_for_name(name_id u32) []string {
	base_source_id, _, ok := g.shared_generic_app_parts(name_id)
	base := flat.source_type_name(base_source_id)
	if !ok {
		return []string{}
	}
	return g.tc.sum_generic_params[base] or {
		g.tc.sum_generic_params[flat.source_type_name(flat.source_type_syntax(base_source_id).unqualified)] or { []string{} }
	}
}

// type_name_is_unbound_generic_decl checks whether a source spelling retains declaration parameters.
fn (g &FlatGen) type_name_is_unbound_generic_decl(name_id u32, params []string, materialized bool) bool {
	if materialized || params.len == 0 {
		return false
	}
	_, args, ok := g.shared_generic_app_parts(name_id)
	if !ok {
		return false
	}
	for arg in args {
		if shared_type_text_uses_generic_params(arg, params) {
			return true
		}
	}
	return false
}

// type_name_contains_generic_placeholder checks the source spelling ID for unresolved generic placeholders.
fn (g &FlatGen) type_name_contains_generic_placeholder(name_id u32) bool {
	if g.skip_generics && !g.placeholder_check_forced { return false }
	for candidate in flat.source_type_syntax(name_id).cgen_placeholder_candidates {
		if candidate !in [flat.source_type_id('C'), flat.source_type_id('JS')]
			&& !types.is_builtin_type_name_id(candidate)
			&& !g.type_name_known(candidate) {
			return true
		}
	}
	return false
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
	interface_matches := g.qualified_interface_c_types(bare_val_type.all_after_last('__'))
	is_known_interface := bare_val_type in interface_matches
	is_known_sum_type := g.is_known_sum_c_type(bare_val_type)
	is_known_builtin := types.is_builtin_type_name(bare_val_type)
	// Multi-return names can contain a module-qualified field component, but the
	// payload is the generated tuple struct rather than a stale source struct.
	if bare_val_type != 'Array' && !is_known_builtin && !is_known_interface && !is_known_sum_type
		&& !bare_val_type.starts_with('multi_return_')
		&& (g.stale_ambiguous_qualified_struct_c_type(bare_val_type)
			|| g.stale_missing_qualified_struct_c_type(bare_val_type)) {
		// Stale generic annotations can lose the declaration module or inherit the
		// generic helper's module. The correctly qualified specialization registers
		// the usable optional; do not emit a phantom payload type here.
		g.emitted_optional_types[opt_name] = true
		return false
	}
	if bare_val_type != 'Array' && !bare_val_type.contains('__')
		&& g.stale_ambiguous_qualified_interface_c_type(bare_val_type) {
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
	// An option payload of an option or a result (`?T` of an option alias `T`)
	// is emitted first: sorted by name, `__v_option___v_option_i64` would come
	// before the `__v_option_i64` it stores.
	if val_type != opt_name {
		if inner_val_type_source_id := g.needed_optional_types.type_id(val_type) {
			inner_val_type := flat.source_type_name(inner_val_type_source_id)

			g.emit_optional_typedef(val_type, inner_val_type)
		}
	}
	g.write_optional_typedef(opt_name, val_type)
	g.emitted_optional_types[opt_name] = true
	return true
}

fn (mut g FlatGen) write_optional_typedef(name string, value_type string) {
	is_result := name == '__v_result' || name.starts_with('__v_result_')
	payload := if is_result && g.has_ierror_interface() {
		'union { IError err; ${value_type} value; };'
	} else {
		'${value_type} value;'
	}
	// A one byte `ok` leaves padding before a payload that holds pointers, and the C
	// compiler may store the payload together with that padding, at an offset that is
	// not a multiple of the pointer size. The conservative GC only looks for pointers at
	// aligned offsets, so it would miss them and free what they point to. A pointer
	// sized `ok` fills the padding without changing the layout.
	ok_type := if (is_result && g.has_ierror_interface())
		|| g.optional_payload_holds_pointers(value_type) {
		'intptr_t'
	} else {
		'bool'
	}
	g.writeln('typedef struct ${name} { ${ok_type} ok; ${payload} } ${name};')
}

// optional_payload_holds_pointers reports whether an option payload of C type
// `value_type` holds a pointer at a pointer aligned offset, and so is aligned at
// least like a pointer.
fn (mut g FlatGen) optional_payload_holds_pointers(value_type string) bool {
	if value_type.ends_with('*') || value_type.starts_with('_fn_ptr_')
		|| value_type.starts_with('__v_result') {
		return true
	}
	if value_type in ['string', 'Array', 'array', 'map', 'chan', 'IError', 'voidptr', 'charptr',
		'byteptr'] {
		return true
	}
	if value_type.starts_with('__v_option_') {
		inner := g.needed_optional_types.type_id(value_type) or { return false }
		return g.optional_payload_holds_pointers(flat.source_type_name(inner))
	}
	if value_type.starts_with('multi_return_') {
		for typ in g.multi_return_types {
			if typ is types.MultiReturn && g.multi_return_c_type_name(typ) == value_type {
				return g.type_holds_aligned_pointer(typ, 0)
			}
		}
		return false
	}
	if info := g.fixed_array_typedefs_needed[value_type] {
		return g.type_holds_aligned_pointer(types.Type(info.arr), 0)
	}
	// Built again whenever the checker has registered more types since (a generic
	// instance registered after a first typedef), so that none of them is missed.
	registered := g.tc.structs.len + g.tc.sum_types.ids.len
	if registered != g.optional_payload_types_from {
		g.optional_payload_types_from = registered
		g.optional_payload_types.clear()
		// Keyed by the C spelling the checker gives the type, the one a payload is
		// written with (`main__Node`, `main__Box_string`, `mod__Node`).
		for name, _ in g.tc.structs {
			typ := types.Type(types.Struct{
				name_id: flat.source_type_id(name)
			})
			g.optional_payload_types[g.tc.c_type(&typ)] = typ
		}
		for name, _ in g.tc.sum_types.ids {
			typ := types.Type(types.SumType{
				name_id: flat.source_type_id(name)
			})
			g.optional_payload_types[g.tc.c_type(&typ)] = typ
		}
	}
	if typ := g.optional_payload_types[value_type] {
		return g.type_holds_aligned_pointer(typ, 0)
	}
	return g.qualified_interface_c_types(value_type.all_after_last('__')).contains(value_type)
}

// type_holds_aligned_pointer reports whether a value of type `t` holds a pointer at a
// pointer aligned offset. A packed or C struct does not count: its alignment can be
// smaller than a pointer's, and a wider `ok` before it would move it.
fn (mut g FlatGen) type_holds_aligned_pointer(t types.Type, depth int) bool {
	if depth > 32 {
		return false
	}
	if _ := g.shared_alias_pointer_type(t) {
		return true
	}
	match t {
		types.String, types.Array, types.Map, types.Channel, types.Pointer, types.FnType,
		types.Interface, types.ResultType {
			return true
		}
		types.ArrayFixed {
			return g.type_holds_aligned_pointer(*t.elem_type, depth + 1)
		}
		types.Alias, types.OptionType {
			return g.type_holds_aligned_pointer(*t.base_type, depth + 1)
		}
		types.MultiReturn {
			for part in t.types {
				if g.type_holds_aligned_pointer(part, depth + 1) {
					return true
				}
			}
			return false
		}
		types.Struct {
			name := flat.source_type_name(t.name_id)
			if flat.source_type_query(t.name_id).has(.c_prefix) || g.struct_decl_pack_for_name(name) != '' {
				return false
			}
			for field in g.tc.struct_fields_for_type(t.name_id) {
				if _ := g.shared_field_info(t.name_id, field.name) {
					return true
				}
				if g.type_holds_aligned_pointer(field.typ, depth + 1) {
					return true
				}
			}
			return false
		}
		types.SumType {
			// A sum stores its variants in a union, which is aligned like its most aligned
			// variant, so it counts when one of them does (not a packed one).
			variants := g.tc.sum_types.type_ids(flat.source_type_name(t.name_id)) or {
				return false
			}
			return g.sum_variants_hold_aligned_pointer(variants, depth + 1)
		}
		else {
			return false
		}
	}
}

fn (g &FlatGen) is_known_sum_c_type(c_type string) bool {
	for name, _ in g.tc.sum_types.ids {
		if g.cname(name) == c_type {
			return true
		}
	}
	return false
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
				enum_name := enum_decl_source_type_id(node, cur_module)
				if name == 'os.AssetMode' && g.is_android_asset_binding(g.tc.cur_file)
					&& g.omits_unused_android_asset_sdk() {
					continue
				}
				cn := g.cname(name)
				if emitted[cn] {
					continue
				}
				emitted[cn] = true
				type_cn := g.enum_typedef_c_name(name)
				is_flag := enum_decl_is_flag(node)
				if backing := enum_decl_backing_type(node) {
					storage_ct := g.enum_emit_storage_c_type(flat.source_type_id(name), backing)
					g.writeln('typedef ${storage_ct} ${type_cn};')
					if is_flag {
						mut field_exprs := map[string]flat.NodeId{}
						for i in 0 .. node.children_count {
							f := g.a.child_node(node_ref, i)
							if f.children_count > 0 {
								field_exprs[f.value] = g.a.child(f, 0)
							}
						}
						mut field_values := map[string]i64{}
						mut val := 0
						for i in 0 .. node.children_count {
							f := g.a.child_node(node_ref, i)
							if f.children_count > 0 {
								mut resolving := map[string]bool{}
								if enum_val := g.enum_field_expr_value_with_enum(g.a.child(f, 0), cur_module, enum_name, mut field_values, field_exprs, mut resolving) {
									val = int(enum_val)
								}
							}
							field_values[f.value] = i64(val)
							cfield := g.cname(f.value)
							g.writeln('static const ${type_cn} ${cn}__${cfield} = (${type_cn})((${storage_ct})1 << ${val});')
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
								if enum_val := g.enum_field_expr_value_with_enum(expr_id, cur_module, enum_name, mut field_values, field_exprs, mut resolving) {
									value = enum_val
									value_known = true
									value_expr = enum_val.str()
								} else {
									// Preserve expressions outside V's 32-bit `int` range so the C
									// storage type can represent wide backed enum values without truncation.
									value_known = false
									value_expr = g.enum_field_expr_to_string_with_enum(expr_id, cur_module, enum_name, cn, field_names) or {
										g.expr_to_string(expr_id)
									}
								}
							}
							if value_known {
								field_values[f.value] = value
							}
							cfield := g.cname(f.value)
							g.writeln('#define ${cn}__${cfield} ((${type_cn})(${value_expr}))')
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
							if enum_val := g.enum_field_expr_value_with_enum(g.a.child(f, 0), cur_module, enum_name, mut field_values, field_exprs, mut resolving) {
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
							if enum_val := g.enum_field_expr_value_with_enum(expr_id, cur_module, enum_name, mut field_values, field_exprs, mut resolving) {
								value = enum_val
								value_known = true
								value_expr = enum_val.str()
							} else {
								value_known = false
								value_expr = g.enum_field_expr_to_string_with_enum(expr_id, cur_module, enum_name, cn, field_names) or {
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
				g.writeln('} ${type_cn};')
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
				fn_cn := g.enum_autostr_c_name(flat.source_type_id(name))
				if !g.enum_autostr_is_used(fn_cn) {
					continue
				}
				if emitted[fn_cn] {
					continue
				}
				emitted[fn_cn] = true
				g.writeln('string ${fn_cn}__autostr(${g.enum_typedef_c_name(name)} it);')
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
				enum_cn := g.cname(name)
				type_cn := g.enum_typedef_c_name(name)
				fn_cn := g.enum_autostr_c_name(flat.source_type_id(name))
				if !g.enum_autostr_is_used(fn_cn) {
					continue
				}
				if emitted[fn_cn] {
					continue
				}
				emitted[fn_cn] = true
				if node.typ_text() == 'flag' {
					// `[flag]` enum: a value can combine several bits, so build the V
					// `Enum{.a | .b}` form by testing each field bit instead of matching a
					// single case (which would send any combination to the integer path).
					g.emit_flag_enum_autostr(node, cur_module, name, enum_cn, fn_cn)
				} else if backing := enum_decl_backing_type(node) {
					storage_ct := g.enum_emit_storage_c_type(flat.source_type_id(name), backing)
					g.writeln('string ${fn_cn}__autostr(${type_cn} it) {')
					for i in 0 .. node.children_count {
						f := g.a.child_node(&node, i)
						raw_fname := f.value
						fname := enum_field_display_name(raw_fname)
						cfield := g.cname(raw_fname)
						g.writeln('\tif (it == ${enum_cn}__${cfield}) return (string){.str = (u8*)"${fname}", .len = ${fname.len}, .is_lit = 1};')
					}
					if enum_storage_c_type_is_unsigned(storage_ct) {
						g.writeln('\treturn strconv__format_uint((u64)(${storage_ct})it, 10);')
					} else {
						g.writeln('\treturn strconv__format_int((i64)(${storage_ct})it, 10);')
					}
					g.writeln('}')
					g.writeln('')
				} else {
					g.writeln('string ${fn_cn}__autostr(${type_cn} it) {')
					for i in 0 .. node.children_count {
						f := g.a.child_node(&node, i)
						raw_fname := f.value
						fname := enum_field_display_name(raw_fname)
						cfield := g.cname(raw_fname)
						// Use ordered comparisons instead of switch cases: enums may opt in
						// to duplicate values, and the first declared name is their auto-str.
						g.writeln('\tif (it == ${enum_cn}__${cfield}) return (string){.str = (u8*)"${fname}", .len = ${fname.len}, .is_lit = 1};')
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

fn (g &FlatGen) enum_autostr_is_used(cname string) bool {
	// Test assertion diagnostics stringify enum operands even when source code does
	// not call `.str()`, so their synthesized helpers are outside markused's call graph.
	return g.test_files.len > 0 || !g.has_used_fn_filter()
		|| g.used_fn_contains('${cname}__autostr')
}

// Metadata uses declaration names independently of autofree's C naming for main enums.
fn enum_decl_source_type_id(node flat.Node, module_name string) u32 {
	name := if module_name !in ['', 'main', 'builtin'] {
		'${module_name}.${node.value}'
	} else {
		node.value
	}
	return flat.source_type_id(name)
}

fn (g &FlatGen) enum_decl_type_name(node flat.Node, module_name string) string {
	if node.value.contains('.') {
		return g.enum_type_codegen_name(node.value)
	}
	candidate := if module_name.len > 0 && module_name !in ['main', 'builtin'] {
		'${module_name}.${node.value}'
	} else {
		node.value
	}
	if candidate in g.tc.enum_names {
		return g.enum_type_codegen_name(candidate)
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
	name := if resolved.len > 0 { resolved } else { candidate }
	return g.enum_type_codegen_name(name)
}

// enum_type_codegen_name preserves V1's main-module namespace in ownership/autofree
// output. Bare builtin enum names stay unqualified.
fn (g &FlatGen) enum_type_codegen_name(name string) string {
	if !g.tc.autofree_mode || name.contains('.') {
		return name
	}
	if module_name := g.enum_modules[name] {
		if module_name in ['', 'main'] {
			return 'main.${name}'
		}
	}
	return name
}

fn (g &FlatGen) enum_type_c_name(name string) string {
	return g.cname(g.enum_type_codegen_name(name))
}

// enum_typedef_c_name returns the C name of the enum's own type. Like structs, a
// main-module enum keeps V1's `main__` namespace there, so it can share its name with a
// type that an included C header declares (`typedef enum { ... } MediaState;`).
// Its values and synthesized helpers keep their `<Enum>__` prefix.
fn (g &FlatGen) enum_typedef_c_name(name string) string {
	if !name.contains('.') {
		if module_name := g.enum_modules[name] {
			if module_name in ['', 'main'] {
				return g.cname('main.${name}')
			}
		}
	}
	return g.cname(name)
}

fn (g &FlatGen) enum_autostr_c_name(type_name_id u32) string {
	name_id := if flat.source_type_query(type_name_id).has(.main_prefix) {
		flat.source_type_syntax(type_name_id).main_base
	} else {
		type_name_id
	}
	name := flat.source_type_name(name_id)
	if name in g.tc.enum_names {
		return g.cname(name)
	}
	if !flat.source_type_shape(name_id).has(.dotted) && g.tc.cur_module.len > 0 {
		qualified := '${g.tc.cur_module}.${name}'
		if qualified in g.tc.enum_names {
			return g.cname(qualified)
		}
	}
	short_name := flat.source_type_name(flat.source_type_syntax(name_id).unqualified)
	if short_name in g.tc.enum_names {
		return g.cname(short_name)
	}
	if !flat.source_type_shape(name_id).has(.dotted) {
		mut match_name := ''
		mut matches := 0
		for enum_name, _ in g.tc.enum_names {
			if flat.source_type_shape(flat.source_type_id(enum_name)).has(.dotted) && flat.source_type_syntax(flat.source_type_id(enum_name)).unqualified == name_id {
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
fn (mut g FlatGen) emit_flag_enum_autostr(node flat.Node, enum_module string, name string, enum_cn string, fn_cn string) {
	short := node.value.all_after_last('.')
	enum_name := enum_decl_source_type_id(node, enum_module)
	mut storage_ct := 'int'
	if backing := enum_decl_backing_type(node) {
		storage_ct = g.enum_emit_storage_c_type(flat.source_type_id(name), backing)
	}
	g.writeln('string ${fn_cn}__autostr(${g.enum_typedef_c_name(name)} it) {')
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
	for i in 0 .. node.children_count {
		f := g.a.child_node(&node, i)
		if f.children_count > 0 {
			mut resolving := map[string]bool{}
			if enum_val := g.enum_field_expr_value_with_enum(g.a.child(f, 0), enum_module, enum_name, mut field_values, field_exprs, mut resolving) {
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
		field_expr := '${enum_cn}__${cfield}'
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

fn (g &FlatGen) enum_field_expr_value_with_enum(id flat.NodeId, enum_module string, enum_name u32, mut field_values map[string]i64, field_exprs map[string]flat.NodeId, mut resolving map[string]bool) ?i64 {
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
			value := g.enum_field_expr_value_with_enum(g.a.child(&node, 0), enum_module, enum_name, mut field_values, field_exprs, mut resolving)?
			return g.enum_comptime_integer_cast(value, node.source_value_id())
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
			mut left := g.enum_field_expr_value_with_enum(g.a.child(&node, 0), enum_module, enum_name, mut field_values, field_exprs, mut resolving)?
			mut right := g.enum_field_expr_value_with_enum(g.a.child(&node, 1), enum_module, enum_name, mut field_values, field_exprs, mut resolving)?
			if node.op in [.div, .mod] && !isnil(g.tc) {
				typ := types.unalias_type(g.usable_expr_type(id)).source_type_id()
				left = g.enum_comptime_integer_cast(left, typ)
				right = g.enum_comptime_integer_cast(right, typ)
			}
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
				.div {
					if isnil(g.tc) {
						left / right
					} else {
						types.comptime_enum_integer_division(left, right, types.unalias_type(g.usable_expr_type(id)).source_type_id(), false)
					}
				}
				.mod {
					if isnil(g.tc) {
						left % right
					} else {
						types.comptime_enum_integer_division(left, right, types.unalias_type(g.usable_expr_type(id)).source_type_id(), true)
					}
				}
				.amp { left & right }
				.pipe { left | right }
				.xor { left ^ right }
				.left_shift { i64(u64(left) << u64(right)) }
				.right_shift {
					typ := if isnil(g.tc) {
						u32(0)
					} else {
						types.unalias_type(g.usable_expr_type(g.a.child(&node, 0))).source_type_id()
					}
					if typ in [flat.source_type_u64, flat.source_type_uint] {
						i64(u64(left) >> right)
					} else {
						left >> right
					}
				}
				.right_shift_unsigned {
					if isnil(g.tc) {
						i64(u64(left) >> u64(right))
					} else {
						types.comptime_enum_unsigned_right_shift(left, right, types.unalias_type(g.usable_expr_type(g.a.child(&node, 0))).source_type_id())
					}
				}
				else { none }
			}
		}
		.selector {
			if field := g.enum_decl_selector_ref_field(id, enum_module, enum_name) {
				return g.enum_decl_field_ref_value(field, enum_module, enum_name, mut field_values, field_exprs, mut resolving)
			}
			prefix := g.enum_decl_selector_base_text(g.a.child(&node, 0))
			if enum_type := g.enum_decl_selector_type(prefix, enum_module) {
				if !isnil(g.tc) {
					return g.tc.comptime_enum_decl_field_value(enum_type, node.value)
				}
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

fn (g &FlatGen) enum_comptime_call_value(id flat.NodeId, enum_module string, enum_name u32, mut field_values map[string]i64, field_exprs map[string]flat.NodeId, mut resolving map[string]bool) ?i64 {
	if !isnil(g.tc) {
		return g.tc.comptime_enum_call_value(id, enum_module, enum_name, mut field_values, field_exprs, mut resolving)
	}
	// Hand-built cgen tests without a checker have no declaration-context metadata.
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
	// This is a straight-line integer evaluator, not an interpreter. Validate the
	// complete body before folding so no skipped statement can change its result.
	mut has_return := false
	for i in 0 .. fn_node.children_count {
		stmt := g.a.child_node(&fn_node, i)
		if has_return {
			return none
		}
		match stmt.kind {
			.param {}
			.decl_assign, .assign {
				if stmt.children_count != 2 || (stmt.kind == .assign && stmt.op != .assign) {
					return none
				}
				lhs := g.a.child_node(stmt, 0)
				if lhs.kind != .ident || lhs.value.len == 0 || lhs.value == '_' {
					return none
				}
			}
			.return_stmt {
				if stmt.children_count != 1 {
					return none
				}
				has_return = true
			}
			else { return none }
		}
	}
	if !has_return {
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
		value := g.enum_field_expr_value_with_enum(arg_id, enum_module, enum_name, mut field_values, field_exprs, mut resolving)?
		locals[param.value] = g.enum_comptime_integer_cast(value, param.typ)
		arg_idx++
	}
	if arg_idx != call.children_count {
		return none
	}
	for i in 0 .. fn_node.children_count {
		stmt := g.a.child_node(&fn_node, i)
		if stmt.kind in [.decl_assign, .assign] {
			if !g.enum_comptime_update_locals(stmt, mut locals, enum_module) {
				return none
			}
			continue
		}
		if stmt.kind == .return_stmt && stmt.children_count > 0 {
			value := g.enum_comptime_expr_value(g.a.child(stmt, 0), locals, enum_module)?
			return g.enum_comptime_integer_cast(value, fn_node.typ)
		}
	}
	return none
}

fn (g &FlatGen) enum_comptime_integer_cast(value i64, typ_id u32) i64 {
	resolved := if isnil(g.tc) || typ_id == 0 {
		typ_id
	} else {
		types.unalias_type(g.tc.parse_type(typ_id)).source_type_id()
	}
	return types.comptime_enum_integer_cast(value, resolved)
}

fn (g &FlatGen) enum_comptime_update_locals(stmt flat.Node, mut locals map[string]i64, enum_module string) bool {
	if stmt.children_count != 2 || (stmt.kind == .assign && stmt.op != .assign) {
		return false
	}
	lhs := g.a.child_node(&stmt, 0)
	if lhs.kind != .ident || lhs.value.len == 0 || lhs.value == '_' {
		return false
	}
	if stmt.kind == .assign && lhs.value !in locals {
		return false
	}
	rhs_id := g.a.child(&stmt, 1)
	value := g.enum_comptime_expr_value(rhs_id, locals, enum_module) or { return false }
	locals[lhs.value] = if isnil(g.tc) {
		value
	} else {
		g.enum_comptime_integer_cast(value, g.usable_expr_type(g.a.child(&stmt, 0)).source_type_id())
	}
	return true
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
			value := g.enum_comptime_expr_value(g.a.child(&node, 0), locals, enum_module)?
			return if node.kind == .cast_expr {
				g.enum_comptime_integer_cast(value, node.source_value_id())
			} else {
				value
			}
		}
		.prefix {
			if node.children_count == 0 {
				return none
			}
			value := g.enum_comptime_expr_value(g.a.child(&node, 0), locals, enum_module)?
			result := match node.op {
				.plus { value }
				.minus { -value }
				.bit_not { ~value }
				else { return none }
			}
			if isnil(g.tc) {
				return result
			}
			// C promotes narrow integer operands for unary arithmetic.
			typ := types.unalias_type(g.usable_expr_type(id)).source_type_id()
			return types.comptime_enum_integer_cast(result, if typ in [
				flat.source_type_i8,
				flat.source_type_u8,
				flat.source_type_id('byte'),
				flat.source_type_i16,
				flat.source_type_u16,
			] {
				flat.source_type_int
			} else {
				typ
			})
		}
		.infix {
			if node.children_count < 2 {
				return none
			}
			mut left := g.enum_comptime_expr_value(g.a.child(&node, 0), locals, enum_module)?
			mut right := g.enum_comptime_expr_value(g.a.child(&node, 1), locals, enum_module)?
			if node.op in [.div, .mod] && !isnil(g.tc) {
				// The runtime division helpers convert operands to the expression type first.
				typ := types.unalias_type(g.usable_expr_type(id)).source_type_id()
				left = g.enum_comptime_integer_cast(left, typ)
				right = g.enum_comptime_integer_cast(right, typ)
			}
			if (node.op in [.div, .mod] && right == 0)
				|| (node.op in [.left_shift, .right_shift, .right_shift_unsigned] && (right < 0
					|| right >= 64)) {
				return none
			}
			result := match node.op {
				.plus { left + right }
				.minus { left - right }
				.mul { left * right }
				.power { enum_foldable_int_power(left, right) }
				.div {
					if isnil(g.tc) {
						left / right
					} else {
						types.comptime_enum_integer_division(left, right, types.unalias_type(g.usable_expr_type(id)).source_type_id(), false)
					}
				}
				.mod {
					if isnil(g.tc) {
						left % right
					} else {
						types.comptime_enum_integer_division(left, right, types.unalias_type(g.usable_expr_type(id)).source_type_id(), true)
					}
				}
				.amp { left & right }
				.pipe { left | right }
				.xor { left ^ right }
				.left_shift { i64(u64(left) << u64(right)) }
				.right_shift {
					typ := if isnil(g.tc) {
						u32(0)
					} else {
						types.unalias_type(g.usable_expr_type(g.a.child(&node, 0))).source_type_id()
					}
					if typ in [flat.source_type_u64, flat.source_type_uint] {
						i64(u64(left) >> right)
					} else {
						left >> right
					}
				}
				.right_shift_unsigned {
					if isnil(g.tc) {
						i64(u64(left) >> u64(right))
					} else {
						types.comptime_enum_unsigned_right_shift(left, right, types.unalias_type(g.usable_expr_type(g.a.child(&node, 0))).source_type_id())
					}
				}
				else { return none }
			}
			if isnil(g.tc) {
				return result
			}
			return types.comptime_enum_integer_cast(result, types.unalias_type(g.usable_expr_type(id)).source_type_id())
		}
		else {
			return none
		}
	}
}

fn enum_foldable_int_literal(value string) ?i64 {
	return types.comptime_enum_integer_literal(value)
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

fn (mut g FlatGen) enum_field_expr_to_string_with_enum(id flat.NodeId, enum_module string, enum_name u32, enum_c_name string, field_names map[string]bool) ?string {
	mut resolving := map[string]bool{}
	return g.enum_field_expr_to_string_resolving(id, enum_module, enum_name, enum_c_name, field_names, false, mut resolving)
}

fn (mut g FlatGen) enum_field_expr_to_string_resolving(id flat.NodeId, enum_module string, enum_name u32, enum_c_name string, field_names map[string]bool, inline_refs bool, mut resolving map[string]bool) ?string {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return none
	}
	node := g.a.nodes[int(id)]
	if node.kind == .call || (node.kind == .infix && node.op == .power) {
		// The checker rejects calls and powers whose values V cannot evaluate.
		// Fold independent ones even when their enclosing expression is symbolic.
		mut values := g.tc.comptime_enum_decl_field_values(enum_name)
		mut numeric_resolving := map[string]bool{}
		value := g.enum_field_expr_value_with_enum(id, enum_module, enum_name, mut values, map[string]flat.NodeId{}, mut numeric_resolving)?
		return value.str()
	}
	match node.kind {
		.ident, .enum_val {
			if node.value in field_names {
				if inline_refs {
					return g.enum_symbolic_member_expr(flat.source_type_name(enum_name), node.value, mut resolving)
				}
				return '${enum_c_name}__${g.cname(node.value)}'
			}
			return g.expr_to_string(id)
		}
		.selector {
			if field := g.enum_decl_selector_ref_field(id, enum_module, enum_name) {
				if field in field_names {
					if inline_refs {
						return g.enum_symbolic_member_expr(flat.source_type_name(enum_name), field, mut resolving)
					}
					return '${enum_c_name}__${g.cname(field)}'
				}
			}
			if node.children_count > 0 {
				prefix := g.enum_decl_selector_base_text(g.a.child(&node, 0))
				if target := g.enum_decl_selector_type(prefix, enum_module) {
					return g.enum_symbolic_member_expr(flat.source_type_name(target), node.value, mut resolving)
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
			inner := g.enum_field_expr_to_string_resolving(g.a.child(&node, 0), enum_module, enum_name, enum_c_name, field_names, inline_refs, mut resolving)?
			return '(${inner})'
		}
		.if_expr {
			if node.children_count != 3 {
				return none
			}
			condition := g.enum_field_expr_to_string_resolving(g.a.child(&node, 0), enum_module, enum_name, enum_c_name, field_names, inline_refs, mut resolving)?
			then_value := g.enum_field_expr_to_string_resolving(g.a.child(&node, 1), enum_module, enum_name, enum_c_name, field_names, inline_refs, mut resolving)?
			else_value := g.enum_field_expr_to_string_resolving(g.a.child(&node, 2), enum_module, enum_name, enum_c_name, field_names, inline_refs, mut resolving)?
			return '((${condition}) ? (${then_value}) : (${else_value}))'
		}
		.block, .expr_stmt {
			if node.children_count != 1 {
				return none
			}
			return g.enum_field_expr_to_string_resolving(g.a.child(&node, 0), enum_module, enum_name, enum_c_name, field_names, inline_refs, mut resolving)
		}
		.cast_expr {
			if node.children_count == 0 {
				return none
			}
			target_type := g.tc.parse_type(g.generic_default_type_id(node.source_value_id()))
			mut ct := g.cast_c_type(target_type)
			if ct.starts_with('fn_ptr:') {
				ct = g.resolve_fn_ptr_type(ct)
			}
			inner := g.enum_field_expr_to_string_resolving(g.a.child(&node, 0), enum_module, enum_name, enum_c_name, field_names, inline_refs, mut resolving)?
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
			inner := g.enum_field_expr_to_string_resolving(g.a.child(&node, 0), enum_module, enum_name, enum_c_name, field_names, inline_refs, mut resolving)?
			ct := g.value_c_type(unalias_type(g.usable_expr_type(id)))
			return '((${ct})(${op}(${inner})))'
		}
		.infix {
			if node.children_count < 2 {
				return none
			}
			op := g.op_str(node.op)
			if op.len == 0 {
				return none
			}
			left := g.enum_field_expr_to_string_resolving(g.a.child(&node, 0), enum_module, enum_name, enum_c_name, field_names, inline_refs, mut resolving)?
			right := g.enum_field_expr_to_string_resolving(g.a.child(&node, 1), enum_module, enum_name, enum_c_name, field_names, inline_refs, mut resolving)?
			if node.op in [.left_shift, .right_shift, .right_shift_unsigned] {
				mut shift_type := *g.usable_expr_type(g.a.child(&node, 0))
				if g.expr_is_in_translated_file(id) && unalias_type(shift_type).source_type_id() == flat.source_type_int {
					shift_type = types.Type(types.i32_)
				}
				ut, bits := g.unsigned_shift_type_parts(shift_type)
				ct := if node.op == .right_shift_unsigned {
					ut
				} else {
					g.value_c_type(unalias_type(shift_type))
				}
				// Enum initializers must remain C integer constant expressions. Unlike
				// runtime shifts, this guard can repeat its side-effect-free operands.
				operand_ct := if node.op == .left_shift { ut } else { ct }
				return '((u64)(${right}) >= ${bits} ? (${ct})0 : (${ct})((${operand_ct})(${left}) ${op} (${right})))'
			}
			ct := g.value_c_type(unalias_type(g.usable_expr_type(id)))
			return '((${ct})((${left}) ${op} (${right})))'
		}
		else {
			return g.expr_to_string(id)
		}
	}
}

// Inline symbolic dependencies so a copied C-derived member does not depend on
// the source enum already having been declared in the generated C.
fn (mut g FlatGen) enum_symbolic_member_expr(enum_name string, field_name string, mut resolving map[string]bool) ?string {
	key := '${enum_name}.${field_name}'
	if resolving[key] {
		return none
	}
	resolving[key] = true
	defer { resolving.delete(key) }
	old_file := g.tc.cur_file
	old_module := g.tc.cur_module
	defer {
		g.tc.cur_file = old_file
		g.tc.cur_module = old_module
	}
	mut cur_module := ''
	for node_idx in g.top_level_nodes() {
		node := g.a.nodes[node_idx]
		if node.kind == .file {
			cur_module = g.tc.file_modules[node.value] or { '' }
			g.tc.cur_file = node.value
			g.tc.cur_module = cur_module
		} else if node.kind == .module_decl {
			cur_module = node.value
			g.tc.cur_module = cur_module
		} else if node.kind == .enum_decl && g.enum_decl_type_name(node, cur_module) == g.enum_type_codegen_name(enum_name) {
			mut field_names := map[string]bool{}
			for i in 0 .. node.children_count {
				field_names[g.a.child_node(&node, i).value] = true
			}
			mut anchor := flat.NodeId(-1)
			mut offset := 0
			for i in 0 .. node.children_count {
				field := g.a.child_node(&node, i)
				if field.children_count > 0 {
					anchor = g.a.child(field, 0)
					offset = 0
				}
				if field.value == field_name {
					if int(anchor) < 0 { return offset.str() }
					value := g.enum_field_expr_to_string_resolving(anchor, cur_module, flat.source_type_id(enum_name), g.cname(enum_name), field_names, true, mut resolving)?
					result := if offset == 0 { '(${value})' } else { '((${value}) + ${offset})' }
					if backing := enum_decl_backing_type(node) {
						storage_ct := g.enum_emit_storage_c_type(flat.source_type_id(enum_name), backing)
						return '((${storage_ct})(${result}))'
					}
					return result
				}
				offset++
			}
			return none
		}
	}
	return none
}

fn (g &FlatGen) enum_decl_field_ref_value(field_name string, enum_module string, enum_name u32, mut field_values map[string]i64, field_exprs map[string]flat.NodeId, mut resolving map[string]bool) ?i64 {
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

fn (g &FlatGen) enum_decl_selector_ref_field(id flat.NodeId, enum_module string, enum_name u32) ?string {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return none
	}
	node := g.a.nodes[int(id)]
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	prefix := g.enum_decl_selector_base_text(g.a.child(&node, 0))
	if !enum_ref_prefix_matches(prefix, enum_module, flat.source_type_name(enum_name)) {
		return none
	}
	return node.value
}

fn (g &FlatGen) enum_decl_selector_type(prefix string, enum_module string) ?u32 {
	// A member in this declaration's module precedes a same-named main enum.
	if !prefix.contains('.') && enum_module !in ['', 'main', 'builtin'] {
		qualified := flat.source_type_id('${enum_module}.${prefix}')
		if resolved := g.enum_selector_base_name(qualified) {
			return resolved
		}
	}
	return g.enum_selector_base_name(flat.source_type_id(prefix))
}

fn (g &FlatGen) enum_decl_selector_base_text(id flat.NodeId) string {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return ''
	}
	node := g.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value_text()
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
	g.type_alias_decls_at_phase(emit_fn_ptr_aliases, false)
}

fn (mut g FlatGen) type_alias_decls_at_phase(emit_fn_ptr_aliases bool, after_arrays bool) {
	mut emitted := false
	mut main_aliases := map[string]bool{}
	old_module := g.tc.cur_module
	defer { g.tc.cur_module = old_module }
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
	for name, target_id in g.tc.type_aliases.ids {
		if flat.source_type_query(target_id).has(.fn_ptr_prefix) || flat.source_type_query(target_id).has(.c_prefix) {
			continue
		}
		if g.has_builtins && !g.tc.autofree_mode {
			continue
		}
		if g.tc.autofree_mode && !main_aliases[name] {
			continue
		}
		alias_module := module_from_qualified_name(name)
		g.tc.cur_module = if alias_module.len > 0 {
			alias_module
		} else if main_aliases[name] {
			'main'
		} else {
			old_module
		}
		parsed_target := g.tc.parse_type(target_id)
		mut ct := if parsed_target is types.FnType {
			g.fn_ptr_type_key(parsed_target)
		} else {
			g.tc.c_type(parsed_target)
		}
		is_fn_ptr_alias := ct.starts_with('fn_ptr:')
		// A pointer still needs its fixed-array typedef, whose elements may be
		// inline sums that become complete only after the sum declaration.
		late_slot_alias := is_fn_ptr_alias && ct.contains('__v3_sum_fixed_pointer_')
		late_alias := late_slot_alias || sum_pointer_needs_erased_storage(parsed_target)
		if late_alias != after_arrays { continue }
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

fn (g &FlatGen) optional_error_field(wrapper string, source string) string {
	if !g.has_ierror_interface() {
		return ''
	}
	if wrapper != '__v_result' && !wrapper.starts_with('__v_result_') {
		return ''
	}
	return ', .err = ${source}.err'
}

fn is_wrapped_c_type(name string) bool {
	return name in ['__v_option', '__v_result'] || name.starts_with('__v_option_')
		|| name.starts_with('__v_result_')
}

fn is_wrapped_c_type_id(name_id u32) bool {
	option_id := flat.source_type_id('__v_option')
	result_id := flat.source_type_id('__v_result')
	if name_id in [option_id, result_id] {
		return true
	}
	prefixes := flat.source_type_syntax(name_id).underscore_prefixes
	return option_id in prefixes || result_id in prefixes
}

// cgen_unalias_type follows canonical alias references without a depth cutoff.
fn cgen_unalias_type(typ &types.Type) &types.Type {
	return unalias_type(typ)
}

// default_init_unalias_type uses canonical storage types for default initialization.
fn default_init_unalias_type(typ &types.Type) &types.Type {
	return unalias_type(typ)
}
