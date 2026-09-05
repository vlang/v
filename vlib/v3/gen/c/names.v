module c

import strings
import v3.gen.c.naming
import v3.types

// c_name converts c name data for c.
fn c_name(name string) string {
	return naming.c_name(name)
}

// CNameCache memoizes naming.c_name results. c_name is a pure function called
// hundreds of thousands of times per build over a few tens of thousands of
// distinct names; the cache lives on the heap so `&FlatGen` query methods can
// populate it, and each parallel worker owns a private instance.
@[heap]
struct CNameCache {
mut:
	entries          map[string]string
	base             &CNameCache = unsafe { nil }
	last_name        string
	last_value       string
	recent_ptrs      [1024]voidptr
	recent_lens      [1024]u16
	recent_values    [1024]string
	recent_populated [1024]bool
}

@[inline]
fn c_name_recent_slot(name string) int {
	return int(((u64(voidptr(name.str)) >> 4) ^ u64(name.len)) & 1023)
}

// c_name_is_pre_sanitized reports names that are already unambiguously in V's
// module-qualified C spelling. C keywords, libc collisions and special builtin
// rewrites never contain `__`, so these names can bypass the map-backed slow
// path after their direct-cache miss.
@[direct_array_access; inline]
fn c_name_is_pre_sanitized(name string) bool {
	mut previous_underscore := false
	mut has_double_underscore := false
	for i in 0 .. name.len {
		c := name[i]
		if (c < `a` || c > `z`) && (c < `A` || c > `Z`) && (c < `0` || c > `9`) && c != `_` {
			return false
		}
		if c == `_` {
			if previous_underscore {
				has_double_underscore = true
			}
			previous_underscore = true
		} else {
			previous_underscore = false
		}
	}
	return has_double_underscore
}

@[direct_array_access; inline]
fn c_name_is_plain_dotted(name string) bool {
	mut has_dot := false
	for i in 0 .. name.len {
		c := name[i]
		if c == `.` {
			has_dot = true
			continue
		}
		if (c < `a` || c > `z`) && (c < `A` || c > `Z`) && (c < `0` || c > `9`) && c != `_` {
			return false
		}
	}
	return has_dot
}

@[direct_array_access; inline]
fn c_name_is_string_literal_symbol(name string) bool {
	if name.len <= 5 || !name.starts_with('_str_') {
		return false
	}
	for i in 5 .. name.len {
		if name[i] < `0` || name[i] > `9` {
			return false
		}
	}
	return true
}

@[inline]
fn (c &CNameCache) recent(name string) ?string {
	if name.len > 65535 {
		return none
	}
	slot := c_name_recent_slot(name)
	if c.recent_populated[slot] && c.recent_ptrs[slot] == voidptr(name.str)
		&& c.recent_lens[slot] == u16(name.len) {
		return c.recent_values[slot]
	}
	return none
}

@[inline]
fn (mut c CNameCache) remember(name string, value string) {
	if name.len > 65535 {
		return
	}
	slot := c_name_recent_slot(name)
	c.recent_ptrs[slot] = voidptr(name.str)
	c.recent_lens[slot] = u16(name.len)
	c.recent_values[slot] = value
	c.recent_populated[slot] = true
}

// ConstShortIndex maps a const short name to its unique primary const name
// ('' marks an ambiguous short name). Full generation freezes it before
// parallel workers start; focused helpers may still build it lazily.
@[heap]
struct ConstShortIndex {
mut:
	built   bool
	entries map[string]string
}

// FnNameFactCache memoizes per-fn-name conclusions (1 = yes, -1 = no) that
// are pure functions of a name plus the fixed signature tables — e.g. whether
// a call target's first parameter is a mut receiver.
@[heap]
struct FnNameFactCache {
mut:
	entries    map[string]i8
	last_name  string
	last_value i8
}

@[heap]
struct ContextNameFactCache {
mut:
	file       string = '\x00'
	module     string
	entries    map[string]i8
	last_name  string
	last_value i8
}

@[inline]
fn (mut c FnNameFactCache) get(name string) i8 {
	if c.last_value != 0 && c.last_name.len == name.len
		&& (unsafe { c.last_name.str == name.str } || c.last_name == name) {
		return c.last_value
	}
	if cached := c.entries[name] {
		c.last_name = name
		c.last_value = cached
		return cached
	}
	return 0
}

@[inline]
fn (mut c FnNameFactCache) put(name string, value i8) {
	c.entries[name] = value
	c.last_name = name
	c.last_value = value
}

@[inline]
fn (mut c ContextNameFactCache) get(name string) i8 {
	if c.last_value != 0 && c.last_name.len == name.len
		&& (unsafe { c.last_name.str == name.str } || c.last_name == name) {
		return c.last_value
	}
	if cached := c.entries[name] {
		c.last_name = name
		c.last_value = cached
		return cached
	}
	return 0
}

@[inline]
fn (mut c ContextNameFactCache) put(name string, value i8) {
	c.entries[name] = value
	c.last_name = name
	c.last_value = value
}

@[inline]
fn (mut c ContextNameFactCache) select_context(file string, module_name string) {
	if c.file.len == file.len && c.module.len == module_name.len && unsafe {
		c.file.str == file.str && c.module.str == module_name.str
	} {
		return
	}
	c.file = file
	c.module = module_name
	c.entries = map[string]i8{}
	c.last_name = ''
	c.last_value = 0
}

// StringLookupCache memoizes context-qualified string lookups whose empty
// result is meaningful. Keeping the cache behind a pointer lets read-only
// FlatGen query methods populate a worker-local cache.
@[heap]
struct StringLookupCache {
mut:
	entries    map[string]string
	last_name  string
	last_value string
	last_valid bool
}

@[inline]
fn (mut c StringLookupCache) get(name string) ?string {
	if c.last_valid && c.last_name.len == name.len
		&& (unsafe { c.last_name.str == name.str } || c.last_name == name) {
		return c.last_value
	}
	if cached := c.entries[name] {
		c.last_name = name
		c.last_value = cached
		c.last_valid = true
		return cached
	}
	return none
}

@[inline]
fn (mut c StringLookupCache) put(name string, value string) {
	c.entries[name] = value
	c.last_name = name
	c.last_value = value
	c.last_valid = true
}

// ContextStringLookupCache memoizes string lookups whose answers depend on the
// current source file and module. Cgen visits functions in source order, so
// replacing on a context switch avoids allocating a compound key on every hot
// lookup while retaining almost all hits.
@[heap]
struct ContextStringLookupCache {
mut:
	file       string = '\x00'
	module     string
	entries    map[string]string
	last_name  string
	last_value string
	last_valid bool
}

@[inline]
fn (mut c ContextStringLookupCache) select_context(file string, module_name string) {
	if c.file.len == file.len && c.module.len == module_name.len && unsafe {
		c.file.str == file.str && c.module.str == module_name.str
	} {
		return
	}
	c.file = file
	c.module = module_name
	c.entries = map[string]string{}
	c.last_name = ''
	c.last_value = ''
	c.last_valid = false
}

fn (mut g FlatGen) reset_context_lookup_caches() {
	g.import_alias_cache = &ContextStringLookupCache{}
	g.enum_selector_cache = &ContextStringLookupCache{}
	g.enum_method_cache = &ContextStringLookupCache{}
	g.qualified_enum_method_cache = &ContextStringLookupCache{}
}

// ScratchLookupCaches holds a generator's memoizing caches while it emits from a
// disposable arena. Both a cached string and the map node holding it come from
// whichever arena is current when the entry is written, so a cache a scratch
// batch populates must not outlive that batch: the next lookup would compare
// against a freed key. The same swap keeps a task that borrows the master
// generator on a pool thread out of the memo maps body lanes concurrently clone.
// See begin_scratch_lookup_caches.
struct ScratchLookupCaches {
	interface_receiver_cache        &StringLookupCache
	normalize_call_cache            &StringLookupCache
	flattened_generic_name_cache    &StringLookupCache
	generic_struct_context_ct_cache &StringLookupCache
	struct_cname_cache              &StringLookupCache
	unique_struct_ct_cache          &StringLookupCache
	alias_method_cache              &StringLookupCache
	import_alias_cache              &ContextStringLookupCache
	enum_selector_cache             &ContextStringLookupCache
	enum_method_cache               &ContextStringLookupCache
	qualified_enum_method_cache     &ContextStringLookupCache
	mut_recv_facts                  &FnNameFactCache
	local_typedef_shadow_facts      &FnNameFactCache
	local_global_shadow_facts       &ContextNameFactCache
	generic_app_cache               &GenericAppCache
	struct_decl_pref_cache          &StructDeclPrefCache
	sum_variant_actual_cache        &SumVariantActualCache
	array_method_cache              map[string]string
	param_types_cache               map[string][]types.Type
}

// begin_scratch_lookup_caches swaps in batch-local memo caches and returns the
// generator's own caches. The caller must pass the result to
// restore_scratch_lookup_caches once the scratch arena is no longer current;
// everything the batch memoized is then dropped together with that arena.
fn (mut g FlatGen) begin_scratch_lookup_caches() ScratchLookupCaches {
	saved := ScratchLookupCaches{
		interface_receiver_cache: g.interface_receiver_cache
		normalize_call_cache: g.normalize_call_cache
		flattened_generic_name_cache: g.flattened_generic_name_cache
		generic_struct_context_ct_cache: g.generic_struct_context_ct_cache
		struct_cname_cache: g.struct_cname_cache
		unique_struct_ct_cache: g.unique_struct_ct_cache
		alias_method_cache: g.alias_method_cache
		import_alias_cache: g.import_alias_cache
		enum_selector_cache: g.enum_selector_cache
		enum_method_cache: g.enum_method_cache
		qualified_enum_method_cache: g.qualified_enum_method_cache
		mut_recv_facts: g.mut_recv_facts
		local_typedef_shadow_facts: g.local_typedef_shadow_facts
		local_global_shadow_facts: g.local_global_shadow_facts
		generic_app_cache: g.generic_app_cache
		struct_decl_pref_cache: g.struct_decl_pref_cache
		sum_variant_actual_cache: g.sum_variant_actual_cache
		array_method_cache: g.array_method_cache
		param_types_cache: g.param_types_cache
	}
	// A cache the generator has disabled stays disabled, so this only changes
	// where entries are written, never whether they are memoized at all.
	g.interface_receiver_cache = scratch_string_lookup_cache(saved.interface_receiver_cache)
	g.normalize_call_cache = scratch_string_lookup_cache(saved.normalize_call_cache)
	g.flattened_generic_name_cache = scratch_string_lookup_cache(saved.flattened_generic_name_cache)
	g.generic_struct_context_ct_cache = scratch_string_lookup_cache(saved.generic_struct_context_ct_cache)
	g.struct_cname_cache = scratch_string_lookup_cache(saved.struct_cname_cache)
	g.unique_struct_ct_cache = scratch_string_lookup_cache(saved.unique_struct_ct_cache)
	g.alias_method_cache = scratch_string_lookup_cache(saved.alias_method_cache)
	g.import_alias_cache = scratch_context_string_lookup_cache(saved.import_alias_cache)
	g.enum_selector_cache = scratch_context_string_lookup_cache(saved.enum_selector_cache)
	g.enum_method_cache = scratch_context_string_lookup_cache(saved.enum_method_cache)
	g.qualified_enum_method_cache = scratch_context_string_lookup_cache(saved.qualified_enum_method_cache)
	g.mut_recv_facts = scratch_fn_name_fact_cache(saved.mut_recv_facts)
	g.local_typedef_shadow_facts = scratch_fn_name_fact_cache(saved.local_typedef_shadow_facts)
	g.local_global_shadow_facts = scratch_context_name_fact_cache(saved.local_global_shadow_facts)
	g.generic_app_cache = scratch_generic_app_cache(saved.generic_app_cache)
	g.struct_decl_pref_cache = scratch_struct_decl_pref_cache(saved.struct_decl_pref_cache)
	g.sum_variant_actual_cache = scratch_sum_variant_actual_cache(saved.sum_variant_actual_cache)
	g.array_method_cache = map[string]string{}
	g.param_types_cache = map[string][]types.Type{}
	return saved
}

// restore_scratch_lookup_caches puts the generator's own caches back. It must run
// after the scratch arena stops being the current one and before it is freed.
fn (mut g FlatGen) restore_scratch_lookup_caches(saved ScratchLookupCaches) {
	g.interface_receiver_cache = saved.interface_receiver_cache
	g.normalize_call_cache = saved.normalize_call_cache
	g.flattened_generic_name_cache = saved.flattened_generic_name_cache
	g.generic_struct_context_ct_cache = saved.generic_struct_context_ct_cache
	g.struct_cname_cache = saved.struct_cname_cache
	g.unique_struct_ct_cache = saved.unique_struct_ct_cache
	g.alias_method_cache = saved.alias_method_cache
	g.import_alias_cache = saved.import_alias_cache
	g.enum_selector_cache = saved.enum_selector_cache
	g.enum_method_cache = saved.enum_method_cache
	g.qualified_enum_method_cache = saved.qualified_enum_method_cache
	g.mut_recv_facts = saved.mut_recv_facts
	g.local_typedef_shadow_facts = saved.local_typedef_shadow_facts
	g.local_global_shadow_facts = saved.local_global_shadow_facts
	g.generic_app_cache = saved.generic_app_cache
	g.struct_decl_pref_cache = saved.struct_decl_pref_cache
	g.sum_variant_actual_cache = saved.sum_variant_actual_cache
	g.array_method_cache = saved.array_method_cache
	g.param_types_cache = saved.param_types_cache
}

fn scratch_string_lookup_cache(cache &StringLookupCache) &StringLookupCache {
	if isnil(cache) {
		return unsafe { nil }
	}
	return &StringLookupCache{}
}

fn scratch_context_string_lookup_cache(cache &ContextStringLookupCache) &ContextStringLookupCache {
	if isnil(cache) {
		return unsafe { nil }
	}
	return &ContextStringLookupCache{}
}

fn scratch_fn_name_fact_cache(cache &FnNameFactCache) &FnNameFactCache {
	if isnil(cache) {
		return unsafe { nil }
	}
	return &FnNameFactCache{}
}

fn scratch_context_name_fact_cache(cache &ContextNameFactCache) &ContextNameFactCache {
	if isnil(cache) {
		return unsafe { nil }
	}
	return &ContextNameFactCache{}
}

fn scratch_struct_decl_pref_cache(cache &StructDeclPrefCache) &StructDeclPrefCache {
	if isnil(cache) {
		return unsafe { nil }
	}
	return &StructDeclPrefCache{}
}

fn scratch_sum_variant_actual_cache(cache &SumVariantActualCache) &SumVariantActualCache {
	if isnil(cache) {
		return unsafe { nil }
	}
	return &SumVariantActualCache{}
}

// scratch_generic_app_cache keeps reading the enclosing arena's entries through
// `base`, and keeps that overlay chain one level deep, exactly like the
// per-worker caches built by new_parallel_worker_config.
fn scratch_generic_app_cache(cache &GenericAppCache) &GenericAppCache {
	if isnil(cache) {
		return unsafe { nil }
	}
	if !isnil(cache.base) {
		return &GenericAppCache{
			base: cache.base
		}
	}
	return &GenericAppCache{
		base: cache
	}
}

// cname is the memoizing wrapper for naming.c_name used on FlatGen hot paths.
@[inline]
fn (g &FlatGen) cname(name string) string {
	if isnil(g.c_name_cache) {
		return naming.c_name(name)
	}
	mut cache := g.c_name_cache
	if cache.last_name.len == name.len
		&& (unsafe { cache.last_name.str == name.str } || cache.last_name == name) {
		return cache.last_value
	}
	if cached := cache.recent(name) {
		cache.last_name = name
		cache.last_value = cached
		return cached
	}
	if c_name_is_pre_sanitized(name) {
		cache.last_name = name
		cache.last_value = name
		cache.remember(name, name)
		return name
	}
	if naming.is_plain_identifier(name) && name != 'malloc' && name != 'int_str' && name != 'exit'
		&& !c_name_is_string_literal_symbol(name) && !naming.is_reserved_word(name)
		&& !naming.is_libc_collision(name) {
		cache.last_name = name
		cache.last_value = name
		cache.remember(name, name)
		return name
	}
	if cached := cache.entries[name] {
		cache.last_name = name
		cache.last_value = cached
		cache.remember(name, cached)
		return cached
	}
	if !isnil(cache.base) {
		if cached := cache.base.recent(name) {
			cache.last_name = name
			cache.last_value = cached
			cache.remember(name, cached)
			return cached
		}
		if cached := cache.base.entries[name] {
			cache.last_name = name
			cache.last_value = cached
			cache.remember(name, cached)
			return cached
		}
	}
	if !name.starts_with('C.') && c_name_is_plain_dotted(name) {
		result := naming.sanitize(name)
		cache.entries[name] = result
		cache.last_name = name
		cache.last_value = result
		cache.remember(name, result)
		return result
	}
	result := naming.c_name(name)
	cache.entries[name] = result
	cache.last_name = name
	cache.last_value = result
	cache.remember(name, result)
	return result
}

fn c_local_name(name string) string {
	local_name := if name.contains('.') { name.all_after_last('.') } else { name }
	return c_name(local_name)
}

// trimmed_space is an allocation-free fast path for trim_space: type texts on
// the cgen hot paths are almost always already clean, and builtin trim clones
// even when there is nothing to trim.
@[inline]
fn trimmed_space(s string) string {
	if s.len == 0 {
		return s
	}
	c0 := s[0]
	cl := s[s.len - 1]
	if c0 != ` ` && c0 != `\n` && c0 != `\t` && c0 != `\v` && c0 != `\f` && c0 != `\r` && cl != ` `
		&& cl != `\n` && cl != `\t` && cl != `\v` && cl != `\f` && cl != `\r` {
		return s
	}
	return s.trim_space()
}

// c_escape supports c escape handling for c.
fn c_escape(s string) string {
	mut out := strings.new_builder(s.len * 4)
	for b in s.bytes() {
		match b {
			`\\` {
				out.write_string('\\\\')
			}
			`"` {
				out.write_string('\\"')
			}
			`\n` {
				out.write_string('\\n')
			}
			`\t` {
				out.write_string('\\t')
			}
			`\r` {
				out.write_string('\\r')
			}
			else {
				if b < 32 || b == 127 {
					v := int(b)
					out.write_u8(`\\`)
					out.write_u8(u8(`0` + ((v >> 6) & 7)))
					out.write_u8(u8(`0` + ((v >> 3) & 7)))
					out.write_u8(u8(`0` + (v & 7)))
				} else {
					out.write_u8(b)
				}
			}
		}
	}
	return out.str()
}

fn c_byte_string_escape(s string) string {
	mut out := strings.new_builder(s.len * 4)
	for b in s.bytes() {
		v := int(b)
		out.write_u8(`\\`)
		out.write_u8(u8(`0` + ((v >> 6) & 7)))
		out.write_u8(u8(`0` + ((v >> 3) & 7)))
		out.write_u8(u8(`0` + (v & 7)))
	}
	return out.str()
}
