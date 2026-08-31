module c

import strings
import v3.gen.c.naming

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
