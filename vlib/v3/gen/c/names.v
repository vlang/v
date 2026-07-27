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
	entries    map[string]string
	base       &CNameCache = unsafe { nil }
	last_name  string
	last_value string
}

// ConstShortIndex maps a const short name to its unique primary const name
// ('' marks an ambiguous short name); built lazily on first query.
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
	entries map[string]i8
}

// StringLookupCache memoizes context-qualified string lookups whose empty
// result is meaningful. Keeping the cache behind a pointer lets read-only
// FlatGen query methods populate a worker-local cache.
@[heap]
struct StringLookupCache {
mut:
	entries map[string]string
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
	if c.file == file && c.module == module_name {
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
	if cached := cache.entries[name] {
		cache.last_name = name
		cache.last_value = cached
		return cached
	}
	if !isnil(cache.base) {
		if cached := cache.base.entries[name] {
			cache.last_name = name
			cache.last_value = cached
			return cached
		}
	}
	result := naming.c_name(name)
	cache.entries[name] = result
	cache.last_name = name
	cache.last_value = result
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
