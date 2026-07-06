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
	entries map[string]string
}

// cname is the memoizing wrapper for naming.c_name used on FlatGen hot paths.
@[inline]
fn (g &FlatGen) cname(name string) string {
	if isnil(g.c_name_cache) {
		return naming.c_name(name)
	}
	mut cache := g.c_name_cache
	if cached := cache.entries[name] {
		return cached
	}
	result := naming.c_name(name)
	cache.entries[name] = result
	return result
}

fn c_local_name(name string) string {
	local_name := if name.contains('.') { name.all_after_last('.') } else { name }
	return c_name(local_name)
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
