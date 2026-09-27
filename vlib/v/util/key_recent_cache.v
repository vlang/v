module util

const key_recent_slots = 128

// KeyRecentCache is a lossy front cache for lookups keyed by up to three
// strings. A hit compares the parts directly (pointer and length first), so the
// caller can skip building the composite key its backing map is keyed by. Only
// use it in front of maps that are not cleared while the cache is alive.
@[heap]
pub struct KeyRecentCache {
mut:
	a      [key_recent_slots]string
	b      [key_recent_slots]string
	c      [key_recent_slots]string
	values [key_recent_slots]string
	states [key_recent_slots]i8 // 1 = found, -1 = known miss, 0 = empty
}

// get returns 1 and the cached value for a hit, -1 for a cached miss, and 0
// when the key is not cached.
@[direct_array_access]
pub fn (c &KeyRecentCache) get(a string, b string, cc string) (i8, string) {
	slot := key_recent_slot(a, b, cc)
	state := c.states[slot]
	if state != 0 && key_part_matches(c.a[slot], a) && key_part_matches(c.b[slot], b)
		&& key_part_matches(c.c[slot], cc) {
		return state, c.values[slot]
	}
	return 0, ''
}

// put records `state` (1 = found, -1 = known miss) and `value` for the key.
@[direct_array_access]
pub fn (mut c KeyRecentCache) put(a string, b string, cc string, state i8, value string) {
	slot := key_recent_slot(a, b, cc)
	c.a[slot] = a
	c.b[slot] = b
	c.c[slot] = cc
	c.values[slot] = value
	c.states[slot] = state
}

@[inline]
fn key_part_matches(a string, b string) bool {
	if a.len != b.len {
		return false
	}
	if unsafe { a.str == b.str } {
		return true
	}
	return a == b
}

// key_part_hash samples a key part so that separately allocated copies of one
// spelling share a slot.
@[direct_array_access; inline]
fn key_part_hash(s string) u32 {
	if s.len == 0 {
		return 0
	}
	mut hash := u32(s.len)
	hash = (hash * 16777619) ^ u32(s[0])
	hash = (hash * 16777619) ^ u32(s[s.len / 2])
	hash = (hash * 16777619) ^ u32(s[s.len - 1])
	return hash
}

@[inline]
fn key_recent_slot(a string, b string, c string) int {
	hash := key_part_hash(a) * 31 + key_part_hash(b) * 7 + key_part_hash(c)
	return int((hash ^ (hash >> 15)) & u32(key_recent_slots - 1))
}
