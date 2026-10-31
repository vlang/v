module util

const key_recent_slots = 128
// key_recent_key_cap is the inline storage for one slot's key parts; keys
// whose parts are longer together are not cached.
const key_recent_key_cap = 160

// KeyRecentCache is a lossy front cache for lookups keyed by up to three
// strings. It keeps its own copy of every key, so callers may pass temporary
// strings, and a hit compares the parts directly: the caller skips building the
// composite key its backing map is keyed by. Values are returned as stored.
// Only use it in front of maps that are not cleared while the cache is alive.
@[heap]
pub struct KeyRecentCache {
mut:
	// 128 slots * 160 bytes of key storage.
	keys   [20480]u8
	a_lens [key_recent_slots]int
	b_lens [key_recent_slots]int
	c_lens [key_recent_slots]int
	values [key_recent_slots]string
	states [key_recent_slots]i8 // 1 = found, -1 = known miss, 0 = empty
}

// get returns 1 and the cached value for a hit, -1 for a cached miss, and 0
// when the key is not cached.
@[direct_array_access]
pub fn (c &KeyRecentCache) get(a string, b string, cc string) (i8, string) {
	if a.len + b.len + cc.len > key_recent_key_cap {
		return 0, ''
	}
	slot := key_recent_slot(a, b, cc)
	state := c.states[slot]
	if state == 0 || c.a_lens[slot] != a.len || c.b_lens[slot] != b.len
		|| c.c_lens[slot] != cc.len {
		return 0, ''
	}
	base := slot * key_recent_key_cap
	// The key bytes are stored inline; compare each part in place.
	unsafe {
		stored := &c.keys[base]
		if (a.len > 0 && vmemcmp(stored, a.str, a.len) != 0)
			|| (b.len > 0 && vmemcmp(stored + a.len, b.str, b.len) != 0)
			|| (cc.len > 0 && vmemcmp(stored + a.len + b.len, cc.str, cc.len) != 0) {
			return 0, ''
		}
	}
	return state, c.values[slot]
}

// put records `state` (1 = found, -1 = known miss) and `value` for the key.
@[direct_array_access]
pub fn (mut c KeyRecentCache) put(a string, b string, cc string, state i8, value string) {
	if a.len + b.len + cc.len > key_recent_key_cap {
		return
	}
	slot := key_recent_slot(a, b, cc)
	base := slot * key_recent_key_cap
	// Copy the key parts into this slot's inline storage.
	unsafe {
		mut stored := &c.keys[base]
		if a.len > 0 {
			vmemcpy(stored, a.str, a.len)
		}
		if b.len > 0 {
			vmemcpy(stored + a.len, b.str, b.len)
		}
		if cc.len > 0 {
			vmemcpy(stored + a.len + b.len, cc.str, cc.len)
		}
	}
	c.a_lens[slot] = a.len
	c.b_lens[slot] = b.len
	c.c_lens[slot] = cc.len
	c.values[slot] = value
	c.states[slot] = state
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
