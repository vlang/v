module intern

// Pool deduplicates strings: interning the same text twice returns the same
// backing storage, so repeated identifiers, keys and tags share one copy
// and compare by pointer first. It is the V-idiomatic equivalent of Go's
// `unique` package.
//
// Contract: treat interned strings as immutable. Mutating one's bytes would
// corrupt the canonical copy shared with every holder.
//
// Persistent (immutable) collections are deliberately out of scope here; they
// need the GC-interaction design deferred in the tracking issue, plus a
// merged ordered map to build on.
pub struct Pool {
mut:
	canonical map[string]string
}

// new_pool creates an empty interning pool.
pub fn new_pool() Pool {
	return Pool{}
}

// intern returns the canonical copy of s, cloning it on first sight.
// Repeated calls with equal strings return storage-identical values.
pub fn (mut p Pool) intern(s string) string {
	if s !in p.canonical {
		p.canonical[s] = s.clone()
	}
	return p.canonical[s]
}

// len returns the number of distinct interned strings.
pub fn (p &Pool) len() int {
	return p.canonical.len
}

// eq reports whether a and b are equal, trying a pointer comparison first so
// interned values compare in O(1) without hashing. Plain (non-interned)
// strings fall back to value comparison and are always correct.
pub fn eq(a string, b string) bool {
	if a.str == b.str && a.len == b.len {
		return true
	}
	return a == b
}
