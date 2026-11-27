module lsh

// universal_prime is the Mersenne prime 2^61 - 1. The hash family used for
// permutations is h(x) = (a*x + b) mod p with a in [1, p) and b in [0, p).
const universal_prime = u64(2305843009213693951)

@[ignore_overflow]
fn max_u64() u64 {
	return u64(0) - 1
}

fn permute(x u64, a u64, b u64) u64 {
	product := u128(a) * u128(x)
	return u64((product + u128(b)) % u128(universal_prime))
}

pub struct MinHashConfig {
pub:
	num_perm int = 128
	seed     u64 = 42
}

// MinHasher builds a MinHash signature from a stream of arbitrary items.
// Duplicate items do not affect the result, exactly as for a set.
pub struct MinHasher[T] {
	num_perm int
	a        []u64
	b        []u64
	hash_fn  fn (T) u64 @[required]
mut:
	signature []u64
	count     int
}

// new_minhasher creates a signature builder with a seeded universal hash
// family. The supplied hash_fn must map equal items to equal u64 values.
pub fn new_minhasher[T](conf MinHashConfig, hash_fn fn (T) u64) MinHasher[T] {
	n := if conf.num_perm > 0 { conf.num_perm } else { 128 }
	mut rng := new_prng(conf.seed)
	mut a := []u64{len: n}
	mut b := []u64{len: n}
	for i in 0 .. n {
		a[i] = rng.below(universal_prime - 1) + 1
		b[i] = rng.below(universal_prime)
	}
	return MinHasher[T]{
		num_perm:  n
		a:         a
		b:         b
		hash_fn:   hash_fn
		signature: []u64{len: n, init: max_u64()}
	}
}

// add folds one item into the signature.
@[direct_array_access]
pub fn (mut m MinHasher[T]) add(item T) {
	if isnil(m.hash_fn) {
		panic('lsh.MinHasher.add: hash_fn is nil; construct with new_minhasher()')
	}
	h := m.hash_fn(item)
	for i in 0 .. m.num_perm {
		v := permute(h, m.a[i], m.b[i])
		if v < m.signature[i] {
			m.signature[i] = v
		}
	}
	m.count++
}

// add_all folds every item of a collection into the signature.
pub fn (mut m MinHasher[T]) add_all(items []T) {
	for item in items {
		m.add(item)
	}
}

// signature returns a copy of the current signature.
pub fn (m &MinHasher[T]) signature() []u64 {
	return m.signature.clone()
}

// len returns the number of items added, including duplicates.
pub fn (m &MinHasher[T]) len() int {
	return m.count
}

// signature_from_items is a one-shot helper for a collection of items.
pub fn signature_from_items[T](items []T, conf MinHashConfig, hash_fn fn (T) u64) []u64 {
	mut m := new_minhasher[T](conf, hash_fn)
	m.add_all(items)
	return m.signature()
}
