module lsh

const table_seed_step = u64(0x9e3779b97f4a7c15)

@[ignore_overflow]
fn table_seed(seed u64, table int) u64 {
	return seed + u64(table) * table_seed_step
}

pub enum StableNorm {
	l1
	l2
}

fn floor_i64(x f64) i64 {
	v := int(x)
	if x < 0.0 && f64(v) != x {
		return i64(v) - 1
	}
	return i64(v)
}

// stable_hash buckets a vector with a p-stable projection. L2 uses Gaussian
// coefficients, L1 uses Cauchy coefficients. Nearby vectors collide often.
pub fn stable_hash(vec []f64, norm StableNorm, seed u64, bucket_width f64) i64 {
	mut rng := new_prng(seed)
	mut dot := 0.0
	for v in vec {
		a := if norm == .l2 { rng.normal() } else { rng.cauchy() }
		dot += a * v
	}
	if bucket_width <= 0.0 {
		return 0
	}
	return floor_i64(dot / bucket_width)
}

// EuclideanIndex is a multi-table p-stable LSH index for approximate nearest
// neighbours under L1 or L2 distance.
pub struct EuclideanIndex {
pub:
	num_tables   int
	bucket_width f64
	norm         StableNorm
	seed         u64
mut:
	tables []map[i64][]string
}

pub fn new_euclidean_index(num_tables int, bucket_width f64, norm StableNorm, seed u64) !EuclideanIndex {
	if num_tables < 1 {
		return error('num_tables must be at least 1')
	}
	if bucket_width <= 0.0 {
		return error('bucket_width must be positive')
	}
	mut tables := []map[i64][]string{len: num_tables}
	for i in 0 .. num_tables {
		tables[i] = map[i64][]string{}
	}
	return EuclideanIndex{
		num_tables:   num_tables
		bucket_width: bucket_width
		norm:         norm
		seed:         seed
		tables:       tables
	}
}

pub fn (mut idx EuclideanIndex) insert(id string, vec []f64) {
	for t in 0 .. idx.num_tables {
		h := stable_hash(vec, idx.norm, table_seed(idx.seed, t), idx.bucket_width)
		idx.tables[t][h] << id
	}
}

pub fn (idx &EuclideanIndex) query(vec []f64) ![]string {
	mut seen := map[string]bool{}
	mut out := []string{}
	for t in 0 .. idx.num_tables {
		h := stable_hash(vec, idx.norm, table_seed(idx.seed, t), idx.bucket_width)
		for id in idx.tables[t][h] {
			if !seen[id] {
				seen[id] = true
				out << id
			}
		}
	}
	return out
}
