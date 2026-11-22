module lsh

import hash

// Match is one candidate id together with its estimated similarity.
pub struct Match {
pub:
	id    string
	score f64
}

// LSHIndex stores signatures in banded buckets so queries only compare
// documents that collided in at least one band.
pub struct LSHIndex {
pub:
	bands         int
	rows_per_band int
mut:
	buckets    map[u64][]int
	ids        []string
	signatures [][]u64
}

// new_lsh_index creates an index with the given number of bands.
pub fn new_lsh_index(num_perm int, bands int) !LSHIndex {
	if bands < 1 {
		return error('bands must be at least 1')
	}
	if num_perm < 1 {
		return error('num_perm must be at least 1')
	}
	if num_perm % bands != 0 {
		return error('num_perm must be divisible by bands')
	}
	return LSHIndex{
		bands:         bands
		rows_per_band: num_perm / bands
		buckets:       map[u64][]int{}
	}
}

// band_key folds a band's values into a 64-bit bucket key. It allocates
// nothing, unlike building and joining a string per band per insert/query.
@[direct_array_access]
fn band_key(band int, rows int, sig []u64) u64 {
	start := band * rows
	mut key := hash.wyhash64_c(u64(band), u64(rows))
	for k in 0 .. rows {
		key = hash.wyhash64_c(key, sig[start + k])
	}
	return key
}

// insert adds a signature under the given id.
pub fn (mut idx LSHIndex) insert(id string, sig []u64) ! {
	if sig.len != idx.bands * idx.rows_per_band {
		return error('signature length does not match index configuration')
	}
	id_index := idx.ids.len
	idx.ids << id
	idx.signatures << sig.clone()
	for band in 0 .. idx.bands {
		idx.buckets[band_key(band, idx.rows_per_band, sig)] << id_index
	}
}

// query returns stored ids that share a band with sig and meet the threshold.
pub fn (idx &LSHIndex) query(sig []u64, threshold f64) ![]Match {
	if sig.len != idx.bands * idx.rows_per_band {
		return error('signature length does not match index configuration')
	}
	mut seen := map[int]bool{}
	mut candidates := []int{}
	for band in 0 .. idx.bands {
		for id_index in idx.buckets[band_key(band, idx.rows_per_band, sig)] {
			if !seen[id_index] {
				seen[id_index] = true
				candidates << id_index
			}
		}
	}
	mut out := []Match{cap: candidates.len}
	for id_index in candidates {
		score := jaccard_estimate(sig, idx.signatures[id_index])
		if score >= threshold {
			out << Match{idx.ids[id_index], score}
		}
	}
	out.sort(a.score > b.score)
	return out
}

// len returns the number of inserted signatures.
pub fn (idx &LSHIndex) len() int {
	return idx.ids.len
}

// banding_probability is the chance that two sets with the given similarity
// become candidates: 1 - (1 - s^rows)^bands.
pub fn banding_probability(similarity f64, bands int, rows int) f64 {
	if bands < 1 || rows < 1 {
		return 0.0
	}
	mut s := similarity
	if s < 0.0 {
		s = 0.0
	}
	if s > 1.0 {
		s = 1.0
	}
	all_match := pow_small(s, rows)
	return 1.0 - pow_small(1.0 - all_match, bands)
}

// optimal_bands picks the band layout whose S-curve is steepest near the
// requested similarity threshold.
pub fn optimal_bands(num_perm int, threshold f64) !(int, int) {
	if num_perm < 1 {
		return error('num_perm must be at least 1')
	}
	mut best_bands := 1
	mut best_rows := num_perm
	mut best_error := 2.0
	for bands := 1; bands <= num_perm; bands++ {
		if num_perm % bands != 0 {
			continue
		}
		rows := num_perm / bands
		p := banding_probability(threshold, bands, rows)
		err := if p > 0.5 { p - 0.5 } else { 0.5 - p }
		if err < best_error {
			best_error = err
			best_bands = bands
			best_rows = rows
		}
	}
	return best_bands, best_rows
}
