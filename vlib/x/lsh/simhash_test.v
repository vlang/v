module lsh

fn test_simhash_identical_vectors() {
	v := [1.0, 2.0, 3.0, 4.0]
	a := simhash(v, 42)
	b := simhash(v, 42)
	assert a == b
	assert simhash_similarity(a, b) == 1.0
	c := simhash([4.0, 3.0, 2.0, 1.0], 42)
	assert simhash_similarity(a, c) < 1.0
}

fn test_simhash_tracks_cosine() {
	base := [1.0, 2.0, 3.0, 4.0]
	near := [1.1, 2.0, 3.0, 3.9]
	far := [-4.0, -3.0, -2.0, -1.0]
	a := simhash(base, 7)
	b := simhash(near, 7)
	c := simhash(far, 7)
	assert simhash_similarity(a, b) > simhash_similarity(a, c)
}

fn test_simhash_bits() {
	a := u64(0b1011)
	b := u64(0b1000)
	assert hamming64(a, b) == 2
	assert simhash_similarity_bits(a, b, 4) == 0.5
}

fn test_metric_helpers() {
	assert hamming64(0, 0) == 0
	assert hamming64(0, 0xff) == 8
	assert cosine_similarity([1.0, 0.0], [1.0, 0.0]) > 0.999
	assert weighted_jaccard([1.0, 1.0], [1.0, 0.0]) == 0.5
}

fn test_stable_hash_deterministic() {
	v := [0.1, 0.2, 0.3]
	assert stable_hash(v, .l2, 99, 1.0) == stable_hash(v, .l2, 99, 1.0)
	assert stable_hash(v, .l1, 99, 1.0) == stable_hash(v, .l1, 99, 1.0)
}

fn test_euclidean_index() {
	mut index := new_euclidean_index(4, 1.0, .l2, 99)!
	index.insert('a', [1.0, 2.0, 3.0])
	index.insert('b', [10.0, -10.0, 5.0])
	res := index.query([1.0, 2.0, 3.0])!
	assert 'a' in res
}
