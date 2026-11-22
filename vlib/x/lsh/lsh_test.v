module lsh

fn test_shingles_word() {
	s := word_shingles('Hello, hello world!')
	assert s == ['hello', 'world']
}

fn test_shingles_char() {
	s := char_shingles('abcab', 2)
	assert s == ['ab', 'bc', 'ca']
}

fn test_unicode_shingles() {
	assert char_shingles('é', 1) == ['é']
	assert char_shingles('aé', 2) == ['aé']
	assert char_shingles('ab', 5).len == 0
	assert word_shingles('café naïve') == ['café', 'naïve']
	assert word_shingles('ÉCOLE') == ['école']
}

fn test_minhash_determinism() {
	conf := MinHashConfig{ num_perm: 128, seed: 7 }
	hash_fn := fn (s string) u64 { return string_hash(s) }
	a := signature_from_items[string](['a', 'b', 'c'], conf, hash_fn)
	b := signature_from_items[string](['c', 'b', 'a'], conf, hash_fn)
	c := signature_from_items[string](['x', 'y', 'z'], conf, hash_fn)
	assert a == b
	assert jaccard_estimate(a, b) == 1.0
	assert jaccard_estimate(a, c) < 0.05
	mut m := new_minhasher[string](conf, hash_fn)
	m.add_all(['a', 'b', 'c'])
	assert m.signature() == a
	assert m.len() == 3
}

fn test_jaccard_accuracy() {
	conf := MinHashConfig{ num_perm: 512, seed: 11 }
	hash_fn := fn (s string) u64 { return string_hash(s) }
	mut left := []string{}
	for i in 0 .. 100 {
		left << 'item${i}'
	}
	mut right := []string{}
	for i in 0 .. 50 {
		right << 'item${i}'
	}
	for i in 100 .. 150 {
		right << 'new${i}'
	}
	est := jaccard_estimate(signature_from_items[string](left, conf, hash_fn),
		signature_from_items[string](right, conf, hash_fn))
	exact := jaccard_strings(left, right)
	assert exact > 0.33 && exact < 0.34
	assert est > 0.28 && est < 0.39
}

fn test_document_index() {
	docs := [
		'the quick brown fox jumps over the lazy dog',
		'the quick brown fox jumped over the lazy dog',
		'a completely different sentence about cats',
	]
	idx := new_document_index(docs, DocumentConfig{ num_perm: 128, bands: 16, seed: 3 })!
	assert idx.len() == 3
	est := idx.estimate(0, 1)!
	exact := idx.exact(0, 1)!
	assert est > 0.4
	assert exact > est - 0.2
	matches := idx.similar(0.3)
	assert matches.len > 0
	assert matches[0].i == 0 && matches[0].j == 1
}

fn test_banding_probability() {
	assert banding_probability(1.0, 10, 5) > 0.999
	assert banding_probability(0.0, 10, 5) == 0.0
	bands, rows := optimal_bands(100, 0.5)!
	assert bands * rows == 100
}

fn test_lsh_index_query() {
	mut idx := new_lsh_index(16, 4)!
	sig := []u64{len: 16, init: 42}
	idx.insert('first', sig)!
	other := []u64{len: 16, init: 0}
	idx.insert('second', other)!
	matches := idx.query(sig, 0.5)!
	assert matches.len == 1
	assert matches[0].id == 'first'
	assert matches[0].score == 1.0
}

fn test_compact_signatures() {
	sig := [u64(1), u64(2), u64(3)]
	assert bbit_signature(sig, 1) == [u64(1), u64(0), u64(1)]
	assert bbit_jaccard(bbit_signature(sig, 1), bbit_signature(sig, 1), 1) == 1.0
	one := one_permutation_signature[string](['a', 'b', 'c'], MinHashConfig{ seed: 9 },
		fn (s string) u64 { return string_hash(s) }, 2)
	assert one.len == 2
	assert one[0] <= one[1]
}
