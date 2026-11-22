# lsh

Locality-sensitive hashing and approximate similarity search for V.

This module is experimental: its API may change. It builds only on `hash`
and the V standard library.

## What is here

- `MinHasher[T]`: streaming MinHash signatures over any item type, using a
  seeded universal hash family.
- `LSHIndex`: banded buckets for candidate retrieval without all-pairs
  comparison.
- `DocumentIndex`: word or character k-gram shingling, signatures, exact and
  estimated Jaccard, and ranked similar-document queries.
- `simhash`: 64-bit Rademacher-hyperplane fingerprints for cosine similarity.
- `stable_hash` / `EuclideanIndex`: p-stable LSH for L1 and L2 distance.
- Compact signatures: b-bit MinHash and one-permutation hashing.
- Metric helpers: Jaccard, weighted Jaccard, cosine and Hamming.

## Quick start

```v
import x.lsh

fn main() {
	docs := [
		'the quick brown fox jumps over the lazy dog',
		'the quick brown fox jumped over the lazy dog',
		'a different sentence about cats',
	]
	idx := lsh.new_document_index(docs, lsh.DocumentConfig{
		num_perm: 128
		bands:    16
		seed:     42
	})!
	for m in idx.similar(0.3) {
		println('${m.i} ~ ${m.j}: ${m.score}')
	}
}
```

## Generic sets

```v
import hash
import x.lsh

fn main() {
	hash_fn := fn (s string) u64 { return hash.sum64_string(s, 0) }
	conf := lsh.MinHashConfig{}
	sig := lsh.signature_from_items[string](['a', 'b', 'c'], conf, hash_fn)
	other := lsh.signature_from_items[string](['a', 'b', 'd'], conf, hash_fn)
	println(lsh.jaccard_estimate(sig, other))
}
```

## Tuning

A pair becomes a candidate when at least one of its `bands` identical blocks
matches. The probability is `1 - (1 - s^rows)^bands`, where `rows` is
`num_perm / bands`. Use `optimal_bands(n, threshold)` to pick a layout whose
S-curve is steepest near your threshold. More permutations reduce variance in
the Jaccard estimate at the cost of memory and time.

## Notes

MinHash estimates are probabilistic. Empty signatures score 1.0 against other
empty signatures and 0.0 against non-empty ones. Hash values are deterministic
for a given seed and compiler version; do not persist them across V releases.

SimHash uses seeded +/-1 (Rademacher) hyperplanes. Its similarity is a coarse
estimate of angular similarity: with 512 votes compressed into 64 bits, close
vectors often share the same fingerprint. Use `cosine_similarity` when a
calibrated value is needed.
