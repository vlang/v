module lsh

import hash

pub struct Pair {
pub:
	i int
	j int
}

pub struct DocMatch {
pub:
	i     int
	j     int
	score f64
}

pub struct DocumentConfig {
pub:
	num_perm int           = 128
	seed     u64           = 42
	bands    int           = 16
	shingle  ShingleConfig = ShingleConfig{}
}

// DocumentIndex holds MinHash signatures and a banded index for a corpus of
// documents. Query it instead of comparing every document pair.
pub struct DocumentIndex {
pub:
	config DocumentConfig
mut:
	doc_shingles [][]string
	signatures   [][]u64
	index        LSHIndex
}

fn string_hash(s string) u64 {
	return hash.sum64_string(s, 0)
}

// new_document_index shingles, signs and indexes every document.
pub fn new_document_index(docs []string, conf DocumentConfig) !DocumentIndex {
	num_perm := if conf.num_perm > 0 { conf.num_perm } else { 128 }
	bands := if conf.bands > 0 { conf.bands } else { 16 }
	if num_perm % bands != 0 {
		return error('num_perm must be divisible by bands')
	}
	sh_conf := ShingleConfig{
		mode:  conf.shingle.mode
		k:     if conf.shingle.k > 0 { conf.shingle.k } else { 5 }
		lower: conf.shingle.lower
	}
	mut index := new_lsh_index(num_perm, bands)!
	mut doc_shingles := [][]string{cap: docs.len}
	mut signatures := [][]u64{cap: docs.len}
	for doc_i, doc in docs {
		sh := shingles(doc, sh_conf)
		sig := signature_from_items[string](sh, MinHashConfig{
			num_perm: num_perm
			seed:     conf.seed
		}, string_hash)
		doc_shingles << sh
		signatures << sig
		index.insert(doc_i.str(), sig)!
	}
	return DocumentIndex{
		config:       conf
		doc_shingles: doc_shingles
		signatures:   signatures
		index:        index
	}
}

pub fn (idx &DocumentIndex) len() int {
	return idx.signatures.len
}

pub fn (idx &DocumentIndex) signature(i int) ![]u64 {
	if i < 0 || i >= idx.signatures.len {
		return error('document index out of range')
	}
	return idx.signatures[i].clone()
}

pub fn (idx &DocumentIndex) shingle_set(i int) ![]string {
	if i < 0 || i >= idx.doc_shingles.len {
		return error('document index out of range')
	}
	return idx.doc_shingles[i].clone()
}

// estimate returns the MinHash Jaccard estimate for two documents.
pub fn (idx &DocumentIndex) estimate(i int, j int) !f64 {
	if i < 0 || j < 0 || i >= idx.signatures.len || j >= idx.signatures.len {
		return error('document index out of range')
	}
	return jaccard_estimate(idx.signatures[i], idx.signatures[j])
}

// exact returns the true Jaccard index of two documents' shingle sets.
pub fn (idx &DocumentIndex) exact(i int, j int) !f64 {
	if i < 0 || j < 0 || i >= idx.doc_shingles.len || j >= idx.doc_shingles.len {
		return error('document index out of range')
	}
	return jaccard_strings(idx.doc_shingles[i], idx.doc_shingles[j])
}

// candidates returns all pairs that collide in at least one band.
pub fn (idx &DocumentIndex) candidates() []Pair {
	mut seen := map[string]bool{}
	mut out := []Pair{}
	for i in 0 .. idx.signatures.len {
		matches := idx.index.query(idx.signatures[i], 0.0) or { return out }
		for m in matches {
			j := m.id.int()
			if j <= i {
				continue
			}
			key := '${i}:${j}'
			if !seen[key] {
				seen[key] = true
				out << Pair{i, j}
			}
		}
	}
	return out
}

// similar returns scored pairs at or above the threshold, best first.
pub fn (idx &DocumentIndex) similar(threshold f64) []DocMatch {
	mut seen := map[string]bool{}
	mut out := []DocMatch{}
	for i in 0 .. idx.signatures.len {
		matches := idx.index.query(idx.signatures[i], threshold) or { return out }
		for m in matches {
			j := m.id.int()
			if j <= i {
				continue
			}
			key := '${i}:${j}'
			if !seen[key] {
				seen[key] = true
				out << DocMatch{i, j, m.score}
			}
		}
	}
	out.sort(a.score > b.score)
	return out
}
