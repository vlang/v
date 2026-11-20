module math

// cosine_similarity computes cosine similarity between two vectors.
pub fn cosine_similarity(a []f64, b []f64) f64 {
	if a.len != b.len || a.len == 0 {
		return 0
	}
	mut dot := 0.0
	mut na := 0.0
	mut nb := 0.0
	for i in 0 .. a.len {
		dot += a[i] * b[i]
		na += a[i] * a[i]
		nb += b[i] * b[i]
	}
	if na == 0 || nb == 0 {
		return 0
	}
	return dot / (math.sqrt(na) * math.sqrt(nb))
}
