module lsh

import encoding.utf8

pub enum ShingleMode {
	word
	char
}

pub struct ShingleConfig {
pub:
	mode  ShingleMode = .word
	k     int         = 5
	lower bool        = true
}

fn is_word_rune(r rune) bool {
	return utf8.is_letter(r) || utf8.is_number(r) || r == `_`
}

// shingles extracts unique, lowercased word or character k-gram tokens from a
// document. Both modes are Unicode-aware and preserve first-seen order.
pub fn shingles(doc string, conf ShingleConfig) []string {
	text := if conf.lower { utf8.to_lower(doc) } else { doc }
	mut seen := map[string]bool{}
	mut out := []string{}
	if conf.mode == .char {
		k := if conf.k > 0 { conf.k } else { 5 }
		mut offsets := []int{cap: text.len + 1}
		offsets << 0
		mut pos := 0
		for pos < text.len {
			_, width := utf8.decode_rune_at(text, pos)
			pos += width
			offsets << pos
		}
		if offsets.len - 1 < k {
			return out
		}
		limit := offsets.len - k
		for i in 0 .. limit {
			s := text[offsets[i]..offsets[i + k]]
			if !seen[s] {
				seen[s] = true
				out << s
			}
		}
		return out
	}
	mut start := -1
	mut pos := 0
	for pos < text.len {
		r, width := utf8.decode_rune_at(text, pos)
		if is_word_rune(r) {
			if start < 0 {
				start = pos
			}
		} else if start >= 0 {
			s := text[start..pos]
			if !seen[s] {
				seen[s] = true
				out << s
			}
			start = -1
		}
		pos += width
	}
	if start >= 0 {
		s := text[start..]
		if !seen[s] {
			out << s
		}
	}
	return out
}

// word_shingles returns the unique lowercase Unicode words of a document.
pub fn word_shingles(doc string) []string {
	return shingles(doc, ShingleConfig{})
}

// char_shingles returns the unique lowercase Unicode character k-grams.
pub fn char_shingles(doc string, k int) []string {
	return shingles(doc, ShingleConfig{ mode: .char, k: k })
}
