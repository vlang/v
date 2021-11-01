module token

// bump token.max_keyword_len, if you add a longer keyword
const max_keyword_len = 11

// KeywordsMatcher provides a faster way of determinining whether a given name
// is a reserved keyword, by doing a comparison with only the keywords that
// have exactly the same length as `name`.
// Benchmarking shows that with -prod, it is 20-25% slower in the worst case
// compared to just using token.keywords[name], but can be 20x faster
// in the case, where there is a length mismatch, and 2x-3x faster in most
// cases, where there is a match.
// Without -prod, with tcc, using KeywordsMatcher is always faster
// (2x to 14x times), compared to using a hash of all the keywords.
pub struct KeywordsMatcher {
mut:
	len_min int = 9999
	len_max int = -1
	words   [max_keyword_len][]WKind
}

struct WKind {
mut:
	word string
	kind Kind
}

pub fn new_keywords_matcher(kw_map map[string]Kind) KeywordsMatcher {
	mut km := KeywordsMatcher{}
	// TODO: remove this loop. It is currently needed, because a
	// fixed array of arrays is not initialised properly automatically
	// as of 2021/10/28
	for i in 0 .. token.max_keyword_len {
		km.words[i] = []WKind{}
	}
	for k, v in kw_map {
		km.add_word(k, v)
	}
	for i in 0 .. token.max_keyword_len {
		if km.words[i].len > 0 {
			km.words[i].sort(a.word < b.word)
			$if trace_keyword_matcher_initialisation ? {
				print('word len: ${i:3} | words: ')
				for w in km.words[i] {
					print('$w.word, ')
				}
				println('')
			}
		}
	}
	return km
}

fn (mut km KeywordsMatcher) add_word(word string, kind Kind) {
	if word.len >= token.max_keyword_len {
		panic('increase max_keyword_len to > $word.len')
	}
	if km.len_max < word.len {
		km.len_max = word.len
	}
	if word.len < km.len_min {
		km.len_min = word.len
	}
	km.words[word.len] << WKind{word, kind}
}

// find returns the Kind given a word, by doing a binary search
// on the sorted list of words for each bin
[direct_array_access]
pub fn (km &KeywordsMatcher) find(word string) Kind {
	wlen := word.len
	if wlen < km.len_min || wlen > km.len_max {
		return Kind.unknown
	}
	list_len := km.words[wlen].len
	if list_len == 0 {
		return Kind.unknown
	}
	mut lo := 0
	mut hi := list_len - 1
	for lo <= hi {
		mid := lo + (hi - lo) / 2
		cmp := km.words[wlen][mid].word.compare(word)
		match cmp {
			0 { return km.words[wlen][mid].kind }
			-1 { lo = mid + 1 }
			1 { hi = mid - 1 }
			else {}
		}
	}
	return Kind.unknown
}
