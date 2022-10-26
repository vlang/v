module token

// KeywordsMatcherTrie provides a faster way of determinining whether a given name
// is a reserved word (belongs to a given set of previously known words `R`).
// See the module description for more details.
[heap]
pub struct KeywordsMatcherTrie {
pub mut:
	nodes   []&TrieNode
	min_len int = 999999
	max_len int
}

// TrieNode is a single node from a trie, used by KeywordsMatcherTrie
pub struct TrieNode {
pub mut:
	children [123]&TrieNode
	value    int = -1 // when != -1, it is a leaf node representing a match
}

// find tries to find the given `word` in the set of all previously added words
// to the KeywordsMatcherTrie instance. It returns -1 if the word was NOT found
// there at all. If the word was found, find will return the `value` (value => 0),
// associated with the word, when it was added.
[direct_array_access]
pub fn (km &KeywordsMatcherTrie) find(word string) int {
	wlen := word.len
	if wlen < km.min_len {
		return -1
	}
	if wlen > km.max_len {
		return -1
	}
	node := km.nodes[wlen]
	if node == unsafe { nil } {
		return -1
	}
	return node.find(word)
}

[inline]
pub fn (km &KeywordsMatcherTrie) matches(word string) bool {
	return km.find(word) != -1
}

// add_word adds the given word to the KeywordsMatcherTrie instance. It associates a non
// negative integer value to it, so later `find` could return the value, when it succeeds.
[direct_array_access]
pub fn (mut km KeywordsMatcherTrie) add_word(word string, value int) {
	wlen := word.len
	if km.max_len < wlen {
		km.max_len = wlen
	}
	if km.min_len > wlen {
		km.min_len = wlen
	}
	if km.nodes[wlen] == unsafe { nil } {
		km.nodes[wlen] = new_trie_node()
	}
	km.nodes[wlen].add_word(word, value, 0)
}

// new_keywords_matcher_trie creates a new KeywordsMatcherTrie instance from a given map
// with string keys, and integer or enum values.
pub fn new_keywords_matcher_trie<T>(kw_map map[string]T) KeywordsMatcherTrie {
	mut km := KeywordsMatcherTrie{
		nodes: []&TrieNode{cap: 20}
	}
	for _ in 0 .. 20 {
		km.nodes << &TrieNode(0)
	}
	for k, v in kw_map {
		km.add_word(k, v)
	}
	// dump(km.min_len)
	// dump(km.max_len)
	// for idx,x in km.nodes { if x != unsafe { nil } { eprintln('>> idx: $idx | ${ptr_str(x)}') } }
	return km
}

// new_keywords_matcher_from_array_trie creates a new KeywordsMatcherTrie instance from a given array
// of strings. The values for the strings, that `find` will return, will be the indexes in that array.
pub fn new_keywords_matcher_from_array_trie(names []string) KeywordsMatcherTrie {
	mut m := map[string]int{}
	for i, name in names {
		m[name] = i
	}
	return new_keywords_matcher_trie<int>(m)
}

//

// new_trie_node creates a new TrieNode instance
pub fn new_trie_node() &TrieNode {
	return &TrieNode{}
}

// show displays the information in `node`, in a more compact/readable format (recursively)
pub fn (node &TrieNode) show(level int) {
	mut non_nil_children := []int{}
	for idx, x in node.children {
		if x != unsafe { nil } {
			non_nil_children << idx
		}
	}
	children := non_nil_children.map(u8(it).ascii_str())
	eprintln('> level: ${level:2} | value: ${node.value:12} | non_nil_children: ${non_nil_children.len:2} | ${children}')
	for x in node.children {
		if x != unsafe { nil } {
			x.show(level + 1)
		}
	}
}

// add_word adds another `word` and `value` pair into the trie, starting from `node` (recursively).
// `word_idx` is jsut used as an accumulator, and starts from 0 at the root of the tree.
pub fn (mut node TrieNode) add_word(word string, value int, word_idx int) {
	first := u8(word[word_idx] or {
		node.value = value
		return
	})
	// eprintln('>> node: ${ptr_str(node)} | first: $first | word_idx: $word_idx')
	mut child_node := node.children[first]
	if child_node == unsafe { nil } {
		child_node = new_trie_node()
		node.children[first] = child_node
	}
	child_node.add_word(word, value, word_idx + 1)
}

// find tries to find a match for `word` to the trie (the set of all previously added words).
// It returns -1 if there is no match, or the value associated with the previously added
// matching word by `add_word`.
[direct_array_access]
pub fn (root &TrieNode) find(word string) int {
	wlen := word.len
	mut node := unsafe { &TrieNode(root) }
	mut idx := 0
	for {
		// eprintln('> match_keyword: `${word:20}` | node: ${ptr_str(node)} | idx: ${idx:3}')
		if idx == wlen {
			k := node.value
			if k != -1 {
				// node.show(0)
				return k
			}
			return -1
		}
		c := word[idx]
		child := node.children[c]
		if child == unsafe { nil } {
			return -1
		}
		node = child
		idx++
	}
	return -1
}
