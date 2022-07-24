module token

pub struct TrieNode {
mut:
	children       [256]&TrieNode
	is_end_of_word bool
	kind           Kind
	prefix         string
	min_len        int = 999999
	max_len        int
}

pub fn new_trie_node(prefix string) &TrieNode {
	return &TrieNode{
		prefix: prefix
	}
}

pub fn (node &TrieNode) show(level int) {
	mut non_nil_children := 0
	for x in node.children {
		if x != unsafe { nil } {
			non_nil_children++
		}
	}
	eprintln('> level: ${level:2} | prefix: ${node.prefix:20} | kind: ${node.kind:12} | minl: ${node.min_len:2} | maxl: ${node.max_len:2} | is_end_of_word: ${int(node.is_end_of_word)} | non_nil_children: ${non_nil_children:2}')
	for x in node.children {
		if x != unsafe { nil } {
			x.show(level + 1)
		}
	}
}

pub fn (mut node TrieNode) add_word(word string, kind Kind, word_idx int) {
	if node.max_len < word.len {
		node.max_len = word.len
	}
	if node.min_len > word.len {
		node.min_len = word.len
	}
	first := u8(word[word_idx] or {
		node.is_end_of_word = true
		node.kind = kind
		return
	})
	// eprintln('>> node: ${ptr_str(node)} | first: $first | word_idx: $word_idx')
	mut child_node := node.children[first]
	if child_node == unsafe { nil } {
		child_node = new_trie_node(word#[..word_idx + 1])
		node.children[first] = child_node
	}
	child_node.add_word(word, kind, word_idx + 1)
}

[direct_array_access]
pub fn (root &TrieNode) find(word string) int {
	if word.len > root.max_len || word.len < root.min_len {
		return -1
	}
	mut node := unsafe { &TrieNode(root) }
	mut idx := 0
	for {
		// eprintln('> match_keyword: `${word:20}` | node: ${ptr_str(node)} | node.prefix: ${node.prefix:15} | idx: ${idx:3}')
		if idx == word.len {
			if node.is_end_of_word {
				// node.show(0)
				return int(node.kind)
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

[heap]
pub struct KeywordsMatcherTrie {
mut:
	nodes   []&TrieNode
	min_len int = 999999
	max_len int
}

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

[direct_array_access]
pub fn (mut km KeywordsMatcherTrie) add_word(word string, kind Kind) {
	wlen := word.len
	if km.max_len < wlen {
		km.max_len = wlen
	}
	if km.min_len > wlen {
		km.min_len = wlen
	}
	if km.nodes[wlen] == unsafe { nil } {
		km.nodes[wlen] = new_trie_node('')
	}
	km.nodes[wlen].add_word(word, kind, 0)
}

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

pub fn new_keywords_matcher_from_array_trie(names []string) KeywordsMatcherTrie {
	mut m := map[string]int{}
	for i, name in names {
		m[name] = i
	}
	return new_keywords_matcher_trie<int>(m)
}
