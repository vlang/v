module token

pub struct TrieNode {
mut:
	children       [123]&TrieNode
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

pub fn new_keywords_matcher_trie<T>(kw_map map[string]T) &TrieNode {
	mut root := new_trie_node('')
	for k, v in kw_map {
		root.add_word(k, v, 0)
	}
	return root
}
