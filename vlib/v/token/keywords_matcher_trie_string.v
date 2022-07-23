module token

const keys_per_node = 256

pub struct KeywordMatcherTrie {
mut:
	nodes   []&TrieNodeString
	keys    []u8
	min_len int = 999999
	max_len int
}

pub struct TrieNodeString {
mut:
	keys_start     int
	is_end_of_word bool
	kind           int
	prefix         string
}

fn (mut km KeywordMatcherTrie) new_node(prefix string) (&TrieNodeString, int) {
	mut node := &TrieNodeString{
		prefix: prefix
	}
	km.nodes << node
	keys_start := km.keys.len
	for _ in 0 .. token.keys_per_node {
		km.keys << 0
	}
	node.keys_start = keys_start
	return node, km.nodes.len - 1
}

pub fn (km &KeywordMatcherTrie) show(level int, node &TrieNodeString) {
	eprintln('> level: ${level:2} | prefix: ${node.prefix:20} | kind: ${node.kind:12} | minl: ${km.min_len:2} | maxl: ${km.max_len:2} | is_end_of_word: ${int(node.is_end_of_word)}')
	for kidx in 0 .. token.keys_per_node {
		k := km.keys[node.keys_start + kidx]
		if k != 0 {
			km.show(level + 1, km.nodes[k])
		}
	}
}

pub fn (mut km KeywordMatcherTrie) add_word(word string, kind int) {
	if km.max_len < word.len {
		km.max_len = word.len
	}
	if km.min_len > word.len {
		km.min_len = word.len
	}
	mut word_idx := 0
	mut node := unsafe { km.nodes[0] }
	for {
		c := word[word_idx] or { break }
		first := u8(c)
		// eprintln('>> node: ${ptr_str(node)} | first: $first | word_idx: $word_idx')
		k := node.keys_start + first
		mut idx := int(km.keys[k])
		if idx == 0 {
			_, idx = km.new_node(word#[..word_idx + 1])
			km.keys[k] = u8(idx)
		}
		node = km.nodes[idx]
		word_idx++
	}
	node.is_end_of_word = true
	node.kind = kind
}

[direct_array_access]
pub fn (km &KeywordMatcherTrie) find(word string) int {
	if word.len > km.max_len || word.len < km.min_len {
		return -1
	}
	mut node := unsafe { km.nodes[0] }
	keys := &km.keys[0]
	nodes := &km.nodes[0]
	for c in word {
		// eprintln('> match_keyword: `${word:20}` | node: ${ptr_str(node)} | node.prefix: ${node.prefix:15} | idx: ${idx:3}')
		k := unsafe { keys[node.keys_start + c] }
		if k == 0 {
			return -1
		}
		node = unsafe { nodes[k] }
	}
	if node.is_end_of_word {
		// km.show(0, node)
		return node.kind
	}
	return -1
}

pub fn new_keywords_matcher_trie_string<T>(kw_map map[string]T) &KeywordMatcherTrie {
	mut km := &KeywordMatcherTrie{
		nodes: []&TrieNodeString{cap: 100}
		keys: []u8{cap: 100 * token.keys_per_node}
	}
	km.new_node('')
	for k, v in kw_map {
		km.add_word(k, int(v))
	}
	// eprintln('> new_keywords_matcher_trie_string kw_map: $kw_map | km.root: ${ptr_str(km.nodes[0])}')
	return km
}
