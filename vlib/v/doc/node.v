module doc

pub fn (nodes []DocNode) find(symname string) ?DocNode {
	for node in nodes {
		if node.name != symname {
			continue
		}
		return node
	}
	return error('symbol not found')
}

// sort_by_name sorts the array based on the symbol names.
pub fn (mut nodes []DocNode) sort_by_name() {
	nodes.sort_with_compare(compare_nodes_by_name)
}

// sort_by_kind sorts the array based on the symbol kind.
pub fn (mut nodes []DocNode) sort_by_kind() {
	nodes.sort_with_compare(compare_nodes_by_kind)
}

fn compare_nodes_by_kind(a &DocNode, b &DocNode) int {
	ak := int((*a).kind)
	bk := int((*b).kind)
	if ak < bk {
		return -1
	}
	if ak > bk {
		return 1
	}
	return 0
}

fn compare_nodes_by_name(a &DocNode, b &DocNode) int {
	al := a.name.to_lower()
	bl := b.name.to_lower()
	return compare_strings(al, bl)
}

// arr() converts the map into an array of `DocNode`.
pub fn (cnts map[string]DocNode) arr() []DocNode {
	mut contents := cnts.keys().map(cnts[it])
	contents.sort_by_name()
	contents.sort_by_kind()
	return contents
}

// merge_comments returns a `string` with the combined contents of `DocNode.comments`.
pub fn (dc DocNode) merge_comments() string {
	return merge_doc_comments(dc.comments)
}

// merge_comments_without_examples returns a `string` with the
// combined contents of `DocNode.comments` - excluding any examples.
pub fn (dc DocNode) merge_comments_without_examples() string {
	mut sans_examples := []DocComment{cap: dc.comments.len}
	for i := 0; i < dc.comments.len; i++ {
		if dc.comments[i].is_example() {
			continue
		}
		if dc.comments[i].is_multi_line_example() {
			i++
			if i == dc.comments.len || !dc.comments[i].has_triple_backtick() {
				eprintln('${dc.file_path}:${dc.pos.line_nr}: warning: expected code block after empty example line:')
				eprintln('// ```')
				if i < dc.comments.len {
					eprintln('Found:')
					eprintln('//' + dc.comments[i].text[1..])
				}
			}
			i++
			for i < dc.comments.len && !dc.comments[i].has_triple_backtick() {
				i++
			}
		} else {
			sans_examples << dc.comments[i]
		}
	}
	return merge_doc_comments(sans_examples)
}

// examples returns a `[]string` containing examples parsed from `DocNode.comments`.
pub fn (dn DocNode) examples() []string {
	mut output := []string{}
	for i := 0; i < dn.comments.len; i++ {
		comment := dn.comments[i]
		if comment.is_example() {
			output << comment.example()
		} else if comment.is_multi_line_example() {
			i++
			if i + 2 < dn.comments.len && dn.comments[i].has_triple_backtick() {
				i++
				mut ml_ex := ''
				for i < dn.comments.len && !dn.comments[i].has_triple_backtick() {
					if ml_ex.len > 0 {
						ml_ex += '\n'
					}
					s := dn.comments[i].text
					if s.len > 2 {
						ml_ex += s[2..]
					}
					i++
				}
				output << ml_ex
			}
		}
	}
	return output
}
