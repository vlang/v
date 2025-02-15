module document

import os

pub const should_sort = os.getenv_opt('VDOC_SORT') or { 'true' }.bool()

pub fn (nodes []DocNode) find(symname string) !DocNode {
	for node in nodes {
		if node.name == symname {
			return node
		}
	}
	return error('symbol not found')
}

// arrange sorts the DocNodes based on their symbols and names.
pub fn (mut nodes []DocNode) arrange() {
	if !should_sort {
		return
	}
	mut kinds := []SymbolKind{}
	for v in nodes {
		if v.kind !in kinds {
			kinds << v.kind
		}
	}
	kinds.sort_with_compare(compare_sym_kinds)
	mut res := []DocNode{}
	for k in kinds {
		mut kind_nodes := nodes.filter(it.kind == k)
		kind_nodes.sort(a.name < b.name)
		res << kind_nodes
	}
	nodes = res.clone()
}

fn compare_sym_kinds(a &SymbolKind, b &SymbolKind) int {
	ak := int(*a)
	bk := int(*b)
	return match true {
		ak < bk { -1 }
		ak > bk { 1 }
		else { 0 }
	}
}

// arr() converts the map into an array of `DocNode`.
pub fn (cnts map[string]DocNode) arr() []DocNode {
	mut contents := cnts.values()
	contents.arrange()
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
