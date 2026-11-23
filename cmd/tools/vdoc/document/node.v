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
	is_readme := dc.comments.len > 0 && dc.comments.all(it.is_readme)
	// The comments of the declaration are chosen before its examples are left out, since the
	// lines of an example would split them.
	comments := if is_readme { dc.comments } else { last_comment_block(dc.comments) }
	mut sans_examples := []DocComment{cap: comments.len}
	for i := 0; i < comments.len; i++ {
		if comments[i].is_example() {
			continue
		}
		if comments[i].is_multi_line_example() {
			if i + 1 == comments.len || !comments[i + 1].has_triple_backtick() {
				eprintln('${dc.file_path}:${dc.pos.line_nr}: warning: expected code block after empty example line, so it is shown as text:')
				eprintln('// ```')
				if i + 1 < comments.len {
					eprintln('Found:')
					eprintln('//' + comments[i + 1].text[1..])
				}
				sans_examples << comments[i]
				continue
			}
			i += 2
			for i < comments.len && !comments[i].has_triple_backtick() {
				i++
			}
		} else {
			sans_examples << comments[i]
		}
	}
	if is_readme {
		return merge_doc_comments(sans_examples)
	}
	return merge_comment_lines(sans_examples)
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
