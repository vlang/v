// Copyright 2026 The V Language. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module markdown

import strings

// NodeKind identifies what kind of AST node a Node represents.
pub enum NodeKind {
	document
	heading
	paragraph
	blockquote
	list
	list_item
	code_block
	fenced_code
	thematic_break
	html_block
	link_ref_def
	table
	table_head
	table_body
	table_row
	table_cell
	definition_list
	definition_term
	definition_desc
	footnote_def
	text
	emphasis
	strong
	code_span
	link
	image
	autolink
	raw_html
	hard_break
	soft_break
	strikethrough
	footnote_ref
	task_checkbox
}

// Alignment is the text alignment of a table cell column.
pub enum Alignment {
	none_
	left
	center
	right
}

// Node is a node in the parsed markdown AST.
// A document is a tree of Nodes with .document as the root.
@[heap]
pub struct Node {
pub mut:
	kind       NodeKind
	level      int
	is_tight   bool
	is_ordered bool
	list_start int = 1
	fence_info string
	literal    string
	dest       string
	title      string
	label      string
	checked    bool
	align      Alignment
	id         string
	fn_label   string
	fn_index   int
	children   []&Node
}

// new_node allocates and returns a new Node of the given kind.
pub fn new_node(kind NodeKind) &Node {
	return &Node{
		kind: kind
	}
}

// append_child appends child as the last child of n.
pub fn (mut n Node) append_child(child &Node) {
	n.children << child
}

// text_content returns the plain-text content of this node and all descendants,
// concatenated in document order.
pub fn (n &Node) text_content() string {
	match n.kind {
		.text, .code_span, .raw_html {
			return n.literal
		}
		else {
			mut sb := strings.new_builder(64)
			for child in n.children {
				sb.write_string(child.text_content())
			}
			return sb.str()
		}
	}
}

// walk traverses n and all its descendants in pre-order (root before children).
// The callback f receives each node; return false from f to stop traversal early.
// walk itself returns false if traversal was stopped, true otherwise.
pub fn (n &Node) walk(f fn (&Node) bool) bool {
	if !f(n) {
		return false
	}
	for child in n.children {
		if !child.walk(f) {
			return false
		}
	}
	return true
}
