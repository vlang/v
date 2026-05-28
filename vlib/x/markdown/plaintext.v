// Copyright 2026 The V Language. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module markdown

import strings

// PlainTextRenderer renders a parsed markdown AST to UTF-8 console-friendly text.
struct PlainTextRenderer {
	opts    Options
	ref_map map[string]LinkRef
mut:
	sb strings.Builder
	// footnote tracking
	fn_order []string
	fn_nodes map[string]&Node
	// list tracking
	list_depth int
	list_nums  []int
}

// render renders the document node to plain text.
pub fn (mut r PlainTextRenderer) render(doc &Node) string {
	r.sb = strings.new_builder(1024)
	if r.opts.footnotes {
		for child in doc.children {
			if child.kind == .footnote_def {
				r.fn_nodes[child.fn_label] = child
			}
		}
	}
	r.render_children(doc)
	if r.opts.footnotes && r.fn_order.len > 0 {
		r.render_footnotes_section()
	}
	return r.sb.str().trim_right('\n') + '\n'
}

fn (mut r PlainTextRenderer) render_node(node &Node) {
	match node.kind {
		.document { r.render_children(node) }
		.heading { r.render_heading(node) }
		.paragraph { r.render_paragraph(node) }
		.blockquote { r.render_blockquote(node) }
		.list { r.render_list(node) }
		.list_item { r.render_list_item(node) }
		.code_block, .fenced_code { r.render_code_block(node) }
		.thematic_break { r.sb.write_string('---\n') }
		.html_block { r.render_html_block(node) }
		.link_ref_def, .footnote_def {}
		.table { r.render_table(node) }
		.table_head, .table_body { r.render_children(node) }
		.table_row { r.render_table_row(node) }
		.table_cell { r.render_table_cell(node) }
		.definition_list { r.render_children(node) }
		.definition_term { r.render_definition_term(node) }
		.definition_desc { r.render_definition_desc(node) }
		.text { r.render_text(node) }
		.emphasis { r.render_wrapped(node, '*') }
		.strong { r.render_wrapped(node, '**') }
		.code_span { r.sb.write_string('`' + node.literal + '`') }
		.link { r.render_link(node) }
		.image { r.render_image(node) }
		.autolink { r.sb.write_string(node.literal) }
		.raw_html { r.render_raw_html(node) }
		.hard_break, .soft_break { r.sb.write_string('\n') }
		.strikethrough { r.render_wrapped(node, '~~') }
		.task_checkbox { r.render_task_checkbox(node) }
		.footnote_ref { r.render_footnote_ref(node) }
	}
}

fn (mut r PlainTextRenderer) render_children(node &Node) {
	for child in node.children {
		r.render_node(child)
	}
}

fn (mut r PlainTextRenderer) render_inline(src string) {
	nodes := parse_inline(src, r.opts, r.ref_map)
	for node in nodes {
		r.render_node(node)
	}
}

fn (mut r PlainTextRenderer) render_heading(node &Node) {
	r.sb.write_string('${'#'.repeat(node.level)} ')
	if node.children.len > 0 {
		r.render_children(node)
	} else {
		r.render_inline(node.literal)
	}
	r.sb.write_string('\n\n')
}

fn (mut r PlainTextRenderer) render_paragraph(node &Node) {
	if node.children.len > 0 {
		r.render_children(node)
	} else {
		r.render_inline(node.literal)
	}
	r.sb.write_string('\n')
}

fn (mut r PlainTextRenderer) render_blockquote(node &Node) {
	mut inner := strings.new_builder(128)
	mut rr := PlainTextRenderer{
		opts:     r.opts
		ref_map:  r.ref_map
		fn_order: r.fn_order.clone()
		fn_nodes: r.fn_nodes
	}
	rr.sb = inner
	rr.render_children(node)
	for line in rr.sb.str().trim_right('\n').split('\n') {
		r.sb.write_string('> ${line}\n')
	}
	// Keep footnote reference order discovered inside the blockquote.
	r.fn_order = rr.fn_order
	r.sb.write_string('\n')
}

fn (mut r PlainTextRenderer) render_list(node &Node) {
	r.list_depth++
	if node.is_ordered {
		r.list_nums << node.list_start
	} else {
		r.list_nums << 0
	}
	r.render_children(node)
	r.list_nums.delete_last()
	r.list_depth--
	if r.list_depth == 0 {
		r.sb.write_string('\n')
	}
}

fn (mut r PlainTextRenderer) render_list_item(node &Node) {
	indent := '  '.repeat(if r.list_depth > 0 { r.list_depth - 1 } else { 0 })
	idx := r.list_nums.len - 1
	marker := if idx >= 0 && r.list_nums[idx] > 0 {
		m := '${r.list_nums[idx]}. '
		r.list_nums[idx]++
		m
	} else {
		'- '
	}
	r.sb.write_string(indent + marker)
	for i, child in node.children {
		if i > 0 {
			r.sb.write_string(' ')
		}
		if child.kind == .paragraph {
			if child.children.len > 0 {
				r.render_children(child)
			} else {
				r.render_inline(child.literal)
			}
		} else if child.kind == .list {
			r.sb.write_string('\n')
			r.render_node(child)
		} else {
			r.render_node(child)
		}
	}
	r.sb.write_string('\n')
}

fn (mut r PlainTextRenderer) render_code_block(node &Node) {
	r.sb.write_string('```\n')
	r.sb.write_string(node.literal.trim_right('\n'))
	r.sb.write_string('\n```\n\n')
}

fn (mut r PlainTextRenderer) render_html_block(node &Node) {
	if r.opts.renderer_opts.unsafe_ {
		r.sb.write_string(node.literal)
	} else {
		r.sb.write_string('[raw HTML omitted]\n')
	}
}

fn (mut r PlainTextRenderer) render_table(node &Node) {
	r.render_children(node)
	r.sb.write_string('\n')
}

fn (mut r PlainTextRenderer) render_table_row(node &Node) {
	r.render_children(node)
	r.sb.write_string('\n')
}

fn (mut r PlainTextRenderer) render_table_cell(node &Node) {
	r.sb.write_string(node.literal.trim_space())
	r.sb.write_string(' | ')
}

fn (mut r PlainTextRenderer) render_definition_term(node &Node) {
	r.render_inline(node.literal)
	r.sb.write_string('\n')
	for child in node.children {
		r.render_node(child)
	}
}

fn (mut r PlainTextRenderer) render_definition_desc(node &Node) {
	r.sb.write_string('  - ')
	r.render_inline(node.literal)
	r.sb.write_string('\n')
}

fn (mut r PlainTextRenderer) render_text(node &Node) {
	content := if r.opts.typographer {
		smart_punctuate(node.literal)
	} else {
		node.literal
	}
	r.sb.write_string(content)
}

fn (mut r PlainTextRenderer) render_wrapped(node &Node, marker string) {
	r.sb.write_string(marker)
	r.render_children(node)
	r.sb.write_string(marker)
}

fn (mut r PlainTextRenderer) render_link(node &Node) {
	r.render_children(node)
	if node.dest.len > 0 {
		r.sb.write_string(' (${node.dest})')
	}
}

fn (mut r PlainTextRenderer) render_image(node &Node) {
	alt := node.text_content().trim_space()
	if alt.len == 0 {
		r.sb.write_string('[image]')
	} else {
		r.sb.write_string('[image: ${alt}]')
	}
	if node.dest.len > 0 {
		r.sb.write_string(' (${node.dest})')
	}
}

fn (mut r PlainTextRenderer) render_raw_html(node &Node) {
	if r.opts.renderer_opts.unsafe_ {
		r.sb.write_string(node.literal)
	}
}

fn (mut r PlainTextRenderer) render_task_checkbox(node &Node) {
	r.sb.write_string(if node.checked { '☑' } else { '☐' })
}

fn (mut r PlainTextRenderer) render_footnote_ref(node &Node) {
	label := node.fn_label
	mut idx := 0
	for i, l in r.fn_order {
		if l == label {
			idx = i + 1
			break
		}
	}
	if idx == 0 {
		r.fn_order << label
		idx = r.fn_order.len
	}
	r.sb.write_string('[${idx}]')
}

fn (mut r PlainTextRenderer) render_footnotes_section() {
	r.sb.write_string('\nFootnotes:\n')
	for label in r.fn_order {
		fn_node := r.fn_nodes[label] or { continue }
		mut idx := 0
		for i, l in r.fn_order {
			if l == label {
				idx = i + 1
				break
			}
		}
		r.sb.write_string('[${idx}] ')
		r.render_inline(fn_node.literal)
		r.sb.write_string('\n')
	}
}
