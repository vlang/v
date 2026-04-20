// Copyright 2026 The V Language. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module markdown

import strings

// HTMLRenderer renders a parsed markdown AST to an HTML string.
struct HTMLRenderer {
	opts    Options
	ref_map map[string]LinkRef
mut:
	sb strings.Builder
	// footnote tracking
	fn_order      []string         // ordered list of encountered fn labels
	fn_nodes      map[string]&Node // label → footnote_def node
	tight_list    bool             // whether we're inside a tight list
	in_table_head bool
}

// render renders the document node to an HTML string.
pub fn (mut r HTMLRenderer) render(doc &Node) string {
	r.sb = strings.new_builder(1024)
	// Pre-collect footnote definitions if extension is enabled.
	if r.opts.footnotes {
		for child in doc.children {
			if child.kind == .footnote_def {
				r.fn_nodes[child.fn_label] = child
			}
		}
	}
	r.render_children(doc)
	// Append footnotes section if any refs were used.
	if r.opts.footnotes && r.fn_order.len > 0 {
		r.render_footnotes_section()
	}
	return r.sb.str()
}

// render_node dispatches rendering to the appropriate method.
fn (mut r HTMLRenderer) render_node(node &Node) {
	match node.kind {
		.document { r.render_children(node) }
		.heading { r.render_heading(node) }
		.paragraph { r.render_paragraph(node) }
		.blockquote { r.render_blockquote(node) }
		.list { r.render_list(node) }
		.list_item { r.render_list_item(node) }
		.code_block { r.render_code_block(node) }
		.fenced_code { r.render_fenced_code(node) }
		.thematic_break { r.render_thematic_break() }
		.html_block { r.render_html_block(node) }
		.link_ref_def {} // already collected, nothing to render
		.table { r.render_table(node) }
		.table_head { r.render_table_section(node, 'thead') }
		.table_body { r.render_table_section(node, 'tbody') }
		.table_row { r.render_table_row(node) }
		.table_cell { r.render_table_cell(node) }
		.definition_list { r.render_definition_list(node) }
		.definition_term { r.render_definition_term(node) }
		.definition_desc { r.render_definition_desc(node) }
		.footnote_def {} // rendered in the footnote section
		// Inline nodes.
		.text { r.render_text(node) }
		.emphasis { r.render_emphasis(node) }
		.strong { r.render_strong(node) }
		.code_span { r.render_code_span(node) }
		.link { r.render_link(node) }
		.image { r.render_image(node) }
		.autolink { r.render_autolink(node) }
		.raw_html { r.render_raw_html(node) }
		.hard_break { r.render_hard_break() }
		.soft_break { r.render_soft_break() }
		.strikethrough { r.render_strikethrough(node) }
		.task_checkbox { r.render_task_checkbox(node) }
		.footnote_ref { r.render_footnote_ref(node) }
	}
}

// render_children renders all children of node.
fn (mut r HTMLRenderer) render_children(node &Node) {
	for child in node.children {
		r.render_node(child)
	}
}

// render_inline parses and renders inline content from a literal string.
fn (mut r HTMLRenderer) render_inline(src string) {
	nodes := parse_inline(src, r.opts, r.ref_map)
	for node in nodes {
		r.render_node(node)
	}
}

// ---- Block elements ----

fn (mut r HTMLRenderer) render_heading(node &Node) {
	tag := 'h${node.level}'
	if node.id.len > 0 {
		r.sb.write_string('<${tag} id="${html_escape(node.id)}">')
	} else {
		r.sb.write_string('<${tag}>')
	}
	if node.children.len > 0 {
		r.render_children(node)
	} else {
		r.render_inline(node.literal)
	}
	r.sb.write_string('</${tag}>\n')
}

fn (mut r HTMLRenderer) render_paragraph(node &Node) {
	if r.tight_list {
		// In a tight list, paragraph content is rendered directly without <p> tags.
		if node.children.len > 0 {
			r.render_children(node)
		} else {
			r.render_inline(node.literal)
		}
		return
	}
	r.sb.write_string('<p>')
	if node.children.len > 0 {
		r.render_children(node)
	} else {
		r.render_inline(node.literal)
	}
	r.sb.write_string('</p>\n')
}

fn (mut r HTMLRenderer) render_blockquote(node &Node) {
	r.sb.write_string('<blockquote>\n')
	r.render_children(node)
	r.sb.write_string('</blockquote>\n')
}

fn (mut r HTMLRenderer) render_list(node &Node) {
	tag := if node.is_ordered { 'ol' } else { 'ul' }
	if node.is_ordered && node.list_start != 1 {
		r.sb.write_string('<${tag} start="${node.list_start}">\n')
	} else {
		r.sb.write_string('<${tag}>\n')
	}
	prev_tight := r.tight_list
	r.tight_list = node.is_tight
	r.render_children(node)
	r.tight_list = prev_tight
	r.sb.write_string('</${tag}>\n')
}

fn (mut r HTMLRenderer) render_list_item(node &Node) {
	// Check if this is a task list item (first child is task_checkbox).
	if r.opts.task_list && node.children.len > 0 && node.children[0].kind == .task_checkbox {
		chk := node.children[0]
		checked_attr := if chk.checked { ' checked=""' } else { '' }
		if r.opts.renderer_opts.xhtml {
			r.sb.write_string('<li><input type="checkbox" disabled=""${checked_attr} /> ')
		} else {
			r.sb.write_string('<li><input type="checkbox" disabled=""${checked_attr}> ')
		}
		for i := 1; i < node.children.len; i++ {
			r.render_node(node.children[i])
		}
		r.sb.write_string('</li>\n')
		return
	}
	r.sb.write_string('<li>')
	r.render_children(node)
	r.sb.write_string('</li>\n')
}

fn (mut r HTMLRenderer) render_code_block(node &Node) {
	r.sb.write_string('<pre><code>')
	r.sb.write_string(html_escape(node.literal))
	r.sb.write_string('</code></pre>\n')
}

fn (mut r HTMLRenderer) render_fenced_code(node &Node) {
	if node.fence_info.len > 0 {
		// Use only the first word of the info string as the language class.
		lang := node.fence_info.split(' ')[0].split('\t')[0]
		r.sb.write_string('<pre><code class="language-${html_escape(lang)}">')
	} else {
		r.sb.write_string('<pre><code>')
	}
	r.sb.write_string(html_escape(node.literal))
	r.sb.write_string('</code></pre>\n')
}

fn (mut r HTMLRenderer) render_thematic_break() {
	if r.opts.renderer_opts.xhtml {
		r.sb.write_string('<hr />\n')
	} else {
		r.sb.write_string('<hr>\n')
	}
}

fn (mut r HTMLRenderer) render_html_block(node &Node) {
	if r.opts.renderer_opts.unsafe_ {
		r.sb.write_string(node.literal)
	} else {
		r.sb.write_string('<!-- raw HTML omitted -->\n')
	}
}

// ---- Table ----

fn (mut r HTMLRenderer) render_table(node &Node) {
	r.sb.write_string('<table>\n')
	r.render_children(node)
	r.sb.write_string('</table>\n')
}

fn (mut r HTMLRenderer) render_table_section(node &Node, tag string) {
	prev_in_table_head := r.in_table_head
	r.in_table_head = tag == 'thead'
	r.sb.write_string('<${tag}>\n')
	r.render_children(node)
	r.sb.write_string('</${tag}>\n')
	r.in_table_head = prev_in_table_head
}

fn (mut r HTMLRenderer) render_table_row(node &Node) {
	// Determine cell tag based on parent kind (table_head uses th).
	// We pass the context via a field or inspect the row context.
	// Since we don't have parent pointer, check if this is a header row via the
	// node's parent tracking. We'll check node.children[0].align as a proxy.
	// Instead, use a simple flag: if any sibling is a table_head, use <th>.
	// For simplicity, we use <td> always and let render_table_cell decide.
	r.sb.write_string('<tr>\n')
	r.render_children(node)
	r.sb.write_string('</tr>\n')
}

fn (mut r HTMLRenderer) render_table_cell(node &Node) {
	// We use a flag in the renderer to know if we're in the head.
	// Simple approach: the cell tag is set by the surrounding context.
	// We'll use <td> and trust the renderer state.
	align_attr := match node.align {
		.left { ' align="left"' }
		.center { ' align="center"' }
		.right { ' align="right"' }
		else { '' }
	}

	cell_tag := if r.in_table_head { 'th' } else { 'td' }
	r.sb.write_string('<${cell_tag}${align_attr}>')
	r.render_inline(node.literal)
	r.sb.write_string('</${cell_tag}>\n')
}

// ---- Definition list ----

fn (mut r HTMLRenderer) render_definition_list(node &Node) {
	r.sb.write_string('<dl>\n')
	r.render_children(node)
	r.sb.write_string('</dl>\n')
}

fn (mut r HTMLRenderer) render_definition_term(node &Node) {
	r.sb.write_string('<dt>')
	r.render_inline(node.literal)
	r.sb.write_string('</dt>\n')
	for child in node.children {
		r.render_node(child)
	}
}

fn (mut r HTMLRenderer) render_definition_desc(node &Node) {
	r.sb.write_string('<dd>')
	r.render_inline(node.literal)
	r.sb.write_string('</dd>\n')
}

// ---- Footnotes ----

fn (mut r HTMLRenderer) render_footnote_ref(node &Node) {
	label := node.fn_label
	// Assign an ordinal on first encounter.
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
	r.sb.write_string('<sup><a href="#fn-${html_escape(label)}" id="fnref-${html_escape(label)}">${idx}</a></sup>')
}

fn (mut r HTMLRenderer) render_footnotes_section() {
	r.sb.write_string('<section class="footnotes">\n<ol>\n')
	for label in r.fn_order {
		fn_node := r.fn_nodes[label] or { continue }
		r.sb.write_string('<li id="fn-${html_escape(label)}">')
		r.render_inline(fn_node.literal)
		r.sb.write_string(' <a href="#fnref-${html_escape(label)}">&#x21A9;</a></li>\n')
	}
	r.sb.write_string('</ol>\n</section>\n')
}

// ---- Inline elements ----

fn (mut r HTMLRenderer) render_text(node &Node) {
	content := if r.opts.typographer {
		smart_punctuate(node.literal)
	} else {
		node.literal
	}
	r.sb.write_string(html_escape(content))
}

fn (mut r HTMLRenderer) render_emphasis(node &Node) {
	r.sb.write_string('<em>')
	r.render_children(node)
	r.sb.write_string('</em>')
}

fn (mut r HTMLRenderer) render_strong(node &Node) {
	r.sb.write_string('<strong>')
	r.render_children(node)
	r.sb.write_string('</strong>')
}

fn (mut r HTMLRenderer) render_code_span(node &Node) {
	r.sb.write_string('<code>')
	r.sb.write_string(html_escape(node.literal))
	r.sb.write_string('</code>')
}

fn (mut r HTMLRenderer) render_link(node &Node) {
	r.sb.write_string('<a href="${html_escape(url_encode(node.dest))}"')
	if node.title.len > 0 {
		r.sb.write_string(' title="${html_escape(node.title)}"')
	}
	r.sb.write_string('>')
	r.render_children(node)
	r.sb.write_string('</a>')
}

fn (mut r HTMLRenderer) render_image(node &Node) {
	alt := node.text_content()
	r.sb.write_string('<img src="${html_escape(url_encode(node.dest))}" alt="${html_escape(alt)}"')
	if node.title.len > 0 {
		r.sb.write_string(' title="${html_escape(node.title)}"')
	}
	if r.opts.renderer_opts.xhtml {
		r.sb.write_string(' />')
	} else {
		r.sb.write_string('>')
	}
}

fn (mut r HTMLRenderer) render_autolink(node &Node) {
	r.sb.write_string('<a href="${html_escape(url_encode(node.dest))}">')
	r.sb.write_string(html_escape(node.literal))
	r.sb.write_string('</a>')
}

fn (mut r HTMLRenderer) render_raw_html(node &Node) {
	if r.opts.renderer_opts.unsafe_ {
		r.sb.write_string(node.literal)
	} else {
		r.sb.write_string('<!-- raw HTML omitted -->')
	}
}

fn (mut r HTMLRenderer) render_hard_break() {
	if r.opts.renderer_opts.xhtml {
		r.sb.write_string('<br />\n')
	} else {
		r.sb.write_string('<br>\n')
	}
}

fn (mut r HTMLRenderer) render_soft_break() {
	if r.opts.renderer_opts.hard_wraps {
		if r.opts.renderer_opts.xhtml {
			r.sb.write_string('<br />\n')
		} else {
			r.sb.write_string('<br>\n')
		}
	} else {
		r.sb.write_string('\n')
	}
}

fn (mut r HTMLRenderer) render_strikethrough(node &Node) {
	r.sb.write_string('<del>')
	r.render_children(node)
	r.sb.write_string('</del>')
}

fn (mut r HTMLRenderer) render_task_checkbox(node &Node) {
	// Rendered inline in render_list_item; standalone fallback:
	checked := if node.checked { ' checked=""' } else { '' }
	if r.opts.renderer_opts.xhtml {
		r.sb.write_string('<input type="checkbox" disabled=""${checked} />')
	} else {
		r.sb.write_string('<input type="checkbox" disabled=""${checked}>')
	}
}
