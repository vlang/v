// Copyright 2026 The V Language. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module markdown

fn test_to_html_heading() {
	assert to_html('# Hello') == '<h1>Hello</h1>\n'
	assert to_html('## World') == '<h2>World</h2>\n'
}

fn test_to_html_paragraph() {
	assert to_html('Hello world') == '<p>Hello world</p>\n'
}

fn test_to_html_thematic_break() {
	assert to_html('---') == '<hr>\n'
}

fn test_to_html_emphasis() {
	html := to_html('*em*')
	assert html.contains('<em>')
}

fn test_to_html_strong() {
	html := to_html('**bold**')
	assert html.contains('<strong>')
}

fn test_to_html_code_span() {
	html := to_html('`code`')
	assert html.contains('<code>')
	assert html.contains('code')
}

fn test_to_html_link() {
	html := to_html('[link](https://example.com)')
	assert html.contains('<a href="https://example.com">')
	assert html.contains('link')
}

fn test_html_escape_in_text() {
	html := to_html('A < B')
	assert html.contains('&lt;')
}

fn test_empty_input() {
	assert to_html('') == ''
}

fn test_multiline_paragraph() {
	html := to_html('line one\nline two')
	assert html.contains('<p>')
	assert html.contains('line one')
}

fn test_fenced_code() {
	html := to_html('```go\nfn main() {}\n```')
	assert html.contains('<code')
	assert html.contains('fn main')
}

fn test_list() {
	html := to_html('- item')
	assert html.contains('<ul>')
	assert html.contains('<li>')
	assert html.contains('item')
}

fn test_ordered_list() {
	html := to_html('1. first')
	assert html.contains('<ol>')
	assert html.contains('<li>')
	assert html.contains('first')
}

fn test_blockquote() {
	html := to_html('> quote')
	assert html.contains('<blockquote>')
	assert html.contains('quote')
}

fn test_list_multiple_items() {
	html := to_html('- item 1\n- item 2')
	assert html.contains('<ul>')
	assert html.contains('item 1')
	assert html.contains('item 2')
}

fn test_invalid_link_ref_def_does_not_create_reference() {
	src := '[bad]: <https://example.com\n\n[bad]'
	html := to_html(src)
	assert !html.contains('<a href=')
	assert html.contains('[bad]')
}

fn test_valid_link_ref_def_is_resolved() {
	src := '[ok]: <https://example.com>\n\n[ok]'
	html := to_html(src)
	assert html.contains('<a href="https://example.com">ok</a>')
}

fn test_gfm_table_header_uses_th_cells() {
	src := '| a | b |\n| --- | --- |\n| 1 | 2 |'
	html := to_html_opts(src, Options{
		extensions: gfm()
	})
	assert html.contains('<thead>')
	assert html.contains('<th>a</th>')
	assert html.contains('<th>b</th>')
}

fn test_emphasis_underscore_intraword_does_not_emphasize() {
	assert to_html('foo_bar_baz') == '<p>foo_bar_baz</p>\n'
	assert to_html('foo_bar_') == '<p>foo_bar_</p>\n'
	assert to_html('_foo_bar') == '<p>_foo_bar</p>\n'
}

fn test_emphasis_star_delimiters_still_emphasize() {
	assert to_html('a*b*c') == '<p>a<em>b</em>c</p>\n'
}

fn test_emphasis_triple_delimiters() {
	assert to_html('***foo***') == '<p><em><strong>foo</strong></em></p>\n'
	assert to_html('___foo___') == '<p><em><strong>foo</strong></em></p>\n'
	assert to_html('foo***bar***baz') == '<p>foo<em><strong>bar</strong></em>baz</p>\n'
}

fn test_emphasis_nested_mixed_runs() {
	assert to_html('**foo *bar***') == '<p><strong>foo <em>bar</em></strong></p>\n'
	assert to_html('*foo **bar***') == '<p><em>foo <strong>bar</strong></em></p>\n'
	assert to_html('*foo**bar**baz*') == '<p><em>foo<strong>bar</strong>baz</em></p>\n'
	assert to_html('*foo **bar** baz*') == '<p><em>foo <strong>bar</strong> baz</em></p>\n'
	assert to_html('**foo *bar* baz**') == '<p><strong>foo <em>bar</em> baz</strong></p>\n'
}

fn test_emphasis_multiple_of_three_resolution() {
	assert to_html('***foo** bar*') == '<p><em><strong>foo</strong> bar</em></p>\n'
	assert to_html('***foo* bar**') == '<p><strong><em>foo</em> bar</strong></p>\n'
	assert to_html('***foo**bar*') == '<p><em><strong>foo</strong>bar</em></p>\n'
}

fn test_emphasis_underscore_punctuation_flanking() {
	assert to_html('foo-_(bar)_') == '<p>foo-<em>(bar)</em></p>\n'
	assert to_html('foo__bar__baz') == '<p>foo__bar__baz</p>\n'
	assert to_html('foo__bar__') == '<p>foo__bar__</p>\n'
	assert to_html('__foo__bar') == '<p>__foo__bar</p>\n'
}

fn test_setext_heading_leading_spaces() {
	// CommonMark allows 0-3 leading spaces on the setext underline.
	assert to_html('Foo\n   ===') == '<h1>Foo</h1>\n'
	assert to_html('Foo\n  ---') == '<h2>Foo</h2>\n'
	assert to_html('Foo\n ===') == '<h1>Foo</h1>\n'
}

fn test_emphasis_leftover_delimiters_are_literal() {
	// Unmatched delimiters become literal text.
	assert to_html('*a**b**') == '<p>*a<strong>b</strong></p>\n'
	assert to_html('**a**b*') == '<p><strong>a</strong>b*</p>\n'
	assert to_html('*foo bar') == '<p>*foo bar</p>\n'
}

fn test_emphasis_mixed_star_underscore() {
	// * and _ delimiters do not pair with each other.
	assert to_html('*foo _bar_ baz*') == '<p><em>foo <em>bar</em> baz</em></p>\n'
	assert to_html('__foo *bar* baz__') == '<p><strong>foo <em>bar</em> baz</strong></p>\n'
}

fn test_link_ref_def_with_leading_spaces() {
	// CommonMark allows 0-3 leading spaces before a link ref def.
	assert to_html(' [foo]: https://example.com\n\n[foo]') == '<p><a href="https://example.com">foo</a></p>\n'
	assert to_html('  [bar]: https://example.org\n\n[bar]') == '<p><a href="https://example.org">bar</a></p>\n'
	assert to_html('   [baz]: https://v-lang.io\n\n[baz]') == '<p><a href="https://v-lang.io">baz</a></p>\n'
}

fn test_link_ref_def_with_four_leading_spaces_is_not_a_ref() {
	// Four leading spaces start an indented code block, not a reference definition.
	src := '    [foo]: https://example.com\n\n[foo]'
	html := to_html(src)
	assert !html.contains('<a href=')
	assert html.contains('[foo]: https://example.com')
}

fn test_setext_heading_multiline_text() {
	// Multi-line setext heading text should preserve soft breaks.
	html := to_html('Foo\nbar\n===')
	assert html == '<h1>Foo\nbar</h1>\n'
}

fn test_task_list() {
	src := '- [ ] unchecked\n- [x] checked\n- [X] also checked'
	html := to_html_opts(src, Options{
		task_list: true
	})
	assert html.contains('<input type="checkbox" disabled="">')
	assert html.contains('<input type="checkbox" disabled="" checked="">')
	assert html.contains('unchecked')
	assert html.contains('checked')
}

fn test_task_list_not_applied_without_extension() {
	// Without the extension, task markers are rendered as plain text.
	html := to_html('- [ ] item')
	assert !html.contains('<input')
	assert html.contains('[ ] item')
}

fn test_task_list_marker_requires_space_after_closing_bracket() {
	// GFM task markers are [ ]/[x]/[X] followed by whitespace or end of item.
	src := '- [x]ok\n- [ ]todo'
	html := to_html_opts(src, Options{
		task_list: true
	})
	assert !html.contains('<input')
	assert html.contains('[x]ok')
	assert html.contains('[ ]todo')
}

fn test_task_list_xhtml_checkbox_self_closing() {
	html := to_html_opts('- [x] done', Options{
		task_list:     true
		renderer_opts: RendererOptions{
			xhtml: true
		}
	})
	assert html.contains('<input type="checkbox" disabled="" checked="" />')
}

fn test_link_ref_def_multiline_title() {
	// CommonMark allows the title on the next line when the destination is alone.
	src := '[foo]: /url\n"a title"\n\n[foo]'
	html := to_html(src)
	assert html.contains('<a href="/url"')
	assert html.contains('title="a title"')
	assert html.contains('>foo</a>')
}

fn test_link_ref_def_multiline_title_single_quotes() {
	src := "[bar]: /path\n'my title'\n\n[bar]"
	html := to_html(src)
	assert html.contains('<a href="/path"')
	assert html.contains('title="my title"')
}

fn test_link_ref_def_multiline_no_title_next_line_is_content() {
	// If the next line is not a title, it becomes normal content.
	src := '[baz]: /url\n\nsome text\n\n[baz]'
	html := to_html(src)
	assert html.contains('<a href="/url">baz</a>')
	assert html.contains('some text')
}

fn test_gfm_helper_sets_core_extension_flags() {
	md := new(Options{
		extensions: gfm()
	})
	assert md.opts.tables
	assert md.opts.strikethrough
	assert md.opts.linkify
	assert md.opts.task_list
}

fn test_individual_extension_helpers_set_flags() {
	md_footnote := new(Options{
		extensions: [Extension(footnote())]
	})
	assert md_footnote.opts.footnotes

	md_typographer := new(Options{
		extensions: [Extension(typographer())]
	})
	assert md_typographer.opts.typographer

	md_definition_list := new(Options{
		extensions: [Extension(definition_list())]
	})
	assert md_definition_list.opts.definition_list
}

fn test_emphasis_goldmark_parity_edge_cases() {
	assert to_html('_a* __*_* b b') == '<p><em>a* __*</em>* b b</p>\n'
	assert to_html('* bb _ *__*a* a_') == '<ul>\n<li>bb _ *__<em>a</em> a_</li>\n</ul>\n'
	assert to_html('baa _ a*aba**_ba') == '<p>baa _ a*aba**_ba</p>\n'
	assert to_html('_a_*_b**_aba*') == '<p><em>a</em><em>_b**_aba</em></p>\n'
	assert to_html('x_ ***b*ab*bb_a*a a') == '<p>x_ <em><em><em>b</em>ab</em>bb_a</em>a a</p>\n'
}
