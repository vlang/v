module main

import document as doc

// doc_helpers_test covers the small string/URL helpers that vdoc uses to
// resolve links, format ids and classify markdown lines. The AST walking, HTML
// generation and the file/dir plumbing are covered by vdoc_test.v.

fn test_slug_replaces_spaces_with_dashes() {
	assert slug('Hello World') == 'Hello-World'
	assert slug('a b  c') == 'a-b--c'
	assert slug('') == ''
	assert slug('no spaces') == 'no-spaces'
}

fn test_escape_escapes_doc_comment_carriers() {
	assert escape('he said "hi"') == 'he said \\"hi\\"'
	assert escape('a\tb') == 'a\\tb'
	assert escape('a\nb') == 'a\\nb'
	assert escape('a\r\nb') == 'a\\nb'
	assert escape('plain') == 'plain'
}

fn test_set_output_type_from_str_accepts_every_alias() {
	assert set_output_type_from_str('html') == .html
	assert set_output_type_from_str('htm') == .html
	assert set_output_type_from_str('md') == .markdown
	assert set_output_type_from_str('markdown') == .markdown
	assert set_output_type_from_str('json') == .json
	assert set_output_type_from_str('text') == .plaintext
	assert set_output_type_from_str('none') == .none
}

// NOTE: `ansi` maps to `.ansi` and everything unrecognised — including the empty
// string — falls through to it, so an unknown format is never an error.
fn test_set_output_type_from_str_defaults_to_ansi() {
	assert set_output_type_from_str('ansi') == .ansi
	assert set_output_type_from_str('bogus') == .ansi
	assert set_output_type_from_str('') == .ansi
}

fn test_gen_footer_text_omits_the_date_when_asked() {
	d := doc.Doc{}
	assert gen_footer_text(&d, false) == 'Powered by vdoc.'
	assert gen_footer_text(&d, true).starts_with('Powered by vdoc. Generated on: ')
	assert gen_footer_text(&d, true).len > 'Powered by vdoc.'.len
}

fn test_get_node_id_joins_the_parent_name() {
	assert get_node_id(doc.DocNode{
		name:        'foo'
		parent_name: 'Bar'
	}) == 'Bar.foo'
	assert get_node_id(doc.DocNode{
		name: 'foo'
	}) == 'foo'
	assert get_node_id(doc.DocNode{
		name: 'foo bar'
	}) == 'foo-bar'
}

fn test_get_sym_name_prefixes_the_parent() {
	assert get_sym_name(doc.DocNode{
		name: 'foo'
	}) == 'foo'
	assert get_sym_name(doc.DocNode{
		name:        'foo'
		parent_name: 'Bar'
	}) == '(Bar) foo'
	assert get_sym_name(doc.DocNode{
		is_readme:   true
		frontmatter: {
			'title': 'Read Me'
		}
	}) == 'Read Me'
}

fn test_is_module_readme_needs_a_comment_or_the_readme_flag() {
	assert is_module_readme(doc.DocNode{
		is_readme: true
	})
	assert is_module_readme(doc.DocNode{
		comments: [doc.DocComment{
			text: 'module docs'
		}]
		content:  'module foo'
		name:     'foo'
	})
	// A node whose content merely *looks* like a module declaration is not one
	// without the comment that goes with it.
	assert !is_module_readme(doc.DocNode{
		content: 'module foo'
		name:    'foo'
	})
	assert !is_module_readme(doc.DocNode{
		name: 'foo'
	})
}

fn test_without_pub_keyword_keeps_the_attributes() {
	assert without_pub_keyword('pub fn f()') == 'fn f()'
	assert without_pub_keyword("@[deprecated: 'use pub fn g']\n@[inline]\npub fn f()") == "@[deprecated: 'use pub fn g']\n@[inline]\nfn f()"
	assert without_pub_keyword('pub struct S {\npub:\n\tx int\n}') == 'struct S {\npub:\n\tx int\n}'
	assert without_pub_keyword('fn T.zero() T') == 'fn T.zero() T'
}

fn test_without_pub_keywords_handles_every_constant_of_a_group() {
	assert without_pub_keywords('pub const a = 1\npub const b = 2') == 'const a = 1\nconst b = 2'
	assert without_pub_keywords('@[markused]\npub const a = 1') == '@[markused]\nconst a = 1'
}

fn test_without_attribute_lines_removes_only_the_leading_attributes() {
	assert without_attribute_lines('fn f()') == 'fn f()'
	assert without_attribute_lines("@[deprecated: 'x']\n@[inline]\nfn f()") == 'fn f()'
	assert without_attribute_lines('@[params]\nstruct S {\n\ta int\n}') == 'struct S {\n\ta int\n}'
}

fn test_is_deprecated_attribute_matches_both_deprecation_attributes() {
	assert is_deprecated_attribute("@[deprecated: 'use g']")
	assert is_deprecated_attribute('@[deprecated]')
	assert is_deprecated_attribute("@[deprecated_after: '2999-01-01']")
	assert !is_deprecated_attribute('@[inline]')
	assert !is_deprecated_attribute("@[export: 'deprecated']")
}

fn test_get_repo_file_link_builds_a_per_host_blob_url() {
	assert get_repo_file_link('https://github.com/vlang/v', 'master', 'vlib/foo.v') == 'https://github.com/vlang/v/blob/master/vlib/foo.v'
	assert get_repo_file_link('https://gitlab.com/a/b', 'master', 'vlib/foo.v') == 'https://gitlab.com/a/b/-/blob/master/vlib/foo.v'
	assert get_repo_file_link('https://git.sir.ht/~x/y', 'master', 'vlib/foo.v') == 'https://git.sir.ht/~x/y/tree/master/vlib/foo.v'
}

fn test_get_repo_file_link_refuses_unknown_hosts_and_missing_files() {
	// An unknown host has no `/blob/` flavour, so the repository URL itself is
	// returned unchanged.
	assert get_repo_file_link('https://example.com/x', 'master', 'vlib/foo.v') == 'https://example.com/x'
	assert get_repo_file_link('not-a-url', 'master', 'vlib/foo.v') == 'not-a-url'
	assert get_repo_file_link('', 'master', 'vlib/foo.v') == ''
	assert get_repo_file_link('https://github.com/vlang/v', 'master', '') == ''
}

fn test_get_src_dir_link_points_at_the_directory_of_the_file() {
	assert get_src_dir_link('https://github.com/vlang/v', 'master', 'vlib/foo.v') == 'https://github.com/vlang/v/blob/master/vlib/'
	assert get_src_dir_link('not-a-url', 'master', 'vlib/foo.v') == 'not-a-url/'
}

fn test_get_src_link_adds_the_line_fragment() {
	assert get_src_link('https://github.com/vlang/v', 'master', 'vlib/foo.v', 42) == 'https://github.com/vlang/v/blob/master/vlib/foo.v#L42'
	assert get_src_link('not-a-url', 'master', 'vlib/foo.v', 42) == 'not-a-url#L42'
}

fn test_normalize_url_path_resolves_dot_segments_and_empty_parts() {
	assert normalize_url_path('') == ''
	assert normalize_url_path('a/b') == 'a/b'
	assert normalize_url_path('/a/b') == '/a/b'
	assert normalize_url_path('/a//b') == '/a/b'
	assert normalize_url_path('/a/./b') == '/a/b'
	assert normalize_url_path('/a/b/../c') == '/a/c'
	assert normalize_url_path('a/../../b') == 'b'
	assert normalize_url_path('/') == '/'
	assert normalize_url_path('///') == '/'
}

// NOTE: a leading `..` is dropped rather than kept, so `/../../..` collapses to
// the root instead of escaping it.
fn test_normalize_url_path_keeps_a_rooted_path_rooted() {
	assert normalize_url_path('/../../..') == '/'
}

fn test_is_relative_markdown_link_rejects_absolute_and_external_targets() {
	assert is_relative_markdown_link('rel.md')
	assert is_relative_markdown_link('./rel.md')
	assert is_relative_markdown_link('../up.md')
	assert is_relative_markdown_link('a/b.md#frag')
	assert !is_relative_markdown_link('')
	assert !is_relative_markdown_link('#anchor')
	assert !is_relative_markdown_link('//example.com/x')
	assert !is_relative_markdown_link('/abs/path')
	assert !is_relative_markdown_link('https://x.com/a')
	assert !is_relative_markdown_link('mailto:a@b.c')
}

fn test_markdown_line_is_list_item_requires_the_separating_space() {
	assert markdown_line_is_list_item('- item')
	assert markdown_line_is_list_item('* item')
	assert markdown_line_is_list_item('+ item')
	assert markdown_line_is_list_item('1. item')
	assert !markdown_line_is_list_item('1.item')
	assert !markdown_line_is_list_item('-')
	assert !markdown_line_is_list_item('x - y')
}

fn test_markdown_line_is_horizontal_rule_needs_three_equal_characters() {
	assert markdown_line_is_horizontal_rule('---')
	assert markdown_line_is_horizontal_rule('===')
	assert markdown_line_is_horizontal_rule('***')
	assert markdown_line_is_horizontal_rule('___')
	assert markdown_line_is_horizontal_rule('~~~~')
	assert markdown_line_is_horizontal_rule('----')
	assert markdown_line_is_horizontal_rule('  ----')
	assert !markdown_line_is_horizontal_rule('--')
	assert !markdown_line_is_horizontal_rule('-a-')
}

fn test_markdown_indent_width_counts_a_tab_as_four() {
	assert markdown_indent_width('a') == 0
	assert markdown_indent_width('    indented') == 4
	assert markdown_indent_width('\tindented') == 4
	assert markdown_indent_width('  \t indented') == 7
}

fn test_markdown_line_starts_new_block_recognises_blocks_and_indent() {
	assert markdown_line_starts_new_block('# heading')
	assert markdown_line_starts_new_block('> quote')
	assert markdown_line_starts_new_block('| table')
	assert markdown_line_starts_new_block('```v')
	assert markdown_line_starts_new_block('1. item')
	assert markdown_line_starts_new_block('---')
	assert markdown_line_starts_new_block('    indented')
	assert !markdown_line_starts_new_block('a')
	assert !markdown_line_starts_new_block('')
}
