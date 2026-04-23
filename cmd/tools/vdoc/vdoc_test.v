// vtest retry: 2
// vtest build: !windows
module main

import os
import arrays
import v.ast
import document as doc
import markdown

const vexe_path = @VEXE
const vexe_ = os.quoted_path(vexe_path)
const tpath = os.join_path(os.vtmp_dir(), 'vod_test_module')

fn testsuite_begin() {
	os.rmdir_all(tpath) or {}
	os.mkdir_all(tpath)!
	os.chdir(tpath)!
}

fn testsuite_end() {
	os.rmdir_all(tpath) or {}
}

fn test_trim_doc_node_description() {
	mod := 'foo'
	mut readme := '## Description

`foo` is a module that provides tools and utility functions to assist in working with bar.
It also assists with composing and testing baz.'
	expected := 'is a module that provides tools and utility functions to assist in working with'
	res := trim_doc_node_description(mod, readme).trim_space()
	assert res == expected

	readme = '# Foo
`foo` is a module that provides tools and utility functions to assist in working with bar.
It also assists with composing and testing baz.'
	res2 := trim_doc_node_description(mod, readme).trim_space()
	assert res2 == res
}

fn test_ignore_rules() {
	os.write_file('.vdocignore', ['pattern1', 'pattern2', '/path1'].join_lines())!
	os.mkdir('subdir')!
	os.write_file(os.join_path('subdir', '.vdocignore'), ['pattern3', '/path2'].join_lines())!
	rules := IgnoreRules.get('.')
	assert rules.patterns['.'] == ['pattern1', 'pattern2']
	assert rules.patterns['./subdir'] == ['pattern3']
	assert rules.paths == {
		'./path1':        true
		'./subdir/path2': true
	}
}

fn test_get_module_list() {
	// For information on leading slash rules, refer to the comments in `IgnoreRules.get`.
	ignore_rules := ['bravo', '/echo', '/foxtrot/golf', 'hotel.v/', 'india/juliett']
	os.write_file('.vdocignore', ignore_rules.join_lines())!

	/* Create some submodules.
	Modules inside `testdata` and `tests` directories and modules that
	only contain `_test.v` files should be ignored by default. */
	// Modules NOT to ignore.
	submodules_no_ignore := [
		'alpha',
		'alpha_bravo', // test `bravo`
		'bravo_charly', // test `bravo`
		'charly',
		'charly/alpha',
		'charly/delta', // test `delta` in separate ignore file in `alpha`
		'charly/echo', // test `/echo`
		'charly/foxtrot/golf', // test `/foxtrot/golf`
		'foxtrot',
		'golf',
		'hotel', // will include a `hotel.v` file, whose pattern is in the ignore list with a trailing slash
	]
	// Modules TO ignore.
	submodules_to_ignore := [
		'alpha/bravo', // test `bravo`
		'alpha/delta', // test `delta` in separate ignore file
		'alpha/india/juliett/kilo', // test `india/juliett`
		'bravo', // test `bravo`
		'echo', // test `/echo`
		'foxtrot/golf', // test `/foxtrot/golf`
		'hotel.v', // test `hotel.v/`
		'tests', // test default
		'testdata', // test default
		'testdata/foxtrot', // test default
	]
	for p in arrays.append(submodules_no_ignore, submodules_to_ignore) {
		os.mkdir_all(p)!
		mod_name := p.all_after_last('/')
		os.write_file(os.join_path(p, '${mod_name}.v'), 'module ${mod_name}')!
	}
	// Create a module that only contains a `_test.v` file.
	os.mkdir('delta')!
	os.write_file(os.join_path('delta', 'delta_test.v'), 'module delta')!
	// Add a `.vdocignore` file to a submodule.
	os.write_file(os.join_path('alpha', '.vdocignore'), 'delta\n')!

	mod_list := get_modules(tpath)
	// dump(mod_list)
	assert mod_list.len == submodules_no_ignore.len
	for m in submodules_no_ignore.map(os.join_path(tpath, it)) {
		assert m in mod_list
	}
	for m in submodules_to_ignore.map(os.join_path(tpath, it)) {
		assert m !in mod_list
	}
	// `delta` only contains a `_test.v` file.
	assert !mod_list.any(it.contains(os.join_path(tpath, 'delta')))
}

fn test_html_highlight_escapes_html_tokens() {
	table := ast.new_table()
	code := 'fn main() {
	// <h1>owned</h1>
	assert 1 < 2
}'
	highlighted := html_highlight(code, table)
	assert highlighted.contains('// &lt;h1&gt;owned&lt;/h1&gt;')
	assert !highlighted.contains('<h1>owned</h1>')
	assert highlighted.contains('<span class="token operator">&lt;</span>')
}

fn test_get_readme_md_src() {
	// a special testcase for `src` dir get_readme
	// https://github.com/vlang/v/issues/24232

	os.mkdir('src')!
	os.write_file('v.mod', "Module {
        name: 'foobar'
        description: 'foobar'
        version: '0.0.0'
        license: 'MIT'
        dependencies: []
}
")!
	os.write_file('src/foobar.v', 'module foobar

// square calculates the second power of `x`
pub fn square(x int) int {
        return x * x
}
')!
	res := os.execute_opt('${vexe_} doc -m src/ -v') or { panic(err) }
	assert res.exit_code == 0
	assert res.output.contains('square')
}

fn test_gen_modules_toc_skips_hash_links_for_prefix_only_groups() {
	mut vd := VDoc{
		cfg: Config{
			is_multi: true
		}
	}
	vd.docs = [
		doc.Doc{
			head: doc.DocNode{
				name: 'main'
			}
		},
		doc.Doc{
			head: doc.DocNode{
				name: 'db.mysql'
			}
		},
		doc.Doc{
			head: doc.DocNode{
				name: 'db.sqlite'
			}
		},
	]
	toc := vd.gen_modules_toc('main')
	assert !toc.contains('href="#"')
	assert toc.contains('<div class="menu-row"><a>db</a></div>')
	assert toc.contains('<li><a href="./db.mysql.html">mysql</a></li>')
}

fn test_gen_modules_toc_uses_prefix_module_page_when_available() {
	mut vd := VDoc{
		cfg: Config{
			is_multi: true
		}
	}
	vd.docs = [
		doc.Doc{
			head: doc.DocNode{
				name: 'db.sqlite'
			}
		},
		doc.Doc{
			head: doc.DocNode{
				name: 'db'
			}
		},
	]
	toc := vd.gen_modules_toc('db')
	assert toc.contains('<div class="menu-row"><a href="./db.html">db</a></div>')
}

fn test_module_overview_uses_post_module_comment_without_readme() {
	mod_dir := 'module_overview'
	os.mkdir(mod_dir)!
	os.write_file(os.join_path(mod_dir, 'overview.v'), "module overview

// `overview` uses the first comment after the module declaration as the module overview.

pub fn greet() string {
	return 'hello'
}
")!
	res := os.execute_opt('${vexe_} doc -no-timestamp -f text -o - -readme -comments ${os.quoted_path(
		'./' + mod_dir)}') or { panic(err) }
	assert res.exit_code == 0
	assert res.output.replace('\r\n', '\n').trim_space() == 'module overview
    `overview` uses the first comment after the module declaration as the module overview.

fn greet() string'
}

fn test_html_keeps_enum_comment_after_top_level_comptime_if() {
	mod_dir := 'issue_23338'
	os.mkdir(mod_dir)!
	os.write_file(os.join_path(mod_dir, 'issue_23338.v'), 'module issue_23338

\$if macos {
}

// Foo lorem ipsum foo.
pub enum Foo {
	foo
}

// Bar ipsum lorem bar.
pub enum Bar {
	bar
}
')!
	res := os.execute_opt('${vexe_} doc -no-timestamp -m -f html -o - -html-only-contents ${os.quoted_path(
		'./' + mod_dir)}') or { panic(err) }
	assert res.exit_code == 0
	output := res.output.replace('\r\n', '\n')
	assert output.contains('Foo lorem ipsum foo.')
	assert output.contains('Bar ipsum lorem bar.')
}

fn test_doc_generates_for_modules_without_public_symbols() {
	mod_dir := 'module_without_public_symbols'
	os.mkdir(mod_dir)!
	os.write_file(os.join_path(mod_dir, 'module_without_public_symbols.v'), 'module module_without_public_symbols

const internal = 1
')!
	res := os.execute_opt('${vexe_} doc -no-timestamp -f text -o - ${os.quoted_path('./' + mod_dir)}') or {
		panic(err)
	}
	assert res.exit_code == 0
	assert res.output.replace('\r\n', '\n').trim_space() == 'module module_without_public_symbols'
}

fn test_resolve_relative_markdown_link() {
	base := 'https://github.com/vlang/v/blob/master/vlib/net/html/'
	assert resolve_relative_markdown_link(base, 'parser_test.v') == 'https://github.com/vlang/v/blob/master/vlib/net/html/parser_test.v'
	assert resolve_relative_markdown_link(base, './html_test.v') == 'https://github.com/vlang/v/blob/master/vlib/net/html/html_test.v'
	assert resolve_relative_markdown_link(base, '../README.md#usage') == 'https://github.com/vlang/v/blob/master/vlib/net/README.md#usage'
}

fn test_resolve_relative_markdown_link_keeps_absolute_urls() {
	base := 'https://github.com/vlang/v/blob/master/vlib/net/html/'
	assert resolve_relative_markdown_link(base, 'https://vlang.io') == 'https://vlang.io'
	assert resolve_relative_markdown_link(base, '/rooted/path') == '/rooted/path'
	assert resolve_relative_markdown_link(base, '#local') == '#local'
}

fn test_markdown_renderer_resolves_relative_links() ! {
	base := 'https://github.com/vlang/v/blob/master/vlib/net/html/'
	mut renderer := markdown.HtmlRenderer{
		transformer: &MdHtmlCodeHighlighter{
			table:              ast.new_table()
			relative_link_base: base
		}
	}
	out := markdown.render('More examples in [parser](parser_test.v).', mut renderer)!
	assert out.contains('<a href="https://github.com/vlang/v/blob/master/vlib/net/html/parser_test.v">')
}

fn test_prepare_markdown_for_html_preserves_blockquote_linebreaks() ! {
	mut renderer := markdown.HtmlRenderer{
		transformer: &MdHtmlCodeHighlighter{
			table: ast.new_table()
		}
	}
	out := markdown.render(prepare_markdown_for_html('> **Note**\n> line one\n> line two'), mut
		renderer)!
	assert out.contains('<blockquote>')
	assert out.contains('<strong>Note</strong><br />line one<br />line two')
}

fn test_prepare_markdown_for_html_skips_fenced_code_blocks() {
	input := '```sh\n> prompt\n> next\n```'
	assert prepare_markdown_for_html(input) == input
}

fn test_markdown_renderer_preserves_wrapped_readme_markdown() ! {
	input := '1. The basic atomic elements of this regex engine are the tokens.\n   In a query string a simple character is a token.\n\n- The basic element **is the token not the sequence of symbols**,\n  and the most simple token, is a single character.\n\n- `|` **the OR operator acts on tokens,** for example `abc|ebc` is not\n  `abc` OR `ebc`.'
	mut renderer := markdown.HtmlRenderer{
		transformer: &MdHtmlCodeHighlighter{
			table: ast.new_table()
		}
	}
	out := markdown.render(prepare_markdown_for_html(input), mut renderer)!
	assert !out.contains('tokens.In')
	assert !out.contains('mostsimple')
	assert !out.contains('not<code>abc</code>')
	assert out.contains('tokens. In a query string a simple character is a token.')
	assert out.contains('the most simple token')
	assert out.contains('is not <code>abc</code> OR <code>ebc</code>')
}
