// vtest retry: 2
// vtest build: !windows
module main

import os
import arrays
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
	code := 'fn main() {
	// <h1>owned</h1>
	assert 1 < 2
}'
	highlighted := html_highlight(code)
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
	res := os.execute_opt('${vexe_} doc -no-timestamp -f text -o - -readme -comments ${os.quoted_path('./' + mod_dir)}') or { panic(err) }
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
	res := os.execute_opt('${vexe_} doc -no-timestamp -m -f html -o - -html-only-contents ${os.quoted_path('./' + mod_dir)}') or { panic(err) }
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
			relative_link_base: base
		}
	}
	out := markdown.render('More examples in [parser](parser_test.v).', mut renderer)!
	assert out.contains('<a href="https://github.com/vlang/v/blob/master/vlib/net/html/parser_test.v">')
}

fn test_prepare_markdown_for_html_preserves_blockquote_linebreaks() ! {
	mut renderer := markdown.HtmlRenderer{
		transformer: &MdHtmlCodeHighlighter{}
	}
	out := markdown.render(prepare_markdown_for_html('> **Note**\n> line one\n> line two'), mut renderer)!
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
		transformer: &MdHtmlCodeHighlighter{}
	}
	out := markdown.render(prepare_markdown_for_html(input), mut renderer)!
	assert !out.contains('tokens.In')
	assert !out.contains('mostsimple')
	assert !out.contains('not<code>abc</code>')
	assert out.contains('tokens. In a query string a simple character is a token.')
	assert out.contains('the most simple token')
	assert out.contains('is not <code>abc</code> OR <code>ebc</code>')
}

fn test_doc_multi_skips_modules_without_valid_files_for_platform() {
	// https://github.com/vlang/v/issues/27464
	// A module whose only V files are filtered out for the target platform (e.g.
	// the `ios`/`macos` modules when generating docs on Linux) is skipped during
	// generation. It must not produce an empty `Doc` that later crashes rendering.
	base_dir := 'skip_platform_modules'
	good_dir := os.join_path(base_dir, 'good')
	skipped_dir := os.join_path(base_dir, 'winonly')
	os.mkdir_all(good_dir)!
	os.mkdir_all(skipped_dir)!
	os.write_file(os.join_path(good_dir, 'good.v'), "module good

// hello returns a greeting.
pub fn hello() string {
	return 'hi'
}
")!
	// This file only compiles on Windows, so on every other platform the `winonly`
	// module has no valid V files and gets skipped. The test never runs on Windows
	// (see the `vtest build: !windows` directive at the top of the file).
	os.write_file(os.join_path(skipped_dir, 'winonly_windows.c.v'), 'module winonly

// only_win does nothing useful here.
pub fn only_win() int {
	return 1
}
')!
	// `-color` exercises the original crash path in `gen_plaintext`.
	res := os.execute_opt('${vexe_} doc -no-timestamp -m -color -f text -o - ${os.quoted_path('./' + base_dir)}') or { panic(err) }
	// The crash showed up as a non-zero exit code (V panic), so this is the key check.
	assert res.exit_code == 0
	assert res.output.contains('hello')
	// The skipped module must not be documented (its public symbol must be absent).
	// Note: `os.execute_opt` merges stderr, where the `Skipping folder: ...winonly`
	// notice is printed, so we check for the rendered symbol rather than the name.
	assert !res.output.contains('only_win')
}

fn test_doc_multi_all_modules_skipped_fails_for_file_output() {
	// https://github.com/vlang/v/issues/27464 (review follow-up)
	// When every discovered module is filtered out for the target platform, there
	// is nothing to document. Writing to a real output path must fail with the same
	// `No documentation found` error as the stdout path, instead of silently
	// creating/cleaning an empty output directory and exiting 0.
	base_dir := 'all_skipped_modules'
	skipped_dir := os.join_path(base_dir, 'winonly')
	out_dir := os.join_path(base_dir, 'out')
	os.mkdir_all(skipped_dir)!
	os.write_file(os.join_path(skipped_dir, 'winonly_windows.c.v'), 'module winonly

pub fn only_win() int {
	return 1
}
')!
	// `os.execute` (not `execute_opt`) so the expected non-zero exit is not an error.
	res := os.execute('${vexe_} doc -no-timestamp -m -f html -o ${os.quoted_path('./' + out_dir)} ${os.quoted_path('./' + base_dir)}')
	assert res.exit_code != 0
	assert res.output.contains('No documentation found')
	// The output directory must not have been created.
	assert !os.exists(out_dir)
}

// The "Available modules" list names modules, not directories: each one as it
// sits under the input root, which is what a reader can act on. `modules/` is an
// ordinary directory now, so a module under one keeps that in its name.
fn test_module_display_name_is_relative_to_the_input_root() {
	assert module_display_name('/tmp/app/modules/foo', '/tmp/app') == 'modules.foo'
	assert module_display_name('/tmp/app/foo', '/tmp/app') == 'foo'
	assert module_display_name('/tmp/app/foo/bar', '/tmp/app/') == 'foo.bar'
	assert module_display_name('./vlib/os', '.') == 'vlib.os'
	assert module_display_name('vlib/v/ast', 'vlib') == 'v.ast'
	// An input that is the module itself, and anything the root does not hold,
	// are named by the directory they are.
	assert module_display_name('/tmp/app', '/tmp/app') == 'app'
	assert module_display_name('/somewhere/else/foo', '/tmp/app') == 'foo'
}

fn write_subdirs_fixture(root string, vmod_extra string, files map[string]string) ! {
	os.mkdir_all(root)!
	os.write_file(os.join_path(root, 'v.mod'), "Module {\n\tname: 'mypkg'\n\tversion: '0.0.1'\n${vmod_extra}}\n")!
	for rel, content in files {
		path := os.join_path(root, rel)
		os.mkdir_all(os.dir(path))!
		os.write_file(path, content)!
	}
}

fn test_vmod_subdirs_are_documented_as_part_of_the_module() {
	root := 'subdirs_mod'
	write_subdirs_fixture(root, "\tsubdirs: ['internal', 'internal/deep']\n", {
		'root.v':                   'module mypkg\n\npub fn root_fn() {}\n'
		'internal/sub.v':           'module mypkg\n\npub fn sub_fn() {}\n'
		'internal/deep/deep.v':     'module mypkg\n\npub fn deep_fn() {}\n'
		'internal/nested/v.mod':    "Module {\n\tname: 'nested'\n}\n"
		'internal/nested/nested.v': 'module nested\n\npub fn nested_fn() {}\n'
		'internal_other/other.v':   'module internal_other\n\npub fn other_fn() {}\n'
	})!
	d := doc.generate(root, true, true, .auto)!
	assert d.head.name == 'mypkg'
	assert d.contents.keys().sorted() == ['deep_fn', 'root_fn', 'sub_fn']
	assert get_modules(root) == [root, os.join_path(root, 'internal', 'nested'),
		os.join_path(root, 'internal_other')]
	res := os.execute_opt('${vexe_} doc -no-timestamp -f text -o - ${root}')!
	assert res.output.contains('fn root_fn()')
	assert res.output.contains('fn sub_fn()')
	assert res.output.contains('fn deep_fn()')
}

fn test_vmod_subdirs_without_root_files() {
	root := 'subdirs_only'
	write_subdirs_fixture(root, "\tsubdirs: ['internal']\n", {
		'internal/sub.v': 'module mypkg\n\npub fn sub_fn() {}\n'
	})!
	d := doc.generate(root, true, true, .auto)!
	assert d.contents.keys() == ['sub_fn']
	assert get_modules(root) == [root]
}

fn test_vmod_subdirs_are_resolved_from_base_url() {
	root := 'subdirs_base_url'
	write_subdirs_fixture(root, "\tbase_url: 'src'\n\tsubdirs: ['internal']\n", {
		'src/root.v':         'module mypkg\n\npub fn root_fn() {}\n'
		'src/internal/sub.v': 'module mypkg\n\npub fn sub_fn() {}\n'
	})!
	src := os.join_path(root, 'src')
	d := doc.generate(src, true, true, .auto)!
	assert d.contents.keys().sorted() == ['root_fn', 'sub_fn']
	assert get_modules(root) == [src]
}

fn test_vmod_subdirs_dot_walks_the_whole_module_tree() {
	root := 'subdirs_dot'
	write_subdirs_fixture(root, "\tsubdirs: ['.']\n", {
		'root.v':                   'module mypkg\n\npub struct Thing {}\n\npub fn (t Thing) method_fn() {}\n'
		'internal/deep/deep.v':     'module mypkg\n\npub fn deep_fn() {}\n'
		'internal/nested/v.mod':    "Module {\n\tname: 'nested'\n}\n"
		'internal/nested/nested.v': 'module nested\n\npub fn nested_fn() {}\n'
	})!
	// A symlink to an already documented file must not document its symbols twice.
	os.symlink(os.real_path(os.join_path(root, 'root.v')), os.join_path(root, 'internal',
		'root_link.v'))!
	d := doc.generate(root, true, true, .auto)!
	assert d.contents.keys().sorted() == ['Thing', 'deep_fn']
	assert d.contents['Thing'].children.filter(it.name == 'method_fn').len == 1
	assert get_modules(root) == [root, os.join_path(root, 'internal', 'nested')]
}

fn test_repo_file_path_for_links_keeps_subdirs() {
	root := 'subdirs_links'
	write_subdirs_fixture(root, "\tsubdirs: ['internal']\n", {
		'root.v':         'module mypkg\n\npub fn root_fn() {}\n'
		'internal/sub.v': 'module mypkg\n\npub fn sub_fn() {}\n'
	})!
	vd := VDoc{
		cfg: Config{
			input_path: root
		}
	}
	real_root := os.real_path(root)
	assert vd.get_repo_file_path_for_links(os.join_path(real_root, 'root.v')) == 'root.v'
	assert vd.get_repo_file_path_for_links(os.join_path(real_root, 'internal', 'sub.v')) == 'internal/sub.v'
}

fn test_vdocignore_applies_to_vmod_subdirs() {
	root := 'subdirs_ignored'
	// A subdir outside of the module folder is documented, but the default rules
	// (e.g. `testdata`) still apply to it.
	os.mkdir_all('subdirs_shared')!
	os.write_file(os.join_path('subdirs_shared', 'shared.v'), 'module mypkg\n\npub fn shared_fn() {}\n')!
	os.mkdir_all(os.join_path('subdirs_shared', 'testdata'))!
	os.write_file(os.join_path('subdirs_shared', 'testdata', 't.v'), 'module mypkg\n\npub fn shared_fixture_fn() {}\n')!
	os.mkdir_all('subdirs_link_target')!
	os.write_file(os.join_path('subdirs_link_target', 'linked.v'), 'module mypkg\n\npub fn linked_fn() {}\n')!
	os.mkdir_all('subdirs_two_links_target')!
	os.write_file(os.join_path('subdirs_two_links_target', 'two.v'), 'module mypkg\n\npub fn two_links_fn() {}\n')!
	write_subdirs_fixture(root, "\tsubdirs: ['internal', 'linked', 'ignored_link', 'allowed_link', '../subdirs_shared']\n", {
		'.vdocignore':           'private\nskip.v\n/linked\n/ignored_link/two.v\n'
		'root.v':                'module mypkg\n\npub fn root_fn() {}\n'
		'internal/sub.v':        'module mypkg\n\npub fn sub_fn() {}\n'
		'internal/skip.v':       'module mypkg\n\npub fn skipped_fn() {}\n'
		'internal/private/p.v':  'module mypkg\n\npub fn private_fn() {}\n'
		'internal/testdata/t.v': 'module mypkg\n\npub fn fixture_fn() {}\n'
	})!
	// Rules match the subdir name used in the module, even when it is a symlink.
	os.symlink(os.real_path('subdirs_link_target'), os.join_path(root, 'linked'))!
	// A file ignored through one alias is still documented when reached through another one.
	os.symlink(os.real_path('subdirs_two_links_target'), os.join_path(root, 'ignored_link'))!
	os.symlink(os.real_path('subdirs_two_links_target'), os.join_path(root, 'allowed_link'))!
	single := os.execute_opt('${vexe_} doc -no-timestamp -f text -o - ${root}')!
	os.execute_opt('${vexe_} doc -no-timestamp -m -f text ${root}')!
	multi := os.read_file(os.join_path(root, '_docs', 'mypkg.txt'))!
	for output in [single.output, multi] {
		assert output.contains('fn root_fn()')
		assert output.contains('fn sub_fn()')
		assert output.contains('fn shared_fn()')
		assert !output.contains('skipped_fn')
		assert !output.contains('private_fn')
		assert !output.contains('fixture_fn')
		assert !output.contains('shared_fixture_fn')
		assert !output.contains('linked_fn')
		assert output.contains('fn two_links_fn()')
	}
}
