// Copyright 2026 The V Language. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module markdown

fn highlight_for_renderer_hook_test(lang string, code string) string {
	return '<b>${lang}:${code}</b>'
}

fn resolve_for_renderer_hook_test(base string, url string) string {
	return '${base}/${url}'
}

fn test_renderer_code_highlighter_preserves_commonmark_block_boundaries() {
	mut r := HTMLRenderer{
		code_highlighter: highlight_for_renderer_hook_test
	}
	assert render('- first\n  ```v ignored\n  <&\n  ```', mut r) == '<ul>\n<li>first\n<pre><code class="language-v"><b>v:<&\n</b></code></pre>\n</li>\n</ul>\n'
	assert render('    <&', mut r) == '<pre><code><b>:<&\n</b></code></pre>\n'
	assert render('```\ntext\n```', mut r) == '<pre><code><b>:text\n</b></code></pre>\n'
}

fn test_renderer_link_resolver_preserves_commonmark_image_alt_text() {
	mut r := HTMLRenderer{
		link_base:     '/docs'
		link_resolver: resolve_for_renderer_hook_test
	}
	assert render('![*label* <https://example.com>](image.png "Title")', mut r) == '<p><img src="/docs/image.png" alt="label https://example.com" title="Title"></p>\n'
	assert render('[link](other.md)', mut r) == '<p><a href="/docs/other.md">link</a></p>\n'
}
