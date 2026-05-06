// Copyright 2026 The V Language. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Module markdown provides CommonMark-compliant markdown parsing and HTML
// rendering with support for GitHub Flavored Markdown and additional
// extensions. It is designed for feature parity with github.com/yuin/goldmark.
//
// Basic usage:
//
//     import x.markdown
//
//     html := markdown.to_html('# Hello\n\nWorld')
//
// With GFM extensions:
//
//     md := markdown.new(extensions: markdown.gfm())
//     html := md.convert('| a | b |\n|---|---|\n| 1 | 2 |')
//
// With fine-grained options:
//
//     md := markdown.new(
//         extensions: [markdown.Extension(markdown.footnote()), markdown.typographer()],
//         parser_opts: markdown.ParserOptions{ auto_heading_id: true },
//         renderer_opts: markdown.RendererOptions{ unsafe_: true, xhtml: true },
//     )
//     html := md.convert(source)
//
// Parse to AST and walk:
//
//     doc := md.parse(source)
//     doc.walk(fn (node &markdown.Node) bool {
//         println(node.kind)
//         return true
//     })
module markdown

// ParserOptions configures parser behaviour.
@[params]
pub struct ParserOptions {
pub mut:
	// auto_heading_id generates an id attribute for every heading node
	// derived from the heading text content (goldmark WithAutoHeadingID).
	auto_heading_id bool
}

// RendererOptions configures HTML renderer behaviour.
@[params]
pub struct RendererOptions {
pub mut:
	// unsafe_ allows raw HTML from the source to be included in the output.
	// When false (the default) raw HTML is replaced with an HTML comment.
	unsafe_ bool
	// hard_wraps converts every newline inside a paragraph to a <br> tag.
	hard_wraps bool
	// xhtml outputs XHTML-style self-closing tags (e.g. <br />).
	xhtml bool
}

// Options configures a Markdown processor.
// Extension flags in the mut section are normally set by calling new() with
// an extensions slice; they can also be set directly.
@[params]
pub struct Options {
pub mut:
	// extensions is the list of extensions applied when new() is called.
	extensions []Extension
	// parser_opts configures the parser.
	parser_opts ParserOptions
	// renderer_opts configures the renderer.
	renderer_opts RendererOptions
	// --- feature flags set by extensions ---
	tables          bool
	strikethrough   bool
	linkify         bool
	task_list       bool
	footnotes       bool
	typographer     bool
	definition_list bool
}

// LinkRef holds a collected link reference definition (url + optional title).
struct LinkRef {
	dest  string
	title string
}

// Markdown is the main markdown processor.  Create one with new() and reuse it
// across multiple convert/parse calls; link reference definitions are cached.
pub struct Markdown {
pub mut:
	opts    Options
	ref_map map[string]LinkRef
}

// Markdown.new creates a Markdown processor with the given options.
// All extensions in opts.extensions are applied immediately.
pub fn Markdown.new(opts Options) Markdown {
	mut m := Markdown{
		opts:    opts
		ref_map: map[string]LinkRef{}
	}
	for ext in opts.extensions {
		ext.extend(mut m)
	}
	return m
}

// to_html converts the markdown source to HTML with the given options.
pub fn to_html(src string, opts Options) string {
	mut md := Markdown.new(opts)
	return md.convert(src)
}

// convert parses the markdown source and renders it to an HTML string.
pub fn (mut m Markdown) convert(src string) string {
	doc := m.parse(src)
	mut r := HTMLRenderer{
		opts:    m.opts
		ref_map: m.ref_map // Use the updated ref_map after parse()
	}
	return r.render(doc)
}

// parse parses the markdown source into an AST and returns the document root.
// Link reference definitions collected during parsing are cached so that
// subsequent parse/convert calls on the same Markdown instance share them.
pub fn (mut m Markdown) parse(src string) &Node {
	mut p := BlockParser.new(src, m.opts, m.ref_map)
	doc := p.parse()
	for k, v in p.ref_map {
		m.ref_map[k] = v
	}
	return doc
}
