// Copyright 2026 The V Language. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

# vlib/x/markdown - Markdown Parser and HTML Renderer

A CommonMark-compliant Markdown parser and HTML renderer for V, with support for GitHub Flavored Markdown (GFM) extensions. Designed for feature parity with [github.com/yuin/goldmark](https://github.com/yuin/goldmark).

## Features

### CommonMark Support
- **Block-level elements**: headings (ATX and setext), paragraphs, blockquotes,
  lists (bullet and ordered), code blocks (indented and fenced), HTML blocks,
  thematic breaks
- **Inline elements**: emphasis (*em* and **strong**), code spans, links
  (inline and reference), images, autolinks, hard/soft line breaks, HTML
  entities, raw HTML
- **Link reference definitions** for DRY Markdown

### GFM Extensions (via `.gfm()` helper or individual extensions)
- **Tables**: `| col | col |` with alignment (`:--`, `:--:`, `--:`)
- **Strikethrough**: `~~text~~`
- **Task lists**: `- [ ] todo` and `- [x] done`
- **Linkify**: bare URLs become links

### Additional Extensions
- **Footnotes**: `[^1]` references and `[^1]: footnote text` definitions
- **Typographer**: smart punctuation (`--` → en-dash, `---` → em-dash,
  `...` → ellipsis, smart quotes)
- **Auto-heading IDs**: automatic `id` attributes on headings from text content
- **Definition lists**: Pandoc-style (requires extension)

## Quick Start

### Basic Usage

```v
import x.markdown

fn main() {
	html := markdown.to_html('# Hello\n\nWorld')
	println(html)
	// Output: <h1>Hello</h1>\n<p>World</p>\n
}
```

### With Extensions

```v oksyntax
mut md := markdown.new(Options{
	extensions: markdown.gfm()
})
html := md.convert('| Name |\n|------|\n| Alice |')
println(html) // Renders as HTML table
```

### Fine-Grained Configuration

```v
import x.markdown

fn main() {
	mut md := markdown.new(markdown.Options{
		extensions:    [markdown.Extension(markdown.footnote()), markdown.typographer()]
		parser_opts:   markdown.ParserOptions{
			auto_heading_id: true
		}
		renderer_opts: markdown.RendererOptions{
			unsafe_: true
			xhtml:   true
		}
	})
	source := '# Title'
	html := md.convert(source)
	println(html)
}
```

### Parse to AST and Walk

```v
import x.markdown

fn main() {
	mut md := markdown.new(markdown.Options{})
	source := '# Hello\n\n`x`'
	doc := md.parse(source)
	doc.walk(fn (node &markdown.Node) bool {
		match node.kind {
			.heading {
				println('Heading level ${node.level}')
			}
			.code_span {
				println('Code: ${node.literal}')
			}
			else {}
		}

		return true
	})
}
```

## API Overview

### Top-Level Functions
- `to_html(src: string) string` - Convert Markdown to HTML with default settings
- `to_html_opts(src: string, opts: Options) string` - Convert with custom options
- `parse_inline(src: string, opts: Options, ref_map: map) []&Node` - Parse inline content only

### Main Structs

#### `Markdown`
The main processor. Create with `new()`, reuse across multiple calls to share link references.

Methods:
- `convert(src: string) string` - Parse and render to HTML in one call
- `parse(src: string) &Node` - Parse to AST only

#### `Options` (@[params])
```v oksyntax
pub struct Options {
pub mut:
	extensions    []Extension
	parser_opts   ParserOptions
	renderer_opts RendererOptions
	// Extension feature flags (set by extensions)
	tables          bool
	strikethrough   bool
	linkify         bool
	task_list       bool
	footnotes       bool
	typographer     bool
	definition_list bool
}
```

#### `ParserOptions` (@[params])
```v oksyntax
pub struct ParserOptions {
pub mut:
	auto_heading_id bool // Generate id from heading text
}
```

#### `RendererOptions` (@[params])
```v oksyntax
pub struct RendererOptions {
pub mut:
	unsafe_    bool // Allow raw HTML (default: false)
	hard_wraps bool // Convert all \n to <br> (default: false)
	xhtml      bool // Output XHTML self-closing tags (default: false)
}
```

#### `Node`
An AST node. Navigate with `.children`, inspect with `.kind`, `.literal`, `.level`, etc.

Methods:
- `text_content() string` - Extract plain text from this node and descendants
- `walk(f: fn(&Node) bool) bool` - Traverse AST pre-order; return false from callback to stop

### Extensions

Available as functions returning extension structs:
- `table()` - GFM tables
- `strikethrough()` - GFM strikethrough
- `linkify()` - Bare URL autolinks
- `task_list()` - GFM task lists
- `footnote()` - Footnote references and definitions
- `typographer()` - Smart punctuation
- `definition_list()` - Pandoc-style definition lists
- `gfm()` - Convenience helper returning `[table(), strikethrough(), linkify(), task_list()]`

## Examples

### Simple Emphasis

```v oksyntax
assert markdown.to_html('*em*').contains('<em>em</em>')
assert markdown.to_html('**strong**').contains('<strong>strong</strong>')
```

### Links and Images

```v oksyntax
// Inline link
html := markdown.to_html('[click](https://example.com)')
// Reference link
html = markdown.to_html('[click][ref]\n\n[ref]: https://example.com')
// Image
html = markdown.to_html('![alt](image.png "title")')
```

### Code Blocks

```v oksyntax
// Indented code
html := markdown.to_html('    code')
// Fenced code
html = markdown.to_html('```v\nfn main() {}\n```')
```

### Lists

```v oksyntax
// Bullet list
html := markdown.to_html('- item 1\n- item 2')
// Ordered list
html = markdown.to_html('1. first\n2. second')
// Task list (enable via extension or task_list option)
html = markdown.to_html_opts('- [x] done', Options{ task_list: true })
```

### Tables (GFM)

```v oksyntax
src := '| Left | Center | Right |\n|:--|:--:|--:|\n| A | B | C |'
html := markdown.to_html_opts(src, Options{ tables: true })
```

### Footnotes

```v oksyntax
src := 'Text[^1]\n\n[^1]: Footnote body.'
html := markdown.to_html_opts(src, Options{ footnotes: true })
// Renders with <sup> reference and footnote section at bottom
```

## Design Notes

### Block Parsing
- Reads source line-by-line, building a block-level AST
- Handles lazy continuation lines for blockquotes and lists
- Collects link reference definitions for inline resolution

### Inline Parsing
- Parses raw text from paragraph/heading/cell nodes using a simple state machine
- Emphasis/strong uses a delimiter-run resolution pass aligned with CommonMark rules
- Backticks, brackets, and HTML are handled specially

### Rendering
- Tree walk via `render_node()` dispatch on `NodeKind`
- Inline nodes parsed on-demand during rendering
- Link references cached in `Markdown` for reuse across multiple convert calls

## Limitations and Known Issues

- Definition list syntax is Pandoc-style; CommonMark does not define this

**Status**: All core features (headings, emphasis, links, code, lists,
blockquotes, task lists, tables, HTML escaping) work reliably without crashes.

## Testing

Run the test suite:

```bash
./vnew -silent test vlib/x/markdown/markdown_test.v
```

Or write your own:

```v oksyntax
import x.markdown

fn test_my_markdown() {
	html := markdown.to_html('# Test')
	assert html == '<h1>Test</h1>\n'
}
```

## Contributing

- Follow V style guidelines (use `./vnew fmt -w` on edits)
- Add tests for new features
- Update documentation for public API changes
- Keep CommonMark compliance as the baseline

## License

MIT, same as V.

## References

- [CommonMark Specification](https://spec.commonmark.org/)
- [GitHub Flavored Markdown](https://github.github.com/gfm/)
- [goldmark (Go implementation)](https://github.com/yuin/goldmark)
