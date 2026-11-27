module svg

import strings

// WriteOptions controls SVG serialization.
pub struct WriteOptions {
pub:
	minify bool
	indent string = '  '
}

// write_document serializes a document to an SVG string.
pub fn write_document(d Document, opts WriteOptions) string {
	mut root := d.root
	if d.css.len > 0 && !has_style_child(root) {
		mut style_el := elem('style')
		style_el.text = css_to_string(d.css)
		root.children.insert(0, style_el)
	}
	mut sb := strings.new_builder(4096)
	if opts.minify {
		write_element(mut sb, root, 0, opts, true)
	} else {
		sb.write_string('<?xml version="1.0" encoding="UTF-8"?>')
		sb.write_u8(`\n`)
		write_element(mut sb, root, 0, opts, false)
		sb.write_u8(`\n`)
	}
	return sb.str()
}

fn has_style_child(e Element) bool {
	for c in e.children {
		if c.tag == 'style' {
			return true
		}
	}
	return false
}

fn css_to_string(rules []CssRule) string {
	mut sb := strings.new_builder(1024)
	for r in rules {
		sb.write_string(r.selector)
		sb.write_string(' { ')
		sb.write_string(r.style.str())
		sb.write_string(' }')
		sb.write_u8(`\n`)
	}
	return sb.str()
}

// to_svg serializes a single element.
pub fn (e Element) to_svg(opts WriteOptions) string {
	mut sb := strings.new_builder(256)
	write_element(mut sb, e, 0, opts, opts.minify)
	return sb.str()
}

fn write_indent(mut sb strings.Builder, depth int, opts WriteOptions) {
	if opts.minify || opts.indent == '' {
		return
	}
	for _ in 0 .. depth {
		sb.write_string(opts.indent)
	}
}

fn write_element(mut sb strings.Builder, e Element, depth int, opts WriteOptions, minify bool) {
	write_indent(mut sb, depth, opts)
	sb.write_u8(`<`)
	sb.write_string(e.tag)
	for a in e.attrs {
		sb.write_string(' ')
		sb.write_string(a.name)
		sb.write_string('="')
		sb.write_string(escape_attr(a.value))
		sb.write_u8(`"`)
	}
	if e.style.len() > 0 {
		sb.write_string(' style="')
		sb.write_string(escape_attr(e.style.str()))
		sb.write_u8(`"`)
	}
	has_children := e.children.len > 0
	has_text := e.text != ''
	if !has_children && !has_text {
		sb.write_string('/>')
		if !minify {
			sb.write_u8(`\n`)
		}
		return
	}
	sb.write_u8(`>`)
	if !minify {
		sb.write_u8(`\n`)
	}
	if has_text {
		if !minify {
			write_indent(mut sb, depth + 1, opts)
		}
		sb.write_string(escape_text(e.text))
		if !minify {
			sb.write_u8(`\n`)
		}
	}
	for child in e.children {
		write_element(mut sb, child, depth + 1, opts, minify)
	}
	if !minify {
		write_indent(mut sb, depth, opts)
	}
	sb.write_string('</')
	sb.write_string(e.tag)
	sb.write_u8(`>`)
	if !minify {
		sb.write_u8(`\n`)
	}
}

// escape_attr escapes characters that are unsafe inside a double-quoted attribute.
pub fn escape_attr(s string) string {
	mut sb := strings.new_builder(s.len + 8)
	for c in s {
		match c {
			`&` {
				sb.write_string('&amp;')
			}
			`"` {
				sb.write_string('&quot;')
			}
			`<` {
				sb.write_string('&lt;')
			}
			`>` {
				sb.write_string('&gt;')
			}
			else {
				sb.write_u8(c)
			}
		}
	}
	return sb.str()
}

// escape_text escapes text node content.
pub fn escape_text(s string) string {
	mut sb := strings.new_builder(s.len + 8)
	for c in s {
		match c {
			`&` {
				sb.write_string('&amp;')
			}
			`<` {
				sb.write_string('&lt;')
			}
			`>` {
				sb.write_string('&gt;')
			}
			else {
				sb.write_u8(c)
			}
		}
	}
	return sb.str()
}
