module main

import document as doc

@[inline]
fn slug(title string) string {
	return title.replace(' ', '-')
}

fn escape(str string) string {
	return str.replace_each(['"', '\\"', '\r\n', '\\n', '\n', '\\n', '\t', '\\t'])
}

fn get_sym_name(dn doc.DocNode) string {
	if dn.is_readme {
		if title := dn.frontmatter['title'] {
			return title
		}
	}
	if dn.parent_name.len > 0 && dn.parent_name != 'void' {
		return '(${dn.parent_name}) ${dn.name}'
	}
	return dn.name
}

fn get_node_id(dn doc.DocNode) string {
	tag := if dn.parent_name.len > 0 && dn.parent_name != 'void' {
		'${dn.parent_name}.${dn.name}'
	} else {
		dn.name
	}
	return slug(tag)
}

// attribute_line_count returns how many lines at the start of a declaration
// are attributes, like `@[inline]`.
fn attribute_line_count(lines []string) int {
	mut count := 0
	for count < lines.len && lines[count].starts_with('@[') {
		count++
	}
	return count
}

// without_pub_keyword removes `pub ` from the declaration line of `content`.
// The attribute lines before the declaration are kept.
fn without_pub_keyword(content string) string {
	mut lines := content.split('\n')
	decl := attribute_line_count(lines)
	if decl < lines.len && lines[decl].starts_with('pub ') {
		lines[decl] = lines[decl]['pub '.len..]
	}
	return lines.join('\n')
}

// without_pub_keywords removes `pub ` from each declaration in a group of constants, like
// `pub const a = 1\npub const b = 2`.
fn without_pub_keywords(content string) string {
	return content.split('\n').map(if it.starts_with('pub const ') { it['pub '.len..] } else { it }).join('\n')
}

// has_own_comments reports whether the variant `variant` of the symbol `symbol` has a doc comment
// that is not the one of `symbol`, so that it is shown with the variant.
fn has_own_comments(variant doc.DocNode, symbol doc.DocNode) bool {
	return variant.comments.len > 0
		&& variant.merge_comments_without_examples() != symbol.merge_comments_without_examples()
}

// has_variant_comments reports whether a variant of the symbol `symbol` has its own doc comment.
// Then each declaration of the symbol is shown with its comment, instead of one comment after
// all of them.
fn has_variant_comments(symbol doc.DocNode) bool {
	return symbol.variants.any(has_own_comments(it, symbol))
}

// without_attribute_lines removes the attribute lines from the start of `content`.
fn without_attribute_lines(content string) string {
	if !content.starts_with('@[') {
		return content
	}
	lines := content.split('\n')
	return lines[attribute_line_count(lines)..].join('\n')
}

// is_deprecated_attribute reports whether `tag` is a `@[deprecated]` or a
// `@[deprecated_after]` attribute.
fn is_deprecated_attribute(tag string) bool {
	return tag.trim_string_left('@[').starts_with('deprecated')
}

fn is_module_readme(dn doc.DocNode) bool {
	return dn.is_readme || (dn.comments.len > 0 && dn.content == 'module ${dn.name}')
}

// trim_doc_node_description returns the nodes trimmed description.
// An example use are the descriptions of the search results in the sidebar.
fn trim_doc_node_description(mod_name string, desc string) string {
	mut dn_desc := desc.replace_each(['\r\n', '\n', '"', '\\"'])
	// Get the first "descriptive" line.
	if dn_desc.starts_with('#') {
		// Handle module READMEs.
		for l in dn_desc.split_into_lines()[1..] {
			if l != '' && !l.starts_with('#') {
				quoted_mod_name := '`${mod_name}`'
				if l.starts_with(quoted_mod_name) {
					// Omit the module name in the description as it is redundant since the name is displayed as well.
					// "`arrays` is a module that..." -> "is a module that..."
					dn_desc = l.all_after(quoted_mod_name).trim_left(' ')
				} else {
					dn_desc = l
				}
				break
			}
		}
	} else {
		dn_desc = dn_desc.all_before('\n')
	}
	// 80 is enough to fill one line.
	if dn_desc.len > 80 {
		dn_desc = dn_desc[..80]
	}
	// If `\` is the last character, it ends with `\"` which leads to a JS error.
	return dn_desc.trim_string_right('\\')
}

fn set_output_type_from_str(format string) OutputType {
	return match format {
		'htm', 'html' { OutputType.html }
		'md', 'markdown' { .markdown }
		'json' { .json }
		'text' { .plaintext }
		'none' { .none }
		else { .ansi }
	}
}

fn gen_footer_text(d &doc.Doc, include_timestamp bool) string {
	footer_text := 'Powered by vdoc.'
	if !include_timestamp {
		return footer_text
	}
	generated_time := d.time_generated
	time_str := '${generated_time.day} ${generated_time.smonth()} ${generated_time.year} ${generated_time.hhmmss()}'
	return '${footer_text} Generated on: ${time_str}'
}
