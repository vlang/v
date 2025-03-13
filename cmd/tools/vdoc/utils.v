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
