module main

import v.doc

@[inline]
fn slug(title string) string {
	return title.replace(' ', '-')
}

fn escape(str string) string {
	return str.replace_each(['"', '\\"', '\r\n', '\\n', '\n', '\\n', '\t', '\\t'])
}

fn get_sym_name(dn doc.DocNode) string {
	sym_name := if dn.parent_name.len > 0 && dn.parent_name != 'void' {
		'(${dn.parent_name}) ${dn.name}'
	} else {
		dn.name
	}
	return sym_name
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
	if dn.comments.len > 0 && dn.content == 'module ${dn.name}' {
		return true
	}
	return false
}

fn trim_doc_node_description(description string) string {
	mut dn_description := description.replace_each(['\r\n', '\n', '"', '\\"'])
	// 80 is enough to fill one line
	if dn_description.len > 80 {
		dn_description = dn_description[..80]
	}
	if dn_description.contains('\n') {
		dn_description = dn_description.split('\n')[0]
	}
	// if \ is last character, it ends with \" which leads to a JS error
	if dn_description.ends_with('\\') {
		dn_description = dn_description.trim_right('\\')
	}
	return dn_description
}

fn set_output_type_from_str(format string) OutputType {
	return match format {
		'htm', 'html' { OutputType.html }
		'md', 'markdown' { .markdown }
		'json' { .json }
		'text' { .plaintext }
		'ansi' { .ansi }
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
