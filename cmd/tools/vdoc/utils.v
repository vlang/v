module main

import os
import v.doc

[inline]
fn slug(title string) string {
	return title.replace(' ', '-')
}

fn escape(str string) string {
	return str.replace_each(['"', '\\"', '\r\n', '\\n', '\n', '\\n', '\t', '\\t'])
}

fn get_sym_name(dn doc.DocNode) string {
	sym_name := if dn.parent_name.len > 0 && dn.parent_name != 'void' {
		'($dn.parent_name) $dn.name'
	} else {
		dn.name
	}
	return sym_name
}

fn get_node_id(dn doc.DocNode) string {
	tag := if dn.parent_name.len > 0 && dn.parent_name != 'void' {
		'${dn.parent_name}.$dn.name'
	} else {
		dn.name
	}
	return slug(tag)
}

fn is_module_readme(dn doc.DocNode) bool {
	if dn.comments.len > 0 && dn.content == 'module $dn.name' {
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
	if '\n' in dn_description {
		dn_description = dn_description.split('\n')[0]
	}
	// if \ is last character, it ends with \" which leads to a JS error
	if dn_description.ends_with('\\') {
		dn_description = dn_description.trim_right('\\')
	}
	return dn_description
}

fn set_output_type_from_str(format string) OutputType {
	output_type := match format {
		'htm', 'html' { OutputType.html }
		'md', 'markdown' { OutputType.markdown }
		'json' { OutputType.json }
		'stdout' { OutputType.stdout }
		else { OutputType.plaintext }
	}
	return output_type
}

fn get_ignore_paths(path string) ?[]string {
	ignore_file_path := os.join_path(path, '.vdocignore')
	ignore_content := os.read_file(ignore_file_path) or {
		return error_with_code('ignore file not found.', 1)
	}
	mut res := []string{}
	if ignore_content.trim_space().len > 0 {
		rules := ignore_content.split_into_lines().map(it.trim_space())
		mut final := []string{}
		for rule in rules {
			if rule.contains('*.') || rule.contains('**') {
				println('vdoc: Wildcards in ignore rules are not allowed for now.')
				continue
			}
			final << rule
		}
		res = final.map(os.join_path(path, it.trim_right('/')))
	} else {
		mut dirs := os.ls(path) or { return []string{} }
		res = dirs.map(os.join_path(path, it)).filter(os.is_dir(it))
	}
	return res.map(it.replace('/', os.path_separator))
}

fn is_included(path string, ignore_paths []string) bool {
	if path.len == 0 {
		return true
	}
	for ignore_path in ignore_paths {
		if ignore_path !in path {
			continue
		}
		return false
	}
	return true
}

fn get_modules_list(path string, ignore_paths2 []string) []string {
	files := os.ls(path) or { return []string{} }
	mut ignore_paths := get_ignore_paths(path) or { []string{} }
	ignore_paths << ignore_paths2
	mut dirs := []string{}
	for file in files {
		fpath := os.join_path(path, file)
		if os.is_dir(fpath) && is_included(fpath, ignore_paths) && !os.is_link(path) {
			dirs << get_modules_list(fpath, ignore_paths.filter(it.starts_with(fpath)))
		} else if fpath.ends_with('.v') && !fpath.ends_with('_test.v') {
			if path in dirs {
				continue
			}
			dirs << path
		}
	}
	dirs.sort()
	return dirs
}

fn gen_footer_text(d &doc.Doc, include_timestamp bool) string {
	footer_text := 'Powered by vdoc.'
	if !include_timestamp {
		return footer_text
	}
	generated_time := d.time_generated
	time_str := '$generated_time.day $generated_time.smonth() $generated_time.year $generated_time.hhmmss()'
	return '$footer_text Generated on: $time_str'
}
