// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.token
import v.errors
import os
import strings

enum State {
	simple // default - no special interpretation of tags, *at all*!
	// That is suitable for the general case of text template interpolation,
	// for example for interpolating arbitrary source code (even V source) templates.
	//
	html // default, only when the template extension is .html
	css  // <style>
	js   // <script>
	// span // span.{
}

fn (mut state State) update(line string) {
	trimmed_line := line.trim_space()
	if is_html_open_tag('style', line) {
		state = .css
	} else if trimmed_line == '</style>' {
		state = .html
	} else if is_html_open_tag('script', line) {
		state = .js
	} else if trimmed_line == '</script>' {
		state = .html
	}
}

const tmpl_str_end = "')\n"

// check HTML open tag `<name attr="x" >`
fn is_html_open_tag(name string, s string) bool {
	trimmed_line := s.trim_space()
	mut len := trimmed_line.len

	if len < name.len {
		return false
	}

	mut sub := trimmed_line[0..1]
	if sub != '<' { // not start with '<'
		return false
	}
	sub = trimmed_line[len - 1..len]
	if sub != '>' { // not end with '<'
		return false
	}
	sub = trimmed_line[len - 2..len - 1]
	if sub == '/' { // self-closing
		return false
	}
	sub = trimmed_line[1..len - 1]
	if sub.contains_any('<>') { // `<name <bad> >`
		return false
	}
	if sub == name { // `<name>`
		return true
	} else {
		len = name.len
		if sub.len <= len { // `<nam>` or `<meme>`
			return false
		}
		if sub[..len + 1] != '${name} ' { // not `<name ...>`
			return false
		}
		return true
	}
}

fn insert_template_code(fn_name string, tmpl_str_start string, line string) string {
	// HTML, may include `@var`
	// escaped by cgen, unless it's a `veb.RawHtml` string
	trailing_bs := tmpl_str_end + 'sb_${fn_name}.write_u8(92)\n' + tmpl_str_start
	replace_pairs := ['\\', '\\\\', r"'", "\\'", r'@@', r'@', r'@', r'$', r'$$', r'\@']
	mut rline := line.replace_each(replace_pairs)
	comptime_call_str := rline.find_between('\${', '}')
	if comptime_call_str.contains("\\'") {
		rline = rline.replace(comptime_call_str, comptime_call_str.replace("\\'", r"'"))
	}
	if rline.ends_with('\\') {
		rline = rline[0..rline.len - 2] + trailing_bs
	}
	return rline
}

// struct to track dependecies and cache templates for reuse without io
struct DependencyCache {
pub mut:
	dependencies map[string][]string
	cache        map[string][]string
}

// custom error to handle issues when including template files
struct IncludeError {
	Error
pub:
	calling_file string
	line_nr      int
	position     int
	col          int
	message      string
}

fn (err IncludeError) msg() string {
	return err.message
}

fn (err IncludeError) line_nr() int {
	return err.line_nr
}

fn (err IncludeError) pos() int {
	return err.position
}

fn (err IncludeError) calling_file() string {
	return err.calling_file
}

fn (err IncludeError) col() int {
	return err.col
}

fn (mut p Parser) process_includes(calling_file string, line_number int, line string, mut dc DependencyCache) ![]string {
	base_path := os.dir(calling_file)
	mut tline_number := line_number
	mut file_name := if line.contains('"') {
		line.split('"')[1]
	} else if line.contains("'") {
		line.split("'")[1]
	} else {
		position := line.index('@include ') or { 0 }
		return &IncludeError{
			calling_file: calling_file
			line_nr:      tline_number // line_number
			position:     position + '@include '.len
			col:          position + '@include '.len
			message:      'path for @include must be quoted with \' or "'
		}
	}
	mut file_ext := os.file_ext(file_name)
	if file_ext == '' {
		file_ext = '.html'
	}
	file_name = file_name.replace(file_ext, '')
	mut file_path := os.real_path(os.join_path_single(base_path, '${file_name}${file_ext}'))

	if !os.exists(file_path) && !file_name.contains('../') {
		// the calling file is probably original way (relative to calling file) and works from the root folder
		path_arr := base_path.split_any('/\\')
		idx := path_arr.index('templates')
		root_path := path_arr[..idx + 1].join('/')
		file_name = file_name.rsplit('../')[0]
		file_path = os.real_path(os.join_path_single(root_path, '${file_name}${file_ext}'))
	}

	// If file hasnt been called before then add to dependency tree
	if !dc.dependencies[file_path].contains(calling_file) {
		dc.dependencies[file_path] << calling_file
	}

	// Circular import detection
	for callee in dc.dependencies[file_path] {
		if dc.dependencies[callee].contains(file_path) {
			return &IncludeError{
				calling_file: calling_file
				line_nr:      tline_number
				position:     line.index('@include ') or { 0 }
				message:      'A recursive call is being made on template ${file_name}'
			}
		}
	}
	mut file_content := []string{}
	if file_path in dc.cache {
		file_content = dc.cache[file_path]
	} else {
		file_content = os.read_lines(file_path) or {
			position := line.index('@include ') or { 0 } + '@include '.len
			return &IncludeError{
				calling_file: calling_file
				line_nr:      tline_number // line_number
				position:     position
				message:      'Reading file `${file_name}` from path: ${file_path} failed'
			}
		}
	}
	// no errors detected in calling file - reset tline_number (error reporting)
	tline_number = 1

	// loop over the imported file
	for i, l in file_content {
		if l.contains('@include ') {
			processed := p.process_includes(file_path, tline_number, l, mut dc) or { return err }
			file_content.delete(i) // remove the include line
			for processed_line in processed.reverse() {
				file_content.insert(i, processed_line)
				tline_number--
			}
		}
	}
	// Add template to parser for reloading
	p.template_paths << file_path
	// Add the imported template to the cache
	dc.cache[file_path] = file_content
	return file_content
}

// compile_file compiles the content of a file by the given path as a template
pub fn (mut p Parser) compile_template_file(template_file string, fn_name string) string {
	mut lines := os.read_lines(template_file) or {
		p.error('reading from ${template_file} failed')
		return ''
	}
	p.template_paths << template_file
	// create a new Dependency tree & cache any templates to avoid further io
	mut dc := DependencyCache{}
	lstartlength := lines.len * 30
	tmpl_str_start := "\tsb_${fn_name}.write_string('"
	mut source := strings.new_builder(1000)
	source.writeln('
import strings
// === veb html template for file: ${template_file} ===
fn veb_tmpl_${fn_name}() string {
	mut sb_${fn_name} := strings.new_builder(${lstartlength})\n

')
	source.write_string(tmpl_str_start)

	mut state := State.simple
	template_ext := os.file_ext(template_file)
	if template_ext.to_lower_ascii() == '.html' {
		state = .html
	}

	mut in_span := false
	mut end_of_line_pos := 0
	mut start_of_line_pos := 0
	mut tline_number := -1 // keep the original line numbers, even after insert/delete ops on lines; `i` changes
	for i := 0; i < lines.len; i++ {
		line := lines[i]
		tline_number++
		start_of_line_pos = end_of_line_pos
		end_of_line_pos += line.len + 1
		if state != .simple {
			state.update(line)
		}
		$if trace_tmpl ? {
			eprintln('>>> tfile: ${template_file}, spos: ${start_of_line_pos:6}, epos:${end_of_line_pos:6}, fi: ${tline_number:5}, i: ${i:5}, state: ${state:10}, line: ${line}')
		}
		if line.contains('@header') {
			position := line.index('@header') or { 0 }
			p.error_with_error(errors.Error{
				message:   "Please use @include 'header' instead of @header (deprecated)"
				file_path: template_file
				pos:       token.Pos{
					len:       '@header'.len
					line_nr:   tline_number
					pos:       start_of_line_pos + position
					last_line: lines.len
				}
				reporter:  .parser
			})
			continue
		}
		if line.contains('@footer') {
			position := line.index('@footer') or { 0 }
			p.error_with_error(errors.Error{
				message:   "Please use @include 'footer' instead of @footer (deprecated)"
				file_path: template_file
				pos:       token.Pos{
					len:       '@footer'.len
					line_nr:   tline_number
					pos:       start_of_line_pos + position
					last_line: lines.len
				}
				reporter:  .parser
			})
			continue
		}
		if line.contains('@include ') {
			lines.delete(i)
			resolved := p.process_includes(template_file, tline_number, line, mut &dc) or {
				if err is IncludeError {
					p.error_with_error(errors.Error{
						message:   err.msg()
						file_path: err.calling_file()
						pos:       token.Pos{
							len:       '@include '.len
							line_nr:   err.line_nr()
							pos:       start_of_line_pos + err.pos()
							col:       err.col()
							last_line: lines.len
						}
						reporter:  .parser
					})
					[]string{}
				} else {
					p.error_with_error(errors.Error{
						message:   'An unknown error has occurred'
						file_path: template_file
						pos:       token.Pos{
							len:       '@include '.len
							line_nr:   tline_number
							pos:       start_of_line_pos
							last_line: lines.len
						}
						reporter:  .parser
					})
					[]string{}
				}
			}
			for resolved_line in resolved.reverse() {
				tline_number--
				lines.insert(i, resolved_line)
			}

			i--
			continue
		}
		if line.contains('@if ') {
			source.writeln(tmpl_str_end)
			pos := line.index('@if') or { continue }
			source.writeln('if ' + line[pos + 4..] + '{')
			source.write_string(tmpl_str_start)
			continue
		}
		if line.contains('@end') {
			source.writeln(tmpl_str_end)
			source.writeln('}')
			source.write_string(tmpl_str_start)
			continue
		}
		if line.contains('@else') {
			source.writeln(tmpl_str_end)
			pos := line.index('@else') or { continue }
			source.writeln('}' + line[pos + 1..] + '{')
			// source.writeln(' } else { ')
			source.write_string(tmpl_str_start)
			continue
		}
		if line.contains('@for') {
			source.writeln(tmpl_str_end)
			pos := line.index('@for') or { continue }
			source.writeln('for ' + line[pos + 4..] + '{')
			source.write_string(tmpl_str_start)
			continue
		}
		if state == .simple {
			// by default, just copy 1:1
			source.writeln(insert_template_code(fn_name, tmpl_str_start, line))
			continue
		}
		// in_write = false
		// The .simple mode ends here. The rest handles .html/.css/.js state transitions.

		if state != .simple {
			if line.contains('@js ') {
				pos := line.index('@js') or { continue }
				source.write_string('<script src="')
				source.write_string(line[pos + 5..line.len - 1])
				source.writeln('"></script>')
				continue
			}
			if line.contains('@css ') {
				pos := line.index('@css') or { continue }
				source.write_string('<link href="')
				source.write_string(line[pos + 6..line.len - 1])
				source.writeln('" rel="stylesheet" type="text/css">')
				continue
			}
		}

		match state {
			.html {
				line_t := line.trim_space()
				if line_t.starts_with('span.') && line.ends_with('{') {
					// `span.header {` => `<span class='header'>`
					class := line.find_between('span.', '{').trim_space()
					source.writeln('<span class="${class}">')
					in_span = true
					continue
				} else if line_t.starts_with('.') && line.ends_with('{') {
					// `.header {` => `<div class='header'>`
					class := line.find_between('.', '{').trim_space()
					trimmed := line.trim_space()
					source.write_string(strings.repeat(`\t`, line.len - trimmed.len)) // add the necessary indent to keep <div><div><div> code clean
					source.writeln('<div class="${class}">')
					continue
				} else if line_t.starts_with('#') && line.ends_with('{') {
					// `#header {` => `<div id='header'>`
					class := line.find_between('#', '{').trim_space()
					source.writeln('<div id="${class}">')
					continue
				} else if line_t == '}' {
					source.write_string(strings.repeat(`\t`, line.len - line_t.len)) // add the necessary indent to keep <div><div><div> code clean
					if in_span {
						source.writeln('</span>')
						in_span = false
					} else {
						source.writeln('</div>')
					}
					continue
				}
			}
			.js {
				// if line.contains('//V_TEMPLATE') {
				source.writeln(insert_template_code(fn_name, tmpl_str_start, line))
				//} else {
				// replace `$` to `\$` at first to escape JavaScript template literal syntax
				// source.writeln(line.replace(r'$', r'\$').replace(r'$$', r'@').replace(r'.$',
				// r'.@').replace(r"'", r"\'"))
				//}
				continue
			}
			.css {
				// disable template variable declaration in inline stylesheet
				// because of  some CSS rules prefixed with `@`.
				source.writeln(line.replace(r'.$', r'.@').replace(r"'", r"\'"))
				continue
			}
			else {}
		}

		if pos := line.index('%') {
			// %translation_key => ${tr('translation_key')}
			mut line_ := line
			if pos + 1 < line.len && line[pos + 1].is_letter() { //|| line[pos + 1] == '_' {
				mut end := pos + 1
				for end < line.len && (line[end].is_letter() || line[end] == `_`) {
					end++
				}
				key := line[pos + 1..end]
				// println('GOT tr key line="${line}" key="${key}"')
				// source.writeln('\${tr("${key}")}')
				line_ = line.replace('%${key}', '\${veb.tr(ctx.lang.str(), "${key}")}')
				// i += key.len
			}
			// println(source.str())
			source.writeln(insert_template_code(fn_name, tmpl_str_start, line_))
			// exit(0)
		} else {
			// by default, just copy 1:1
			source.writeln(insert_template_code(fn_name, tmpl_str_start, line))
		}
	}

	source.writeln(tmpl_str_end)
	source.writeln('\t_tmpl_res_${fn_name} := sb_${fn_name}.str() ')
	source.writeln('\treturn _tmpl_res_${fn_name}')
	source.writeln('}')
	source.writeln('// === end of veb html template_file: ${template_file} ===')

	mut result := source.str()
	if result.contains('veb.') {
		result = 'import veb\n' + result
	}
	$if trace_tmpl_expansion ? {
		eprintln('>>>>>>> template expanded to:')
		eprintln(result)
		eprintln('-----------------------------')
	}

	return result
}
