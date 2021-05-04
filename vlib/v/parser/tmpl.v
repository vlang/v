// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.token
import v.errors
import os
import strings

const tmpl_str_start = "sb.write_string('"

const tmpl_str_end = "' ) "

enum State {
	html
	css // <style>
	js // <script>
	// span // span.{
}

// check HTML open tag `<name attr="x" >`
fn is_html_open_tag(name string, s string) bool {
	mut len := s.len
	if len < name.len {
		return false
	}
	mut sub := s[0..1]
	if sub != '<' { // not start with '<'
		return false
	}
	sub = s[len - 1..len]
	if sub != '>' { // not end with '<'
		return false
	}
	sub = s[len - 2..len - 1]
	if sub == '/' { // self-closing
		return false
	}
	sub = s[1..len - 1]
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
		if sub[..len + 1] != '$name ' { // not `<name ...>`
			return false
		}
		return true
	}
}

// compile_file compiles the content of a file by the given path as a template
pub fn (mut p Parser) compile_template_file(template_file string, fn_name string) string {
	mut lines := os.read_lines(template_file) or {
		p.error('reading from $template_file failed')
		return ''
	}
	basepath := os.dir(template_file)
	lstartlength := lines.len * 30
	mut source := strings.new_builder(1000)
	source.writeln('
import strings
// === vweb html template ===
fn vweb_tmpl_${fn_name}() {
mut sb := strings.new_builder($lstartlength)\n

')
	source.write_string(parser.tmpl_str_start)
	mut state := State.html
	mut in_span := false
	mut end_of_line_pos := 0
	mut start_of_line_pos := 0
	mut tline_number := -1 // keep the original line numbers, even after insert/delete ops on lines; `i` changes
	for i := 0; i < lines.len; i++ {
		oline := lines[i]
		tline_number++
		start_of_line_pos = end_of_line_pos
		end_of_line_pos += oline.len + 1
		$if trace_tmpl ? {
			eprintln('>>> tfile: $template_file, spos: ${start_of_line_pos:6}, epos:${end_of_line_pos:6}, fi: ${tline_number:5}, i: ${i:5}, line: $oline')
		}
		line := oline.trim_space()
		if is_html_open_tag('style', line) {
			state = .css
		} else if line == '</style>' {
			state = .html
		} else if is_html_open_tag('script', line) {
			state = .js
		} else if line == '</script>' {
			state = .html
		}
		if line.contains('@header') {
			position := line.index('@header') or { 0 }
			p.error_with_error(errors.Error{
				message: "Please use @include 'header' instead of @header (deprecated)"
				file_path: template_file
				pos: token.Position{
					len: '@header'.len
					line_nr: tline_number
					pos: start_of_line_pos + position
					last_line: lines.len
				}
				reporter: .parser
			})
		} else if line.contains('@footer') {
			position := line.index('@footer') or { 0 }
			p.error_with_error(errors.Error{
				message: "Please use @include 'footer' instead of @footer (deprecated)"
				file_path: template_file
				pos: token.Position{
					len: '@footer'.len
					line_nr: tline_number
					pos: start_of_line_pos + position
					last_line: lines.len
				}
				reporter: .parser
			})
		}
		if line.contains('@include ') {
			lines.delete(i)
			mut file_name := line.split("'")[1]
			mut file_ext := os.file_ext(file_name)
			if file_ext == '' {
				file_ext = '.html'
			}
			file_name = file_name.replace(file_ext, '')
			// relative path, starting with the current folder
			mut templates_folder := os.real_path(basepath)
			if file_name.contains('/') && file_name.starts_with('/') {
				// an absolute path
				templates_folder = ''
			}
			file_path := os.real_path(os.join_path(templates_folder, '$file_name$file_ext'))
			$if trace_tmpl ? {
				eprintln('>>> basepath: "$basepath" , template_file: "$template_file" , fn_name: "$fn_name" , @include line: "$line" , file_name: "$file_name" , file_ext: "$file_ext" , templates_folder: "$templates_folder" , file_path: "$file_path"')
			}
			file_content := os.read_file(file_path) or {
				position := line.index('@include ') or { 0 } + '@include '.len
				p.error_with_error(errors.Error{
					message: 'Reading file $file_name from path: $file_path failed'
					details: "Failed to @include '$file_name'"
					file_path: template_file
					pos: token.Position{
						len: '@include '.len + file_name.len
						line_nr: tline_number
						pos: start_of_line_pos + position
						last_line: lines.len
					}
					reporter: .parser
				})
				''
			}
			file_splitted := file_content.split_into_lines().reverse()
			for f in file_splitted {
				tline_number--
				lines.insert(i, f)
			}
			i--
		} else if line.contains('@js ') {
			pos := line.index('@js') or { continue }
			source.write_string('<script src="')
			source.write_string(line[pos + 5..line.len - 1])
			source.writeln('"></script>')
		} else if line.contains('@css ') {
			pos := line.index('@css') or { continue }
			source.write_string('<link href="')
			source.write_string(line[pos + 6..line.len - 1])
			source.writeln('" rel="stylesheet" type="text/css">')
		} else if line.contains('@if ') {
			source.writeln(parser.tmpl_str_end)
			pos := line.index('@if') or { continue }
			source.writeln('if ' + line[pos + 4..] + '{')
			source.writeln(parser.tmpl_str_start)
		} else if line.contains('@end') {
			// Remove new line byte
			source.go_back(1)
			source.writeln(parser.tmpl_str_end)
			source.writeln('}')
			source.writeln(parser.tmpl_str_start)
		} else if line.contains('@else') {
			// Remove new line byte
			source.go_back(1)
			source.writeln(parser.tmpl_str_end)
			source.writeln(' } else { ')
			source.writeln(parser.tmpl_str_start)
		} else if line.contains('@for') {
			source.writeln(parser.tmpl_str_end)
			pos := line.index('@for') or { continue }
			source.writeln('for ' + line[pos + 4..] + '{')
			source.writeln(parser.tmpl_str_start)
		} else if state == .html && line.contains('span.') && line.ends_with('{') {
			// `span.header {` => `<span class='header'>`
			class := line.find_between('span.', '{').trim_space()
			source.writeln('<span class="$class">')
			in_span = true
		} else if state == .html && line.contains('.') && line.ends_with('{') {
			// `.header {` => `<div class='header'>`
			class := line.find_between('.', '{').trim_space()
			source.writeln('<div class="$class">')
		} else if state == .html && line.contains('#') && line.ends_with('{') {
			// `#header {` => `<div id='header'>`
			class := line.find_between('#', '{').trim_space()
			source.writeln('<div id="$class">')
		} else if state == .html && line == '}' {
			if in_span {
				source.writeln('</span>')
				in_span = false
			} else {
				source.writeln('</div>')
			}
		} else if state == .js {
			// replace `$` to `\$` at first to escape JavaScript template literal syntax
			source.writeln(line.replace(r'$', r'\$').replace(r'$$', r'@').replace(r'.$',
				r'.@').replace(r"'", r"\'"))
		} else if state == .css {
			// disable template variable declaration in inline stylesheet
			// because of  some CSS rules prefixed with `@`.
			source.writeln(line.replace(r'.$', r'.@').replace(r"'", r"\'"))
		} else {
			// HTML, may include `@var`
			// escaped by cgen, unless it's a `vweb.RawHtml` string
			source.writeln(line.replace(r'@', r'$').replace(r'$$', r'@').replace(r'.$',
				r'.@').replace(r"'", r"\'"))
		}
	}
	source.writeln(parser.tmpl_str_end)
	source.writeln('_tmpl_res_$fn_name := sb.str() ')
	source.writeln('}')
	source.writeln('// === end of vweb html template ===')
	result := source.str()
	return result
}
