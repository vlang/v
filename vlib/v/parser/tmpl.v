// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
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
	css // <style>
	js // <script>
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
		if sub[..len + 1] != '$name ' { // not `<name ...>`
			return false
		}
		return true
	}
}

fn insert_template_code(fn_name string, tmpl_str_start string, line string) string {
	// HTML, may include `@var`
	// escaped by cgen, unless it's a `vweb.RawHtml` string
	trailing_bs := parser.tmpl_str_end + 'sb_${fn_name}.write_u8(92)\n' + tmpl_str_start
	round1 := ['\\', '\\\\', r"'", "\\'", r'@', r'$']
	round2 := [r'$$', r'\@', r'.$', r'.@']
	mut rline := line.replace_each(round1).replace_each(round2)
	comptime_call_str := rline.find_between('\${', '}')
	if comptime_call_str.contains("\\'") {
		rline = rline.replace(comptime_call_str, comptime_call_str.replace("\\'", r"'"))
	}
	if rline.ends_with('\\') {
		rline = rline[0..rline.len - 2] + trailing_bs
	}

	return rline
}

// compile_file compiles the content of a file by the given path as a template
pub fn (mut p Parser) compile_template_file(template_file string, fn_name string) string {
	mut lines := os.read_lines(template_file) or {
		p.error('reading from $template_file failed')
		return ''
	}
	basepath := os.dir(template_file)
	lstartlength := lines.len * 30
	tmpl_str_start := "\tsb_${fn_name}.write_string('"
	mut source := strings.new_builder(1000)
	source.writeln('
import strings
// === vweb html template ===
fn vweb_tmpl_${fn_name}() string {
	mut sb_$fn_name := strings.new_builder($lstartlength)\n

')
	source.write_string(tmpl_str_start)
	//
	mut state := State.simple
	template_ext := os.file_ext(template_file)
	if template_ext.to_lower() == '.html' {
		state = .html
	}
	//
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
			eprintln('>>> tfile: $template_file, spos: ${start_of_line_pos:6}, epos:${end_of_line_pos:6}, fi: ${tline_number:5}, i: ${i:5}, state: ${state:10}, line: $line')
		}
		if line.contains('@header') {
			position := line.index('@header') or { 0 }
			p.error_with_error(errors.Error{
				message: "Please use @include 'header' instead of @header (deprecated)"
				file_path: template_file
				pos: token.Pos{
					len: '@header'.len
					line_nr: tline_number
					pos: start_of_line_pos + position
					last_line: lines.len
				}
				reporter: .parser
			})
			continue
		}
		if line.contains('@footer') {
			position := line.index('@footer') or { 0 }
			p.error_with_error(errors.Error{
				message: "Please use @include 'footer' instead of @footer (deprecated)"
				file_path: template_file
				pos: token.Pos{
					len: '@footer'.len
					line_nr: tline_number
					pos: start_of_line_pos + position
					last_line: lines.len
				}
				reporter: .parser
			})
			continue
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
			file_path := os.real_path(os.join_path_single(templates_folder, '$file_name$file_ext'))
			$if trace_tmpl ? {
				eprintln('>>> basepath: "$basepath" , template_file: "$template_file" , fn_name: "$fn_name" , @include line: "$line" , file_name: "$file_name" , file_ext: "$file_ext" , templates_folder: "$templates_folder" , file_path: "$file_path"')
			}
			file_content := os.read_file(file_path) or {
				position := line.index('@include ') or { 0 } + '@include '.len
				p.error_with_error(errors.Error{
					message: 'Reading file $file_name from path: $file_path failed'
					details: "Failed to @include '$file_name'"
					file_path: template_file
					pos: token.Pos{
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
			continue
		}
		if line.contains('@if ') {
			source.writeln(parser.tmpl_str_end)
			pos := line.index('@if') or { continue }
			source.writeln('if ' + line[pos + 4..] + '{')
			source.writeln(tmpl_str_start)
			continue
		}
		if line.contains('@end') {
			// Remove new line byte
			source.go_back(1)
			source.writeln(parser.tmpl_str_end)
			source.writeln('}')
			source.writeln(tmpl_str_start)
			continue
		}
		if line.contains('@else') {
			// Remove new line byte
			source.go_back(1)
			source.writeln(parser.tmpl_str_end)
			source.writeln(' } else { ')
			source.writeln(tmpl_str_start)
			continue
		}
		if line.contains('@for') {
			source.writeln(parser.tmpl_str_end)
			pos := line.index('@for') or { continue }
			source.writeln('for ' + line[pos + 4..] + '{')
			source.writeln(tmpl_str_start)
			continue
		}
		if state == .simple {
			// by default, just copy 1:1
			source.writeln(insert_template_code(fn_name, tmpl_str_start, line))
			continue
		}
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
					source.writeln('<span class="$class">')
					in_span = true
					continue
				} else if line_t.starts_with('.') && line.ends_with('{') {
					// `.header {` => `<div class='header'>`
					class := line.find_between('.', '{').trim_space()
					trimmed := line.trim_space()
					source.write_string(strings.repeat(`\t`, line.len - trimmed.len)) // add the necessary indent to keep <div><div><div> code clean
					source.writeln('<div class="$class">')
					continue
				} else if line_t.starts_with('#') && line.ends_with('{') {
					// `#header {` => `<div id='header'>`
					class := line.find_between('#', '{').trim_space()
					source.writeln('<div id="$class">')
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
		// by default, just copy 1:1
		source.writeln(insert_template_code(fn_name, tmpl_str_start, line))
	}

	source.writeln(parser.tmpl_str_end)
	source.writeln('\t_tmpl_res_$fn_name := sb_${fn_name}.str() ')
	source.writeln('\treturn _tmpl_res_$fn_name')
	source.writeln('}')
	source.writeln('// === end of vweb html template_file: $template_file ===')

	result := source.str()
	$if trace_tmpl_expansion ? {
		eprintln('>>>>>>> template expanded to:')
		eprintln(result)
		eprintln('-----------------------------')
	}

	return result
}
