// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module tmpl

import os
import strings

const (
	str_start = "sb.write(\'"
	str_end   = "\' ) "
)

// compile_file compiles the content of a file by the given path as a template
pub fn compile_file(path, fn_name string) string {
	mut html := os.read_file(path) or {
		panic('html failed')
	}
	return compile_template(html, fn_name)
}

enum State {
	html
	css // <style>
	js  // <script>
	//span // span.{
}

pub fn compile_template(html_, fn_name string) string {
	// lines := os.read_lines(path)
	mut html := html_.trim_space()
	mut lines := html.split_into_lines()
	mut s := strings.new_builder(1000)
	// base := path.all_after_last('/').replace('.html', '')
	s.writeln("
import strings
// === vweb html template ===
fn vweb_tmpl_${fn_name}() {
mut sb := strings.new_builder(${lines.len * 30})\n
header := \' \' // TODO remove
_ = header

")
	s.writeln(str_start)
	mut state := State.html
	mut in_span := false
	//for _line in lines {
	for i := 0; i < lines.len; i ++ {
		line := lines[i].trim_space()
		if line == '<style>' {
			state = .css
		} else if line == '</style>' {
			state = .html
		}
		else if line == '<script>' {
			state = .js
		}
		else if line == '</script>' {
			state = .html
		}
		if line.contains('@include ') {
			// TODO
			pos := line.index('@include ') or {
				continue
			}
			mut file_name := line[pos + 9..]
			file_path := os.join_path('templates', '${file_name}.html')
			mut file_content := os.read_file(file_path) or {
				panic('reading file $file_name failed')
			}
			file_content = file_content.replace("\'", '"')
			include_file_lines := file_content.split_into_lines()
			lines_before := lines[0 .. i].clone()
			lines_after := lines[i + 1 .. lines.len].clone()
			lines = lines_before
			lines << include_file_lines
			lines << lines_after
			i--
			continue
		} else if line.contains('@js ') {
			pos := line.index('@js') or {
				continue
			}
			s.write('<script src=') // " is inserted in the template
			s.write(line[pos + 4..])
			s.writeln('></script>')
		} else if line.contains('@css ') {
			pos := line.index('@css') or {
				continue
			}
			s.write('<link href=')
			s.write(line[pos + 4..])
			s.writeln(' rel="stylesheet" type="text/css">')
		} else if line.contains('@if ') {
			s.writeln(str_end)
			pos := line.index('@if') or {
				continue
			}
			s.writeln('if ' + line[pos + 4..] + '{')
			s.writeln(str_start)
		} else if line.contains('@end') {
			s.writeln(str_end)
			s.writeln('}')
			s.writeln(str_start)
		} else if line.contains('@else') {
			s.writeln(str_end)
			s.writeln(' } else { ')
			s.writeln(str_start)
		} else if line.contains('@for') {
			s.writeln(str_end)
			pos := line.index('@for') or {
				continue
			}
			s.writeln('for ' + line[pos + 4..] + '{')
			s.writeln(str_start)
		} else if state == .html && line.contains('span.') && line.ends_with('{') {
			// `span.header {` => `<span class='header'>`
			class := line.find_between('span.', '{').trim_space()
			s.writeln('<span class="$class">')
			in_span  = true
		} else if state == .html && line.contains('.') && line.ends_with('{') {
			// `.header {` => `<div class='header'>`
			class := line.find_between('.', '{').trim_space()
			s.writeln('<div class="$class">')
		} else if state == .html && line.contains('#') && line.ends_with('{') {
			// `#header {` => `<div id='header'>`
			class := line.find_between('#', '{').trim_space()
			s.writeln('<div id="$class">')
		} else if state == .html && line == '}' {
			if in_span {
				s.writeln('</span>')
				in_span = false
			} else {
				s.writeln('</div>')
			}
		} else {
			// HTML, may include `@var`
			s.writeln(line.replace('@', '\x24').replace("'", '"'))
		}
	}
	s.writeln(str_end)
	s.writeln('_tmpl_res_$fn_name := sb.str() ')
	s.writeln('}')
	s.writeln('// === end of vweb html template ===')
	return s.str()
}
