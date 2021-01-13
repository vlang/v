// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module tmpl

import os
import strings

const (
	str_start = "sb.write('"
	str_end   = "' ) "
)

// compile_file compiles the content of a file by the given path as a template
pub fn compile_file(path string, fn_name string) string {
	html := os.read_file(path) or { panic('html failed') }
	return compile_template(html, fn_name)
}

enum State {
	html
	css // <style>
	js // <script>
	// span // span.{
}

pub fn compile_template(html_ string, fn_name string) string {
	// lines := os.read_lines(path)
	mut html := html_.trim_space()
	mut header := ''
	mut footer := ''
	if os.exists('templates/header.html') && html.contains('@header') {
		h := os.read_file('templates/header.html') or {
			panic('reading file templates/header.html failed')
		}
		header = h.trim_space().replace("'", '"')
		html = header + html
	}
	if os.exists('templates/footer.html') && html.contains('@footer') {
		f := os.read_file('templates/footer.html') or {
			panic('reading file templates/footer.html failed')
		}
		footer = f.trim_space().replace("'", '"')
		html += footer
	}
	mut lines := html.split_into_lines()
	lstartlength := lines.len * 30
	mut s := strings.new_builder(1000)
	// base := path.all_after_last('/').replace('.html', '')
	s.writeln("
import strings
// === vweb html template ===
fn vweb_tmpl_${fn_name}() {
mut sb := strings.new_builder($lstartlength)\n
header := \' \' // TODO remove
_ = header
footer := \' \' // TODO remove
_ = footer

")
	s.write(str_start)
	mut state := State.html
	mut in_span := false
	// for _line in lines {
	for i := 0; i < lines.len; i++ {
		line := lines[i].trim_space()
		if line == '<style>' {
			state = .css
		} else if line == '</style>' {
			state = .html
		} else if line == '<script>' {
			state = .js
		} else if line == '</script>' {
			state = .html
		}
		if line.contains('@include ') && false {
			// TODO
			pos := line.index('@include ') or { continue }
			file_name := line[pos + 9..]
			file_path := os.join_path('templates', '${file_name}.html')
			mut file_content := os.read_file(file_path) or {
				panic('reading file $file_name failed')
			}
			file_content = file_content.replace("'", '"')
			lines2 := file_content.split_into_lines()
			for l in lines2 {
				lines.insert(i + 1, l)
			}
			continue
			// s.writeln(file_content)
		} else if line.contains('@js ') {
			pos := line.index('@js') or { continue }
			s.write('<script src="')
			s.write(line[pos + 5..line.len - 1])
			s.writeln('"></script>')
		} else if line.contains('@css ') {
			pos := line.index('@css') or { continue }
			s.write('<link href="')
			s.write(line[pos + 6..line.len - 1])
			s.writeln('" rel="stylesheet" type="text/css">')
		} else if line.contains('@if ') {
			s.writeln(str_end)
			pos := line.index('@if') or { continue }
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
			pos := line.index('@for') or { continue }
			s.writeln('for ' + line[pos + 4..] + '{')
			s.writeln(str_start)
		} else if state == .html && line.contains('span.') && line.ends_with('{') {
			// `span.header {` => `<span class='header'>`
			class := line.find_between('span.', '{').trim_space()
			s.writeln('<span class="$class">')
			in_span = true
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
			// escaped by cgen, unless it's a `vweb.RawHtml` string
			s.writeln(line.replace('@', '$').replace("'", '"'))
		}
	}
	s.writeln(str_end)
	s.writeln('_tmpl_res_$fn_name := sb.str() ')
	s.writeln('}')
	s.writeln('// === end of vweb html template ===')
	return s.str()
}
