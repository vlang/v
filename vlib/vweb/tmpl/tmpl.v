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

pub fn compile_template(content, fn_name string) string {
	// lines := os.read_lines(path)
	mut html := content
	mut header := ''
	if os.exists('header.html') && html.contains('@header') {
		h := os.read_file('header.html') or {
			panic('reading file header.html failed')
		}
		header = h.replace("\'", '"')
		html = header + html
	}
	lines := html.split_into_lines()
	mut s := strings.new_builder(1000)
	// base := path.all_after_last('/').replace('.html', '')
	s.writeln("
	import strings
	// === vweb html template ===
	fn vweb_tmpl_${fn_name}() {
	mut sb := strings.new_builder(${lines.len * 30})
	header := \' \' // TODO remove
	_ = header
	//footer := \'footer\'
")
	s.writeln(str_start)
	mut state := State.html
	mut in_span := false
	for _line in lines {
		line := _line.trim_space()
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
		if line.contains('@if ') {
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
