// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module tmpl

import os
import strings

const (
	str_start = "sb.write(\'"
	str_end = "\' ) "
)

pub fn compile_template(path string) string {
	// lines := os.read_lines(path)
	mut html := os.read_file(path)or{
		panic('html failed')
	}
	mut header := ''
	if os.exists('header.html') && html.contains('@header') {
		h := os.read_file('header.html')or{
			panic('reading file header.html failed')
		}
		header = h.replace("\'", '"')
		html = header + html
	}
	lines := html.split_into_lines()
	mut s := strings.new_builder(1000)
	// base := path.all_after_last('/').replace('.html', '')
	s.writeln("
mut sb := strings.new_builder(${lines.len * 30})
header := \' \' // TODO remove
_ = header
//footer := \'footer\'
")
	s.writeln(str_start)
	mut in_css := true // false
	for _line in lines {
		line := _line.trim_space()
		if line == '<style>' {
			in_css = true
		}
		else if line == '</style>' {
			// in_css = false
		}
		if line.contains('@if ') {
			s.writeln(str_end)
			pos := line.index('@if') or {
				continue
			}
			s.writeln('if ' + line[pos + 4..] + '{')
			s.writeln(str_start)
		}
		else if line.contains('@end') {
			s.writeln(str_end)
			s.writeln('}')
			s.writeln(str_start)
		}
		else if line.contains('@else') {
			s.writeln(str_end)
			s.writeln(' } else { ')
			s.writeln(str_start)
		}
		else if line.contains('@for') {
			s.writeln(str_end)
			pos := line.index('@for') or {
				continue
			}
			s.writeln('for ' + line[pos + 4..] + '{')
			s.writeln(str_start)
		}
		else if !in_css && line.contains('.') && line.ends_with('{') {
			class := line.find_between('.', '{')
			s.writeln('<div class="$class">')
		}
		else if !in_css && line == '}' {
			s.writeln('</div>')
		}
		// HTML, may include `@var`
		else {
			s.writeln(line.replace('@', '\x24').replace("'", '"'))
		}
	}
	s.writeln(str_end)
	s.writeln('tmpl_res := sb.str() }')
	return s.str()
}
