// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module pref

pub struct LineInfo {
pub mut:
	line_nr      int    // a quick single file run when called with v -line-info (contains line nr to inspect)
	path         string // same, but stores the path being parsed
	expr         string // "os.foo()" V code (expression) which needs autocomplete, right only function calls
	col          int
	is_running   bool            // so that line info is fetched only on the second checker run
	vars_printed map[string]bool // to avoid dups
}

fn (mut p Preferences) parse_line_info(line string) {
	// println("parse_line_info '${line}'")
	// Note: windows C:\Users\DDT\AppData\Local\Temp\sample_text.v:18:3
	format_err := 'wrong format, use `-line-info "file.v:24:7"'
	vals := line.split(':')
	if vals.len < 3 {
		eprintln(format_err)
		return
	}
	file_name := vals[..vals.len - 2].join(':')
	line_nr := vals[vals.len - 2].int() - 1

	if !file_name.ends_with('.v') || line_nr == -1 {
		eprintln(format_err)
		return
	}

	// Third value can be a column or expression for autocomplete like `os.create()`
	third := vals[vals.len - 1]
	if third[0].is_digit() {
		col := third.int() - 1
		p.linfo = LineInfo{
			line_nr: line_nr
			path:    file_name
			col:     col
		}
	} else {
		expr := third
		p.linfo = LineInfo{
			line_nr: line_nr
			path:    file_name
			expr:    expr
		}
	}
}
