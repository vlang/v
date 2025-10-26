// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module pref

// Method copy from vls/lsp.v
pub enum Method {
	unknown         @['unknown']
	initialize      @['initialize']
	initialized     @['initialized']
	did_open        @['textDocument/didOpen']
	did_change      @['textDocument/didChange']
	definition      @['textDocument/definition']
	completion      @['textDocument/completion']
	signature_help  @['textDocument/signatureHelp']
	set_trace       @['$/setTrace']
	cancel_request  @['$/cancelRequest']
	shutdown        @['shutdown']
	exit            @['exit']
}

pub struct LineInfo {
pub mut:
	method       Method
	path         string // same, but stores the path being parsed
	line_nr      int    // a quick single file run when called with v -line-info (contains line nr to inspect)
	col          int
	vars_printed map[string]bool // to avoid dups
}

fn (mut p Preferences) parse_line_info(line string) {
	format_err := 'wrong format, use `-line-info "file.v:24:7"'
	vals := line.split(':')
	if vals.len < 3 {
		eprintln(format_err)
		return
	}
	file_name := vals[..vals.len - 2].join(':')
	line_nr := vals[vals.len - 2].int()

	if (!file_name.ends_with('.v') && !file_name.ends_with('.vv')) || line_nr == -1 {
		eprintln(format_err)
		return
	}

	// Third value is column
	third := vals[vals.len - 1]
	mut col := 0
	method := if third.starts_with('fn^') {
		col = third[3..].int() - 1
		Method.signature_help
	} else if third.starts_with('gd^') {
		col = third[3..].int() - 1
		Method.definition
	} else if third[0].is_digit() {
		col = third.int() - 1
		Method.completion
	} else {
		Method.unknown
	}
	p.linfo = LineInfo{
		method:  method
		line_nr: line_nr
		path:    file_name
		col:     col
	}
}
