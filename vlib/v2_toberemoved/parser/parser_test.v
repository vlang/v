// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: !windows
module parser

import os
import v2.pref
import v2.token

fn parse_code_for_test(code string) {
	tmp_file := '/tmp/v2_parser_test_${os.getpid()}.v'
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	prefs := &pref.Preferences{}
	mut file_set := token.FileSet.new()
	mut par := Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	assert files.len == 1
}

fn test_if_condition_parses_selector_struct_init_before_infix() {
	parse_code_for_test('
module main

fn main() {
	mut pos := token.Pos{}
	if pos == token.Pos{} && true {
		x := 1
	}
}
')
}

fn test_if_condition_keeps_uppercase_selector_followed_by_block() {
	parse_code_for_test('
module main

fn main() int {
	ch := 0
	if ch == C.EOF {
		return -1
	}
	return ch
}
')
}

fn test_match_branch_keeps_empty_uppercase_branch_block() {
	parse_code_for_test('
module main

type Obj = EmptyScopeObject | AsmRegister

struct AsmRegister {}
struct EmptyScopeObject {}

fn main() {
	node := Obj(EmptyScopeObject{})
	match node {
		AsmRegister, EmptyScopeObject {}
	}
}
')
}

fn test_global_unsafe_block_parses_leading_array_literal_stmt() {
	parse_code_for_test('
module main

struct File {}

__global files = unsafe { []&File{} }

fn main() {}
')
}

fn test_array_literal_parses_backslash_char_literal() {
	parse_code_for_test('
module main

fn main() {
	x := [u8(`\\\\`)]
	_ = x
}
')
}

fn test_call_argument_parses_untyped_backslash_char_array_literal() {
	parse_code_for_test('
module main

fn b(bytes []u8) []u8 {
	return bytes
}

fn main() {
	x := b([`\\\\`])
	_ = x
}
')
}
