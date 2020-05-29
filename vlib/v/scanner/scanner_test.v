// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module scanner

import v.token


struct TestStruct {
	test	string
}

fn (mut t TestStruct) test_struct() {
	assert @STRUCT == 'TestStruct'
}

fn (mut t TestStruct) test_struct_w_return() string {
	assert @STRUCT == 'TestStruct'
	return t.test
}

fn (mut t TestStruct) test_struct_w_high_order(cb fn(int)string) string {
	assert @STRUCT == 'TestStruct'
	return 'test'+cb(2)
}

struct TestFn { }

fn (mut t TestFn) tst_1() {
	assert @FN == 'tst_1'
}

fn (mut t TestFn) tst_2(cb fn(int)) {
	assert @FN == 'tst_2'
	cb(1)
}

fn fn_name_mod_level() {
	assert @FN == 'fn_name_mod_level'
}

fn fn_name_mod_level_high_order(cb fn(int)) {
	assert @FN == 'fn_name_mod_level_high_order'
	cb(1)
}

fn test_scan() {
	text := 'println(2 + 3)'
	mut scanner := new_scanner(text, .skip_comments)
	mut token_kinds := []token.Kind{}
	for {
		tok := scanner.scan()
		if tok.kind == .eof {
			break
		}
		token_kinds << tok.kind
	}
	assert token_kinds.len == 6
	assert token_kinds[0] == .name
	assert token_kinds[1] == .lpar
	assert token_kinds[2] == .number
	assert token_kinds[3] == .plus
	assert token_kinds[4] == .number
	assert token_kinds[5] == .rpar
	// test number costants input format
	mut c := 0xa0
	assert c == 0xa0
	c = 0b1001
	assert c == 9
	c = 1000000
	assert c == 1000000
	// test float conversion and reading
	d := 23000000e-3
	assert int(d) == 23000
	mut e := 1.2E3 * -1e-1
	assert e == -120.0
	e = 1.2E3 * 1e-1
	assert e == 120.0
	assert 1.23e+10 == 1.23e10
	assert 1.23e+10 == 1.23e0010
	assert (-1.23e+10) == (1.23e0010 * -1.0)

	// Test @FN
	assert @FN == 'test_scan'

	fn_name_mod_level()
	fn_name_mod_level_high_order(fn(i int){
		t := i + 1
		assert t == 2
	})

	tfn := TestFn{}
	tfn.tst_1()
	tfn.tst_2(fn(i int){
		t := i + 1
		assert t == 2
	})

	// Test @MOD
	assert @MOD == 'scanner'

	// Test @STRUCT
	assert @STRUCT == ''

	ts := TestStruct { test: "test" }
	ts.test_struct()
	r1 := ts.test_struct_w_return()
	r2 := ts.test_struct_w_high_order(fn(i int)string{
		assert @STRUCT == ''
		return i.str()
	})
	assert r1 == 'test'
	assert r2 == 'test2'

}

fn test_vmod_file() {
	content := @VMOD_FILE
	assert content.len > 0
	assert content.contains('Module {')
	assert content.contains('name:')
	assert content.contains('version:')
	assert content.contains('description:')
}
