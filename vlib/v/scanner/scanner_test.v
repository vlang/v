// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module scanner

import v.token
import v.pref

fn scan_kinds(text string) []token.Kind {
	mut scanner := new_scanner(text, .skip_comments, &pref.Preferences{})
	mut token_kinds := []token.Kind{}
	for {
		tok := scanner.scan()
		if tok.kind == .eof {
			break
		}
		token_kinds << tok.kind
	}
	return token_kinds
}

fn test_scan() {
	token_kinds := scan_kinds('println(2 + 3)')
	assert token_kinds.len == 6
	assert token_kinds[0] == .name
	assert token_kinds[1] == .lpar
	assert token_kinds[2] == .number
	assert token_kinds[3] == .plus
	assert token_kinds[4] == .number
	assert token_kinds[5] == .rpar
}

fn test_number_constant_input_format() {
	mut c := 0xa0
	assert c == 0xa0
	c = 0b1001
	assert c == 9
	c = 1000000
	assert c == 1000000
}

fn test_float_conversion_and_reading() {
	d := 23000000e-3
	assert int(d) == 23000
	mut e := 1.2E3 * -1e-1
	assert e == -120.0
	e = 1.2E3 * 1e-1
	x := 55.
	assert e == 120.0
	assert 1.23e+10 == 1.23e10
	assert 1.23e+10 == 1.23e0010
	assert (-1.23e+10) == (1.23e0010 * -1.0)
	assert x == 55.0
}

fn test_float_without_fraction() {
	mut result := scan_kinds('x := 10.')
	assert result.len == 3
	assert result[0] == .name
	assert result[1] == .decl_assign
	assert result[2] == .number
	result = scan_kinds('return 3., 4.')
	assert result.len == 4
	assert result[0] == .key_return
	assert result[1] == .number
	assert result[2] == .comma
	assert result[3] == .number
	result = scan_kinds('fun(5.)')
	assert result.len == 4
	assert result[0] == .name
	assert result[1] == .lpar
	assert result[2] == .number
	assert result[3] == .rpar
}

fn test_reference_bools() {
	result := scan_kinds('true && false')
	assert result.len == 3
	assert result[0] == .key_true
	assert result[1] == .and
	assert result[2] == .key_false
}

fn test_reference_var() {
	result := scan_kinds('&foo')
	assert result.len == 2
	assert result[0] == .amp
	assert result[1] == .name
}

fn test_array_of_references() {
	result := scan_kinds('[]&foo')
	assert result.len == 4
	assert result[0] == .lsbr
	assert result[1] == .rsbr
	assert result[2] == .amp
	assert result[3] == .name
}

fn test_ref_array_of_references() {
	result := scan_kinds('&[]&foo')
	assert result.len == 5
	assert result[0] == .amp
	assert result[1] == .lsbr
	assert result[2] == .rsbr
	assert result[3] == .amp
	assert result[4] == .name
}

fn test_ref_ref_foo() {
	result := scan_kinds('&&foo')
	assert result.len == 3
	assert result[0] == .amp
	assert result[1] == .amp
	assert result[2] == .name
}

fn test_array_of_ref_ref_foo() {
	result := scan_kinds('[]&&foo')
	assert result.len == 5
	assert result[0] == .lsbr
	assert result[1] == .rsbr
	assert result[2] == .amp
	assert result[3] == .amp
	assert result[4] == .name
}

fn test_ref_ref_array_ref_ref_foo() {
	result := scan_kinds('&&[]&&foo')
	assert result.len == 7
	assert result[0] == .amp
	assert result[1] == .amp
	assert result[2] == .lsbr
	assert result[3] == .rsbr
	assert result[4] == .amp
	assert result[5] == .amp
	assert result[6] == .name
}

fn test_escape_string() {
	assert '\x61' == 'a'
	assert '\x62' == 'b'
}
