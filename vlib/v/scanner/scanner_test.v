// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module scanner

import (
	v.token
)

fn test_scan() {
	text := 'println(2 + 3)'
	mut scanner := new_scanner(text, .skip_comments)
	mut token_kinds := []token.Kind
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
	d := f64(23000000e-3)
	assert int(d) == 23000
	mut e := f64(1.2E3) * f64(-1e-1)
	assert e == -120.0
	e = f64(1.2E3) * f64(1e-1)
	assert e == 120.0
	assert 1.23e+10 == 1.23e10
	assert 1.23e+10 == 1.23e0010
	assert -1.23e+10 == 1.23e0010 * -1.0
}
