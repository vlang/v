// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module scanner

import (
	compiler2.token
)

fn test_scan() {
	text := 'println(2 + 3)'
	mut scanner := new_scanner(text)
	mut tokens := []token.Token
	for {
		res := scanner.scan()
		if res.tok == .eof {
			break
		}
		tokens << res.tok
	}
	assert tokens.len == 6
	assert tokens[0] == .name
	assert tokens[1] == .lpar
	assert tokens[2] == .number
	assert tokens[3] == .plus
	assert tokens[4] == .number
	assert tokens[5] == .rpar

}

