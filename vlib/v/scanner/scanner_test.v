// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module scanner

import (
	v.token
)

fn test_scan() {
	text := 'println(2 + 3)'
	mut scanner := new_scanner(text)
	mut token_kinds := []token.TokenKind
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

}

