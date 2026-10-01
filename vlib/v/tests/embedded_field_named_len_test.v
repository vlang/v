struct LenStr {
mut:
	len  int
	data &u8 = unsafe { nil }
}

struct LenToken {
	LenStr
mut:
	kind int
}

fn clear_len_token(mut token LenToken) {
	token.len = 0
	token.kind = 1
}

fn len_of_token(token &LenToken) int {
	return token.len
}

fn test_embedded_field_named_len() {
	mut token := LenToken{}
	token.len = 5
	assert token.len == 5
	assert token.LenStr.len == 5
	assert len_of_token(&token) == 5
	clear_len_token(mut token)
	assert token.len == 0
	assert token.kind == 1
	token.len += 3
	assert len_of_token(token) == 3
}
