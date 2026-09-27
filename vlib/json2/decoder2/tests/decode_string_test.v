import json2.decoder2 as json

fn test_json_escape_low_chars() {
	assert json.decode[string](r'"\u001b"')! == '\u001b'
	assert json.decode[string](r'"\u000f"')! == '\u000f'
	assert json.decode[string](r'" "')! == '\u0020'
	assert json.decode[string](r'"\u0000"')! == '\u0000'
}

fn test_json_string() {
	assert json.decode[string](r'"te\u2714st"')! == 'te✔st'
	// assert json.decode[string]('te✔st')! == 'te✔st'
}

fn test_json_string_simple_escapes_continue_scanning() {
	assert json.decode[string](r'"line\nend"')! == 'line\nend'
	assert json.decode[string](r'"say \"hello\" after"')! == 'say "hello" after'
	assert json.decode[string](r'"left\\right after"')! == 'left\\right after'
}

fn test_json_string_emoji() {
	assert json.decode[string](r'"🐈"')! == '🐈'
	assert json.decode[string](r'"💀"')! == '💀'
	assert json.decode[string](r'"🐈💀"')! == '🐈💀'
}

fn test_json_string_non_ascii() {
	assert json.decode[string](r'"\u3072\u3089\u304c\u306a"')! == 'ひらがな'
	assert json.decode[string]('"a\\u3072b\\u3089c\\u304cd\\u306ae fgh"')! == 'aひbらcがdなe fgh'
	assert json.decode[string]('"\\u3072\\u3089\\u304c\\u306a"')! == 'ひらがな'
}

fn test_json_utf16_surrogate_pairs() {
	assert json.decode[string](r'"\uD83D\uDE00"')! == '😀'
	assert json.decode[string](r'"music: \uD834\uDD1E"')! == 'music: 𝄞'
	assert json.decode[string](r'"\ud83d\ude00 \uD83D\uDE03"')! == '😀 😃'
}

fn test_json_rejects_invalid_utf16_surrogates() {
	for input in [r'"\uD83D"', r'"\uDE00"', r'"\uD83D\u0041"'] {
		mut failed := false
		json.decode[string](input) or { failed = true }
		assert failed, 'Expected invalid surrogate sequence `${input}` to fail'
	}
}

fn test_utf8_strings_are_not_modified() {
	assert json.decode[string]('"ü"')! == 'ü'
	assert json.decode[string]('"Schilddrüsenerkrankungen"')! == 'Schilddrüsenerkrankungen'
}
