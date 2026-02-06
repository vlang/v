module main

import encoding.utf8

fn test_true() {
	assert utf8.is_punct('!', 0)
	assert utf8.is_rune_punct(`!`)
	assert utf8.is_punct('.', 0)
	assert utf8.is_rune_punct(`.`)
	assert utf8.is_punct(',', 0)
	assert utf8.is_rune_punct(`,`)
	assert utf8.is_punct(';', 0)
	assert utf8.is_rune_punct(`;`)
	assert utf8.is_punct("'", 0)
	assert utf8.is_rune_punct(`'`)
	assert utf8.is_punct(',', 0)
	assert utf8.is_rune_punct(`,`)
	assert utf8.is_punct('/', 0)
	assert utf8.is_rune_punct(`/`)
	assert utf8.is_punct('*', 0)
	assert utf8.is_rune_punct(`*`)
}

fn test_false() {
	assert !utf8.is_punct('a', 0)
	assert !utf8.is_rune_punct(`a`)
	assert !utf8.is_punct('ç', 0)
	assert !utf8.is_rune_punct(`ç`)
	assert !utf8.is_punct('á', 0)
	assert !utf8.is_rune_punct(`á`)
	assert !utf8.is_punct('-', 0)
	assert !utf8.is_rune_punct(`-`)
}
