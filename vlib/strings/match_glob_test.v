fn test_main() {
	assert '1*b'.match_glob('*[*]b') == true
	assert '1*b'.match_glob('1[*]b') == true

	assert '1****b'.match_glob('1*b') == true
	assert '1****b'.match_glob('1[*]b') == false
	assert '1****b'.match_glob('*[*]b') == true
	assert '*b'.match_glob('*[*]b') == true

	assert '**'.match_glob('*[*]') == true
	assert '**'.match_glob('[*]*') == true

	assert '**'.match_glob('*?') == true
	assert '**'.match_glob('?*') == true

	assert '**'.match_glob('[*]?') == true
	assert '**'.match_glob('?[*]') == true

	assert '**'.match_glob('??') == true
}
