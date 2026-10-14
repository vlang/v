import toml

fn test_scanner_lookbehind_preserves_signed_numbers_and_datetime() ! {
	doc := toml.parse_text('negative = -12
positive = +34
float = -1.25
date = 1979-05-27 07:32:00Z
')!
	assert doc.value('negative').int() == -12
	assert doc.value('positive').int() == 34
	assert doc.value('float').f64() == -1.25
	assert doc.value('date').datetime() == toml.DateTime{'1979-05-27 07:32:00Z'}
}
