fn test_hex_methods_on_u8() {
	assert u8(1).hex() == '01'
	assert u8(` `).hex() == '20'
	assert u8(255).hex() == 'ff'
}

fn test_hex_methods_on_char() {
	assert char(1).hex() == '01'
	assert char(` `).hex() == '20'
	assert char(255).hex() == 'ff'
}

fn test_hex_method_on_runes() {
	assert ` `.hex() == '20'
	assert `喂`.hex() == '5582'
	// latin:
	assert `A`.hex() == '41'
	assert `Z`.hex() == '5a'
	// cyrillic:
	assert `Д`.hex() == '414'
	assert `Я`.hex() == '42f'
	// emojies:
	assert `💣`.hex() == '1f4a3'
}
