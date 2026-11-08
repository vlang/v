module unicode

fn test_version() {
	assert unicode_version() == '16.0.0'
}

fn test_categories_latin() {
	assert category(`A`) == cat_lu
	assert category(`a`) == cat_ll
	assert category(`5`) == cat_nd
	assert category(` `) == cat_zs
	assert category(`.`) == cat_po
	assert category_name(`A`) == 'Lu'
}

fn test_categories_scripts() {
	// Greek, Cyrillic, Arabic, Han, Devanagari letters.
	assert is_letter(rune(0x03b1))
	assert is_letter(rune(0x0436))
	assert is_letter(rune(0x0627))
	assert is_letter(rune(0x4e2d))
	assert is_letter(rune(0x0915))
	// Combining mark, supplementary plane letter.
	assert is_mark(rune(0x0301))
	assert is_letter(rune(0x10400))
	// Unassigned stays Cn.
	assert category(rune(0x0378)) == cat_cn
}

fn test_numbers() {
	assert is_number(`5`)
	// Roman numeral (Nl) and vulgar fraction (No).
	assert is_number(rune(0x2167))
	assert is_number(rune(0x00bc))
	assert !is_digit(rune(0x2167))
}

fn test_space() {
	assert is_space(` `)
	assert is_space(rune(0x00a0))
	assert is_space(rune(0x1680))
	assert is_space(rune(0x2000))
	assert is_space(rune(0x3000))
	assert !is_space(`a`)
}

fn test_punct_symbol() {
	assert is_punct(`!`)
	assert is_punct(rune(0x201c))
	assert is_symbol(`$`)
	assert is_symbol(rune(0x20ac))
	assert is_control(rune(0x00))
	assert is_control(rune(0x7f))
}

fn test_case_mapping() {
	assert to_lower(`A`) == `a`
	assert to_upper(`b`) == `B`
	// Greek sigma: upper maps to capital, lower of capital is small sigma.
	assert to_upper(rune(0x03c3)) == rune(0x03a3)
	assert to_lower(rune(0x03a3)) == rune(0x03c3)
	// Latin dotless i / dotted I.
	assert to_lower(rune(0x0130)) == rune(0x0069)
	// Dotless i maps to capital I per UCD field 12.
	assert to_upper(rune(0x0131)) == rune(0x0049)
	// Titlecase.
	assert to_title(rune(0x01c9)) == rune(0x01c8)
}

fn test_folding() {
	assert fold(`A`) == `a`
	// Full folding expands sharp s.
	assert fold_full(rune(0x00df)) == 'ss'
	assert fold_full(`A`) == 'a'
}

fn test_alpha_case_props() {
	assert is_alphabetic(`A`)
	assert is_alphabetic(rune(0x00aa))
	assert !is_alphabetic(`5`)
	assert is_upper(`A`)
	assert is_lower(`a`)
	assert !is_upper(`a`)
}
