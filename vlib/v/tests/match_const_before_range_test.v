const bsp_char = 0x08
const lf_char = 0x0A
const cr_char = 0x0D
const newline = 0x0A

fn classify(c u32) string {
	return match c {
		bsp_char { 'bsp' }
		cr_char, lf_char { 'nl' }
		0...0x1F { 'ctrl' }
		else { 'char' }
	}
}

fn same_value_consts(c int) string {
	return match c {
		lf_char { 'lf' }
		newline { 'newline' }
		else { 'other' }
	}
}

fn test_const_cases_before_an_overlapping_range() {
	assert classify(8) == 'bsp'
	assert classify(10) == 'nl'
	assert classify(13) == 'nl'
	assert classify(3) == 'ctrl'
	assert classify(0x1F) == 'ctrl'
	assert classify(65) == 'char'
}

fn test_consts_with_the_same_value_are_distinct_cases() {
	assert same_value_consts(10) == 'lf'
	assert same_value_consts(11) == 'other'
}
