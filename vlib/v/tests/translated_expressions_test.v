@[translated]
module main

const translated_regs = [3, 12, 13]!

fn test_translated_expression_conditions_and_pointer_statements() {
	value := true
	if if value { true } else { false } {
		assert 13 - -5 == 18
	} else {
		assert false
	}
	regs := [3, 12, 13]!
	i := 0
	mut total := 0
	if value {
		for i = 0; i < sizeof(regs) / sizeof(regs[0]); i++ {
			total += regs[i]
		}
	} else {
		assert false
	}
	assert total == 28
	assert sizeof(translated_regs) / sizeof(translated_regs[0]) == 3
	mut values := [7, 0]
	unsafe {
		ptr := &values[0]
		dst := &values[1]
		ch := *ptr++
		*dst++ = ch
	}
	assert values == [7, 7]
	if value {
		goto done
	}
	assert false
	done:
	0
}

fn test_translated_if_expression_in_match_subject() {
	value := true
	result := match if value { 1 } else { 2 } {
		1 { 42 }
		else { 0 }
	}
	assert result == 42
}

fn test_translated_sizeof_later_constant_and_postfix_continuation() {
	assert sizeof(translated_later_regs) == sizeof(translated_regs)
	mut current := 3
	offset := 2
	result := current++ +
		offset
	assert result == 5
	assert current == 4
}

const translated_later_regs = [5, 8, 13]!
