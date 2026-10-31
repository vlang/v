module builtin

// *****************************************************************************
// * [ rpad Tests ]
// *****************************************************************************
fn test_rpad_basic() {
	assert 'abc'.rpad('x', 3) == 'abcxxx'
	assert 'abc'.rpad('x', 1) == 'abcx'
	assert 'abc'.rpad('ab', 2) == 'abcabab'
	assert 'abc'.rpad('123', 3) == 'abc123123123'
}

fn test_rpad_empty_strings() {
	assert ''.rpad('x', 3) == 'xxx'
	assert ''.rpad('ab', 2) == 'abab'
	assert 'abc'.rpad('', 3) == 'abc'
	assert ''.rpad('', 3) == ''
}

fn test_rpad_whitespace() {
	assert 'abc'.rpad(' ', 3) == 'abc   '
	assert 'abc'.rpad('\t', 2) == 'abc\t\t'
	assert 'abc'.rpad('\n', 2) == 'abc\n\n'
}

fn test_rpad_unicode() {
	assert 'abc'.rpad('é', 2) == 'abcéé'
	assert 'abc'.rpad('🔥', 2) == 'abc🔥🔥'
	assert '你好'.rpad('界', 2) == '你好界界'
	assert '🌍'.rpad('🚀', 3) == '🌍🚀🚀🚀'
}

fn test_rpad_nonpositive_times() {
	for times in [0, -1, -100] {
		assert 'abc'.rpad('x', times) == 'abc'
		assert 'abc'.rpad('ab', times) == 'abc'
		assert ''.rpad('x', times) == ''
		assert 'abc'.rpad('', times) == 'abc'
		assert ''.rpad('', times) == ''
	}
}

fn test_rpad_large() {
	result := 'abc'.rpad('x', 10000)

	assert result.len == 10003
	assert result.starts_with('abc')
	assert result.ends_with('xxxxx')
}

// *****************************************************************************
// * [ lpad Tests ]
// *****************************************************************************
fn test_lpad_basic() {
	assert 'abc'.lpad('x', 3) == 'xxxabc'
	assert 'abc'.lpad('x', 1) == 'xabc'
	assert 'abc'.lpad('ab', 2) == 'abababc'
	assert 'abc'.lpad('123', 3) == '123123123abc'
}

fn test_lpad_empty_strings() {
	assert ''.lpad('x', 3) == 'xxx'
	assert ''.lpad('ab', 2) == 'abab'
	assert 'abc'.lpad('', 3) == 'abc'
	assert ''.lpad('', 3) == ''
}

fn test_lpad_whitespace() {
	assert 'abc'.lpad(' ', 3) == '   abc'
	assert 'abc'.lpad('\t', 2) == '\t\tabc'
	assert 'abc'.lpad('\n', 2) == '\n\nabc'
}

fn test_lpad_unicode() {
	assert 'abc'.lpad('é', 2) == 'ééabc'
	assert 'abc'.lpad('🔥', 2) == '🔥🔥abc'
	assert '你好'.lpad('界', 2) == '界界你好'
	assert '🌍'.lpad('🚀', 3) == '🚀🚀🚀🌍'
}

fn test_lpad_nonpositive_times() {
	for times in [0, -1, -100] {
		assert 'abc'.lpad('x', times) == 'abc'
		assert 'abc'.lpad('ab', times) == 'abc'
		assert ''.lpad('x', times) == ''
		assert 'abc'.lpad('', times) == 'abc'
		assert ''.lpad('', times) == ''
	}
}

fn test_lpad_large() {
	result := 'abc'.lpad('x', 10000)

	assert result.len == 10003
	assert result.starts_with('xxxxx')
	assert result.ends_with('abc')
}

// *****************************************************************************
// * [ rfill Tests ]
// *****************************************************************************
fn test_rfill_basic() {
	assert 'abc'.rfill(`x`, 6) == 'abcxxx'
	assert 'abc'.rfill(`x`, 4) == 'abcx'
	assert 'abc'.rfill(`0`, 8) == 'abc00000'
	assert 'a'.rfill(`-`, 5) == 'a----'
}

fn test_rfill_exact_length() {
	assert 'abc'.rfill(`x`, 3) == 'abc'
	assert 'hello'.rfill(`x`, 5) == 'hello'
	assert '🔥'.rfill(`x`, 1) == '🔥'
}

fn test_rfill_already_longer() {
	assert 'abcdef'.rfill(`x`, 3) == 'abcdef'
	assert 'abcdef'.rfill(`x`, 1) == 'abcdef'
	assert '你好世界'.rfill(`x`, 2) == '你好世界'
}

fn test_rfill_empty_string() {
	assert ''.rfill(`x`, 1) == 'x'
	assert ''.rfill(`x`, 5) == 'xxxxx'
	assert ''.rfill(`🔥`, 3) == '🔥🔥🔥'
}

fn test_rfill_unicode() {
	assert 'é'.rfill(`x`, 3) == 'éxx'
	assert '你好'.rfill(`界`, 4) == '你好界界'
	assert '🔥'.rfill(`🚀`, 3) == '🔥🚀🚀'
	assert '🌍🚀'.rfill(`⭐`, 4) == '🌍🚀⭐⭐'
}

fn test_rfill_whitespace() {
	assert 'abc'.rfill(` `, 6) == 'abc   '
	assert 'abc'.rfill(`\t`, 5) == 'abc\t\t'
	assert 'abc'.rfill(`\n`, 5) == 'abc\n\n'
}

fn test_rfill_nonpositive_max_len() {
	for target_len in [0, -1, -100] {
		assert 'abc'.rfill(`x`, target_len) == 'abc'
		assert 'abc'.rfill(`0`, target_len) == 'abc'
		assert ''.rfill(`x`, target_len) == ''
		assert 'abc'.rfill(`🔥`, target_len) == 'abc'
		assert ''.rfill(`🔥`, target_len) == ''
	}
}

fn test_rfill_large() {
	result := 'abc'.rfill(`x`, 10000)

	assert result.len == 10000
	assert result.starts_with('abc')
	assert result.ends_with('xxxxx')
}

fn test_rfill_unicode_length() {
	result := '🔥'.rfill(`🚀`, 10)

	assert result.runes().len == 10
	assert result == '🔥' + '🚀'.repeat(9)
}

// *****************************************************************************
// * [ lfill Tests ]
// *****************************************************************************
fn test_lfill_basic() {
	assert 'abc'.lfill(`x`, 6) == 'xxxabc'
	assert 'abc'.lfill(`x`, 4) == 'xabc'
	assert 'abc'.lfill(`0`, 8) == '00000abc'
	assert 'a'.lfill(`-`, 5) == '----a'
}

fn test_lfill_exact_length() {
	assert 'abc'.lfill(`x`, 3) == 'abc'
	assert 'hello'.lfill(`x`, 5) == 'hello'
	assert '🔥'.lfill(`x`, 1) == '🔥'
}

fn test_lfill_already_longer() {
	assert 'abcdef'.lfill(`x`, 3) == 'abcdef'
	assert 'abcdef'.lfill(`x`, 1) == 'abcdef'
	assert '你好世界'.lfill(`x`, 2) == '你好世界'
}

fn test_lfill_empty_string() {
	assert ''.lfill(`x`, 1) == 'x'
	assert ''.lfill(`x`, 5) == 'xxxxx'
	assert ''.lfill(`🔥`, 3) == '🔥🔥🔥'
}

fn test_lfill_unicode() {
	assert 'é'.lfill(`x`, 3) == 'xxé'
	assert '你好'.lfill(`界`, 4) == '界界你好'
	assert '🔥'.lfill(`🚀`, 3) == '🚀🚀🔥'
	assert '🌍🚀'.lfill(`⭐`, 4) == '⭐⭐🌍🚀'
}

fn test_lfill_whitespace() {
	assert 'abc'.lfill(` `, 6) == '   abc'
	assert 'abc'.lfill(`\t`, 5) == '\t\tabc'
	assert 'abc'.lfill(`\n`, 5) == '\n\nabc'
}

fn test_lfill_nonpositive_max_len() {
	for target_len in [0, -1, -100] {
		assert 'abc'.lfill(`x`, target_len) == 'abc'
		assert 'abc'.lfill(`0`, target_len) == 'abc'
		assert ''.lfill(`x`, target_len) == ''
		assert 'abc'.lfill(`🔥`, target_len) == 'abc'
		assert ''.lfill(`🔥`, target_len) == ''
	}
}

fn test_lfill_large() {
	result := 'abc'.lfill(`x`, 10000)

	assert result.len == 10000
	assert result.starts_with('xxxxx')
	assert result.ends_with('abc')
}

fn test_lfill_unicode_length() {
	result := '🔥'.lfill(`🚀`, 10)

	assert result.runes().len == 10
	assert result == '🚀'.repeat(9) + '🔥'
}

// *****************************************************************************
// * [ Cross-Function Tests ]
// *****************************************************************************
fn test_padding_does_not_modify_original() {
	original := 'hello'

	_ := original.rpad('x', 3)
	_ := original.lpad('x', 3)
	_ := original.rfill(`x`, 10)
	_ := original.lfill(`x`, 10)

	assert original == 'hello'
}

fn test_padding_symmetry() {
	assert 'abc'.rpad('x', 3) == 'abcxxx'
	assert 'abc'.lpad('x', 3) == 'xxxabc'

	assert 'abc'.rfill(`x`, 6) == 'abcxxx'
	assert 'abc'.lfill(`x`, 6) == 'xxxabc'
}

fn test_fill_output_length() {
	for length in 1 .. 101 {
		right := 'abc'.rfill(`x`, length)
		left := 'abc'.lfill(`x`, length)

		expected := if length > 3 { length } else { 3 }

		assert right.runes().len == expected
		assert left.runes().len == expected
	}
}

fn test_pad_output_length() {
	for times in 1 .. 101 {
		right := 'abc'.rpad('xy', times)
		left := 'abc'.lpad('xy', times)

		assert right.len == 3 + 2 * times
		assert left.len == 3 + 2 * times
	}
}

fn test_chained_operations() {
	right := 'abc'.rpad('x', 2)
	result := right.lpad('y', 2)
	assert result == 'yyabcxx'

	left := 'abc'.lfill(`0`, 5)
	filled := left.rfill(`x`, 8)
	assert filled == '00abcxxx'
}

fn test_chained_operations_in_one() {
	padded := 'abc'.rpad('x', 2).lpad('y', 2)
	assert padded == 'yyabcxx'

	filled := 'abc'.lfill(`0`, 5).rfill(`x`, 8)
	assert filled == '00abcxxx'
}
