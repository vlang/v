module strconv

import strings

fn test_format_str_pads_to_len0() {
	p := BF_param{
		len0:   8
		pad_ch: ` `
	}
	assert format_str('hi', p) == '      hi'
	assert format_str('12345678', p) == '12345678'
	assert format_str('123456789', p) == '123456789'
}

fn test_format_str_pad_ch_and_align() {
	right := BF_param{
		len0:   8
		pad_ch: `*`
	}
	assert format_str('hi', right) == '******hi'
	left := BF_param{
		len0:   8
		pad_ch: `*`
		align:  .left
	}
	assert format_str('hi', left) == 'hi******'
}

fn test_format_str_ignores_len0_at_or_below_visible_length() {
	zero := BF_param{
		len0: 0
	}
	assert format_str('hi', zero) == 'hi'
	shorter := BF_param{
		len0: 1
	}
	assert format_str('hi', shorter) == 'hi'
	exact := BF_param{
		len0: 2
	}
	assert format_str('hi', exact) == 'hi'
}

fn test_format_str_default_len0_returns_unchanged() {
	// BF_param{} defaults len0 to -1, which must not pad.
	assert format_str('hi', BF_param{}) == 'hi'
}

fn test_format_str_counts_utf8_runes_as_one_column() {
	p := BF_param{
		len0:   6
		pad_ch: `.`
	}
	assert format_str('ána', p) == '...ána'
	assert format_str('año', p) == '...año'
}

fn test_format_str_sb_matches_format_str() {
	right := BF_param{
		len0:   8
		pad_ch: ` `
	}
	left := BF_param{
		len0:   8
		pad_ch: `*`
		align:  .left
	}
	mut sb := strings.new_builder(16)
	format_str_sb('hi', right, mut sb)
	sb.write_u8(`|`)
	format_str_sb('hi', left, mut sb)
	sb.write_u8(`|`)
	format_str_sb('hi', BF_param{}, mut sb)
	sb.write_u8(`|`)
	format_str_sb('longer than len0', BF_param{
		len0: 2
	}, mut sb)
	assert sb.str() == '      hi|hi******|hi|longer than len0'
}

fn test_format_dec_sb_right_pads_with_spaces() {
	p := BF_param{
		len0:   6
		pad_ch: ` `
	}
	mut sb := strings.new_builder(16)
	format_dec_sb(42, p, mut sb)
	sb.write_u8(`|`)
	format_dec_sb(0, p, mut sb)
	sb.write_u8(`|`)
	format_dec_sb(1234567, p, mut sb)
	assert sb.str() == '    42|     0|1234567'
}

fn test_format_dec_sb_zero_pad_fills_with_zeros() {
	p := BF_param{
		len0:   6
		pad_ch: `0`
	}
	mut sb := strings.new_builder(16)
	format_dec_sb(42, p, mut sb)
	sb.write_u8(`|`)
	format_dec_sb(0, p, mut sb)
	sb.write_u8(`|`)
	format_dec_sb(123456, p, mut sb)
	assert sb.str() == '000042|000000|123456'
}

fn test_format_dec_sb_zero_pad_writes_sign_before_padding() {
	// A negative value must not be swallowed by the zero padding.
	p := BF_param{
		len0:     6
		pad_ch:   `0`
		positive: false
	}
	mut sb := strings.new_builder(16)
	format_dec_sb(42, p, mut sb)
	sb.write_u8(`|`)
	with_flag := BF_param{
		len0:      6
		pad_ch:    `0`
		positive:  false
		sign_flag: true
	}
	format_dec_sb(42, with_flag, mut sb)
	assert sb.str() == '-00042|-00042'
}

fn test_format_dec_sb_non_zero_pad_writes_sign_before_padding() {
	p := BF_param{
		len0:      8
		pad_ch:    `*`
		positive:  false
		sign_flag: true
	}
	mut sb := strings.new_builder(16)
	format_dec_sb(42, p, mut sb)
	assert sb.str() == '*****-42'
}

fn test_format_dec_sb_left_aligns() {
	p := BF_param{
		len0:   6
		pad_ch: ` `
		align:  .left
	}
	mut sb := strings.new_builder(16)
	format_dec_sb(42, p, mut sb)
	sb.write_u8(`|`)
	shorter := BF_param{
		len0:  2
		align: .left
	}
	format_dec_sb(42, shorter, mut sb)
	assert sb.str() == '42    |42'
}

fn test_format_dec_sb_writes_max_u64() {
	p := BF_param{
		len0: 25
	}
	mut sb := strings.new_builder(32)
	format_dec_sb(u64(-1), p, mut sb)
	assert sb.str() == '     18446744073709551615'
}

fn test_format_dec_sb_zero_has_no_padding_without_len0() {
	mut sb := strings.new_builder(16)
	format_dec_sb(0, BF_param{}, mut sb)
	assert sb.str() == '0'
}

fn test_dec_digits() {
	assert dec_digits(u64(0)) == 1
	assert dec_digits(u64(5)) == 1
	assert dec_digits(u64(9)) == 1
	assert dec_digits(u64(10)) == 2
	assert dec_digits(u64(99)) == 2
	assert dec_digits(u64(100)) == 3
	assert dec_digits(u64(999)) == 3
	assert dec_digits(u64(1000)) == 4
	assert dec_digits(u64(9999)) == 4
	assert dec_digits(u64(10_000)) == 5
	assert dec_digits(u64(99_999)) == 5
	assert dec_digits(u64(100_000)) == 6
	assert dec_digits(u64(1_000_000)) == 7
	assert dec_digits(u64(10_000_000_000)) == 11
	assert dec_digits(u64(999_999_999_999_999_999)) == 18
	assert dec_digits(u64(-1)) == 20
}

fn test_format_fl_renders_fixed_decimals() {
	p := BF_param{
		len1: 3
	}
	assert format_fl(1.0 / 3.0, p) == '0.333'
	assert format_fl(2.0, p) == '2.000'
	assert format_fl(0.0, p) == '0.000'
	assert format_fl(1.5, p) == '1.500'
	assert format_fl(123.456, p) == '123.456'
}

fn test_format_fl_uses_the_magnitude_of_the_input() {
	// format_fl takes the absolute value; the caller supplies the sign.
	assert format_fl(-1.25, BF_param{
		len1: 3
	}) == '1.250'
}

fn test_format_fl_zero_pads_to_len0() {
	p := BF_param{
		len1:   3
		len0:   10
		pad_ch: `0`
	}
	assert format_fl(1.0 / 3.0, p) == '000000.333'
}

fn test_format_fl_sign_flag_prefixes_a_plus() {
	p := BF_param{
		len1:      3
		sign_flag: true
	}
	assert format_fl(1.0 / 3.0, p) == '+0.333'
}

fn test_format_fl_rm_tail_zero_drops_the_fraction() {
	p := BF_param{
		len1:         4
		rm_tail_zero: true
	}
	assert format_fl(1234.5, p) == '1234.5'
	assert format_fl(2.0, BF_param{
		len1:         3
		rm_tail_zero: true
	}) == '2'
}

fn test_format_es_renders_scientific_notation() {
	p := BF_param{
		len1: 3
	}
	assert format_es(1.0 / 3.0, p) == '3.333e-01'
	assert format_es(2.0, p) == '2.000e+00'
	assert format_es(1234.5, p) == '1.235e+03'
}

fn test_format_es_rm_tail_zero_and_padding() {
	assert format_es(2.0, BF_param{
		len1:         3
		rm_tail_zero: true
	}) == '2e+00'
	p := BF_param{
		len1:   3
		len0:   12
		pad_ch: ` `
	}
	assert format_es(2.0, p) == '   2.000e+00'
}

fn test_format_fl_old_matches_format_fl() {
	p := BF_param{
		len1: 3
	}
	assert format_fl_old(1.0 / 3.0, p) == '0.333'
	assert format_fl_old(2.0, p) == '2.000'
	assert format_fl_old(-1.25, p) == '1.250'
}

fn test_format_dec_old_pads_decimals() {
	spaces := BF_param{
		len0: 8
	}
	assert format_dec_old(u64(42), spaces) == '      42'
	zeros := BF_param{
		len0:   8
		pad_ch: `0`
	}
	assert format_dec_old(u64(42), zeros) == '00000042'
	signed := BF_param{
		len0:      8
		pad_ch:    `*`
		positive:  false
		sign_flag: true
	}
	assert format_dec_old(u64(42), signed) == '*****-42'
}

fn test_f64_to_str_lnd1_fixed_decimals() {
	assert f64_to_str_lnd1(0.0, 0) == '0'
	assert f64_to_str_lnd1(1.0, 0) == '1'
	assert f64_to_str_lnd1(1.0, 2) == '1.00'
	assert f64_to_str_lnd1(34.2, 2) == '34.20'
	assert f64_to_str_lnd1(34.2, 6) == '34.200000'
	assert f64_to_str_lnd1(123.456, 2) == '123.46'
	assert f64_to_str_lnd1(123.456, 6) == '123.456000'
}

fn test_f64_to_str_lnd1_rounds_positive_values() {
	assert f64_to_str_lnd1(0.5, 0) == '1'
	assert f64_to_str_lnd1(1.5, 0) == '2'
	assert f64_to_str_lnd1(2.5, 0) == '3'
	assert f64_to_str_lnd1(1.0 / 3.0, 2) == '0.33'
}

fn test_f64_to_str_lnd1_large_and_small_magnitudes() {
	assert f64_to_str_lnd1(1e21, 2) == '1000000000000000000000.00'
	assert f64_to_str_lnd1(1e-7, 2) == '0.00'
}
