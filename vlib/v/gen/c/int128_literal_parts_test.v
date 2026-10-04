module c

import v.flat

fn test_numeric_literal_emission_preserves_underscore_and_plain_spellings() {
	for text, expected in {
		'1000':                 '1000'
		'1_000':                '1000'
		'0x_ff':                '0xff'
		'0o7_7':                '077'
		'18446744073709551616': '__v_u128_make(1ULL, 0ULL)'
	} {
		mut a := flat.FlatAst.new()
		id := a.add_val(.int_literal, text)
		mut g := FlatGen.new()
		g.a = &a
		g.gen_expr(id)
		assert g.sb.str() == expected, text
	}
	for text, expected in {
		'1000.5':   '1000.5'
		'1_000.5':  '1000.5'
		'1.0e+1_0': '1.0e+10'
	} {
		mut a := flat.FlatAst.new()
		id := a.add_val(.float_literal, text)
		mut g := FlatGen.new()
		g.a = &a
		g.gen_expr(id)
		assert g.sb.str() == expected, text
	}
}

fn test_int128_literal_parts_keeps_the_64_bit_boundary_in_every_base() {
	for text in ['0', '123', '18_446_744_073_709_551_615', '18446744073709551615', '0xffffffffffffffff',
		'0o1777777777777777777777', '0b${'1'.repeat(64)}', '${'0'.repeat(80)}18446744073709551615',
		'0x${'0'.repeat(80)}ffffffffffffffff', '0o${'0'.repeat(80)}1777777777777777777777',
		'0b${'0'.repeat(80)}${'1'.repeat(64)}'] {
		assert int128_literal_parts(text) == none, text
	}
	for text in ['18446744073709551616', '18_446_744_073_709_551_616', '0x10000000000000000',
		'0o2000000000000000000000', '0b1${'0'.repeat(64)}', '${'0'.repeat(80)}18446744073709551616',
		'0x${'0'.repeat(80)}10000000000000000', '0o${'0'.repeat(80)}2000000000000000000000',
		'0b${'0'.repeat(80)}1${'0'.repeat(64)}'] {
		parts := int128_literal_parts(text) or { panic('missing wide literal ${text}') }
		assert parts.high == 1, text
		assert parts.low == 0, text
	}
}

fn test_int128_literal_parts_keeps_full_width_and_invalid_values() {
	for text in ['340282366920938463463374607431768211455', '0x${'f'.repeat(32)}',
		'0b${'1'.repeat(128)}'] {
		parts := int128_literal_parts(text) or { panic('missing wide literal ${text}') }
		assert parts.high == ~u64(0), text
		assert parts.low == ~u64(0), text
	}
	for text in ['', '-1', '-18446744073709551616', '0x', '0x0000000000000000g',
		'0b000000000000000000000000000000002', '18446744073709551616x',
		'340282366920938463463374607431768211456', '0x1${'0'.repeat(32)}', '0b1${'0'.repeat(128)}'] {
		assert int128_literal_parts(text) == none, text
	}
}
