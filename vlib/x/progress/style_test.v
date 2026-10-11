module progress

import strings
import term

fn draw_to_string(s Style, f f64, cells int) string {
	mut sb := strings.new_builder(32)
	s.draw(mut sb, f, cells)
	return sb.str()
}

fn test_classic_style() {
	s := ClassicStyle{}
	assert draw_to_string(s, 0.0, 10) == ' [>         ]'
	assert draw_to_string(s, 0.5, 10) == ' [=====>    ]'
	assert draw_to_string(s, 1.0, 10) == ' [==========]'
	// out-of-range fractions are clamped; zero cells draws only the frame
	assert draw_to_string(s, 7.0, 10) == ' [==========]'
	assert draw_to_string(s, -1.0, 10) == ' [>         ]'
	assert draw_to_string(s, 0.5, 0) == ' []'
}

fn test_block_style() {
	s := BlockStyle{}
	assert draw_to_string(s, 0.0, 10) == '▕          ▏'
	assert draw_to_string(s, 0.5, 10) == '▕█████     ▏'
	assert draw_to_string(s, 0.55, 10) == '▕█████▌    ▏'
	assert draw_to_string(s, 1.0, 10) == '▕██████████▏'
	// every eighth gets its own glyph, and the width never changes
	for i in 0 .. 81 {
		line := draw_to_string(s, f64(i) / 80.0, 10)
		assert line.runes().len == 12
	}
}

fn test_cut_to_counts_columns_not_runes() {
	assert cut_to('abcdef', 3) == 'abc'
	assert cut_to('abc', 10) == 'abc'
	assert cut_to('abc', 0) == ''
	// each emoji is two columns wide: three would be six columns
	assert term.printable_len(cut_to('🌑🌒🌓', 5)) <= 5
	assert term.printable_len(cut_to('🌑🌒🌓', 1)) == 0 // cannot fit even one
}

fn test_cut_to_uses_plain_text_when_styled_text_is_shortened() {
	styled := '\x1b[31mred\x1b[0m'
	assert cut_to(styled, 3) == styled
	assert cut_to(styled, 1) == 'r'
	assert cut_to('\x1b(Babcdef', 3) == 'abc'
	assert cut_to('\x1b]8;;https://example.test\x1b\\abcdef\x1b]8;;\x1b\\', 3) == 'abc'
	assert cut_to('\x1bPignored\x1b\\abcdef', 3) == 'abc'
}
