// Regression test: rune constants holding control characters must be emitted
// as valid C char literals. Previously they were written verbatim into the
// generated `#define`, so a `\r` produced a raw carriage return that split the
// macro across two lines and broke C compilation (e.g. vlang/coreutils `fold`).
const nul_char = `\0`
const bell_char = `\a`
const back_char = `\b`
const tab_char = `\t`
const newline_char = `\n`
const vtab_char = `\v`
const formfeed_char = `\f`
const return_char = `\r`
const escape_char = `\e`
const del_char = rune(127)

fn test_control_char_rune_consts() {
	assert int(nul_char) == 0
	assert int(bell_char) == 7
	assert int(back_char) == 8
	assert int(tab_char) == 9
	assert int(newline_char) == 10
	assert int(vtab_char) == 11
	assert int(formfeed_char) == 12
	assert int(return_char) == 13
	assert int(escape_char) == 27
	assert int(del_char) == 127
}

fn test_control_char_consts_usable_in_expr() {
	// exercise the consts in real expressions, matching the coreutils `fold` usage
	mut column := 5
	column = if return_char == `\r` { 0 } else { column + 1 }
	assert column == 0
	assert back_char == `\b`
	assert tab_char != newline_char
}
