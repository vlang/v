module util

// v_literal_parse_base selects decimal parsing for V integer literals unless
// an explicit 0b, 0o, or 0x prefix requests base inference. It skips an optional sign.
@[inline]
pub fn v_literal_parse_base(value string) int {
	start := if value.len > 0 && value[0] in [`+`, `-`] { 1 } else { 0 }
	if value.len - start >= 2 && value[start] == `0`
		&& value[start + 1] in [`b`, `B`, `o`, `O`, `x`, `X`] {
		return 0
	}
	return 10
}
