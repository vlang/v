// The deprecated-`byte` type check must not fire on assembler text: the `.byte`
// directive is not the V type (see #28739). On other targets the `$if amd64` branch
// is skipped instead of parsed, and must not be reported either.
fn byte_directive_in_asm() int {
	mut result := 0
	$if amd64 {
		asm amd64 {
			.byte 0x90
			mov result, 7
			; =r (result)
		}
	} $else {
		result = 7
	}
	return result
}

fn test_byte_directive_in_inline_asm_is_not_the_byte_type() {
	assert byte_directive_in_asm() == 7
}
