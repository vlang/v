// The deprecated-`byte` type check must not fire on assembler text: `.byte` is a
// directive and `byte ptr` an operand size, neither is the V type (see #28739).
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
