// vtest build: !msvc

fn test_raw_template_with_named_operands() {
	lhs := 19
	rhs := 23
	mut result := 0
	asm amd64 raw {
		"movl %[lhs], %[result]\n\t"
		"addl %[rhs], %[result]\n\t"
		; [result] "=r" (result)
		; [lhs] "r" (lhs)
		  [rhs] "r" (rhs)
		; cc
	}
	assert result == 42
}

fn test_intel_syntax_and_three_operand_reordering() {
	$if !tinyc {
		lhs := 6
		rhs := 23
		mut intel_result := 19
		asm amd64 intel {
			add intel_result, rhs
			; +r (intel_result)
			; r (rhs)
			; cc
		}
		assert intel_result == 42

		mut att_result := 0
		asm amd64 {
			imul att_result, lhs, 7
			; =r (att_result)
			; r (lhs)
			; cc
		}
		assert att_result == 42
	}
}
