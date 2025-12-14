module builtin

// VAssertMetaInfo is used during assertions. An instance of it is filled in by compile time generated code, when an assertion fails.
pub struct VAssertMetaInfo {
pub:
	fpath   string // the source file path of the assertion
	line_nr int    // the line number of the assertion
	fn_name string // the function name in which the assertion is
	src     string // the actual source line of the assertion
	op      string // the operation of the assertion, i.e. '==', '<', 'call', etc ...
	llabel  string // the left side of the infix expressions as source
	rlabel  string // the right side of the infix expressions as source
	lvalue  string // the stringified *actual value* of the left side of a failed assertion
	rvalue  string // the stringified *actual value* of the right side of a failed assertion
	message string // the value of the `message` from `assert cond, message`
	has_msg bool   // false for assertions like `assert cond`, true for `assert cond, 'oh no'`
}

// free frees the memory occupied by the assertion meta data. It is called automatically by
// the code, that V's test framework generates, after all other callbacks have been called.
@[manualfree; unsafe]
pub fn (ami &VAssertMetaInfo) free() {
	unsafe {
		ami.fpath.free()
		ami.fn_name.free()
		ami.src.free()
		ami.op.free()
		ami.llabel.free()
		ami.rlabel.free()
		ami.lvalue.free()
		ami.rvalue.free()
		ami.message.free()
	}
}

fn __print_assert_failure(i &VAssertMetaInfo) {
	eprintln('${i.fpath}:${i.line_nr + 1}: FAIL: fn ${i.fn_name}: assert ${i.src}')
	if i.op.len > 0 && i.op != 'call' {
		if i.llabel == i.lvalue {
			eprintln('   left value: ${i.llabel}')
		} else {
			eprintln('   left value: ${i.llabel} = ${i.lvalue}')
		}
		if i.rlabel == i.rvalue {
			eprintln('  right value: ${i.rlabel}')
		} else {
			eprintln('  right value: ${i.rlabel} = ${i.rvalue}')
		}
	}
	if i.has_msg {
		eprintln('      message: ${i.message}')
	}
}
