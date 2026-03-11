// Tests for ARM64 backend large struct returns (> 16 bytes)
// This tests the fix from commit 9919ddeb5 which handles:
// - Saving/restoring x8 (indirect return pointer) for large struct returns
// - Proper stack frame layout with callee-saved registers
// - Large struct parameter copying
// - String operations (string is 24 bytes: ptr + len + is_lit)

fn print_str(s string) {
	C.write(1, s.str, s.len)
	C.write(1, c'\n', 1)
}

// ===================== FUNCTIONS RETURNING STRINGS =====================

// Test 1: Return a string literal (24 bytes indirect return)
fn get_hello() string {
	return 'hello'
}

// Test 2: Return concatenated string
fn get_greeting(name string) string {
	return 'Hello, ' + name
}

// Test 3: Nested string returns
fn get_formal_greeting(name string) string {
	greeting := get_greeting(name)
	return greeting + '!'
}

// Test 4: Multiple string concatenations
fn concat_three(a string, b string, c string) string {
	return a + b + c
}

// Test 5: String function calling another string function
fn get_full_message() string {
	greeting := get_hello()
	suffix := ' world'
	return greeting + suffix
}

// Test 6: Chain of string returns
fn chain_a() string {
	return 'A'
}

fn chain_b() string {
	return chain_a() + 'B'
}

fn chain_c() string {
	return chain_b() + 'C'
}

// Test 7: Conditional string return
fn conditional_string(flag bool) string {
	if flag {
		return 'true_case'
	}
	return 'false_case'
}

// ===================== MAIN =====================

fn main() {
	print_str('--- String Return Tests (Large Struct > 16 bytes) ---')

	// Test 1: Basic string literal return
	s1 := get_hello()
	print_str(s1)

	// Test 2: String concatenation return
	s2 := get_greeting('Alice')
	print_str(s2)

	// Test 3: Nested string function calls
	s3 := get_formal_greeting('Bob')
	print_str(s3)

	// Test 4: Multiple parameters concatenation
	s4 := concat_three('One', '-Two-', 'Three')
	print_str(s4)

	// Test 5: Function calling function returning strings
	s5 := get_full_message()
	print_str(s5)

	// Test 6: Chain of string returns
	s6 := chain_c()
	print_str(s6)

	// Test 7: Conditional string returns
	s7a := conditional_string(true)
	s7b := conditional_string(false)
	print_str(s7a)
	print_str(s7b)

	// Test 8: Direct string operations
	direct := 'Direct' + ' ' + 'Test'
	print_str(direct)

	// Test 9: Interpolation with string return
	name := 'Claude'
	interp := 'Name: ${name}'
	print_str(interp)

	// Test 10: Chained string concat expression
	chained := 'A' + 'B' + 'C' + 'D' + 'E'
	print_str(chained)

	print_str('--- All tests completed ---')
}
