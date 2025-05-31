fn foo() int {
	return 1
}

fn zoo() int {
	return 123
}

fn test_dump_of_functions() {
	x := dump(foo)
	y := dump(zoo)
	dump(foo())
	dump(zoo())
	dump(x)
	dump(y)
	dump(x())
	dump(y())
	assert voidptr(x) != 0
	assert voidptr(y) != 0
	assert foo == x
	assert y == zoo
}

//

struct StructWithStrMethodTakingReference {
	x int
}

pub fn (t &StructWithStrMethodTakingReference) str() string {
	return 'StructWithStrMethodTakingReference{x: ${t.x}}'
}

fn test_dump_of_type_that_has_custom_str_method_with_reference_parameter() {
	s := StructWithStrMethodTakingReference{123}
	assert dump(s).x == 123
	ps := &StructWithStrMethodTakingReference{456}
	assert dump(ps).x == 456
}

//

struct StructWithNormalStrMethod {
	x int
}

pub fn (t StructWithNormalStrMethod) str() string {
	return 'StructWithNormalStrMethod{x: ${t.x}}'
}

fn test_dump_of_type_that_has_normal_custom_str_method() {
	s := StructWithNormalStrMethod{123}
	assert dump(s).x == 123
	ps := &StructWithNormalStrMethod{456}
	assert dump(ps).x == 456
}

//

struct StructWithoutStrMethod {
	x int
}

fn test_dump_of_type_that_has_no_custom_str_method() {
	s := StructWithoutStrMethod{123}
	assert dump(s).x == 123
	ps := &StructWithoutStrMethod{456}
	assert dump(ps).x == 456
}

fn test_nil_values_and_voidptr_values_can_be_dumped_in_the_same_program() {
	// Note, that nil is its own type in the main v repo,
	// while dump() generates `_v_dump_expr_voidptr` for *both* `nil` and `voidptr` values.
	a := unsafe { nil }
	b := voidptr(456)
	dump(a)
	dump(b)
	assert true
}
