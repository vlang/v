#include "@DIR/c_inferred_global_assignment_test.h"

// There is no V declaration for this lower-case C global. Every checker fork
// first reads it in a typed context, then uses its inferred type for assignments.
fn infer_global_from_return_type() f64 { return C.v_inferred_assignment_value }

fn expect_f64(value f64) f64 { return value }

fn set_inferred_global_first() {
	_ = expect_f64(C.v_inferred_assignment_value)
	C.v_inferred_assignment_value = f64(1.25)
}

fn set_inferred_global_second() {
	_ = expect_f64(C.v_inferred_assignment_value)
	C.v_inferred_assignment_value = f64(2.5)
}

fn set_inferred_global_third() {
	_ = expect_f64(C.v_inferred_assignment_value)
	C.v_inferred_assignment_value = f64(3.75)
}

fn set_inferred_global_again_after_other_bodies() {
	_ = expect_f64(C.v_inferred_assignment_value)
	C.v_inferred_assignment_value = f64(1.25)
}

fn read_inferred_global() f64 {
	_ = expect_f64(C.v_inferred_assignment_value)
	value := C.v_inferred_assignment_value
	return value
}

fn test_undeclared_c_global_assignments_across_function_bodies() {
	assert infer_global_from_return_type() == 0.0
	set_inferred_global_first()
	assert read_inferred_global() == 1.25
	set_inferred_global_second()
	assert read_inferred_global() == 2.5
	set_inferred_global_third()
	assert read_inferred_global() == 3.75
	set_inferred_global_again_after_other_bodies()
	assert read_inferred_global() == 1.25
}
