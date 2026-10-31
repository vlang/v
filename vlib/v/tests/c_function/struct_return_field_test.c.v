module main

#include "@VMODROOT/struct_return_field.h"

@[typedef]
struct C.VCallPoint {
	row    u32
	column u32
}

fn C.v_call_point(x int) C.VCallPoint
fn C.v_call_point_from_point(p C.VCallPoint) C.VCallPoint
fn C.VCallPointFactory(x int) C.VCallPoint
fn C.v_call_point_pointer(x int) &C.VCallPoint

fn test_c_call_returned_struct_field() {
	assert C.v_call_point(5).row == 5
	assert int(C.v_call_point(5).row) + 1 == 6
	assert C.v_call_point(5).column == 2
	assert C.VCallPointFactory(8).row == 8
	assert C.v_call_point_pointer(9).row == 9
	points := [C.v_call_point(3), C.v_call_point(7)]
	mut total := u32(0)
	for point in points {
		total += C.v_call_point_from_point(point).row
	}
	assert total == 10
}
