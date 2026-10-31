@[translated]
module main

enum RefHandle {
	invalid = -1
	first
	second
	third
}

type RefCount = int

fn ref_handle_index(obj &RefHandle) int {
	return int(unsafe { *obj })
}

fn ref_int_value(n &int) int {
	return unsafe { *n }
}

fn ref_f32_value(n &f32) f32 {
	return unsafe { *n }
}

fn ref_count_value(n &RefCount) int {
	return int(unsafe { *n })
}

fn test_cast_value_passed_to_a_reference_parameter() {
	mut total := 0
	for i := 0; i < 3; i++ {
		total += ref_handle_index(unsafe { RefHandle(i) })
	}
	assert total == 3
	big := i64(41)
	assert ref_int_value(int(big)) == 41
	assert ref_f32_value(f32(big)) == 41.0
	assert ref_count_value(RefCount(7)) == 7
}
