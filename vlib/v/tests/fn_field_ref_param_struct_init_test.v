struct FnFields {
	by_val fn (int) int  = unsafe { nil }
	by_ref fn (&int) int = unsafe { nil }
}

fn by_val(n int) int {
	return n + 1
}

fn by_ref(n &int) int {
	return *n + 1
}

fn test_inline_fn_field_with_ref_param_type_does_not_collide() {
	fields := FnFields{
		by_val: by_val
		by_ref: by_ref
	}
	value := 41
	assert fields.by_val(41) == 42
	assert fields.by_ref(&value) == 42
}
