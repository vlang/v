import strconv

fn test_result_or_temp_preserves_u64_storage() {
	value := strconv.atou64('10737418240')!
	assert value == u64(10_737_418_240)
}
