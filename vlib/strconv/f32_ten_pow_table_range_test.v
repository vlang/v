module strconv

fn test_issue_20877_ten_pow_table_32_stays_in_u32_range() {
	assert ten_pow_table_32.len == 10
	assert ten_pow_table_32[ten_pow_table_32.len - 1] == u32(1_000_000_000)
}
