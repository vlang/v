fn issue_28018_value(ok bool) !u32 {
	if ok {
		return 1
	}
	return error('failed')
}

fn value_with_platform_fallback(ok bool) u32 {
	value := issue_28018_value(ok) or {
		$if !windows {
			panic(err)
		} $else {
			u32(0)
		}
	}
	return value
}

fn test_or_block_with_platform_comptime_if() {
	assert value_with_platform_fallback(true) == 1
}
