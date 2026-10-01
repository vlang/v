fn option_void_unref(h int, mut log []string) ? {
	if h < 0 {
		return none
	}
	log << 'unref ${h}'
}

fn option_void_wrap(h int, mut log []string) ? {
	return option_void_unref(h, mut log)
}

fn test_returning_an_option_void_call_passes_its_outcome_on() {
	mut log := []string{}
	mut failed := []int{}
	option_void_wrap(1, mut log) or { failed << 1 }
	option_void_wrap(-1, mut log) or { failed << -1 }
	assert log == ['unref 1']
	assert failed == [-1]
}
