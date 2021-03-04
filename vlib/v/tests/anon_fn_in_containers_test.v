fn test_anon_fn_in_map() {
	mut woop := map{
		'what': fn () string {
			return 'whoopity whoop'
		}
	}
	assert woop['what']() == 'whoopity whoop'

	woop['shat'] = fn () string {
		return 'shoopity shoop'
	}
	assert woop['shat']() == 'shoopity shoop'
}

fn test_anon_fn_in_array() {
	mut woop := [fn () string {
		return 'whoopity whoop'
	}]
	assert woop[0]() == 'whoopity whoop'

	woop[0] = fn () string {
		return 'shoopity shoop'
	}
	assert woop[0]() == 'shoopity shoop'
}

fn test_anon_fn_in_fixed_array() {
	mut woop := [fn () string {
		return 'whoopity whoop'
	}]!
	assert woop[0]() == 'whoopity whoop'

	woop[0] = fn () string {
		return 'shoopity shoop'
	}
	assert woop[0]() == 'shoopity shoop'
}
