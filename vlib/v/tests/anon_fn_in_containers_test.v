fn test_anon_fn_in_map() {
	mut woop := map{
		'what': fn() string {
			return 'whoopity whoop'
		}
	}
	assert woop['what']() == 'whoopity whoop'

	woop['shat'] = fn() string {
		return 'shoopity shoop'
	}
	assert woop['shat']() == 'shoopity shoop'
}
