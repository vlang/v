import clipboard

fn run_test(is_primary bool) {
	mut cb := if is_primary { clipboard.new_primary() } else { clipboard.new() }
	if !cb.is_available() {
		return
	}
	assert cb.check_ownership() == false
	assert cb.copy('I am a good boy!') == true
	// assert cb.check_ownership() == true TODO
	assert cb.paste() == 'I am a good boy!'
	cb.clear_all()
	assert cb.paste().len <= 0
	cb.destroy()
}

fn test_primary() {
	$if linux || freebsd {
		// run_test(true)
		return
	}
}

fn test_clipboard() {
	$if linux || freebsd {
		return
	}
	run_test(false)
}
