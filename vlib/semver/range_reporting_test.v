module semver

fn test_is_valid_range_accepts_a_real_range() {
	assert is_valid_range('^0.1.47')
	assert is_valid_range('>=0.4.0 <0.6.0')
	assert is_valid_range('*')
	assert is_valid_range('0.1.47')
	assert is_valid_range('~2.0.0')
}

fn test_is_valid_range_rejects_garbage() {
	// These are the shapes a typo produces, and they are exactly the ones
	// version_satisfies answers false for today.
	assert !is_valid_range('not-a-version')
	assert !is_valid_range('>= ')
	assert !is_valid_range('>>=1.0.0')
}

fn test_satisfies_or_error_agrees_with_satisfies_on_good_input() {
	v := semver.from('0.1.50') or { panic(err) }
	assert v.satisfies('^0.1.47') == true
	assert v.satisfies_or_error('^0.1.47') or { panic(err) } == true
	assert v.satisfies('^0.2.0') == false
	assert v.satisfies_or_error('^0.2.0') or { panic(err) } == false
}

fn test_satisfies_or_error_reports_an_unparseable_range() {
	v := semver.from('0.1.50') or { panic(err) }
	// satisfies says false, which is why a caller cannot tell this from a miss.
	assert v.satisfies('not-a-version') == false
	// satisfies_or_error says what actually happened.
	mut msg := ''
	v.satisfies_or_error('not-a-version') or { msg = err.msg() }
	assert msg.contains('not-a-version'), msg
}

fn test_satisfies_or_error_names_the_offending_input() {
	v := semver.from('0.1.50') or { panic(err) }
	mut msg := ''
	v.satisfies_or_error('>= ') or { msg = err.msg() }
	assert msg.contains('>= '), msg
}

fn test_the_empty_range_is_valid_and_matches_releases() {
	// Empty ranges are valid and follow the usual release/prerelease matching rules.
	v := semver.from('0.1.50') or { panic(err) }
	assert is_valid_range('')
	assert v.satisfies('') == true
	assert v.satisfies_or_error('') or { panic(err) } == true
	prerelease := semver.from('1.0.0-alpha') or { panic(err) }
	assert !prerelease.satisfies('')
	assert !prerelease.satisfies_or_error('') or { panic(err) }
}
