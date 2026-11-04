module user

fn test_current() {
	u := current()!
	assert u.username != ''
	assert u.home_dir != ''
}

fn test_lookup_current() {
	u := current()!
	u2 := lookup(u.username)!
	assert u2.username == u.username
}

fn test_lookup_unknown() {
	if _ := lookup('__definitely_not_a_user__') {
		assert false
	} else {
		assert err.msg().contains('unknown user')
	}
}

fn test_lookup_id_current() {
	u := current()!
	if u.uid != '' {
		u2 := lookup_id(u.uid)!
		assert u2.username == u.username
	}
}
