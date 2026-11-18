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
	$if windows {
		u2 := lookup_id(u.uid)!
		assert u2.username == u.username
	} $else {
		if u.gid == '' {
			// current() fell back (no passwd entry for euid here),
			// so lookup_id cannot round-trip on this host
			return
		}
		u2 := lookup_id(u.uid)!
		assert u2.username == u.username
	}
}

fn test_lookup_id_rejects_malformed() {
	mut bad := ['', 'not-a-uid', '0suffix', '-1', '99999999999999999999']
	$if windows {
		bad = ['not-a-uid', '0', '12345']
	}
	for id in bad {
		if _ := lookup_id(id) {
			assert false, 'lookup_id(`${id}`) must fail'
		} else {
			assert err.msg().contains('uid')
		}
	}
}
