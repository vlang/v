import time

fn test_as_utc() {
	t := time.now()
	u := t.as_utc()
	dump(u)
	assert u.is_local == false
}

fn test_as_local() {
	t := time.now()
	l := t.as_local()
	dump(l)
	assert l.is_local == true
}

fn test_local_to_utc() {
	n := time.now()
	u := n.local_to_utc()
	dump(u)
	o := time.offset()
	dump(o)
	if o != 0 {
		assert n != u
		back := u.utc_to_local() // convert it back to local time
		assert n == back, 'the converted original local->utc->local time, should be the same as the original local time'

		double_u := u.local_to_utc()
		assert u == double_u, 'calling t.local_to_utc().local_to_utc() several times, should not change the time'
	}
}

fn test_utc_to_local() {
	z := time.Time{}
	assert z.is_local == false, 'simply constructing a time instance, should construct an UTC time'
	l := z.utc_to_local()
	dump(l)
	o := time.offset()
	dump(o)
	if o != 0 {
		assert z != l, 'when there is a time offset, the local time and the utc time should be different'
		assert l == l.utc_to_local(), 'converting a local to local time should not change the time'
		assert l == l.utc_to_local().utc_to_local(), 'double converting a local to local time to local time, should not change the time'
	}
	sz := z.format_rfc3339()
	dump(sz)
	assert sz == '0000-00-00T00:00:00.000Z'
}
