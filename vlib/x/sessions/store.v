module sessions

import time

pub interface Store[T] {
mut:
	// get the current session data if the id exists and if it's not expired
	get(sid string, max_age time.Duration) !T
	// destroy session data for `sid`
	destroy(sid string) !
	// set session data for `val`
	set(sid string, val T) !
}

// get data from all sessions, optional to implement.
pub fn (mut s Store) all[T]() ![]T {
	return []T{}
}

// clear all session data, optional to implement.
pub fn (mut s Store) clear[T]() ! {}
