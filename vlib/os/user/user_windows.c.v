module user

import os

pub fn current() !User {
	mut username := os.loginname() or { '' }
	if username == '' {
		username = os.getenv('USERNAME')
	}
	if username == '' {
		username = os.getenv('USER')
	}
	if username == '' {
		username = os.getenv('LOGNAME')
	}
	home := os.home_dir()
	return User{
		uid:      ''
		gid:      ''
		username: username
		name:     username
		home_dir: home
	}
}

pub fn lookup(name string) !User {
	u := current()!
	if u.username != name {
		return error('user: unknown user ${name}')
	}
	return u
}

pub fn lookup_id(uid string) !User {
	u := current()!
	// current() reports uid '' on Windows (no numeric account IDs are
	// exposed), so compare directly like lookup() does: only the known
	// uid matches, every other requested ID is unknown.
	if u.uid != uid {
		return error('user: unknown uid ${uid}')
	}
	return u
}
