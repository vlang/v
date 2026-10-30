// Module user provides user account information, similar to Go's os/user.
module user

import os

// User represents a user account.
pub struct User {
pub:
	uid      string
	gid      string
	username string
	name     string
	home_dir string
}

// current returns the current user. On Unix, uid/gid are populated from the
// `id` command when available; on Windows they are left empty because the
// platform has no portable equivalent. Username and home directory come from
// the environment and os.home_dir().
pub fn current() !User {
	mut username := os.getenv('USERNAME')
	if username == '' {
		username = os.getenv('USER')
	}
	if username == '' {
		username = os.getenv('LOGNAME')
	}
	home := os.home_dir()
	mut uid := ''
	mut gid := ''
	$if windows {
	} $else {
		// best-effort: ask the system
		res := os.execute('id -u')
		if res.exit_code == 0 {
			uid = res.output.trim_space()
		}
		res2 := os.execute('id -g')
		if res2.exit_code == 0 {
			gid = res2.output.trim_space()
		}
	}
	return User{
		uid:      uid
		gid:      gid
		username: username
		name:     username
		home_dir: home
	}
}

// lookup looks up a user by name. Only the current user is supported portably;
// other names return an error.
pub fn lookup(name string) !User {
	u := current()!
	if u.username != name {
		return error('user: unknown user ${name}')
	}
	return u
}

// lookup_id looks up a user by uid. Only the current uid is supported portably.
pub fn lookup_id(uid string) !User {
	u := current()!
	if u.uid != '' && u.uid != uid {
		return error('user: unknown uid ${uid}')
	}
	return u
}
