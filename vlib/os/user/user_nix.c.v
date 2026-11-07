module user

import os

#include <pwd.h>

struct C.passwd {
	pw_name   &char
	pw_passwd &char
	pw_uid    int
	pw_gid    int
	pw_gecos  &char
	pw_dir    &char
	pw_shell  &char
}

fn C.getpwuid(int) &C.passwd
fn C.getpwnam(&char) &C.passwd

fn make_user(pw &C.passwd) User {
	unsafe {
		username := cstring_to_vstring(pw.pw_name)
		mut name := cstring_to_vstring(pw.pw_gecos)
		if name == '' {
			name = username
		}
		mut home := cstring_to_vstring(pw.pw_dir)
		if home == '' {
			home = os.home_dir()
		}
		return User{
			uid:      int(pw.pw_uid).str()
			gid:      int(pw.pw_gid).str()
			username: username
			name:     name
			home_dir: home
		}
	}
}

pub fn current() !User {
	uid := os.geteuid()
	pw := C.getpwuid(uid)
	if isnil(pw) {
		mut username := os.getenv('USER')
		if username == '' {
			username = os.getenv('LOGNAME')
		}
		if username == '' {
			username = os.loginname() or { '' }
		}
		home := os.home_dir()
		return User{
			uid:      uid.str()
			gid:      ''
			username: username
			name:     username
			home_dir: home
		}
	}
	return make_user(pw)
}

pub fn lookup(name string) !User {
	pw := C.getpwnam(name.str)
	if isnil(pw) {
		return error('user: unknown user ${name}')
	}
	return make_user(pw)
}

pub fn lookup_id(uid string) !User {
	uid_int := uid.int()
	pw := C.getpwuid(uid_int)
	if isnil(pw) {
		return error('user: unknown uid ${uid}')
	}
	return make_user(pw)
}
