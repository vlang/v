module user

import os

#include <pwd.h>
#include <grp.h>

struct C.passwd {
	pw_name   &char
	pw_passwd &char
	pw_uid    int
	pw_gid    int
	pw_gecos  &char
	pw_dir    &char
	pw_shell  &char
}

struct C.group {
	gr_name   &char
	gr_passwd &char
	gr_gid    int
	gr_mem    &&char
}

fn C.getpwuid(int) &C.passwd
fn C.getpwnam(&char) &C.passwd
fn C.getgrgid(int) &C.group
fn C.getgrnam(&char) &C.group

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

fn make_group(gr &C.group) Group {
	unsafe {
		name := cstring_to_vstring(gr.gr_name)
		return Group{
			gid:  int(gr.gr_gid).str()
			name: name
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

pub fn lookup_group(name string) !Group {
	gr := C.getgrnam(name.str)
	if isnil(gr) {
		return error('user: unknown group ${name}')
	}
	return make_group(gr)
}

pub fn lookup_group_id(gid string) !Group {
	gid_int := gid.int()
	gr := C.getgrgid(gid_int)
	if isnil(gr) {
		return error('user: unknown gid ${gid}')
	}
	return make_group(gr)
}
