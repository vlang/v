module os

pub struct C.utsname {
mut:
	sysname  &char
	nodename &char
	release  &char
	version  &char
	machine  &char
}

pub struct C.utimbuf {
	actime  int
	modtime int
}
