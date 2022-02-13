module os

struct C.utsname {
mut:
	sysname  &char
	nodename &char
	release  &char
	version  &char
	machine  &char
}

struct C.utimbuf {
	actime  int
	modtime int
}
