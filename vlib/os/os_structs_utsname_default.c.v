module os

$if !windows {
	#include <sys/utsname.h>
	#include <utime.h>
}

// C.utsname/C.utimbuf are declared here for the V checker; the C compiler uses the real headers on non-Windows.
pub struct C.utsname {
mut:
	sysname  &char
	nodename &char
	release  &char
	version  &char
	machine  &char
}

pub struct C.utimbuf {
	actime  i64
	modtime i64
}
