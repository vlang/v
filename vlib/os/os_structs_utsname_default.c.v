module os

$if !windows {
	#include <sys/utsname.h>
	#include <utime.h>
} $else {
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
}
