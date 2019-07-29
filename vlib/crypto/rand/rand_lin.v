module rand

// #include <unistd.h>
#include <sys/syscall.h>
#include <linux/random.h>

import const (
	SYS_getrandom
	GRND_NONBLOCK
)

pub fn read(bytes_needed int) ?[]byte {	
	mut buffer := malloc(bytes_needed)
	C.syscall(SYS_getrandom, buffer, bytes_needed, GRND_NONBLOCK)
	return new_array_from_c_array_no_alloc_rand(bytes_needed, 1, 1, buffer)
}
