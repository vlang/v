module time

#flag -framework CoreServices
#include <CoreServices/CoreServices.h>
#include <math.h>
#include <mach/mach_time.h>
// in ms
fn ticks() double {
	// #return glfwGetTime() * 1000.0;
	// return glfw.get_time() * double(1000.0)
	t := i64(C.mach_absolute_time())
	# Nanoseconds elapsedNano = AbsoluteToNanoseconds( *(AbsoluteTime *) &t );
	# return (double)(* (uint64_t *) &elapsedNano) / 1000000;
	return double(0)
}

fn sleep(seconds int) {
	C.sleep(seconds)
}

fn usleep(seconds int) {
	C.usleep(seconds)
}

fn sleep_ms(seconds int) {
	C.usleep(seconds * 1000)
}

