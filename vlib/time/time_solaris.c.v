module time

// dummy to compile with all compilers
pub fn linux_now() Time {
	return Time{}
}

pub fn darwin_now() Time {
	return Time{}
}

fn solaris_now() Time {
       // get the high precision time as UTC realtime clock
       // and use the nanoseconds part
       mut ts := C.timespec{}
       C.clock_gettime(C.CLOCK_REALTIME, &ts)
       t := C.time(0)
       tm := C.localtime(&t)
       // if the second part (very rare) is different
       // microseconds is set to zero since it passed the second
       // also avoid divide by zero if nsec is zero
       if int(t) != ts.tv_sec || ts.tv_nsec == 0 {
               return convert_ctime(tm, 0)
       }
       return convert_ctime(tm, int(ts.tv_nsec/1000))
}
