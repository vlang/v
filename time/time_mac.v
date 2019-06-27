// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module time

//#flag -framework CoreServices
//#include <CoreServices/CoreServices.h>
//#include <mach/mach_time.h>

pub fn now() Time {
	# time_t t = time(0);

	res := Time{}
	# res.sec = t;
	return res
}

pub fn sleep(t Time) {
	if t.sec > i64(0) {
		C.sleep(t.sec)
	}
	if t.nsec > i32(0) {
		C.usleep((t.nsec+i32(999))/i32(1000))
	}
}

pub fn parse(s string) ?Time {
	return Time{} // TODO
}
