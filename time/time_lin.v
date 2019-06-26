// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module time

pub fn now() Time {
	# struct timespec t = {0, 0};
	# struct timespec m = {0, 0};
	# clock_gettime(CLOCK_REALTIME, &t);
	# clock_gettime(CLOCK_MONOTONIC_RAW, &m);

	res := Time{}
	# res.sec = t.tv_sec;
	# res.nsec = t.tv_nsec;
	# res.mono = 1000000000*m.tv_sec + m.tv_nsec;
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

pub fn parse(s, fmt string) ?Time {
	t := Time{}
	cs := s.cstr()
	cfmt := fmt.cstr()
	ok := 0
	# struct tm tm;
	# memset(&tm, 0, sizeof(struct tm));
	# ok = strptime(cs, cfmt, &tm);
	if ok == 0 {
		return error('time.parse: invalid time string')
	}
	# t.sec = mktime(&tm);
	if t.sec < i64(0) {
		return error('time.parse: invalid time string')
	}
	return t
}
