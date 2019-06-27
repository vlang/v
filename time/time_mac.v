// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module time

// TODO: nanosecond and monotonic
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

// TODO: Thread safety
fn (t Time) local() Info {
	info := Info{}
	# struct tm *tm = localtime(&t.sec);
	# info.year = tm->tm_year + 1900;
	# info.month = tm->tm_mon + 1;
	# info.day = tm->tm_mday;
	# info.hour = tm->tm_hour;
	# info.minute = tm->tm_min;
	# info.second = tm->tm_sec;
	# info.yday = tm->tm_yday;
	# info.wday = tm->tm_wday;
	return info
}

// TODO: Thread safety
fn (t Time) utc() Info {
	info := Info{}
	# struct tm *tm = gmtime(&t.sec);
	# info.year = tm->tm_year + 1900;
	# info.month = tm->tm_mon + 1;
	# info.day = tm->tm_mday;
	# info.hour = tm->tm_hour;
	# info.minute = tm->tm_min;
	# info.second = tm->tm_sec;
	# info.yday = tm->tm_yday;
	# info.wday = tm->tm_wday;
	return info
}

// TODO: Thread safety
fn (t Time) format(fmt string) string {
	res := ''
	cfmt := fmt.cstr()
	# char buf[1024];
	# struct tm *tm = localtime(&t.sec);
	# strftime(buf, 1024, cfmt, tm);
	# res = tos2(buf);
	return res
}

// TODO: Not implemented yet
pub fn parse(s string) ?Time {
	return Time{}
}
