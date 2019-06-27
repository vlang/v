// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module time

pub fn make(i Info) ?Time {
	t := Time{}
	# struct tm tm;
	# tm.tm_year = i.year - 1900;
	# tm.tm_mon = i.month - 1;
	# tm.tm_mday = i.day;
	# tm.tm_hour = i.hour;
	# tm.tm_min = i.minute;
	# tm.tm_sec = i.second;
	# tm.tm_yday = i.yday;
	# tm.tm_wday = i.wday;
	# tm.tm_isdst = 0;
	# t.sec = mktime(&tm);
	if t.sec < i64(0) {
		return error('time.make: invalid time infomation')
	}
	return t
}

pub fn days(n int) Time {
	return Time{
		sec: 86400*n
	}
}

pub fn hours(n int) Time {
	return Time{
		sec: 3600*n
	}
}

pub fn minutes(n int) Time {
	return Time{
		sec: 60*n
	}
}

pub fn seconds(n int) Time {
	return Time{
		sec: n
	}
}

pub fn milliseconds(n int) Time {
	return Time{
		nsec: 1000000*n
	}
}

pub fn microseconds(n int) Time {
	return Time{
		nsec: 1000*n
	}
}

pub fn nanoseconds(n int) Time {
	return Time{
		nsec: n
	}
}
