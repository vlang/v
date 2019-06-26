// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module time

pub fn now() Time {
	# time_t t = time(0);

	res := Time{}
	# res.sec = t;
	return res
}

pub fn sleep(t Time) {
	if t.sec > i64(0) || t.nsec > i32(0) {
		C.Sleep(i64(1000)*t.sec+(i64(t.nsec)+i64(999999))/i64(1000000))
	}
}

pub fn parse(s string) ?Time {
	return Time{} // TODO
}
