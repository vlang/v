// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

struct C.tm {
	tm_year int
	tm_mon  int
	tm_mday int
	tm_hour int
	tm_min  int
	tm_sec  int
	tm_gmtoff int // seconds
}

fn C.timegm(&tm) time_t

fn make_unix_time(t C.tm) int {
	return int(C.timegm(&t))
}
