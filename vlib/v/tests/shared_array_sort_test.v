module main

import time

struct Alarms {
mut:
	times shared []time.Time
}

fn (alarms Alarms) add(alarm time.Time) {
	lock alarms.times {
		alarms.times << alarm
		alarms.times.sort(a < b)
	}
}

fn test_sorting_shared_arrays() ? {
	alarms := Alarms{}
	utc := time.utc()
	alarms.add(utc)
	alarms.add(time.parse_iso8601('2022-03-01')?)
	alarms.add(time.parse_iso8601('3001-03-01')?)
	alarms.add(time.parse_iso8601('2002-03-01')?)
	alarms.add(time.parse_iso8601('3002-03-01')?)
	alarms.add(time.parse_iso8601('2021-03-01')?)
	println(alarms)
	lock alarms.times {
		assert alarms.times.len == 6
		assert alarms.times[0].year == 2002
		assert alarms.times.pop().year == 3002
		assert alarms.times.pop().year == 3001
		assert alarms.times.len == 4
	}
}
