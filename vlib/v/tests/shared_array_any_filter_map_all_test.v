module main

import time

struct Alarms {
mut:
	times shared []time.Time
}

fn (shared alarms Alarms) add(alarm time.Time) {
	lock alarms.times {
		println(alarms.times.filter(it == alarm))
		println(alarms.times.all(it == alarm))
		println(alarms.times.map(it.year))
		////
		if alarms.times.any(it == alarm) {
			return
		}
		alarms.times << alarm
	}
}

fn test_generated_shared_array_methods() {
	shared alarms := Alarms{
		times: []time.Time{cap: 10}
	}
	utc := time.utc()
	alarms.add(utc)
	alarms.add(utc)
	lock alarms.times {
		println(alarms.times)
		assert alarms.times.len == 1, 'length should be exactly 1'
	}
}
