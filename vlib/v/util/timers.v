// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module util

import time

[heap]
pub struct Timers {
pub mut:
	swatches     map[string]time.StopWatch
	should_print bool
}

pub fn new_timers(should_print bool) &Timers {
	return &Timers{
		swatches: map[string]time.StopWatch{}
		should_print: should_print
	}
}

const timers = new_timers(false)

pub fn get_timers() &Timers {
	return util.timers
}

pub fn timing_start(label string) {
	get_timers().start(label)
}

pub fn timing_measure(label string) {
	get_timers().show(label)
}

pub fn timing_measure_cumulative(label string) {
	get_timers().measure_cumulative(label)
}

pub fn timing_set_should_print(should_print bool) {
	mut t := util.timers
	t.should_print = should_print
}

pub fn (mut t Timers) start(name string) {
	mut sw := t.swatches[name] or { time.new_stopwatch({}) }
	sw.start()
	t.swatches[name] = sw
}

pub fn (mut t Timers) measure(name string) i64 {
	if name !in t.swatches {
		timer_keys := t.swatches.keys()
		eprintln('> Timer `$name` was NOT started.')
		eprintln('>   Available timers:')
		eprintln('>   $timer_keys')
	}
	ms := t.swatches[name].elapsed().microseconds()
	return ms
}

pub fn (mut t Timers) measure_cumulative(name string) i64 {
	ms := t.measure(name)
	if name !in t.swatches {
		return ms
	}
	mut sw := t.swatches[name]
	sw.pause()
	t.swatches[name] = sw
	return ms
}

pub fn (mut t Timers) measure_pause(name string) {
	if name !in t.swatches {
		return
	}
	mut sw := t.swatches[name]
	sw.pause()
	t.swatches[name] = sw
}

pub fn (mut t Timers) measure_resume(name string) {
	if name !in t.swatches {
		return
	}
	mut sw := t.swatches[name]
	sw.start()
	t.swatches[name] = sw
}

pub fn (mut t Timers) message(name string) string {
	ms := f64(t.measure(name)) / 1000.0
	value := bold('${ms:-8.3f}')
	formatted_message := '$value ms $name'
	return formatted_message
}

pub fn (mut t Timers) show(label string) {
	formatted_message := t.message(label)
	if t.should_print {
		println(formatted_message)
	}
}

pub fn (mut t Timers) show_if_exists(label string) {
	if label !in t.swatches {
		return
	}
	t.show(label)
}

pub fn (mut t Timers) dump_all() {
	for k, _ in t.swatches {
		elapsed := t.message(k)
		println(elapsed)
	}
}
