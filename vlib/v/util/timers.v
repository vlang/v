// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
@[has_globals]
module util

import time

__global g_timers = new_timers(should_print: false, label: 'g_timers')

@[heap]
pub struct Timers {
	label string
pub mut:
	swatches     shared map[string]time.StopWatch
	should_print bool
	// already_shown records for which of the swatches .show() or .show_if_exists() had been called already
	already_shown []string
}

@[params]
pub struct TimerParams {
pub:
	should_print bool
	label        string
}

pub fn new_timers(params TimerParams) &Timers {
	$if trace_timers_creation ? {
		eprintln('>>>> new_timers, should_print: ${params.should_print} | label: ${params.label}')
	}
	return &Timers{
		label:         params.label
		swatches:      map[string]time.StopWatch{}
		should_print:  params.should_print
		already_shown: []string{cap: 100}
	}
}

pub fn get_timers() &Timers {
	return g_timers
}

pub fn timing_start(label string) {
	mut t := get_timers()
	t.start(label)
}

pub fn timing_measure(label string) {
	g_timers.show(label)
}

pub fn timing_measure_cumulative(label string) {
	g_timers.measure_cumulative(label)
}

pub fn timing_set_should_print(should_print bool) {
	g_timers.should_print = should_print
}

pub fn (mut t Timers) start(name string) {
	mut sw := t.tsafe_get_sw(name) or { time.new_stopwatch() }
	sw.start()
	t.tsafe_set_sw(name, sw)
}

pub fn (mut t Timers) measure(name string) i64 {
	mut sw := t.tsafe_get_sw(name) or {
		timer_keys := t.tsafe_get_keys()
		eprintln('> Timer `${name}` was NOT started.')
		eprintln('>   Available timers:')
		eprintln('>   ${timer_keys}')
		time.new_stopwatch()
	}
	ms := sw.elapsed().microseconds()
	sw.pause()
	t.tsafe_set_sw(name, sw)
	return ms
}

pub fn (mut t Timers) measure_cumulative(name string) i64 {
	ms := t.measure(name)
	mut sw := t.tsafe_get_sw(name) or { return ms }
	sw.pause()
	t.tsafe_set_sw(name, sw)
	return ms
}

pub fn (mut t Timers) measure_pause(name string) {
	mut sw := t.tsafe_get_sw(name) or { return }
	sw.pause()
	t.tsafe_set_sw(name, sw)
}

pub fn (mut t Timers) measure_resume(name string) {
	mut sw := t.tsafe_get_sw(name) or { return }
	sw.start()
	t.tsafe_set_sw(name, sw)
}

pub fn (mut t Timers) message(name string) string {
	ms := f64(t.measure(name)) / 1000.0
	value := bold('${ms:-8.3f}')
	formatted_message := '${value} ms ${name}'
	return formatted_message
}

pub fn (mut t Timers) show(label string) {
	if v_memory_panic {
		// Showing timers uses string interpolation, and allocating in this case
		// will just cause a second panic to be shown; it is better to just not show anything
		return
	}
	if t.should_print {
		formatted_message := t.message(label)
		println(formatted_message)
	}
	t.already_shown << label
}

pub fn (mut t Timers) show_if_exists(label string) {
	t.tsafe_get_sw(label) or { return }
	t.show(label)
	t.already_shown << label
}

pub fn (mut t Timers) show_remaining() {
	keys := t.tsafe_get_keys()
	for k in keys {
		if k in t.already_shown {
			continue
		}
		t.show(k)
	}
}

pub fn (mut t Timers) dump_all() {
	keys := t.tsafe_get_keys()
	for k in keys {
		elapsed := t.message(k)
		println(elapsed)
	}
}

//

fn (mut t Timers) tsafe_get_keys() []string {
	keys := rlock t.swatches {
		t.swatches.keys()
	}
	return keys
}

fn (mut t Timers) tsafe_set_sw(name string, sw time.StopWatch) {
	lock t.swatches {
		t.swatches[name] = sw
	}
}

fn (mut t Timers) tsafe_get_sw(name string) ?time.StopWatch {
	rlock t.swatches {
		if name !in t.swatches {
			return none
		}
		return t.swatches[name]
	}
	return none
}
