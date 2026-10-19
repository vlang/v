module trace

import json2
import sync

fn test_tracer_aggregates_spans() {
	mut t := new_tracer()
	mut a := t.span('work')
	mut x := 0
	for _ in 0 .. 100000 {
		x += 1
	}
	_ = x
	a.end()
	a.end() // double end is a safe no-op
	mut b := t.span('work')
	b.end()
	rep := t.report()
	assert rep.len == 1
	assert rep[0].name == 'work'
	assert rep[0].count == 2, 'double end must not double-count'
	assert rep[0].total > 0, 'busy span must take measurable time'
	assert rep[0].max_ns <= rep[0].total
	assert rep[0].avg_ns == f64(rep[0].total) / 2.0
	assert t.event_count() == 2
}

fn test_tracer_nested_names_and_reset() {
	mut t := new_tracer()
	mut outer := t.span('outer')
	mut inner := t.span('inner')
	inner.end()
	outer.end()
	rep := t.report()
	assert rep.len == 2
	names := rep.map(it.name)
	assert 'outer' in names && 'inner' in names
	t.reset()
	assert t.report().len == 0
	assert t.event_count() == 0
}

fn test_tracer_chrome_export() {
	mut t := new_tracer()
	mut s := t.span('req "quoted"')
	s.end()
	out := t.to_chrome_trace()
	assert out.starts_with('{"traceEvents":[')
	assert out.ends_with(']}')
	assert out.contains('"ph":"X"')
	assert out.contains('"name":"req \\"quoted\\""'), 'names must be JSON-escaped'
	assert out.contains('"dur":')
	assert out.contains('"tid":')
}

fn test_tracer_concurrent_spans() {
	mut t := new_tracer()
	done_ch := chan bool{cap: 8}
	defer {
		done_ch.close()
	}
	for _ in 0 .. 8 {
		spawn fn [mut t, done_ch] () {
			for _ in 0 .. 25 {
				mut s := t.span('job')
				s.end()
			}
			done_ch <- true
		}()
	}
	for _ in 0 .. 8 {
		_ := <-done_ch
	}
	rep := t.report()
	assert rep.len == 1
	assert rep[0].count == 200, 'every span from every thread must be counted'
	assert t.event_count() == 200
}

fn test_chrome_export_preserves_control_bytes() {
	name := [u8(0), 1, 8, 9, 10, 12, 13, 31, 34, 92].bytestr()
	assert json_escape(name) == '\\u0000\\u0001\\u0008\\t\\n\\u000c\\r\\u001f\\"\\\\'
	mut t := new_tracer()
	mut s := t.span(name)
	s.end()
	assert t.to_chrome_trace().contains('"pid":')
	decoded := json2.parse(t.to_chrome_trace())!
	events := decoded.as_map()['traceEvents']!.as_array()
	assert events.len == 1
	assert events[0].as_map()['name']!.str() == name
}

fn test_span_keeps_starting_thread() {
	mut t := new_tracer()
	mut s := t.span('moved')
	starting_thread := sync.thread_id()
	assert s.tid == starting_thread
	done := spawn fn [mut s] () {
		s.end()
	}()
	done.wait()
	assert t.events.len == 1
	assert t.events[0].tid == starting_thread
}
