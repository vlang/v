module trace

import os
import sync
import time

// SpanStats aggregates one span name: how often it ran and how long it took,
// in nanoseconds of the monotonic clock.
pub struct SpanStats {
mut:
	count    u64
	total_ns u64
	max_ns   u64
}

// SpanReport is one row of Tracer.report, sorted by total time descending.
pub struct SpanReport {
pub:
	name   string
	count  u64
	total  u64
	avg_ns f64
	max_ns u64
}

// TraceEvent is one completed span for Chrome-trace export.
struct TraceEvent {
	name     string
	start_us u64
	dur_us   u64
	tid      u64
}

// Tracer collects explicitly instrumented spans (Tracy-zones style) across
// threads: name aggregates plus a Chrome-trace event log viewable in
// Perfetto or chrome://tracing. It answers "where is my server spending
// time" at function granularity. Kernel-level sampling is deliberately out
// of scope here (it needs a signal/sampler design of its own).
@[heap]
pub struct Tracer {
mut:
	mutex  &sync.Mutex
	stats  map[string]SpanStats
	events []TraceEvent
}

// new_tracer creates an empty Tracer.
pub fn new_tracer() &Tracer {
	return &Tracer{
		mutex: sync.new_mutex()
	}
}

// Span is one open measurement. End each instance exactly once; repeating end on
// the same instance is a safe no-op. Copies have independent completion state.
// Spans are values, so nesting is the caller's structure and needs
// no thread-local stack: concurrent spans from any threads aggregate by
// name under the tracer mutex.
pub struct Span {
mut:
	tracer &Tracer = unsafe { nil }
	name   string
	start  u64
	tid    u64
	ended  bool
}

// span opens a measurement with the monotonic clock.
pub fn (mut t Tracer) span(name string) Span {
	return Span{t, name, time.sys_mono_now(), thread_id_here(), false}
}

// end closes the measurement, attributing the elapsed time to its name.
@[unsafe]
pub fn (mut s Span) end() {
	if s.ended || s.tracer == unsafe { nil } {
		return
	}
	s.ended = true
	dur := time.sys_mono_now() - s.start
	s.tracer.record(s.name, s.start, dur, s.tid)
}

// record aggregates one completed span plus its trace event.
fn (mut t Tracer) record(name string, start_ns u64, dur_ns u64, tid u64) {
	t.mutex.lock()
	defer {
		t.mutex.unlock()
	}
	mut st := t.stats[name] or { SpanStats{} }
	st.count++
	st.total_ns += dur_ns
	if dur_ns > st.max_ns {
		st.max_ns = dur_ns
	}
	t.stats[name] = st
	t.events << TraceEvent{name, start_ns / 1000, dur_ns / 1000, tid}
}

// thread_id_here attributes the calling thread. sync.thread_id is used when
// available; the indirection keeps a single call site.
fn thread_id_here() u64 {
	return sync.thread_id()
}

// report returns per-name aggregates sorted by total time descending.
pub fn (t &Tracer) report() []SpanReport {
	t.mutex.lock()
	defer {
		t.mutex.unlock()
	}
	mut out := []SpanReport{}
	for name, st in t.stats {
		avg := if st.count == 0 { 0.0 } else { f64(st.total_ns) / f64(st.count) }
		out << SpanReport{name, st.count, st.total_ns, avg, st.max_ns}
	}
	out.sort(a.total > b.total)
	return out
}

// event_count returns the number of recorded trace events.
pub fn (t &Tracer) event_count() int {
	t.mutex.lock()
	defer {
		t.mutex.unlock()
	}
	return t.events.len
}

// reset drops all aggregates and events.
pub fn (mut t Tracer) reset() {
	t.mutex.lock()
	defer {
		t.mutex.unlock()
	}
	t.stats = map[string]SpanStats{}
	t.events = []TraceEvent{}
}

// to_chrome_trace renders the event log in Chrome trace-event JSON:
// {"traceEvents": [{"name":...,"ph":"X","ts":us,"dur":us,"tid":..}, ...]}.
pub fn (t &Tracer) to_chrome_trace() string {
	t.mutex.lock()
	defer {
		t.mutex.unlock()
	}
	mut parts := []string{cap: t.events.len}
	for e in t.events {
		parts << '{"name":"${json_escape(e.name)}","ph":"X","ts":${e.start_us},"dur":${e.dur_us},"tid":${e.tid},"pid":${os.getpid()}}'
	}
	return '{"traceEvents":[' + parts.join(',') + ']}'
}

// json_escape escapes a span name for the JSON export.
fn json_escape(s string) string {
	mut b := []u8{cap: s.len + 8}
	for c in s.bytes() {
		match c {
			u8(34) {
				b << u8(92)
				b << u8(34)
			}
			u8(92) {
				b << u8(92)
				b << u8(92)
			}
			10 {
				b << u8(92)
				b << u8(110)
			}
			13 {
				b << u8(92)
				b << u8(114)
			}
			9 {
				b << u8(92)
				b << u8(116)
			}
			else {
				if c < 0x20 {
					b << '\\u00'.bytes()
					b << '0123456789abcdef'[c >> 4]
					b << '0123456789abcdef'[c & 0x0f]
				} else {
					b << c
				}
			}
		}
	}
	return b.bytestr()
}
