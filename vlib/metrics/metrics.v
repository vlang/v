module metrics

import sync
import math

// Labels are a metric's dimension set, e.g. {'route': '/api/users'}.
// label_key renders a canonical encoding so equal label sets share a series.
fn label_key(labels map[string]string) string {
	mut keys := labels.keys()
	keys.sort()
	mut out := []string{}
	for k in keys {
		assert valid_name(k, false), 'metrics: invalid label name ${k}'
		out << '${k}="${escape_label(labels[k])}"'
	}
	return out.join(',')
}

// SeriesId names one labeled series of an instrument.
struct SeriesId {
	name   string
	labels string
}

fn series_map_key(id SeriesId) string {
	return id.name + '\x00' + id.labels
}

// Counter is a monotonically increasing cumulative counter.
@[heap]
pub struct Counter {
mut:
	mutex &sync.Mutex = sync.new_mutex()
	vals  map[string]f64
}

// Gauge is a point-in-time value that goes up and down.
@[heap]
pub struct Gauge {
mut:
	mutex &sync.Mutex = sync.new_mutex()
	vals  map[string]f64
}

// Histogram accumulates observations into fixed buckets plus sum and count,
// in the Prometheus exposition model.
@[heap]
pub struct Histogram {
mut:
	mutex   &sync.Mutex = sync.new_mutex()
	bounds  []f64
	buckets map[string][]u64
	sums    map[string]f64
	counts  map[string]u64
}

// Registry owns the instruments; create instruments through it so names stay
// namespaced in one place.
@[heap]
pub struct Registry {
mut:
	mutex      &sync.Mutex = sync.new_mutex()
	counters   map[string]&Counter
	gauges     map[string]&Gauge
	histograms map[string]&Histogram
}

// new_registry creates an empty Registry.
pub fn new_registry() &Registry {
	return &Registry{}
}

// counter returns the named counter, creating it on first use.
pub fn (mut r Registry) counter(name string) &Counter {
	assert valid_name(name, true), 'metrics: invalid instrument name ${name}'
	r.mutex.lock()
	defer { r.mutex.unlock() }
	for existing in r.histograms.keys() {
		assert name !in [existing + '_bucket', existing + '_sum', existing + '_count'], 'metrics: instrument name conflicts with histogram samples'
	}
	assert name !in r.gauges, 'metrics: instrument name already used'
	assert name !in r.histograms, 'metrics: instrument name already used'
	if name !in r.counters {
		r.counters[name] = &Counter{
			mutex: sync.new_mutex()
		}
	}
	return r.counters[name] or { panic('metrics: missing counter') }
}

// gauge returns the named gauge, creating it on first use.
pub fn (mut r Registry) gauge(name string) &Gauge {
	assert valid_name(name, true), 'metrics: invalid instrument name ${name}'
	r.mutex.lock()
	defer { r.mutex.unlock() }
	for existing in r.histograms.keys() {
		assert name !in [existing + '_bucket', existing + '_sum', existing + '_count'], 'metrics: instrument name conflicts with histogram samples'
	}
	assert name !in r.counters, 'metrics: instrument name already used'
	assert name !in r.histograms, 'metrics: instrument name already used'
	if name !in r.gauges {
		r.gauges[name] = &Gauge{
			mutex: sync.new_mutex()
		}
	}
	return r.gauges[name] or { panic('metrics: missing gauge') }
}

// histogram returns the named histogram with the given bucket upper bounds
// (must be finite and strictly ascending), creating it on first use.
// Reusing a name requires the same bounds. Invalid configuration panics.
pub fn (mut r Registry) histogram(name string, bounds []f64) &Histogram {
	assert valid_name(name, true), 'metrics: invalid instrument name ${name}'
	r.mutex.lock()
	defer { r.mutex.unlock() }
	for existing in r.histograms.keys() {
		assert name !in [existing + '_bucket', existing + '_sum', existing + '_count'], 'metrics: instrument name conflicts with histogram samples'
	}
	assert name !in r.counters, 'metrics: instrument name already used'
	assert name !in r.gauges, 'metrics: instrument name already used'
	for suffix in ['_bucket', '_sum', '_count'] {
		sample := name + suffix
		assert sample !in r.counters && sample !in r.gauges && sample !in r.histograms, 'metrics: histogram sample name already used'
	}
	for i, bound in bounds {
		assert !math.is_nan(bound) && !math.is_inf(bound, 0), 'metrics: bucket bounds must be finite'
		assert i == 0 || bound > bounds[i - 1], 'metrics: bucket bounds must be strictly ascending'
	}
	if existing := r.histograms[name] {
		assert existing.bounds == bounds, 'metrics: bucket bounds differ'
	}
	if name !in r.histograms {
		r.histograms[name] = &Histogram{
			mutex:  sync.new_mutex()
			bounds: bounds.clone()
		}
	}
	return r.histograms[name] or { panic('metrics: missing histogram') }
}

// add increments the labeled series by v. Negative or NaN increments panic;
// use a gauge for values that decrease.
pub fn (mut c Counter) add(v f64, labels map[string]string) {
	assert v >= 0, 'metrics: counter increments must be nonnegative'
	k := series_map_key(SeriesId{'', label_key(labels)})
	c.mutex.lock()
	defer {
		c.mutex.unlock()
	}
	c.vals[k] = c.vals[k] + v
}

// get returns the current value of the labeled series (0 if absent).
pub fn (c &Counter) get(labels map[string]string) f64 {
	k := series_map_key(SeriesId{'', label_key(labels)})
	c.mutex.lock()
	defer {
		c.mutex.unlock()
	}
	return c.vals[k]
}

// set assigns the labeled series.
pub fn (mut g Gauge) set(v f64, labels map[string]string) {
	k := series_map_key(SeriesId{'', label_key(labels)})
	g.mutex.lock()
	defer {
		g.mutex.unlock()
	}
	g.vals[k] = v
}

// inc adds v to the labeled series.
pub fn (mut g Gauge) inc(v f64, labels map[string]string) {
	k := series_map_key(SeriesId{'', label_key(labels)})
	g.mutex.lock()
	defer {
		g.mutex.unlock()
	}
	g.vals[k] = g.vals[k] + v
}

// get returns the current value of the labeled series (0 if absent).
pub fn (g &Gauge) get(labels map[string]string) f64 {
	k := series_map_key(SeriesId{'', label_key(labels)})
	g.mutex.lock()
	defer {
		g.mutex.unlock()
	}
	return g.vals[k]
}

// record adds one observation to the labeled series' buckets, sum and count.
pub fn (mut h Histogram) record(v f64, labels map[string]string) {
	assert 'le' !in labels, 'metrics: histogram label le is reserved'
	k := series_map_key(SeriesId{'', label_key(labels)})
	h.mutex.lock()
	defer {
		h.mutex.unlock()
	}
	mut counts := h.buckets[k]
	if counts.len == 0 {
		counts = []u64{len: h.bounds.len + 1}
	}
	for i, b in h.bounds {
		if v <= b {
			counts[i]++
		}
	}
	counts[counts.len - 1]++ // +Inf bucket
	h.buckets[k] = counts
	h.sums[k] = h.sums[k] + v
	h.counts[k] = h.counts[k] + 1
}

// bucket_counts returns the cumulative per-bucket counts of the labeled
// series, plus one trailing +Inf entry.
pub fn (h &Histogram) bucket_counts(labels map[string]string) []u64 {
	k := series_map_key(SeriesId{'', label_key(labels)})
	h.mutex.lock()
	defer {
		h.mutex.unlock()
	}
	return h.buckets[k].clone()
}

// sum returns the sum of observations of the labeled series.
pub fn (h &Histogram) sum(labels map[string]string) f64 {
	k := series_map_key(SeriesId{'', label_key(labels)})
	h.mutex.lock()
	defer {
		h.mutex.unlock()
	}
	return h.sums[k]
}

// count returns the number of observations of the labeled series.
pub fn (h &Histogram) count(labels map[string]string) u64 {
	k := series_map_key(SeriesId{'', label_key(labels)})
	h.mutex.lock()
	defer {
		h.mutex.unlock()
	}
	return h.counts[k]
}

// to_prometheus renders all series in Prometheus text exposition format:
// one HELP/TYPE header pair per instrument plus one line per labeled series.
pub fn (mut r Registry) to_prometheus() string {
	r.mutex.lock()
	defer { r.mutex.unlock() }
	mut out := []string{}
	for name in r.counters.keys() {
		mut c := r.counters[name] or { continue }
		out << '# HELP ${name} counter'
		out << '# TYPE ${name} counter'
		c.mutex.lock()
		out << r.render_samples(name, c.vals)
		c.mutex.unlock()
	}
	for name in r.gauges.keys() {
		mut g := r.gauges[name] or { continue }
		out << '# HELP ${name} gauge'
		out << '# TYPE ${name} gauge'
		g.mutex.lock()
		out << r.render_samples(name, g.vals)
		g.mutex.unlock()
	}
	for name in r.histograms.keys() {
		mut h := r.histograms[name] or { continue }
		out << '# HELP ${name} histogram'
		out << '# TYPE ${name} histogram'
		h.mutex.lock()
		keys := h.buckets.keys()
		for k in keys {
			lbl := render_labels(k)
			prefix := if lbl == '' { '' } else { lbl + ',' }
			counts := h.buckets[k]
			for i, b in h.bounds {
				out << '${name}_bucket{${prefix}${bucket_le(b)}} ${counts[i]}'
			}
			out << '${name}_bucket{${prefix}${bucket_le_inf()}} ${counts[counts.len - 1]}'
			out << '${name}_sum{${lbl}} ${sample_value(h.sums[k])}'
			out << '${name}_count{${lbl}} ${h.counts[k]}'
		}
		h.mutex.unlock()
	}
	return out.join('\n') + '\n'
}

// render_samples renders counter/gauge series lines.
fn (mut r Registry) render_samples(name string, vals map[string]f64) string {
	mut out := []string{}
	for k, v in vals {
		lbl := render_labels(k)
		if lbl == '' {
			out << '${name} ${sample_value(v)}'
		} else {
			out << '${name}{${lbl}} ${sample_value(v)}'
		}
	}
	return out.join('\n')
}

// render_labels extracts canonical, already escaped `k="v"` pairs.
fn render_labels(k string) string {
	return k.all_after('\x00')
}

fn escape_label(value string) string {
	return value.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n')
}

fn valid_name(name string, metric bool) bool {
	if name == '' { return false }
	for i, ch in name {
		if (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`) || ch == `_`
			|| (metric && ch == `:`) || (i > 0 && ch >= `0` && ch <= `9`) {
			continue
		}
		return false
	}
	return true
}

fn sample_value(value f64) string {
	if math.is_nan(value) { return 'NaN' }
	if math.is_inf(value, 1) { return '+Inf' }
	if math.is_inf(value, -1) { return '-Inf' }
	return value.str()
}

// bucket_le renders the le="bound" label fragment.
fn bucket_le(b f64) string {
	return 'le="${b}"'
}

// bucket_le_inf renders the le="+Inf" label fragment.
fn bucket_le_inf() string {
	return 'le="+Inf"'
}
