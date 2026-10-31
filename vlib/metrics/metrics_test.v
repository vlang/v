module metrics

import math
import os

fn test_counter_aggregation() {
	mut r := new_registry()
	mut c := r.counter('http_requests_total')
	c.add(1.0, {
		'route': '/api/users'
	})
	c.add(2.0, {
		'route': '/api/users'
	})
	c.add(1.0, {
		'route': '/health'
	})
	assert c.get({
		'route': '/api/users'
	}) == 3.0
	assert c.get({
		'route': '/health'
	}) == 1.0
	assert c.get({
		'route': '/missing'
	}) == 0.0
	// key order must not matter
	assert c.get({
		'route': '/api/users'
		'extra': 'x'
	}) == 0.0
}

fn test_gauge_levels() {
	mut r := new_registry()
	mut g := r.gauge('queue_depth')
	g.set(5.0, {})
	g.inc(-2.0, {})
	assert g.get({}) == 3.0
	g.inc(10.0, {
		'queue': 'a'
	})
	assert g.get({
		'queue': 'a'
	}) == 10.0
	assert g.get({}) == 3.0, 'label sets must stay separate'
}

fn test_histogram_buckets() {
	mut r := new_registry()
	mut h := r.histogram('http_latency_ms', [5.0, 10.0, 50.0])
	h.record(3.0, {
		'route': '/x'
	})
	h.record(7.0, {
		'route': '/x'
	})
	h.record(100.0, {
		'route': '/x'
	})
	assert h.bucket_counts({
		'route': '/x'
	}) == [u64(1), 2, 2, 3], 'cumulative buckets plus +Inf'
	assert h.sum({
		'route': '/x'
	}) == 110.0
	assert h.count({
		'route': '/x'
	}) == 3
}

fn test_prometheus_exposition() {
	mut r := new_registry()
	mut c := r.counter('reqs')
	c.add(2.0, {
		'route': '/a'
	})
	mut g := r.gauge('temp')
	g.set(21.5, {})
	out := r.to_prometheus()
	assert out.contains('# TYPE reqs counter')
	assert out.contains('reqs{route="/a"} 2')
	assert out.contains('# TYPE temp gauge')
	assert out.contains('temp 21.5')
	mut h := r.histogram('lat', [10.0])
	h.record(5.0, {})
	prom := r.to_prometheus()
	assert prom.contains('lat_bucket{le="10.0"} 1')
	assert prom.contains('lat_bucket{le="+Inf"} 1')
	assert prom.contains('lat_sum{} 5')
	assert prom.contains('lat_count{} 1')
}

fn test_labels_are_unambiguous_and_escaped() {
	mut r := new_registry()
	mut c := r.counter('requests')
	c.add(3, {
		'a': 'x,b=y'
	})
	c.add(4, {
		'a': 'x'
		'b': 'y'
	})
	assert c.get({
		'a': 'x,b=y'
	}) == 3
	assert c.get({
		'b': 'y'
		'a': 'x'
	}) == 4
	mut g := r.gauge('escaped')
	g.set(1, {
		'route': 'line\n"quoted"\\end'
	})
	out := r.to_prometheus()
	assert out.contains('requests{a="x,b=y"} 3')
	assert out.contains('requests{a="x",b="y"} 4')
	assert out.contains(r'escaped{route="line\n\"quoted\"\\end"} 1')
}

fn test_labeled_histogram_exposition_has_bucket_separator() {
	mut r := new_registry()
	mut h := r.histogram('latency', [10.0])
	h.record(3, {
		'route': '/x'
	})
	out := r.to_prometheus()
	assert out.contains('latency_bucket{route="/x",le="10.0"} 1')
	assert out.contains('latency_bucket{route="/x",le="+Inf"} 1')
	assert out.contains('latency_sum{route="/x"} 3')
}

fn test_prometheus_special_values() {
	mut r := new_registry()
	mut g := r.gauge('special')
	g.set(math.inf(1), {
		'kind': 'positive'
	})
	g.set(math.inf(-1), {
		'kind': 'negative'
	})
	g.set(math.nan(), {
		'kind': 'nan'
	})
	out := r.to_prometheus()
	assert out.contains('special{kind="positive"} +Inf')
	assert out.contains('special{kind="negative"} -Inf')
	assert out.contains('special{kind="nan"} NaN')
}

fn test_scrape_while_registering_and_updating_instruments() {
	mut r := new_registry()
	mut c := r.counter('events')
	mut g := r.gauge('level')
	worker := spawn fn [mut r, mut c, mut g] () {
		for i in 0 .. 500 {
			c.add(1, {
				'worker': 'one'
			})
			g.set(f64(i), {
				'worker': 'one'
			})
			mut extra := r.gauge('extra_${i % 4}')
			extra.set(f64(i), {})
		}
	}()
	for _ in 0 .. 100 {
		out := r.to_prometheus()
		assert out.contains('# TYPE events counter')
	}
	worker.wait()
	assert c.get({
		'worker': 'one'
	}) == 500
	assert g.get({
		'worker': 'one'
	}) == 499
	assert r.to_prometheus().contains('# TYPE extra_3 gauge')
}

fn test_invalid_instruments_fail_explicitly() {
	dir := os.join_path(os.vtmp_dir(), 'metrics_invalid_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	cases := [
		"mut c := r.counter('total'); c.add(-1, {})",
		"r.histogram('latency', [2.0, 1.0])",
		"r.counter('bad\\nname')",
		"r.counter('latency_count'); r.histogram('latency', [1.0])",
		"r.histogram('latency', [1.0]); r.counter('latency_count')",
	]
	for i, body in cases {
		path := os.join_path(dir, 'invalid_${i}.v')
		os.write_file(path, 'module main\nimport metrics\nfn main() {\nmut r := metrics.new_registry()\n${body}\n}\n')!
		result := os.exec([@VEXE, 'run', path])
		assert result.exit_code != 0, body
		assert result.output.contains('metrics:'), result.output
	}
}
