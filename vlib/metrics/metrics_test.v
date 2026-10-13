module metrics

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
