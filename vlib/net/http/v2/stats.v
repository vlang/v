// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

// Statistics for performance monitoring
pub struct Stats {
pub mut:
	total_requests       u64
	successful_requests  u64
	failed_requests      u64
	total_bytes_sent     u64
	total_bytes_received u64
	total_time_ms        u64
	min_time_ms          u64 = 999999
	max_time_ms          u64
}

// record_request records statistics for a single request.
pub fn (mut s Stats) record_request(success bool, bytes_sent int, bytes_received int, time_ms u64) {
	s.total_requests++
	if success {
		s.successful_requests++
	} else {
		s.failed_requests++
	}
	s.total_bytes_sent += u64(bytes_sent)
	s.total_bytes_received += u64(bytes_received)
	s.total_time_ms += time_ms

	if time_ms < s.min_time_ms {
		s.min_time_ms = time_ms
	}
	if time_ms > s.max_time_ms {
		s.max_time_ms = time_ms
	}
}

// avg_time_ms calculates and returns the average request time in milliseconds.
pub fn (s Stats) avg_time_ms() f64 {
	if s.total_requests == 0 {
		return 0.0
	}
	return f64(s.total_time_ms) / f64(s.total_requests)
}

// success_rate calculates and returns the request success rate as a percentage.
pub fn (s Stats) success_rate() f64 {
	if s.total_requests == 0 {
		return 0.0
	}
	return f64(s.successful_requests) / f64(s.total_requests) * 100.0
}

// print displays the performance statistics to stdout.
pub fn (s Stats) print() {
	println('Performance Statistics:')
	println('  Total requests: ${s.total_requests}')
	println('  Successful: ${s.successful_requests}')
	println('  Failed: ${s.failed_requests}')
	println('  Success rate: ${s.success_rate():.2f}%')
	println('  Total bytes sent: ${s.total_bytes_sent}')
	println('  Total bytes received: ${s.total_bytes_received}')
	println('  Average time: ${s.avg_time_ms():.2f}ms')
	println('  Min time: ${s.min_time_ms}ms')
	println('  Max time: ${s.max_time_ms}ms')
}
