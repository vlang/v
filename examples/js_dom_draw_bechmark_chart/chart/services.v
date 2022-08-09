module main

import net.http
import json

fn typescript_sqlite_memory() ?FrameworkBenchmarkResponse {
	url := 'http://localhost:3000/sqlite-memory/$benchmark_loop_length'
	res := http.get(url) or { panic(err) }
	framework_benchmark_response := json.decode(FrameworkBenchmarkResponse, res.body)?
	return framework_benchmark_response
}

fn v_sqlite_memory() ?FrameworkBenchmarkResponse {
	url := 'http://localhost:4000/sqlite-memory/$benchmark_loop_length'
	res := http.get(url) or { panic(err) }
	framework_benchmark_response := json.decode(FrameworkBenchmarkResponse, res.body)?
	return framework_benchmark_response
}

fn v_sqlite_file() ?FrameworkBenchmarkResponse {
	// url := 'http://localhost:3000/sqlite-memory/$benchmark_loop_length'
	// res := http.get(url) or { panic(err) }
	// framework_benchmark_response := json.decode(FrameworkBenchmarkResponse, res.body)?
	framework_benchmark_response := FrameworkBenchmarkResponse{
		insert: []
		@select: []
		update: []
	}
	return framework_benchmark_response
}
