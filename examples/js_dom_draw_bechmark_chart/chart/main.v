module main

import vweb
import os
import json
import arrays
import net.http
import math
import v.util.version

[table: 'benchmark']
struct Task {
mut:
	id     u32    [primary; serial; sql: serial]
	title  string
	status string
}

struct FrameworkBenchmarkResponse {
	insert  []int
	@select []int
	update  []int
}

struct FrameworkPlatform {
mut:
	v_sqlite_memory []int
	// v_sqlite_file            []int
	typescript_sqlite_memory []int
}

fn (framework_platform FrameworkPlatform) to_map() map[string][]int {
	mut mapa := map[string][]int{}

	mapa['v_sqlite_memory'] = framework_platform.v_sqlite_memory
	// mapa['v_sqlite_file'] = framework_platform.v_sqlite_file
	mapa['typescript_sqlite_memory'] = framework_platform.typescript_sqlite_memory
	return mapa
}

const (
	http_port             = 3001
	benchmark_loop_length = 20
)

struct App {
	vweb.Context
}

enum SqliteDbConnection {
	sqlite_memory
	sqlite_file
}

fn main() {
	vweb.run(new_app(), http_port)
}

pub fn (mut app App) before_request() {
	os.execute_or_panic('v -b js_browser draw.js.v ')
}

fn new_app() &App {
	mut app := &App{}
	app.serve_static('/favicon.ico', 'favicon.ico')
	app.serve_static('/draw.js', 'draw.js')
	app.mount_static_folder_at(os.resource_abs_path('.'), '/')
	return app
}

['/'; get]
pub fn (mut app App) controller_get_all_task() ?vweb.Result {
	v_version := version.full_v_version(true)
	orm_stmt_kinds := ['insert', 'select', 'update']

	mut attribute_names := map[string][]string{}
	// Used to garante the chart proposionalite
	mut max_benchmark := map[string]int{}
	mut from_framework := map[string]string{}
	mut maxs := map[string][]int{}
	mut framework_platform := map[string]map[string][]int{}
	mut table := map[string]map[string]map[string]string{}

	chart_colors := ['gray', 'red', 'orange', 'purple', 'red', 'orange', 'purple']
	for orm_stmt_kind in orm_stmt_kinds {
		match orm_stmt_kind {
			'insert' {
				framework_platform[orm_stmt_kind] = insert_framework_benchmark_times() or {
					return error('')
				}.to_map()
			}
			'select' {
				framework_platform[orm_stmt_kind] = select_framework_benchmark_times() or {
					return error('')
				}.to_map()
			}
			'update' {
				framework_platform[orm_stmt_kind] = update_framework_benchmark_times() or {
					return error('')
				}.to_map()
			}
			else {}
		}

		for key, values in framework_platform[orm_stmt_kind] {
			attribute_names[orm_stmt_kind] << key
			maxs[orm_stmt_kind] << arrays.max(values) or { continue }
		}

		from_framework[orm_stmt_kind] = json.encode(framework_platform[orm_stmt_kind])
		table[orm_stmt_kind] = gen_table_info(attribute_names[orm_stmt_kind], framework_platform[orm_stmt_kind])
		max_benchmark[orm_stmt_kind] = arrays.max(maxs[orm_stmt_kind]) or { continue }
	}

	return $vweb.html()
}

fn insert_framework_benchmark_times() !FrameworkPlatform {
	numbers := FrameworkPlatform{
		v_sqlite_memory: v_sqlite_memory()!.insert
		// v_sqlite_file: v_sqlite_file()!.insert
		typescript_sqlite_memory: typescript_sqlite_memory()!.insert
	}

	return numbers
}

fn select_framework_benchmark_times() !FrameworkPlatform {
	numbers := FrameworkPlatform{
		v_sqlite_memory: v_sqlite_memory()!.@select
		// v_sqlite_file: v_sqlite_file()!.@select
		typescript_sqlite_memory: typescript_sqlite_memory()!.@select
	}

	return numbers
}

fn update_framework_benchmark_times() !FrameworkPlatform {
	numbers := FrameworkPlatform{
		v_sqlite_memory: v_sqlite_memory()!.update
		// v_sqlite_file: v_sqlite_file()!.@select
		typescript_sqlite_memory: typescript_sqlite_memory()!.update
	}

	return numbers
}

fn typescript_sqlite_memory() !FrameworkBenchmarkResponse {
	url := 'http://localhost:3000/sqlite-memory/$benchmark_loop_length'
	res := http.get(url) or { panic(err) }
	framework_benchmark_response := json.decode(FrameworkBenchmarkResponse, res.body)!
	return framework_benchmark_response
}

fn v_sqlite_memory() !FrameworkBenchmarkResponse {
	url := 'http://localhost:4000/sqlite-memory/$benchmark_loop_length'
	res := http.get(url) or { panic(err) }
	framework_benchmark_response := json.decode(FrameworkBenchmarkResponse, res.body)!
	return framework_benchmark_response
}

fn v_sqlite_file() !FrameworkBenchmarkResponse {
	// url := 'http://localhost:3000/sqlite-memory/$benchmark_loop_length'
	// res := http.get(url) or { panic(err) }
	// framework_benchmark_response := json.decode(FrameworkBenchmarkResponse, res.body)!
	framework_benchmark_response := FrameworkBenchmarkResponse{
		insert: []
		@select: []
		update: []
	}
	return framework_benchmark_response
}

fn gen_table_info(attribute_names []string, framework_platform map[string][]int) map[string]map[string]string {
	mut table := map[string]map[string]string{}

	// nanoseconds
	mut max_times := map[string]int{}
	mut ten_perc_max_times := map[string]int{}
	mut min_times := map[string]int{}
	mut ten_perc_min_times := map[string]int{}

	// bigger to calculate percent
	mut max := 0.0
	mut ten_perc_max := 0.0
	mut min := 0.0
	mut ten_perc_min := 0.0

	// percentes
	mut max_fast := map[string]int{}
	mut ten_perc_max_fast := map[string]int{}
	mut min_fast := map[string]int{}
	mut ten_perc_min_fast := map[string]int{}

	// nanoseconds
	for idx, name in attribute_names {
		// qtd. of values in 10 % of arrays
		ten_perc := int(framework_platform[name].len / 10)

		// get 10% highter
		mut min_ten_array := framework_platform[name].clone()
		min_ten_array.sort()
		min_ten_array.trim(ten_perc)

		// get 10% lower
		mut max_ten_array := framework_platform[name].clone()
		max_ten_array.sort(a > b)
		max_ten_array.trim(ten_perc)

		// popule array with nanoseconds to which benchmark
		max_times[name] = arrays.max(framework_platform[name]) or { 0 } // int
		ten_perc_max_times[name] = arrays.sum(max_ten_array) or { 0 } / ten_perc // int
		min_times[name] = arrays.min(framework_platform[name]) or { 0 } // int
		ten_perc_min_times[name] = arrays.sum(min_ten_array) or { 0 } / ten_perc // int

		// set bigger values
		if idx < 1 {
			max = f64(max_times[name])
			ten_perc_max = f64(ten_perc_max_times[name])
			min = f64(min_times[name])
			ten_perc_min = f64(ten_perc_min_times[name])
		} else {
			if max < f64(max_times[name]) {
				max = f64(max_times[name])
			}
			if ten_perc_max < f64(ten_perc_max_times[name]) {
				ten_perc_max = f64(ten_perc_max_times[name])
			}
			if min < f64(min_times[name]) {
				min = f64(min_times[name])
			}
			if ten_perc_min < f64(ten_perc_min_times[name]) {
				ten_perc_min = f64(ten_perc_min_times[name])
			}
		}
	}

	// percents
	for name in attribute_names {
		max_fast[name] = int(max / f64(max_times[name]))
		ten_perc_max_fast[name] = int(ten_perc_max / f64(ten_perc_max_times[name]))
		min_fast[name] = int(min / f64(min_times[name]))
		ten_perc_min_fast[name] = int(ten_perc_min / f64(ten_perc_min_times[name]))
	}

	for name in attribute_names {
		table[name]['max.'] = '${math.round_sig(f64(max_times[name]) / 1000000, 2)} ms (${max_fast[name]}x faster)'
		table[name]['10% max.'] = '${math.round_sig(f64(ten_perc_max_times[name]) / 1000000,
			2)} ms (${ten_perc_max_fast[name]}x faster)'
		table[name]['min.'] = '${math.round_sig(f64(min_times[name]) / 1000000, 2)} ms (${min_fast[name]}x faster)'
		table[name]['10% min.'] = '${math.round_sig(f64(ten_perc_min_times[name]) / 1000000,
			2)} ms (${ten_perc_min_fast[name]}x faster)'
	}
	return table
}
