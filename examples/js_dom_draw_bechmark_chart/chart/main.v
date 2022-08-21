module main

import vweb
import os
import json
import arrays
import net.http

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
	benchmark_loop_length = 100
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
	orm_stmt_kinds := [ "insert", "select", "update"]
	mut attribute_names := []string{}
	// Used to garante the chart proposionalite
	mut max_benchmark := 0
	chart_colors := ['gray', 'red', 'orange', 'purple']

	framework_platform := insert_framework_benchmark_times().to_map()//{'v_sqlite_memory': [18488, 8861, 13650, 7522, 8374, 8230, 7243, 7326, 8504, 7225, 7062, 8522, 7236, 7367, 7972, 7487, 7288, 9353, 7189, 7139, 9351, 7127, 7164, 7880, 7053, 8089, 8378, 8018, 7234, 7716, 7208, 7048, 7598, 7107, 7126, 8662, 7699, 7094, 7807, 7945, 7066, 7991, 7121, 7369, 9332, 7413, 7195, 7962, 7070, 7085, 8041, 7061, 7741, 308755, 11733, 6447, 6518, 7282, 17696, 6676, 6176, 6875, 5989, 6910, 6536, 9915, 10418, 9896, 10721, 14180, 13619, 10579, 11926, 12727, 10833, 11386, 11355, 9163, 10555, 10265, 8982, 8759, 9568, 8522, 10593, 10605, 12032, 14101, 9772, 11868, 9913, 10284, 9643, 10048, 12076, 9108, 7736, 6995, 6483, 5942], 'typescript_sqlite_memory': [565320, 229547, 200475, 150701, 149066, 143816, 142433, 141018, 142310, 141260, 156255, 154454, 372381, 139682, 136137, 145062, 140527, 146196, 158410, 147935, 206780, 216237, 275554, 207284, 152195, 385703, 168595, 187742, 192827, 169224, 192226, 185396, 158938, 152301, 152012, 154898, 152670, 150480, 335871, 200471, 279429, 231102, 203049, 169223, 151597, 151794, 158856, 161908, 151193, 160302, 206533, 413112, 154068, 152136, 158000, 183887, 200871, 161406, 151375, 182077, 187616, 268446, 169719, 164540, 195728, 423302, 176532, 167253, 154343, 170239, 205769, 182234, 157041, 155303, 168761, 160008, 167993, 158045, 160103, 155013, 306164, 158089, 152934, 279396, 248253, 173022, 166740, 205300, 165810, 151906, 147041, 162239, 206070, 327950, 210147, 179608, 172642, 164889, 158637, 157771]}"

	mut maxs := []int{} //mut maxs := map[string][]int{}
	for key, values in framework_platform {
		attribute_names << key
		maxs << arrays.max(values)?
	}

	max_benchmark = arrays.max(maxs)?

	// inserts_from_framework := json.encode(framework_platform) // string// json encoded
	mut from_framework := map[string]string{}
	from_framework["insert"] = json.encode(insert_framework_benchmark_times().to_map()) // string// json encoded
	from_framework["select"] = json.encode(select_framework_benchmark_times().to_map()) // string// json encoded
	from_framework["update"] = json.encode(update_framework_benchmark_times().to_map()) // string// json encoded
	mut table := gen_table_info(attribute_names, framework_platform)

	return $vweb.html()
}

fn insert_framework_benchmark_times() FrameworkPlatform {
	numbers := FrameworkPlatform{
		v_sqlite_memory: v_sqlite_memory()!.insert
		// v_sqlite_file: v_sqlite_file()!.insert
		typescript_sqlite_memory: typescript_sqlite_memory()!.insert
	}

	return numbers
}

fn select_framework_benchmark_times() FrameworkPlatform {
	numbers := FrameworkPlatform{
		v_sqlite_memory: v_sqlite_memory()!.@select
		// v_sqlite_file: v_sqlite_file()!.@select
		typescript_sqlite_memory: typescript_sqlite_memory()!.@select
	}

	return numbers
}

fn update_framework_benchmark_times() FrameworkPlatform {
	numbers := FrameworkPlatform{
		v_sqlite_memory: v_sqlite_memory()!.update
		// v_sqlite_file: v_sqlite_file()!.@select
		typescript_sqlite_memory: typescript_sqlite_memory()!.update
	}

	return numbers
}

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
		// max_fast[name] = int(100 - (f64(max_times[name] * 100) / max))
		// ten_perc_max_fast[name] = int(100 - (f64(ten_perc_max_times[name] * 100) / ten_perc_max))
		// min_fast[name] = int(100 - (f64(min_times[name] * 100) / min))
		// ten_perc_min_fast[name] = int(100 - (f64(ten_perc_min_times[name] * 100) / ten_perc_min))
	}

	for name in attribute_names {
		table[name]['max.'] = '${max_times[name]} ns (${max_fast[name]}x faster)'
		table[name]['10% max.'] = '${ten_perc_max_times[name]} ns (${ten_perc_max_fast[name]}x faster)'
		table[name]['min.'] = '${min_times[name]} ns (${min_fast[name]}x faster)'
		table[name]['10% min.'] = '${ten_perc_min_times[name]} ns (${ten_perc_min_fast[name]}x faster)'
	}
	return table
}
