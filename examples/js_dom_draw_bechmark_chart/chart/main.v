module main

import vweb
import os
import json
import arrays

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
	mut attribute_names := []string{}
	// Used to garante the chart proposionalite
	mut max_benchmark := 0
	chart_colors := ['gray', 'red', 'orange', 'purple']

	framework_platform := insert_framework_benchmark_times().to_map()

	mut maxs := []int{}
	for key, values in framework_platform {
		attribute_names << key
		maxs << arrays.max(values)?
	}

	max_benchmark = arrays.max(maxs)?

	inserts_from_framework := json.encode(framework_platform) // string// json encoded
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
