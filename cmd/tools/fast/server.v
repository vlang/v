// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import veb
import time

pub struct Context {
	veb.Context
}

struct App {
	veb.StaticHandler
}

// ChartPoint is one point of the compile-time chart, serialised as JSON for
// the small canvas chart on the index page.
struct ChartPoint {
	date   string
	hash   string
	v_c    int
	v_self int
	hello  int
	// peak-RSS five-number summaries (KB): [min, q1, median, q3, max]
	self_rss  []int
	hello_rss []int
}

// serve starts the veb web app. It binds to localhost only, since this is meant
// to run on a local machine.
fn serve(args []string) ! {
	mut port := 8080
	for i, a in args {
		if a == '-port' && i + 1 < args.len {
			port = args[i + 1].int()
		}
	}
	// make sure the database and table exist before we start accepting requests
	mut db := open_db()!
	db.close()!

	mut app := &App{}
	elog('Starting the fast.vlang.io web app on http://localhost:${port} (db: ${db_path})')
	veb.run_at[App, Context](mut app, port: port, family: .ip, host: 'localhost')!
}

// index renders the main dashboard: a chart plus the full benchmark history.
pub fn (mut app App) index(mut ctx Context) veb.Result {
	mut db := open_db() or { return ctx.server_error('database error: ${err}') }
	defer {
		db.close() or {}
	}
	list := load_benchmarks(db) or { return ctx.server_error('database error: ${err}') }

	rows := build_rows(list)
	count := rows.len
	latest := if count > 0 { '${rows[0].commit_hash} · ${rows[0].timestamp}' } else { '—' }
	generated := time.now().format_ss()
	return $veb.html()
}

// chart_points converts stored benchmarks (newest first) into chronological
// chart points (oldest first), shared by the JSON endpoints and the static export.
fn chart_points(list []Benchmark) []ChartPoint {
	mut points := []ChartPoint{cap: list.len}
	for i := list.len - 1; i >= 0; i-- {
		b := list[i]
		points << ChartPoint{
			date:      b.commit_date.format()
			hash:      b.commit_hash
			v_c:       b.v_c_ms
			v_self:    b.v_self_ms
			hello:     b.hello_ms
			self_rss:  [b.self_rss_min_kb, b.self_rss_q1_kb, b.self_rss_med_kb, b.self_rss_q3_kb,
				b.self_rss_max_kb]
			hello_rss: [b.hello_rss_min_kb, b.hello_rss_q1_kb, b.hello_rss_med_kb, b.hello_rss_q3_kb,
				b.hello_rss_max_kb]
		}
	}
	return points
}

// benchmarks_json serves the chart data. The static export writes the same JSON
// to a `benchmarks.json` file, so the page's chart works in both modes.
@['/benchmarks.json']
pub fn (mut app App) benchmarks_json(mut ctx Context) veb.Result {
	return render_chart_json(mut ctx)
}

// api_benchmarks serves the same data under a stable API path.
@['/api/benchmarks']
pub fn (mut app App) api_benchmarks(mut ctx Context) veb.Result {
	return render_chart_json(mut ctx)
}

// render_chart_json is a free function (not an App method) so veb does not
// register it as its own route.
fn render_chart_json(mut ctx Context) veb.Result {
	mut db := open_db() or { return ctx.server_error('database error: ${err}') }
	defer {
		db.close() or {}
	}
	list := load_benchmarks(db) or { return ctx.server_error('database error: ${err}') }
	return ctx.json(chart_points(list))
}

// health is a tiny liveness endpoint, handy when running as a background job.
@['/health']
pub fn (mut app App) health(mut ctx Context) veb.Result {
	return ctx.text('ok')
}
