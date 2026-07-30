// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import time
import json2
import encoding.html

// cmd_export renders the dashboard to a static site (index.html + benchmarks.json)
// for hosting on GitHub Pages, since fast.vlang.io is served statically. The page
// is identical to the veb `/` route; its chart fetches the sibling benchmarks.json.
//
//   v run . export [-o <dir>]     (default dir: ./site)
fn cmd_export(args []string) ! {
	mut out := os.join_path(os.getwd(), 'site')
	for i, a in args {
		if a == '-o' && i + 1 < args.len {
			out = args[i + 1]
		}
	}
	os.mkdir_all(out)!

	mut db := open_db()!
	list := load_benchmarks(db)!
	db.close()!

	// same view-model as the index handler, rendered with $tmpl instead of $veb.html.
	// $veb.html() HTML-escapes interpolations but $tmpl does not, so escape the
	// user-controlled commit messages here to avoid breaking the `title` attribute
	// or injecting markup into the published static page.
	mut rows := build_rows(list)
	for mut r in rows {
		r.message = html.escape(r.message)
	}
	count := rows.len
	latest := if count > 0 { '${rows[0].commit_hash} · ${rows[0].timestamp}' } else { '—' }
	generated := time.now().format_ss()
	page := $tmpl('templates/index.html')

	os.write_file(os.join_path(out, 'index.html'), page)!
	os.write_file(os.join_path(out, 'benchmarks.json'), json2.encode(chart_points(list)))!
	elog('exported ${count} benchmarks -> ${os.join_path(out, 'index.html')} (+ benchmarks.json)')
}
