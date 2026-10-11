// static_export.v - a static site generator built on `veb.render_request`.
//
// ## Why this example exists
//
// veb is an HTTP server, so the usual way to get a page out of a veb app is to
// run it and ask. That is fine for a server and wrong for a site that should be
// a directory of files: a static export needs the *same* HTML without a process,
// a port, or a second rendering implementation that can drift from the first.
//
// `veb.render_request` answers that with one call, because it is the same code
// path veb's own tests take - the route table is generated from the app's
// methods and the request is dispatched through the dispatcher the server loop
// uses. `Context.conn` stays nil, so nothing is written to a socket.
//
// ## What it demonstrates, in order
//
//   1. the export list is explicit, not crawled - see `routes` below;
//   2. each route is rendered in-process, with no listener;
//   3. the URL-to-file mapping is the one GitHub Pages serves directly, which
//      includes `/404` being a FILE rather than a directory;
//   4. the count is checked, so a page silently missing from `dist/` fails the
//      build instead of shipping a short site.
//
// Run it:
//
//	v -prod run . --export     # writes dist/
//	v run .                       # previews at http://localhost:8023
//
// `-prod` is not for speed here: it compiles veb's templates into the binary, so
// a template error is a build error rather than something a visitor sees.
module main

import veb
import net.http
import os

const port = 8023

// site_root and out_dir are resolved from this file rather than the working
// directory, so the same command produces the same site from any directory.
const site_root = os.dir(@FILE)
const out_dir = os.join_path(site_root, 'dist')

// routes is the export list. A crawler follows links and can silently skip a
// page whose only link is generated at runtime; this list fails loudly instead -
// `export()` compares what it wrote against it.
//
// `/404` is on the list for the same reason `output_path` special-cases it:
// GitHub Pages serves `dist/404.html` by exact name for an unknown path, and an
// export that omits it gets Pages' own error page with broken navigation.
const routes = ['/', '/404', '/docs/getting-started']

struct App {
	veb.StaticHandler
}

struct Context {
	veb.Context
}

@['/']
fn (app &App) index(mut ctx Context) veb.Result {
	return $veb.html()
}

// not_found is veb's 404 answer, overridden on Context because that is where
// veb declares it and calls it. It is also routed (`/404`) so the export can
// reach it: a template alone has no URL, and the export loop only knows how to
// GET.
//
@['/404']
fn (mut ctx Context) not_found() veb.Result {
	result := $veb.html('404.html')
	ctx.res.set_status(.not_found)
	return result
}

@['/docs/getting-started']
fn (app &App) getting_started(mut ctx Context) veb.Result {
	return $veb.html()
}

fn main() {
	mut app := &App{}
	if os.args.len > 1 && os.args[1] == '--export' {
		export(mut app) or { panic('export: ' + err.msg()) }
		return
	}
	app.handle_static(os.join_path(site_root, 'static'), true)!
	veb.run[App, Context](mut app, port)
}

fn export(mut app &App) ! {
	mut written := 0
	for route in routes {
		// The status question is settled by taking the body either way: veb fills
		// `res.body` for a 404 answer too, and `/404` deliberately 404s upstream
		// of this call. An empty body is the only failure, because that is the
		// one state no page should be in.
		body := render(mut app, route)
		if body == '' {
			panic('export: route "${route}" produced an empty body')
		}
		out := output_path(route)
		os.mkdir_all(os.dir(out))!
		os.write_file(out, body)!
		println('  wrote ${out} (${body.len} bytes)')
		written++
	}
	if written != routes.len {
		panic('export: expected ${routes.len} routes, wrote ${written}')
	}
	println('exported ${written} route(s) to ${out_dir}')
}

// render renders one route in-process. The host is fixed because a static site
// has no host; only the path is variable.
fn render(mut app &App, path string) string {
	ctx := veb.render_request[App, Context](mut app, http.Request{
		method: .get
		url:    path
	}) or { panic(err) }
	return ctx.res.body
}

// output_path maps a route URL to the file the export writes - the three shapes
// GitHub Pages serves directly. `/` is the only path with no subdirectory,
// `/404` is an exact name, and everything else is a directory with an index.
fn output_path(path string) string {
	if path == '/' {
		return os.join_path(out_dir, 'index.html')
	}
	if path == '/404' {
		return os.join_path(out_dir, '404.html')
	}
	return os.join_path(out_dir, path.trim_left('/'), 'index.html')
}
