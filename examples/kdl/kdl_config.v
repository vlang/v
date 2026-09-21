// Reads a KDL configuration file into a struct.
// Run with: v run examples/kdl/kdl_config.v examples/kdl/config.kdl
module main

import kdl
import os

struct Route {
	path    string
	timeout f64
}

struct Config {
	app_name string
	version  string
	host     string
	port     i64
	tls      bool
	max_conn i64
mut:
	routes []Route
}

fn load(path string) !Config {
	doc := kdl.parse_file(path)!
	app := doc.get('app') or { return error('missing `app` node') }
	server := doc.get('server') or { return error('missing `server` node') }
	limits := doc.get('limits') or { kdl.Node{} }
	tls := server.child('tls') or { kdl.Node{} }
	mut cfg := Config{
		app_name: app.arg(0).as_string() or { return error('`app` needs a name') }
		version:  app.prop('version').as_string() or { '0.0.0' }
		host:     server.prop('host').as_string() or { 'localhost' }
		port:     server.prop('port').as_int() or { 80 }
		tls:      tls.arg(0).as_bool() or { false }
		max_conn: limits.prop('max-connections').as_int() or { 100 }
	}
	for r in server.children_named('route') {
		cfg.routes << Route{
			path:    r.arg(0).as_string() or { return error('`route` needs a path') }
			timeout: r.prop('timeout').as_f64() or { 30.0 }
		}
	}
	return cfg
}

fn main() {
	path := if os.args.len > 1 { os.args[1] } else { os.join_path(os.dir(@FILE), 'config.kdl') }
	cfg := load(path) or {
		eprintln('${path}: ${err.msg()}')
		exit(1)
	}
	println('${cfg.app_name} ${cfg.version}: ${cfg.host}:${cfg.port} tls=${cfg.tls} max=${cfg.max_conn}')
	for r in cfg.routes {
		println('  ${r.path} (timeout ${r.timeout}s)')
	}
}
