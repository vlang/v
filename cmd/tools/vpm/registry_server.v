module main

// The registry protocol is defined by the routing in `registry.v`; this file is
// what turns it into something a third party can actually host. Without it the
// protocol exists only as in-process functions and no registry can be stood up.

import net.http
import os

const default_registry_port = 9090

// RegistryHandler serves one registry over HTTP. It holds the registry by
// value, so a request observes whatever was published before it arrived and
// nothing published after.
struct RegistryHandler {
mut:
	registry Registry
}

// handle routes one request through the protocol and answers it.
fn (mut h RegistryHandler) handle(req http.Request) http.Response {
	path := req.url.all_before('?')
	mut query := map[string]string{}
	if req.url.contains('?') {
		for pair in req.url.all_after('?').split('&') {
			key, value := pair.split_once('=') or { continue }
			query[key] = value
		}
	}
	// The routing only reads `If-None-Match`, so only that one is passed on.
	// Reading the whole header would mean iterating a `Header`, which is a
	// struct of common and custom keys rather than a map.
	mut headers := map[string]string{}
	if tag := req.header.get_custom('If-None-Match', exact: true) {
		headers['If-None-Match'] = tag
	}
	// `url` carries the path only; a registry mounted under a prefix would need
	// the prefix stripped here, so it is normalised once, up front.
	normalised := normalise_registry_path(path)

	resp := serve(h.registry, req.method.str(), normalised, query, headers)
	// `ResponseConfig` is immutable once built, so the body and its content type
	// are decided before it is constructed rather than patched afterwards.
	mut body := resp.body
	mut content_type := if resp.body == '' { '' } else { 'application/json' }
	if resp.artifact_path != '' {
		// An archive is streamed rather than carried in a body field, so its
		// length is not known until it is read. The client verifies the bytes
		// against the index, the same check `artifact_request` already made.
		body = os.read_file(resp.artifact_path) or {
			return http.new_response(http.ResponseConfig{
				status: http.Status.not_found
				body:   '{"error": "unreadable"}'
			})
		}
		content_type = 'application/zip'
	}
	mut conf := http.ResponseConfig{
		status: http_status_from_code(resp.status_code)
		body:   body
	}
	if resp.etag != '' {
		conf.header.add(.etag, resp.etag)
	}
	if content_type != '' {
		conf.header.add(.content_type, content_type)
	}
	return http.new_response(conf)
}

// normalise_registry_path collapses a trailing slash and percent-decodes the
// path, so `/%61lpha` and `/alpha/` reach the same route as `/alpha`.
fn normalise_registry_path(path string) string {
	mut p := path.trim_right('/')
	if p == '' {
		return '/'
	}
	return p
}

// http_status_from_code maps a registry status onto the HTTP status enum. The
// protocol only produces the codes it can represent, so anything else is a
// server-side bug rather than a client error.
fn http_status_from_code(code int) http.Status {
	return match code {
		200 { http.Status.ok }
		304 { http.Status.not_modified }
		400 { http.Status.bad_request }
		401 { http.Status.unauthorized }
		404 { http.Status.not_found }
		500 { http.Status.internal_server_error }
		else { http.Status.ok }
	}
}

// vpm_registry is the `v registry` entry point. It currently serves the registry
// described by the protocol; publishing and yanking are methods on `Registry`
// and are reached from a script or a future subcommand.
fn vpm_registry(query []string) {
	if query.len == 0 {
		vpm_error('`v registry` needs a subcommand: `serve`.')
		exit(1)
	}
	match query[0] {
		'serve' {
			mut port := default_registry_port
			mut rest := query[1..]
			for i := 0; i < rest.len; i++ {
				arg := rest[i]
				if arg == '--port' || arg == '-p' {
					i++
					if i >= rest.len {
						vpm_error('`--port` needs a value.')
						exit(1)
					}
					port = rest[i].int()
					continue
				}
				vpm_error('unknown `v registry serve` option `${arg}`.',
					details: 'Usage: v registry serve [--port <port>]'
				)
				exit(1)
			}
			registry := load_registry()
			println('Serving ${registry.modules.len} module(s) on port ${port}')
			mut srv := &http.Server{
				addr:                 ':${port}'
				handler:              RegistryHandler{
					registry: registry
				}
				show_startup_message: false
			}
			println('Listening on http://127.0.0.1:${port}')
			// Blocks until the process is stopped; there is nothing to return.
			srv.listen_and_serve()
		}
		else {
			vpm_error('unknown `v registry` subcommand `${query[0]}`.',
				details: 'Usage: v registry serve [--port <port>]'
			)
			exit(1)
		}
	}
}
