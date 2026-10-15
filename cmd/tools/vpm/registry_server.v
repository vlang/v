module main

// The registry protocol is defined by the routing in `registry.v`; this file is
// what turns it into something a third party can actually host. Without it the
// protocol exists only as in-process functions and no registry can be stood up.

import net
import net.http
import net.urllib
import strconv

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
		values := urllib.parse_query(req.url.all_after('?')) or {
			return http.new_response(http.ResponseConfig{
				status: http.Status.bad_request
				body:   '{"error": "invalid query encoding"}'
			})
		}
		for pair in values.data {
			query[pair.key] = pair.value
		}
	}
	// The routing only reads `If-None-Match`, so only that one is passed on.
	// Reading the whole header would mean iterating a `Header`, which is a
	// struct of common and custom keys rather than a map.
	mut headers := map[string]string{}
	if tag := req.header.get(.if_none_match) {
		headers['If-None-Match'] = tag
	}
	// `url` carries the path only; a registry mounted under a prefix would need
	// the prefix stripped here, so it is normalised once, up front.
	normalised := normalise_registry_path(path)

	resp := serve(h.registry, req.method.str(), normalised, query, headers)
	return registry_http_response(resp)
}

// registry_http_response sends the exact bytes already verified by routing.
fn registry_http_response(resp RegistryResponse) http.Response {
	content_type := if resp.artifact_path != '' {
		'application/zip'
	} else if resp.body != '' {
		'application/json'
	} else {
		''
	}
	mut conf := http.ResponseConfig{
		status: http_status_from_code(resp.status_code)
		body:   resp.body
	}
	if resp.etag != '' {
		conf.header.add(.etag, resp.etag)
	}
	if content_type != '' {
		conf.header.add(.content_type, content_type)
	}
	return http.new_response(conf)
}

// normalise_registry_path removes trailing slashes while retaining the request path's
// percent-encoded spelling.
fn normalise_registry_path(path string) string {
	p := path.trim_right('/')
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

// parse_registry_port accepts a decimal TCP port without ignored suffixes.
fn parse_registry_port(value string) !int {
	if value == '' || !value.bytes().all(it.is_digit()) {
		return error('`--port` needs a port number, but got `${value}`.')
	}
	port := strconv.atoi(value) or {
		return error('`--port` needs a port number, but got `${value}`.')
	}
	if port < 1 || port > 65535 {
		return error('`--port` needs a port number, but got `${value}`.')
	}
	return port
}

// registry_listen_tcp binds every IPv4 interface before startup is announced.
// Windows must opt out of net's address reuse and reserve the port exclusively.
fn registry_listen_tcp(port int) !&net.TcpListener {
	$if windows {
		return registry_listen_tcp_exclusive(port)
	} $else {
		return net.listen_tcp(.ip, ':${port}')
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
			rest := query[1..]
			for i := 0; i < rest.len; i++ {
				arg := rest[i]
				if arg == '--port' || arg == '-p' {
					i++
					if i >= rest.len {
						vpm_error('`--port` needs a value.')
						exit(1)
					}
					port = parse_registry_port(rest[i]) or {
						vpm_error(err.msg())
						exit(1)
					}
					continue
				}
				vpm_error('unknown `v registry serve` option `${arg}`.',
					details: 'Usage: v registry serve [--port <port>]'
				)
				exit(1)
			}
			registry := load_registry() or {
				vpm_error(err.msg())
				exit(1)
			}
			registry.checked_signing_key() or {
				vpm_error(err.msg())
				exit(1)
			}
			listener := registry_listen_tcp(port) or {
				vpm_error('failed to listen on port ${port}: ${err.msg()}')
				exit(1)
			}
			mut srv := &http.Server{
				addr:                 ':${port}'
				listener:             listener
				handler:              RegistryHandler{
					registry: registry
				}
				show_startup_message: false
			}
			println('Serving ${registry.modules.len} module(s) on port ${port}')
			println('Listening on http://0.0.0.0:${port} (all IPv4 interfaces; no authentication)')
			flush_stdout()
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
