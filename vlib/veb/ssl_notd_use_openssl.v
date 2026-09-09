module veb

import io
import net
import net.http
import net.mbedtls
import os
import time

@[params]
pub struct RunParams {
pub:
	// use `family: .ip, host: 'localhost'` when you want it to bind only to 127.0.0.1
	family                    net.AddrFamily = .ip6
	host                      string
	port                      int  = default_port
	nr_workers                int  = 1
	show_startup_message      bool = true
	timeout_in_seconds        int  = 30
	max_request_buffer_size   int  = 8192
	benchmark_page_generation bool // for the "page rendered in X ms"
	ssl_config                mbedtls.SSLConnectConfig
}

fn run_at_with_ssl[A, X](mut global_app A, params RunParams) ! {
	routes := generate_routes[A, X](global_app)!
	controllers_sorted := check_duplicate_routes_in_controllers[A](global_app, routes)!
	if params.show_startup_message {
		println('[veb] Running app on https://${startup_host(params)}:${params.port}/')
	}
	flush_stdout()
	mut ssl_listener := mbedtls.new_ssl_listener(listen_addr(params), params.ssl_config)!
	defer {
		ssl_listener.shutdown() or {}
	}
	ssl_params := &SslRequestParams{
		global_app:                unsafe { voidptr(&global_app) }
		controllers_sorted:        controllers_sorted
		routes:                    &routes
		benchmark_page_generation: params.benchmark_page_generation
		max_request_buffer_size:   if params.max_request_buffer_size > 0 {
			params.max_request_buffer_size
		} else {
			max_read
		}
	}
	$if A is BeforeAcceptApp {
		global_app.before_accept_loop()
	}
	for {
		mut ssl_conn := ssl_listener.accept() or {
			eprintln('[veb] accept() failed, reason: ${err}; skipping')
			continue
		}
		ssl_conn.duration = params.timeout_in_seconds * time.second
		spawn handle_ssl_connection[A, X](mut ssl_conn, ssl_params)
	}
}

fn handle_ssl_connection[A, X](mut ssl_conn mbedtls.SSLConn, params &SslRequestParams) {
	defer {
		ssl_conn.shutdown() or {}
	}
	mut reader := io.new_buffered_reader(
		reader: ssl_conn
		cap:    params.max_request_buffer_size
	)
	defer {
		unsafe {
			reader.free()
		}
	}
	for {
		req := read_request_from_buffered_reader(mut reader) or {
			if err !is io.Eof {
				write_ssl_response(mut ssl_conn, http_400) or {}
			}
			return
		}
		completed_context := handle_ssl_request[A, X](req, params) or {
			write_ssl_response(mut ssl_conn, http_400) or {}
			return
		}
		if completed_context.takeover_mode != .none {
			eprintln('[veb] HTTPS connections do not support takeover connections yet; closing the connection after this response.')
		}
		write_ssl_context_response(mut ssl_conn, completed_context) or {
			eprintln('[veb] error sending HTTPS response: ${err}')
			return
		}
		if completed_context.takeover_mode != .none
			|| should_close_connection(completed_context.req, completed_context.res, completed_context.client_wants_to_close) {
			return
		}
	}
}

fn write_ssl_context_response(mut ssl_conn mbedtls.SSLConn, completed_context &Context) ! {
	if !completed_context.done && completed_context.return_type == .normal {
		return error('context did not send a response')
	}
	match completed_context.return_type {
		.normal {
			write_ssl_response(mut ssl_conn, completed_context.res)!
		}
		.file {
			write_ssl_response(mut ssl_conn, completed_context.res)!
			if completed_context.return_file == '' {
				return error('missing file response path')
			}
			mut file := os.open(completed_context.return_file)!
			defer {
				file.close()
			}
			mut buf := []u8{len: max_read}
			for {
				n := file.read(mut buf) or {
					if err is io.Eof {
						break
					}
					return err
				}
				if n <= 0 {
					break
				}
				ssl_conn.write(buf[..n])!
			}
		}
	}
}

fn write_ssl_response(mut ssl_conn mbedtls.SSLConn, resp http.Response) ! {
	ssl_conn.write(resp.bytes())!
}
