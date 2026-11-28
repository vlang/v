module veb

import net
import net.mbedtls
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
	// max_request_body_size is the largest request body, in bytes, that the server accepts
	// (64 MiB by default). It answers a request that has a larger body with `413`, and
	// closes the connection. 0 removes the limit: a body is kept in memory, so use it only
	// where every client is trusted. A negative value is an error.
	max_request_body_size int = default_max_request_body_size
}

fn run_at_with_ssl[A, X](mut global_app A, params RunParams) ! {
	routes := generate_routes[A, X](global_app)!
	controllers_sorted := check_duplicate_routes_in_controllers[A](global_app, routes)!
	if params.show_startup_message {
		println('[veb] Running app on https://${startup_host(params)}:${params.port}/')
	}
	flush_stdout()
	mut ssl_listener := mbedtls.new_ssl_listener(listen_addr(params), params.ssl_config,
		family: params.family
	)!
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
		max_request_body_size:     params.max_request_body_size
		read_header_timeout:       params.timeout_in_seconds * time.second
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
