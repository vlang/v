module http

// HTTP/3 server support — compiled only with `-d use_ngtcp2`.
import net.http.common
import net.http.v3

fn maybe_start_h3(s &Server, tls_addr string, handler_fn fn (common.ServerRequest) common.ServerResponse) {
	h3_addr := if s.h3_addr != '' { s.h3_addr } else { tls_addr }
	if s.enable_h3 {
		spawn start_h3_server(v3.ServerConfig{
			addr:      h3_addr
			cert_file: s.cert_file
			key_file:  s.key_file
			handler:   handler_fn
		})
	}
}

fn start_h3_server(config v3.ServerConfig) {
	mut server := v3.new_server(config) or {
		eprintln('[HTTP/3] failed to start: ${err}')
		return
	}
	server.listen_and_serve() or {
		eprintln('[HTTP/3] server error: ${err}')
	}
}
