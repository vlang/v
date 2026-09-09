module veb

import net
import time

@[params]
pub struct SSLConnectConfig {
pub:
	verify                 string
	cert                   string
	cert_key               string
	validate               bool
	in_memory_verification bool
	read_timeout           time.Duration
}

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
	ssl_config                SSLConnectConfig
}

fn run_at_with_ssl[A, X](mut _global_app A, _params RunParams) ! {
	return error('veb HTTPS server requires net.mbedtls; -d use_openssl only avoids mbedtls for HTTP apps')
}
