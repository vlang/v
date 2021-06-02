module openssl

import time

enum Select {
	read
	write
	except
}

pub struct SSLConn {
mut:
	sslctx   &C.SSL_CTX
	ssl      &C.SSL
	handle   int
	duration time.Duration
}

pub fn new_ssl_conn() &SSLConn {
	return &SSLConn{
		sslctx: 0
		ssl: 0
		handle: 0
	}
}
