module websocket

import net.openssl

const (
	is_used = openssl.is_used
)

fn (mut ws Client) connect_ssl() {
	ws.log.info('Using secure SSL connection')
	C.SSL_load_error_strings()
	ws.sslctx = C.SSL_CTX_new(C.SSLv23_client_method())
	if ws.sslctx == 0 {
		ws.log.fatal("Couldn't get ssl context")
	}
	ws.ssl = C.SSL_new(ws.sslctx)
	if ws.ssl == 0 {
		ws.log.fatal("Couldn't create OpenSSL instance.")
	}
	if C.SSL_set_fd(ws.ssl, ws.socket.sockfd) != 1 {
		ws.log.fatal("Couldn't assign ssl to socket.")
	}
	if C.SSL_connect(ws.ssl) != 1 {
		ws.log.fatal("Couldn't connect using SSL.")
	}
}
