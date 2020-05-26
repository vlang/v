module websocket

// On linux, prefer a localy build openssl, because it is
// much more likely for it to be newer, than the system
// openssl from libssl-dev. If there is no local openssl,
// the next flag is harmless, since it will still use the
// (older) system openssl.
#flag linux -I/usr/local/include/openssl -L/usr/local/lib
#flag -lssl
// MacPorts
#flag darwin -I/opt/local/include
#flag darwin -L/opt/local/lib
// Brew
#flag darwin -I/usr/local/opt/openssl/include
#flag darwin -L/usr/local/opt/openssl/lib
#include <openssl/rand.h>
#include <openssl/ssl.h>
#include <openssl/err.h>
struct SSL_CTX {
}

struct SSL {
}

struct SSL_METHOD {
}

fn C.SSL_load_error_strings()

fn C.SSL_library_init()

fn C.SSLv23_client_method() &C.SSL_METHOD

fn C.SSL_CTX_new() &C.SSL_CTX

fn C.SSL_new() &C.SSL

fn C.SSL_set_fd() int

fn C.SSL_connect() int

fn C.SSL_shutdown()

fn C.SSL_free()

fn C.SSL_CTX_free()

fn C.SSL_write() int

fn C.SSL_read() int

fn (mut ws Client) connect_ssl() {
	l.i('Using secure SSL connection')
	C.SSL_load_error_strings()
	C.SSL_library_init()
	ws.sslctx = C.SSL_CTX_new(C.SSLv23_client_method())
	if ws.sslctx == 0 {
		l.f("Couldn't get ssl context")
	}
	ws.ssl = C.SSL_new(ws.sslctx)
	if ws.ssl == 0 {
		l.f("Couldn't create OpenSSL instance.")
	}
	if C.SSL_set_fd(ws.ssl, ws.socket.sockfd) != 1 {
		l.f("Couldn't assign ssl to socket.")
	}
	if C.SSL_connect(ws.ssl) != 1 {
		l.f("Couldn't connect using SSL.")
	}
}
