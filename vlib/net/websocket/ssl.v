module ws

#flag -lssl
#include <openssl/rand.h>
#include <openssl/ssl.h>
#include <openssl/err.h>

struct C.SSL_CTX
struct C.SSL
struct C.SSL_METHOD
fn C.SSL_load_error_strings()
fn C.SSL_library_init()
fn C.SSLv23_client_method() &SSL_METHOD
fn C.SSL_CTX_new() &SSL_CTX
fn C.SSL_new() &SSL
fn C.SSL_set_fd() int
fn C.SSL_connect() int
fn C.SSL_shutdown()
fn C.SSL_free()
fn C.SSL_CTX_free()
fn C.SSL_write() int
fn C.SSL_read() int

fn (ws mut Client) connect_ssl(){
	l.i("Using secure SSL connection")
	C.SSL_load_error_strings()
	C.SSL_library_init()

	ws.sslctx = SSL_CTX_new(SSLv23_client_method())
	if ws.sslctx == C.NULL {
		l.f("Couldn't get ssl context")
	}

	ws.ssl = SSL_new(ws.sslctx)
	if ws.ssl == C.NULL {
		l.f("Couldn't create OpenSSL instance.")
	}

	if SSL_set_fd(ws.ssl, ws.socket.sockfd) != 1 {
		l.f("Couldn't assign ssl to socket.")
	}

	if SSL_connect(ws.ssl) != 1 {
		l.f("Couldn't connect using SSL.")
	}
}