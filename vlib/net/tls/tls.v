// net.tls is an opt-in, pure-V TLS 1.3 (RFC 8446) implementation for TCP.
// Build with -d pure_tls to enable; otherwise net.ssl is used.
// Pure-V, TLS 1.3 only, ECDSA P-256/P-384/P-521, ML-DSA-44/65/87 and RSA
// leaves, RSA/ECDSA/ML-DSA direct-root chain verification, trust roots that
// resolve on every OS (explicit store, SSL_CERT_FILE/DIR, Unix paths,
// embedded Mozilla bundle). X25519MLKEM768 hybrid key exchange (the
// x.crypto.mlkem library also implements raw 512/1024 variants, which net.tls
// does not negotiate). CertificateVerify schemes:
// ecdsa 0x0403, 0x0503, 0x0603, mldsa 0x0904/0x0905/0x0906, rsa_pss_rsae
// 0x0804/0x0805/0x0806, rsa_pkcs1 0x0401/0x0501/0x0601. Stateless session
// tickets with psk_dhe_ke resumption (no 0-RTT), mTLS via CertificateRequest
// when Config.verify_client is set. No TLS 1.2, no DTLS.
// Cipher suites: AES-128-GCM-SHA256, AES-256-GCM-SHA384 (SHA-384 schedule)
// and ChaCha20-Poly1305-SHA256, negotiated by server preference
// (AES-128-GCM first).
module tls

import crypto.rand
import net

// Listener is a pure-V TLS 1.3 listener.
pub struct Listener {
mut:
	tcp net.TcpListener
	cfg Config
}

// listen binds a TLS listener. Default bind address is 127.0.0.1:0.
// listen generates a ticket encryption key when cfg.ticket_key is empty so
// every connection on the listener shares stateless session tickets.
pub fn listen(cfg Config) !&Listener {
	mut c := cfg
	if c.ticket_key.len == 0 {
		c.ticket_key = rand.bytes(32)!
	}
	if c.ticket_key.len != 32 {
		return error('tls: invalid ticket key length')
	}
	mut tl := net.listen_tcp(.ip, '127.0.0.1:0')!
	return &Listener{
		tcp: tl
		cfg: c
	}
}

pub fn (mut l Listener) accept() !&Conn {
	mut tcp := l.tcp.accept()!
	mut c := &Conn{
		cfg:       l.cfg
		tcp:       *tcp
		is_server: true
	}
	c.handshake() or {
		mut t := c.tcp
		t.close() or {}
		return err
	}
	return c
}

pub fn (mut l Listener) close() ! {
	l.tcp.close()!
}

pub fn (l &Listener) addr() !net.Addr {
	return l.tcp.addr()
}

// new_conn is a placeholder; full handshake lands in Phase 2.
pub fn new_conn(cfg Config) !&Conn {
	return &Conn{
		cfg: cfg
	}
}

pub fn (c &Conn) is_closed() bool {
	return c.closed
}

// negotiated_cipher returns the cipher suite negotiated for the connection.
// Before the handshake completes it reports AES-128-GCM (the zero value).
pub fn (c &Conn) negotiated_cipher() CipherSuite {
	return c.suite
}

pub const tls_version_1_3 = u16(0x0303)

// ProtocolVersion as used inside the supported_versions extension.
pub const supported_version_tls_1_3 = u16(0x0304)

// TLS 1.3 cipher suites (RFC 8446 §B.4). Only AEAD suites are defined.
pub enum CipherSuite {
	tls_aes_128_gcm_sha256       = 0x1301
	tls_aes_256_gcm_sha384       = 0x1302
	tls_chacha20_poly1305_sha256 = 0x1303
}

// ContentType (RFC 8446 §5.1).
pub enum ContentType {
	change_cipher_spec = 20
	alert              = 21
	handshake          = 22
	application_data   = 23
}

// HandshakeType (RFC 8446 §4).
pub enum HandshakeType {
	client_hello         = 1
	server_hello         = 2
	new_session_ticket   = 4
	end_of_early_data    = 5
	encrypted_extensions = 8
	certificate          = 11
	certificate_request  = 13
	certificate_verify   = 15
	finished             = 20
	key_update           = 24
	message_hash         = 254
}

// AlertLevel / AlertDescription (RFC 8446 §6).
pub enum AlertLevel {
	warning = 1
	fatal   = 2
}

pub enum AlertDescription {
	close_notify                    = 0
	unexpected_message              = 10
	bad_record_mac                  = 20
	record_overflow                 = 22
	handshake_failure               = 40
	bad_certificate                 = 42
	unsupported_certificate         = 43
	certificate_revoked             = 44
	certificate_expired             = 45
	certificate_unknown             = 46
	illegal_parameter               = 47
	unknown_ca                      = 48
	access_denied                   = 49
	decode_error                    = 50
	decrypt_error                   = 51
	protocol_version                = 70
	insufficient_security           = 71
	internal_error                  = 80
	inappropriate_fallback          = 86
	user_canceled                   = 90
	missing_extension               = 109
	unsupported_extension           = 110
	unrecognized_name               = 112
	bad_certificate_status_response = 113
	unknown_psk_identity            = 115
	certificate_required            = 116
	no_application_protocol         = 120
}

// PostQuantumMode controls whether hybrid PQ key exchange is offered.
pub enum PostQuantumMode {
	off            // classical only
	hybrid_prefer  // offer X25519MLKEM768, fall back to classical
	hybrid_require // require X25519MLKEM768, fail otherwise
}

// Config holds the settings for a TLS 1.3 connection (client or server).
// The field names intentionally resemble net.ssl's options, but the types
// differ: verify is a bool (not a CA path string), server_name is a Config
// field (net.ssl passes the hostname to connect() instead), and the
// verify_chain/ca_store/pq_mode/version fields have no net.ssl counterpart.
// File paths or PEM bytes are accepted for cert/key; intermediates,
// RSA-PSS CA signatures, SHA-1/MD5 and non-0x0303 versions
// are not supported in v1.
@[params]
pub struct Config {
pub mut:
	server_name    string
	alpn_protocols []string
	// Client side: CA bundle path or PEM bytes. Empty => default trust roots:
	// explicit ca_store wins, else SSL_CERT_FILE, SSL_CERT_DIR, Unix bundle
	// paths, then the embedded Mozilla bundle (works on every OS).
	ca_cert     string
	ca_cert_pem []u8
	// Server side: certificate + private key (PEM or file path).
	cert     string
	cert_pem []u8
	key      string
	key_pem  []u8
	// Client cert (optional, for mTLS).
	client_cert string
	client_key  string
	verify      bool = true
	validate    bool
	in_memory   bool
	pq_mode     PostQuantumMode
	// Min/max TLS version. Only 1.3 is supported in v1.
	min_version  u16  = tls_version_1_3
	max_version  u16  = tls_version_1_3
	verify_chain bool = true
	// Cipher suites to offer (client) or accept (server), in preference order
	// for selection. Empty means the implemented default
	// (AES-128-GCM, ChaCha20-Poly1305, AES-256-GCM).
	// Cipher suites to offer (client) or accept (server), in preference order
	// for selection. Empty means the implemented default
	// (AES-128-GCM, ChaCha20-Poly1305, AES-256-GCM).
	cipher_suites []CipherSuite
	// Optional trust store for chain verification. If empty, system roots are used.
	ca_store CertStore
	// Session-ticket encryption key (server side, 32 bytes). Generated at
	// listen() when empty and shared by every connection on that listener.
	ticket_key []u8
	// Resumption ticket from a previous connection (client side). When set,
	// the client offers it as a PSK with psk_dhe_ke.
	resume_from ?SessionState
	// Request a client certificate (server side, mTLS). The client cert is
	// verified against ca_store/system roots when verify_chain is set.
	verify_client bool
}

// Conn is defined in conn.v.

// ListenerOptions is an empty placeholder for future listener settings.
// ListenerOptions carries no options in v1 and is unrelated to
// net.ssl.SSLListenerOptions (which selects the listen address family);
// listen() always binds 127.0.0.1:0.
pub struct ListenerOptions {}
