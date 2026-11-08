module tls

// TLS 1.3 (RFC 8446) version constant. Only 0x0303 is ever sent on the
// wire; the real version is negotiated via the supported_versions extension.
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

// Config mirrors the subset of net.ssl.SSLConnectConfig needed by this module.
// File paths or PEM bytes are accepted for cert/key/verify, matching
// net.ssl's in_memory_verification convention.
@[params]
pub struct Config {
pub mut:
	server_name    string
	alpn_protocols []string
	// Client side: CA bundle path or PEM bytes. Empty => system store (best effort).
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
	min_version u16 = tls_version_1_3
	max_version u16 = tls_version_1_3
}

// Conn is defined in conn.v.

// Listener is a pure-V TLS 1.3 listener.
pub struct Listener {
mut:
	cfg  Config
	addr string
}

// ListenerOptions mirrors net.ssl.SSLListenerOptions (placeholder).
pub struct ListenerOptions {}

// new_conn is a placeholder; full handshake lands in Phase 2.
pub fn new_conn(cfg Config) !&Conn {
	return &Conn{
		cfg: cfg
	}
}

pub fn (c &Conn) is_closed() bool {
	return c.closed
}
