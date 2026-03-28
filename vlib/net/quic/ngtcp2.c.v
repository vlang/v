module quic

// ngtcp2 C library bindings for QUIC support
// ngtcp2: https://github.com/ngtcp2/ngtcp2
//
// Installation:
//   macOS:   brew install ngtcp2
//   Ubuntu:  apt-get install libngtcp2-dev
//   Build:   See QUIC_LIBRARY_EVALUATION.md

#flag -lngtcp2
#flag -lngtcp2_crypto_ossl
#flag -lssl
#flag -lcrypto

#flag darwin -I/opt/homebrew/include
#flag darwin -L/opt/homebrew/lib
#flag darwin -I/opt/homebrew/opt/openssl@3/include
#flag darwin -L/opt/homebrew/opt/openssl@3/lib
#flag darwin -I/opt/homebrew/opt/libngtcp2/include
#flag darwin -L/opt/homebrew/opt/libngtcp2/lib

#include <ngtcp2/ngtcp2.h>
#include <ngtcp2/ngtcp2_crypto.h>
#include <ngtcp2/ngtcp2_crypto_ossl.h>

// Ngtcp2Conn is an opaque handle for an ngtcp2 connection.
pub type Ngtcp2Conn = voidptr

// Ngtcp2Settings is an opaque handle for ngtcp2 connection settings.
pub type Ngtcp2Settings = voidptr

// Ngtcp2TransportParams is an opaque handle for ngtcp2 transport parameters.
pub type Ngtcp2TransportParams = voidptr

// Ngtcp2Callbacks is an opaque handle for ngtcp2 callback functions.
pub type Ngtcp2Callbacks = voidptr

// Ngtcp2Cid is an opaque handle for an ngtcp2 connection ID.
pub type Ngtcp2Cid = voidptr

// Ngtcp2Path is an opaque handle for an ngtcp2 network path.
pub type Ngtcp2Path = voidptr

// Ngtcp2Pkt is an opaque handle for an ngtcp2 packet.
pub type Ngtcp2Pkt = voidptr

// Ngtcp2Vec is an opaque handle for an ngtcp2 scatter-gather vector.
pub type Ngtcp2Vec = voidptr

// QUIC stream write flags for ngtcp2_conn_writev_stream (ngtcp2 API).
pub const ngtcp2_write_stream_flag_none = u32(0x00)
pub const ngtcp2_write_stream_flag_fin = u32(0x01)
pub const ngtcp2_write_stream_flag_more = u32(0x02)

// QUIC recv_stream_data callback flags (from ngtcp2 API).
pub const ngtcp2_stream_data_flag_fin = u32(0x01)

// ngtcp2 error codes per RFC 9000.
pub const ngtcp2_err_invalid_argument = -201
pub const ngtcp2_err_nobuf = -203
pub const ngtcp2_err_proto = -205
pub const ngtcp2_err_invalid_state = -206
pub const ngtcp2_err_ack_frame = -207
pub const ngtcp2_err_stream_id_blocked = -208
pub const ngtcp2_err_stream_in_use = -209
pub const ngtcp2_err_stream_data_blocked = -210
pub const ngtcp2_err_flow_control = -211
pub const ngtcp2_err_connection_id_limit = -212
pub const ngtcp2_err_stream_limit = -213
pub const ngtcp2_err_final_size = -214
pub const ngtcp2_err_crypto = -215
pub const ngtcp2_err_pkt_num_exhausted = -216
pub const ngtcp2_err_required_transport_param = -217
pub const ngtcp2_err_malformed_transport_param = -218
pub const ngtcp2_err_frame_encoding = -219
pub const ngtcp2_err_tls_decrypt = -220
pub const ngtcp2_err_stream_shut_wr = -221
pub const ngtcp2_err_stream_not_found = -222
pub const ngtcp2_err_stream_state = -226
pub const ngtcp2_err_recv_version_negotiation = -229
pub const ngtcp2_err_closing = -230
pub const ngtcp2_err_draining = -231
pub const ngtcp2_err_transport_param = -234
pub const ngtcp2_err_discard_pkt = -235
pub const ngtcp2_err_conn_id_blocked = -237
pub const ngtcp2_err_internal = -238
pub const ngtcp2_err_crypto_buffer_exceeded = -239
pub const ngtcp2_err_write_stream_more = -240
pub const ngtcp2_err_retry = -241
pub const ngtcp2_err_drop_conn = -242
pub const ngtcp2_err_aead_limit_reached = -243
pub const ngtcp2_err_no_viable_path = -244
pub const ngtcp2_err_version_negotiation = -245
pub const ngtcp2_err_handshake_timeout = -246
pub const ngtcp2_err_version_negotiation_failure = -247
pub const ngtcp2_err_idle_close = -248

// Connection ID structure
pub struct Ngtcp2CidStruct {
pub mut:
	datalen u64
	data    [20]u8
}

// Preferred Address struct
pub struct Ngtcp2PreferredAddrStruct {
pub mut:
	cid                   Ngtcp2CidStruct
	ipv4                  [16]u8 // sockaddr_in
	ipv6                  [28]u8 // sockaddr_in6
	ipv4_present          u8
	ipv6_present          u8
	stateless_reset_token [16]u8
}

// Address structure (matches ngtcp2_addr: pointer + socklen_t)
pub struct Ngtcp2Addr {
pub mut:
	addr    voidptr // ngtcp2_sockaddr*
	addrlen u32     // ngtcp2_socklen
}

// Path structure (matches ngtcp2_path: local + remote + user_data)
pub struct Ngtcp2PathStruct {
pub mut:
	local     Ngtcp2Addr
	remote    Ngtcp2Addr
	user_data voidptr
}

// Packet info
pub struct Ngtcp2PktInfo {
pub mut:
	ecn u8
}

// Vector for scatter-gather I/O
pub struct Ngtcp2VecStruct {
pub mut:
	base voidptr
	len  u64
}

// Stream data structure
pub struct Ngtcp2StreamData {
pub mut:
	stream_id i64
	flags     u32
	data      voidptr
	datalen   u64
}

// Ngtcp2Ccerr holds connection close error information for CONNECTION_CLOSE frames.
pub struct Ngtcp2Ccerr {
pub mut:
	ccerr_type int // ngtcp2_ccerr_type
	error_code u64
	frame_type u64
	reason     voidptr
	reasonlen  usize
}

// Settings structure
pub struct Ngtcp2SettingsStruct {
pub mut:
	qlog_write                     voidptr
	cc_algo                        int
	initial_ts                     u64
	initial_rtt                    u64
	log_printf                     voidptr
	max_tx_udp_payload_size        u64
	token                          &u8
	tokenlen                       u64
	token_type                     int
	rand_ctx                       voidptr // struct with 1 voidptr
	max_window                     u64
	max_stream_window              u64
	ack_thresh                     u64
	no_tx_udp_payload_size_shaping u8
	handshake_timeout              u64
	preferred_versions             &u32
	preferred_versionslen          u64
	available_versions             &u32
	available_versionslen          u64
	original_version               u32
	no_pmtud                       u8
	initial_pkt_num                u32
	pmtud_probes                   &u16
	pmtud_probeslen                u64
	glitch_ratelim_burst           u64
	glitch_ratelim_rate            u64
}

// Ngtcp2VersionInfo holds ngtcp2 version information.
pub struct Ngtcp2VersionInfo {
pub mut:
	chosen_version        u32
	available_versions    &u8
	available_versionslen u64
}

// Transport parameters
pub struct Ngtcp2TransportParamsStruct {
pub mut:
	preferred_addr                      Ngtcp2PreferredAddrStruct
	original_dcid                       Ngtcp2CidStruct
	initial_scid                        Ngtcp2CidStruct
	retry_scid                          Ngtcp2CidStruct
	initial_max_stream_data_bidi_local  u64
	initial_max_stream_data_bidi_remote u64
	initial_max_stream_data_uni         u64
	initial_max_data                    u64
	initial_max_streams_bidi            u64
	initial_max_streams_uni             u64
	max_idle_timeout                    u64
	max_udp_payload_size                u64
	active_connection_id_limit          u64
	ack_delay_exponent                  u64
	max_ack_delay                       u64
	max_datagram_frame_size             u64
	stateless_reset_token_present       u8
	disable_active_migration            u8
	original_dcid_present               u8
	initial_scid_present                u8
	retry_scid_present                  u8
	preferred_addr_present              u8
	stateless_reset_token               [16]u8
	grease_quic_bit                     u8
	version_info                        Ngtcp2VersionInfo
	version_info_present                u8
}

// Callbacks structure (simplified)
pub struct Ngtcp2CallbacksStruct {
pub mut:
	client_initial                 voidptr
	recv_crypto_data               voidptr
	handshake_completed            voidptr
	recv_version_negotiation       voidptr
	encrypt                        voidptr
	decrypt                        voidptr
	hp_mask                        voidptr
	recv_stream_data               voidptr
	acked_stream_data_offset       voidptr
	stream_open                    voidptr
	stream_close                   voidptr
	recv_stateless_reset           voidptr
	recv_retry                     voidptr
	extend_max_streams_bidi        voidptr
	extend_max_streams_uni         voidptr
	rand                           voidptr
	get_new_connection_id          voidptr
	remove_connection_id           voidptr
	update_key                     voidptr
	path_validation                voidptr
	select_preferred_addr          voidptr
	stream_reset                   voidptr
	extend_max_remote_streams_bidi voidptr
	extend_max_remote_streams_uni  voidptr
	extend_max_stream_data         voidptr
	dcid_status                    voidptr
	handshake_confirmed            voidptr
	recv_new_token                 voidptr
	delete_crypto_aead_ctx         voidptr
	delete_crypto_cipher_ctx       voidptr
	recv_datagram                  voidptr
	ack_datagram                   voidptr
	lost_datagram                  voidptr
	get_path_challenge_data        voidptr
	stream_stop_sending            voidptr
	version_negotiation            voidptr
	recv_rx_key                    voidptr
	recv_tx_key                    voidptr
}

// C function declarations
fn C.ngtcp2_conn_client_new(pconn &voidptr, dcid &Ngtcp2CidStruct, scid &Ngtcp2CidStruct, path &Ngtcp2PathStruct, version u32, callbacks &Ngtcp2CallbacksStruct, settings &Ngtcp2SettingsStruct, params &Ngtcp2TransportParamsStruct, mem voidptr, user_data voidptr) int

fn C.ngtcp2_conn_del(conn voidptr)

fn C.ngtcp2_conn_read_pkt(conn voidptr, path &Ngtcp2PathStruct, pi &Ngtcp2PktInfo, pkt voidptr, pktlen u64, ts u64) int

fn C.ngtcp2_conn_write_pkt(conn voidptr, path &Ngtcp2PathStruct, pi &Ngtcp2PktInfo, dest voidptr, destlen u64, ts u64) i64

fn C.ngtcp2_conn_writev_stream(conn voidptr, path &Ngtcp2PathStruct, pi &Ngtcp2PktInfo, dest voidptr, destlen u64, pdatalen &i64, flags u32, stream_id i64, datav &Ngtcp2VecStruct, datavcnt u64, ts u64) i64

fn C.ngtcp2_conn_open_bidi_stream(conn voidptr, pstream_id &i64, user_data voidptr) int

fn C.ngtcp2_conn_open_uni_stream(conn voidptr, pstream_id &i64, user_data voidptr) int

fn C.ngtcp2_conn_shutdown_stream(conn voidptr, flags u32, stream_id i64, app_error_code u64) int

fn C.ngtcp2_conn_shutdown_stream_write(conn voidptr, flags u32, stream_id i64, app_error_code u64) int

fn C.ngtcp2_conn_shutdown_stream_read(conn voidptr, flags u32, stream_id i64, app_error_code u64) int

fn C.ngtcp2_conn_close_stream(conn voidptr, stream_id i64, app_error_code u64) int

fn C.ngtcp2_conn_write_connection_close(conn voidptr, path voidptr, pi voidptr, dest &u8, destlen usize, ccerr voidptr, ts u64) isize

fn C.ngtcp2_ccerr_default(ccerr voidptr)

fn C.ngtcp2_ccerr_set_transport_error(ccerr voidptr, error_code u64, reason voidptr, reasonlen usize)

fn C.ngtcp2_conn_get_max_data_left(conn voidptr) u64

fn C.ngtcp2_conn_get_streams_bidi_left(conn voidptr) u64

fn C.ngtcp2_conn_get_streams_uni_left(conn voidptr) u64

fn C.ngtcp2_settings_default(settings &Ngtcp2SettingsStruct)

fn C.ngtcp2_transport_params_default(params &Ngtcp2TransportParamsStruct)

fn C.ngtcp2_conn_set_remote_transport_params(conn voidptr, params &Ngtcp2TransportParamsStruct) int

fn C.ngtcp2_conn_get_remote_transport_params(conn voidptr) &Ngtcp2TransportParamsStruct

fn C.ngtcp2_conn_submit_crypto_data(conn voidptr, crypto_level u32, data voidptr, datalen u64) int

fn C.ngtcp2_conn_submit_new_token(conn voidptr, token voidptr, tokenlen u64) int

fn C.ngtcp2_conn_get_handshake_completed(conn voidptr) int

fn C.ngtcp2_conn_get_expiry(conn voidptr) u64

fn C.ngtcp2_conn_handle_expiry(conn voidptr, ts u64) int

fn C.ngtcp2_conn_get_idle_expiry(conn voidptr) u64

fn C.ngtcp2_conn_get_pto(conn voidptr) u64

fn C.ngtcp2_strerror(liberr int) &char

fn C.ngtcp2_err_is_fatal(liberr int) int

fn C.ngtcp2_version(least_version int) &Ngtcp2VersionInfo

fn C.ngtcp2_is_bidi_stream(stream_id i64) int

// Crypto functions
fn C.ngtcp2_crypto_ctx_initial(ctx voidptr) int

fn C.ngtcp2_crypto_derive_and_install_rx_key(conn voidptr, key voidptr, iv voidptr, hp_key voidptr, crypto_level u32, secret voidptr, secretlen u64) int

fn C.ngtcp2_crypto_derive_and_install_tx_key(conn voidptr, key voidptr, iv voidptr, hp_key voidptr, crypto_level u32, secret voidptr, secretlen u64) int

// ngtcp2_crypto callback helpers (from libngtcp2_crypto_ossl)
// These can be directly assigned to ngtcp2_callbacks fields.
fn C.ngtcp2_crypto_client_initial_cb(conn voidptr, user_data voidptr) int
fn C.ngtcp2_crypto_recv_crypto_data_cb(conn voidptr, encryption_level int, offset u64, data &u8, datalen usize, user_data voidptr) int
fn C.ngtcp2_crypto_encrypt_cb(dest &u8, aead voidptr, aead_ctx voidptr, plaintext &u8, plaintextlen usize, nonce &u8, noncelen usize, aad &u8, aadlen usize) int
fn C.ngtcp2_crypto_decrypt_cb(dest &u8, aead voidptr, aead_ctx voidptr, ciphertext &u8, ciphertextlen usize, nonce &u8, noncelen usize, aad &u8, aadlen usize) int
fn C.ngtcp2_crypto_hp_mask_cb(dest &u8, hp voidptr, hp_ctx voidptr, sample &u8) int
fn C.ngtcp2_crypto_recv_retry_cb(conn voidptr, hd voidptr, user_data voidptr) int
fn C.ngtcp2_crypto_update_key_cb(conn voidptr, rx_secret &u8, tx_secret &u8, rx_aead_ctx voidptr, rx_iv &u8, tx_aead_ctx voidptr, tx_iv &u8, current_rx_secret &u8, current_tx_secret &u8, secretlen usize, user_data voidptr) int
fn C.ngtcp2_crypto_delete_crypto_aead_ctx_cb(conn voidptr, aead_ctx voidptr, user_data voidptr)
fn C.ngtcp2_crypto_delete_crypto_cipher_ctx_cb(conn voidptr, cipher_ctx voidptr, user_data voidptr)
fn C.ngtcp2_crypto_get_path_challenge_data_cb(conn voidptr, data &u8, user_data voidptr) int
fn C.ngtcp2_crypto_version_negotiation_cb(conn voidptr, version u32, client_dcid voidptr, user_data voidptr) int

// ngtcp2_crypto_ossl context management
fn C.ngtcp2_crypto_ossl_init() int
fn C.ngtcp2_crypto_ossl_ctx_new(pctx &voidptr, ssl voidptr) int
fn C.ngtcp2_crypto_ossl_ctx_del(ctx voidptr)
fn C.ngtcp2_crypto_ossl_ctx_set_ssl(ctx voidptr, ssl voidptr)
fn C.ngtcp2_crypto_ossl_configure_client_session(ssl voidptr) int

// TLS native handle
fn C.ngtcp2_conn_set_tls_native_handle(conn voidptr, tls_native_handle voidptr)

// QuicPathAddrs holds per-connection socket address storage.
// It must outlive the ngtcp2_path that points into it.
// Matches the QuicPathAddrs typedef in quic_stubs.c.
// Fields use [16]u64 (128 bytes, 8-byte aligned) to match sockaddr_storage's
// alignment requirement on 64-bit platforms.
pub struct QuicPathAddrs {
pub mut:
	local_addr     [16]u64 // sockaddr_storage: 128 bytes, 8-byte aligned
	remote_addr    [16]u64 // sockaddr_storage: 128 bytes, 8-byte aligned
	local_addrlen  u32
	remote_addrlen u32
}

// QuicStreamEvents holds pending stream events from C callbacks.
// The C-side callbacks write FIN/close events here via user_data,
// and the V-side drains them after conn_read_pkt.
pub struct QuicStreamEvents {
pub mut:
	fin_stream_ids    [64]i64
	fin_count         int
	closed_stream_ids [64]i64
	closed_count      int
	overflow          int
}

// Custom C callbacks (defined in quic_stubs.c)
fn C.quic_rand_cb(dest &u8, destlen usize, rand_ctx voidptr)
fn C.quic_get_new_connection_id_cb(conn voidptr, cid voidptr, token &u8, cidlen usize, user_data voidptr) int
fn C.quic_init_callbacks(cb &Ngtcp2CallbacksStruct)
fn C.quic_setup_crypto(conn voidptr, ssl voidptr, hostname &char) int
fn C.quic_cleanup_crypto(ssl voidptr)
fn C.quic_resolve_and_set_path(path &Ngtcp2PathStruct, addrs &QuicPathAddrs, hostname &char, port int) int

// OpenSSL helpers
fn C.SSL_set_tlsext_host_name(ssl voidptr, name &char) int

// Timestamp helper
fn C.ngtcp2_timestamp() u64

// conn_client_new creates a new QUIC client connection.
pub fn conn_client_new(dcid &Ngtcp2CidStruct, scid &Ngtcp2CidStruct, path &Ngtcp2PathStruct, version u32, callbacks &Ngtcp2CallbacksStruct, settings &Ngtcp2SettingsStruct, params &Ngtcp2TransportParamsStruct, user_data voidptr) !voidptr {
	mut conn := unsafe { nil }
	rv := C.ngtcp2_conn_client_new(&conn, dcid, scid, path, version, callbacks, settings,
		params, unsafe { nil }, user_data)
	if rv != 0 {
		return error('ngtcp2_conn_client_new failed: ${strerror(rv)}')
	}
	return conn
}

// conn_del deletes a QUIC connection
pub fn conn_del(conn voidptr) {
	C.ngtcp2_conn_del(conn)
}

// conn_read_pkt reads a QUIC packet
pub fn conn_read_pkt(conn voidptr, path &Ngtcp2PathStruct, pi &Ngtcp2PktInfo, pkt []u8, ts u64) !int {
	rv := C.ngtcp2_conn_read_pkt(conn, path, pi, pkt.data, u64(pkt.len), ts)
	if rv < 0 {
		return error('ngtcp2_conn_read_pkt failed: ${strerror(int(rv))}')
	}
	return int(rv)
}

// conn_write_pkt writes a QUIC packet
pub fn conn_write_pkt(conn voidptr, path &Ngtcp2PathStruct, pi &Ngtcp2PktInfo, dest []u8, ts u64) !int {
	rv := C.ngtcp2_conn_write_pkt(conn, path, pi, dest.data, u64(dest.len), ts)
	if rv < 0 {
		return error('ngtcp2_conn_write_pkt failed: ${strerror(int(rv))}')
	}
	return int(rv)
}

// conn_writev_stream writes stream data with optional flags (e.g., FIN).
pub fn conn_writev_stream(conn voidptr, path &Ngtcp2PathStruct, pi &Ngtcp2PktInfo, dest []u8, stream_id i64, data []u8, ts u64, flags u32) !(int, i64) {
	mut datalen := i64(0)
	vec := Ngtcp2VecStruct{
		base: data.data
		len:  u64(data.len)
	}
	rv := C.ngtcp2_conn_writev_stream(conn, path, pi, dest.data, u64(dest.len), &datalen,
		flags, stream_id, &vec, 1, ts)
	if rv < 0 {
		return error('ngtcp2_conn_writev_stream failed: ${strerror(int(rv))}')
	}
	return int(rv), datalen
}

// conn_open_bidi_stream opens a bidirectional stream
pub fn conn_open_bidi_stream(conn voidptr, user_data voidptr) !i64 {
	mut stream_id := i64(0)
	rv := C.ngtcp2_conn_open_bidi_stream(conn, &stream_id, user_data)
	if rv != 0 {
		return error('ngtcp2_conn_open_bidi_stream failed: ${strerror(rv)}')
	}
	return stream_id
}

// conn_open_uni_stream opens a unidirectional stream
pub fn conn_open_uni_stream(conn voidptr, user_data voidptr) !i64 {
	mut stream_id := i64(0)
	rv := C.ngtcp2_conn_open_uni_stream(conn, &stream_id, user_data)
	if rv != 0 {
		return error('ngtcp2_conn_open_uni_stream failed: ${strerror(rv)}')
	}
	return stream_id
}

// conn_shutdown_stream shuts down a stream
pub fn conn_shutdown_stream(conn voidptr, stream_id i64, app_error_code u64) ! {
	rv := C.ngtcp2_conn_shutdown_stream(conn, 0, stream_id, app_error_code)
	if rv != 0 {
		return error('ngtcp2_conn_shutdown_stream failed: ${strerror(rv)}')
	}
}

// shutdown_stream_write wraps ngtcp2_conn_shutdown_stream_write.
pub fn shutdown_stream_write(conn voidptr, flags u32, stream_id i64, app_error_code u64) int {
	return C.ngtcp2_conn_shutdown_stream_write(conn, flags, stream_id, app_error_code)
}

// shutdown_stream_read wraps ngtcp2_conn_shutdown_stream_read.
pub fn shutdown_stream_read(conn voidptr, flags u32, stream_id i64, app_error_code u64) int {
	return C.ngtcp2_conn_shutdown_stream_read(conn, flags, stream_id, app_error_code)
}

// conn_write_connection_close writes a CONNECTION_CLOSE frame into dest.
pub fn conn_write_connection_close(conn voidptr, dest []u8, error_code u64, reason string, ts u64) !int {
	mut ccerr := Ngtcp2Ccerr{}
	C.ngtcp2_ccerr_default(&ccerr)
	mut reason_ptr := unsafe { nil }
	if reason.len > 0 {
		reason_ptr = voidptr(reason.str)
	}
	C.ngtcp2_ccerr_set_transport_error(&ccerr, error_code, reason_ptr, usize(reason.len))
	result := C.ngtcp2_conn_write_connection_close(conn, unsafe { nil }, unsafe { nil },
		dest.data, usize(dest.len), &ccerr, ts)
	if result < 0 {
		return error('connection close write failed: ${strerror(int(result))}')
	}
	return int(result)
}

// settings_default initializes settings with default values
pub fn settings_default(settings &Ngtcp2SettingsStruct) {
	C.ngtcp2_settings_default(settings)
}

// transport_params_default initializes transport params with default values
pub fn transport_params_default(params &Ngtcp2TransportParamsStruct) {
	C.ngtcp2_transport_params_default(params)
}

// conn_set_remote_transport_params sets remote transport parameters
pub fn conn_set_remote_transport_params(conn voidptr, params &Ngtcp2TransportParamsStruct) ! {
	rv := C.ngtcp2_conn_set_remote_transport_params(conn, params)
	if rv != 0 {
		return error('ngtcp2_conn_set_remote_transport_params failed: ${strerror(rv)}')
	}
}

// conn_get_handshake_completed checks if handshake is completed
pub fn conn_get_handshake_completed(conn voidptr) bool {
	return C.ngtcp2_conn_get_handshake_completed(conn) != 0
}

// conn_get_expiry returns the next expiry time for the connection in nanoseconds.
pub fn conn_get_expiry(conn voidptr) u64 {
	return C.ngtcp2_conn_get_expiry(conn)
}

// conn_handle_expiry notifies ngtcp2 that the connection timer has fired.
pub fn conn_handle_expiry(conn voidptr, ts u64) ! {
	rv := C.ngtcp2_conn_handle_expiry(conn, ts)
	if rv != 0 {
		return error('ngtcp2_conn_handle_expiry failed: ${strerror(rv)}')
	}
}

// strerror returns error string
pub fn strerror(liberr int) string {
	return unsafe { cstring_to_vstring(C.ngtcp2_strerror(liberr)) }
}

// err_is_fatal checks if error is fatal
pub fn err_is_fatal(liberr int) bool {
	return C.ngtcp2_err_is_fatal(liberr) != 0
}

// is_bidi_stream checks if stream ID is bidirectional
pub fn is_bidi_stream(stream_id i64) bool {
	return C.ngtcp2_is_bidi_stream(stream_id) != 0
}

// is_uni_stream checks if stream ID is unidirectional
pub fn is_uni_stream(stream_id i64) bool {
	return C.ngtcp2_is_bidi_stream(stream_id) == 0
}

// setup_crypto configures TLS/crypto for a client connection
pub fn setup_crypto(conn voidptr, ssl voidptr, hostname string) ! {
	rv := C.quic_setup_crypto(conn, ssl, &char(hostname.str))
	if rv != 0 {
		return error('failed to setup crypto for QUIC connection (code: ${rv})')
	}
}

// get_version returns ngtcp2 version information
pub fn get_version() Ngtcp2VersionInfo {
	info := C.ngtcp2_version(0)
	return unsafe { *info }
}
