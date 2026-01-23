// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
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

#include <ngtcp2/ngtcp2.h>
#include <ngtcp2/ngtcp2_crypto.h>
#include <ngtcp2/ngtcp2_crypto_ossl.h>

// Core ngtcp2 types (opaque pointers)
pub type Ngtcp2Conn = voidptr
pub type Ngtcp2Settings = voidptr
pub type Ngtcp2TransportParams = voidptr
pub type Ngtcp2Callbacks = voidptr
pub type Ngtcp2Cid = voidptr
pub type Ngtcp2Path = voidptr
pub type Ngtcp2Pkt = voidptr
pub type Ngtcp2Vec = voidptr

// Error codes
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
	datalen u8
	data    [20]u8
}

// Path structure
pub struct Ngtcp2PathStruct {
pub mut:
	local_addr_len  u32
	local_addr      voidptr
	remote_addr_len u32
	remote_addr     voidptr
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

// Settings structure
pub struct Ngtcp2SettingsStruct {
pub mut:
	max_tx_udp_payload_size                     u64
	ack_delay_exponent                          u8
	max_ack_delay                               u64
	max_udp_payload_size                        u64
	initial_max_stream_data_bidi_local          u64
	initial_max_stream_data_bidi_remote         u64
	initial_max_stream_data_uni                 u64
	initial_max_data                            u64
	initial_max_streams_bidi                    u64
	initial_max_streams_uni                     u64
	max_idle_timeout                            u64
	active_connection_id_limit                  u64
	stateless_reset_token_present               u8
	stateless_reset_token                       [16]u8
	preferred_address_present                   u8
	disable_active_migration                    u8
	original_version                            u32
	no_pmtud                                    u8
	initial_rtt                                 u64
	initial_max_stream_data_bidi_local_present  u8
	initial_max_stream_data_bidi_remote_present u8
	initial_max_stream_data_uni_present         u8
	initial_max_data_present                    u8
	initial_max_streams_bidi_present            u8
	initial_max_streams_uni_present             u8
	max_idle_timeout_present                    u8
	active_connection_id_limit_present          u8
	max_udp_payload_size_present                u8
	ack_delay_exponent_present                  u8
	max_ack_delay_present                       u8
	max_tx_udp_payload_size_present             u8
}

// Transport parameters
pub struct Ngtcp2TransportParamsStruct {
pub mut:
	initial_max_stream_data_bidi_local  u64
	initial_max_stream_data_bidi_remote u64
	initial_max_stream_data_uni         u64
	initial_max_data                    u64
	initial_max_streams_bidi            u64
	initial_max_streams_uni             u64
	max_idle_timeout                    u64
	max_udp_payload_size                u64
	active_connection_id_limit          u64
	ack_delay_exponent                  u8
	max_ack_delay                       u64
	disable_active_migration            u8
	stateless_reset_token_present       u8
	stateless_reset_token               [16]u8
	preferred_address_present           u8
	original_version                    u32
	retry_scid_present                  u8
	retry_scid                          Ngtcp2CidStruct
	initial_scid_present                u8
	initial_scid                        Ngtcp2CidStruct
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
fn C.ngtcp2_conn_client_new(pconn &&voidptr, dcid &Ngtcp2CidStruct, scid &Ngtcp2CidStruct, path &Ngtcp2PathStruct, version u32, callbacks &Ngtcp2CallbacksStruct, settings &Ngtcp2SettingsStruct, params &Ngtcp2TransportParamsStruct, mem voidptr, user_data voidptr) int

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

fn C.ngtcp2_is_uni_stream(stream_id i64) int

// Crypto functions
fn C.ngtcp2_crypto_ctx_initial(ctx voidptr) int

fn C.ngtcp2_crypto_derive_and_install_rx_key(conn voidptr, key voidptr, iv voidptr, hp_key voidptr, crypto_level u32, secret voidptr, secretlen u64) int

fn C.ngtcp2_crypto_derive_and_install_tx_key(conn voidptr, key voidptr, iv voidptr, hp_key voidptr, crypto_level u32, secret voidptr, secretlen u64) int

// Version info
pub struct Ngtcp2VersionInfo {
pub mut:
	age               int
	version_num       int
	version_str       &char
	proto_version_num int
	proto_version_str &char
}

// V wrapper functions

// conn_client_new creates a new QUIC client connection
pub fn conn_client_new(dcid &Ngtcp2CidStruct, scid &Ngtcp2CidStruct, path &Ngtcp2PathStruct, version u32, callbacks &Ngtcp2CallbacksStruct, settings &Ngtcp2SettingsStruct, params &Ngtcp2TransportParamsStruct, user_data voidptr) !voidptr {
	mut conn := unsafe { nil }
	mut conn_ptr := &conn
	rv := C.ngtcp2_conn_client_new(&conn_ptr, dcid, scid, path, version, callbacks, settings,
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

// conn_writev_stream writes stream data
pub fn conn_writev_stream(conn voidptr, path &Ngtcp2PathStruct, pi &Ngtcp2PktInfo, dest []u8, stream_id i64, data []u8, ts u64) !(int, i64) {
	mut datalen := i64(0)
	vec := Ngtcp2VecStruct{
		base: data.data
		len:  u64(data.len)
	}
	rv := C.ngtcp2_conn_writev_stream(conn, path, pi, dest.data, u64(dest.len), &datalen,
		0, stream_id, &vec, 1, ts)
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
	return C.ngtcp2_is_uni_stream(stream_id) != 0
}

// get_version returns ngtcp2 version information
pub fn get_version() Ngtcp2VersionInfo {
	info := C.ngtcp2_version(0)
	return unsafe { *info }
}
