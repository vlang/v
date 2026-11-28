// Copyright (c) 2026 blackshirt.
//
// alert.v models the RFC 8446 §6 alert vocabulary: the AlertLevel/AlertDescription
// enums and their wire encoding — shared transport vocabulary each adapter frames or
// maps its own way (QUIC folds the code into a CRYPTO_ERROR, RFC 9001 §4.8).
module handshake

// AlertLevel classifies an alert as warning or fatal (RFC 8446 §6).
// In TLS 1.3 only close_notify may be a warning; every other alert is fatal.
pub enum AlertLevel as u8 {
	warning = 1
	fatal   = 2
}

// AlertDescription identifies the alert reason (RFC 8446 §6.2).
pub enum AlertDescription as u8 {
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

// Alert is the 2-byte TLS alert payload, read-only outside this module; @[noinit]
// keeps a caller from fabricating an out-of-range level.
@[noinit]
pub struct Alert {
pub:
	level       AlertLevel       // warning (1) or fatal (2)
	description AlertDescription // what went wrong (RFC 8446 §6.2)
}

// encode serializes the alert to 2 bytes.
pub fn (a Alert) encode() []u8 {
	return [u8(a.level), u8(a.description)]
}

// parse_alert decodes a 2-byte alert payload.
pub fn parse_alert(data []u8) !Alert {
	if data.len != 2 {
		return error('handshake: alert payload must be exactly 2 bytes, got ${data.len}')
	}
	level := match data[0] {
		1 { AlertLevel.warning }
		2 { AlertLevel.fatal }
		else {
			return error('handshake: invalid alert level ${data[0]}')
		}
	}
	// The cast is unsafe on purpose: RFC 8446 §6 treats unknown alert types as error alerts.
	desc := unsafe { AlertDescription(data[1]) }
	return Alert{
		level:       level
		description: desc
	}
}

// quic_connection_error maps an alert to the QUIC CRYPTO_ERROR application error code
// (RFC 9001 §4.8): 0x100 plus the alert description byte.
pub fn (d AlertDescription) quic_connection_error() u32 {
	return 0x100 + u32(d)
}

// new_alert builds an alert with the given description; RFC 8446 §6 makes every alert
// except close_notify fatal, so close_notify is the only warning-level description.
// Sending close_notify at warning level is deliberate interoperability heritage
// (matching Go crypto/tls); §6 also permits a fatal-level close_notify, so a
// receiver must accept the description under both levelings and gate only on
// non-close_notify warnings.
pub fn new_alert(description AlertDescription) Alert {
	return Alert{
		level:       if description == .close_notify { .warning } else { .fatal }
		description: description
	}
}
