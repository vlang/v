module quic

// Client-side handling of a received Version Negotiation packet (RFC 9000
// §6.2, §17.2.1). header.v's parse_version_negotiation (Phase 1) already
// parses the wire format; this file adds the client-side interpretation
// policy for what to do with a parsed one.

// handle_version_negotiation inspects a parsed Version Negotiation packet
// against quic_v1, the only version this module implements or ever offers.
// There is no "fall back to a lower version" outcome here, unlike some
// other protocols' version negotiation -- a VN packet is ALWAYS terminal
// for this client, just for one of two distinct reasons:
//
//   - The server's offered-version list INCLUDES v1: per RFC 9000 §6.2, "A
//     server MUST NOT send a Version Negotiation packet if it would
//     accept... the version indicated in the client's Initial packet." If
//     it did so anyway while still listing v1 as supported, that is not a
//     legitimate negotiation signal -- a compliant server would simply
//     have continued the handshake in v1. Treated as a hard protocol
//     violation, not a retry trigger.
//   - The list does NOT include v1: the normal, if still terminal, case --
//     this client has no other version to fall back to, so the connection
//     attempt fails cleanly.
//
// Both outcomes return an error (there is no success return for this
// function to make); the message distinguishes which case occurred.
pub fn handle_version_negotiation(vn QuicVersionNegotiation) ! {
	for v in vn.versions {
		if v == quic_v1 {
			return error('quic: server sent Version Negotiation listing v1 (the version this client already used) -- PROTOCOL_VIOLATION per RFC 9000 §6.2')
		}
	}
	return error('quic: server does not support QUIC v1 (Version Negotiation offered: ${vn.versions}); this client implements only v1, connection cannot proceed')
}
