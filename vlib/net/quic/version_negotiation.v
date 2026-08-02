module quic

// Client-side handling of a received Version Negotiation packet (RFC 9000
// §6.2, §17.2.1). header.v's parse_version_negotiation (Phase 1) already
// parses the wire format; this file adds the client-side interpretation
// policy for what to do with a parsed one.

// handle_version_negotiation inspects a parsed Version Negotiation packet
// against quic_v1, the only version this module implements or ever offers.
// VN packets are UNAUTHENTICATED (sent before any keys exist), so RFC 9000
// §6.2 draws a sharp line between the two possible outcomes -- a caller
// must not conflate them into one "VN always aborts" policy:
//
//   - The server's offered-version list INCLUDES v1: RFC 9000 §6.2, second
//     sentence -- "A client MUST discard a Version Negotiation packet that
//     lists the QUIC version selected by the client." This is the exact
//     shape a spoofed, off-path-injected VN packet would have (an attacker
//     with no visibility into the real server's actual (non-)response
//     cannot know whether v1 will be listed, but a client that abandons
//     its attempt on ANY VN packet turns this into a trivial connection-
//     kill primitive for such an attacker). Discarding it means exactly
//     that: this function returns successfully and the caller continues
//     the existing connection attempt as if the packet had never arrived
//     -- NOT an error, and NOT a retry with a different version.
//   - The list does NOT include v1: the normal, genuinely terminal case --
//     this client has no other version to fall back to, so the connection
//     attempt fails cleanly. This is the only outcome that returns an
//     error.
pub fn handle_version_negotiation(vn QuicVersionNegotiation) ! {
	for v in vn.versions {
		if v == quic_v1 {
			return
		}
	}
	return error('quic: server does not support QUIC v1 (Version Negotiation offered: ${vn.versions}); this client implements only v1, connection cannot proceed')
}
