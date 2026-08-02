module quic

// Coalesced-packet splitting (RFC 9000 §12.2). Initial, 0-RTT, and
// Handshake packets each carry a Length field, so multiple such packets can
// be coalesced back-to-back into one UDP datagram. Three packet forms have
// NO Length field and therefore always consume the rest of the datagram,
// never leaving room for anything to follow: a short header packet (no
// Length field at all, RFC 9000 §17.3.1), a Version Negotiation packet
// (version == 0, RFC 9000 §17.2.1), and a Retry packet (RFC 9000 §17.2.5).
// This is why split_coalesced_datagram's loop unconditionally stops the
// moment it encounters any of the three, rather than needing a separate
// explicit "assert nothing follows" check -- there is structurally nowhere
// else in the loop for it to continue to.

// CoalescedPacket is one packet's raw bytes, sliced out of a (possibly
// multi-packet) UDP datagram, along with its header form -- callers use
// `form` to route to the right subsequent parser (parse_long_header vs.
// parse_short_header vs. parse_version_negotiation) without re-deriving it.
pub struct CoalescedPacket {
pub:
	bytes []u8
	form  HeaderForm
}

// split_coalesced_datagram splits one received UDP datagram into its
// constituent QUIC packets, in wire order. Each returned packet's `bytes`
// is exactly that one packet's span, sliced (not copied) from `datagram`.
pub fn split_coalesced_datagram(datagram []u8) ![]CoalescedPacket {
	mut packets := []CoalescedPacket{}
	mut offset := 0
	for offset < datagram.len {
		remaining := datagram[offset..]
		form := peek_header_form(remaining)!

		if form == .short {
			// RFC 9000 §17.3.1's Fixed Bit (0x40) MUST be 1 for a genuine
			// short header packet, and is always sent in the clear (unlike
			// the reserved bits). A byte run that looks short-header-shaped
			// (top bit clear) but has the Fixed Bit ALSO clear is not a
			// real packet at all -- most commonly, trailing raw zero-byte
			// UDP datagram padding a PEER's implementation appended after
			// its last real packet rather than using PADDING frames (see
			// the real captured quiche server datagram this was discovered
			// against in coalesce_test.v, whose own Length-declared packet
			// ends far short of the datagram's full size -- this client's
			// own outgoing Initial packets pad internally instead, see
			// pad_initial_payload below, but a received datagram from
			// another implementation must still be tolerated either way).
			// Treat it as "no more real packets in this datagram" and stop,
			// rather than fabricating a bogus packet for it.
			if remaining[0] & 0x40 == 0 {
				break
			}
			packets << CoalescedPacket{
				bytes: remaining
				form:  .short
			}
			break
		}

		if remaining.len < 5 {
			return error('quic: truncated long header while splitting coalesced datagram (need at least 5 bytes, have ${remaining.len})')
		}
		version := (u32(remaining[1]) << 24) | (u32(remaining[2]) << 16) | (u32(remaining[3]) << 8) | u32(remaining[4])
		if version == 0 {
			// RFC 9000 §12.2: "there is no situation where a Retry or
			// Version Negotiation packet is coalesced with another
			// packet." A VN-shaped candidate appearing after this loop
			// has already collected an earlier real packet (offset != 0)
			// cannot be a genuine VN packet from a compliant sender --
			// same failure-closed treatment as the non-v1-version check
			// below, since anything past this point is untrustworthy.
			if offset != 0 {
				return error('quic: Version Negotiation packet cannot be coalesced after another packet (datagram offset ${offset}), per RFC 9000 §12.2')
			}
			// Version Negotiation: no Length field, consumes the rest of
			// the datagram -- and per RFC 9000 §17.2.1, a server never
			// coalesces anything else with it anyway. VN packets are
			// exempt from the Fixed Bit requirement below (it doesn't
			// apply to them), which is why this check runs first.
			packets << CoalescedPacket{
				bytes: remaining
				form:  .long
			}
			break
		}

		// As above for the short-header case: a non-VN long-header
		// candidate with a clear Fixed Bit is not a real packet -- stop
		// rather than misparse it.
		if remaining[0] & 0x40 == 0 {
			break
		}

		// peek_long_header_type's bit mapping is QUIC-v1-specific (see its
		// own doc comment in header.v) -- interpreting a non-v1 version's
		// type bits under the v1 mapping could misclassify it as `.retry`
		// below and consume the rest of the datagram, silently dropping any
		// real coalesced packets that follow. The length-having branch
		// below is already protected transitively (parse_long_header checks
		// this internally), but the retry branch calls peek_long_header_type
		// directly, so this check must run before EITHER branch, not just
		// be left to parse_long_header.
		if version != quic_v1 {
			return error('quic: coalesced packet has unsupported QUIC version 0x${version:08x}: only QUIC v1 (0x00000001) is supported')
		}

		typ := peek_long_header_type(remaining[0])!
		if typ == .retry {
			// Retry: also no Length field, also always consumes the rest
			// (RFC 9000 §17.2.5) -- the Retry Token has no explicit length
			// prefix; its extent is implicitly "everything except the
			// trailing 16-byte Integrity Tag", which retry.v's own parser
			// resolves, not this one.
			packets << CoalescedPacket{
				bytes: remaining
				form:  .long
			}
			break
		}

		// Initial, 0-RTT, or Handshake: has a Length field covering
		// (packet number + payload), so more packets may follow.
		header, header_len := parse_long_header(remaining)!
		total_len := u64(header_len) + header.length
		if total_len > u64(remaining.len) {
			return error('quic: coalesced packet at datagram offset ${offset} claims length ${total_len}, exceeding the ${remaining.len} bytes remaining')
		}
		packets << CoalescedPacket{
			bytes: remaining[..int(total_len)]
			form:  .long
		}
		offset += int(total_len)
	}
	return packets
}

// min_initial_datagram_size is RFC 9000 §14.1's minimum UDP datagram size
// for any datagram carrying an Initial packet -- an anti-amplification
// measure (a server must not be usable to amplify traffic toward a spoofed
// victim address by more than a small factor before the client's address
// has been validated).
pub const min_initial_datagram_size = 1200

// pad_initial_payload appends PADDING frame bytes (wire value 0x00, RFC
// 9000 §19.1) to a not-yet-protected Initial packet's payload so that the
// FINAL protected packet -- `header_len` bytes of already-built header +
// this (possibly padded) payload + `aead_tag_len` bytes of AEAD tag --
// reaches at least min_initial_datagram_size (1200) bytes. A no-op if the
// packet would already reach that size unpadded.
//
// This is RFC 9000 §14.1's PRIMARY padding mechanism -- "adding PADDING
// frames to the Initial packet" -- and the one every real implementation
// (quiche, ngtcp2, quinn) actually uses: the padding lands INSIDE the
// packet's own Length-delimited boundary and is authenticated by AEAD,
// unlike raw bytes appended to the datagram after protection (RFC 9000
// §12.2 requires every long-header packet's own Length field to cover its
// full extent; anything past it is a SEPARATE coalesced packet or, if it
// doesn't parse as one, discardable garbage -- not part of THIS packet).
// Callers must add this padding, recompute the header's own Length field
// to include it, and re-encode the header BEFORE calling protect_packet --
// see initial_exchange_test.v for the full sequence (padding first changes
// the payload length that flows into `length` and hence the AEAD-protected
// packet, so it cannot be applied to an already-protected packet the way a
// prior version of this padding scheme did).
pub fn pad_initial_payload(payload []u8, header_len int, aead_tag_len int) []u8 {
	total := header_len + payload.len + aead_tag_len
	if total >= min_initial_datagram_size {
		return payload
	}
	mut out := payload.clone()
	out << []u8{len: min_initial_datagram_size - total}
	return out
}
