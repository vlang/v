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
//
// split_coalesced_datagram draws a sharp line between two different ways a
// candidate chunk can be "wrong", handled by opposite policies:
//   - A VN or Retry packet appearing at a NONZERO offset (something else
//     was already collected before it) is a definite RFC 9000 §12.2
//     protocol violation -- no compliant sender ever produces this -- and
//     fails the WHOLE call.
//   - A chunk that fails to structurally resolve into ANY legitimate next
//     packet (too short for a header, an unsupported version, or a Length
//     field claiming more than remains) is treated as RFC 9000 §14.1's
//     explicitly sanctioned "invalid packet" datagram padding: splitting
//     stops there and whatever was already collected is returned
//     successfully, discarding only the trailing remainder.
// A subsequent long-header packet whose DCID differs from the first
// packet's is excluded from the result (RFC 9000 §12.2, SHOULD) but does
// NOT stop the walk -- unlike the two cases above, scanning continues past
// it for whatever legitimately-addressed packet may follow.

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
	// RFC 9000 §12.2: "Receivers SHOULD ignore any subsequent packets with
	// a different Destination Connection ID than the first packet in the
	// datagram." Only tracked for long-header, Length-having packets
	// (Initial/0-RTT/Handshake) -- a short-header packet's DCID length
	// isn't on the wire (it depends on connection context this stateless
	// splitter doesn't have), so it can't be compared, and VN/Retry can
	// only ever be the datagram's sole packet (enforced above) so there is
	// never a "subsequent" one to check against them.
	mut first_dcid := ?[]u8(none)
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
				form: .short
			}
			break
		}

		if remaining.len < 5 {
			// RFC 9000 §14.1 explicitly allows a sender to pad an Initial
			// datagram by coalescing "invalid packets, which a receiver
			// will discard" -- a handful of leftover bytes too short for
			// even a long header's fixed prefix is exactly that shape.
			// Whatever real packets were already collected before this
			// point remain valid; only this trailing remainder is
			// discarded, not the whole datagram.
			break
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
				form: .long
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
			// Same RFC 9000 §14.1 "invalid packet" allowance as above: an
			// unsupported-version trailing chunk is exactly the shape a
			// real sender may legitimately use as datagram padding --
			// discard it and stop, rather than failing packets already
			// validated before it.
			break
		}

		typ := peek_long_header_type(remaining[0])!
		if typ == .retry {
			// RFC 9000 §12.2: "there is no situation where a Retry or
			// Version Negotiation packet is coalesced with another
			// packet." Same failure-closed treatment as the VN check above
			// -- a Retry-shaped candidate appearing after this loop has
			// already collected an earlier real packet cannot be a genuine
			// Retry from a compliant sender.
			if offset != 0 {
				return error('quic: Retry packet cannot be coalesced after another packet (datagram offset ${offset}), per RFC 9000 §12.2')
			}
			// Retry: also no Length field, also always consumes the rest
			// (RFC 9000 §17.2.5) -- the Retry Token has no explicit length
			// prefix; its extent is implicitly "everything except the
			// trailing 16-byte Integrity Tag", which retry.v's own parser
			// resolves, not this one.
			packets << CoalescedPacket{
				bytes: remaining
				form: .long
			}
			break
		}

		// Initial, 0-RTT, or Handshake: has a Length field covering
		// (packet number + payload), so more packets may follow.
		header, header_len := parse_long_header(remaining) or { break }
		total_len := u64(header_len) + header.length
		if total_len > u64(remaining.len) {
			// Same RFC 9000 §14.1 allowance: a Length field claiming more
			// than actually remains cannot be trusted to delimit a real
			// next packet, so there is no safe way to keep walking past
			// it either -- discard it and stop, rather than failing
			// packets already validated before it.
			break
		}
		// NOTE: no check here rejects an Initial packet for the overall
		// datagram being under RFC 9000 14.1's 1200-byte floor. RFC
		// 9000 14.1's discard MUST binds SERVERS receiving an
		// undersized CLIENT Initial (anti-amplification); a server's
		// OWN padding requirement applies only to ACK-ELICITING Initial
		// packets (Table 3 marks ACK/PADDING/CONNECTION_CLOSE "N", not
		// ack-eliciting), so a legitimate server response containing
		// only e.g. an ACK or an early CONNECTION_CLOSE can be smaller
		// than 1200 bytes -- this splitter has no visibility into frame
		// contents (packets are still AEAD-protected here) to tell that
		// case apart from a non-compliant/malicious one. A prior round
		// added an unconditional size-based discard here and broke
		// exactly this legitimate case (Codex P2, pullrequestreview-
		// 4843018164, reverting pullrequestreview-4839675790's fix).
		// The real requirement (server-side, role-aware, and only
		// after decryption reveals whether the payload is ack-
		// eliciting) belongs in a later phase with that visibility
		// (Phase 9/13's connection state machine), not this stateless,
		// pre-decryption, role-agnostic packet splitter.
		if fd := first_dcid {
			if header.dcid != fd {
				// Ignore -- not error -- per the SHOULD above: this one
				// packet is excluded from the result, but its own Length
				// field still tells us exactly where it ends, so keep
				// walking the datagram for whatever legitimately-addressed
				// packet may follow it.
				offset += int(total_len)
				continue
			}
		} else {
			first_dcid = header.dcid.clone()
		}
		packets << CoalescedPacket{
			bytes: remaining[..int(total_len)]
			form: .long
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
// `header_len` MUST be the full on-wire header length INCLUDING the packet
// number field -- i.e. `encode_long_header(...).len + pn_length`, not just
// the encoded long header by itself (see initial_exchange_test.v for the
// exact call shape). The packet number is encoded separately from the long
// header proper but still occupies bytes of the final protected packet
// this function is sizing against; omitting it under-counts `total` by
// 1-4 bytes and silently under-pads below the RFC 9000 §14.1 floor.
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
