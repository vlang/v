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
			// UDP datagram padding appended after the last real packet
			// (see pad_datagram_for_initial below, and the real captured
			// quiche datagram this was discovered against, whose own
			// Length-declared packet ends far short of the datagram's full
			// size). Treat it as "no more real packets in this datagram"
			// and stop, rather than fabricating a bogus packet for it.
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

// pad_datagram_for_initial pads `datagram` with trailing zero bytes so it
// meets min_initial_datagram_size, a no-op if it already does. This is raw
// UDP-datagram-level padding OUTSIDE any QUIC packet's own Length-declared
// boundary -- NOT QUIC PADDING frames inside a packet's AEAD-protected
// payload. Confirmed against a real independent implementation: the
// captured quiche Client Initial packet in testdata/tls13_vectors/ pads
// this exact way (its own Length field covers only ~336 of the datagram's
// 1200 bytes; the rest is trailing zero bytes never covered by AEAD at
// all), found while building Phase 3's known-answer test.
pub fn pad_datagram_for_initial(datagram []u8) []u8 {
	if datagram.len >= min_initial_datagram_size {
		return datagram
	}
	mut out := datagram.clone()
	out << []u8{len: min_initial_datagram_size - datagram.len}
	return out
}
