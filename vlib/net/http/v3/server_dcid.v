module v3

// DCID extraction and connection lookup for incoming QUIC packets (RFC 9000 §5.2).
import net

// default_cid_len is the default connection ID length in bytes used by
// the server for parsing short header DCID fields (RFC 9000 §5.2).
const default_cid_len = 18

// extract_dcid_from_packet extracts the destination connection ID from a QUIC
// packet header as a hex string. For short headers (bit 7 = 0), DCID starts at
// byte 1 with length cid_len. For long headers (bit 7 = 1), byte 5 holds the
// DCID length and DCID starts at byte 6 (RFC 9000 §5.2).
pub fn extract_dcid_from_packet(packet []u8, cid_len int) !string {
	if packet.len < 2 {
		return error('packet too short to extract DCID')
	}

	is_long := (packet[0] & 0x80) != 0

	if is_long {
		return extract_dcid_long_header(packet)
	}
	return extract_dcid_short_header(packet, cid_len)
}

// extract_dcid_short_header reads DCID from a short header packet where the
// DCID starts at byte 1 and has the given cid_len (RFC 9000 §17.3).
fn extract_dcid_short_header(packet []u8, cid_len int) !string {
	end := 1 + cid_len
	if packet.len < end {
		return error('packet too short for short header DCID (need ${end}, have ${packet.len})')
	}
	return bytes_to_hex(packet[1..end])
}

// extract_dcid_long_header reads DCID from a long header packet where byte 5
// holds the DCID length and DCID starts at byte 6 (RFC 9000 §17.2).
fn extract_dcid_long_header(packet []u8) !string {
	if packet.len < 6 {
		return error('packet too short for long header DCID length field')
	}
	dcid_len := int(packet[5])
	end := 6 + dcid_len
	if packet.len < end {
		return error('packet too short for long header DCID (need ${end}, have ${packet.len})')
	}
	if dcid_len == 0 {
		return ''
	}
	return bytes_to_hex(packet[6..end])
}

// bytes_to_hex converts a byte slice to a lowercase hex string.
fn bytes_to_hex(data []u8) string {
	hex_chars := '0123456789abcdef'
	mut result := []u8{cap: data.len * 2}
	for b in data {
		result << hex_chars[b >> 4]
		result << hex_chars[b & 0x0f]
	}
	return result.bytestr()
}

// lookup_or_create_connection finds an existing connection by DCID or creates
// a new one. Uses CID-based lookup per RFC 9000 §5.2; falls back to creating
// a new connection for unknown CIDs (initial packets).
fn (mut s Server) lookup_or_create_connection(packet []u8, addr net.Addr) !&ServerConnection {
	addr_str := '${addr.str()}'
	dcid := extract_dcid_from_packet(packet, default_cid_len) or { '' }

	s.mu.lock()
	if dcid.len > 0 {
		if mut existing := s.connections[dcid] {
			s.mu.unlock()
			return existing
		}
	}

	if s.connections.len >= s.config.max_connections {
		s.mu.unlock()
		return error('H3_EXCESSIVE_LOAD: max connections limit reached (${s.config.max_connections})')
	}

	new_conn := s.create_connection(addr_str) or {
		s.mu.unlock()
		return error('failed to create connection: ${err}')
	}

	cid_key := if dcid.len > 0 {
		dcid
	} else {
		bytes_to_hex(new_conn.quic_conn.conn_id)
	}
	s.connections[cid_key] = new_conn
	s.mu.unlock()
	return new_conn
}
