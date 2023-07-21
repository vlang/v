module conv

// htn64 - DON'T USE, use hton64 instead
[deprecated: 'use hton64() instead']
[deprecated_after: '2023-12-31']
pub fn htn64(host u64) u64 {
	return hton64(host)
}

// hton64 converts the 64 bit value `host` to the net format (htonll)
pub fn hton64(host u64) u64 {
	$if little_endian {
		// vfmt off
		return ((host >> 56) & 0x00000000_000000FF) |
			   ((host >> 40) & 0x00000000_0000FF00) |
			   ((host >> 24) & 0x00000000_00FF0000) |
			   ((host >> 8)  & 0x00000000_FF000000) |
			   ((host << 8)  & 0x000000FF_00000000) |
			   ((host << 24) & 0x0000FF00_00000000) |
			   ((host << 40) & 0x00FF0000_00000000) |
			   ((host << 56) & 0xFF000000_00000000)
		// vfmt on
	} $else {
		return host
	}
}

// htn32 - DON'T USE, use hton32 instead
[deprecated: 'use hton32() instead']
[deprecated_after: '2023-12-31']
pub fn htn32(host u32) u32 {
	return hton32(host)
}

// hton32 converts the 32 bit value `host` to the net format (htonl)
pub fn hton32(host u32) u32 {
	$if little_endian {
		// vfmt off
		return ((host >> 24) & 0x0000_00FF) |
			   ((host >> 8)  & 0x0000_FF00) |
			   ((host << 8)  & 0x00FF_0000) |
			   ((host << 24) & 0xFF00_0000)
		// vfmt on
	} $else {
		return host
	}
}

// htn16 - DON'T USE, use hton16 instead
[deprecated: 'use hton16() instead']
[deprecated_after: '2023-12-31']
pub fn htn16(host u16) u16 {
	return hton16(host)
}

// hton16 converts the 16 bit value `host` to the net format (htons)
pub fn hton16(host u16) u16 {
	$if little_endian {
		// vfmt off
		return ((host >> 8) & 0x00FF) |
			   ((host << 8) & 0xFF00)
		// vfmt on
	} $else {
		return host
	}
}

// nth64 - DON'T USE, use ntoh64 instead
[deprecated: 'use ntoh64() instead']
[deprecated_after: '2023-12-31']
pub fn nth64(net u64) u64 {
	return ntoh64(net)
}

// ntoh64 converts the 64 bit value `net` to the host format (ntohll)
pub fn ntoh64(net u64) u64 {
	return hton64(net)
}

// nth32 - DON'T USE, use ntoh32 instead
[deprecated: 'use ntoh32() instead']
[deprecated_after: '2023-12-31']
pub fn nth32(net u32) u32 {
	return ntoh32(net)
}

// ntoh32 converts the 32 bit value `net` to the host format (ntohl)
pub fn ntoh32(net u32) u32 {
	return hton32(net)
}

// nth16 - DON'T USE, use ntoh16 instead
[deprecated: 'use ntoh16() instead']
[deprecated_after: '2023-12-31']
pub fn nth16(net u16) u16 {
	return ntoh16(net)
}

// ntoh16 converts the 16 bit value `net` to the host format (ntohs)
pub fn ntoh16(net u16) u16 {
	return hton16(net)
}
