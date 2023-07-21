module conv

// htn64 converts a the 64 bit value `host` to the net format (htonll)
pub fn htn64(host u64) u64 {
	$if little_endian {
		$if amd64 && !tinyc {
			mut r := host
			asm volatile amd64 {
				bswap a
				; +r (r) as a
			}
			return r
		} $else {
			// vfmt off
			return (
				((host >> 56) & 0x00000000_000000FF) |
				((host >> 40) & 0x00000000_0000FF00) |
				((host >> 24) & 0x00000000_00FF0000) |
				((host >> 8)  & 0x00000000_FF000000) |
				((host << 8)  & 0x000000FF_00000000) |
				((host << 24) & 0x0000FF00_00000000) |
				((host << 40) & 0x00FF0000_00000000) |
				((host << 56) & 0xFF000000_00000000)
			)
			// vfmt on
		}
	} $else {
		return host
	}
}

// htn32 converts the 32 bit value `host` to the net format (htonl)
pub fn htn32(host u32) u32 {
	$if little_endian {
		$if !tinyc && (amd64 || i386) {
			mut r := host
			asm amd64 {
				bswap a
				; +r (r) as a
			}
			return r
		} $else {
			// vfmt off
			return (
				((host >> 24) & 0x0000_00FF) |
				((host >> 8)  & 0x0000_FF00) |
				((host << 8)  & 0x00FF_0000) |
				((host << 24) & 0xFF00_0000)
			)
			// vfmt on
		}
	} $else {
		return host
	}
}

// htn16 converts the 16 bit value `host` to the net format (htons)
pub fn htn16(host u16) u16 {
	$if little_endian {
		$if !tinyc && (amd64 || i386) {
			mut r := host
			asm amd64 {
				rol a, 8
				; +r (r) as a
			}
			return r
		} $else {
			// vfmt off
			return (
				((host >> 8) & 0x00FF) |
				((host << 8) & 0xFF00)
			)
			// vfmt on
		}
	} $else {
		return host
	}
}

// nth64 converts the 64 bit value `net` to the host format (ntohll)
pub fn nth64(net u64) u64 {
	return htn64(net)
}

// nth32 converts the 32 bit value `net` to the host format (ntohl)
pub fn nth32(net u32) u32 {
	return htn32(net)
}

// nth16 converts the 16 bit value `net` to the host format (ntohs)
pub fn nth16(net u16) u16 {
	return htn16(net)
}
