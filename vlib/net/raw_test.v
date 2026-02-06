import net

fn test_protocol_enum() {
	assert int(net.Protocol.icmp) == 1
	assert int(net.Protocol.tcp) == 6
	assert int(net.Protocol.udp) == 17
	assert int(net.Protocol.icmpv6) == 58
	assert int(net.Protocol.raw) == 255
}

fn test_socket_type_raw() {
	assert int(net.SocketType.raw) > 0
}

fn test_raw_socket_config_defaults() {
	config := net.RawSocketConfig{}
	assert config.family == .ip
	assert config.protocol == .icmp
}

fn test_raw_socket_config_custom() {
	config := net.RawSocketConfig{
		family:   .ip6
		protocol: .icmpv6
	}
	assert config.family == .ip6
	assert config.protocol == .icmpv6
}

fn test_raw_socket_creation_requires_privileges() {
	mut sock := net.new_raw_socket(family: .ip, protocol: .icmp) or {
		assert err.msg().len > 0
		return
	}
	sock.close() or {}
}

fn test_raw_socket_protocols() {
	protocols := [
		net.Protocol.icmp,
		net.Protocol.icmpv6,
		net.Protocol.raw,
	]

	for proto in protocols {
		config := net.RawSocketConfig{
			family:   if proto == .icmpv6 { net.AddrFamily.ip6 } else { net.AddrFamily.ip }
			protocol: proto
		}
		assert config.protocol == proto
	}
}
