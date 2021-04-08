import net

struct C.sockaddr_in6 {
	sin6_addr [16]byte
}

struct C.sockaddr_in {
	sin_addr [4]byte
}

fn test_offsets() {
	assert __offsetof(C.sockaddr_in6, sin6_addr) == __offsetof(net.Ip6, addr) + __offsetof(net.Addr, addr)
	assert __offsetof(C.sockaddr_in, sin_addr) == __offsetof(net.Ip, addr) + __offsetof(net.Addr, addr)
}

fn test_size() {
	assert sizeof(C.sockaddr_in) == sizeof(net.Ip)
	assert sizeof(C.sockaddr_in6) == sizeof(net.Ip6)
}
