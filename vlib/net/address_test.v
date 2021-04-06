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
