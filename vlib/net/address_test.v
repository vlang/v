module net

fn test_diagnostics() {
	dump(aoffset)
	eprintln('--------')
	in6 := C.sockaddr_in6{}
	our_ip6 := Ip6{}
	$if macos {
		dump(__offsetof(C.sockaddr_in6, sin6_len))
	}
	dump(__offsetof(C.sockaddr_in6, sin6_family))
	dump(__offsetof(C.sockaddr_in6, sin6_port))
	dump(__offsetof(C.sockaddr_in6, sin6_addr))
	$if macos {
		dump(sizeof(in6.sin6_len))
	}
	dump(sizeof(in6.sin6_family))
	dump(sizeof(in6.sin6_port))
	dump(sizeof(in6.sin6_addr))
	dump(sizeof(in6))
	eprintln('')
	dump(__offsetof(Ip6, port))
	dump(__offsetof(Ip6, addr))
	dump(sizeof(our_ip6.port))
	dump(sizeof(our_ip6.addr))
	dump(sizeof(our_ip6))
	eprintln('--------')
	in4 := C.sockaddr_in{}
	our_ip4 := Ip{}
	$if macos {
		dump(__offsetof(C.sockaddr_in, sin_len))
	}
	dump(__offsetof(C.sockaddr_in, sin_family))
	dump(__offsetof(C.sockaddr_in, sin_port))
	dump(__offsetof(C.sockaddr_in, sin_addr))
	$if macos {
		dump(sizeof(in4.sin_len))
	}
	dump(sizeof(in4.sin_family))
	dump(sizeof(in4.sin_port))
	dump(sizeof(in4.sin_addr))
	dump(sizeof(in4))
	eprintln('')
	dump(__offsetof(Ip, port))
	dump(__offsetof(Ip, addr))
	dump(sizeof(our_ip4.port))
	dump(sizeof(our_ip4.addr))
	dump(sizeof(our_ip4))
	eprintln('--------')
	dump(__offsetof(C.sockaddr_un, sun_path))
	dump(__offsetof(Unix, path))
	eprintln('--------')
}

fn test_sizes_unix_sun_path() {
	x1 := C.sockaddr_un{}
	x2 := Unix{}
	assert sizeof(x1.sun_path) == sizeof(x2.path)
}

fn test_offsets_ipv6() {
	assert __offsetof(C.sockaddr_in6, sin6_addr) == __offsetof(Ip6, addr) + aoffset
	assert __offsetof(C.sockaddr_in6, sin6_port) == __offsetof(Ip6, port) + aoffset
}

fn test_offsets_ipv4() {
	assert __offsetof(C.sockaddr_in, sin_addr) == __offsetof(Ip, addr) + aoffset
	assert __offsetof(C.sockaddr_in, sin_port) == __offsetof(Ip, port) + aoffset
}

fn test_offsets_unix() {
	assert __offsetof(C.sockaddr_un, sun_path) == __offsetof(Unix, path) + aoffset
}

fn test_sizes_ipv6() {
	assert sizeof(C.sockaddr_in6) == sizeof(Ip6) + aoffset
}

fn test_sizes_ipv4() {
	assert sizeof(C.sockaddr_in) == sizeof(Ip) + aoffset
}

fn test_sizes_unix() {
	assert sizeof(C.sockaddr_un) == sizeof(Unix) + aoffset
}

fn test_ip_str() {
	ip1 := new_ip(1337, addr_ip_any).str()
	expected1 := '0.0.0.0:1337'
	assert ip1.len == expected1.len
	assert ip1 == expected1

	addr := [byte(2), 0, 2, 2]!
	ip2 := new_ip(2202, addr).str()
	expected2 := '2.0.2.2:2202'
	assert ip2.len == expected2.len
	assert ip2 == expected2
}

fn test_ip6_str() {
	ip1 := new_ip6(1337, addr_ip6_any).str()
	expected1 := '[::]:1337'
	assert ip1.len == expected1.len
	assert ip1 == expected1

	addr := [byte(2), 0, 2, 2, 2, 0, 1, 1, 2, 3, 2, 1, 2, 3, 5, 2]!
	ip2 := new_ip6(2022, addr).str()
	println(ip2)
	expected2 := '[200:202:200:101:203:201:203:502]:2022'
	assert ip2.len == expected2.len
	assert ip2 == expected2
}
