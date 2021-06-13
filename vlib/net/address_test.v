module net

$if windows {
	$if msvc {
		#include <afunix.h>
	} $else {
		#include "@VROOT/vlib/net/afunix.h"
	}
} $else {
	#include <sys/un.h>
}

struct C.sockaddr_in6 {
	sin6_len    u8
	sin6_family u16
	sin6_port   u16
	sin6_addr   [4]u32
}

struct C.sockaddr_in {
	sin_len    u8
	sin_family u16
	sin_port   u16
	sin_addr   u32
	sin_zero   [8]i8
}

struct C.sockaddr_un {
	sun_path int
}

const aoffset = __offsetof(Addr, addr)

fn test_diagnostics() {
	dump(net.aoffset)
	eprintln('--------')
	in6 := C.sockaddr_in6{}
	our_ip6 := Ip6{}
	dump(__offsetof(C.sockaddr_in6, sin6_len))
	dump(__offsetof(C.sockaddr_in6, sin6_family))
	dump(__offsetof(C.sockaddr_in6, sin6_port))
	dump(__offsetof(C.sockaddr_in6, sin6_addr))
	dump(sizeof(in6.sin6_len))
	dump(sizeof(in6.sin6_family))
	dump(sizeof(in6.sin6_port))
	dump(sizeof(in6.sin6_addr))
	eprintln('')
	dump(__offsetof(Ip6, port))
	dump(__offsetof(Ip6, addr))
	dump(sizeof(our_ip6.port))
	dump(sizeof(our_ip6.addr))
	eprintln('--------')
	in4 := C.sockaddr_in{}
	our_ip4 := Ip{}
	dump(__offsetof(C.sockaddr_in, sin_len))
	dump(__offsetof(C.sockaddr_in, sin_family))
	dump(__offsetof(C.sockaddr_in, sin_port))
	dump(__offsetof(C.sockaddr_in, sin_addr))
	dump(sizeof(in4.sin_len))
	dump(sizeof(in4.sin_family))
	dump(sizeof(in4.sin_port))
	dump(sizeof(in4.sin_addr))
	eprintln('')
	dump(__offsetof(Ip, port))
	dump(__offsetof(Ip, addr))
	dump(sizeof(our_ip4.port))
	dump(sizeof(our_ip4.addr))
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
	assert __offsetof(C.sockaddr_in6, sin6_addr) == __offsetof(Ip6, addr) + net.aoffset
	assert __offsetof(C.sockaddr_in6, sin6_port) == __offsetof(Ip6, port) + net.aoffset
}

fn test_offsets_ipv4() {
	assert __offsetof(C.sockaddr_in, sin_addr) == __offsetof(Ip, addr) + net.aoffset
	assert __offsetof(C.sockaddr_in, sin_port) == __offsetof(Ip, port) + net.aoffset
}

fn test_offsets_unix() {
	assert __offsetof(C.sockaddr_un, sun_path) == __offsetof(Unix, path) + net.aoffset
}

fn test_sizes_ipv6() {
	assert sizeof(C.sockaddr_in6) == sizeof(Ip6)
}

fn test_sizes_ipv4() {
	assert sizeof(C.sockaddr_in) == sizeof(Ip)
}

fn test_sizes_unix() {
	assert sizeof(C.sockaddr_un) == sizeof(Unix)
	// ^ the above fails for strict == on linux with:
	// > assert sizeof(Type(80)) == sizeof(Type(84))
	//       Left value: 110
	//      Right value: 104
}
