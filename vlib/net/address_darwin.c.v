module net

const max_unix_path = 106

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

[_pack: '1']
struct Ip6 {
	port      u16
	flow_info u32
	addr      [16]byte
	scope_id  u32
}

[_pack: '1']
struct Ip {
	port u16
	addr [4]byte
	// Pad to size so that socket functions
	// dont complain to us (see  in.h and bind())
	// TODO(emily): I would really like to use
	// some constant calculations here
	// so that this doesnt have to be hardcoded
	sin_pad [10]byte
}

[_pack: '1']
struct Unix {
	path [max_unix_path]byte
}

[_pack: '1']
struct Addr {
pub:
	len  u8
	f    u16
	addr AddrData
}
