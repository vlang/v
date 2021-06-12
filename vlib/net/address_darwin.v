module net

const max_unix_path = 104

[_pack: '1']
struct Ip6 {
	port      u16
	flow_info u32
	addr      [16]byte
	scope_id  u32
	sin6_pad  [2]byte
}

[_pack: '4']
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
