module net

[_pack: '2']
struct Ip6 {
	port u16
	flow_info u32
	addr [16]byte
	scope_id u32 
}

[_pack: '4']
struct Ip {
	port u16
	addr [4]byte
}

[_pack: '8']
struct Addr {
pub:
	f u16
	addr AddrData
}
