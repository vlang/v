module net

pub const fionbio = u32(0x8004667e)

// WsaError is the subset of Windows socket errors needed for cross C generation.
pub enum WsaError {
	wsaeintr                    = 10004
	wsaewouldblock              = 10035
	wsaeinprogress              = 10036
	wsaeafnosupport             = 10047
	wsaeaddrnotavail            = 10049
	wsaenetdown                 = 10050
	wsaenetunreach              = 10051
	wsaenotconn                 = 10057
	wsaeconnrefused             = 10061
	wsaehostdown                = 10064
	wsaehostunreach             = 10065
	wsaeprotonosupport          = 10043
	wsaetimedout                = 10060
	wsahost_not_found           = 11001
	wsatry_again                = 11002
	wsano_recovery              = 11003
	wsano_data                  = 11004
	wsa_ipsec_name_policy_error = 11033
}

// wsa_error casts an int to its WsaError value.
pub fn wsa_error(code int) WsaError {
	return unsafe { WsaError(code) }
}
