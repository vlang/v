module net

#flag -lws2_32
#include <winsock2.h>
#include <Ws2tcpip.h>

struct C.WSAData {
mut:
	wVersion u16
	wHighVersion u16	
	szDescription [257]byte
	szSystemStatus [129]byte
	iMaxSockets u16
	iMaxUdpDg u16
	lpVendorInfo byteptr
}


const (
	WSA_V22 = 0x202 // C.MAKEWORD(2, 2)
	ERROR_CODES = {

		//socket creation errors
        "not_authorized": C.WSAEACCES,
        "not_supported": C.WSAEAFNOSUPPORT,
        "invalid": C.WSAEINVAL,//exact signification depending on the context
        "m_file": C.WSAEMFILE,
        "no_buffer": C.WSAENOBUFS,
        "no_memory": C.WSA_NOT_ENOUGH_MEMORY,
        "protocol_not_supported": C.WSAEPROTONOSUPPORT,
        
        //connection errors
        "connection_refused": C.WSAECONNREFUSED,
        "memory_fault": C.WSAEFAULT,
        "again": C.WSATRY_AGAIN,
        "would_block":C.WSAEWOULDBLOCK,
        "invalid_descriptor": C.WSAEBADF,
        "interrupted": C.WSAEINTR,
        "address_unavailable": C.WSAEADDRNOTAVAIL,
        "address_in_use": C.WSAEADDRINUSE,
        "op_not_supported": C.WSAEOPNOTSUPP,
        "not_connected": C.WSAENOTCONN,
        "connection_reset": C.WSAECONNRESET,
        "required_dest_addr": C.WSAEDESTADDRREQ,
        "msg_size": C.WSAEMSGSIZE,
        "host_down": C.WSAEHOSTDOWN,
        "host_unreachable": C.WSAEHOSTUNREACH,

		//windows specific:
		"in_progress": C.WSAEINPROGRESS,
		"loop": C.WSAELOOP,
		"remote": C.WSAEREMOTE,
		"system_not_ready": C.WSASYSNOTREADY

	}
)

fn init() {
	mut wsadata := C.WSAData{}
	res := C.WSAStartup(WSA_V22, &wsadata)
	if res != 0 {
		panic('socket: WSAStartup failed')
	}
}

fn error_code() int {
	return C.WSAGetLastError()
}

