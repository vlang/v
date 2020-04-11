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

pub const (
	MSG_NOSIGNAL = 0
)
