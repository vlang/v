module unix

import net

const use_net = net.no_timeout

// 104 for macos, 108 for linux => use the minimum
const max_sun_path = 104

// Select represents a select operation
enum Select {
	read
	write
	except
}

// SocketType are the available sockets
// enum SocketType {
// 	dgram = C.SOCK_DGRAM
// 	stream = C.SOCK_STREAM
// 	seqpacket = C.SOCK_SEQPACKET
// }
