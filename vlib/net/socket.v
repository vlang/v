module net

#include <netdb.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <errno.h>

struct Conn {
pub: 
	sockfd int
}

struct Listener {
	listener   int
	their_addr voidptr
}

import const ( 
	AF_UNSPEC 
	AF_INET 
	SOCK_STREAM 
	AI_PASSIVE 
	PF_INET 
	SOL_SOCKET 
	SO_REUSEADDR 
	INADDR_ANY 
) 

struct C.addrinfo {
mut: 
	ai_family int 
	ai_socktype int 
	ai_flags int 
	ai_protocol int 
	ai_addrlen int 
	ai_next voidptr 
	ai_addr voidptr 
} 

struct C.in_addr {
mut: 
	s_addr int 
 
} 

struct C.sockaddr_in {
mut: 
	sin_family int 
	sin_port int 
	sin_addr C.in_addr 
 
} 

struct C.sockaddr_storage {
 
} 

fn dial(addr string, port int) Conn {
	println('net.dial("$addr":$port)')
	mut hints := C.addrinfo{} 
	servinfo := &C.addrinfo{!} 
	mut rp := &C.addrinfo{!} 
	mut sockfd := -1
	// allow IPv4 or IPv6
	hints.ai_family = AF_UNSPEC 
	// avoid name lookup for port
	// hints.ai_flags = AI_NUMERICSERV 
	hints.ai_socktype = SOCK_STREAM 
	hints.ai_flags = AI_PASSIVE 
	strport := '$port'
	connbad := Conn{}
	c_addr := addr.cstr()
	rv := C.getaddrinfo(c_addr, strport.cstr(), &hints, &servinfo)
	if rv != 0 { 
		println('Getaddrinfo failed ')
		return connbad
	}
	// Loop through all the results and connect to the first we can
	for rp = servinfo; !isnil(rp); rp = rp.ai_next  { 
		sockfd = C.socket(rp.ai_family, rp.ai_socktype, rp.ai_protocol) 
		if sockfd == -1 {
			continue 
		} 
		if C.connect(sockfd, rp.ai_addr, rp.ai_addrlen) {
			C.close(sockfd) 
			continue 
		} 
		break 
	}
	if sockfd == -1 {
		println('socket: Cannot connect to host')
		return connbad
	}
	C.freeaddrinfo(servinfo) 
	conn := Conn {
		sockfd: sockfd,
	}
	return conn
}

fn listen(addr string, port int) Listener {
	mut hints := C.addrinfo{} 
	res := &C.addrinfo{!} 
        new_fd := 0
        BACKLOG := 10
        strport := port.str() 
        option := 1
        listener := C.socket(PF_INET, SOCK_STREAM, 0) 
        C.setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, &option, sizeof(int)) 
        bad_listener := Listener{their_addr: 0}
        if listener < 0 {
                println('couldnt create listen scoket')
                return bad_listener
        }
        // socket address used for the server
	mut server_address := C.sockaddr_in{} 
	server_address.sin_family = AF_INET 
        // htons: host to network short: transforms a value in host byte
        // ordering format to a short value in network byte ordering format
	server_address.sin_port = C.htons(port) 
        // htonl: host to network long: same as htons but to long
        server_address.sin_addr.s_addr = C.htonl(INADDR_ANY) 
	// size := sizeof(C.sockaddr_in) // 16 
        if C.bind(listener, &server_address, 16) < 0 { 
                println('cant bind')
                return bad_listener
        }
        if C.listen(listener, BACKLOG) < 0 { 
                println('cant listen')
                return bad_listener
        }
        l := Listener {
                listener: listener,
                their_addr: 0
        }
        return l
}

// accept an incoming connection
fn (l Listener) accept() Conn {
	their_addr := C.sockaddr_storage{} 
	addr_size := 128  // sizeof (l.their_addr) 
	return Conn {
		sockfd: C.accept(l.listener, &their_addr, &addr_size) 
	} 
}

 

