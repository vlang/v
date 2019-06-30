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
	SOCK_STREAM 
	AI_PASSIVE 
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

