module net

import time
import encoding

enum RawProto {
    IPPROTO_IP = 0          
    IPPROTO_HOPOPTS = 0   
    IPPROTO_ICMP = 1           
    IPPROTO_IGMP = 2           
    IPPROTO_IPIP = 4           
    IPPROTO_TCP = 6           
    IPPROTO_EGP = 8
    IPPROTO_PUP = 12           
    IPPROTO_UDP = 17          
    IPPROTO_IDP = 22         
    IPPROTO_TP = 29          
    IPPROTO_DCCP = 33           
    IPPROTO_IPV6 = 41    
    IPPROTO_ROUTING = 43
    IPPROTO_FRAGMENT = 44 
    IPPROTO_RSVP = 46         
    IPPROTO_GRE = 47          
    IPPROTO_ESP = 50 
    IPPROTO_AH = 51       
    IPPROTO_ICMPV6 = 58  
    IPPROTO_NONE = 59  
    IPPROTO_DSTOPTS = 60 
    IPPROTO_MTP = 92           
    IPPROTO_ENCAP = 98          
    IPPROTO_PIM = 103          
    IPPROTO_COMP = 108           
    IPPROTO_SCTP = 132         
    IPPROTO_UDPLITE = 136 
    IPPROTO_RAW = 255           
}

const (
	raw_default_read_timeout = 30 * time.second
	raw_default_write_timeout = 30 * time.second
)

struct RawSocket{
	handle int
}

pub struct RawConn {
pub mut:
	sock RawSocket
mut:
	write_deadline time.Time
	read_deadline  time.Time
	read_timeout   time.Duration
	write_timeout  time.Duration
}

pub fn dial_raw(address string, proto int) ?&RawConn {
	mut s := new_raw_socket(proto) ?
	s.connect(address) ?
	return &RawConn{
		sock: s
		read_timeout: net.raw_default_read_timeout
		write_timeout: net.raw_default_write_timeout
	}
}


pub fn (mut c RawConn) write_to_ptr(addr Addr, b byteptr, len int) ?int{
	res := C.sendto(c.sock.handle, b, len, 0, &addr.addr, addr.len)
	if res >= 0 {
		return res
	}
	code := error_code()
	if code == int(error_ewouldblock) {
		c.wait_for_write() ?
		socket_error(C.sendto(c.sock.handle, b, len 0, &addr.addr, addr.len)) ?
	} else {
		wrap_error(code) ?
	}
	return none
} 

pub fn (mut c RawConn) write_ptr(b byteptr, len int) ?int {
	remote := c.sock.remote() or { return err_no_remote }
	return c.write_to_ptr(remote, b, len)
}

pub fn (mut c RawConn) write(buf []byte) ?int {
	return c.write_ptr(buf.data, buf.len)
}

pub fn (mut c RawConn) write_str(s string) ?int {
	return c.write_ptr(s.str, s.len)
}

pub fn (mut c RawConn) close() ? {
	c.sock.close() ?
	return none
}

fn (mut s RawSocket) connect (a string) ? {
	addr := resolve_addr(a, .inet, .raw) ?
	res := C.connect(s.handle, &addr.addr, addr.len)
	if res == 0 {
		return none
	}
	_ := error_code()
	write_result := s.@select(.write, net.connect_timeout) ?
	if write_result {
		return none
	}
	except_result := s.@select(.except, net.connect_timeout) ?
	if except_result {
		return err_connect_failed
	}
	return err_connect_failed
}

fn (mut s RawSocket) @select(test Select, timeout timeDuration) ?bool {
	return @select(s.handle, test, timeout)
}


fn new_raw_socket(proto int) ?RawSocket {
	sockfd := socket_error(C.socket(SocketFamily.inet, SocketType.raw, proto))
	mut s := &RawSocket {
		handle: sockfd
	}
	s.set_option_int(.reuse_addr, 1) 
	return s
}

pub fn (mut s RawConn) set_option_int(opt Socketoption, value int) ? {
	socket_error(C.setsockopt(s.handle, IPPROTO_IP, C.IPHDRINCL, int(opt), &value, sizeof(int))) ?
	return none
}

pub fn (mut c RawConn) read_deadline() ?time.Time {
	if c.read_deadline.unix == 0 {
		return c.read_deadline
	}
	return none
}

pub fn (mut c RawConn) set_read_deadline(deadline time.Time) {
	c.read_deadline = deadline
}

pub fn (mut c RawConn) write_deadline() ?time.Time {
	if c.write_deadline.unix == 0 {
		return c.write_deadline
	}
	return none
}

pub fn (mut c RawConn) set_write_deadline(deadline time.Time) {
	c.write_deadline = deadline
}

pub fn (c &RawConn) read_timeout() time.Duration {
	return c.read_timeout
}

pub fn (mut c RawConn) set_read_timeout(t time.Duration) {
	c.read_timeout = t
}

pub fn (c &RawConn) write_timeout() time.Duration {
	return c.write_timeout
}

pub fn (mut c RawConn) set_write_timeout(t time.Duration) {
	c.write_timeout = t
}

[inline]
pub fn (mut c RawConn) wait_for_read() ? {
	return wait_for_read(c.sock.handle, c.read_deadline, c.read_timeout)
}

[inline]
pub fn (mut c RawConn) wait_for_write() ? {
	return wait_for_write(c.sock.handle, c.write_deadline, c.write_timeout)
}


fn (mut s RawSocket) close() ? {
	return shutdown(s.handle)
}

pub fn (s &RawSocket) address() ?Addr {
	mut addr := C.sockaddr_in{}
	size := sizeof(C.sockaddr_in)
	sockaddr := unsafe { &C.sockaddr(&addr)}
	C.getsockname(s.handle, sockaddr, &size)
	return new_addr(sockaddr)
}

pub fn (c RawConn) str() string {
	s := c.sock.str().replace('\n', ' ').replace('  ', ' ')
	return 'RawConn{ write_deadline: $c.write_deadline, read_deadline: $c.read_deadline, read_timeout: $c.read_timeout, write_timeout: $c.write_timeout, sock: $s }'
}

fn raw_socket_from_handle(sockfd int) ?RawSocket {
	mut s := RawSocket {
		handle: sockfd
	}
	s.set_option_int(.reuse_addr, 1) ?
	return s
}

pub fn (mut c RawConn) write_to(addr Addr, buf []byte) ?int {
	return c.write_to_ptr(addr, buf.data, buf.len)
}

pub fn (mut c RawConn) write_to_string(addr Addr, s string) ?int {
	return c.write_to_ptr(addr, s.str, s.len)
}

fn resolve_wrapper(raddr string) ?Addr {
	x := resolve_addr(raddr, .inet, .raw) or { return none }
	return x
}

pub fn (mut c RawConn) read(mut buf []byte) ?(int, Addr) {
	mut addr_from := C.sockaddr{}
	len := sizeof(C.sockaddr)
	mut res := wrap_read_result(C.recvfrom(c.sock.handle, buf.data, buf.len, 0, &addr_from,
		&len)) ?
	if res > 0 {
		addr := new_addr(addr_from) ?
		return res, addr
	}
	code := error_code()
	if code == int(error_ewouldblock) {
		c.wait_for_read() ?
		res = wrap_read_result(C.recvfrom(c.sock.handle, buf.data, buf.len, 0, &addr_from,
			&len)) ?
		res2 := socket_error(res) ?
		addr := new_addr(addr_from) ?
		return res2, addr
	} else {
		wrap_error(code) ?
	}
	return none
}

pub fn listen_raw(proto int) ?&RawConn {
	s := new_udp_socket(proto) ?
	return &UdpConn{
		sock: s
		read_timeout: net.raw_default_read_timeout
		write_timeout: net.raw_default_write_timeout
	}
}
