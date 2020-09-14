module net

fn C.gethostname() int
// hostname returns the host name reported by the kernel.
pub fn hostname() ?string {
	mut name := [256]byte{}
	// https://www.ietf.org/rfc/rfc1035.txt
	// The host name is returned as a null-terminated string.
	namebp := byteptr(name)
	res := C.gethostname(namebp, 256)
	if res != 0 {
		return error('net.hostname: failed with $res')
	}
	return tos_clone(namebp)
}

