module rand

const (
	// Getrand_Max_Bytes = 33554431
	O_RDONLY = 0x0000
	RNDGETENTCNT = 0x01
)

// import const (
// 	RNDGETENTCNT
// 	O_RDONLY
// )

pub fn read(bytes_needed int) []byte {	

	mut buffer := malloc(bytes_needed)
	mut entropy := 0
	fd_random := C.open('/dev/urandom', O_RDONLY)
  	// if !C.ioctl(fd_random, RNDGETENTCNT, &entropy) {
	// 	// not random device
	// 	panic('not random device')
	// }
	// if entropy < bytes_needed * 8 {
	// 	// not enough entropy
	// 	panic('not enough entropy')
	// }
	amount := C.read(fd_random, buffer, bytes_needed) //Grab as much data as we possibly can
	C.close(fd_random)

	return new_array_from_c_array_no_alloc_rand(bytes_needed, 1, 1, buffer)
}
