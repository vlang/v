module rand

pub fn read(bytes_needed int) []byte {	
	mut buffer := malloc(bytes_needed)
	if $windows {
        // use BCRYPT_USE_SYSTEM_PREFERRED_RNG as we passed null as algo
        status := C.BCryptGenRandom(0, buffer, bytes_needed, BCRYPT_USE_SYSTEM_PREFERRED_RNG)

        if !C.NT_SUCCESS(status) {
            panic('crypt.random.read: error')
        }
    }
    else {
        
    }

	// NOTE: temp until we have []bytes(buff)
	return new_array_from_c_array_no_alloc_rand(bytes_needed, 1, 1, buffer)
}

// NOTE: temp until we have []bytes(buff)
fn new_array_from_c_array_no_alloc_rand(len, cap, elm_size int, c_array voidptr) array {
	arr := array {
		len: len
		cap: cap
		element_size: elm_size
		data: c_array
	}
	return arr
}