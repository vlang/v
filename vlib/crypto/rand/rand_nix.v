module rand

const (
	// Getrand_Max_Bytes = 33554431
	// O_RDONLY = 0x0000
	// RNDGETENTCNT = 0x01
)

	// mut entropy := 0
	// fd_random := C.open('/dev/urandom', O_RDONLY)
  	// if !C.ioctl(fd_random, RNDGETENTCNT, &entropy) {
	// 	// not random device
	// 	panic('not random device')
	// }
	// if entropy < bytes_needed * 8 {
	// 	// not enough entropy
	// 	panic('not enough entropy')
	// }
	// amount := C.read(fd_random, buffer, bytes_needed) //Grab as much data as we possibly can
	// C.close(fd_random)


// // Alternate pseudo-random implementation for use on
// // systems without a reliable /dev/urandom.

// // newReader returns a new pseudorandom generator that
// // seeds itself by reading from entropy. If entropy == nil,
// // the generator seeds itself by reading from the system's
// // random number generator, typically /dev/random.
// // The Read method on the returned reader always returns
// // the full amount asked for, or else it returns an error.
// //
// // The generator uses the X9.31 algorithm with AES-128,
// // reseeding after every 1 MB of generated data.

// pub fn read(bytes_needed int) ?[]byte {

// }

// func (r *reader) Read(b []byte) (n int, err error) {
// 	r.mu.Lock()
// 	defer r.mu.Unlock()
// 	n = len(b)

// 	for len(b) > 0 {
// 		if r.budget == 0 {
// 			_, err := io.ReadFull(r.entropy, r.seed[0:])
// 			if err != nil {
// 				return n - len(b), err
// 			}
// 			_, err = io.ReadFull(r.entropy, r.key[0:])
// 			if err != nil {
// 				return n - len(b), err
// 			}
// 			r.cipher, err = aes.NewCipher(r.key[0:])
// 			if err != nil {
// 				return n - len(b), err
// 			}
// 			r.budget = 1 << 20 // reseed after generating 1MB
// 		}
// 		r.budget -= aes.BlockSize

// 		// ANSI X9.31 (== X9.17) algorithm, but using AES in place of 3DES.
// 		//
// 		// single block:
// 		// t = encrypt(time)
// 		// dst = encrypt(t^seed)
// 		// seed = encrypt(t^dst)
// 		ns := time.Now().UnixNano()
// 		binary.BigEndian.PutUint64(r.time[:], uint64(ns))
// 		r.cipher.Encrypt(r.time[0:], r.time[0:])
// 		for i := 0; i < aes.BlockSize; i++ {
// 			r.dst[i] = r.time[i] ^ r.seed[i]
// 		}
// 		r.cipher.Encrypt(r.dst[0:], r.dst[0:])
// 		for i := 0; i < aes.BlockSize; i++ {
// 			r.seed[i] = r.time[i] ^ r.dst[i]
// 		}
// 		r.cipher.Encrypt(r.seed[0:], r.seed[0:])

// 		m := copy(b, r.dst[0:])
// 		b = b[m:]
// 	}

// 	return n, nil
// }
