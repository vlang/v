module rand

	// if atomic.CompareAndSwapInt32(&r.used, 0, 1) {
	// 	// First use of randomness. Start timer to warn about
	// 	// being blocked on entropy not being available.
	// 	t := time.AfterFunc(60*time.Second, warnBlocked)
	// 	defer t.Stop()
	// }
	// r.mu.Lock()
	// if r.prov == 0 {
	// 	const provType = syscall.PROV_RSA_FULL
	// 	const flags = syscall.CRYPT_VERIFYCONTEXT | syscall.CRYPT_SILENT
	// 	err := syscall.CryptAcquireContext(&r.prov, nil, nil, provType, flags)
	// 	if err != nil {
	// 		r.mu.Unlock()
	// 		return 0, os.NewSyscallError("CryptAcquireContext", err)
	// 	}
	// }
	// r.mu.Unlock()

	// if len(b) == 0 {
	// 	return 0, nil
	// }
	// err = syscall.CryptGenRandom(r.prov, uint32(len(b)), &b[0])
	// if err != nil {
	// 	return 0, os.NewSyscallError("CryptGenRandom", err)
	// }
	// return len(b), nil

	bcripd_genrand_handle := voidptr()

    bytes_needed = rand_pool_bytes_needed(pool, 1 /*entropy_factor*/);
    buffer = rand_pool_add_begin(pool, bytes_needed);
    if (buffer != NULL) {
        size_t bytes = 0;
        if (BCryptGenRandom(NULL, buffer, bytes_needed,
                            BCRYPT_USE_SYSTEM_PREFERRED_RNG) == STATUS_SUCCESS)
            bytes = bytes_needed;

        rand_pool_add_end(pool, bytes, 8 * bytes);
        entropy_available = rand_pool_entropy_available(pool);
    }
    if (entropy_available > 0)
        return entropy_availabl