// vtest build: !msvc

fn test_empty_inline_block() {
	// must compile
	asm amd64 {
		; ; ; memory
	}

	asm volatile amd64 {
		; ; ; memory
	}

	asm amd64 {
	}

	asm volatile amd64 {
	}

	asm arm64 {
		; ; ; memory
	}

	asm volatile arm64 {
		; ; ; memory
	}

	asm arm64 {
	}

	asm volatile arm64 {
	}

	asm i386 {
		; ; ; memory
	}

	asm volatile i386 {
		; ; ; memory
	}

	asm i386 {
	}

	asm volatile i386 {
	}

	asm rv64 {
		; ; ; memory
	}

	asm volatile rv64 {
		; ; ; memory
	}

	asm rv64 {
	}

	asm volatile rv64 {
	}

	asm loongarch64 {
		; ; ; memory
	}

	asm volatile loongarch64 {
		; ; ; memory
	}

	asm loongarch64 {
	}

	asm volatile loongarch64 {
	}

	asm ppc64le {
		; ; ; memory
	}

	asm volatile ppc64le {
		; ; ; memory
	}

	asm ppc64le {
	}

	asm volatile ppc64le {
	}

	asm s390x {
		; ; ; memory
	}

	asm volatile s390x {
		; ; ; memory
	}

	asm s390x {
	}

	asm volatile s390x {
	}

	assert true
}
