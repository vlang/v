.text
.globl v_sha256_block

// void v_sha256_block(uint32_t state[8], const uint8_t block[64])
// The argument registers are selected for the System V or Win64 amd64 ABI.
#ifdef _WIN64
#define STATE_PTR %rcx
#define BLOCK_PTR %rdx
#else
#define STATE_PTR %rdi
#define BLOCK_PTR %rsi
#endif

v_sha256_block:
	pushq %rbx
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15
	subq $272, %rsp
	movq STATE_PTR, 256(%rsp)

	.macro LOAD_WORD offset
		movl \offset(BLOCK_PTR), %eax
		bswapl %eax
		movl %eax, \offset(%rsp)
	.endm

	LOAD_WORD 0
	LOAD_WORD 4
	LOAD_WORD 8
	LOAD_WORD 12
	LOAD_WORD 16
	LOAD_WORD 20
	LOAD_WORD 24
	LOAD_WORD 28
	LOAD_WORD 32
	LOAD_WORD 36
	LOAD_WORD 40
	LOAD_WORD 44
	LOAD_WORD 48
	LOAD_WORD 52
	LOAD_WORD 56
	LOAD_WORD 60

	// Expand W[16..63] in the local stack frame.
	movl $64, %edi
.Lschedule:
	movl -8(%rsp,%rdi), %eax
	movl %eax, %ecx
	movl %eax, %edx
	rorl $17, %eax
	rorl $19, %ecx
	shrl $10, %edx
	xorl %ecx, %eax
	xorl %edx, %eax

	movl -60(%rsp,%rdi), %r8d
	movl %r8d, %r9d
	movl %r8d, %r10d
	rorl $7, %r8d
	rorl $18, %r9d
	shrl $3, %r10d
	xorl %r9d, %r8d
	xorl %r10d, %r8d

	addl -28(%rsp,%rdi), %eax
	addl %r8d, %eax
	addl -64(%rsp,%rdi), %eax
	movl %eax, (%rsp,%rdi)
	addl $4, %edi
	cmpl $256, %edi
	jb .Lschedule

	movq 256(%rsp), %rdi
	movl 0(%rdi), %eax
	movl 4(%rdi), %ebx
	movl 8(%rdi), %ecx
	movl 12(%rdi), %edx
	movl 16(%rdi), %r8d
	movl 20(%rdi), %r9d
	movl 24(%rdi), %r10d
	movl 28(%rdi), %r11d

	.macro ROUND offset, constant
		// T1 = h + Sigma1(e) + Ch(e, f, g) + K + W[i].
		movl %r8d, %r12d
		rorl $6, %r12d
		movl %r8d, %r14d
		rorl $11, %r14d
		xorl %r14d, %r12d
		movl %r8d, %r14d
		rorl $25, %r14d
		xorl %r14d, %r12d

		movl %r8d, %r13d
		andl %r9d, %r13d
		movl %r8d, %r14d
		notl %r14d
		andl %r10d, %r14d
		xorl %r14d, %r13d
		addl %r11d, %r12d
		addl %r13d, %r12d
		addl $\constant, %r12d
		addl \offset(%rsp), %r12d

		// T2 = Sigma0(a) + Maj(a, b, c).
		movl %eax, %r13d
		rorl $2, %r13d
		movl %eax, %r14d
		rorl $13, %r14d
		xorl %r14d, %r13d
		movl %eax, %r14d
		rorl $22, %r14d
		xorl %r14d, %r13d
		movl %eax, %r14d
		andl %ebx, %r14d
		movl %eax, %r15d
		andl %ecx, %r15d
		xorl %r15d, %r14d
		movl %ebx, %r15d
		andl %ecx, %r15d
		xorl %r15d, %r14d
		addl %r14d, %r13d

		movl %edx, %r14d
		addl %r12d, %r14d
		addl %r13d, %r12d
		movl %r10d, %r11d
		movl %r9d, %r10d
		movl %r8d, %r9d
		movl %r14d, %r8d
		movl %ecx, %edx
		movl %ebx, %ecx
		movl %eax, %ebx
		movl %r12d, %eax
	.endm

	ROUND 0, 0x428a2f98
	ROUND 4, 0x71374491
	ROUND 8, 0xb5c0fbcf
	ROUND 12, 0xe9b5dba5
	ROUND 16, 0x3956c25b
	ROUND 20, 0x59f111f1
	ROUND 24, 0x923f82a4
	ROUND 28, 0xab1c5ed5
	ROUND 32, 0xd807aa98
	ROUND 36, 0x12835b01
	ROUND 40, 0x243185be
	ROUND 44, 0x550c7dc3
	ROUND 48, 0x72be5d74
	ROUND 52, 0x80deb1fe
	ROUND 56, 0x9bdc06a7
	ROUND 60, 0xc19bf174
	ROUND 64, 0xe49b69c1
	ROUND 68, 0xefbe4786
	ROUND 72, 0x0fc19dc6
	ROUND 76, 0x240ca1cc
	ROUND 80, 0x2de92c6f
	ROUND 84, 0x4a7484aa
	ROUND 88, 0x5cb0a9dc
	ROUND 92, 0x76f988da
	ROUND 96, 0x983e5152
	ROUND 100, 0xa831c66d
	ROUND 104, 0xb00327c8
	ROUND 108, 0xbf597fc7
	ROUND 112, 0xc6e00bf3
	ROUND 116, 0xd5a79147
	ROUND 120, 0x06ca6351
	ROUND 124, 0x14292967
	ROUND 128, 0x27b70a85
	ROUND 132, 0x2e1b2138
	ROUND 136, 0x4d2c6dfc
	ROUND 140, 0x53380d13
	ROUND 144, 0x650a7354
	ROUND 148, 0x766a0abb
	ROUND 152, 0x81c2c92e
	ROUND 156, 0x92722c85
	ROUND 160, 0xa2bfe8a1
	ROUND 164, 0xa81a664b
	ROUND 168, 0xc24b8b70
	ROUND 172, 0xc76c51a3
	ROUND 176, 0xd192e819
	ROUND 180, 0xd6990624
	ROUND 184, 0xf40e3585
	ROUND 188, 0x106aa070
	ROUND 192, 0x19a4c116
	ROUND 196, 0x1e376c08
	ROUND 200, 0x2748774c
	ROUND 204, 0x34b0bcb5
	ROUND 208, 0x391c0cb3
	ROUND 212, 0x4ed8aa4a
	ROUND 216, 0x5b9cca4f
	ROUND 220, 0x682e6ff3
	ROUND 224, 0x748f82ee
	ROUND 228, 0x78a5636f
	ROUND 232, 0x84c87814
	ROUND 236, 0x8cc70208
	ROUND 240, 0x90befffa
	ROUND 244, 0xa4506ceb
	ROUND 248, 0xbef9a3f7
	ROUND 252, 0xc67178f2

	addl 0(%rdi), %eax
	addl 4(%rdi), %ebx
	addl 8(%rdi), %ecx
	addl 12(%rdi), %edx
	addl 16(%rdi), %r8d
	addl 20(%rdi), %r9d
	addl 24(%rdi), %r10d
	addl 28(%rdi), %r11d
	movl %eax, 0(%rdi)
	movl %ebx, 4(%rdi)
	movl %ecx, 8(%rdi)
	movl %edx, 12(%rdi)
	movl %r8d, 16(%rdi)
	movl %r9d, 20(%rdi)
	movl %r10d, 24(%rdi)
	movl %r11d, 28(%rdi)

	addq $272, %rsp
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbx
	ret
