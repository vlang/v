//
// Assembler program to print "Hello World!"
// to stdout.
//
// X0-X2 - parameters to Unix system calls
// X16 - Mach System Call function number
//

.global _start			// Provide program starting address to linker
.align 4			// Make sure everything is aligned properly

// Setup the parameters to print hello world
// and then call the Kernel to do it.
_start: mov	X0, #1		// 1 = StdOut
	adr	X0, helloworld 	// string to print
	//mov	X2, #13	    	// length of our string
	bl      _puts
	//mov	X16, #4		// Unix write system call
	//svc	#0x80		// Call kernel to output the string

// Setup the parameters to exit the program
// and then call the kernel to do it.
	mov     X0, #0		// Use 0 return code
	mov     X16, #1		// System call number 1 terminates this program
	svc     #0x80		// Call kernel to terminate the program

helloworld:      .ascii  "Hello World!\n"
