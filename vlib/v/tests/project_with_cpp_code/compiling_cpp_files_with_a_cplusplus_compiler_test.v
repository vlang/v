module main

#flag @VMODROOT/implementation.o

fn C.sizeof_char() int

fn test_the_implementation_object_file_was_compiled_with_a_c_plus_plus_compiler() {
	res := C.sizeof_char()
	dump(res)
	if res == 4 {
		eprintln('implementation.o was compiled with a C compiler. Fail.')
	} else if res == 1 {
		println('implementation.o was compiled with a C++ compiler. Good.')
	}
	assert res == 1 // valid for C++ compilers
}
