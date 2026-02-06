// vtest build: !(windows && tinyc|| msvc) // TODO
module main

#flag @VMODROOT/implementation.o
#include "@VMODROOT/implementation.h"

fn C.sizeof_char() int

fn test_the_implementation_object_file_was_compiled_with_a_c_plus_plus_compiler() {
	res := C.sizeof_char()
	dump(res)
	if res == sizeof(int) {
		eprintln('implementation.o was compiled with a C compiler. Fail.')
	} else if res == sizeof(char) {
		println('implementation.o was compiled with a C++ compiler. Good.')
	} else {
		eprintln('¯\\_(ツ)_/¯ ... unknown C/C++ compiler')
	}
	assert res == sizeof(char)
}
