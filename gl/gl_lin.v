module gl

#include "glad.h"
#include "glad.c"
fn init_glad() {
	println('init_glad() win')
	// # ok= gladLoadGLLoader((GLADloadproc) glfwGetProcAddress);
	ok := C.gladLoadGL()
	if !ok {
		os.exit('Failed to initialize glad OpenGL context')
	}
	// C.printf("initglad test: glClear ADDR=%p\n', glClear);
}

