module gl

#include <Cocoa/Cocoa.h>
#include <CoreGraphics/CoreGraphics.h>
// #include <OpenGL/gl.h>
// #flag -framework OpenGL
#include  "/Users/alex/code/lang/gl/glad.h"
#include "/Users/alex/code/lang/gl/glad.c"
fn init_glad() {
	println('init_glad() win')
	ok := C.gladLoadGL()
	// # ok= gladLoadGLLoader((GLADloadproc) glfwGetProcAddress);
	if !ok {
		os.exit('Failed to initialize glad OpenGL context')
	}
	// # printf("glClear ADDR=%p\n", glClear);
}

