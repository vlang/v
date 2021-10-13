#if defined(SOKOL_GLCORE33) || defined(SOKOL_GLES2) || defined(SOKOL_GLES3)
	#if defined(_WIN32)
		#include <GL/gl.h>
	#elif defined(__APPLE__)
		#ifndef GL_SILENCE_DEPRECATION
			#define GL_SILENCE_DEPRECATION
		#endif
		#if defined(TARGET_OS_IPHONE) && !TARGET_OS_IPHONE
			#include <OpenGL/gl3.h>
		#else
			#include <OpenGLES/ES3/gl.h>
			#include <OpenGLES/ES3/glext.h>
		#endif
		#include <GL/gl.h>
	#elif defined(__EMSCRIPTEN__) || defined(__ANDROID__)
		#if defined(SOKOL_GLES3)
			#include <GLES3/gl3.h>
		#elif defined(SOKOL_GLES2)
			#include <GLES2/gl2.h>
			#include <GLES2/gl2ext.h>
		#endif
	#elif defined(__linux__) || defined(__unix__)
		#include <GL/gl.h>
	#endif
	void v_sapp_gl_read_rgba_pixels(int x, int y, int width, int height, unsigned char* pixels) {
		glReadPixels(x, y, width, height, GL_RGBA, GL_UNSIGNED_BYTE, pixels);
	}
#else
	void v_sapp_gl_read_rgba_pixels(int x, int y, int width, int height, unsigned char* pixels) {
		// TODO
	}
#endif
