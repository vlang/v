#if defined(SOKOL_D3D11)
	void v_sapp_screenshot(const char * filename) {
		// TODO
	}
#elif defined(SOKOL_METAL)
	void v_sapp_screenshot(const char * filename) {
		// TODO
	}
#elif defined(SOKOL_WGPU)
	void v_sapp_screenshot(const char * filename) {
		// TODO
	}
#elif defined(SOKOL_GLCORE33) || defined(SOKOL_GLES2) || defined(SOKOL_GLES3)
	// include platform specific GL headers (or on Win32: use an embedded GL loader)
    #if !defined(SOKOL_EXTERNAL_GL_LOADER)
        #if defined(_WIN32)
            #if defined(SOKOL_GLCORE33) && !defined(SOKOL_EXTERNAL_GL_LOADER)
                #ifndef WIN32_LEAN_AND_MEAN
                #define WIN32_LEAN_AND_MEAN
                #endif
                #ifndef NOMINMAX
                #define NOMINMAX
                #endif
                #include <windows.h>
                #define _SOKOL_USE_WIN32_GL_LOADER (1)
                #pragma comment (lib, "kernel32")   // GetProcAddress()
            #endif
        #elif defined(__APPLE__)
            #include <TargetConditionals.h>
            #ifndef GL_SILENCE_DEPRECATION
                #define GL_SILENCE_DEPRECATION
            #endif
            #if defined(TARGET_OS_IPHONE) && !TARGET_OS_IPHONE
                #include <OpenGL/gl3.h>
            #else
                #include <OpenGLES/ES3/gl.h>
                #include <OpenGLES/ES3/glext.h>
            #endif
        #elif defined(__EMSCRIPTEN__) || defined(__ANDROID__)
            #if defined(SOKOL_GLES3)
                #include <GLES3/gl3.h>
            #elif defined(SOKOL_GLES2)
                #ifndef GL_EXT_PROTOTYPES
                #define GL_GLEXT_PROTOTYPES
                #endif
                #include <GLES2/gl2.h>
                #include <GLES2/gl2ext.h>
            #endif
        #elif defined(__linux__) || defined(__unix__)
            #define GL_GLEXT_PROTOTYPES
            #include <GL/gl.h>
        #endif
    #endif
	void v_sapp_screenshot(const char * filename) {
		int window_width = sapp_width();
		int window_height = sapp_height();
		const int num_pixels = window_width * window_height * 3;
		unsigned char pixels[num_pixels];

		glPixelStorei(GL_PACK_ALIGNMENT, 1);
		glReadBuffer(GL_FRONT);
		glReadPixels(0, 0, window_width, window_height, GL_BGR_EXT, GL_UNSIGNED_BYTE, pixels);

		FILE *outputFile = fopen(filename, "w");
		short header[] = {0, 2, 0, 0, 0, 0, (short) window_width, (short) window_height, 24};

		fwrite(&header, sizeof(header), 1, outputFile);
		fwrite(pixels, num_pixels, 1, outputFile);
		fclose(outputFile);
	}
#endif
