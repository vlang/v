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
