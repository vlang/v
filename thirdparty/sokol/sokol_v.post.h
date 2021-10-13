#if defined(SOKOL_D3D11)
	void v_sapp_screenshot(const char * filename) {}
#elif defined(SOKOL_METAL)
	void v_sapp_screenshot(const char * filename) {}
#elif defined(SOKOL_WGPU)
	void v_sapp_screenshot(const char * filename) {}
#elif defined(SOKOL_GLCORE33) || defined(SOKOL_GLES2) || defined(SOKOL_GLES3)
	void v_sapp_screenshot(const char * filename) {
		int windowWidth = sapp_width();
		int windowHeight = sapp_height();
		const int numberOfPixels = windowWidth * windowHeight * 3;
		unsigned char pixels[numberOfPixels];

		glPixelStorei(GL_PACK_ALIGNMENT, 1);
		glReadBuffer(GL_FRONT);
		glReadPixels(0, 0, windowWidth, windowHeight, GL_BGR_EXT, GL_UNSIGNED_BYTE, pixels);

		FILE *outputFile = fopen(filename, "w");
		short header[] = {0, 2, 0, 0, 0, 0, (short) windowWidth, (short) windowHeight, 24};

		fwrite(&header, sizeof(header), 1, outputFile);
		fwrite(pixels, numberOfPixels, 1, outputFile);
		fclose(outputFile);
	}
#endif
