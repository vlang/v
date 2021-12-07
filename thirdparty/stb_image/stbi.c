#define STB_IMAGE_IMPLEMENTATION
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb_image.h"
#include "stb_image_write.h"

void set_png_compression_level(int level);
void write_force_png_filter(int level);
void write_tga_with_rle(int level);

void set_png_compression_level(int level) {
	stbi_write_png_compression_level = level;
}

void write_force_png_filter(int level){
	stbi_write_force_png_filter = level;
}

void write_tga_with_rle(int level) {
	stbi_write_tga_with_rle = level;
}