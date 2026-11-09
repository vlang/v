// Independent oracle: rasterize a detached FT_Glyph through FT_Glyph_To_Bitmap.
// The backend instead transforms/renders a face slot and caches its A8 masks.
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H
#include <math.h>
#include <stdint.h>

static int v_canvas_test_ft_glyph_index(const uint8_t* data, uint64_t length, uint32_t codepoint) {
	FT_Library library = NULL;
	FT_Face face = NULL;
	if (FT_Init_FreeType(&library)) return -1;
	if (FT_New_Memory_Face(library, data, (FT_Long)length, 0, &face)) {
		FT_Done_FreeType(library);
		return -1;
	}
	int index = (int)FT_Get_Char_Index(face, codepoint);
	FT_Done_Face(face);
	FT_Done_FreeType(library);
	return index;
}

static int v_canvas_test_ft_glyph_count(const uint8_t* data, uint64_t length) {
	FT_Library library = NULL;
	FT_Face face = NULL;
	if (FT_Init_FreeType(&library)) return -1;
	if (FT_New_Memory_Face(library, data, (FT_Long)length, 0, &face)) {
		FT_Done_FreeType(library);
		return -1;
	}
	int count = (int)face->num_glyphs;
	FT_Done_Face(face);
	FT_Done_FreeType(library);
	return count;
}

static uint32_t v_canvas_test_over(uint32_t destination, uint32_t source, unsigned int mask) {
	unsigned int alpha = source >> 24;
	unsigned int covered_alpha = (alpha * mask + 127) / 255;
	uint32_t result = 0;
	for (int shift = 0; shift <= 24; shift += 8) {
		unsigned int dst = (destination >> shift) & 255;
		unsigned int src = (source >> shift) & 255;
		unsigned int out;
		if (alpha == 255) out = (dst * (255 - mask) + src * mask + 127) / 255;
		else {
			unsigned int premul = shift == 24 ? alpha : (src * alpha + 127) / 255;
			out = (premul * mask + 127) / 255 + (dst * (255 - covered_alpha) + 127) / 255;
		}
		if (out > 255) out = 255;
		result |= out << shift;
	}
	return result;
}

static int v_canvas_test_ft_render(const uint8_t* data, uint64_t length, uint16_t id,
	float size, const float matrix[4], float x, float y, int monochrome,
	int width, int height, uint32_t background, uint32_t color, uint32_t* pixels) {
	FT_Library library = NULL;
	FT_Face face = NULL;
	FT_Glyph glyph = NULL;
	int result = 0;
	if ((result = FT_Init_FreeType(&library))) goto done;
	if ((result = FT_New_Memory_Face(library, data, (FT_Long)length, 0, &face))) goto done;
	if ((result = FT_Set_Char_Size(face, 0, (FT_F26Dot6)(size * 64), 72, 72))) goto done;
	FT_Int32 flags = FT_LOAD_NO_HINTING | FT_LOAD_NO_BITMAP | FT_LOAD_NO_AUTOHINT;
	if (monochrome) flags |= FT_LOAD_TARGET_MONO | FT_LOAD_MONOCHROME;
	if ((result = FT_Load_Glyph(face, id, flags))) goto done;
	if ((result = FT_Get_Glyph(face->glyph, &glyph))) goto done;
	FT_Matrix transform = {
		(FT_Fixed)(matrix[0] * 65536), (FT_Fixed)(-matrix[1] * 65536),
		(FT_Fixed)(-matrix[2] * 65536), (FT_Fixed)(matrix[3] * 65536)
	};
	if ((result = FT_Glyph_Transform(glyph, &transform, NULL))) goto done;
	// Quantize the reference position to quarter pixels without calling the
	// canvas positioning or packed-glyph helpers.
	double quarter_x = floor((double)x * 4 + 0.5) / 4;
	double quarter_y = floor((double)y * 4 + 0.5) / 4;
	int anchor_x = (int)floor(quarter_x);
	int anchor_y = (int)floor(quarter_y);
	FT_Vector delta = {
		(FT_Pos)((quarter_x - anchor_x) * 64),
		-(FT_Pos)((quarter_y - anchor_y) * 64)
	};
	if ((result = FT_Glyph_To_Bitmap(&glyph,
		monochrome ? FT_RENDER_MODE_MONO : FT_RENDER_MODE_NORMAL, &delta, 1))) goto done;
	FT_BitmapGlyph image = (FT_BitmapGlyph)glyph;
	FT_Bitmap* bitmap = &image->bitmap;
	for (int i = 0; i < width * height; ++i) pixels[i] = background;
	for (unsigned int row = 0; row < bitmap->rows; ++row) {
		int dy = anchor_y - image->top + (int)row;
		if (dy < 0 || dy >= height) continue;
		const unsigned char* input = bitmap->buffer + row * bitmap->pitch;
		for (unsigned int column = 0; column < bitmap->width; ++column) {
			int dx = anchor_x + image->left + (int)column;
			if (dx < 0 || dx >= width) continue;
			unsigned int coverage = monochrome
				? ((input[column / 8] >> (7 - column % 8)) & 1) * 255 : input[column];
			pixels[dy * width + dx] = v_canvas_test_over(pixels[dy * width + dx], color, coverage);
		}
	}
done:
	if (glyph) FT_Done_Glyph(glyph);
	if (face) FT_Done_Face(face);
	if (library) FT_Done_FreeType(library);
	return result;
}
