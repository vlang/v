// FreeType owns the face and its current glyph slot. The wrapper owns a copy of
// the font bytes for the entire face lifetime; no pointer into a V array escapes.
#ifndef V_CANVAS_TEXT_FREETYPE_LINUX_H
#define V_CANVAS_TEXT_FREETYPE_LINUX_H

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_OUTLINE_H
#include <limits.h>
#include <math.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
	FT_Library library;
	FT_Face face;
	unsigned char* data;
} v_canvas_ft_font;

static void v_canvas_ft_font_free(void* handle) {
	v_canvas_ft_font* font = (v_canvas_ft_font*)handle;
	if (!font) return;
	if (font->face) FT_Done_Face(font->face);
	if (font->library) FT_Done_FreeType(font->library);
	free(font->data);
	free(font);
}

static void* v_canvas_ft_font_from_data(const uint8_t* data, uint64_t length) {
	if (!data || !length || length > LONG_MAX || length > SIZE_MAX) return NULL;
	v_canvas_ft_font* font = (v_canvas_ft_font*)calloc(1, sizeof(*font));
	if (!font) return NULL;
	font->data = (unsigned char*)malloc((size_t)length);
	if (!font->data) {
		v_canvas_ft_font_free(font);
		return NULL;
	}
	memcpy(font->data, data, (size_t)length);
	if (FT_Init_FreeType(&font->library) ||
		FT_New_Memory_Face(font->library, font->data, (FT_Long)length, 0, &font->face)) {
		v_canvas_ft_font_free(font);
		return NULL;
	}
	return font;
}

static int v_canvas_ft_font_has_color(void* handle) {
	return FT_HAS_COLOR(((v_canvas_ft_font*)handle)->face) != 0;
}

static int v_canvas_ft_font_is_scalable(void* handle) {
	return FT_IS_SCALABLE(((v_canvas_ft_font*)handle)->face) != 0;
}

// transform uses the same y-up affine convention as the CoreText bridge.
// The caller copies the bitmap before another glyph is loaded into this face.
static int v_canvas_ft_load_glyph(void* handle, double size, const double transform[6],
	uint16_t glyph, float sub_x, float sub_y, int hinting, int monochrome,
	int embedded_bitmaps, int linear_metrics, int max_dimension, int bounds[4],
	float advance[2]) {
	v_canvas_ft_font* font = (v_canvas_ft_font*)handle;
	FT_Face face = font->face;
	if (!isfinite(size) || size <= 0 || size > 256 || glyph >= face->num_glyphs) return -1;
	for (int i = 0; i < 4; ++i) {
		if (!isfinite(transform[i]) || fabs(transform[i]) >= 32768) return -1;
	}
	// FT_Set_Char_Size clamps requests below one point. Preserve the requested
	// device size by applying the remaining scale to the outline transform.
	double load_size = size < 1 ? 1 : size;
	double residual_scale = size / load_size;
	FT_Error error = FT_Set_Char_Size(face, 0, (FT_F26Dot6)floor(load_size * 64 + 0.5), 72, 72);
	if (error) return error;
	FT_Matrix matrix = {
		(FT_Fixed)floor(transform[0] * residual_scale * 65536 + 0.5),
		(FT_Fixed)floor(transform[2] * residual_scale * 65536 + 0.5),
		(FT_Fixed)floor(transform[1] * residual_scale * 65536 + 0.5),
		(FT_Fixed)floor(transform[3] * residual_scale * 65536 + 0.5)
	};
	FT_Vector delta = {
		(FT_Pos)floor((double)sub_x * 64 + 0.5),
		-(FT_Pos)floor((double)sub_y * 64 + 0.5)
	};
	FT_Set_Transform(face, &matrix, &delta);
	FT_Int32 flags = FT_LOAD_DEFAULT;
	// FreeType transforms outlines, not embedded bitmaps. Prefer the outline
	// whenever a transform or a fractional translation must be applied.
	if (!embedded_bitmaps || matrix.xx != 65536 || matrix.yy != 65536 ||
		matrix.xy || matrix.yx || delta.x || delta.y) flags |= FT_LOAD_NO_BITMAP;
	if (hinting == 0) flags |= FT_LOAD_NO_HINTING | FT_LOAD_NO_AUTOHINT;
	if (monochrome) flags |= FT_LOAD_TARGET_MONO | FT_LOAD_MONOCHROME;
	else if (hinting == 1) flags |= FT_LOAD_TARGET_LIGHT;
	else flags |= FT_LOAD_TARGET_NORMAL;
	error = FT_Load_Glyph(face, glyph, flags);
	if (error) return error;
	FT_GlyphSlot slot = face->glyph;
	if (slot->format == FT_GLYPH_FORMAT_OUTLINE) {
		FT_BBox box;
		FT_Outline_Get_CBox(&slot->outline, &box);
		double width = ceil((double)box.xMax / 64) - floor((double)box.xMin / 64);
		double height = ceil((double)box.yMax / 64) - floor((double)box.yMin / 64);
		if (width > max_dimension || height > max_dimension) return -2;
	}
	error = FT_Render_Glyph(slot, monochrome ? FT_RENDER_MODE_MONO : FT_RENDER_MODE_NORMAL);
	if (error) return error;
	FT_Bitmap* bitmap = &slot->bitmap;
	if (bitmap->width > (unsigned int)max_dimension || bitmap->rows > (unsigned int)max_dimension ||
		slot->bitmap_left < -32768 || slot->bitmap_left > 32767 ||
		slot->bitmap_top < -32767 || slot->bitmap_top > 32768) return -2;
	if (bitmap->pixel_mode != FT_PIXEL_MODE_GRAY && bitmap->pixel_mode != FT_PIXEL_MODE_MONO &&
		bitmap->pixel_mode != FT_PIXEL_MODE_GRAY2 && bitmap->pixel_mode != FT_PIXEL_MODE_GRAY4 &&
		bitmap->width && bitmap->rows) return -3;
	bounds[0] = slot->bitmap_left;
	bounds[1] = -slot->bitmap_top;
	bounds[2] = (int)bitmap->width;
	bounds[3] = (int)bitmap->rows;
	if (linear_metrics) {
		double x = (double)slot->linearHoriAdvance / 65536;
		advance[0] = (float)(x * transform[0] * residual_scale);
		advance[1] = (float)(-x * transform[1] * residual_scale);
	} else {
		advance[0] = (float)slot->advance.x / 64;
		advance[1] = -(float)slot->advance.y / 64;
	}
	return 0;
}

static void v_canvas_ft_copy_bitmap(void* handle, uint8_t* output) {
	FT_Bitmap* bitmap = &((v_canvas_ft_font*)handle)->face->glyph->bitmap;
	if (!bitmap->width || !bitmap->rows) return;
	const unsigned char* top = bitmap->buffer;
	if (bitmap->pitch < 0) top += (size_t)(bitmap->rows - 1) * (size_t)(-bitmap->pitch);
	for (unsigned int y = 0; y < bitmap->rows; ++y) {
		const unsigned char* row = top + (ptrdiff_t)y * bitmap->pitch;
		for (unsigned int x = 0; x < bitmap->width; ++x) {
			unsigned int value;
			switch (bitmap->pixel_mode) {
			case FT_PIXEL_MODE_MONO: value = row[x >> 3] & (0x80 >> (x & 7)) ? 255 : 0; break;
			case FT_PIXEL_MODE_GRAY2: value = ((row[x >> 2] >> (6 - (x & 3) * 2)) & 3) * 85; break;
			case FT_PIXEL_MODE_GRAY4: value = ((row[x >> 1] >> (4 - (x & 1) * 4)) & 15) * 17; break;
			default: value = bitmap->num_grays > 1 ? row[x] * 255u / (bitmap->num_grays - 1) : 0;
			}
			output[(size_t)y * bitmap->width + x] = (uint8_t)value;
		}
	}
}

#endif
