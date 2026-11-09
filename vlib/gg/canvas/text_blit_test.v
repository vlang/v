module canvas

struct TextMaskCase {
	name       string
	color      u32
	background u32
	expected   []u32
}

// Fixed A8 coverage isolates the lowp compositor from host font rasterization.
// These vectors are calculated with exact rational arithmetic from Skia m144's
// NEON lowp uniform_color, scale_u8, lerp_u8 and srcover stages in
// SkRasterPipeline_opts.h:
// premultiply the source, round each division by 255 to nearest, then composite.
// They are not captured from gg.canvas or a platform's CoreGraphics output.
fn text_mask_cases() []TextMaskCase {
	return [
		TextMaskCase{
			name:     'black on transparent'
			color:    0xff000000
			expected: [u32(0x00000000), 0x01000000, 0x3f000000, 0x7f000000, 0x80000000, 0xc0000000,
				0xfe000000, 0xff000000]
		},
		TextMaskCase{
			name:       'opaque on white'
			color:      0xff143cc8
			background: 0xffffffff
			expected:   [u32(0xffffffff), 0xfffefeff, 0xffc5cff1, 0xff8a9ee4, 0xff899de3, 0xff4e6cd6,
				0xff153dc8, 0xff143cc8]
		},
		TextMaskCase{
			name:       'translucent on opaque'
			color:      0x80c81e1e
			background: 0xffdce6f0
			expected:   [u32(0xffdce6f0), 0xffdbe5ef, 0xffd9cdd6, 0xffd7b3bb, 0xffd7b4bc, 0xffd49aa1,
				0xffd28287, 0xffd28287]
		},
		TextMaskCase{
			name:       'translucent on translucent'
			color:      0x80c81e1e
			background: 0x80402010
			expected:   [u32(0x80402010), 0x80402010, 0x90512012, 0xa0621f13, 0xa0622014, 0xb0731f15,
				0xbf841f17, 0xc0841f17]
		},
		TextMaskCase{
			name:       'transparent source'
			color:      0x0044ccee
			background: 0x80402010
			expected:   [u32(0x80402010), 0x80402010, 0x80402010, 0x80402010, 0x80402010, 0x80402010,
				0x80402010, 0x80402010]
		},
	]
}

fn test_a8_mask_blits_match_reference_pixels() {
	$if macos {
		glyph := Glyph{
			width:     4
			height:    2
			image:     [u8(0), 1, 63, 127, 128, 192, 254, 255]
			has_image: true
		}
		for c in text_mask_cases() {
			mut surface := new_raster_surface(width: 4, height: 2)!
			surface.clear(c.background)
			surface.blit_glyph_mask(glyph, 0, 0, c.color)
			assert surface.pixels == c.expected, c.name
		}
	}
}

fn test_a8_mask_clipping_preserves_source_stride() {
	$if macos {
		mut glyph := Glyph{
			left:      -1
			top:       -1
			width:     4
			height:    2
			image:     [u8(0), 1, 63, 127, 128, 192, 254, 255]
			has_image: true
		}
		mut surface := new_raster_surface(width: 3, height: 2)!
		surface.clear(0x80402010)
		// Drop the first row and column, retaining coverage 192, 254 and 255.
		surface.blit_glyph_mask(glyph, 0, 0, 0x80c81e1e)
		assert surface.pixels == [u32(0xb0731f15), 0xbf841f17, 0xc0841f17, 0x80402010, 0x80402010,
			0x80402010]
		surface.clear(0x80402010)
		glyph.image[0] = 255
		// Only the first source pixel fits at the bottom-right corner.
		surface.blit_glyph_mask(glyph, 3, 2, 0x80c81e1e)
		assert surface.pixels == [u32(0x80402010), 0x80402010, 0x80402010, 0x80402010, 0x80402010,
			0xc0841f17]
		surface.clear(0x80402010)
		surface.blit_glyph_mask(glyph, -10, -10, 0x80c81e1e)
		surface.blit_glyph_mask(glyph, 10, 10, 0x80c81e1e)
		assert surface.pixels.all(it == u32(0x80402010))
	}
}
