// vtest build: macos
module canvas

fn C.CFGetRetainCount(ref voidptr) i64

fn test_wrapped_typeface_releases_its_retained_font_and_rejects_reuse() {
	$if macos {
		mut face := Typeface.from_file('/System/Library/Fonts/Supplemental/Arial.ttf')!
		defer { face.free() }
		before := C.CFGetRetainCount(face.ct_font)
		mut wrapped := Typeface.from_ct_font(face.ct_font)!
		assert C.CFGetRetainCount(face.ct_font) == before + 1
		wrapped.free()
		wrapped.free()
		assert C.CFGetRetainCount(face.ct_font) == before
		mut s := new_raster_surface(width: 40, height: 40)!
		s.clear(0xff123456)
		s.draw_glyphs(Font{ typeface: wrapped }, [u16(36)], [Point{2, 25}], Point{}, Paint{}) or {
			assert err.msg().contains('freed')
			assert s.pixels.all(it == 0xff123456)
			return
		}
		assert false, 'a freed font was passed to CoreText'
	}
}

fn test_strike_cache_is_bounded_and_preserves_recently_used_entries() {
	$if macos {
		clear_text_cache()
		defer { clear_text_cache() }
		mut face := Typeface.from_file('/System/Library/Fonts/Supplemental/Arial.ttf')!
		defer { face.free() }
		mut s := new_raster_surface(width: 80, height: 80)!
		for i in 0 .. max_text_strikes {
			s.draw_glyphs(Font{ typeface: face, size: f32(8 + i) }, [u16(36)],
				[Point{1, 60}], Point{}, Paint{})!
		}
		assert strike_cache.strikes.len == max_text_strikes
		oldest := strike_cache.strikes[0].rec.key()
		second := strike_cache.strikes[1].rec.key()
		s.draw_glyphs(Font{ typeface: face, size: 8 }, [u16(36)], [Point{1, 60}], Point{}, Paint{})!
		assert strike_cache.strikes.last().rec.key() == oldest
		s.draw_glyphs(Font{ typeface: face, size: f32(8 + max_text_strikes) }, [u16(36)],
			[Point{1, 60}], Point{}, Paint{})!
		assert strike_cache.strikes.len == max_text_strikes
		assert strike_cache.strikes.any(it.rec.key() == oldest)
		assert !strike_cache.strikes.any(it.rec.key() == second)
		clear_text_cache()
		clear_text_cache()
		assert strike_cache.strikes.len == 0
		// Clearing native strike resources does not close the caller's face.
		s.draw_glyphs(Font{ typeface: face, size: 8 }, [u16(36)], [Point{1, 60}], Point{}, Paint{})!
		assert strike_cache.strikes.len == 1
	}
}

fn test_glyph_cache_limits_masks_and_regenerates_identical_pixels() {
	$if macos {
		clear_text_cache()
		defer { clear_text_cache() }
		mut face := Typeface.from_file('/System/Library/Fonts/Supplemental/Arial.ttf')!
		defer { face.free() }
		rec := make_rec(Font{ typeface: face, size: 128 }, 0xff000000,
			SurfaceProps{}, RasterMatrix.identity())!
		mut strike := find_or_create_strike(rec, face)!
		first := strike.glyph(u32(36) << 2)!
		expected := first.image.clone()
		defer { unsafe { expected.free() } }
		for id in 37 .. 550 {
			strike.glyph(u32(id) << 2)!
			assert strike.glyphs.len <= max_strike_glyphs
			assert strike.image_bytes <= max_strike_image_bytes
		}
		assert u32(36) << 2 !in strike.glyph_index
		regenerated := strike.glyph(u32(36) << 2)!
		assert regenerated.image == expected
	}
}

fn test_typeface_free_removes_only_its_owned_strikes() {
	$if macos {
		clear_text_cache()
		defer { clear_text_cache() }
		mut first := Typeface.from_file('/System/Library/Fonts/Supplemental/Arial.ttf')!
		mut second := Typeface.from_ct_font(first.ct_font)!
		defer { second.free() }
		mut s := new_raster_surface(width: 40, height: 40)!
		for face in [first, second] {
			s.draw_glyphs(Font{ typeface: face }, [u16(36)], [Point{2, 25}], Point{}, Paint{})!
		}
		assert strike_cache.strikes.len == 2
		first.free()
		assert strike_cache.strikes.len == 1
		assert strike_cache.strikes[0].rec.typeface_id == second.id
		s.draw_glyphs(Font{ typeface: second }, [u16(36)], [Point{2, 25}], Point{}, Paint{})!
	}
}
