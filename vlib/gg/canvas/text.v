module canvas

// CPU glyph rendering uses CoreText on macOS and FreeType on Linux. The native
// masks share scaler records, mask gamma, subpixel positioning and the BGRA8
// compositor. The macOS backend reproduces Skia's CoreGraphics raster text path.

pub enum FontEdging {
	alias
	antialias
	subpixel_antialias
}

pub enum FontHinting {
	none
	slight
	normal
	full
}

// Font holds the settings of an SkFont. The defaults are SkFont's.
pub struct Font {
pub mut:
	typeface         &Typeface = unsafe { nil }
	size             f32       = 12
	scale_x          f32       = 1
	skew_x           f32
	edging           FontEdging  = .antialias
	hinting          FontHinting = .normal
	subpixel         bool
	linear_metrics   bool
	embolden         bool
	baseline_snap    bool = true
	embedded_bitmaps bool
}

// SurfaceProps mirrors the text-related parts of SkSurfaceProps. Only an
// unknown pixel geometry is supported (no LCD masks).
pub struct SurfaceProps {
pub:
	text_contrast f32 = 0.5
	text_gamma    f32
}
