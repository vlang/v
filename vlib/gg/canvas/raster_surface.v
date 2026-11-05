module canvas

// RasterSurface is a CPU pixel buffer in BGRA8 premultiplied order (each u32
// is 0xAARRGGBB, Skia's kN32 SkPMColor on little-endian Apple platforms)
// that draws like an SkCanvas over SkSurfaces::WrapPixels() with an sRGB
// color space.
pub struct RasterSurface {
pub:
	width  int
	height int
pub mut:
	pixels []u32
	props  SurfaceProps
	matrix Matrix = Matrix.identity()
}

@[params]
pub struct RasterSurfaceConfig {
pub:
	width  int
	height int
	props  SurfaceProps
}

pub fn new_raster_surface(cfg RasterSurfaceConfig) !RasterSurface {
	if cfg.width <= 0 || cfg.height <= 0 {
		return error('gg.canvas: invalid raster surface size ${cfg.width}x${cfg.height}')
	}
	return RasterSurface{
		width:  cfg.width
		height: cfg.height
		pixels: []u32{len: cfg.width * cfg.height}
		props:  cfg.props
	}
}

// clear fills the surface with a premultiplied 0xAARRGGBB value.
pub fn (mut s RasterSurface) clear(pm_color u32) {
	for i in 0 .. s.pixels.len {
		s.pixels[i] = pm_color
	}
}

pub fn (mut s RasterSurface) set_matrix(m Matrix) {
	s.matrix = m
}
