module canvas

pub enum PaintStyle {
	fill
	stroke
	stroke_and_fill
}

pub enum StrokeJoin {
	miter
	round
	bevel
}

pub enum StrokeCap {
	butt
	round
	square
}

// Paint holds the color, shader and style of a draw, like SkPaint.
pub struct Paint {
pub mut:
	color        Color4f = Color4f{0, 0, 0, 1}
	anti_alias   bool
	style        PaintStyle
	stroke_width f32
	miter_limit  f32 = 4
	join         StrokeJoin
	cap          StrokeCap
	// A gradient shades the draw instead of the color, whose alpha would
	// modulate it (only opaque colors are supported).
	gradient ?Gradient
	// Dither the shading, like SkPaint::setDither(); gradients require it.
	dither bool
}

// Paint.rgba8 creates a fill paint from 8-bit components, like SkPaint::setColor().
pub fn Paint.rgba8(r u8, g u8, b u8, a u8) Paint {
	return Paint{
		color: Color4f.from_rgba8(r, g, b, a)
	}
}

// stroke_radius returns half the stroke width.
fn (p Paint) stroke_radius() f32 {
	return f32(0.5) * p.stroke_width
}

// effective_style mirrors SkStrokeRec(paint).getStyle(): a zero-width
// stroke is a hairline.
fn (p Paint) is_hairline() bool {
	return p.style == .stroke && p.stroke_width == 0
}

// shading returns the paint's shading and gradient uniforms. Gradients need
// dithering; a translucent paint color multiplies the gradient by its alpha,
// like Graphite's ShadingParams::handlePaintAlpha().
fn (p Paint) shading() !(Shading, GradientUniforms) {
	g := p.gradient or { return Shading.solid, GradientUniforms{} }
	if !p.dither {
		return error('gg.canvas: gradients without dithering are not supported')
	}
	mut shading, mut u := gradient_uniforms(g)!
	if p.color.a != 1 {
		shading = shading.with_alpha()
		// KeyContext's paint color: the opaque RGB with the paint's alpha.
		u.paint_color = p.color
		u.opaque = false
	}
	return shading, u
}
