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
// an opaque paint color and dithering.
fn (p Paint) shading() !(Shading, GradientUniforms) {
	g := p.gradient or { return Shading.solid, GradientUniforms{} }
	if p.color.a != 1 {
		return error('gg.canvas: translucent gradient paints are not supported')
	}
	if !p.dither {
		return error('gg.canvas: gradients without dithering are not supported')
	}
	return gradient_uniforms(g)!
}

// solid_only returns an error for paints with a shader, for the renderers
// that only draw solid colors.
fn (p Paint) solid_only() ! {
	if p.gradient != none {
		return error('gg.canvas: gradients are only supported on rects and rrects')
	}
}
