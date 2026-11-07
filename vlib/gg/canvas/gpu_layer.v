module canvas

import sokol.gfx

// TextureKind is the fragment texture a draw samples: the gradients' dither
// LUT or a layer's image.
enum TextureKind {
	none
	dither
	image
}

// TextureBinding is a draw's texture and sampler; draws with the same
// binding share an index in the draw list's texture binding cache, like
// Graphite's TextureDataCache.
struct TextureBinding {
	kind  TextureKind
	image gfx.Image
}

fn same_texture_binding(a TextureBinding, b TextureBinding) bool {
	return a.kind == b.kind && a.image.id == b.image.id
}

// ImageUniforms are the uniforms of an image drawn with hardware tiling:
// Graphite's LocalMatrix block (upper2x2, translation), the CoordNormalize
// block's invDimensions and the ColorSpaceTransform block's mode.
struct ImageUniforms {
	upper2x2       [4]f32
	translation    [2]f32
	inv_dimensions [2]f32
	mode           f32
}

// image_uniform_size is the size of ImageUniforms in CombinedUniformData,
// padded to 16 bytes.
const image_uniform_size = 48

fn same_image_uniforms(a ImageUniforms, b ImageUniforms) bool {
	return a.upper2x2 == b.upper2x2 && a.translation == b.translation
		&& a.inv_dimensions == b.inv_dimensions && a.mode == b.mode
}

fn (u ImageUniforms) serialize(mut out []f32) {
	for v in u.upper2x2 {
		out << v
	}
	out << u.translation[0]
	out << u.translation[1]
	out << u.inv_dimensions[0]
	out << u.inv_dimensions[1]
	out << u.mode
	out << 0
	out << 0
	out << 0
}

// LayerImage is the texture of a restored layer, which the parent device
// draws like Graphite's Device::drawSpecial().
struct LayerImage {
	image          gfx.Image
	texture_width  int
	texture_height int
}

// approx_size mirrors skgpu::GetApproxSize() for one dimension: the size of
// an SkBackingFit::kApprox texture.
fn approx_size(value_in int) int {
	value := if value_in < 16 { 16 } else { value_in }
	if value & (value - 1) == 0 {
		return value
	}
	mut ceil_pow2 := 1
	for ceil_pow2 < value {
		ceil_pow2 <<= 1
	}
	if value <= 1024 {
		return ceil_pow2
	}
	floor_pow2 := ceil_pow2 >> 1
	mid := floor_pow2 + (floor_pow2 >> 1)
	return if value <= mid { mid } else { ceil_pow2 }
}

// post_translate mirrors SkM44::postTranslate(dx, dy) on a 2D matrix: each
// column gains (dx, dy) times its last row entry.
fn post_translate(m Matrix, dx f32, dy f32) Matrix {
	v := m.m
	mut out := v
	for col in 0 .. 3 {
		w := v[6 + col]
		x := dx * w
		y := dy * w
		out[col] = v[col] + x
		out[3 + col] = v[3 + col] + y
	}
	return Matrix{
		m: out
	}
}

// new_layer_surface creates the device of a layer: `width` x `height`
// device pixels in an approximately sized texture, initially transparent.
fn (mut ctx GpuContext) new_layer_surface(width int, height int) !&GpuSurface {
	return ctx.new_sized_surface(width, height, approx_size(width), approx_size(height), 1)!
}

// save_layer mirrors SkCanvas::saveLayer() without a paint: later draws go
// to a transparent layer covering the device clip bounds (intersected with
// `bounds`, in local coordinates), which the matching restore() draws onto
// the device through the clip of the save.
pub fn (mut s GpuSurface) save_layer(bounds ?Rect) ! {
	if s.layer != unsafe { nil } {
		mut top := s.layer
		top.save_layer(bounds)!
		return
	}
	// SkCanvas::internalSaveLayer(): the save, then the layer unless the
	// clip is empty.
	s.save()
	clip_bounds := s.conservative_clip_bounds() or { return }
	mut layer_bounds := clip_bounds
	if b := bounds {
		content := map_rect_points(s.transform.matrix, b.sorted()).round_out()
		layer_bounds = if sk_rects_intersect(layer_bounds, content) {
			layer_bounds.intersect(content)
		} else {
			Rect{}
		}
	}
	if layer_bounds.is_empty_negative_or_nan() {
		// abortLayer: the save clips everything out.
		s.clip_shape(rect_shape(Rect{}), .intersect, true) or {
			s.restore()
			return err
		}
		return
	}
	ox := layer_bounds.left()
	oy := layer_bounds.top()
	mut layer := s.ctx.new_layer_surface(int(layer_bounds.width()), int(layer_bounds.height())) or {
		s.restore()
		return err
	}
	layer.is_layer = true
	layer.origin_x = int(ox)
	layer.origin_y = int(oy)
	// SkDevice::setDeviceCoordinateSystem(): the prior device's matrix and its
	// global-to-device mapping, translated to the layer's origin.
	layer.global_to_device = post_translate(s.global_to_device, -ox, -oy)
	layer.ctm = s.ctm
	layer.transform = Transform.new(post_translate(s.transform.matrix, -ox, -oy))
	s.layer = layer
}

// restore_layer ends the active layer of this device, like
// SkCanvas::internalRestore(): the save that created it is restored, then
// the layer is drawn through the restored clip.
fn (mut s GpuSurface) restore_layer() {
	mut layer := s.layer
	s.layer = unsafe { nil }
	s.device_restore()
	s.draw_layer(mut layer) or {
		if s.deferred_error == '' {
			s.deferred_error = err.msg()
		}
	}
}

// draw_layer mirrors SkDevice::drawDevice() followed by Graphite's
// Device::drawSpecial(): the layer's device bounds as an antialiased edge-AA
// quad at its origin, shaded with its texture (nearest sampling and the
// identity local matrix).
fn (mut s GpuSurface) draw_layer(mut layer GpuSurface) ! {
	// The layer is freed after this device's next flush, even if its own
	// flush fails.
	s.retired << layer
	layer.flush()!
	saved := s.transform
	defer {
		s.transform = saved
	}
	s.transform = Transform.new(Matrix.translate(f32(layer.origin_x), f32(layer.origin_y)))
	paint := Paint{
		anti_alias:  true
		layer_image: LayerImage{
			image:          layer.color
			texture_width:  layer.texture_width
			texture_height: layer.texture_height
		}
	}
	s.draw_shape(ShapeGeometry{
		kind: .rect
		rect: Rect.ltrb(0, 0, f32(layer.width), f32(layer.height))
	}, paint)!
}

// image_shader_sources turns a render step's solid-color shaders into the
// ones Graphite generates for an image drawn with hardware tiling.
fn image_shader_sources(vert string, frag string, clipped bool) (string, string) {
	snippets := gradient_snippets()
	varying := '    float2 CoordNormalize_3_Var [[user(locn1)]];\n'
	mut v := paint_varyings(vert.replace('float4 color_1;\n', snippets['fields_image']), clipped,
		varying)
	mut lines := []string{}
	for line in v.split('\n') {
		if line.starts_with('_out.SolidColor_1_Var = ') {
			for out in snippets['vertex_image'].trim_right('\n').split('\n') {
				// Clipped draws already pass the uniform index.
				if clipped && out.starts_with('_out.ssboIndexVar') {
					continue
				}
				lines << out
			}
			continue
		}
		lines << line
	}
	v = rename_clip_block(lines.join('\n'))
	mut f := frag.replace('struct CombinedUniformData {', snippets['preamble'] +
		'struct CombinedUniformData {')
	f = paint_varyings(f.replace('float4 color_1;\n', snippets['fields_image']), clipped,
		varying)
	if !f.contains('uint uniformSsboIndex;') {
		f = f.replace('    const device CombinedUniforms* _anonInterface0;\n',
			'    const device CombinedUniforms* _anonInterface0;\n    uint uniformSsboIndex;\n')
	}
	f = f.replace('    constant IntrinsicUniforms* _anonInterface1;\n',
		'    constant IntrinsicUniforms* _anonInterface1;\n    sampler2D image_4;\n')
	f = f.replace('bool _frontFacing [[front_facing]]',
		'texture2d<float> image_4_Tex [[texture(0)]], sampler image_4_Smplr [[sampler(0)]], bool _frontFacing [[front_facing]]')
	sampler_globals := 'Globals _globals{&_anonInterface0, {}, &_anonInterface1, {image_4_Tex, image_4_Smplr}};'
	f = f.replace('Globals _globals{&_anonInterface0, &_anonInterface1};', sampler_globals)
	f = f.replace('Globals _globals{&_anonInterface0, {}, &_anonInterface1};', sampler_globals)
	k := f.index('fragment Outputs') or { 0 }
	head := f[..k]
	body := snippets['main_image']
	mut main := shift_inlined_variables(f[k..], max_inlined_variable(body) + 1)
	ssbo := '_globals.uniformSsboIndex = _in.ssboIndexVar;\n'
	main = main.replace(ssbo, '')
	main = main.replace('float4 outColor_1 = _in.SolidColor_1_Var;\n', ssbo + body)
	return v, renumber_temporaries(rename_clip_block(head + main))
}
