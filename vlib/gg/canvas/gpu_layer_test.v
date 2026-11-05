module canvas

import crypto.sha256

fn test_approx_size_follows_skia() {
	// skgpu::GetApproxSize(): at least 16, powers of two up to 1024, then
	// also the midpoint between two powers of two.
	assert approx_size(1) == 16
	assert approx_size(16) == 16
	assert approx_size(38) == 64
	assert approx_size(54) == 64
	assert approx_size(1024) == 1024
	assert approx_size(1025) == 1536
	assert approx_size(1600) == 2048
}

fn test_post_translate_moves_the_translation() {
	m := post_translate(Matrix.values([f32(2), 0.5, 3, 0.25, 2, 4, 0, 0, 1]!), -4, -6)
	assert m.m == [f32(2), 0.5, -1, 0.25, 2, -2, 0, 0, 1]!
}

struct ImageShaderCase {
	step     RenderStep
	clipped  bool
	vertex   string
	fragment string
}

// The SHA-256 prefixes of the shaders Graphite compiles to draw a restored
// layer (traced from saveLayer() scenes).
const image_shader_cases = [
	ImageShaderCase{.cover_bounds, false, '2df2a425a78e449c', '045d187189c38657'},
	ImageShaderCase{.analytic_rrect, false, '265ca8e40a98f98e', 'bce37212a7e9b04c'},
	ImageShaderCase{.analytic_rrect, true, '6bd167b773b51698', 'fa38f10477a8e205'},
	ImageShaderCase{.cover_bounds, true, 'bd321cbf69fbe2d6', '4f8ff0b0d5e814c1'},
	ImageShaderCase{.per_edge_aa_quad, false, '01bf3ddc467a63cb', '375f0e8b6f17d935'},
]

fn test_image_shaders_match_graphite() {
	for c in image_shader_cases {
		vert, frag := solid_shader_sources(c.step, c.clipped)
		v, f := image_shader_sources(vert, frag, c.clipped)
		assert sha256.sum(v.bytes()).hex()[..16] == c.vertex, '${c.step} ${c.clipped}'
		assert sha256.sum(f.bytes()).hex()[..16] == c.fragment, '${c.step} ${c.clipped}'
	}
}

fn layer_scene_hash(mut ctx GpuContext, rotated bool) !string {
	mut s := ctx.new_surface(width: 64, height: 48)!
	defer {
		s.free()
	}
	s.clear(Color4f{})
	if rotated {
		s.set_matrix(Matrix.values([f32(0.8660254), -0.5, 20, 0.5, 0.8660254, -6, 0, 0, 1]!))
		s.clip_rrect(RRect.rect_radii(Rect.ltrb(4, 4, 40, 30), [Point{6, 6}, Point{6, 6}, Point{6, 6},
			Point{6, 6}]!), .intersect, true)!
		s.save_layer(none)!
		s.draw_rect(Rect.ltrb(0, 0, 64, 48), Paint{
			color:      Color4f{f32(0.9), f32(0.1), f32(0.1), 1}
			anti_alias: true
		})!
	} else {
		s.save_layer(none)!
		s.draw_rrect(RRect.rect_radii(Rect.ltrb(4.5, 4.5, 40.5, 30.5), [Point{6, 6}, Point{6, 6},
			Point{6, 6}, Point{6, 6}]!), Paint{
			color:      Color4f{f32(0.2), f32(0.4), f32(0.8), 1}
			anti_alias: true
		})!
	}
	s.restore()
	mut px := []u8{len: 64 * 48 * 4}
	s.read_pixels(px.data, 64 * 4)!
	return sha256.sum(px).hex()[..16]
}

// Graphite's pixels for a layer drawn back unclipped (the bounds renderer
// with the image shader), and for a layer under a rotated rrect clip (drawn
// back in a 4x MSAA pass through the clip's stencil).
fn test_layers_match_graphite_pixels() {
	mut ctx := new_gpu_context() or {
		eprintln('skipping: ${err}')
		return
	}
	defer {
		ctx.free()
	}
	assert layer_scene_hash(mut ctx, false)! == '13893267e421a9e1'
	assert layer_scene_hash(mut ctx, true)! == '96a69c16c9ee468c'
}

// A fractional rectangular clip trims the restored image to an edge-AA quad.
// Graphite's bytes come from the fractional-layer scene in Vox's oracle.
fn test_fractional_layer_restore_matches_graphite_pixels() {
	$if !macos {
		return
	}
	mut ctx := new_gpu_context()!
	defer {
		ctx.free()
	}
	mut s := ctx.new_surface(width: 64, height: 48)!
	defer {
		s.free()
	}
	s.clear(Color4f{})
	s.clip_rect(Rect.ltrb(2.5, 2.25, 60.5, 44.75), .intersect, true)!
	s.save_layer(none)!
	s.draw_rect(Rect.ltrb(0, 0, 64, 48), Paint{
		color:      Color4f{f32(0.9), f32(0.1), f32(0.1), 1}
		anti_alias: true
	})!
	s.restore()
	// This must exercise the quad renderer, rather than a fallback path.
	assert s.draws.any(it.step == .per_edge_aa_quad)
	mut pixels := []u8{len: 64 * 48 * 4}
	s.read_pixels(pixels.data, 64 * 4)!
	assert sha256.sum(pixels).hex() == '7779e12274bb7dfbf6f6b16453ed7187db993420c79b51a01a84871f27cb8407'
}

fn test_per_edge_quad_shaders_match_graphite() {
	for c in [
		ImageShaderCase{.per_edge_aa_quad, false, '21204b5c2895ba41', '6a40e8d3e6ceeaae'},
		ImageShaderCase{.per_edge_aa_quad, true, 'c7c35201c9f99121', 'ce5a507b35487892'},
	] {
		vert, frag := solid_shader_sources(c.step, c.clipped)
		assert sha256.sum(vert.bytes()).hex()[..16] == c.vertex
		assert sha256.sum(frag.bytes()).hex()[..16] == c.fragment
	}
}
