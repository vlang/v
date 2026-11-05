module canvas

// draw_path draws a path like SkCanvas::drawPath() on a Graphite device.
// Paths that Graphite draws as rects, ovals or double rects use those
// renderers; other filled paths use the MSAA tessellation renderers (convex
// wedges, or stencil-and-cover wedges). It returns an error for draws that
// need renderers which are not ported yet (strokes, inverse fills, large
// complex paths).
pub fn (mut s GpuSurface) draw_path(path Path, paint Paint) ! {
	if !path.is_finite() {
		return
	}
	if path.is_inverse_fill() {
		return error('gg.canvas: inverse-filled paths are not supported')
	}
	if paint.style == .stroke_and_fill {
		return error('gg.canvas: stroke-and-fill needs the tessellating stroke renderer')
	}
	if s.quick_reject(path.bounds(), paint) {
		return
	}
	// Device::drawPath()
	if _, _ := path.is_line() {
		if paint.style != .fill {
			return error('gg.canvas: stroked lines are not supported')
		}
		return
	}
	if path.is_a == .rrect {
		// Device::drawPath() draws the rrect deduced from the contour.
		return error('gg.canvas: rounded-rectangle paths are not supported')
	}
	if oval := path.is_oval() {
		// Device::drawOval() draws the oval as a rounded rect.
		s.draw_rrect_unchecked(RRect.oval(oval), paint)!
		return
	}
	if rect, closed := path.is_rect() {
		if paint.style == .fill || closed {
			s.device_draw_rect(rect, paint)!
			return
		}
	}
	if paint.style == .fill {
		if rects, dirs := path.nested_fill_rects() {
			if path.fill_type == .winding && dirs[0] == dirs[1] {
				s.device_draw_rect(rects[0], paint)!
			} else {
				s.device_draw_drrect(RRect.from_rect(rects[0]), RRect.from_rect(rects[1]),
					paint)!
			}
			return
		}
	}
	if paint.style == .stroke {
		s.draw_stroke(path, ClipShape{
			kind: .path
			rect: path.bounds()
		}, paint)!
		return
	}
	s.draw_path_fill(path, paint)!
}

// draw_path_fill mirrors Device::drawGeometry() for a filled path with
// PathRendererStrategy::kTessellation: convex paths use the shaded convex
// wedges, others the stencil-and-cover wedges.
fn (mut s GpuSurface) draw_path_fill(path Path, paint Paint) ! {
	if !s.transform.valid() {
		return
	}
	if int(s.transform.typ) > int(TransformType.affine) {
		return error('gg.canvas: perspective transforms are not supported')
	}
	clip := s.visit_clip_stack_for_draw(ClipShape{
		kind: .path
		rect: path.bounds()
	}, paint)!
	if clip.clipped_out {
		return
	}
	// Device::chooseMSAARenderer()
	convex := path.is_convex()
	if !convex && path.verbs.len >= 50 && clip.shape_bounds.area() > f32(256 * 256) {
		return error('gg.canvas: the tessellated curves and triangles renderer is not ported')
	}
	clipped := !clip.analytic.is_empty()
	clip_uniforms := if clipped {
		analytic_clip_uniforms(clip.analytic)
	} else {
		AnalyticClipUniforms{}
	}
	color := paint.color.premul()
	depends_on_dst := clipped || !color.is_opaque()
	s.depth_index++
	mut order := DrawOrder{
		depth: s.depth_index
	}
	clip_order := s.update_clip_state_for_draw(clip.elements, clip.draw_bounds, order.depth)
	order.depends_on_paint_order(clip_order)
	if depends_on_dst {
		order.depends_on_paint_order(s.bounds.most_recent_draw(clip.draw_bounds))
	}
	draw_clip := DrawClip{
		scissor:      clip.scissor
		draw_bounds:  clip.draw_bounds
		shape_bounds: clip.shape_bounds
	}
	mut patches := []u8{}
	count, n4 := write_wedge_patches(mut patches, path, s.transform.matrix, order.depth)!
	matrix := local_to_device_uniforms(s.transform.matrix)
	if convex {
		s.record_step(.wedges_shaded, depends_on_dst, patches, PaintUniforms{
			...matrix
			color:      color
			with_color: true
			clipped:    clipped
			clip:       clip_uniforms
		}, order, 0, 0, draw_clip, u32(count))
		s.draws[s.draws.len - 1].template_count = wedge_vertex_count(n4)
	} else {
		order.stencil_index = s.add_disjoint_stencil(order.paint_order, clip.draw_bounds)
		step := if path.fill_type == .even_odd {
			RenderStep.stencil_wedges_even_odd
		} else {
			RenderStep.stencil_wedges_winding
		}
		s.record_step(step, false, patches, matrix, order, 0, 0, draw_clip, u32(count))
		s.draws[s.draws.len - 1].template_count = wedge_vertex_count(n4)
		mut cover := []u8{cap: cover_bounds_instance_stride}
		write_cover_bounds_instance(mut cover, path.bounds(), s.transform, order.depth, 0)
		s.record_step(.cover_fill, depends_on_dst, cover, PaintUniforms{
			color:   color
			clipped: clipped
			clip:    clip_uniforms
		}, order, 1, 0, draw_clip, 1)
	}
	s.bounds.record_draw(clip.draw_bounds, order.paint_order)
}

// add_disjoint_stencil mirrors Device::IntersectionTreeSet::add(): the index
// of the first stencil set of the paint order whose draws `r` does not
// intersect, adding `r` to it.
fn (mut s GpuSurface) add_disjoint_stencil(order u16, r Rect) u16 {
	mut trees := s.disjoint[order] or { [][]Rect{} }
	for i, tree in trees {
		if r.is_empty_negative_or_nan() || !tree.any(it.intersects(r)) {
			if !r.is_empty_negative_or_nan() {
				trees[i] << r
			}
			s.disjoint[order] = trees
			return u16(i)
		}
	}
	trees << [r]
	s.disjoint[order] = trees
	return u16(trees.len - 1)
}
