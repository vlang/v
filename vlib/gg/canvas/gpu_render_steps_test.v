module canvas

// The expected instance data was captured from the Metal command stream of
// Skia Graphite (Chromium 155 revision, macOS) for the same single draws on a
// 64x48 surface. The data is independent of the GPU; it decides the shader
// inputs, so any difference changes rendered pixels.

struct InstanceCase {
	name     string
	matrix   [9]f32
	paint    Paint
	kind     string
	edges    [4]f32
	radii    [8]f32
	expected string
}

const identity_values = [f32(1), 0, 0, 0, 1, 0, 0, 0, 1]!

fn instance_cases() []InstanceCase {
	fill := Paint{
		color:      Color4f{0.25, 0.5, 0.75, 1}
		anti_alias: true
	}
	translucent := Paint{
		color:      Color4f{0.25, 0.5, 0.75, 0.5}
		anti_alias: true
	}
	gray := Paint{
		color:      Color4f{0.5, 0.5, 0.5, 1}
		anti_alias: true
	}
	return [
		InstanceCase{
			name:     'fill_rrect'
			matrix:   identity_values
			paint:    fill
			kind:     'rrect'
			edges:    [f32(5.25), 4.5, 45.75, 34.75]!
			radii:    [f32(6), 6, 6, 6, 6, 6, 6, 6]!
			expected: '0000c0400000c0400000c0400000c0400000c0400000c0400000c0400000c0400000a840000090400000374200000b420000cc4100009d410000803f0000803f00ff7f3f000000000000803f0000000000000000000000000000803f0000000000000000000000000000803f'
		},
		InstanceCase{
			name:     'fill_rect_fractional'
			matrix:   identity_values
			paint:    translucent
			kind:     'rect'
			edges:    [f32(5.25), 4.5, 45.75, 34.75]!
			expected: '000080bf000080bf000080bf000080bf0000a84000003742000037420000a840000090400000904000000b4200000b420000cc4100009d410000803f0000803f00ff7f3f000000000000803f0000000000000000000000000000803f0000000000000000000000000000803f'
		},
		InstanceCase{
			name:     'fill_rect_aligned'
			matrix:   identity_values
			paint:    translucent
			kind:     'rect'
			edges:    [f32(5), 4, 45, 34]!
			expected: '0000a04000008040000034420000084200ff7f3f000000000000803f0000000000000000000000000000803f0000000000000000000000000000803f'
		},
		InstanceCase{
			name:     'elliptical'
			matrix:   identity_values
			paint:    Paint{
				color:      Color4f{1, 0, 0, 1}
				anti_alias: true
			}
			kind:     'rrect'
			edges:    [f32(3.75), 6.5, 53.75, 39.5]!
			radii:    [f32(13), 9, 15, 10, 17, 11, 19, 12]!
			expected: '0000504100007041000088410000984100001041000020410000304100004041000070400000d0400000574200001e420000e6410000b8410000803f0000803f00ff7f3f000000000000803f0000000000000000000000000000803f0000000000000000000000000000803f'
		},
		InstanceCase{
			name:     'stroke_miter'
			matrix:   identity_values
			paint:    Paint{
				color:        Color4f{0, 0.5, 0, 1}
				anti_alias:   true
				style:        .stroke
				stroke_width: 2.5
			}
			kind:     'rrect'
			edges:    [f32(6.25), 5.5, 50.5, 40.25]!
			radii:    [f32(4), 4, 4, 4, 4, 4, 4, 4]!
			expected: '000000c0000000000000a03f0000803f000080400000804000008040000080400000c8400000b04000004a42000021420000e3410000b741000000000000803f00ff7f3f000000000000803f0000000000000000000000000000803f0000000000000000000000000000803f'
		},
		InstanceCase{
			name:     'stroke_bevel_rect'
			matrix:   identity_values
			paint:    Paint{
				color:        Color4f{0, 0.5, 0, 1}
				anti_alias:   true
				style:        .stroke
				stroke_width: 3
				join:         .bevel
			}
			kind:     'rect'
			edges:    [f32(6.25), 5.5, 50.5, 40.25]!
			expected: '000000c0000000000000c03f00000000000000000000000000000000000000000000c8400000b04000004a42000021420000e3410000b741000000000000803f00ff7f3f000000000000803f0000000000000000000000000000803f0000000000000000000000000000803f'
		},
		InstanceCase{
			name:     'stroke_round_thick'
			matrix:   identity_values
			paint:    Paint{
				color:        Color4f{0, 0.5, 0, 1}
				anti_alias:   true
				style:        .stroke
				stroke_width: 30
				join:         .round
			}
			kind:     'rect'
			edges:    [f32(6.25), 5.5, 30.5, 20.25]!
			expected: '000000c00000000000007041000080bf000000000000000000000000000000000000c8400000b0400000f4410000a2410000934100004e410000803f0000803f00ff7f3f000000000000803f0000000000000000000000000000803f0000000000000000000000000000803f'
		},
		InstanceCase{
			name:     'hairline_rrect'
			matrix:   identity_values
			paint:    Paint{
				color:      Color4f{0, 0, 1, 1}
				anti_alias: true
				style:      .stroke
			}
			kind:     'rrect'
			edges:    [f32(6.25), 5.5, 50.5, 40.25]!
			radii:    [f32(3), 5, 6, 2, 4, 4, 0, 0]!
			expected: '0000a0c0000000c10000c0c0000000c00000a0400000004000008040000000000000c8400000b04000004a42000021420000e3410000b741000000000000803f00ff7f3f000000000000803f0000000000000000000000000000803f0000000000000000000000000000803f'
		},
		InstanceCase{
			name:     'oval'
			matrix:   identity_values
			paint:    gray
			kind:     'oval'
			edges:    [f32(10.5), 8.25, 50, 40]!
			expected: '00009e4100009e4100009e4100009e4100007e4100007e4100007e4100007e41000028410000044100004842000020420000f2410000c1410000803f0000803f00ff7f3f000000000000803f0000000000000000000000000000803f0000000000000000000000000000803f'
		},
		InstanceCase{
			name:     'thin_rect'
			matrix:   identity_values
			paint:    gray
			kind:     'rect'
			edges:    [f32(10.5), 8.25, 11.25, 40]!
			expected: '000080bf000080bf000080bf000080bf000028410000344100003441000028410000044100000441000020420000204200002e410000c1410000803f000080bf00ff7f3f000000000000803f0000000000000000000000000000803f0000000000000000000000000000803f'
		},
		InstanceCase{
			name:     'non_aa_rrect'
			matrix:   identity_values
			paint:    Paint{
				color: Color4f{0.5, 0.5, 0.5, 1}
			}
			kind:     'rrect'
			edges:    [f32(5.25), 4.5, 45.75, 34.75]!
			radii:    [f32(6), 6, 6, 6, 6, 6, 6, 6]!
			expected: '0000c0400000c0400000c0400000c0400000c0400000c0400000c0400000c0400000a0400000a0400000384200000c420000cc410000a0410000803f0000803f00ff7f3f000000000000803f0000000000000000000000000000803f0000000000000000000000000000803f'
		},
		InstanceCase{
			name:     'anisotropic'
			matrix:   [f32(2), 0, 1, 0, 0.75, 3, 0, 0, 1]!
			paint:    gray
			kind:     'rrect'
			edges:    [f32(3.25), 4.5, 25.75, 34.75]!
			radii:    [f32(3), 3, 3, 3, 3, 3, 3, 3]!
			expected: '000040400000404000004040000040400000404000004040000040400000404000005040000090400000ce4100000b420000684100009d410000803fabaaaa3f00ff7f3f00000000000000400000000000000000000000000000403f000000000000803f000040400000803f'
		},
		InstanceCase{
			name:     'rotated'
			matrix:   [f32(0.8660254037844387), -0.49999999999999994, 20, 0.49999999999999994,
				0.8660254037844387, -6, 0, 0, 1]!
			paint:    gray
			kind:     'rrect'
			edges:    [f32(10.5), 8.25, 40, 30]!
			radii:    [f32(4), 4, 4, 4, 4, 4, 4, 4]!
			expected: '00008040000080400000804000008040000080400000804000008040000080400000284100000441000020420000f0410000ca41000099410000803f0000803f00ff7f3f00000000d7b35d3f0000003f00000000000000bfd7b35d3f000000000000a0410000c0c00000803f'
		},
		InstanceCase{
			name:     'skewed_stroke'
			matrix:   [f32(1), 0.3, 0, 0.1, 1, 0, 0, 0, 1]!
			paint:    Paint{
				color:        Color4f{0.5, 0.5, 0.5, 1}
				anti_alias:   true
				style:        .stroke
				stroke_width: 2
				join:         .round
			}
			kind:     'rect'
			edges:    [f32(10.5), 8.25, 40, 30]!
			expected: '000000c0000000000000803f000080bf000000000000000000000000000000000000284100000441000020420000f0410000ca41000099410000000039029f3f00ff7f3f000000000000803fcdcccc3d000000009a99993e0000803f0000000000000000000000000000803f'
		},
	]
}

// recording_surface returns a surface that records draws without GPU
// resources; recording never touches sokol.gfx.
fn recording_surface(width int, height int, m [9]f32) GpuSurface {
	return GpuSurface{
		width:        width
		height:       height
		sample_count: 1
		transform:    Transform.new(Matrix.values(m))
		bounds:       new_bounds_manager(width, height)
		clip:         new_clip_stack(width, height)
	}
}

fn test_instance_data_matches_graphite() {
	for c in instance_cases() {
		mut s := recording_surface(64, 48, c.matrix)
		e := c.edges
		rect := Rect.ltrb(e[0], e[1], e[2], e[3])
		match c.kind {
			'rect' {
				s.draw_rect(rect, c.paint) or { panic(err) }
			}
			'oval' {
				s.draw_oval(rect, c.paint) or { panic(err) }
			}
			else {
				r := c.radii
				s.draw_rrect(RRect.rect_radii(rect, [Point{r[0], r[1]}, Point{r[2], r[3]},
					Point{r[4], r[5]}, Point{r[6], r[7]}]!), c.paint) or { panic(err) }
			}
		}
		assert s.draws.len == 1, c.name
		assert s.draws[0].instance.hex() == c.expected, c.name
	}
}

fn test_quick_reject_and_culling_take_no_depth() {
	mut s := recording_surface(64, 48, identity_values)
	paint := Paint{
		color:      Color4f{1, 0, 0, 1}
		anti_alias: true
	}
	// Transparent paints and offscreen draws are dropped before recording.
	s.draw_rect(Rect.ltrb(1, 1, 10, 10), Paint{
		color: Color4f{1, 0, 0, 0}
	})!
	s.draw_rect(Rect.ltrb(80, 1, 90, 10), paint)!
	assert s.draws.len == 0
	assert s.depth_index == 0
	s.draw_rect(Rect.ltrb(1.5, 1, 10, 10), paint)!
	assert s.draws.len == 1
	assert s.draws[0].order.depth == 1
}

fn test_elliptical_strokes_use_the_stroke_renderer() {
	mut s := recording_surface(64, 48, identity_values)
	stroke := Paint{
		color:        Color4f{1, 0, 0, 1}
		anti_alias:   true
		style:        .stroke
		stroke_width: 2
	}
	rr := RRect.rect_radii(Rect.ltrb(5, 5, 40, 30), [Point{8, 3}, Point{8, 3}, Point{8, 3},
		Point{8, 3}]!)
	// Elliptical strokes are not simple shapes: Graphite strokes them with
	// the tessellated stroke renderer.
	s.draw_rrect(rr, stroke)!
	assert s.draws.len == 1 && s.draws[0].step == .tessellate_strokes
	assert s.draws[0].instance_count == 8
}
