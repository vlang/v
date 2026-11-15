module canvas

import os

struct GlslShaderCase {
	name     string
	step     RenderStep
	clipped  bool
	shading  Shading
	vertex   string
	fragment string
}

fn glsl_shader_cases() []GlslShaderCase {
	mut cases := []GlslShaderCase{}
	for step in [RenderStep.analytic_rrect, .cover_bounds, .stencil_wedges_winding,
		.stencil_wedges_even_odd, .cover_fill, .convex_wedges, .wedges_shaded, .tessellate_strokes,
		.cover_inverse_clip, .cover_clip, .cover_inverse_fill, .per_edge_aa_quad] {
		for clipped in [false, true] {
			for shading in [Shading.solid, .linear_4, .linear_8, .radial_4, .radial_8, .linear_4_alpha,
				.linear_8_alpha, .radial_4_alpha, .radial_8_alpha, .image] {
				if !writes_color(step) && (clipped || shading != .solid) { continue }
				// Layers are restored as rectangular geometry, never as paths.
				if shading == .image && step !in [.cover_bounds, .analytic_rrect, .per_edge_aa_quad] {
					continue
				}
				mut vs, mut fs := solid_shader_sources(step, clipped)
				if shading == .image {
					vs, fs = image_shader_sources(vs, fs, clipped)
				} else if shading.is_gradient() {
					vs = gradient_vertex_source(vs, step, shading, clipped)
					fs = gradient_fragment_source(fs, shading, clipped)
				}
				cases << GlslShaderCase{'${step}_${clipped}_${shading}', step, clipped, shading, vs, fs}
			}
		}
	}
	return cases
}

fn test_glsl_all_canvas_variants_preserve_packed_uniform_layouts() {
	for c in glsl_shader_cases() {
		vs := glsl_shader_source(c.vertex, true)!
		assert vs.starts_with('#version 430 core\n'), c.name
		assert vs.contains('gl_Position.z = 2.0 * gl_Position.z - gl_Position.w;'), c.name
		if has_uniforms(c.step) {
			assert glsl_paint_uniform_size(c.vertex)! == step_uniform_size(c.step, c.clipped, c.shading), c.name
			assert vs.contains('layout(std430, binding = 0)'), c.name
		} else {
			assert glsl_paint_uniform_size(c.vertex)! == 0, c.name
		}
		if writes_color(c.step) {
			fs := glsl_shader_source(c.fragment, false)!
			assert glsl_paint_uniform_size(c.fragment)! == step_uniform_size(c.step, c.clipped, c.shading), c.name
			assert fs.contains('layout(std430, binding = 8)'), c.name
			assert !fs.contains('gl_Position'), c.name
			if c.shading != .solid {
				assert fs.contains('uniform sampler2D canvas_texture;'), c.name
			}
		}
	}
}

// Run the independent GLSL frontend when installed. Linux canvas CI requires
// it explicitly, while ordinary canvas unit tests do not need a GPU SDK.
fn test_glsl_all_canvas_variants_compile_and_link() {
	validator := os.find_abs_path_of_executable('glslangValidator') or {
		assert os.getenv('V_CANVAS_REQUIRE_GLSLANG') != '1', 'glslangValidator is required by Linux canvas CI'
		return
	}
	dir := os.join_path(os.temp_dir(), 'v_canvas_glsl_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	for c in glsl_shader_cases() {
		vert := os.join_path(dir, '${c.name}.vert')
		frag := os.join_path(dir, '${c.name}.frag')
		os.write_file(vert, glsl_shader_source(c.vertex, true)!)!
		os.write_file(frag, if writes_color(c.step) {
			glsl_shader_source(c.fragment, false)!
		} else {
			glsl_depth_fragment_source
		})!
		result := os.exec([validator, '-l', '-q', '--reflect-all-block-variables', vert, frag])
		assert result.exit_code == 0, '${c.name}: ${result.output}'
		// Compare the independent frontend's actual std430 record strides
		// with the bytes uploaded by the canvas, not just our layout parser.
		mut reflected := 0
		for line in result.output.split_into_lines() {
			if !line.contains('topLevelArrayStride ') { continue }
			stride := line.all_after('topLevelArrayStride ').all_before(',').int()
			assert stride == step_uniform_size(c.step, c.clipped, c.shading), '${c.name}: ${line}'
			reflected++
		}
		assert (reflected > 0) == has_uniforms(c.step), c.name
	}
	vs, fs := glsl_load_msaa_sources()
	vert := os.join_path(dir, 'load_msaa.vert')
	frag := os.join_path(dir, 'load_msaa.frag')
	os.write_file(vert, vs)!
	os.write_file(frag, fs)!
	result := os.exec([validator, '-l', vert, frag])
	assert result.exit_code == 0, result.output
}

fn test_glsl_rejects_unsupported_storage_and_interface_changes() {
	vs, _ := solid_shader_sources(.analytic_rrect, false)
	for invalid in [vs.replace('float4 color_1;', 'half4 color_1;'),
		vs.replace('float4 color_1;', 'float3 color_1;'),
		vs.replace('float4 color_1;', 'float4 color_1 [[align(32)]];'),
		vs.replace('#include <metal_stdlib>', '#include <unsupported_shader_header>'),
		vs.replace('[[attribute(0)]]', '[[unsupported_attribute(0)]]'),
		vs.replace('[[user(locn0) center_no_perspective]]', '[[user(locn0) unsupported_interpolation]]'),
		vs.replace('thread float2&', 'thread float3&')] {
		if _ := glsl_shader_source(invalid, true) {
			assert false, 'unsupported shader source was accepted'
		} else {
			assert err.msg().contains('unsupported'), err.msg()
		}
	}
}

fn test_glsl_vector_relations_preserve_componentwise_comparisons() {
	assert glsl_vector_comparisons('all((a.xy <= b.zw)) && any(a != b)')! == 'all(lessThanEqual(a.xy, b.zw)) && any(notEqual(a, b))'
	assert glsl_vector_comparisons('select(vec2(x), y, (z >= vec2(-0.5)))')! == 'mix(vec2(x), y, greaterThanEqual(z, vec2(-0.5)))'
}
