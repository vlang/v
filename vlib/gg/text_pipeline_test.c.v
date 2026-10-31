// vtest build: !msvc
module gg

import fontstash
import gg.render
import sokol.gfx

fn test_font_pipeline_restores_default_and_custom_selections() {
	fons := fontstash.create_internal(&C.FONSparams{
		width:  64
		height: 64
	})
	assert fons != unsafe { nil }
	defer {
		fontstash.delete_internal(fons)
	}
	font := $embed_file('@VEXEROOT/examples/assets/fonts/RobotoMono-Regular.ttf')
	font_id := fons.add_font_mem('pipeline-test', font.to_bytes().clone(), true)
	assert font_id != fontstash.invalid
	mut font_pipeline := render.Pipeline{
		id:         80
		blend_mode: .alpha
	}
	for i in 0 .. render.num_primitive_types {
		font_pipeline.pips[i] = gfx.Pipeline{ id: u32(800 + i) }
	}
	ctx := &Context{
		r:           render.new_context(max_vertices: 64, max_commands: 16)
		pipeline:    &PipelineContainer{
			font:  font_pipeline
			alpha: render.Pipeline{ id: 90 }
		}
		ft:          &FT{
			fons:        fons
			font_normal: font_id
		}
		font_inited: true
	}
	mut target := FonsRenderTarget{
		ctx:       ctx
		atlas_img: gfx.Image{ id: 900 }
		atlas_smp: gfx.Sampler{ id: 901 }
	}
	verts := [f32(0), 0, 1, 0, 0, 1]!
	coords := [f32(0), 0, 1, 0, 0, 1]!
	colors := [u32(0xffffffff), 0xffffffff, 0xffffffff]!
	for depth in [0, 2, render.max_stack_depth - 1] {
		for _ in 0 .. depth {
			ctx.r.push_pipeline()
		}
		for custom in [false, true] {
			ctx.r.load_default_pipeline()
			if custom {
				mut pipeline := font_pipeline
				pipeline.id = 42
				pipeline.blend_mode = .multiply
				pipeline.pips[3] = gfx.Pipeline{ id: 403 }
				ctx.r.load_pipeline(pipeline)
			}
			selected := ctx.r.current_pipeline()
			for alpha in [u8(255), 128] {
				ctx.r.rewind()
				ctx.set_text_cfg(color: Color{ r: 255, a: alpha })
				assert ctx.r.current_pipeline() == selected
				fons_render_draw(&target, &verts[0], &coords[0], &colors[0], 3)
				assert ctx.r.current_pipeline() == selected
				ctx.r.begin_triangles()
				ctx.r.v2f(0, 0)
				ctx.r.v2f(1, 0)
				ctx.r.v2f(0, 1)
				ctx.r.end()
				assert ctx.r.current_pipeline() == selected
				assert ctx.r.num_commands() == 2
				assert ctx.r.error() == .no_error
			}
		}
		for _ in 0 .. depth {
			ctx.r.pop_pipeline()
		}
		assert ctx.r.error() == .no_error
	}
}

fn test_translucent_dashed_line_uses_alpha_for_every_batch() {
	mut alpha := render.Pipeline{
		id:         50
		blend_mode: .alpha
	}
	for i in 0 .. render.num_primitive_types {
		alpha.pips[i] = gfx.Pipeline{ id: u32(500 + i) }
	}
	ctx := &Context{
		r:        render.new_context(max_vertices: 64, max_commands: 16)
		pipeline: &PipelineContainer{ alpha: alpha }
	}
	ctx.r.matrix_mode_projection()
	ctx.r.ortho(0, 32, 8, 0, -1, 1)
	ctx.draw_line_with_config(1, 1, 25, 1, PenConfig{
		color:     Color{ r: 255, a: 128 }
		thickness: 2
		line_type: .dashed
	})
	assert ctx.r.num_vertices() == 12
	assert ctx.r.num_commands() == 1
	assert ctx.r.current_pipeline() == alpha
	frame := ctx.r.render_cpu(32, 8, 0xffff0000)
	assert frame.pixels[2 * 32 + 2] != 0xffff0000
	assert frame.pixels[2 * 32 + 2] == frame.pixels[2 * 32 + 14]
	assert frame.pixels[2 * 32 + 8] == 0xffff0000
}
