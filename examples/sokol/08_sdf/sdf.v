// A Signed Distance Field rendering demo, ported from https://github.com/floooh/sokol-samples/blob/master/sapp/sdf-sapp.c
// which in turn is based on https://iquilezles.org/articles/mandelbulb/ and https://www.shadertoy.com/view/ltfSWn
// vtest build: misc-tooling // needs .h files that are produced by `v shader`
import sokol.sapp
import sokol.gfx

#include "@VMODROOT/sdf.h" # It should be generated with `v shader .`

fn C.sdf_shader_desc(gfx.Backend) &gfx.ShaderDesc

@[packed]
struct C.vs_params_t {
mut:
	aspect f32
	time   f32
}

struct State {
mut:
	pip     gfx.Pipeline
	bind    gfx.Bindings
	paction gfx.PassAction
	params  C.vs_params_t
}

fn init(mut state State) {
	gfx.setup(sapp.create_desc())

	fsq_verts := [f32(-1.0), -3.0, 3.0, 1.0, -1.0, 1.0]!
	state.bind.vertex_buffers[0] = gfx.make_buffer(gfx.BufferDesc{
		label: c'fsq vertices'
		data:  gfx.Range{&fsq_verts[0], sizeof(fsq_verts)}
	})

	mut pipeline := gfx.PipelineDesc{}
	pipeline.layout.attrs[C.ATTR_vs_position].format = .float2
	pipeline.shader = gfx.make_shader(voidptr(C.sdf_shader_desc(gfx.query_backend())))
	state.pip = gfx.make_pipeline(&pipeline)

	// No need to clear the window, since the shader will overwrite the whole framebuffer
	state.paction.colors[0].load_action = .dontcare
}

fn frame(mut state State) {
	w, h := sapp.width(), sapp.height()
	state.params.time += f32(sapp.frame_duration())
	state.params.aspect = f32(w) / f32(h)
	gfx.begin_pass(sapp.create_default_pass(state.paction))
	gfx.apply_pipeline(state.pip)
	gfx.apply_bindings(state.bind)
	gfx.apply_uniforms(.vs, C.SLOT_vs_params, gfx.Range{&state.params, sizeof(state.params)})
	gfx.draw(0, 3, 1)
	gfx.end_pass()
	gfx.commit()
}

fn main() {
	sapp.run(sapp.Desc{
		window_title:      c'SDF Rendering'
		width:             512
		height:            512
		frame_userdata_cb: frame
		init_userdata_cb:  init
		cleanup_cb:        gfx.shutdown
		icon:              sapp.IconDesc{
			sokol_default: true
		}
		user_data:         &State{}
	})
}
