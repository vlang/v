module render

import sokol.gfx

pub enum CommandKind {
	draw
	viewport
	scissor
}

pub struct Command {
pub mut:
	kind            CommandKind
	primitive       Primitive
	base_vertex     int
	num_vertices    int
	pipeline        gfx.Pipeline
	image           gfx.Image
	sampler         gfx.Sampler
	uniform_index   int
	blend_additive  bool
	x               int
	y               int
	w               int
	h               int
	origin_top_left bool
	layer_id        int
}

pub struct Uniform {
pub mut:
	mvp Mat4
	tm  Mat4
}
