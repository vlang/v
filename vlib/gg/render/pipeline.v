module render

import sokol.gfx

pub const num_primitive_types = 6

pub struct Pipeline {
pub mut:
	id         int
	blend_mode BlendMode
	pips       [num_primitive_types]gfx.Pipeline
}

pub fn (ctx &Context) make_pipeline(desc gfx.PipelineDesc) Pipeline {
	mut st := ctx.st
	mut p := Pipeline{}
	if !gfx.is_valid() {
		p.id = st.next_pipeline_id
		st.next_pipeline_id++
		return p
	}
	p.id = st.next_pipeline_id
	st.next_pipeline_id++
	// determine blend mode from blend state
	if desc.colors[0].blend.enabled {
		if desc.colors[0].blend.src_factor_rgb == .src_alpha && desc.colors[0].blend.dst_factor_rgb == .one {
			p.blend_mode = .add
		} else if desc.colors[0].blend.src_factor_rgb == .dst_color {
			p.blend_mode = .multiply
		} else if desc.colors[0].blend.src_factor_rgb == .one_minus_dst_color {
			p.blend_mode = .screen
		} else {
			p.blend_mode = .alpha
		}
	} else {
		p.blend_mode = .none
	}

	for i in 0 .. num_primitive_types {
		mut d := desc
		d.shader = st.shader
		d.layout.buffers[0].stride = int(sizeof(Vertex))
		d.layout.attrs[0].offset = 0
		d.layout.attrs[0].format = .float3
		d.layout.attrs[0].buffer_index = 0
		d.layout.attrs[1].offset = 12
		d.layout.attrs[1].format = .float2
		d.layout.attrs[1].buffer_index = 0
		d.layout.attrs[2].offset = 20
		d.layout.attrs[2].format = .ubyte4n
		d.layout.attrs[2].buffer_index = 0
		d.layout.attrs[3].offset = 24
		d.layout.attrs[3].format = .float
		d.layout.attrs[3].buffer_index = 0
		d.index_type = .none
		d.sample_count = st.sample_count
		d.face_winding = st.face_winding
		if d.colors[0].write_mask == ._default {
			d.colors[0].write_mask = .rgb
		}
		match i {
			0 { d.primitive_type = .points }
			1 { d.primitive_type = .lines }
			2 { d.primitive_type = .line_strip }
			3 { d.primitive_type = .triangles }
			4 { d.primitive_type = .triangle_strip }
			5 { d.primitive_type = .triangle_strip }
			else {}
		}
		if i == 5 {
			// quads are emulated with triangles (sgl does the same), so no
			// separate pipeline object is created for them.
			p.pips[i] = p.pips[3]
		} else {
			p.pips[i] = gfx.make_pipeline(&d)
		}
	}
	return p
}

pub fn (ctx &Context) load_pipeline(p Pipeline) {
	mut st := ctx.st
	st.pip_stack[st.pip_tos] = p
}

pub fn (ctx &Context) load_default_pipeline() {
	mut st := ctx.st
	st.pip_stack[st.pip_tos] = st.def_pip
}

pub fn (ctx &Context) push_pipeline() {
	mut st := ctx.st
	if st.pip_tos < max_stack_depth - 1 {
		st.pip_tos++
		st.pip_stack[st.pip_tos] = st.pip_stack[st.pip_tos - 1]
	} else {
		st.error = .stack_overflow
	}
}

pub fn (ctx &Context) pop_pipeline() {
	mut st := ctx.st
	if st.pip_tos > 0 {
		st.pip_tos--
	} else {
		st.error = .stack_underflow
	}
}

pub fn (ctx &Context) destroy_pipeline(p Pipeline) {
	for i in 0 .. num_primitive_types {
		if i != 5 {
			mut pip := p.pips[i]
			pip.free()
		}
	}
}
