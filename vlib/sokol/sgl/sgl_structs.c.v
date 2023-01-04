module sgl

import sokol.gfx

[typedef]
struct C.sgl_pipeline {
	id u32
}

pub type Pipeline = C.sgl_pipeline

[typedef]
struct C.sgl_context {
	id u32
}

pub type Context = C.sgl_context

// ContextDesc
//
// Describes the initialization parameters of a rendering context.
// Creating additional contexts is useful if you want to render
// in separate sokol-gfx passes.
// ContextDesc is sgl_context_desc_t
pub type ContextDesc = C.sgl_context_desc_t

[typedef]
struct C.sgl_context_desc_t {
	max_vertices int // default: 64k
	max_commands int // default: 16k
	color_format gfx.PixelFormat // C.sg_pixel_format
	depth_format gfx.PixelFormat // C.sg_pixel_format
	sample_count int
}

pub type Desc = C.sgl_desc_t

[typedef]
struct C.sgl_desc_t {
	max_vertices       int // size for vertex buffer
	max_commands       int // size of uniform- and command-buffers
	context_pool_size  int // max number of contexts (including default context), default: 4
	pipeline_pool_size int // size of internal pipeline pool, default: 64
	color_format       gfx.PixelFormat // C.sg_pixel_format
	depth_format       gfx.PixelFormat // C.sg_pixel_format
	sample_count       int
	face_winding       gfx.FaceWinding // C.sg_face_winding // default front face winding is CCW
}
