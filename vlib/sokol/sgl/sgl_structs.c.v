module sgl

import sokol.gfx

@[typedef]
pub struct C.sgl_pipeline {
	id u32
}

pub type Pipeline = C.sgl_pipeline

@[typedef]
pub struct C.sgl_context {
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

@[typedef]
pub struct C.sgl_context_desc_t {
	max_vertices i32             // default: 64k
	max_commands i32             // default: 16k
	color_format gfx.PixelFormat // C.sg_pixel_format
	depth_format gfx.PixelFormat // C.sg_pixel_format
	sample_count i32
}

pub type Desc = C.sgl_desc_t

@[typedef]
pub struct C.sgl_desc_t {
pub:
	max_vertices       i32             // size for vertex buffer
	max_commands       i32             // size of uniform- and command-buffers
	context_pool_size  i32             // max number of contexts (including default context), default: 4
	pipeline_pool_size i32             // size of internal pipeline pool, default: 64
	color_format       gfx.PixelFormat // C.sg_pixel_format
	depth_format       gfx.PixelFormat // C.sg_pixel_format
	sample_count       i32
	face_winding       gfx.FaceWinding // C.sg_face_winding // default front face winding is CCW
pub mut:
	allocator C.sgl_allocator_t // optional memory allocation overrides (default: malloc/free)
	logger    C.sgl_logger_t    // optional memory allocation overrides (default: SOKOL_LOG(message))
}
