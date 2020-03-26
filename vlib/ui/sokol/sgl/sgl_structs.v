module sgl

// should be in a proper module
pub enum SglError {
    no_error
	vertices_full
	commands_full
	stack_overflow
	stack_underfloat
}

pub struct C.sgl_pipeline {
	id u32
}

pub struct C.sgl_desc_t {
    max_vertices int       /* size for vertex buffer */
    max_commands int       /* size of uniform- and command-buffers */
    pipeline_pool_size int /* size of the internal pipeline pool, default is 64 */
    color_format C.sg_pixel_format
    depth_format C.sg_pixel_format
    sample_count int
    face_winding C.sg_face_winding /* default front face winding is CCW */
}
