module sgl

/* setup/shutdown/misc */
fn C.sgl_setup(desc &C.sgl_desc_t)
fn C.sgl_shutdown()
fn C.sgl_error() C.sgl_error_t
fn C.sgl_defaults()
fn C.sgl_rad(deg f32) f32
fn C.sgl_deg(rad f32) f32

/* create and destroy pipeline objects */
fn C.sgl_make_pipeline(desc &C.sg_pipeline_desc) C.sgl_pipeline
fn C.sgl_destroy_pipeline(pip C.sgl_pipeline)

/* render state functions */
fn C.sgl_viewport(x int, y int, w int, h int, origin_top_left bool)
fn C.sgl_viewportf(x f32, y f32, w f32, h f32, origin_top_left bool)
fn C.sgl_scissor_rect(x int, y int, w int, h int, origin_top_left bool)
fn C.sgl_scissor_rectf(x f32, y f32, w f32, h f32, origin_top_left bool)
fn C.sgl_enable_texture()
fn C.sgl_disable_texture()
fn C.sgl_texture(img C.sg_image)

/* pipeline stack functions */
fn C.sgl_default_pipeline()
fn C.sgl_load_pipeline(pip C.sgl_pipeline)
fn C.sgl_push_pipeline()
fn C.sgl_pop_pipeline()

/* matrix stack functions */
fn C.sgl_matrix_mode_modelview()
fn C.sgl_matrix_mode_projection()
fn C.sgl_matrix_mode_texture()
fn C.sgl_load_identity()
fn C.sgl_load_matrix(m [16]f32)
fn C.sgl_load_transpose_matrix(m [16]f32)
fn C.sgl_mult_matrix(m [16]f32)
fn C.sgl_mult_transpose_matrix(m [16]f32)
fn C.sgl_rotate(angle_rad f32, x f32, y f32, z f32)
fn C.sgl_scale(x f32, y f32, z f32)
fn C.sgl_translate(x f32, y f32, z f32)
fn C.sgl_frustum(l f32, r f32, b f32, t f32, n f32, f f32)
fn C.sgl_ortho(l f32, r f32, b f32, t f32, n f32, f f32)
fn C.sgl_perspective(fov_y f32, aspect f32, z_near f32, z_far f32)
fn C.sgl_lookat(eye_x f32, eye_y f32, eye_z f32, center_x f32, center_y f32, center_z f32, up_x f32, up_y f32, up_z f32)
fn C.sgl_push_matrix()
fn C.sgl_pop_matrix()

/* these functions only set the internal 'current texcoord / color' (valid inside or outside begin/end) */
fn C.sgl_t2f(u f32, v f32)
fn C.sgl_c3f(r f32, g f32, b f32)
fn C.sgl_c4f(r f32, g f32, b f32, a f32)
fn C.sgl_c3b(r byte, g byte, b byte)
fn C.sgl_c4b(r byte, g byte, b byte, a byte)
fn C.sgl_c1i(rgba u32)

/* define primitives, each begin/end is one draw command */
fn C.sgl_begin_points()
fn C.sgl_begin_lines()
fn C.sgl_begin_line_strip()
fn C.sgl_begin_triangles()
fn C.sgl_begin_triangle_strip()
fn C.sgl_begin_quads()
fn C.sgl_v2f(x f32, y f32)
fn C.sgl_v3f(x f32, y f32, z f32)
fn C.sgl_v2f_t2f(x f32, y f32, u f32, v f32)
fn C.sgl_v3f_t2f(x f32, y f32, z f32, u f32, v f32)
fn C.sgl_v2f_c3f(x f32, y f32, r f32, g f32, b f32)
fn C.sgl_v2f_c3b(x f32, y f32, r byte, g byte, b byte)
fn C.sgl_v2f_c4f(x f32, y f32, r f32, g f32, b f32, a f32)
fn C.sgl_v2f_c4b(x f32, y f32, r byte, g byte, b byte, a byte)
fn C.sgl_v2f_c1i(x f32, y f32, rgba u32)
fn C.sgl_v3f_c3f(x f32, y f32, z f32, r f32, g f32, b f32)
fn C.sgl_v3f_c3b(x f32, y f32, z f32, r byte, g byte, b byte)
fn C.sgl_v3f_c4f(x f32, y f32, z f32, r f32, g f32, b f32, a f32)
fn C.sgl_v3f_c4b(x f32, y f32, z f32, r byte, g byte, b byte, a byte)
fn C.sgl_v3f_c1i(x f32, y f32, z f32, rgba u32)
fn C.sgl_v2f_t2f_c3f(x f32, y f32, u f32, v f32, r f32, g f32, b f32)
fn C.sgl_v2f_t2f_c3b(x f32, y f32, u f32, v f32, r byte, g byte, b byte)
fn C.sgl_v2f_t2f_c4f(x f32, y f32, u f32, v f32, r f32, g f32, b f32, a f32)
fn C.sgl_v2f_t2f_c4b(x f32, y f32, u f32, v f32, r byte, g byte, b byte, a byte)
fn C.sgl_v2f_t2f_c1i(x f32, y f32, u f32, v f32, rgba u32)
fn C.sgl_v3f_t2f_c3f(x f32, y f32, z f32, u f32, v f32, r f32, g f32, b f32)
fn C.sgl_v3f_t2f_c3b(x f32, y f32, z f32, u f32, v f32, r byte, g byte, b byte)
fn C.sgl_v3f_t2f_c4f(x f32, y f32, z f32, u f32, v f32, r f32, g f32, b f32, a f32)
fn C.sgl_v3f_t2f_c4b(x f32, y f32, z f32, u f32, v f32, r byte, g byte, b byte, a byte)
fn C.sgl_v3f_t2f_c1i(x f32, y f32, z f32, u f32, v f32, rgba u32)
fn C.sgl_end()

/* render everything */
fn C.sgl_draw()
