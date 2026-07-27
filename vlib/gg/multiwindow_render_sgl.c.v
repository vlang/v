module gg

$if gg_multiwindow ? {
	import sokol.sgl
}

fn multiwindow_sgl_api_guard() {
	$if !gg_multiwindow ? {
		panic(multiwindow_render_unavailable_message())
	}
}

// defaults restores SGL's default drawing state for this window context.
pub fn (mut context WindowSglContext) defaults() {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.defaults()
	}
}

// viewport sets the framebuffer viewport used by subsequent SGL drawing.
pub fn (mut context WindowSglContext) viewport(x int, y int, width int, height int, origin_top_left bool) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.viewport(x, y, width, height, origin_top_left)
	}
}

// scissor_rect sets an integer framebuffer scissor for subsequent SGL drawing.
pub fn (mut context WindowSglContext) scissor_rect(x int, y int, width int, height int, origin_top_left bool) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.scissor_rect(x, y, width, height, origin_top_left)
	}
}

// scissor_rectf sets a floating-point framebuffer scissor for subsequent SGL drawing.
pub fn (mut context WindowSglContext) scissor_rectf(x f32, y f32, width f32, height f32, origin_top_left bool) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.scissor_rectf(x, y, width, height, origin_top_left)
	}
}

// enable_texture enables sampling from the texture selected on this window context.
pub fn (mut context WindowSglContext) enable_texture() {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.enable_texture()
	}
}

// disable_texture disables texture sampling for subsequent SGL vertices.
pub fn (mut context WindowSglContext) disable_texture() {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.disable_texture()
	}
}

// load_default_pipeline selects SGL's built-in pipeline for subsequent drawing.
pub fn (mut context WindowSglContext) load_default_pipeline() {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.load_default_pipeline()
	}
}

// default_pipeline selects SGL's built-in pipeline through the legacy alias.
pub fn (mut context WindowSglContext) default_pipeline() {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.default_pipeline()
	}
}

// push_pipeline saves the current SGL pipeline selection on its stack.
pub fn (mut context WindowSglContext) push_pipeline() {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.push_pipeline()
	}
}

// pop_pipeline restores the previous SGL pipeline selection from its stack.
pub fn (mut context WindowSglContext) pop_pipeline() {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.pop_pipeline()
	}
}

// matrix_mode_modelview selects the model-view matrix stack for subsequent operations.
pub fn (mut context WindowSglContext) matrix_mode_modelview() {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.matrix_mode_modelview()
	}
}

// matrix_mode_projection selects the projection matrix stack for subsequent operations.
pub fn (mut context WindowSglContext) matrix_mode_projection() {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.matrix_mode_projection()
	}
}

// matrix_mode_texture selects the texture matrix stack for subsequent operations.
pub fn (mut context WindowSglContext) matrix_mode_texture() {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.matrix_mode_texture()
	}
}

// load_identity replaces the selected SGL matrix with the identity matrix.
pub fn (mut context WindowSglContext) load_identity() {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.load_identity()
	}
}

// load_matrix replaces the selected SGL matrix with a column-major matrix.
pub fn (mut context WindowSglContext) load_matrix(matrix []f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.load_matrix(matrix)
	}
}

// load_transpose_matrix replaces the selected SGL matrix with the transpose of `matrix`.
pub fn (mut context WindowSglContext) load_transpose_matrix(matrix []f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.load_transpose_matrix(matrix)
	}
}

// mult_matrix multiplies the selected SGL matrix by a column-major matrix.
pub fn (mut context WindowSglContext) mult_matrix(matrix []f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.mult_matrix(matrix)
	}
}

// mult_transpose_matrix multiplies the selected SGL matrix by the transpose of `matrix`.
pub fn (mut context WindowSglContext) mult_transpose_matrix(matrix []f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.mult_transpose_matrix(matrix)
	}
}

// rotate applies an axis-angle rotation to the selected SGL matrix.
pub fn (mut context WindowSglContext) rotate(angle_rad f32, x f32, y f32, z f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.rotate(angle_rad, x, y, z)
	}
}

// scale applies a three-axis scale to the selected SGL matrix.
pub fn (mut context WindowSglContext) scale(x f32, y f32, z f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.scale(x, y, z)
	}
}

// translate applies a three-axis translation to the selected SGL matrix.
pub fn (mut context WindowSglContext) translate(x f32, y f32, z f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.translate(x, y, z)
	}
}

// frustum multiplies the selected SGL matrix by a perspective frustum projection.
pub fn (mut context WindowSglContext) frustum(left f32, right f32, bottom f32, top f32, near_plane f32, far_plane f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.frustum(left, right, bottom, top, near_plane, far_plane)
	}
}

// ortho multiplies the selected SGL matrix by an orthographic projection.
pub fn (mut context WindowSglContext) ortho(left f32, right f32, bottom f32, top f32, near_plane f32, far_plane f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.ortho(left, right, bottom, top, near_plane, far_plane)
	}
}

// perspective multiplies the selected SGL matrix by a perspective projection.
pub fn (mut context WindowSglContext) perspective(fov_y f32, aspect f32, z_near f32, z_far f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.perspective(fov_y, aspect, z_near, z_far)
	}
}

// lookat multiplies the selected SGL matrix by a camera view transform.
pub fn (mut context WindowSglContext) lookat(eye_x f32, eye_y f32, eye_z f32, center_x f32, center_y f32, center_z f32, up_x f32, up_y f32, up_z f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.lookat(eye_x, eye_y, eye_z, center_x, center_y, center_z, up_x, up_y, up_z)
	}
}

// push_matrix saves the selected SGL matrix on its current stack.
pub fn (mut context WindowSglContext) push_matrix() {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.push_matrix()
	}
}

// pop_matrix restores the selected SGL matrix from its current stack.
pub fn (mut context WindowSglContext) pop_matrix() {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.pop_matrix()
	}
}

// t2f sets the texture coordinates attached to subsequently emitted vertices.
pub fn (mut context WindowSglContext) t2f(u f32, v f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.t2f(u, v)
	}
}

// c3f sets a floating-point RGB color for subsequently emitted vertices.
pub fn (mut context WindowSglContext) c3f(red f32, green f32, blue f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.c3f(red, green, blue)
	}
}

// c4f sets a floating-point RGBA color for subsequently emitted vertices.
pub fn (mut context WindowSglContext) c4f(red f32, green f32, blue f32, alpha f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.c4f(red, green, blue, alpha)
	}
}

// c3b sets a byte RGB color for subsequently emitted vertices.
pub fn (mut context WindowSglContext) c3b(red u8, green u8, blue u8) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.c3b(red, green, blue)
	}
}

// c4b sets a byte RGBA color for subsequently emitted vertices.
pub fn (mut context WindowSglContext) c4b(red u8, green u8, blue u8, alpha u8) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.c4b(red, green, blue, alpha)
	}
}

// c1i sets a packed RGBA color for subsequently emitted vertices.
pub fn (mut context WindowSglContext) c1i(rgba u32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.c1i(rgba)
	}
}

// point_size sets the rasterized size of subsequently emitted points.
pub fn (mut context WindowSglContext) point_size(size f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.point_size(size)
	}
}

// begin_points starts a point primitive batch that must be closed with `end`.
pub fn (mut context WindowSglContext) begin_points() {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.begin_points()
	}
}

// begin_lines starts an independent-line batch that must be closed with `end`.
pub fn (mut context WindowSglContext) begin_lines() {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.begin_lines()
	}
}

// begin_line_strip starts a connected-line batch that must be closed with `end`.
pub fn (mut context WindowSglContext) begin_line_strip() {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.begin_line_strip()
	}
}

// begin_triangles starts an independent-triangle batch that must be closed with `end`.
pub fn (mut context WindowSglContext) begin_triangles() {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.begin_triangles()
	}
}

// begin_triangle_strip starts a connected-triangle batch that must be closed with `end`.
pub fn (mut context WindowSglContext) begin_triangle_strip() {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.begin_triangle_strip()
	}
}

// begin_quads starts a quad batch that must be closed with `end`.
pub fn (mut context WindowSglContext) begin_quads() {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.begin_quads()
	}
}

// v2f emits a 2D vertex using the current color and texture coordinates.
pub fn (mut context WindowSglContext) v2f(x f32, y f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.v2f(x, y)
	}
}

// v3f emits a 3D vertex using the current color and texture coordinates.
pub fn (mut context WindowSglContext) v3f(x f32, y f32, z f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.v3f(x, y, z)
	}
}

// v2f_t2f emits a 2D vertex with explicit texture coordinates and the current color.
pub fn (mut context WindowSglContext) v2f_t2f(x f32, y f32, u f32, v f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.v2f_t2f(x, y, u, v)
	}
}

// v3f_t2f emits a 3D vertex with explicit texture coordinates and the current color.
pub fn (mut context WindowSglContext) v3f_t2f(x f32, y f32, z f32, u f32, v f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.v3f_t2f(x, y, z, u, v)
	}
}

// v2f_c3f emits a 2D vertex with a floating-point RGB color.
pub fn (mut context WindowSglContext) v2f_c3f(x f32, y f32, red f32, green f32, blue f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.v2f_c3f(x, y, red, green, blue)
	}
}

// v2f_c3b emits a 2D vertex with a byte RGB color.
pub fn (mut context WindowSglContext) v2f_c3b(x f32, y f32, red u8, green u8, blue u8) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.v2f_c3b(x, y, red, green, blue)
	}
}

// v2f_c4f emits a 2D vertex with a floating-point RGBA color.
pub fn (mut context WindowSglContext) v2f_c4f(x f32, y f32, red f32, green f32, blue f32, alpha f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.v2f_c4f(x, y, red, green, blue, alpha)
	}
}

// v2f_c4b emits a 2D vertex with a byte RGBA color.
pub fn (mut context WindowSglContext) v2f_c4b(x f32, y f32, red u8, green u8, blue u8, alpha u8) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.v2f_c4b(x, y, red, green, blue, alpha)
	}
}

// v2f_c1i emits a 2D vertex with a packed RGBA color.
pub fn (mut context WindowSglContext) v2f_c1i(x f32, y f32, rgba u32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.v2f_c1i(x, y, rgba)
	}
}

// v3f_c3f emits a 3D vertex with a floating-point RGB color.
pub fn (mut context WindowSglContext) v3f_c3f(x f32, y f32, z f32, red f32, green f32, blue f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.v3f_c3f(x, y, z, red, green, blue)
	}
}

// v3f_c3b emits a 3D vertex with a byte RGB color.
pub fn (mut context WindowSglContext) v3f_c3b(x f32, y f32, z f32, red u8, green u8, blue u8) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.v3f_c3b(x, y, z, red, green, blue)
	}
}

// v3f_c4f emits a 3D vertex with a floating-point RGBA color.
pub fn (mut context WindowSglContext) v3f_c4f(x f32, y f32, z f32, red f32, green f32, blue f32, alpha f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.v3f_c4f(x, y, z, red, green, blue, alpha)
	}
}

// v3f_c4b emits a 3D vertex with a byte RGBA color.
pub fn (mut context WindowSglContext) v3f_c4b(x f32, y f32, z f32, red u8, green u8, blue u8, alpha u8) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.v3f_c4b(x, y, z, red, green, blue, alpha)
	}
}

// v3f_c1i emits a 3D vertex with a packed RGBA color.
pub fn (mut context WindowSglContext) v3f_c1i(x f32, y f32, z f32, rgba u32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.v3f_c1i(x, y, z, rgba)
	}
}

// v2f_t2f_c3f emits a 2D vertex with texture coordinates and a floating-point RGB color.
pub fn (mut context WindowSglContext) v2f_t2f_c3f(x f32, y f32, u f32, v f32, red f32, green f32, blue f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.v2f_t2f_c3f(x, y, u, v, red, green, blue)
	}
}

// v2f_t2f_c3b emits a 2D vertex with texture coordinates and a byte RGB color.
pub fn (mut context WindowSglContext) v2f_t2f_c3b(x f32, y f32, u f32, v f32, red u8, green u8, blue u8) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.v2f_t2f_c3b(x, y, u, v, red, green, blue)
	}
}

// v2f_t2f_c4f emits a 2D vertex with texture coordinates and a floating-point RGBA color.
pub fn (mut context WindowSglContext) v2f_t2f_c4f(x f32, y f32, u f32, v f32, red f32, green f32, blue f32, alpha f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.v2f_t2f_c4f(x, y, u, v, red, green, blue, alpha)
	}
}

// v2f_t2f_c4b emits a 2D vertex with texture coordinates and a byte RGBA color.
pub fn (mut context WindowSglContext) v2f_t2f_c4b(x f32, y f32, u f32, v f32, red u8, green u8, blue u8, alpha u8) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.v2f_t2f_c4b(x, y, u, v, red, green, blue, alpha)
	}
}

// v2f_t2f_c1i emits a 2D vertex with texture coordinates and a packed RGBA color.
pub fn (mut context WindowSglContext) v2f_t2f_c1i(x f32, y f32, u f32, v f32, rgba u32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.v2f_t2f_c1i(x, y, u, v, rgba)
	}
}

// v3f_t2f_c3f emits a 3D vertex with texture coordinates and a floating-point RGB color.
pub fn (mut context WindowSglContext) v3f_t2f_c3f(x f32, y f32, z f32, u f32, v f32, red f32, green f32, blue f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.v3f_t2f_c3f(x, y, z, u, v, red, green, blue)
	}
}

// v3f_t2f_c3b emits a 3D vertex with texture coordinates and a byte RGB color.
pub fn (mut context WindowSglContext) v3f_t2f_c3b(x f32, y f32, z f32, u f32, v f32, red u8, green u8, blue u8) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.v3f_t2f_c3b(x, y, z, u, v, red, green, blue)
	}
}

// v3f_t2f_c4f emits a 3D vertex with texture coordinates and a floating-point RGBA color.
pub fn (mut context WindowSglContext) v3f_t2f_c4f(x f32, y f32, z f32, u f32, v f32, red f32, green f32, blue f32, alpha f32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.v3f_t2f_c4f(x, y, z, u, v, red, green, blue, alpha)
	}
}

// v3f_t2f_c4b emits a 3D vertex with texture coordinates and a byte RGBA color.
pub fn (mut context WindowSglContext) v3f_t2f_c4b(x f32, y f32, z f32, u f32, v f32, red u8, green u8, blue u8, alpha u8) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.v3f_t2f_c4b(x, y, z, u, v, red, green, blue, alpha)
	}
}

// v3f_t2f_c1i emits a 3D vertex with texture coordinates and a packed RGBA color.
pub fn (mut context WindowSglContext) v3f_t2f_c1i(x f32, y f32, z f32, u f32, v f32, rgba u32) {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.v3f_t2f_c1i(x, y, z, u, v, rgba)
	}
}

// end closes the current primitive batch and records it for this window's SGL pass.
pub fn (mut context WindowSglContext) end() {
	multiwindow_sgl_api_guard()
	$if gg_multiwindow ? {
		context.activate_sgl_managed_or_panic()
		sgl.end()
	}
}

// texture selects a window-owned image and sampler for subsequent textured primitives.
pub fn (mut context WindowSglContext) texture(image WindowImageId, sampler WindowSamplerId) ! {
	$if gg_multiwindow ? {
		context.texture_managed(image, sampler)!
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}

// load_pipeline selects a window-owned SGL pipeline for subsequent drawing.
pub fn (mut context WindowSglContext) load_pipeline(id WindowSglPipelineId) ! {
	$if gg_multiwindow ? {
		context.load_pipeline_managed(id)!
	} $else {
		return error(multiwindow_render_unavailable_message())
	}
}
