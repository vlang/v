// BEAM Backend: Sokol SGL (immediate-mode) functions bridged to Erlang gl via vbeam_sokol.
// Codegen intercepts all sgl.* calls -> vbeam_sokol:sgl_* (runtime module).
// SGL immediate-mode calls map directly to OpenGL immediate-mode (gl:vertex, gl:color, etc.)
// for maximum performance — no gen_server round-trip needed.
// Stub bodies below are dead code — codegen emits direct calls to vbeam_sokol.
module sgl

import sokol.gfx
import sokol.memory

// ALL types must be defined here since this module has ZERO .v files.

pub const version = gfx.version + 1
pub const context = Context{0x00010001}

// --- Enums ---

pub enum SglError {
	no_error        = 0
	vertices_full   = 1
	uniforms_full   = 2
	commands_full   = 3
	stack_overflow  = 4
	stack_underflow = 5
	no_context      = 6
}

// --- Allocator and logger structs ---

pub struct SglAllocator {
pub mut:
	alloc_fn  memory.FnAllocatorAlloc = unsafe { nil }
	free_fn   memory.FnAllocatorFree  = unsafe { nil }
	user_data voidptr
}

pub struct SglLogger {
pub mut:
	func      memory.FnLogCb = unsafe { nil }
	user_data voidptr
}

// --- Structs ---

pub struct Pipeline {
	id u32
}

pub struct Context {
	id u32
}

pub struct ContextDesc {
	max_vertices int
	max_commands int
	color_format gfx.PixelFormat
	depth_format gfx.PixelFormat
	sample_count int
}

pub struct Desc {
pub:
	max_vertices       int
	max_commands       int
	context_pool_size  int
	pipeline_pool_size int
	color_format       gfx.PixelFormat
	depth_format       gfx.PixelFormat
	sample_count       int
	face_winding       gfx.FaceWinding
pub mut:
	allocator SglAllocator
	logger    SglLogger
}

// --- Functions ---

pub fn setup(desc &Desc) {
}

pub fn shutdown() {
}

pub fn error() SglError {
	return .no_error
}

pub fn context_error(ctx Context) SglError {
	return .no_error
}

pub fn rad(deg f32) f32 {
	return deg * 0.01745329252
}

pub fn deg(r f32) f32 {
	return r * 57.2957795131
}

// context functions
pub fn make_context(desc &ContextDesc) Context {
	return Context{}
}

pub fn destroy_context(ctx Context) {
}

pub fn set_context(ctx Context) {
}

pub fn get_context() Context {
	return Context{}
}

pub fn default_context() Context {
	return Context{0x00010001}
}

// pipeline functions
pub fn make_pipeline(desc &gfx.PipelineDesc) Pipeline {
	return Pipeline{}
}

pub fn context_make_pipeline(ctx Context, desc &gfx.PipelineDesc) Pipeline {
	return Pipeline{}
}

pub fn destroy_pipeline(pip Pipeline) {
}

// render state functions
pub fn defaults() {
}

pub fn viewport(x int, y int, w int, h int, origin_top_left bool) {
}

pub fn scissor_rect(x int, y int, w int, h int, origin_top_left bool) {
}

pub fn scissor_rectf(x f32, y f32, w f32, h f32, origin_top_left bool) {
}

pub fn enable_texture() {
}

pub fn disable_texture() {
}

pub fn texture(img gfx.Image, smp gfx.Sampler) {
}

// pipeline stack functions
pub fn load_default_pipeline() {
}

pub fn default_pipeline() {
}

pub fn load_pipeline(pip Pipeline) {
}

pub fn push_pipeline() {
}

pub fn pop_pipeline() {
}

// matrix stack functions
pub fn matrix_mode_modelview() {
}

pub fn matrix_mode_projection() {
}

pub fn matrix_mode_texture() {
}

pub fn load_identity() {
}

pub fn load_matrix(m []f32) {
}

pub fn load_transpose_matrix(m []f32) {
}

pub fn mult_matrix(m []f32) {
}

pub fn mult_transpose_matrix(m []f32) {
}

pub fn rotate(angle_rad f32, x f32, y f32, z f32) {
}

pub fn scale(x f32, y f32, z f32) {
}

pub fn translate(x f32, y f32, z f32) {
}

pub fn frustum(l f32, r f32, b f32, t f32, n f32, f f32) {
}

pub fn ortho(l f32, r f32, b f32, t f32, n f32, f f32) {
}

pub fn perspective(fov_y f32, aspect f32, z_near f32, z_far f32) {
}

pub fn lookat(eye_x f32, eye_y f32, eye_z f32, center_x f32, center_y f32, center_z f32, up_x f32, up_y f32,
	up_z f32) {
}

pub fn push_matrix() {
}

pub fn pop_matrix() {
}

// texcoord / color functions
pub fn t2f(u f32, v f32) {
}

pub fn c3f(r f32, g f32, b f32) {
}

pub fn c4f(r f32, g f32, b f32, a f32) {
}

pub fn c3b(r u8, g u8, b u8) {
}

pub fn c4b(r u8, g u8, b u8, a u8) {
}

pub fn c1i(rgba u32) {
}

pub fn point_size(s f32) {
}

// primitive functions
pub fn begin_points() {
}

pub fn begin_lines() {
}

pub fn begin_line_strip() {
}

pub fn begin_triangles() {
}

pub fn begin_triangle_strip() {
}

pub fn begin_quads() {
}

pub fn v2f(x f32, y f32) {
}

pub fn v3f(x f32, y f32, z f32) {
}

pub fn v2f_t2f(x f32, y f32, u f32, v f32) {
}

pub fn v3f_t2f(x f32, y f32, z f32, u f32, v f32) {
}

pub fn v2f_c3f(x f32, y f32, r f32, g f32, b f32) {
}

pub fn v2f_c3b(x f32, y f32, r u8, g u8, b u8) {
}

pub fn v2f_c4f(x f32, y f32, r f32, g f32, b f32, a f32) {
}

pub fn v2f_c4b(x f32, y f32, r u8, g u8, b u8, a u8) {
}

pub fn v2f_c1i(x f32, y f32, rgba u32) {
}

pub fn v3f_c3f(x f32, y f32, z f32, r f32, g f32, b f32) {
}

pub fn v3f_c3b(x f32, y f32, z f32, r u8, g u8, b u8) {
}

pub fn v3f_c4f(x f32, y f32, z f32, r f32, g f32, b f32, a f32) {
}

pub fn v3f_c4b(x f32, y f32, z f32, r u8, g u8, b u8, a u8) {
}

pub fn v3f_c1i(x f32, y f32, z f32, rgba u32) {
}

pub fn v2f_t2f_c3f(x f32, y f32, u f32, v f32, r f32, g f32, b f32) {
}

pub fn v2f_t2f_c3b(x f32, y f32, u f32, v f32, r u8, g u8, b u8) {
}

pub fn v2f_t2f_c4f(x f32, y f32, u f32, v f32, r f32, g f32, b f32, a f32) {
}

pub fn v2f_t2f_c4b(x f32, y f32, u f32, v f32, r u8, g u8, b u8, a u8) {
}

pub fn v2f_t2f_c1i(x f32, y f32, u f32, v f32, rgba u32) {
}

pub fn v3f_t2f_c3f(x f32, y f32, z f32, u f32, v f32, r f32, g f32, b f32) {
}

pub fn v3f_t2f_c3b(x f32, y f32, z f32, u f32, v f32, r u8, g u8, b u8) {
}

pub fn v3f_t2f_c4f(x f32, y f32, z f32, u f32, v f32, r f32, g f32, b f32, a f32) {
}

pub fn v3f_t2f_c4b(x f32, y f32, z f32, u f32, v f32, r u8, g u8, b u8, a u8) {
}

pub fn v3f_t2f_c1i(x f32, y f32, z f32, u f32, v f32, rgba u32) {
}

pub fn end() {
}

// render recorded commands
pub fn draw() {
}

pub fn context_draw(ctx Context) {
}
