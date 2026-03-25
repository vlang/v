module sgl

import sokol.gfx
import sokol.memory

// Public types

pub struct Pipeline {
pub:
	id u32
}

pub struct Context {
pub:
	id u32
}

pub struct ContextDesc {
pub mut:
	max_vertices int
	max_commands int
	color_format gfx.PixelFormat
	depth_format gfx.PixelFormat
	sample_count int
}

pub struct Allocator {
pub mut:
	alloc_fn  memory.FnAllocatorAlloc = unsafe { nil }
	free_fn   memory.FnAllocatorFree  = unsafe { nil }
	user_data voidptr
}

pub struct Logger {
pub mut:
	func      memory.FnLogCb = unsafe { nil }
	user_data voidptr
}

pub struct Desc {
pub mut:
	max_vertices       int
	max_commands       int
	context_pool_size  int
	pipeline_pool_size int
	color_format       gfx.PixelFormat
	depth_format       gfx.PixelFormat
	sample_count       int
	face_winding       gfx.FaceWinding
	allocator          Allocator
	logger             Logger
}

// Internal structs used by the pure V implementation.

struct Vertex {
mut:
	pos   [3]f32
	uv    [2]f32
	rgba  u32
	psize f32
}

struct Matrix {
mut:
	v [4][4]f32
}

struct Uniform {
mut:
	mvp Matrix
	tm  Matrix
}

struct DrawArgs {
mut:
	pip           gfx.Pipeline
	img           gfx.Image
	smp           gfx.Sampler
	base_vertex   int
	num_vertices  int
	uniform_index int
	max_vertices  int
}

struct ViewportArgs {
mut:
	x               int
	y               int
	w               int
	h               int
	origin_top_left bool
}

struct ScissorRectArgs {
mut:
	x               int
	y               int
	w               int
	h               int
	origin_top_left bool
}

union CommandArgs {
	draw         DrawArgs
	viewport     ViewportArgs
	scissor_rect ScissorRectArgs
}

struct Command {
mut:
	cmd      CommandType
	layer_id int
	args     CommandArgs
}

struct Slot {
mut:
	id    u32
	state gfx.ResourceState
}

struct Pool {
mut:
	size       int
	queue_top  int
	gen_ctrs   &u32 = unsafe { nil }
	free_queue &int = unsafe { nil }
}

struct PipelineInternal {
mut:
	slot Slot
	pip  [num_primitive_types]gfx.Pipeline
}

struct PipelinePool {
mut:
	pool Pool
	pips &PipelineInternal = unsafe { nil }
}

const max_stack_depth = 64

struct ContextInternal {
mut:
	slot            Slot
	desc            ContextDesc
	frame_id        u32
	update_frame_id u32
	// vertex buffer
	vertices_cap  int
	vertices_next int
	vertices_ptr  &Vertex = unsafe { nil }
	// uniform buffer
	uniforms_cap  int
	uniforms_next int
	uniforms_ptr  &Uniform = unsafe { nil }
	// command buffer
	commands_cap  int
	commands_next int
	commands_ptr  &Command = unsafe { nil }
	// state tracking
	base_vertex       int
	vtx_count         int
	error             SglError
	in_begin          bool
	layer_id          int
	u                 f32
	v                 f32
	rgba              u32
	point_size        f32
	cur_prim_type     PrimitiveType
	cur_img           gfx.Image
	cur_smp           gfx.Sampler
	texturing_enabled bool
	matrix_dirty      bool
	// sokol-gfx resources
	vbuf    gfx.Buffer
	def_pip Pipeline
	bind    gfx.Bindings
	// pipeline stack
	pip_tos   int
	pip_stack [max_stack_depth]Pipeline
	// matrix stacks
	cur_matrix_mode MatrixMode
	matrix_tos      [num_matrixmodes]int
	matrix_stack    [num_matrixmodes][max_stack_depth]Matrix
}

struct ContextPool {
mut:
	pool     Pool
	contexts &ContextInternal = unsafe { nil }
}

// Global state
struct SglState {
mut:
	init_cookie  u32
	desc         Desc
	def_img      gfx.Image
	def_smp      gfx.Sampler
	shd          gfx.Shader
	def_ctx_id   Context
	cur_ctx_id   Context
	cur_ctx      &ContextInternal = unsafe { nil }
	pip_pool     PipelinePool
	context_pool ContextPool
}

const invalid_slot_index = 0
const default_context_pool_size = 4
const default_pipeline_pool_size = 64
const default_max_vertices = (1 << 17)
const default_max_commands = (1 << 15)
// Large point batches can disappear on some backends/drivers once they exceed 16-bit counts.
const max_point_batch_vertices = 0xFFFF
const slot_shift = 16
const max_pool_size = (1 << slot_shift)
const slot_mask = (max_pool_size - 1)
const init_cookie = u32(0xABCDABCD)
