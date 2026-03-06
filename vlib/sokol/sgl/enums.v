module sgl

pub enum SglError {
	no_error
	vertices_full
	uniforms_full
	commands_full
	stack_overflow
	stack_underflow
	no_context
}

enum PrimitiveType {
	points
	lines
	line_strip
	triangles
	triangle_strip
	quads
}

const num_primitive_types = 6

enum CommandType {
	draw
	viewport
	scissor_rect
}

enum MatrixMode {
	modelview
	projection
	texture
}

const num_matrixmodes = 3
