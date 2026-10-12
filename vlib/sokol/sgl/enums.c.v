module sgl

// SglError is C.sgl_error_t
pub enum SglError {
	no_error        = 0 // 0
	vertices_full   = 1
	uniforms_full   = 2
	commands_full   = 3
	stack_overflow  = 4
	stack_underflow = 5
	no_context      = 6
}
