module sgl

// SglError is C.sgl_error_t
pub enum SglError {
	no_error        = C.SGL_NO_ERROR // 0
	vertices_full   = C.SGL_ERROR_VERTICES_FULL
	uniforms_full   = C.SGL_ERROR_UNIFORMS_FULL
	commands_full   = C.SGL_ERROR_COMMANDS_FULL
	stack_overflow  = C.SGL_ERROR_STACK_OVERFLOW
	stack_underflow = C.SGL_ERROR_STACK_UNDERFLOW
	no_context      = C.SGL_ERROR_NO_CONTEXT
}
