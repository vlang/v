// Wrapper around 2d context and WebGL APIs

module ctx

pub struct ContextAttributes {
pub:
	alpha          bool
	desynchronized bool
}

pub enum PowerPreference {
	default_
	high_performance
	low_performance
}

pub struct WebGLAttributes {
pub:
	alpha                     bool
	desynchronized            bool
	antialias                 bool
	depth                     bool
	fail_if_major_perf_caveat bool
	power_preference          PowerPreference
	premultiplied_alpha       bool
	preserve_drawing_buffer   bool
	stencil                   bool
}

pub struct NoneContext {}

pub type ContextResult = CanvasRenderingContext2D | NoneContext | WebGLRenderingContext
