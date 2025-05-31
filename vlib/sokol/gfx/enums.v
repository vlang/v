module gfx

pub enum Backend {
	glcore33
	gles3
	d3d11
	metal_ios
	metal_macos
	metal_simulator
	wgpu
	dummy
}

// PixelFormat is C.sg_pixel_format
pub enum PixelFormat as u32 {
	_default // value 0 reserved for default-init
	none

	r8
	r8sn
	r8ui
	r8si

	r16
	r16sn
	r16ui
	r16si
	r16f
	rg8
	rg8sn
	rg8ui
	rg8si

	r32ui
	r32si
	r32f
	rg16
	rg16sn
	rg16ui
	rg16si
	rg16f
	rgba8
	srgb8a8
	rgba8sn
	rgba8ui
	rgba8si
	bgra8
	rgb10a2
	rg11b10f
	rgb9e5

	rg32ui
	rg32si
	rg32f
	rgba16
	rgba16sn
	rgba16ui
	rgba16si
	rgba16f

	rgba32ui
	rgba32si
	rgba32f

	depth
	depth_stencil

	bc1_rgba
	bc2_rgba
	bc3_rgba
	bc3_srgba
	bc4_r
	bc4_rsn
	bc5_rg
	bc5_rgsn
	bc6h_rgbf
	bc6h_rgbuf
	bc7_rgba
	bc7_srgba
	pvrtc_rgb_2bpp  // deprecated
	pvrtc_rgb_4bpp  // deprecated
	pvrtc_rgba_2bpp // deprecated
	pvrtc_rgba_4bpp // deprecated
	etc2_rgb8
	etc2_srgb8
	etc2_rgb8a1
	etc2_rgba8
	etc2_srgb8a8
	eac_r11
	eac_r11sn
	eac_rg11
	eac_rg11sn
	astc_4x4_rgba
	astc_4x4_srgba

	_num
	_force_u32 = 0x7FFFFFFF
}

pub enum ResourceState as u32 {
	initial
	alloc
	valid
	failed
	invalid
	_force_u32 = 0x7FFFFFFF
}

pub enum Usage as u32 {
	_default // value 0 reserved for default-init
	immutable
	dynamic
	stream
	_num
	_force_u32 = 0x7FFFFFFF
}

pub enum BufferType as u32 {
	_default // value 0 reserved for default-init
	vertexbuffer
	indexbuffer
	storagebuffer
	_num
	_force_u32 = 0x7FFFFFFF
}

pub enum IndexType as u32 {
	_default // value 0 reserved for default-init
	none
	uint16
	uint32
	_num
	_force_u32 = 0x7FFFFFFF
}

pub enum ImageType as u32 {
	_default // value 0 reserved for default-init
	_2d
	cube
	_3d
	array
	_num
	_force_u32 = 0x7FFFFFFF
}

pub enum ImageSampleType as u32 {
	_default // value 0 reserved for default-init
	float
	depth
	sint
	uint
	unfilterable_float
	_num
	_force_u32 = 0x7FFFFFFF
}

pub enum SamplerType as u32 {
	_default
	filtering
	nonfiltering
	comparison
	_num
	_force_u32 = 0x7FFFFFFF
}

pub enum CubeFace as u32 {
	pos_x
	neg_x
	pos_y
	neg_y
	pos_z
	neg_z
	num
	_force_u32 = 0x7fffffff
}

pub enum ShaderStage as u32 {
	vs
	fs
	_force_u32 = 0x7FFFFFFF
}

pub enum PrimitiveType as u32 {
	_default // value 0 reserved for default-init
	points
	lines
	line_strip
	triangles
	triangle_strip
	_num
	_force_u32 = 0x7FFFFFFF
}

pub enum Filter as u32 {
	_default // value 0 reserved for default-init
	none
	nearest
	linear
	_num
	_force_u32 = 0x7fffffff
}

pub enum Wrap as u32 {
	_default // value 0 reserved for default-init
	repeat   // The default wrap mode.
	clamp_to_edge
	clamp_to_border // not supported on all backends and platforms. To check for support, call sg_query_features(), and check the "clamp_to_border" boolean in the returned sg_features struct. Platforms which don't support SG_WRAP_CLAMP_TO_BORDER will silently fall back to clamp_to_edge without a validation error.
	mirrored_repeat
	_num
	_force_u32 = 0x7FFFFFFF
}

pub enum BorderColor as u32 {
	_default // value 0 reserved for default-init
	transparent_black
	opaque_black
	opaque_white
	_num
	_force_u32 = 0x7FFFFFFF
}

pub enum VertexFormat as u32 {
	invalid
	float
	float2
	float3
	float4
	byte4
	byte4n // normalized
	ubyte4
	ubyte4n
	short2
	short2n
	ushort2n
	short4
	short4n
	ushort4n
	uint10_n2
	half2
	half4
	_num
	_force_u32 = 0x7FFFFFFF
}

pub enum VertexStep as u32 {
	_default // value 0 reserved for default-init
	per_vertex
	per_instance
	_num
	_force_u32 = 0x7FFFFFFF
}

pub enum UniformType as u32 {
	invalid
	float
	float2
	float3
	float4
	int
	int2
	int3
	int4
	mat4
	_num
	_force_u32 = 0x7FFFFFFF
}

pub enum CullMode as u32 {
	_default // value 0 reserved for default-init
	none
	front
	back
	_num
	_force_u32 = 0x7FFFFFFF
}

// FaceWindin is C.sg_face_winding
pub enum FaceWinding as u32 {
	default // value 0 reserved for default-init
	ccw
	cw
	num
	force_u32 = 0x7FFFFFFF
}

pub enum CompareFunc as u32 {
	_default // value 0 reserved for default-init
	never
	less
	equal
	less_equal
	greater
	not_equal
	greater_equal
	always
	_num
	_force_u32 = 0x7FFFFFFF
}

pub enum StencilOp as u32 {
	_default // value 0 reserved for default-init
	keep
	zero
	replace
	incr_clamp
	decr_clamp
	invert
	incr_wrap
	decr_wrap
	_num
	_force_u32 = 0x7FFFFFFF
}

pub enum BlendFactor as u32 {
	_default // value 0 reserved for default-init
	zero
	one
	src_color
	one_minus_src_color
	src_alpha
	one_minus_src_alpha
	dst_color
	one_minus_dst_color
	dst_alpha
	one_minus_dst_alpha
	src_alpha_saturated
	blend_color
	one_minus_blend_color
	blend_alpha
	one_minus_blend_alpha
	_num
	_force_u32 = 0x7FFFFFFF
}

pub enum BlendOp as u32 {
	_default // value 0 reserved for default-init
	add
	subtract
	reverse_subtract
	_num
	_force_u32 = 0x7FFFFFFF
}

pub enum ColorMask as u32 {
	_default   = 0    // value 0 reserved for default-init
	none       = 0x10 // special value for 'all channels disabled
	r          = 1
	g          = 2
	rg         = 3
	b          = 4
	rb         = 5
	gb         = 6
	rgb        = 7
	a          = 8
	ra         = 9
	ga         = 0xa
	rga        = 0xb
	ba         = 0xc
	rba        = 0xd
	gba        = 0xe
	rgba       = 0xf
	_force_u32 = 0x7FFFFFFF
}

pub enum LoadAction as u32 {
	_default
	clear
	load
	dontcare
	_force_u32 = 0x7FFFFFFF
}

pub enum StoreAction as u32 {
	_default
	store
	dontcare
	_force_u32 = 0x7FFFFFFF
}

pub enum UniformLayout as u32 {
	uniformlayout_default = 0                  // value 0 reserved for default-init
	uniformlayout_native // default: layout depends on currently active backend
	uniformlayout_std140 // std140: memory layout according to std140
	_num
	_force_u32            = 0x7FFFFFFF
}
