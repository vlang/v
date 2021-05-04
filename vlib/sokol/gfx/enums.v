module gfx

pub enum Backend {
    glcore33
    gles2
    gles3
    d3d11
    metal_ios
    metal_macos
    metal_simulator
    dummy
}

pub enum PixelFormat {
    _default    /* value 0 reserved for default-init */
    @none

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
    rgba8sn
    rgba8ui
    rgba8si
    bgra8
    rgb10a2
    rg11b10f

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
    bc4_r
    bc4_rsn
    bc5_rg
    bc5_rgsn
    bc6h_rgbf
    bc6h_rgbuf
    bc7_rgba
    pvrtc_rgb_2bpp
    pvrtc_rgb_4bpp
    pvrtc_rgba_2bpp
    pvrtc_rgba_4bpp
    etc2_rgb8
    etc2_rgb8a1
    etc2_rgba8
    etc2_rg11
    etc2_rg11sn

    _num
}

pub enum ResourceState {
    initial
    alloc
    valid
    failed
    invalid
}

pub enum Usage {
    _default      /* value 0 reserved for default-init */
    immutable
    dynamic
    stream
    _num
}

pub enum BufferType {
    _default         /* value 0 reserved for default-init */
    vertexbuffer
    indexbuffer
    _num
}

pub enum IndexType {
    _default   /* value 0 reserved for default-init */
    @none
    uint16
    uint32
    _num
}

pub enum ImageType {
    _default  /* value 0 reserved for default-init */
    _2d
    cube
    _3d
    array
    _num
}

pub enum CubeFace {
	pos_x
	neg_x
	pos_y
	neg_y
	pos_z
	neg_z
	num
	_force_u32 = 0x7fffffff
}

pub enum ShaderStage {
	vs
	fs
}

pub enum PrimitiveType {
    _default  /* value 0 reserved for default-init */
    points
    lines
    line_strip
    triangles
    triangle_strip
    _num
}

pub enum Filter {
    _default /* value 0 reserved for default-init */
    nearest
    linear
    nearest_mipmap_nearest
    nearest_mipmap_linear
    linear_mipmap_nearest
    linear_mipmap_linear
    _num
}

pub enum Wrap {
    _default   /* value 0 reserved for default-init */
    repeat
    clamp_to_edge
    clamp_to_border
    mirrored_repeat
    _num
}

pub enum BorderColor {
    _default    /* value 0 reserved for default-init */
    transparent_black
    opaque_black
    opaque_white
    _num
}

pub enum VertexFormat {
    invalid
    float
    float2
    float3
    float4
    byte4
    byte4n
    ubyte4
    ubyte4n
    short2
    short2n
    ushort2n
    short4
    short4n
    ushort4n
    uint10_n2
    _num
}

pub enum VertexStep {
    _default     /* value 0 reserved for default-init */
    per_vertex
    per_instance
    _num
}

pub enum UniformType {
    invalid
    float
    float2
    float3
    float4
    mat4
    _num
}

pub enum CullMode {
    _default   /* value 0 reserved for default-init */
    @none
    front
    back
    _num
}

pub enum FaceWinding {
    _facewinding_default    /* value 0 reserved for default-init */
    facewinding_ccw
    facewinding_cw
    _facewinding_num
}

pub enum CompareFunc {
    _default    /* value 0 reserved for default-init */
    never
    less
    equal
    less_equal
    greater
    not_equal
    greater_equal
    always
    _num
}

pub enum StencilOp {
    _default      /* value 0 reserved for default-init */
    keep
    zero
    replace
    incr_clamp
    decr_clamp
    invert
    incr_wrap
    decr_wrap
    _num
}

pub enum BlendFactor {
    _default    /* value 0 reserved for default-init */
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
}

pub enum BlendOp {
    _default    /* value 0 reserved for default-init */
    add
    subtract
    reverse_subtract
    _num
}

pub enum ColorMask {
    _default = 0      /* value 0 reserved for default-init */
    @none = 0x10     /* special value for 'all channels disabled */
    r = 1
    g = 2
    b = 4
    a = 8
    rgb = 0x7
    rgba = 0xF
}

pub enum Action {
    _default
    clear
    load
    dontcare
    _num
}
