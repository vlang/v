// vtest build: macos
module gfx

#include "@DIR/image_transfer_metal_test.h"

fn C.v_sokol_transfer_test_device() voidptr

const transfer_width = 5
const transfer_height = 3

const transfer_shader = '#include <metal_stdlib>
using namespace metal;
struct In {
	float2 position [[attribute(0)]];
	uint code [[attribute(1)]];
	int offset [[attribute(2)]];
};
struct Out {
	float4 position [[position]];
	float4 color;
};
vertex Out vs_main(In in [[stage_in]]) {
	Out out;
	out.position = float4(in.position, 0.0, 1.0);
	uint code = in.code + uint(in.offset);
	out.color = float4(float(code & 255u), float((code >> 8) & 255u), float((code >> 16) & 255u), 255.0) / 255.0;
	return out;
}
fragment float4 fs_main(Out in [[stage_in]]) {
	return in.color;
}
'

struct TransferVertex {
	x      f32
	y      f32
	code   u32
	offset i32
}

fn setup_headless_metal() {
	mut desc := Desc{}
	desc.environment.defaults.color_format = .bgra8
	desc.environment.defaults.depth_format = .none
	desc.environment.defaults.sample_count = 1
	desc.environment.metal.device = C.v_sokol_transfer_test_device()
	setup(&desc)
	assert is_valid()
}

fn make_render_target(sample_count int) Image {
	mut desc := ImageDesc{
		render_target: true
		width:         transfer_width
		height:        transfer_height
		pixel_format:  .bgra8
		sample_count:  sample_count
	}
	return make_image(&desc)
}

fn test_image_transfers_and_integer_attributes() {
	setup_headless_metal()
	defer {
		shutdown()
	}
	image := make_render_target(1)
	assert query_image_state(image) == .valid
	// A padded source pitch exercises the row-by-row staging copy.
	pitch := transfer_width * 4 + 7
	mut source := []u8{len: pitch * transfer_height, init: u8(0xee)}
	for y in 0 .. transfer_height {
		for x in 0 .. transfer_width {
			for channel in 0 .. 4 {
				source[y * pitch + x * 4 + channel] = u8(x * 40 + y * 9 + channel * 3)
			}
		}
	}
	assert write_image(image, 0, 0, transfer_width, transfer_height, source.data, pitch)
	mut round_trip := []u8{len: pitch * transfer_height, init: u8(0x11)}
	assert read_image(image, 0, 0, transfer_width, transfer_height, round_trip.data, pitch)
	for y in 0 .. transfer_height {
		row := y * pitch
		assert round_trip[row..row + transfer_width * 4] == source[row..row + transfer_width * 4]
		// Row padding is not written.
		assert round_trip[row + transfer_width * 4..row + pitch] == []u8{len: 7, init: u8(0x11)}
	}
	// A sub-rectangle read.
	mut sub := []u8{len: 2 * 4 * 2}
	assert read_image(image, 2, 1, 2, 2, sub.data, 8)
	assert sub[0..8] == source[pitch + 8..pitch + 16]
	assert sub[8..16] == source[2 * pitch + 8..2 * pitch + 16]

	// Draw the left half with a color carried in uint and int attributes,
	// loading the uploaded pixels for the right half.
	mtl_set_shader_compile_options(2, 3, true)
	mut shader_desc := ShaderDesc{}
	shader_desc.vs.source = &char(transfer_shader.str)
	shader_desc.vs.entry = c'vs_main'
	shader_desc.fs.source = &char(transfer_shader.str)
	shader_desc.fs.entry = c'fs_main'
	shader := make_shader(&shader_desc)
	mtl_set_shader_compile_options(0, 0, false)
	assert query_shader_state(shader) == .valid
	mut pipeline_desc := PipelineDesc{
		shader:         shader
		primitive_type: .triangle_strip
	}
	pipeline_desc.layout.buffers[0].stride = int(sizeof(TransferVertex))
	pipeline_desc.layout.attrs[0] = VertexAttrDesc{
		format: .float2
		offset: 0
	}
	pipeline_desc.layout.attrs[1] = VertexAttrDesc{
		format: .uint
		offset: 8
	}
	pipeline_desc.layout.attrs[2] = VertexAttrDesc{
		format: .int
		offset: 12
	}
	pipeline_desc.colors[0].pixel_format = .bgra8
	pipeline_desc.depth.pixel_format = .none
	pipeline := make_pipeline(&pipeline_desc)
	assert query_pipeline_state(pipeline) == .valid
	// 0x00305a13 encodes red 0x13, green 0x5a, blue 0x30 after the -7 offset.
	vertices := [
		TransferVertex{-1, -1, 0x00305a1a, -7},
		TransferVertex{0, -1, 0x00305a1a, -7},
		TransferVertex{-1, 1, 0x00305a1a, -7},
		TransferVertex{0, 1, 0x00305a1a, -7},
	]
	buffer := make_buffer(&BufferDesc{
		data: Range{
			ptr:  vertices.data
			size: uint(vertices.len * int(sizeof(TransferVertex)))
		}
	})
	mut attachments_desc := AttachmentsDesc{}
	attachments_desc.colors[0].image = image
	attachments := make_attachments(&attachments_desc)
	mut pass := Pass{
		attachments: attachments
	}
	pass.action.colors[0].load_action = .load
	begin_pass(&pass)
	apply_pipeline(pipeline)
	mut bindings := Bindings{}
	bindings.vertex_buffers[0] = buffer
	apply_bindings(&bindings)
	draw(0, 4, 1)
	end_pass()
	mut drawn := []u8{len: transfer_width * 4 * transfer_height}
	assert read_image(image, 0, 0, transfer_width, transfer_height, drawn.data, transfer_width * 4)
	for y in 0 .. transfer_height {
		for x in 0 .. transfer_width {
			pixel := drawn[(y * transfer_width + x) * 4..(y * transfer_width + x + 1) * 4].clone()
			// The left half ends at x = 2.5: pixel centers 0.5 and 1.5 are
			// inside, 2.5 lies on the edge and 3.5 and 4.5 keep the upload.
			if x < 2 {
				assert pixel == [u8(0x30), 0x5a, 0x13, 0xff], '(${x}, ${y})'
			} else if x >= 3 {
				assert pixel == source[y * pitch + x * 4..y * pitch + x * 4 + 4].clone(), '(${x}, ${y})'
			}
		}
	}
	// Later passes in the same frame continue after a flush.
	assert flush()
	commit()

	// Unsupported requests fail without touching the destination.
	msaa := make_render_target(4)
	assert query_image_state(msaa) == .valid
	mut untouched := []u8{len: 16, init: u8(0x5c)}
	assert !read_image(msaa, 0, 0, 1, 1, untouched.data, 4)
	assert !read_image(image, 4, 0, 2, 1, untouched.data, 8)
	assert !read_image(image, 0, 0, 2, 1, untouched.data, 7)
	assert !write_image(image, -1, 0, 1, 1, untouched.data, 4)
	assert untouched == []u8{len: 16, init: u8(0x5c)}
}
