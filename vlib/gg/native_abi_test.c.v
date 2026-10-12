module gg

import sokol.gfx as _
import encoding.vorbis as _
import sokol.sapp as _
import sokol.sgl as _
import sokol.sfons as _
import sokol.audio as _

#flag darwin @VEXEROOT/thirdparty/sokol/graphics_abi_test.c

fn C.v_graphics_native_layout(typ i32, member i32) i64

fn test_native_layout_sg_desc() {
	$if macos {
		assert i64(sizeof(C.sg_desc)) == C.v_graphics_native_layout(0, -1), 'sg_desc size'
		assert i64(__offsetof(C.sg_desc, _start_canary)) == C.v_graphics_native_layout(0, 0), 'sg_desc._start_canary offset'
		assert i64(__offsetof(C.sg_desc, buffer_pool_size)) == C.v_graphics_native_layout(0, 1), 'sg_desc.buffer_pool_size offset'
		assert i64(__offsetof(C.sg_desc, image_pool_size)) == C.v_graphics_native_layout(0, 2), 'sg_desc.image_pool_size offset'
		assert i64(__offsetof(C.sg_desc, sampler_pool_size)) == C.v_graphics_native_layout(0, 3), 'sg_desc.sampler_pool_size offset'
		assert i64(__offsetof(C.sg_desc, shader_pool_size)) == C.v_graphics_native_layout(0, 4), 'sg_desc.shader_pool_size offset'
		assert i64(__offsetof(C.sg_desc, pipeline_pool_size)) == C.v_graphics_native_layout(0, 5), 'sg_desc.pipeline_pool_size offset'
		assert i64(__offsetof(C.sg_desc, attachments_pool_size)) == C.v_graphics_native_layout(0, 6), 'sg_desc.attachments_pool_size offset'
		assert i64(__offsetof(C.sg_desc, uniform_buffer_size)) == C.v_graphics_native_layout(0, 7), 'sg_desc.uniform_buffer_size offset'
		assert i64(__offsetof(C.sg_desc, max_commit_listeners)) == C.v_graphics_native_layout(0, 8), 'sg_desc.max_commit_listeners offset'
		assert i64(__offsetof(C.sg_desc, disable_validation)) == C.v_graphics_native_layout(0, 9), 'sg_desc.disable_validation offset'
		assert i64(__offsetof(C.sg_desc, mtl_force_managed_storage_mode)) == C.v_graphics_native_layout(0, 10), 'sg_desc.mtl_force_managed_storage_mode offset'
		assert i64(__offsetof(C.sg_desc, mtl_use_command_buffer_with_retained_references)) == C.v_graphics_native_layout(0, 11), 'sg_desc.mtl_use_command_buffer_with_retained_references offset'
		assert i64(__offsetof(C.sg_desc, wgpu_disable_bindgroups_cache)) == C.v_graphics_native_layout(0, 12), 'sg_desc.wgpu_disable_bindgroups_cache offset'
		assert i64(__offsetof(C.sg_desc, wgpu_bindgroups_cache_size)) == C.v_graphics_native_layout(0, 13), 'sg_desc.wgpu_bindgroups_cache_size offset'
		assert i64(__offsetof(C.sg_desc, allocator)) == C.v_graphics_native_layout(0, 14), 'sg_desc.allocator offset'
		assert i64(__offsetof(C.sg_desc, logger)) == C.v_graphics_native_layout(0, 15), 'sg_desc.logger offset'
		assert i64(__offsetof(C.sg_desc, environment)) == C.v_graphics_native_layout(0, 16), 'sg_desc.environment offset'
		assert i64(__offsetof(C.sg_desc, _end_canary)) == C.v_graphics_native_layout(0, 17), 'sg_desc._end_canary offset'
	}
}

fn test_native_layout_sg_color_target_state() {
	$if macos {
		assert i64(sizeof(C.sg_color_target_state)) == C.v_graphics_native_layout(1, -1), 'sg_color_target_state size'
		assert i64(__offsetof(C.sg_color_target_state, pixel_format)) == C.v_graphics_native_layout(1, 0), 'sg_color_target_state.pixel_format offset'
		assert i64(__offsetof(C.sg_color_target_state, write_mask)) == C.v_graphics_native_layout(1, 1), 'sg_color_target_state.write_mask offset'
		assert i64(__offsetof(C.sg_color_target_state, blend)) == C.v_graphics_native_layout(1, 2), 'sg_color_target_state.blend offset'
	}
}

fn test_native_layout_sg_pipeline_desc() {
	$if macos {
		assert i64(sizeof(C.sg_pipeline_desc)) == C.v_graphics_native_layout(2, -1), 'sg_pipeline_desc size'
		assert i64(__offsetof(C.sg_pipeline_desc, _start_canary)) == C.v_graphics_native_layout(2, 0), 'sg_pipeline_desc._start_canary offset'
		assert i64(__offsetof(C.sg_pipeline_desc, shader)) == C.v_graphics_native_layout(2, 1), 'sg_pipeline_desc.shader offset'
		assert i64(__offsetof(C.sg_pipeline_desc, layout)) == C.v_graphics_native_layout(2, 2), 'sg_pipeline_desc.layout offset'
		assert i64(__offsetof(C.sg_pipeline_desc, depth)) == C.v_graphics_native_layout(2, 3), 'sg_pipeline_desc.depth offset'
		assert i64(__offsetof(C.sg_pipeline_desc, stencil)) == C.v_graphics_native_layout(2, 4), 'sg_pipeline_desc.stencil offset'
		assert i64(__offsetof(C.sg_pipeline_desc, color_count)) == C.v_graphics_native_layout(2, 5), 'sg_pipeline_desc.color_count offset'
		assert i64(__offsetof(C.sg_pipeline_desc, colors)) == C.v_graphics_native_layout(2, 6), 'sg_pipeline_desc.colors offset'
		assert i64(__offsetof(C.sg_pipeline_desc, primitive_type)) == C.v_graphics_native_layout(2, 7), 'sg_pipeline_desc.primitive_type offset'
		assert i64(__offsetof(C.sg_pipeline_desc, index_type)) == C.v_graphics_native_layout(2, 8), 'sg_pipeline_desc.index_type offset'
		assert i64(__offsetof(C.sg_pipeline_desc, cull_mode)) == C.v_graphics_native_layout(2, 9), 'sg_pipeline_desc.cull_mode offset'
		assert i64(__offsetof(C.sg_pipeline_desc, face_winding)) == C.v_graphics_native_layout(2, 10), 'sg_pipeline_desc.face_winding offset'
		assert i64(__offsetof(C.sg_pipeline_desc, sample_count)) == C.v_graphics_native_layout(2, 11), 'sg_pipeline_desc.sample_count offset'
		assert i64(__offsetof(C.sg_pipeline_desc, blend_color)) == C.v_graphics_native_layout(2, 12), 'sg_pipeline_desc.blend_color offset'
		assert i64(__offsetof(C.sg_pipeline_desc, alpha_to_coverage_enabled)) == C.v_graphics_native_layout(2, 13), 'sg_pipeline_desc.alpha_to_coverage_enabled offset'
		assert i64(__offsetof(C.sg_pipeline_desc, label)) == C.v_graphics_native_layout(2, 14), 'sg_pipeline_desc.label offset'
		assert i64(__offsetof(C.sg_pipeline_desc, _end_canary)) == C.v_graphics_native_layout(2, 15), 'sg_pipeline_desc._end_canary offset'
	}
}

fn test_native_layout_sg_pipeline_info() {
	$if macos {
		assert i64(sizeof(C.sg_pipeline_info)) == C.v_graphics_native_layout(3, -1), 'sg_pipeline_info size'
		assert i64(__offsetof(C.sg_pipeline_info, slot)) == C.v_graphics_native_layout(3, 0), 'sg_pipeline_info.slot offset'
	}
}

fn test_native_layout_sg_pipeline() {
	$if macos {
		assert i64(sizeof(C.sg_pipeline)) == C.v_graphics_native_layout(4, -1), 'sg_pipeline size'
		assert i64(__offsetof(C.sg_pipeline, id)) == C.v_graphics_native_layout(4, 0), 'sg_pipeline.id offset'
	}
}

fn test_native_layout_sg_attachments() {
	$if macos {
		assert i64(sizeof(C.sg_attachments)) == C.v_graphics_native_layout(5, -1), 'sg_attachments size'
		assert i64(__offsetof(C.sg_attachments, id)) == C.v_graphics_native_layout(5, 0), 'sg_attachments.id offset'
	}
}

fn test_native_layout_sg_attachment_desc() {
	$if macos {
		assert i64(sizeof(C.sg_attachment_desc)) == C.v_graphics_native_layout(6, -1), 'sg_attachment_desc size'
		assert i64(__offsetof(C.sg_attachment_desc, image)) == C.v_graphics_native_layout(6, 0), 'sg_attachment_desc.image offset'
		assert i64(__offsetof(C.sg_attachment_desc, mip_level)) == C.v_graphics_native_layout(6, 1), 'sg_attachment_desc.mip_level offset'
		assert i64(__offsetof(C.sg_attachment_desc, slice)) == C.v_graphics_native_layout(6, 2), 'sg_attachment_desc.slice offset'
	}
}

fn test_native_layout_sg_attachments_desc() {
	$if macos {
		assert i64(sizeof(C.sg_attachments_desc)) == C.v_graphics_native_layout(7, -1), 'sg_attachments_desc size'
		assert i64(__offsetof(C.sg_attachments_desc, _start_canary)) == C.v_graphics_native_layout(7, 0), 'sg_attachments_desc._start_canary offset'
		assert i64(__offsetof(C.sg_attachments_desc, colors)) == C.v_graphics_native_layout(7, 1), 'sg_attachments_desc.colors offset'
		assert i64(__offsetof(C.sg_attachments_desc, resolves)) == C.v_graphics_native_layout(7, 2), 'sg_attachments_desc.resolves offset'
		assert i64(__offsetof(C.sg_attachments_desc, depth_stencil)) == C.v_graphics_native_layout(7, 3), 'sg_attachments_desc.depth_stencil offset'
		assert i64(__offsetof(C.sg_attachments_desc, label)) == C.v_graphics_native_layout(7, 4), 'sg_attachments_desc.label offset'
		assert i64(__offsetof(C.sg_attachments_desc, _end_canary)) == C.v_graphics_native_layout(7, 5), 'sg_attachments_desc._end_canary offset'
	}
}

fn test_native_layout_sg_attachments_info() {
	$if macos {
		assert i64(sizeof(C.sg_attachments_info)) == C.v_graphics_native_layout(8, -1), 'sg_attachments_info size'
		assert i64(__offsetof(C.sg_attachments_info, slot)) == C.v_graphics_native_layout(8, 0), 'sg_attachments_info.slot offset'
	}
}

fn test_native_layout_sg_bindings() {
	$if macos {
		assert i64(sizeof(C.sg_bindings)) == C.v_graphics_native_layout(9, -1), 'sg_bindings size'
		assert i64(__offsetof(C.sg_bindings, _start_canary)) == C.v_graphics_native_layout(9, 0), 'sg_bindings._start_canary offset'
		assert i64(__offsetof(C.sg_bindings, vertex_buffers)) == C.v_graphics_native_layout(9, 1), 'sg_bindings.vertex_buffers offset'
		assert i64(__offsetof(C.sg_bindings, vertex_buffer_offsets)) == C.v_graphics_native_layout(9, 2), 'sg_bindings.vertex_buffer_offsets offset'
		assert i64(__offsetof(C.sg_bindings, index_buffer)) == C.v_graphics_native_layout(9, 3), 'sg_bindings.index_buffer offset'
		assert i64(__offsetof(C.sg_bindings, index_buffer_offset)) == C.v_graphics_native_layout(9, 4), 'sg_bindings.index_buffer_offset offset'
		assert i64(__offsetof(C.sg_bindings, vs)) == C.v_graphics_native_layout(9, 5), 'sg_bindings.vs offset'
		assert i64(__offsetof(C.sg_bindings, fs)) == C.v_graphics_native_layout(9, 6), 'sg_bindings.fs offset'
		assert i64(__offsetof(C.sg_bindings, _end_canary)) == C.v_graphics_native_layout(9, 7), 'sg_bindings._end_canary offset'
	}
}

fn test_native_layout_sg_stage_bindings() {
	$if macos {
		assert i64(sizeof(C.sg_stage_bindings)) == C.v_graphics_native_layout(10, -1), 'sg_stage_bindings size'
		assert i64(__offsetof(C.sg_stage_bindings, images)) == C.v_graphics_native_layout(10, 0), 'sg_stage_bindings.images offset'
		assert i64(__offsetof(C.sg_stage_bindings, samplers)) == C.v_graphics_native_layout(10, 1), 'sg_stage_bindings.samplers offset'
		assert i64(__offsetof(C.sg_stage_bindings, storage_buffers)) == C.v_graphics_native_layout(10, 2), 'sg_stage_bindings.storage_buffers offset'
	}
}

fn test_native_layout_sg_shader_desc() {
	$if macos {
		assert i64(sizeof(C.sg_shader_desc)) == C.v_graphics_native_layout(11, -1), 'sg_shader_desc size'
		assert i64(__offsetof(C.sg_shader_desc, _start_canary)) == C.v_graphics_native_layout(11, 0), 'sg_shader_desc._start_canary offset'
		assert i64(__offsetof(C.sg_shader_desc, attrs)) == C.v_graphics_native_layout(11, 1), 'sg_shader_desc.attrs offset'
		assert i64(__offsetof(C.sg_shader_desc, vs)) == C.v_graphics_native_layout(11, 2), 'sg_shader_desc.vs offset'
		assert i64(__offsetof(C.sg_shader_desc, fs)) == C.v_graphics_native_layout(11, 3), 'sg_shader_desc.fs offset'
		assert i64(__offsetof(C.sg_shader_desc, label)) == C.v_graphics_native_layout(11, 4), 'sg_shader_desc.label offset'
		assert i64(__offsetof(C.sg_shader_desc, _end_canary)) == C.v_graphics_native_layout(11, 5), 'sg_shader_desc._end_canary offset'
	}
}

fn test_native_layout_sg_shader_attr_desc() {
	$if macos {
		assert i64(sizeof(C.sg_shader_attr_desc)) == C.v_graphics_native_layout(12, -1), 'sg_shader_attr_desc size'
		assert i64(__offsetof(C.sg_shader_attr_desc, name)) == C.v_graphics_native_layout(12, 0), 'sg_shader_attr_desc.name offset'
		assert i64(__offsetof(C.sg_shader_attr_desc, sem_name)) == C.v_graphics_native_layout(12, 1), 'sg_shader_attr_desc.sem_name offset'
		assert i64(__offsetof(C.sg_shader_attr_desc, sem_index)) == C.v_graphics_native_layout(12, 2), 'sg_shader_attr_desc.sem_index offset'
	}
}

fn test_native_layout_sg_shader_stage_desc() {
	$if macos {
		assert i64(sizeof(C.sg_shader_stage_desc)) == C.v_graphics_native_layout(13, -1), 'sg_shader_stage_desc size'
		assert i64(__offsetof(C.sg_shader_stage_desc, source)) == C.v_graphics_native_layout(13, 0), 'sg_shader_stage_desc.source offset'
		assert i64(__offsetof(C.sg_shader_stage_desc, bytecode)) == C.v_graphics_native_layout(13, 1), 'sg_shader_stage_desc.bytecode offset'
		assert i64(__offsetof(C.sg_shader_stage_desc, entry)) == C.v_graphics_native_layout(13, 2), 'sg_shader_stage_desc.entry offset'
		assert i64(__offsetof(C.sg_shader_stage_desc, d3d11_target)) == C.v_graphics_native_layout(13, 3), 'sg_shader_stage_desc.d3d11_target offset'
		assert i64(__offsetof(C.sg_shader_stage_desc, uniform_blocks)) == C.v_graphics_native_layout(13, 4), 'sg_shader_stage_desc.uniform_blocks offset'
		assert i64(__offsetof(C.sg_shader_stage_desc, storage_buffers)) == C.v_graphics_native_layout(13, 5), 'sg_shader_stage_desc.storage_buffers offset'
		assert i64(__offsetof(C.sg_shader_stage_desc, images)) == C.v_graphics_native_layout(13, 6), 'sg_shader_stage_desc.images offset'
		assert i64(__offsetof(C.sg_shader_stage_desc, samplers)) == C.v_graphics_native_layout(13, 7), 'sg_shader_stage_desc.samplers offset'
		assert i64(__offsetof(C.sg_shader_stage_desc, image_sampler_pairs)) == C.v_graphics_native_layout(13, 8), 'sg_shader_stage_desc.image_sampler_pairs offset'
	}
}

fn test_native_layout_sg_shader_uniform_block_desc() {
	$if macos {
		assert i64(sizeof(C.sg_shader_uniform_block_desc)) == C.v_graphics_native_layout(14, -1), 'sg_shader_uniform_block_desc size'
		assert i64(__offsetof(C.sg_shader_uniform_block_desc, size)) == C.v_graphics_native_layout(14, 0), 'sg_shader_uniform_block_desc.size offset'
		assert i64(__offsetof(C.sg_shader_uniform_block_desc, layout)) == C.v_graphics_native_layout(14, 1), 'sg_shader_uniform_block_desc.layout offset'
		assert i64(__offsetof(C.sg_shader_uniform_block_desc, uniforms)) == C.v_graphics_native_layout(14, 2), 'sg_shader_uniform_block_desc.uniforms offset'
	}
}

fn test_native_layout_sg_shader_uniform_desc() {
	$if macos {
		assert i64(sizeof(C.sg_shader_uniform_desc)) == C.v_graphics_native_layout(15, -1), 'sg_shader_uniform_desc size'
		assert i64(__offsetof(C.sg_shader_uniform_desc, name)) == C.v_graphics_native_layout(15, 0), 'sg_shader_uniform_desc.name offset'
		assert i64(__offsetof(C.sg_shader_uniform_desc, type)) == C.v_graphics_native_layout(15, 1), 'sg_shader_uniform_desc.type offset'
		assert i64(__offsetof(C.sg_shader_uniform_desc, array_count)) == C.v_graphics_native_layout(15, 2), 'sg_shader_uniform_desc.array_count offset'
	}
}

fn test_native_layout_sg_shader_image_desc() {
	$if macos {
		assert i64(sizeof(C.sg_shader_image_desc)) == C.v_graphics_native_layout(16, -1), 'sg_shader_image_desc size'
		assert i64(__offsetof(C.sg_shader_image_desc, used)) == C.v_graphics_native_layout(16, 0), 'sg_shader_image_desc.used offset'
		assert i64(__offsetof(C.sg_shader_image_desc, multisampled)) == C.v_graphics_native_layout(16, 1), 'sg_shader_image_desc.multisampled offset'
		assert i64(__offsetof(C.sg_shader_image_desc, image_type)) == C.v_graphics_native_layout(16, 2), 'sg_shader_image_desc.image_type offset'
		assert i64(__offsetof(C.sg_shader_image_desc, sample_type)) == C.v_graphics_native_layout(16, 3), 'sg_shader_image_desc.sample_type offset'
	}
}

fn test_native_layout_sg_shader_storage_buffer_desc() {
	$if macos {
		assert i64(sizeof(C.sg_shader_storage_buffer_desc)) == C.v_graphics_native_layout(17, -1), 'sg_shader_storage_buffer_desc size'
		assert i64(__offsetof(C.sg_shader_storage_buffer_desc, used)) == C.v_graphics_native_layout(17, 0), 'sg_shader_storage_buffer_desc.used offset'
		assert i64(__offsetof(C.sg_shader_storage_buffer_desc, readonly)) == C.v_graphics_native_layout(17, 1), 'sg_shader_storage_buffer_desc.readonly offset'
	}
}

fn test_native_layout_sg_shader_sampler_desc() {
	$if macos {
		assert i64(sizeof(C.sg_shader_sampler_desc)) == C.v_graphics_native_layout(18, -1), 'sg_shader_sampler_desc size'
		assert i64(__offsetof(C.sg_shader_sampler_desc, used)) == C.v_graphics_native_layout(18, 0), 'sg_shader_sampler_desc.used offset'
		assert i64(__offsetof(C.sg_shader_sampler_desc, sampler_type)) == C.v_graphics_native_layout(18, 1), 'sg_shader_sampler_desc.sampler_type offset'
	}
}

fn test_native_layout_sg_shader_image_sampler_pair_desc() {
	$if macos {
		assert i64(sizeof(C.sg_shader_image_sampler_pair_desc)) == C.v_graphics_native_layout(19, -1), 'sg_shader_image_sampler_pair_desc size'
		assert i64(__offsetof(C.sg_shader_image_sampler_pair_desc, used)) == C.v_graphics_native_layout(19, 0), 'sg_shader_image_sampler_pair_desc.used offset'
		assert i64(__offsetof(C.sg_shader_image_sampler_pair_desc, image_slot)) == C.v_graphics_native_layout(19, 1), 'sg_shader_image_sampler_pair_desc.image_slot offset'
		assert i64(__offsetof(C.sg_shader_image_sampler_pair_desc, sampler_slot)) == C.v_graphics_native_layout(19, 2), 'sg_shader_image_sampler_pair_desc.sampler_slot offset'
		assert i64(__offsetof(C.sg_shader_image_sampler_pair_desc, glsl_name)) == C.v_graphics_native_layout(19, 3), 'sg_shader_image_sampler_pair_desc.glsl_name offset'
	}
}

fn test_native_layout_sg_shader_info() {
	$if macos {
		assert i64(sizeof(C.sg_shader_info)) == C.v_graphics_native_layout(20, -1), 'sg_shader_info size'
		assert i64(__offsetof(C.sg_shader_info, slot)) == C.v_graphics_native_layout(20, 0), 'sg_shader_info.slot offset'
	}
}

fn test_native_layout_sg_range() {
	$if macos {
		assert i64(sizeof(C.sg_range)) == C.v_graphics_native_layout(21, -1), 'sg_range size'
		assert i64(__offsetof(C.sg_range, ptr)) == C.v_graphics_native_layout(21, 0), 'sg_range.ptr offset'
		assert i64(__offsetof(C.sg_range, size)) == C.v_graphics_native_layout(21, 1), 'sg_range.size offset'
	}
}

fn test_native_layout_sg_color() {
	$if macos {
		assert i64(sizeof(C.sg_color)) == C.v_graphics_native_layout(22, -1), 'sg_color size'
		assert i64(__offsetof(C.sg_color, r)) == C.v_graphics_native_layout(22, 0), 'sg_color.r offset'
		assert i64(__offsetof(C.sg_color, g)) == C.v_graphics_native_layout(22, 1), 'sg_color.g offset'
		assert i64(__offsetof(C.sg_color, b)) == C.v_graphics_native_layout(22, 2), 'sg_color.b offset'
		assert i64(__offsetof(C.sg_color, a)) == C.v_graphics_native_layout(22, 3), 'sg_color.a offset'
	}
}

fn test_native_layout_sg_shader() {
	$if macos {
		assert i64(sizeof(C.sg_shader)) == C.v_graphics_native_layout(23, -1), 'sg_shader size'
		assert i64(__offsetof(C.sg_shader, id)) == C.v_graphics_native_layout(23, 0), 'sg_shader.id offset'
	}
}

fn test_native_layout_sg_frame_stats_gl() {
	$if macos {
		assert i64(sizeof(C.sg_frame_stats_gl)) == C.v_graphics_native_layout(24, -1), 'sg_frame_stats_gl size'
		assert i64(__offsetof(C.sg_frame_stats_gl, num_bind_buffer)) == C.v_graphics_native_layout(24, 0), 'sg_frame_stats_gl.num_bind_buffer offset'
		assert i64(__offsetof(C.sg_frame_stats_gl, num_active_texture)) == C.v_graphics_native_layout(24, 1), 'sg_frame_stats_gl.num_active_texture offset'
		assert i64(__offsetof(C.sg_frame_stats_gl, num_bind_texture)) == C.v_graphics_native_layout(24, 2), 'sg_frame_stats_gl.num_bind_texture offset'
		assert i64(__offsetof(C.sg_frame_stats_gl, num_bind_sampler)) == C.v_graphics_native_layout(24, 3), 'sg_frame_stats_gl.num_bind_sampler offset'
		assert i64(__offsetof(C.sg_frame_stats_gl, num_use_program)) == C.v_graphics_native_layout(24, 4), 'sg_frame_stats_gl.num_use_program offset'
		assert i64(__offsetof(C.sg_frame_stats_gl, num_render_state)) == C.v_graphics_native_layout(24, 5), 'sg_frame_stats_gl.num_render_state offset'
		assert i64(__offsetof(C.sg_frame_stats_gl, num_vertex_attrib_pointer)) == C.v_graphics_native_layout(24, 6), 'sg_frame_stats_gl.num_vertex_attrib_pointer offset'
		assert i64(__offsetof(C.sg_frame_stats_gl, num_vertex_attrib_divisor)) == C.v_graphics_native_layout(24, 7), 'sg_frame_stats_gl.num_vertex_attrib_divisor offset'
		assert i64(__offsetof(C.sg_frame_stats_gl, num_enable_vertex_attrib_array)) == C.v_graphics_native_layout(24, 8), 'sg_frame_stats_gl.num_enable_vertex_attrib_array offset'
		assert i64(__offsetof(C.sg_frame_stats_gl, num_disable_vertex_attrib_array)) == C.v_graphics_native_layout(24, 9), 'sg_frame_stats_gl.num_disable_vertex_attrib_array offset'
		assert i64(__offsetof(C.sg_frame_stats_gl, num_uniform)) == C.v_graphics_native_layout(24, 10), 'sg_frame_stats_gl.num_uniform offset'
	}
}

fn test_native_layout_sg_frame_stats_d3d11_pass() {
	$if macos {
		assert i64(sizeof(C.sg_frame_stats_d3d11_pass)) == C.v_graphics_native_layout(25, -1), 'sg_frame_stats_d3d11_pass size'
		assert i64(__offsetof(C.sg_frame_stats_d3d11_pass, num_om_set_render_targets)) == C.v_graphics_native_layout(25, 0), 'sg_frame_stats_d3d11_pass.num_om_set_render_targets offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11_pass, num_clear_render_target_view)) == C.v_graphics_native_layout(25, 1), 'sg_frame_stats_d3d11_pass.num_clear_render_target_view offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11_pass, num_clear_depth_stencil_view)) == C.v_graphics_native_layout(25, 2), 'sg_frame_stats_d3d11_pass.num_clear_depth_stencil_view offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11_pass, num_resolve_subresource)) == C.v_graphics_native_layout(25, 3), 'sg_frame_stats_d3d11_pass.num_resolve_subresource offset'
	}
}

fn test_native_layout_sg_frame_stats_d3d11_pipeline() {
	$if macos {
		assert i64(sizeof(C.sg_frame_stats_d3d11_pipeline)) == C.v_graphics_native_layout(26, -1), 'sg_frame_stats_d3d11_pipeline size'
		assert i64(__offsetof(C.sg_frame_stats_d3d11_pipeline, num_rs_set_state)) == C.v_graphics_native_layout(26, 0), 'sg_frame_stats_d3d11_pipeline.num_rs_set_state offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11_pipeline, num_om_set_depth_stencil_state)) == C.v_graphics_native_layout(26, 1), 'sg_frame_stats_d3d11_pipeline.num_om_set_depth_stencil_state offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11_pipeline, num_om_set_blend_state)) == C.v_graphics_native_layout(26, 2), 'sg_frame_stats_d3d11_pipeline.num_om_set_blend_state offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11_pipeline, num_ia_set_primitive_topology)) == C.v_graphics_native_layout(26, 3), 'sg_frame_stats_d3d11_pipeline.num_ia_set_primitive_topology offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11_pipeline, num_ia_set_input_layout)) == C.v_graphics_native_layout(26, 4), 'sg_frame_stats_d3d11_pipeline.num_ia_set_input_layout offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11_pipeline, num_vs_set_shader)) == C.v_graphics_native_layout(26, 5), 'sg_frame_stats_d3d11_pipeline.num_vs_set_shader offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11_pipeline, num_vs_set_constant_buffers)) == C.v_graphics_native_layout(26, 6), 'sg_frame_stats_d3d11_pipeline.num_vs_set_constant_buffers offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11_pipeline, num_ps_set_shader)) == C.v_graphics_native_layout(26, 7), 'sg_frame_stats_d3d11_pipeline.num_ps_set_shader offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11_pipeline, num_ps_set_constant_buffers)) == C.v_graphics_native_layout(26, 8), 'sg_frame_stats_d3d11_pipeline.num_ps_set_constant_buffers offset'
	}
}

fn test_native_layout_sg_frame_stats_d3d11_bindings() {
	$if macos {
		assert i64(sizeof(C.sg_frame_stats_d3d11_bindings)) == C.v_graphics_native_layout(27, -1), 'sg_frame_stats_d3d11_bindings size'
		assert i64(__offsetof(C.sg_frame_stats_d3d11_bindings, num_ia_set_vertex_buffers)) == C.v_graphics_native_layout(27, 0), 'sg_frame_stats_d3d11_bindings.num_ia_set_vertex_buffers offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11_bindings, num_ia_set_index_buffer)) == C.v_graphics_native_layout(27, 1), 'sg_frame_stats_d3d11_bindings.num_ia_set_index_buffer offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11_bindings, num_vs_set_shader_resources)) == C.v_graphics_native_layout(27, 2), 'sg_frame_stats_d3d11_bindings.num_vs_set_shader_resources offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11_bindings, num_ps_set_shader_resources)) == C.v_graphics_native_layout(27, 3), 'sg_frame_stats_d3d11_bindings.num_ps_set_shader_resources offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11_bindings, num_vs_set_samplers)) == C.v_graphics_native_layout(27, 4), 'sg_frame_stats_d3d11_bindings.num_vs_set_samplers offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11_bindings, num_ps_set_samplers)) == C.v_graphics_native_layout(27, 5), 'sg_frame_stats_d3d11_bindings.num_ps_set_samplers offset'
	}
}

fn test_native_layout_sg_frame_stats_d3d11_uniforms() {
	$if macos {
		assert i64(sizeof(C.sg_frame_stats_d3d11_uniforms)) == C.v_graphics_native_layout(28, -1), 'sg_frame_stats_d3d11_uniforms size'
		assert i64(__offsetof(C.sg_frame_stats_d3d11_uniforms, num_update_subresource)) == C.v_graphics_native_layout(28, 0), 'sg_frame_stats_d3d11_uniforms.num_update_subresource offset'
	}
}

fn test_native_layout_sg_frame_stats_d3d11_draw() {
	$if macos {
		assert i64(sizeof(C.sg_frame_stats_d3d11_draw)) == C.v_graphics_native_layout(29, -1), 'sg_frame_stats_d3d11_draw size'
		assert i64(__offsetof(C.sg_frame_stats_d3d11_draw, num_draw_indexed_instanced)) == C.v_graphics_native_layout(29, 0), 'sg_frame_stats_d3d11_draw.num_draw_indexed_instanced offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11_draw, num_draw_indexed)) == C.v_graphics_native_layout(29, 1), 'sg_frame_stats_d3d11_draw.num_draw_indexed offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11_draw, num_draw_instanced)) == C.v_graphics_native_layout(29, 2), 'sg_frame_stats_d3d11_draw.num_draw_instanced offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11_draw, num_draw)) == C.v_graphics_native_layout(29, 3), 'sg_frame_stats_d3d11_draw.num_draw offset'
	}
}

fn test_native_layout_sg_frame_stats_d3d11() {
	$if macos {
		assert i64(sizeof(C.sg_frame_stats_d3d11)) == C.v_graphics_native_layout(30, -1), 'sg_frame_stats_d3d11 size'
		assert i64(__offsetof(C.sg_frame_stats_d3d11, pass)) == C.v_graphics_native_layout(30, 0), 'sg_frame_stats_d3d11.pass offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11, pipeline)) == C.v_graphics_native_layout(30, 1), 'sg_frame_stats_d3d11.pipeline offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11, bindings)) == C.v_graphics_native_layout(30, 2), 'sg_frame_stats_d3d11.bindings offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11, uniforms)) == C.v_graphics_native_layout(30, 3), 'sg_frame_stats_d3d11.uniforms offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11, draw)) == C.v_graphics_native_layout(30, 4), 'sg_frame_stats_d3d11.draw offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11, num_map)) == C.v_graphics_native_layout(30, 5), 'sg_frame_stats_d3d11.num_map offset'
		assert i64(__offsetof(C.sg_frame_stats_d3d11, num_unmap)) == C.v_graphics_native_layout(30, 6), 'sg_frame_stats_d3d11.num_unmap offset'
	}
}

fn test_native_layout_sg_frame_stats_metal_idpool() {
	$if macos {
		assert i64(sizeof(C.sg_frame_stats_metal_idpool)) == C.v_graphics_native_layout(31, -1), 'sg_frame_stats_metal_idpool size'
		assert i64(__offsetof(C.sg_frame_stats_metal_idpool, num_added)) == C.v_graphics_native_layout(31, 0), 'sg_frame_stats_metal_idpool.num_added offset'
		assert i64(__offsetof(C.sg_frame_stats_metal_idpool, num_released)) == C.v_graphics_native_layout(31, 1), 'sg_frame_stats_metal_idpool.num_released offset'
		assert i64(__offsetof(C.sg_frame_stats_metal_idpool, num_garbage_collected)) == C.v_graphics_native_layout(31, 2), 'sg_frame_stats_metal_idpool.num_garbage_collected offset'
	}
}

fn test_native_layout_sg_frame_stats_metal_pipeline() {
	$if macos {
		assert i64(sizeof(C.sg_frame_stats_metal_pipeline)) == C.v_graphics_native_layout(32, -1), 'sg_frame_stats_metal_pipeline size'
		assert i64(__offsetof(C.sg_frame_stats_metal_pipeline, num_set_blend_color)) == C.v_graphics_native_layout(32, 0), 'sg_frame_stats_metal_pipeline.num_set_blend_color offset'
		assert i64(__offsetof(C.sg_frame_stats_metal_pipeline, num_set_cull_mode)) == C.v_graphics_native_layout(32, 1), 'sg_frame_stats_metal_pipeline.num_set_cull_mode offset'
		assert i64(__offsetof(C.sg_frame_stats_metal_pipeline, num_set_front_facing_winding)) == C.v_graphics_native_layout(32, 2), 'sg_frame_stats_metal_pipeline.num_set_front_facing_winding offset'
		assert i64(__offsetof(C.sg_frame_stats_metal_pipeline, num_set_stencil_reference_value)) == C.v_graphics_native_layout(32, 3), 'sg_frame_stats_metal_pipeline.num_set_stencil_reference_value offset'
		assert i64(__offsetof(C.sg_frame_stats_metal_pipeline, num_set_depth_bias)) == C.v_graphics_native_layout(32, 4), 'sg_frame_stats_metal_pipeline.num_set_depth_bias offset'
		assert i64(__offsetof(C.sg_frame_stats_metal_pipeline, num_set_render_pipeline_state)) == C.v_graphics_native_layout(32, 5), 'sg_frame_stats_metal_pipeline.num_set_render_pipeline_state offset'
		assert i64(__offsetof(C.sg_frame_stats_metal_pipeline, num_set_depth_stencil_state)) == C.v_graphics_native_layout(32, 6), 'sg_frame_stats_metal_pipeline.num_set_depth_stencil_state offset'
	}
}

fn test_native_layout_sg_frame_stats_metal_bindings() {
	$if macos {
		assert i64(sizeof(C.sg_frame_stats_metal_bindings)) == C.v_graphics_native_layout(33, -1), 'sg_frame_stats_metal_bindings size'
		assert i64(__offsetof(C.sg_frame_stats_metal_bindings, num_set_vertex_buffer)) == C.v_graphics_native_layout(33, 0), 'sg_frame_stats_metal_bindings.num_set_vertex_buffer offset'
		assert i64(__offsetof(C.sg_frame_stats_metal_bindings, num_set_vertex_texture)) == C.v_graphics_native_layout(33, 1), 'sg_frame_stats_metal_bindings.num_set_vertex_texture offset'
		assert i64(__offsetof(C.sg_frame_stats_metal_bindings, num_set_vertex_sampler_state)) == C.v_graphics_native_layout(33, 2), 'sg_frame_stats_metal_bindings.num_set_vertex_sampler_state offset'
		assert i64(__offsetof(C.sg_frame_stats_metal_bindings, num_set_fragment_buffer)) == C.v_graphics_native_layout(33, 3), 'sg_frame_stats_metal_bindings.num_set_fragment_buffer offset'
		assert i64(__offsetof(C.sg_frame_stats_metal_bindings, num_set_fragment_texture)) == C.v_graphics_native_layout(33, 4), 'sg_frame_stats_metal_bindings.num_set_fragment_texture offset'
		assert i64(__offsetof(C.sg_frame_stats_metal_bindings, num_set_fragment_sampler_state)) == C.v_graphics_native_layout(33, 5), 'sg_frame_stats_metal_bindings.num_set_fragment_sampler_state offset'
	}
}

fn test_native_layout_sg_frame_stats_metal_uniforms() {
	$if macos {
		assert i64(sizeof(C.sg_frame_stats_metal_uniforms)) == C.v_graphics_native_layout(34, -1), 'sg_frame_stats_metal_uniforms size'
		assert i64(__offsetof(C.sg_frame_stats_metal_uniforms, num_set_vertex_buffer_offset)) == C.v_graphics_native_layout(34, 0), 'sg_frame_stats_metal_uniforms.num_set_vertex_buffer_offset offset'
		assert i64(__offsetof(C.sg_frame_stats_metal_uniforms, num_set_fragment_buffer_offset)) == C.v_graphics_native_layout(34, 1), 'sg_frame_stats_metal_uniforms.num_set_fragment_buffer_offset offset'
	}
}

fn test_native_layout_sg_frame_stats_metal() {
	$if macos {
		assert i64(sizeof(C.sg_frame_stats_metal)) == C.v_graphics_native_layout(35, -1), 'sg_frame_stats_metal size'
		assert i64(__offsetof(C.sg_frame_stats_metal, idpool)) == C.v_graphics_native_layout(35, 0), 'sg_frame_stats_metal.idpool offset'
		assert i64(__offsetof(C.sg_frame_stats_metal, pipeline)) == C.v_graphics_native_layout(35, 1), 'sg_frame_stats_metal.pipeline offset'
		assert i64(__offsetof(C.sg_frame_stats_metal, bindings)) == C.v_graphics_native_layout(35, 2), 'sg_frame_stats_metal.bindings offset'
		assert i64(__offsetof(C.sg_frame_stats_metal, uniforms)) == C.v_graphics_native_layout(35, 3), 'sg_frame_stats_metal.uniforms offset'
	}
}

fn test_native_layout_sg_frame_stats_wgpu_uniforms() {
	$if macos {
		assert i64(sizeof(C.sg_frame_stats_wgpu_uniforms)) == C.v_graphics_native_layout(36, -1), 'sg_frame_stats_wgpu_uniforms size'
		assert i64(__offsetof(C.sg_frame_stats_wgpu_uniforms, num_set_bindgroup)) == C.v_graphics_native_layout(36, 0), 'sg_frame_stats_wgpu_uniforms.num_set_bindgroup offset'
		assert i64(__offsetof(C.sg_frame_stats_wgpu_uniforms, size_write_buffer)) == C.v_graphics_native_layout(36, 1), 'sg_frame_stats_wgpu_uniforms.size_write_buffer offset'
	}
}

fn test_native_layout_sg_frame_stats_wgpu_bindings() {
	$if macos {
		assert i64(sizeof(C.sg_frame_stats_wgpu_bindings)) == C.v_graphics_native_layout(37, -1), 'sg_frame_stats_wgpu_bindings size'
		assert i64(__offsetof(C.sg_frame_stats_wgpu_bindings, num_set_vertex_buffer)) == C.v_graphics_native_layout(37, 0), 'sg_frame_stats_wgpu_bindings.num_set_vertex_buffer offset'
		assert i64(__offsetof(C.sg_frame_stats_wgpu_bindings, num_skip_redundant_vertex_buffer)) == C.v_graphics_native_layout(37, 1), 'sg_frame_stats_wgpu_bindings.num_skip_redundant_vertex_buffer offset'
		assert i64(__offsetof(C.sg_frame_stats_wgpu_bindings, num_set_index_buffer)) == C.v_graphics_native_layout(37, 2), 'sg_frame_stats_wgpu_bindings.num_set_index_buffer offset'
		assert i64(__offsetof(C.sg_frame_stats_wgpu_bindings, num_skip_redundant_index_buffer)) == C.v_graphics_native_layout(37, 3), 'sg_frame_stats_wgpu_bindings.num_skip_redundant_index_buffer offset'
		assert i64(__offsetof(C.sg_frame_stats_wgpu_bindings, num_create_bindgroup)) == C.v_graphics_native_layout(37, 4), 'sg_frame_stats_wgpu_bindings.num_create_bindgroup offset'
		assert i64(__offsetof(C.sg_frame_stats_wgpu_bindings, num_discard_bindgroup)) == C.v_graphics_native_layout(37, 5), 'sg_frame_stats_wgpu_bindings.num_discard_bindgroup offset'
		assert i64(__offsetof(C.sg_frame_stats_wgpu_bindings, num_set_bindgroup)) == C.v_graphics_native_layout(37, 6), 'sg_frame_stats_wgpu_bindings.num_set_bindgroup offset'
		assert i64(__offsetof(C.sg_frame_stats_wgpu_bindings, num_skip_redundant_bindgroup)) == C.v_graphics_native_layout(37, 7), 'sg_frame_stats_wgpu_bindings.num_skip_redundant_bindgroup offset'
		assert i64(__offsetof(C.sg_frame_stats_wgpu_bindings, num_bindgroup_cache_hits)) == C.v_graphics_native_layout(37, 8), 'sg_frame_stats_wgpu_bindings.num_bindgroup_cache_hits offset'
		assert i64(__offsetof(C.sg_frame_stats_wgpu_bindings, num_bindgroup_cache_misses)) == C.v_graphics_native_layout(37, 9), 'sg_frame_stats_wgpu_bindings.num_bindgroup_cache_misses offset'
		assert i64(__offsetof(C.sg_frame_stats_wgpu_bindings, num_bindgroup_cache_collisions)) == C.v_graphics_native_layout(37, 10), 'sg_frame_stats_wgpu_bindings.num_bindgroup_cache_collisions offset'
		assert i64(__offsetof(C.sg_frame_stats_wgpu_bindings, num_bindgroup_cache_hash_vs_key_mismatch)) == C.v_graphics_native_layout(37, 11), 'sg_frame_stats_wgpu_bindings.num_bindgroup_cache_hash_vs_key_mismatch offset'
	}
}

fn test_native_layout_sg_frame_stats_wgpu() {
	$if macos {
		assert i64(sizeof(C.sg_frame_stats_wgpu)) == C.v_graphics_native_layout(38, -1), 'sg_frame_stats_wgpu size'
		assert i64(__offsetof(C.sg_frame_stats_wgpu, uniforms)) == C.v_graphics_native_layout(38, 0), 'sg_frame_stats_wgpu.uniforms offset'
		assert i64(__offsetof(C.sg_frame_stats_wgpu, bindings)) == C.v_graphics_native_layout(38, 1), 'sg_frame_stats_wgpu.bindings offset'
	}
}

fn test_native_layout_sg_frame_stats() {
	$if macos {
		assert i64(sizeof(C.sg_frame_stats)) == C.v_graphics_native_layout(39, -1), 'sg_frame_stats size'
		assert i64(__offsetof(C.sg_frame_stats, frame_index)) == C.v_graphics_native_layout(39, 0), 'sg_frame_stats.frame_index offset'
		assert i64(__offsetof(C.sg_frame_stats, num_passes)) == C.v_graphics_native_layout(39, 1), 'sg_frame_stats.num_passes offset'
		assert i64(__offsetof(C.sg_frame_stats, num_apply_viewport)) == C.v_graphics_native_layout(39, 2), 'sg_frame_stats.num_apply_viewport offset'
		assert i64(__offsetof(C.sg_frame_stats, num_apply_scissor_rect)) == C.v_graphics_native_layout(39, 3), 'sg_frame_stats.num_apply_scissor_rect offset'
		assert i64(__offsetof(C.sg_frame_stats, num_apply_pipeline)) == C.v_graphics_native_layout(39, 4), 'sg_frame_stats.num_apply_pipeline offset'
		assert i64(__offsetof(C.sg_frame_stats, num_apply_bindings)) == C.v_graphics_native_layout(39, 5), 'sg_frame_stats.num_apply_bindings offset'
		assert i64(__offsetof(C.sg_frame_stats, num_apply_uniforms)) == C.v_graphics_native_layout(39, 6), 'sg_frame_stats.num_apply_uniforms offset'
		assert i64(__offsetof(C.sg_frame_stats, num_draw)) == C.v_graphics_native_layout(39, 7), 'sg_frame_stats.num_draw offset'
		assert i64(__offsetof(C.sg_frame_stats, num_update_buffer)) == C.v_graphics_native_layout(39, 8), 'sg_frame_stats.num_update_buffer offset'
		assert i64(__offsetof(C.sg_frame_stats, num_append_buffer)) == C.v_graphics_native_layout(39, 9), 'sg_frame_stats.num_append_buffer offset'
		assert i64(__offsetof(C.sg_frame_stats, num_update_image)) == C.v_graphics_native_layout(39, 10), 'sg_frame_stats.num_update_image offset'
		assert i64(__offsetof(C.sg_frame_stats, size_apply_uniforms)) == C.v_graphics_native_layout(39, 11), 'sg_frame_stats.size_apply_uniforms offset'
		assert i64(__offsetof(C.sg_frame_stats, size_update_buffer)) == C.v_graphics_native_layout(39, 12), 'sg_frame_stats.size_update_buffer offset'
		assert i64(__offsetof(C.sg_frame_stats, size_append_buffer)) == C.v_graphics_native_layout(39, 13), 'sg_frame_stats.size_append_buffer offset'
		assert i64(__offsetof(C.sg_frame_stats, size_update_image)) == C.v_graphics_native_layout(39, 14), 'sg_frame_stats.size_update_image offset'
		assert i64(__offsetof(C.sg_frame_stats, gl)) == C.v_graphics_native_layout(39, 15), 'sg_frame_stats.gl offset'
		assert i64(__offsetof(C.sg_frame_stats, d3d11)) == C.v_graphics_native_layout(39, 16), 'sg_frame_stats.d3d11 offset'
		assert i64(__offsetof(C.sg_frame_stats, metal)) == C.v_graphics_native_layout(39, 17), 'sg_frame_stats.metal offset'
		assert i64(__offsetof(C.sg_frame_stats, wgpu)) == C.v_graphics_native_layout(39, 18), 'sg_frame_stats.wgpu offset'
	}
}

fn test_native_layout_sg_pass_action() {
	$if macos {
		assert i64(sizeof(C.sg_pass_action)) == C.v_graphics_native_layout(40, -1), 'sg_pass_action size'
		assert i64(__offsetof(C.sg_pass_action, colors)) == C.v_graphics_native_layout(40, 0), 'sg_pass_action.colors offset'
		assert i64(__offsetof(C.sg_pass_action, depth)) == C.v_graphics_native_layout(40, 1), 'sg_pass_action.depth offset'
		assert i64(__offsetof(C.sg_pass_action, stencil)) == C.v_graphics_native_layout(40, 2), 'sg_pass_action.stencil offset'
	}
}

fn test_native_layout_sg_metal_swapchain() {
	$if macos {
		assert i64(sizeof(C.sg_metal_swapchain)) == C.v_graphics_native_layout(41, -1), 'sg_metal_swapchain size'
		assert i64(__offsetof(C.sg_metal_swapchain, current_drawable)) == C.v_graphics_native_layout(41, 0), 'sg_metal_swapchain.current_drawable offset'
		assert i64(__offsetof(C.sg_metal_swapchain, depth_stencil_texture)) == C.v_graphics_native_layout(41, 1), 'sg_metal_swapchain.depth_stencil_texture offset'
		assert i64(__offsetof(C.sg_metal_swapchain, msaa_color_texture)) == C.v_graphics_native_layout(41, 2), 'sg_metal_swapchain.msaa_color_texture offset'
	}
}

fn test_native_layout_sg_d3d11_swapchain() {
	$if macos {
		assert i64(sizeof(C.sg_d3d11_swapchain)) == C.v_graphics_native_layout(42, -1), 'sg_d3d11_swapchain size'
		assert i64(__offsetof(C.sg_d3d11_swapchain, render_view)) == C.v_graphics_native_layout(42, 0), 'sg_d3d11_swapchain.render_view offset'
		assert i64(__offsetof(C.sg_d3d11_swapchain, resolve_view)) == C.v_graphics_native_layout(42, 1), 'sg_d3d11_swapchain.resolve_view offset'
		assert i64(__offsetof(C.sg_d3d11_swapchain, depth_stencil_view)) == C.v_graphics_native_layout(42, 2), 'sg_d3d11_swapchain.depth_stencil_view offset'
	}
}

fn test_native_layout_sg_wgpu_swapchain() {
	$if macos {
		assert i64(sizeof(C.sg_wgpu_swapchain)) == C.v_graphics_native_layout(43, -1), 'sg_wgpu_swapchain size'
		assert i64(__offsetof(C.sg_wgpu_swapchain, render_view)) == C.v_graphics_native_layout(43, 0), 'sg_wgpu_swapchain.render_view offset'
		assert i64(__offsetof(C.sg_wgpu_swapchain, resolve_view)) == C.v_graphics_native_layout(43, 1), 'sg_wgpu_swapchain.resolve_view offset'
		assert i64(__offsetof(C.sg_wgpu_swapchain, depth_stencil_view)) == C.v_graphics_native_layout(43, 2), 'sg_wgpu_swapchain.depth_stencil_view offset'
	}
}

fn test_native_layout_sg_gl_swapchain() {
	$if macos {
		assert i64(sizeof(C.sg_gl_swapchain)) == C.v_graphics_native_layout(44, -1), 'sg_gl_swapchain size'
		assert i64(__offsetof(C.sg_gl_swapchain, framebuffer)) == C.v_graphics_native_layout(44, 0), 'sg_gl_swapchain.framebuffer offset'
	}
}

fn test_native_layout_sg_swapchain() {
	$if macos {
		assert i64(sizeof(C.sg_swapchain)) == C.v_graphics_native_layout(45, -1), 'sg_swapchain size'
		assert i64(__offsetof(C.sg_swapchain, width)) == C.v_graphics_native_layout(45, 0), 'sg_swapchain.width offset'
		assert i64(__offsetof(C.sg_swapchain, height)) == C.v_graphics_native_layout(45, 1), 'sg_swapchain.height offset'
		assert i64(__offsetof(C.sg_swapchain, sample_count)) == C.v_graphics_native_layout(45, 2), 'sg_swapchain.sample_count offset'
		assert i64(__offsetof(C.sg_swapchain, color_format)) == C.v_graphics_native_layout(45, 3), 'sg_swapchain.color_format offset'
		assert i64(__offsetof(C.sg_swapchain, depth_format)) == C.v_graphics_native_layout(45, 4), 'sg_swapchain.depth_format offset'
		assert i64(__offsetof(C.sg_swapchain, metal)) == C.v_graphics_native_layout(45, 5), 'sg_swapchain.metal offset'
		assert i64(__offsetof(C.sg_swapchain, d3d11)) == C.v_graphics_native_layout(45, 6), 'sg_swapchain.d3d11 offset'
		assert i64(__offsetof(C.sg_swapchain, wgpu)) == C.v_graphics_native_layout(45, 7), 'sg_swapchain.wgpu offset'
		assert i64(__offsetof(C.sg_swapchain, gl)) == C.v_graphics_native_layout(45, 8), 'sg_swapchain.gl offset'
	}
}

fn test_native_layout_sg_pass() {
	$if macos {
		assert i64(sizeof(C.sg_pass)) == C.v_graphics_native_layout(46, -1), 'sg_pass size'
		assert i64(__offsetof(C.sg_pass, _start_canary)) == C.v_graphics_native_layout(46, 0), 'sg_pass._start_canary offset'
		assert i64(__offsetof(C.sg_pass, action)) == C.v_graphics_native_layout(46, 1), 'sg_pass.action offset'
		assert i64(__offsetof(C.sg_pass, attachments)) == C.v_graphics_native_layout(46, 2), 'sg_pass.attachments offset'
		assert i64(__offsetof(C.sg_pass, swapchain)) == C.v_graphics_native_layout(46, 3), 'sg_pass.swapchain offset'
		assert i64(__offsetof(C.sg_pass, label)) == C.v_graphics_native_layout(46, 4), 'sg_pass.label offset'
		assert i64(__offsetof(C.sg_pass, _end_canary)) == C.v_graphics_native_layout(46, 5), 'sg_pass._end_canary offset'
	}
}

fn test_native_layout_sg_buffer_desc() {
	$if macos {
		assert i64(sizeof(C.sg_buffer_desc)) == C.v_graphics_native_layout(47, -1), 'sg_buffer_desc size'
		assert i64(__offsetof(C.sg_buffer_desc, _start_canary)) == C.v_graphics_native_layout(47, 0), 'sg_buffer_desc._start_canary offset'
		assert i64(__offsetof(C.sg_buffer_desc, size)) == C.v_graphics_native_layout(47, 1), 'sg_buffer_desc.size offset'
		assert i64(__offsetof(C.sg_buffer_desc, type)) == C.v_graphics_native_layout(47, 2), 'sg_buffer_desc.type offset'
		assert i64(__offsetof(C.sg_buffer_desc, usage)) == C.v_graphics_native_layout(47, 3), 'sg_buffer_desc.usage offset'
		assert i64(__offsetof(C.sg_buffer_desc, data)) == C.v_graphics_native_layout(47, 4), 'sg_buffer_desc.data offset'
		assert i64(__offsetof(C.sg_buffer_desc, label)) == C.v_graphics_native_layout(47, 5), 'sg_buffer_desc.label offset'
		assert i64(__offsetof(C.sg_buffer_desc, gl_buffers)) == C.v_graphics_native_layout(47, 6), 'sg_buffer_desc.gl_buffers offset'
		assert i64(__offsetof(C.sg_buffer_desc, mtl_buffers)) == C.v_graphics_native_layout(47, 7), 'sg_buffer_desc.mtl_buffers offset'
		assert i64(__offsetof(C.sg_buffer_desc, d3d11_buffer)) == C.v_graphics_native_layout(47, 8), 'sg_buffer_desc.d3d11_buffer offset'
		assert i64(__offsetof(C.sg_buffer_desc, wgpu_buffer)) == C.v_graphics_native_layout(47, 9), 'sg_buffer_desc.wgpu_buffer offset'
		assert i64(__offsetof(C.sg_buffer_desc, _end_canary)) == C.v_graphics_native_layout(47, 10), 'sg_buffer_desc._end_canary offset'
	}
}

fn test_native_layout_sg_slot_info() {
	$if macos {
		assert i64(sizeof(C.sg_slot_info)) == C.v_graphics_native_layout(48, -1), 'sg_slot_info size'
		assert i64(__offsetof(C.sg_slot_info, state)) == C.v_graphics_native_layout(48, 0), 'sg_slot_info.state offset'
		assert i64(__offsetof(C.sg_slot_info, res_id)) == C.v_graphics_native_layout(48, 1), 'sg_slot_info.res_id offset'
	}
}

fn test_native_layout_sg_buffer_info() {
	$if macos {
		assert i64(sizeof(C.sg_buffer_info)) == C.v_graphics_native_layout(49, -1), 'sg_buffer_info size'
		assert i64(__offsetof(C.sg_buffer_info, slot)) == C.v_graphics_native_layout(49, 0), 'sg_buffer_info.slot offset'
		assert i64(__offsetof(C.sg_buffer_info, update_frame_index)) == C.v_graphics_native_layout(49, 1), 'sg_buffer_info.update_frame_index offset'
		assert i64(__offsetof(C.sg_buffer_info, append_frame_index)) == C.v_graphics_native_layout(49, 2), 'sg_buffer_info.append_frame_index offset'
		assert i64(__offsetof(C.sg_buffer_info, append_pos)) == C.v_graphics_native_layout(49, 3), 'sg_buffer_info.append_pos offset'
		assert i64(__offsetof(C.sg_buffer_info, append_overflow)) == C.v_graphics_native_layout(49, 4), 'sg_buffer_info.append_overflow offset'
		assert i64(__offsetof(C.sg_buffer_info, num_slots)) == C.v_graphics_native_layout(49, 5), 'sg_buffer_info.num_slots offset'
		assert i64(__offsetof(C.sg_buffer_info, active_slot)) == C.v_graphics_native_layout(49, 6), 'sg_buffer_info.active_slot offset'
	}
}

fn test_native_layout_sg_buffer() {
	$if macos {
		assert i64(sizeof(C.sg_buffer)) == C.v_graphics_native_layout(50, -1), 'sg_buffer size'
		assert i64(__offsetof(C.sg_buffer, id)) == C.v_graphics_native_layout(50, 0), 'sg_buffer.id offset'
	}
}

fn test_native_layout_sg_image_desc() {
	$if macos {
		assert i64(sizeof(C.sg_image_desc)) == C.v_graphics_native_layout(51, -1), 'sg_image_desc size'
		assert i64(__offsetof(C.sg_image_desc, _start_canary)) == C.v_graphics_native_layout(51, 0), 'sg_image_desc._start_canary offset'
		assert i64(__offsetof(C.sg_image_desc, type)) == C.v_graphics_native_layout(51, 1), 'sg_image_desc.type offset'
		assert i64(__offsetof(C.sg_image_desc, render_target)) == C.v_graphics_native_layout(51, 2), 'sg_image_desc.render_target offset'
		assert i64(__offsetof(C.sg_image_desc, width)) == C.v_graphics_native_layout(51, 3), 'sg_image_desc.width offset'
		assert i64(__offsetof(C.sg_image_desc, height)) == C.v_graphics_native_layout(51, 4), 'sg_image_desc.height offset'
		assert i64(__offsetof(C.sg_image_desc, num_slices)) == C.v_graphics_native_layout(51, 5), 'sg_image_desc.num_slices offset'
		assert i64(__offsetof(C.sg_image_desc, num_mipmaps)) == C.v_graphics_native_layout(51, 6), 'sg_image_desc.num_mipmaps offset'
		assert i64(__offsetof(C.sg_image_desc, usage)) == C.v_graphics_native_layout(51, 7), 'sg_image_desc.usage offset'
		assert i64(__offsetof(C.sg_image_desc, pixel_format)) == C.v_graphics_native_layout(51, 8), 'sg_image_desc.pixel_format offset'
		assert i64(__offsetof(C.sg_image_desc, sample_count)) == C.v_graphics_native_layout(51, 9), 'sg_image_desc.sample_count offset'
		assert i64(__offsetof(C.sg_image_desc, data)) == C.v_graphics_native_layout(51, 10), 'sg_image_desc.data offset'
		assert i64(__offsetof(C.sg_image_desc, label)) == C.v_graphics_native_layout(51, 11), 'sg_image_desc.label offset'
		assert i64(__offsetof(C.sg_image_desc, gl_textures)) == C.v_graphics_native_layout(51, 12), 'sg_image_desc.gl_textures offset'
		assert i64(__offsetof(C.sg_image_desc, gl_texture_target)) == C.v_graphics_native_layout(51, 13), 'sg_image_desc.gl_texture_target offset'
		assert i64(__offsetof(C.sg_image_desc, mtl_textures)) == C.v_graphics_native_layout(51, 14), 'sg_image_desc.mtl_textures offset'
		assert i64(__offsetof(C.sg_image_desc, d3d11_texture)) == C.v_graphics_native_layout(51, 15), 'sg_image_desc.d3d11_texture offset'
		assert i64(__offsetof(C.sg_image_desc, d3d11_shader_resource_view)) == C.v_graphics_native_layout(51, 16), 'sg_image_desc.d3d11_shader_resource_view offset'
		assert i64(__offsetof(C.sg_image_desc, wgpu_texture)) == C.v_graphics_native_layout(51, 17), 'sg_image_desc.wgpu_texture offset'
		assert i64(__offsetof(C.sg_image_desc, wgpu_texture_view)) == C.v_graphics_native_layout(51, 18), 'sg_image_desc.wgpu_texture_view offset'
		assert i64(__offsetof(C.sg_image_desc, _end_canary)) == C.v_graphics_native_layout(51, 19), 'sg_image_desc._end_canary offset'
	}
}

fn test_native_layout_sg_sampler_desc() {
	$if macos {
		assert i64(sizeof(C.sg_sampler_desc)) == C.v_graphics_native_layout(52, -1), 'sg_sampler_desc size'
		assert i64(__offsetof(C.sg_sampler_desc, _start_canary)) == C.v_graphics_native_layout(52, 0), 'sg_sampler_desc._start_canary offset'
		assert i64(__offsetof(C.sg_sampler_desc, min_filter)) == C.v_graphics_native_layout(52, 1), 'sg_sampler_desc.min_filter offset'
		assert i64(__offsetof(C.sg_sampler_desc, mag_filter)) == C.v_graphics_native_layout(52, 2), 'sg_sampler_desc.mag_filter offset'
		assert i64(__offsetof(C.sg_sampler_desc, mipmap_filter)) == C.v_graphics_native_layout(52, 3), 'sg_sampler_desc.mipmap_filter offset'
		assert i64(__offsetof(C.sg_sampler_desc, wrap_u)) == C.v_graphics_native_layout(52, 4), 'sg_sampler_desc.wrap_u offset'
		assert i64(__offsetof(C.sg_sampler_desc, wrap_v)) == C.v_graphics_native_layout(52, 5), 'sg_sampler_desc.wrap_v offset'
		assert i64(__offsetof(C.sg_sampler_desc, wrap_w)) == C.v_graphics_native_layout(52, 6), 'sg_sampler_desc.wrap_w offset'
		assert i64(__offsetof(C.sg_sampler_desc, min_lod)) == C.v_graphics_native_layout(52, 7), 'sg_sampler_desc.min_lod offset'
		assert i64(__offsetof(C.sg_sampler_desc, max_lod)) == C.v_graphics_native_layout(52, 8), 'sg_sampler_desc.max_lod offset'
		assert i64(__offsetof(C.sg_sampler_desc, border_color)) == C.v_graphics_native_layout(52, 9), 'sg_sampler_desc.border_color offset'
		assert i64(__offsetof(C.sg_sampler_desc, compare)) == C.v_graphics_native_layout(52, 10), 'sg_sampler_desc.compare offset'
		assert i64(__offsetof(C.sg_sampler_desc, max_anisotropy)) == C.v_graphics_native_layout(52, 11), 'sg_sampler_desc.max_anisotropy offset'
		assert i64(__offsetof(C.sg_sampler_desc, label)) == C.v_graphics_native_layout(52, 12), 'sg_sampler_desc.label offset'
		assert i64(__offsetof(C.sg_sampler_desc, gl_sampler)) == C.v_graphics_native_layout(52, 13), 'sg_sampler_desc.gl_sampler offset'
		assert i64(__offsetof(C.sg_sampler_desc, mtl_sampler)) == C.v_graphics_native_layout(52, 14), 'sg_sampler_desc.mtl_sampler offset'
		assert i64(__offsetof(C.sg_sampler_desc, d3d11_sampler)) == C.v_graphics_native_layout(52, 15), 'sg_sampler_desc.d3d11_sampler offset'
		assert i64(__offsetof(C.sg_sampler_desc, wgpu_sampler)) == C.v_graphics_native_layout(52, 16), 'sg_sampler_desc.wgpu_sampler offset'
		assert i64(__offsetof(C.sg_sampler_desc, _end_canary)) == C.v_graphics_native_layout(52, 17), 'sg_sampler_desc._end_canary offset'
	}
}

fn test_native_layout_sg_image_info() {
	$if macos {
		assert i64(sizeof(C.sg_image_info)) == C.v_graphics_native_layout(53, -1), 'sg_image_info size'
		assert i64(__offsetof(C.sg_image_info, slot)) == C.v_graphics_native_layout(53, 0), 'sg_image_info.slot offset'
		assert i64(__offsetof(C.sg_image_info, upd_frame_index)) == C.v_graphics_native_layout(53, 1), 'sg_image_info.upd_frame_index offset'
		assert i64(__offsetof(C.sg_image_info, num_slots)) == C.v_graphics_native_layout(53, 2), 'sg_image_info.num_slots offset'
		assert i64(__offsetof(C.sg_image_info, active_slot)) == C.v_graphics_native_layout(53, 3), 'sg_image_info.active_slot offset'
	}
}

fn test_native_layout_sg_image() {
	$if macos {
		assert i64(sizeof(C.sg_image)) == C.v_graphics_native_layout(54, -1), 'sg_image size'
		assert i64(__offsetof(C.sg_image, id)) == C.v_graphics_native_layout(54, 0), 'sg_image.id offset'
	}
}

fn test_native_layout_sg_sampler() {
	$if macos {
		assert i64(sizeof(C.sg_sampler)) == C.v_graphics_native_layout(55, -1), 'sg_sampler size'
		assert i64(__offsetof(C.sg_sampler, id)) == C.v_graphics_native_layout(55, 0), 'sg_sampler.id offset'
	}
}

fn test_native_layout_sg_image_data() {
	$if macos {
		assert i64(sizeof(C.sg_image_data)) == C.v_graphics_native_layout(56, -1), 'sg_image_data size'
		assert i64(__offsetof(C.sg_image_data, subimage)) == C.v_graphics_native_layout(56, 0), 'sg_image_data.subimage offset'
	}
}

fn test_native_layout_sg_features() {
	$if macos {
		assert i64(sizeof(C.sg_features)) == C.v_graphics_native_layout(57, -1), 'sg_features size'
		assert i64(__offsetof(C.sg_features, origin_top_left)) == C.v_graphics_native_layout(57, 0), 'sg_features.origin_top_left offset'
		assert i64(__offsetof(C.sg_features, image_clamp_to_border)) == C.v_graphics_native_layout(57, 1), 'sg_features.image_clamp_to_border offset'
		assert i64(__offsetof(C.sg_features, mrt_independent_blend_state)) == C.v_graphics_native_layout(57, 2), 'sg_features.mrt_independent_blend_state offset'
		assert i64(__offsetof(C.sg_features, mrt_independent_write_mask)) == C.v_graphics_native_layout(57, 3), 'sg_features.mrt_independent_write_mask offset'
		assert i64(__offsetof(C.sg_features, storage_buffer)) == C.v_graphics_native_layout(57, 4), 'sg_features.storage_buffer offset'
	}
}

fn test_native_layout_sg_limits() {
	$if macos {
		assert i64(sizeof(C.sg_limits)) == C.v_graphics_native_layout(58, -1), 'sg_limits size'
		assert i64(__offsetof(C.sg_limits, max_image_size_2d)) == C.v_graphics_native_layout(58, 0), 'sg_limits.max_image_size_2d offset'
		assert i64(__offsetof(C.sg_limits, max_image_size_cube)) == C.v_graphics_native_layout(58, 1), 'sg_limits.max_image_size_cube offset'
		assert i64(__offsetof(C.sg_limits, max_image_size_3d)) == C.v_graphics_native_layout(58, 2), 'sg_limits.max_image_size_3d offset'
		assert i64(__offsetof(C.sg_limits, max_image_size_array)) == C.v_graphics_native_layout(58, 3), 'sg_limits.max_image_size_array offset'
		assert i64(__offsetof(C.sg_limits, max_image_array_layers)) == C.v_graphics_native_layout(58, 4), 'sg_limits.max_image_array_layers offset'
		assert i64(__offsetof(C.sg_limits, max_vertex_attrs)) == C.v_graphics_native_layout(58, 5), 'sg_limits.max_vertex_attrs offset'
		assert i64(__offsetof(C.sg_limits, gl_max_vertex_uniform_components)) == C.v_graphics_native_layout(58, 6), 'sg_limits.gl_max_vertex_uniform_components offset'
		assert i64(__offsetof(C.sg_limits, gl_max_combined_texture_image_units)) == C.v_graphics_native_layout(58, 7), 'sg_limits.gl_max_combined_texture_image_units offset'
	}
}

fn test_native_layout_sg_vertex_layout_state() {
	$if macos {
		assert i64(sizeof(C.sg_vertex_layout_state)) == C.v_graphics_native_layout(59, -1), 'sg_vertex_layout_state size'
		assert i64(__offsetof(C.sg_vertex_layout_state, buffers)) == C.v_graphics_native_layout(59, 0), 'sg_vertex_layout_state.buffers offset'
		assert i64(__offsetof(C.sg_vertex_layout_state, attrs)) == C.v_graphics_native_layout(59, 1), 'sg_vertex_layout_state.attrs offset'
	}
}

fn test_native_layout_sg_vertex_buffer_layout_state() {
	$if macos {
		assert i64(sizeof(C.sg_vertex_buffer_layout_state)) == C.v_graphics_native_layout(60, -1), 'sg_vertex_buffer_layout_state size'
		assert i64(__offsetof(C.sg_vertex_buffer_layout_state, stride)) == C.v_graphics_native_layout(60, 0), 'sg_vertex_buffer_layout_state.stride offset'
		assert i64(__offsetof(C.sg_vertex_buffer_layout_state, step_func)) == C.v_graphics_native_layout(60, 1), 'sg_vertex_buffer_layout_state.step_func offset'
		assert i64(__offsetof(C.sg_vertex_buffer_layout_state, step_rate)) == C.v_graphics_native_layout(60, 2), 'sg_vertex_buffer_layout_state.step_rate offset'
	}
}

fn test_native_layout_sg_vertex_attr_state() {
	$if macos {
		assert i64(sizeof(C.sg_vertex_attr_state)) == C.v_graphics_native_layout(61, -1), 'sg_vertex_attr_state size'
		assert i64(__offsetof(C.sg_vertex_attr_state, buffer_index)) == C.v_graphics_native_layout(61, 0), 'sg_vertex_attr_state.buffer_index offset'
		assert i64(__offsetof(C.sg_vertex_attr_state, offset)) == C.v_graphics_native_layout(61, 1), 'sg_vertex_attr_state.offset offset'
		assert i64(__offsetof(C.sg_vertex_attr_state, format)) == C.v_graphics_native_layout(61, 2), 'sg_vertex_attr_state.format offset'
	}
}

fn test_native_layout_sg_stencil_state() {
	$if macos {
		assert i64(sizeof(C.sg_stencil_state)) == C.v_graphics_native_layout(62, -1), 'sg_stencil_state size'
		assert i64(__offsetof(C.sg_stencil_state, enabled)) == C.v_graphics_native_layout(62, 0), 'sg_stencil_state.enabled offset'
		assert i64(__offsetof(C.sg_stencil_state, front)) == C.v_graphics_native_layout(62, 1), 'sg_stencil_state.front offset'
		assert i64(__offsetof(C.sg_stencil_state, back)) == C.v_graphics_native_layout(62, 2), 'sg_stencil_state.back offset'
		assert i64(__offsetof(C.sg_stencil_state, read_mask)) == C.v_graphics_native_layout(62, 3), 'sg_stencil_state.read_mask offset'
		assert i64(__offsetof(C.sg_stencil_state, write_mask)) == C.v_graphics_native_layout(62, 4), 'sg_stencil_state.write_mask offset'
		assert i64(__offsetof(C.sg_stencil_state, ref)) == C.v_graphics_native_layout(62, 5), 'sg_stencil_state.ref offset'
	}
}

fn test_native_layout_sg_depth_state() {
	$if macos {
		assert i64(sizeof(C.sg_depth_state)) == C.v_graphics_native_layout(63, -1), 'sg_depth_state size'
		assert i64(__offsetof(C.sg_depth_state, pixel_format)) == C.v_graphics_native_layout(63, 0), 'sg_depth_state.pixel_format offset'
		assert i64(__offsetof(C.sg_depth_state, compare)) == C.v_graphics_native_layout(63, 1), 'sg_depth_state.compare offset'
		assert i64(__offsetof(C.sg_depth_state, write_enabled)) == C.v_graphics_native_layout(63, 2), 'sg_depth_state.write_enabled offset'
		assert i64(__offsetof(C.sg_depth_state, bias)) == C.v_graphics_native_layout(63, 3), 'sg_depth_state.bias offset'
		assert i64(__offsetof(C.sg_depth_state, bias_slope_scale)) == C.v_graphics_native_layout(63, 4), 'sg_depth_state.bias_slope_scale offset'
		assert i64(__offsetof(C.sg_depth_state, bias_clamp)) == C.v_graphics_native_layout(63, 5), 'sg_depth_state.bias_clamp offset'
	}
}

fn test_native_layout_sg_stencil_face_state() {
	$if macos {
		assert i64(sizeof(C.sg_stencil_face_state)) == C.v_graphics_native_layout(64, -1), 'sg_stencil_face_state size'
		assert i64(__offsetof(C.sg_stencil_face_state, compare)) == C.v_graphics_native_layout(64, 0), 'sg_stencil_face_state.compare offset'
		assert i64(__offsetof(C.sg_stencil_face_state, fail_op)) == C.v_graphics_native_layout(64, 1), 'sg_stencil_face_state.fail_op offset'
		assert i64(__offsetof(C.sg_stencil_face_state, depth_fail_op)) == C.v_graphics_native_layout(64, 2), 'sg_stencil_face_state.depth_fail_op offset'
		assert i64(__offsetof(C.sg_stencil_face_state, pass_op)) == C.v_graphics_native_layout(64, 3), 'sg_stencil_face_state.pass_op offset'
	}
}

fn test_native_layout_sg_blend_state() {
	$if macos {
		assert i64(sizeof(C.sg_blend_state)) == C.v_graphics_native_layout(65, -1), 'sg_blend_state size'
		assert i64(__offsetof(C.sg_blend_state, enabled)) == C.v_graphics_native_layout(65, 0), 'sg_blend_state.enabled offset'
		assert i64(__offsetof(C.sg_blend_state, src_factor_rgb)) == C.v_graphics_native_layout(65, 1), 'sg_blend_state.src_factor_rgb offset'
		assert i64(__offsetof(C.sg_blend_state, dst_factor_rgb)) == C.v_graphics_native_layout(65, 2), 'sg_blend_state.dst_factor_rgb offset'
		assert i64(__offsetof(C.sg_blend_state, op_rgb)) == C.v_graphics_native_layout(65, 3), 'sg_blend_state.op_rgb offset'
		assert i64(__offsetof(C.sg_blend_state, src_factor_alpha)) == C.v_graphics_native_layout(65, 4), 'sg_blend_state.src_factor_alpha offset'
		assert i64(__offsetof(C.sg_blend_state, dst_factor_alpha)) == C.v_graphics_native_layout(65, 5), 'sg_blend_state.dst_factor_alpha offset'
		assert i64(__offsetof(C.sg_blend_state, op_alpha)) == C.v_graphics_native_layout(65, 6), 'sg_blend_state.op_alpha offset'
	}
}

fn test_native_layout_sg_color_attachment_action() {
	$if macos {
		assert i64(sizeof(C.sg_color_attachment_action)) == C.v_graphics_native_layout(66, -1), 'sg_color_attachment_action size'
		assert i64(__offsetof(C.sg_color_attachment_action, load_action)) == C.v_graphics_native_layout(66, 0), 'sg_color_attachment_action.load_action offset'
		assert i64(__offsetof(C.sg_color_attachment_action, store_action)) == C.v_graphics_native_layout(66, 1), 'sg_color_attachment_action.store_action offset'
		assert i64(__offsetof(C.sg_color_attachment_action, clear_value)) == C.v_graphics_native_layout(66, 2), 'sg_color_attachment_action.clear_value offset'
	}
}

fn test_native_layout_sg_depth_attachment_action() {
	$if macos {
		assert i64(sizeof(C.sg_depth_attachment_action)) == C.v_graphics_native_layout(67, -1), 'sg_depth_attachment_action size'
		assert i64(__offsetof(C.sg_depth_attachment_action, load_action)) == C.v_graphics_native_layout(67, 0), 'sg_depth_attachment_action.load_action offset'
		assert i64(__offsetof(C.sg_depth_attachment_action, store_action)) == C.v_graphics_native_layout(67, 1), 'sg_depth_attachment_action.store_action offset'
		assert i64(__offsetof(C.sg_depth_attachment_action, clear_value)) == C.v_graphics_native_layout(67, 2), 'sg_depth_attachment_action.clear_value offset'
	}
}

fn test_native_layout_sg_stencil_attachment_action() {
	$if macos {
		assert i64(sizeof(C.sg_stencil_attachment_action)) == C.v_graphics_native_layout(68, -1), 'sg_stencil_attachment_action size'
		assert i64(__offsetof(C.sg_stencil_attachment_action, load_action)) == C.v_graphics_native_layout(68, 0), 'sg_stencil_attachment_action.load_action offset'
		assert i64(__offsetof(C.sg_stencil_attachment_action, store_action)) == C.v_graphics_native_layout(68, 1), 'sg_stencil_attachment_action.store_action offset'
		assert i64(__offsetof(C.sg_stencil_attachment_action, clear_value)) == C.v_graphics_native_layout(68, 2), 'sg_stencil_attachment_action.clear_value offset'
	}
}

fn test_native_layout_sg_pixelformat_info() {
	$if macos {
		assert i64(sizeof(C.sg_pixelformat_info)) == C.v_graphics_native_layout(69, -1), 'sg_pixelformat_info size'
		assert i64(__offsetof(C.sg_pixelformat_info, sample)) == C.v_graphics_native_layout(69, 0), 'sg_pixelformat_info.sample offset'
		assert i64(__offsetof(C.sg_pixelformat_info, filter)) == C.v_graphics_native_layout(69, 1), 'sg_pixelformat_info.filter offset'
		assert i64(__offsetof(C.sg_pixelformat_info, render)) == C.v_graphics_native_layout(69, 2), 'sg_pixelformat_info.render offset'
		assert i64(__offsetof(C.sg_pixelformat_info, blend)) == C.v_graphics_native_layout(69, 3), 'sg_pixelformat_info.blend offset'
		assert i64(__offsetof(C.sg_pixelformat_info, msaa)) == C.v_graphics_native_layout(69, 4), 'sg_pixelformat_info.msaa offset'
		assert i64(__offsetof(C.sg_pixelformat_info, depth)) == C.v_graphics_native_layout(69, 5), 'sg_pixelformat_info.depth offset'
		assert i64(__offsetof(C.sg_pixelformat_info, compressed)) == C.v_graphics_native_layout(69, 6), 'sg_pixelformat_info.compressed offset'
		assert i64(__offsetof(C.sg_pixelformat_info, bytes_per_pixel)) == C.v_graphics_native_layout(69, 7), 'sg_pixelformat_info.bytes_per_pixel offset'
	}
}

fn test_native_layout_sg_environment_defaults() {
	$if macos {
		assert i64(sizeof(C.sg_environment_defaults)) == C.v_graphics_native_layout(70, -1), 'sg_environment_defaults size'
		assert i64(__offsetof(C.sg_environment_defaults, color_format)) == C.v_graphics_native_layout(70, 0), 'sg_environment_defaults.color_format offset'
		assert i64(__offsetof(C.sg_environment_defaults, depth_format)) == C.v_graphics_native_layout(70, 1), 'sg_environment_defaults.depth_format offset'
		assert i64(__offsetof(C.sg_environment_defaults, sample_count)) == C.v_graphics_native_layout(70, 2), 'sg_environment_defaults.sample_count offset'
	}
}

fn test_native_layout_sg_metal_environment() {
	$if macos {
		assert i64(sizeof(C.sg_metal_environment)) == C.v_graphics_native_layout(71, -1), 'sg_metal_environment size'
		assert i64(__offsetof(C.sg_metal_environment, device)) == C.v_graphics_native_layout(71, 0), 'sg_metal_environment.device offset'
	}
}

fn test_native_layout_sg_d3d11_environment() {
	$if macos {
		assert i64(sizeof(C.sg_d3d11_environment)) == C.v_graphics_native_layout(72, -1), 'sg_d3d11_environment size'
		assert i64(__offsetof(C.sg_d3d11_environment, device)) == C.v_graphics_native_layout(72, 0), 'sg_d3d11_environment.device offset'
		assert i64(__offsetof(C.sg_d3d11_environment, device_context)) == C.v_graphics_native_layout(72, 1), 'sg_d3d11_environment.device_context offset'
	}
}

fn test_native_layout_sg_wgpu_environment() {
	$if macos {
		assert i64(sizeof(C.sg_wgpu_environment)) == C.v_graphics_native_layout(73, -1), 'sg_wgpu_environment size'
		assert i64(__offsetof(C.sg_wgpu_environment, device)) == C.v_graphics_native_layout(73, 0), 'sg_wgpu_environment.device offset'
	}
}

fn test_native_layout_sg_environment() {
	$if macos {
		assert i64(sizeof(C.sg_environment)) == C.v_graphics_native_layout(74, -1), 'sg_environment size'
		assert i64(__offsetof(C.sg_environment, defaults)) == C.v_graphics_native_layout(74, 0), 'sg_environment.defaults offset'
		assert i64(__offsetof(C.sg_environment, metal)) == C.v_graphics_native_layout(74, 1), 'sg_environment.metal offset'
		assert i64(__offsetof(C.sg_environment, d3d11)) == C.v_graphics_native_layout(74, 2), 'sg_environment.d3d11 offset'
		assert i64(__offsetof(C.sg_environment, wgpu)) == C.v_graphics_native_layout(74, 3), 'sg_environment.wgpu offset'
	}
}

fn test_native_layout_sg_commit_listener() {
	$if macos {
		assert i64(sizeof(C.sg_commit_listener)) == C.v_graphics_native_layout(75, -1), 'sg_commit_listener size'
		assert i64(__offsetof(C.sg_commit_listener, func)) == C.v_graphics_native_layout(75, 0), 'sg_commit_listener.func offset'
		assert i64(__offsetof(C.sg_commit_listener, user_data)) == C.v_graphics_native_layout(75, 1), 'sg_commit_listener.user_data offset'
	}
}

fn test_native_layout_sg_trace_hooks() {
	$if macos {
		assert i64(sizeof(C.sg_trace_hooks)) == C.v_graphics_native_layout(76, -1), 'sg_trace_hooks size'
		assert i64(__offsetof(C.sg_trace_hooks, user_data)) == C.v_graphics_native_layout(76, 0), 'sg_trace_hooks.user_data offset'
		assert i64(__offsetof(C.sg_trace_hooks, reset_state_cache)) == C.v_graphics_native_layout(76, 1), 'sg_trace_hooks.reset_state_cache offset'
		assert i64(__offsetof(C.sg_trace_hooks, make_buffer)) == C.v_graphics_native_layout(76, 2), 'sg_trace_hooks.make_buffer offset'
		assert i64(__offsetof(C.sg_trace_hooks, make_image)) == C.v_graphics_native_layout(76, 3), 'sg_trace_hooks.make_image offset'
		assert i64(__offsetof(C.sg_trace_hooks, make_sampler)) == C.v_graphics_native_layout(76, 4), 'sg_trace_hooks.make_sampler offset'
		assert i64(__offsetof(C.sg_trace_hooks, make_shader)) == C.v_graphics_native_layout(76, 5), 'sg_trace_hooks.make_shader offset'
		assert i64(__offsetof(C.sg_trace_hooks, make_pipeline)) == C.v_graphics_native_layout(76, 6), 'sg_trace_hooks.make_pipeline offset'
		assert i64(__offsetof(C.sg_trace_hooks, make_attachments)) == C.v_graphics_native_layout(76, 7), 'sg_trace_hooks.make_attachments offset'
		assert i64(__offsetof(C.sg_trace_hooks, destroy_buffer)) == C.v_graphics_native_layout(76, 8), 'sg_trace_hooks.destroy_buffer offset'
		assert i64(__offsetof(C.sg_trace_hooks, destroy_image)) == C.v_graphics_native_layout(76, 9), 'sg_trace_hooks.destroy_image offset'
		assert i64(__offsetof(C.sg_trace_hooks, destroy_sampler)) == C.v_graphics_native_layout(76, 10), 'sg_trace_hooks.destroy_sampler offset'
		assert i64(__offsetof(C.sg_trace_hooks, destroy_shader)) == C.v_graphics_native_layout(76, 11), 'sg_trace_hooks.destroy_shader offset'
		assert i64(__offsetof(C.sg_trace_hooks, destroy_pipeline)) == C.v_graphics_native_layout(76, 12), 'sg_trace_hooks.destroy_pipeline offset'
		assert i64(__offsetof(C.sg_trace_hooks, destroy_attachments)) == C.v_graphics_native_layout(76, 13), 'sg_trace_hooks.destroy_attachments offset'
		assert i64(__offsetof(C.sg_trace_hooks, update_buffer)) == C.v_graphics_native_layout(76, 14), 'sg_trace_hooks.update_buffer offset'
		assert i64(__offsetof(C.sg_trace_hooks, update_image)) == C.v_graphics_native_layout(76, 15), 'sg_trace_hooks.update_image offset'
		assert i64(__offsetof(C.sg_trace_hooks, append_buffer)) == C.v_graphics_native_layout(76, 16), 'sg_trace_hooks.append_buffer offset'
		assert i64(__offsetof(C.sg_trace_hooks, begin_pass)) == C.v_graphics_native_layout(76, 17), 'sg_trace_hooks.begin_pass offset'
		assert i64(__offsetof(C.sg_trace_hooks, apply_viewport)) == C.v_graphics_native_layout(76, 18), 'sg_trace_hooks.apply_viewport offset'
		assert i64(__offsetof(C.sg_trace_hooks, apply_scissor_rect)) == C.v_graphics_native_layout(76, 19), 'sg_trace_hooks.apply_scissor_rect offset'
		assert i64(__offsetof(C.sg_trace_hooks, apply_pipeline)) == C.v_graphics_native_layout(76, 20), 'sg_trace_hooks.apply_pipeline offset'
		assert i64(__offsetof(C.sg_trace_hooks, apply_bindings)) == C.v_graphics_native_layout(76, 21), 'sg_trace_hooks.apply_bindings offset'
		assert i64(__offsetof(C.sg_trace_hooks, apply_uniforms)) == C.v_graphics_native_layout(76, 22), 'sg_trace_hooks.apply_uniforms offset'
		assert i64(__offsetof(C.sg_trace_hooks, draw)) == C.v_graphics_native_layout(76, 23), 'sg_trace_hooks.draw offset'
		assert i64(__offsetof(C.sg_trace_hooks, end_pass)) == C.v_graphics_native_layout(76, 24), 'sg_trace_hooks.end_pass offset'
		assert i64(__offsetof(C.sg_trace_hooks, commit)) == C.v_graphics_native_layout(76, 25), 'sg_trace_hooks.commit offset'
		assert i64(__offsetof(C.sg_trace_hooks, alloc_buffer)) == C.v_graphics_native_layout(76, 26), 'sg_trace_hooks.alloc_buffer offset'
		assert i64(__offsetof(C.sg_trace_hooks, alloc_image)) == C.v_graphics_native_layout(76, 27), 'sg_trace_hooks.alloc_image offset'
		assert i64(__offsetof(C.sg_trace_hooks, alloc_sampler)) == C.v_graphics_native_layout(76, 28), 'sg_trace_hooks.alloc_sampler offset'
		assert i64(__offsetof(C.sg_trace_hooks, alloc_shader)) == C.v_graphics_native_layout(76, 29), 'sg_trace_hooks.alloc_shader offset'
		assert i64(__offsetof(C.sg_trace_hooks, alloc_pipeline)) == C.v_graphics_native_layout(76, 30), 'sg_trace_hooks.alloc_pipeline offset'
		assert i64(__offsetof(C.sg_trace_hooks, alloc_attachments)) == C.v_graphics_native_layout(76, 31), 'sg_trace_hooks.alloc_attachments offset'
		assert i64(__offsetof(C.sg_trace_hooks, dealloc_buffer)) == C.v_graphics_native_layout(76, 32), 'sg_trace_hooks.dealloc_buffer offset'
		assert i64(__offsetof(C.sg_trace_hooks, dealloc_image)) == C.v_graphics_native_layout(76, 33), 'sg_trace_hooks.dealloc_image offset'
		assert i64(__offsetof(C.sg_trace_hooks, dealloc_sampler)) == C.v_graphics_native_layout(76, 34), 'sg_trace_hooks.dealloc_sampler offset'
		assert i64(__offsetof(C.sg_trace_hooks, dealloc_shader)) == C.v_graphics_native_layout(76, 35), 'sg_trace_hooks.dealloc_shader offset'
		assert i64(__offsetof(C.sg_trace_hooks, dealloc_pipeline)) == C.v_graphics_native_layout(76, 36), 'sg_trace_hooks.dealloc_pipeline offset'
		assert i64(__offsetof(C.sg_trace_hooks, dealloc_attachments)) == C.v_graphics_native_layout(76, 37), 'sg_trace_hooks.dealloc_attachments offset'
		assert i64(__offsetof(C.sg_trace_hooks, init_buffer)) == C.v_graphics_native_layout(76, 38), 'sg_trace_hooks.init_buffer offset'
		assert i64(__offsetof(C.sg_trace_hooks, init_image)) == C.v_graphics_native_layout(76, 39), 'sg_trace_hooks.init_image offset'
		assert i64(__offsetof(C.sg_trace_hooks, init_sampler)) == C.v_graphics_native_layout(76, 40), 'sg_trace_hooks.init_sampler offset'
		assert i64(__offsetof(C.sg_trace_hooks, init_shader)) == C.v_graphics_native_layout(76, 41), 'sg_trace_hooks.init_shader offset'
		assert i64(__offsetof(C.sg_trace_hooks, init_pipeline)) == C.v_graphics_native_layout(76, 42), 'sg_trace_hooks.init_pipeline offset'
		assert i64(__offsetof(C.sg_trace_hooks, init_attachments)) == C.v_graphics_native_layout(76, 43), 'sg_trace_hooks.init_attachments offset'
		assert i64(__offsetof(C.sg_trace_hooks, uninit_buffer)) == C.v_graphics_native_layout(76, 44), 'sg_trace_hooks.uninit_buffer offset'
		assert i64(__offsetof(C.sg_trace_hooks, uninit_image)) == C.v_graphics_native_layout(76, 45), 'sg_trace_hooks.uninit_image offset'
		assert i64(__offsetof(C.sg_trace_hooks, uninit_sampler)) == C.v_graphics_native_layout(76, 46), 'sg_trace_hooks.uninit_sampler offset'
		assert i64(__offsetof(C.sg_trace_hooks, uninit_shader)) == C.v_graphics_native_layout(76, 47), 'sg_trace_hooks.uninit_shader offset'
		assert i64(__offsetof(C.sg_trace_hooks, uninit_pipeline)) == C.v_graphics_native_layout(76, 48), 'sg_trace_hooks.uninit_pipeline offset'
		assert i64(__offsetof(C.sg_trace_hooks, uninit_attachments)) == C.v_graphics_native_layout(76, 49), 'sg_trace_hooks.uninit_attachments offset'
		assert i64(__offsetof(C.sg_trace_hooks, fail_buffer)) == C.v_graphics_native_layout(76, 50), 'sg_trace_hooks.fail_buffer offset'
		assert i64(__offsetof(C.sg_trace_hooks, fail_image)) == C.v_graphics_native_layout(76, 51), 'sg_trace_hooks.fail_image offset'
		assert i64(__offsetof(C.sg_trace_hooks, fail_sampler)) == C.v_graphics_native_layout(76, 52), 'sg_trace_hooks.fail_sampler offset'
		assert i64(__offsetof(C.sg_trace_hooks, fail_shader)) == C.v_graphics_native_layout(76, 53), 'sg_trace_hooks.fail_shader offset'
		assert i64(__offsetof(C.sg_trace_hooks, fail_pipeline)) == C.v_graphics_native_layout(76, 54), 'sg_trace_hooks.fail_pipeline offset'
		assert i64(__offsetof(C.sg_trace_hooks, fail_attachments)) == C.v_graphics_native_layout(76, 55), 'sg_trace_hooks.fail_attachments offset'
		assert i64(__offsetof(C.sg_trace_hooks, push_debug_group)) == C.v_graphics_native_layout(76, 56), 'sg_trace_hooks.push_debug_group offset'
		assert i64(__offsetof(C.sg_trace_hooks, pop_debug_group)) == C.v_graphics_native_layout(76, 57), 'sg_trace_hooks.pop_debug_group offset'
	}
}

fn test_native_layout_sg_sampler_info() {
	$if macos {
		assert i64(sizeof(C.sg_sampler_info)) == C.v_graphics_native_layout(77, -1), 'sg_sampler_info size'
		assert i64(__offsetof(C.sg_sampler_info, slot)) == C.v_graphics_native_layout(77, 0), 'sg_sampler_info.slot offset'
	}
}

fn test_native_layout_sg_allocator() {
	$if macos {
		assert i64(sizeof(C.sg_allocator)) == C.v_graphics_native_layout(78, -1), 'sg_allocator size'
		assert i64(__offsetof(C.sg_allocator, alloc_fn)) == C.v_graphics_native_layout(78, 0), 'sg_allocator.alloc_fn offset'
		assert i64(__offsetof(C.sg_allocator, free_fn)) == C.v_graphics_native_layout(78, 1), 'sg_allocator.free_fn offset'
		assert i64(__offsetof(C.sg_allocator, user_data)) == C.v_graphics_native_layout(78, 2), 'sg_allocator.user_data offset'
	}
}

fn test_native_layout_sg_logger() {
	$if macos {
		assert i64(sizeof(C.sg_logger)) == C.v_graphics_native_layout(79, -1), 'sg_logger size'
		assert i64(__offsetof(C.sg_logger, func)) == C.v_graphics_native_layout(79, 0), 'sg_logger.func offset'
		assert i64(__offsetof(C.sg_logger, user_data)) == C.v_graphics_native_layout(79, 1), 'sg_logger.user_data offset'
	}
}

fn test_native_layout_sapp_allocator() {
	$if macos {
		assert i64(sizeof(C.sapp_allocator)) == C.v_graphics_native_layout(80, -1), 'sapp_allocator size'
		assert i64(__offsetof(C.sapp_allocator, alloc_fn)) == C.v_graphics_native_layout(80, 0), 'sapp_allocator.alloc_fn offset'
		assert i64(__offsetof(C.sapp_allocator, free_fn)) == C.v_graphics_native_layout(80, 1), 'sapp_allocator.free_fn offset'
		assert i64(__offsetof(C.sapp_allocator, user_data)) == C.v_graphics_native_layout(80, 2), 'sapp_allocator.user_data offset'
	}
}

fn test_native_layout_sapp_logger() {
	$if macos {
		assert i64(sizeof(C.sapp_logger)) == C.v_graphics_native_layout(81, -1), 'sapp_logger size'
		assert i64(__offsetof(C.sapp_logger, func)) == C.v_graphics_native_layout(81, 0), 'sapp_logger.func offset'
		assert i64(__offsetof(C.sapp_logger, user_data)) == C.v_graphics_native_layout(81, 1), 'sapp_logger.user_data offset'
	}
}

fn test_native_layout_sapp_range() {
	$if macos {
		assert i64(sizeof(C.sapp_range)) == C.v_graphics_native_layout(82, -1), 'sapp_range size'
		assert i64(__offsetof(C.sapp_range, ptr)) == C.v_graphics_native_layout(82, 0), 'sapp_range.ptr offset'
		assert i64(__offsetof(C.sapp_range, size)) == C.v_graphics_native_layout(82, 1), 'sapp_range.size offset'
	}
}

fn test_native_layout_sapp_image_desc() {
	$if macos {
		assert i64(sizeof(C.sapp_image_desc)) == C.v_graphics_native_layout(83, -1), 'sapp_image_desc size'
		assert i64(__offsetof(C.sapp_image_desc, width)) == C.v_graphics_native_layout(83, 0), 'sapp_image_desc.width offset'
		assert i64(__offsetof(C.sapp_image_desc, height)) == C.v_graphics_native_layout(83, 1), 'sapp_image_desc.height offset'
		assert i64(__offsetof(C.sapp_image_desc, cursor_hotspot_x)) == C.v_graphics_native_layout(83, 2), 'sapp_image_desc.cursor_hotspot_x offset'
		assert i64(__offsetof(C.sapp_image_desc, cursor_hotspot_y)) == C.v_graphics_native_layout(83, 3), 'sapp_image_desc.cursor_hotspot_y offset'
		assert i64(__offsetof(C.sapp_image_desc, pixels)) == C.v_graphics_native_layout(83, 4), 'sapp_image_desc.pixels offset'
	}
}

fn test_native_layout_sapp_icon_desc() {
	$if macos {
		assert i64(sizeof(C.sapp_icon_desc)) == C.v_graphics_native_layout(84, -1), 'sapp_icon_desc size'
		assert i64(__offsetof(C.sapp_icon_desc, sokol_default)) == C.v_graphics_native_layout(84, 0), 'sapp_icon_desc.sokol_default offset'
		assert i64(__offsetof(C.sapp_icon_desc, images)) == C.v_graphics_native_layout(84, 1), 'sapp_icon_desc.images offset'
	}
}

fn test_native_layout_sapp_gl_desc() {
	$if macos {
		assert i64(sizeof(C.sapp_gl_desc)) == C.v_graphics_native_layout(85, -1), 'sapp_gl_desc size'
		assert i64(__offsetof(C.sapp_gl_desc, major_version)) == C.v_graphics_native_layout(85, 0), 'sapp_gl_desc.major_version offset'
		assert i64(__offsetof(C.sapp_gl_desc, minor_version)) == C.v_graphics_native_layout(85, 1), 'sapp_gl_desc.minor_version offset'
	}
}

fn test_native_layout_sapp_win32_desc() {
	$if macos {
		assert i64(sizeof(C.sapp_win32_desc)) == C.v_graphics_native_layout(86, -1), 'sapp_win32_desc size'
		assert i64(__offsetof(C.sapp_win32_desc, console_utf8)) == C.v_graphics_native_layout(86, 0), 'sapp_win32_desc.console_utf8 offset'
		assert i64(__offsetof(C.sapp_win32_desc, console_create)) == C.v_graphics_native_layout(86, 1), 'sapp_win32_desc.console_create offset'
		assert i64(__offsetof(C.sapp_win32_desc, console_attach)) == C.v_graphics_native_layout(86, 2), 'sapp_win32_desc.console_attach offset'
	}
}

fn test_native_layout_sapp_html5_desc() {
	$if macos {
		assert i64(sizeof(C.sapp_html5_desc)) == C.v_graphics_native_layout(87, -1), 'sapp_html5_desc size'
		assert i64(__offsetof(C.sapp_html5_desc, canvas_selector)) == C.v_graphics_native_layout(87, 0), 'sapp_html5_desc.canvas_selector offset'
		assert i64(__offsetof(C.sapp_html5_desc, canvas_resize)) == C.v_graphics_native_layout(87, 1), 'sapp_html5_desc.canvas_resize offset'
		assert i64(__offsetof(C.sapp_html5_desc, preserve_drawing_buffer)) == C.v_graphics_native_layout(87, 2), 'sapp_html5_desc.preserve_drawing_buffer offset'
		assert i64(__offsetof(C.sapp_html5_desc, premultiplied_alpha)) == C.v_graphics_native_layout(87, 3), 'sapp_html5_desc.premultiplied_alpha offset'
		assert i64(__offsetof(C.sapp_html5_desc, ask_leave_site)) == C.v_graphics_native_layout(87, 4), 'sapp_html5_desc.ask_leave_site offset'
		assert i64(__offsetof(C.sapp_html5_desc, update_document_title)) == C.v_graphics_native_layout(87, 5), 'sapp_html5_desc.update_document_title offset'
		assert i64(__offsetof(C.sapp_html5_desc, bubble_mouse_events)) == C.v_graphics_native_layout(87, 6), 'sapp_html5_desc.bubble_mouse_events offset'
		assert i64(__offsetof(C.sapp_html5_desc, bubble_touch_events)) == C.v_graphics_native_layout(87, 7), 'sapp_html5_desc.bubble_touch_events offset'
		assert i64(__offsetof(C.sapp_html5_desc, bubble_wheel_events)) == C.v_graphics_native_layout(87, 8), 'sapp_html5_desc.bubble_wheel_events offset'
		assert i64(__offsetof(C.sapp_html5_desc, bubble_key_events)) == C.v_graphics_native_layout(87, 9), 'sapp_html5_desc.bubble_key_events offset'
		assert i64(__offsetof(C.sapp_html5_desc, bubble_char_events)) == C.v_graphics_native_layout(87, 10), 'sapp_html5_desc.bubble_char_events offset'
		assert i64(__offsetof(C.sapp_html5_desc, use_emsc_set_main_loop)) == C.v_graphics_native_layout(87, 11), 'sapp_html5_desc.use_emsc_set_main_loop offset'
		assert i64(__offsetof(C.sapp_html5_desc, emsc_set_main_loop_simulate_infinite_loop)) == C.v_graphics_native_layout(87, 12), 'sapp_html5_desc.emsc_set_main_loop_simulate_infinite_loop offset'
	}
}

fn test_native_layout_sapp_ios_desc() {
	$if macos {
		assert i64(sizeof(C.sapp_ios_desc)) == C.v_graphics_native_layout(88, -1), 'sapp_ios_desc size'
		assert i64(__offsetof(C.sapp_ios_desc, keyboard_resizes_canvas)) == C.v_graphics_native_layout(88, 0), 'sapp_ios_desc.keyboard_resizes_canvas offset'
	}
}

fn test_native_layout_sapp_environment_defaults() {
	$if macos {
		assert i64(sizeof(C.sapp_environment_defaults)) == C.v_graphics_native_layout(89, -1), 'sapp_environment_defaults size'
		assert i64(__offsetof(C.sapp_environment_defaults, color_format)) == C.v_graphics_native_layout(89, 0), 'sapp_environment_defaults.color_format offset'
		assert i64(__offsetof(C.sapp_environment_defaults, depth_format)) == C.v_graphics_native_layout(89, 1), 'sapp_environment_defaults.depth_format offset'
		assert i64(__offsetof(C.sapp_environment_defaults, sample_count)) == C.v_graphics_native_layout(89, 2), 'sapp_environment_defaults.sample_count offset'
	}
}

fn test_native_layout_sapp_metal_environment() {
	$if macos {
		assert i64(sizeof(C.sapp_metal_environment)) == C.v_graphics_native_layout(90, -1), 'sapp_metal_environment size'
		assert i64(__offsetof(C.sapp_metal_environment, device)) == C.v_graphics_native_layout(90, 0), 'sapp_metal_environment.device offset'
	}
}

fn test_native_layout_sapp_d3d11_environment() {
	$if macos {
		assert i64(sizeof(C.sapp_d3d11_environment)) == C.v_graphics_native_layout(91, -1), 'sapp_d3d11_environment size'
		assert i64(__offsetof(C.sapp_d3d11_environment, device)) == C.v_graphics_native_layout(91, 0), 'sapp_d3d11_environment.device offset'
		assert i64(__offsetof(C.sapp_d3d11_environment, device_context)) == C.v_graphics_native_layout(91, 1), 'sapp_d3d11_environment.device_context offset'
	}
}

fn test_native_layout_sapp_wgpu_environment() {
	$if macos {
		assert i64(sizeof(C.sapp_wgpu_environment)) == C.v_graphics_native_layout(92, -1), 'sapp_wgpu_environment size'
		assert i64(__offsetof(C.sapp_wgpu_environment, device)) == C.v_graphics_native_layout(92, 0), 'sapp_wgpu_environment.device offset'
	}
}

fn test_native_layout_sapp_vulkan_environment() {
	$if macos {
		assert i64(sizeof(C.sapp_vulkan_environment)) == C.v_graphics_native_layout(93, -1), 'sapp_vulkan_environment size'
		assert i64(__offsetof(C.sapp_vulkan_environment, instance)) == C.v_graphics_native_layout(93, 0), 'sapp_vulkan_environment.instance offset'
		assert i64(__offsetof(C.sapp_vulkan_environment, physical_device)) == C.v_graphics_native_layout(93, 1), 'sapp_vulkan_environment.physical_device offset'
		assert i64(__offsetof(C.sapp_vulkan_environment, device)) == C.v_graphics_native_layout(93, 2), 'sapp_vulkan_environment.device offset'
		assert i64(__offsetof(C.sapp_vulkan_environment, queue)) == C.v_graphics_native_layout(93, 3), 'sapp_vulkan_environment.queue offset'
		assert i64(__offsetof(C.sapp_vulkan_environment, queue_family_index)) == C.v_graphics_native_layout(93, 4), 'sapp_vulkan_environment.queue_family_index offset'
	}
}

fn test_native_layout_sapp_environment() {
	$if macos {
		assert i64(sizeof(C.sapp_environment)) == C.v_graphics_native_layout(94, -1), 'sapp_environment size'
		assert i64(__offsetof(C.sapp_environment, defaults)) == C.v_graphics_native_layout(94, 0), 'sapp_environment.defaults offset'
		assert i64(__offsetof(C.sapp_environment, metal)) == C.v_graphics_native_layout(94, 1), 'sapp_environment.metal offset'
		assert i64(__offsetof(C.sapp_environment, d3d11)) == C.v_graphics_native_layout(94, 2), 'sapp_environment.d3d11 offset'
		assert i64(__offsetof(C.sapp_environment, wgpu)) == C.v_graphics_native_layout(94, 3), 'sapp_environment.wgpu offset'
		assert i64(__offsetof(C.sapp_environment, vulkan)) == C.v_graphics_native_layout(94, 4), 'sapp_environment.vulkan offset'
	}
}

fn test_native_layout_sapp_metal_swapchain() {
	$if macos {
		assert i64(sizeof(C.sapp_metal_swapchain)) == C.v_graphics_native_layout(95, -1), 'sapp_metal_swapchain size'
		assert i64(__offsetof(C.sapp_metal_swapchain, current_drawable)) == C.v_graphics_native_layout(95, 0), 'sapp_metal_swapchain.current_drawable offset'
		assert i64(__offsetof(C.sapp_metal_swapchain, depth_stencil_texture)) == C.v_graphics_native_layout(95, 1), 'sapp_metal_swapchain.depth_stencil_texture offset'
		assert i64(__offsetof(C.sapp_metal_swapchain, msaa_color_texture)) == C.v_graphics_native_layout(95, 2), 'sapp_metal_swapchain.msaa_color_texture offset'
	}
}

fn test_native_layout_sapp_d3d11_swapchain() {
	$if macos {
		assert i64(sizeof(C.sapp_d3d11_swapchain)) == C.v_graphics_native_layout(96, -1), 'sapp_d3d11_swapchain size'
		assert i64(__offsetof(C.sapp_d3d11_swapchain, render_view)) == C.v_graphics_native_layout(96, 0), 'sapp_d3d11_swapchain.render_view offset'
		assert i64(__offsetof(C.sapp_d3d11_swapchain, resolve_view)) == C.v_graphics_native_layout(96, 1), 'sapp_d3d11_swapchain.resolve_view offset'
		assert i64(__offsetof(C.sapp_d3d11_swapchain, depth_stencil_view)) == C.v_graphics_native_layout(96, 2), 'sapp_d3d11_swapchain.depth_stencil_view offset'
	}
}

fn test_native_layout_sapp_wgpu_swapchain() {
	$if macos {
		assert i64(sizeof(C.sapp_wgpu_swapchain)) == C.v_graphics_native_layout(97, -1), 'sapp_wgpu_swapchain size'
		assert i64(__offsetof(C.sapp_wgpu_swapchain, render_view)) == C.v_graphics_native_layout(97, 0), 'sapp_wgpu_swapchain.render_view offset'
		assert i64(__offsetof(C.sapp_wgpu_swapchain, resolve_view)) == C.v_graphics_native_layout(97, 1), 'sapp_wgpu_swapchain.resolve_view offset'
		assert i64(__offsetof(C.sapp_wgpu_swapchain, depth_stencil_view)) == C.v_graphics_native_layout(97, 2), 'sapp_wgpu_swapchain.depth_stencil_view offset'
	}
}

fn test_native_layout_sapp_vulkan_swapchain() {
	$if macos {
		assert i64(sizeof(C.sapp_vulkan_swapchain)) == C.v_graphics_native_layout(98, -1), 'sapp_vulkan_swapchain size'
		assert i64(__offsetof(C.sapp_vulkan_swapchain, render_image)) == C.v_graphics_native_layout(98, 0), 'sapp_vulkan_swapchain.render_image offset'
		assert i64(__offsetof(C.sapp_vulkan_swapchain, render_view)) == C.v_graphics_native_layout(98, 1), 'sapp_vulkan_swapchain.render_view offset'
		assert i64(__offsetof(C.sapp_vulkan_swapchain, resolve_image)) == C.v_graphics_native_layout(98, 2), 'sapp_vulkan_swapchain.resolve_image offset'
		assert i64(__offsetof(C.sapp_vulkan_swapchain, resolve_view)) == C.v_graphics_native_layout(98, 3), 'sapp_vulkan_swapchain.resolve_view offset'
		assert i64(__offsetof(C.sapp_vulkan_swapchain, depth_stencil_image)) == C.v_graphics_native_layout(98, 4), 'sapp_vulkan_swapchain.depth_stencil_image offset'
		assert i64(__offsetof(C.sapp_vulkan_swapchain, depth_stencil_view)) == C.v_graphics_native_layout(98, 5), 'sapp_vulkan_swapchain.depth_stencil_view offset'
		assert i64(__offsetof(C.sapp_vulkan_swapchain, render_finished_semaphore)) == C.v_graphics_native_layout(98, 6), 'sapp_vulkan_swapchain.render_finished_semaphore offset'
		assert i64(__offsetof(C.sapp_vulkan_swapchain, present_complete_semaphore)) == C.v_graphics_native_layout(98, 7), 'sapp_vulkan_swapchain.present_complete_semaphore offset'
	}
}

fn test_native_layout_sapp_gl_swapchain() {
	$if macos {
		assert i64(sizeof(C.sapp_gl_swapchain)) == C.v_graphics_native_layout(99, -1), 'sapp_gl_swapchain size'
		assert i64(__offsetof(C.sapp_gl_swapchain, framebuffer)) == C.v_graphics_native_layout(99, 0), 'sapp_gl_swapchain.framebuffer offset'
	}
}

fn test_native_layout_sapp_swapchain() {
	$if macos {
		assert i64(sizeof(C.sapp_swapchain)) == C.v_graphics_native_layout(100, -1), 'sapp_swapchain size'
		assert i64(__offsetof(C.sapp_swapchain, width)) == C.v_graphics_native_layout(100, 0), 'sapp_swapchain.width offset'
		assert i64(__offsetof(C.sapp_swapchain, height)) == C.v_graphics_native_layout(100, 1), 'sapp_swapchain.height offset'
		assert i64(__offsetof(C.sapp_swapchain, sample_count)) == C.v_graphics_native_layout(100, 2), 'sapp_swapchain.sample_count offset'
		assert i64(__offsetof(C.sapp_swapchain, color_format)) == C.v_graphics_native_layout(100, 3), 'sapp_swapchain.color_format offset'
		assert i64(__offsetof(C.sapp_swapchain, depth_format)) == C.v_graphics_native_layout(100, 4), 'sapp_swapchain.depth_format offset'
		assert i64(__offsetof(C.sapp_swapchain, metal)) == C.v_graphics_native_layout(100, 5), 'sapp_swapchain.metal offset'
		assert i64(__offsetof(C.sapp_swapchain, d3d11)) == C.v_graphics_native_layout(100, 6), 'sapp_swapchain.d3d11 offset'
		assert i64(__offsetof(C.sapp_swapchain, wgpu)) == C.v_graphics_native_layout(100, 7), 'sapp_swapchain.wgpu offset'
		assert i64(__offsetof(C.sapp_swapchain, vulkan)) == C.v_graphics_native_layout(100, 8), 'sapp_swapchain.vulkan offset'
		assert i64(__offsetof(C.sapp_swapchain, gl)) == C.v_graphics_native_layout(100, 9), 'sapp_swapchain.gl offset'
	}
}

fn test_native_layout_sapp_desc() {
	$if macos {
		assert i64(sizeof(C.sapp_desc)) == C.v_graphics_native_layout(101, -1), 'sapp_desc size'
		assert i64(__offsetof(C.sapp_desc, init_cb)) == C.v_graphics_native_layout(101, 0), 'sapp_desc.init_cb offset'
		assert i64(__offsetof(C.sapp_desc, frame_cb)) == C.v_graphics_native_layout(101, 1), 'sapp_desc.frame_cb offset'
		assert i64(__offsetof(C.sapp_desc, cleanup_cb)) == C.v_graphics_native_layout(101, 2), 'sapp_desc.cleanup_cb offset'
		assert i64(__offsetof(C.sapp_desc, event_cb)) == C.v_graphics_native_layout(101, 3), 'sapp_desc.event_cb offset'
		assert i64(__offsetof(C.sapp_desc, user_data)) == C.v_graphics_native_layout(101, 4), 'sapp_desc.user_data offset'
		assert i64(__offsetof(C.sapp_desc, init_userdata_cb)) == C.v_graphics_native_layout(101, 5), 'sapp_desc.init_userdata_cb offset'
		assert i64(__offsetof(C.sapp_desc, frame_userdata_cb)) == C.v_graphics_native_layout(101, 6), 'sapp_desc.frame_userdata_cb offset'
		assert i64(__offsetof(C.sapp_desc, cleanup_userdata_cb)) == C.v_graphics_native_layout(101, 7), 'sapp_desc.cleanup_userdata_cb offset'
		assert i64(__offsetof(C.sapp_desc, event_userdata_cb)) == C.v_graphics_native_layout(101, 8), 'sapp_desc.event_userdata_cb offset'
		assert i64(__offsetof(C.sapp_desc, width)) == C.v_graphics_native_layout(101, 9), 'sapp_desc.width offset'
		assert i64(__offsetof(C.sapp_desc, height)) == C.v_graphics_native_layout(101, 10), 'sapp_desc.height offset'
		assert i64(__offsetof(C.sapp_desc, sample_count)) == C.v_graphics_native_layout(101, 11), 'sapp_desc.sample_count offset'
		assert i64(__offsetof(C.sapp_desc, swap_interval)) == C.v_graphics_native_layout(101, 12), 'sapp_desc.swap_interval offset'
		assert i64(__offsetof(C.sapp_desc, high_dpi)) == C.v_graphics_native_layout(101, 13), 'sapp_desc.high_dpi offset'
		assert i64(__offsetof(C.sapp_desc, fullscreen)) == C.v_graphics_native_layout(101, 14), 'sapp_desc.fullscreen offset'
		assert i64(__offsetof(C.sapp_desc, alpha)) == C.v_graphics_native_layout(101, 15), 'sapp_desc.alpha offset'
		assert i64(__offsetof(C.sapp_desc, window_title)) == C.v_graphics_native_layout(101, 16), 'sapp_desc.window_title offset'
		assert i64(__offsetof(C.sapp_desc, enable_clipboard)) == C.v_graphics_native_layout(101, 17), 'sapp_desc.enable_clipboard offset'
		assert i64(__offsetof(C.sapp_desc, clipboard_size)) == C.v_graphics_native_layout(101, 18), 'sapp_desc.clipboard_size offset'
		assert i64(__offsetof(C.sapp_desc, enable_dragndrop)) == C.v_graphics_native_layout(101, 19), 'sapp_desc.enable_dragndrop offset'
		assert i64(__offsetof(C.sapp_desc, max_dropped_files)) == C.v_graphics_native_layout(101, 20), 'sapp_desc.max_dropped_files offset'
		assert i64(__offsetof(C.sapp_desc, max_dropped_file_path_length)) == C.v_graphics_native_layout(101, 21), 'sapp_desc.max_dropped_file_path_length offset'
		assert i64(__offsetof(C.sapp_desc, icon)) == C.v_graphics_native_layout(101, 22), 'sapp_desc.icon offset'
		assert i64(__offsetof(C.sapp_desc, allocator)) == C.v_graphics_native_layout(101, 23), 'sapp_desc.allocator offset'
		assert i64(__offsetof(C.sapp_desc, logger)) == C.v_graphics_native_layout(101, 24), 'sapp_desc.logger offset'
		assert i64(__offsetof(C.sapp_desc, gl)) == C.v_graphics_native_layout(101, 25), 'sapp_desc.gl offset'
		assert i64(__offsetof(C.sapp_desc, win32)) == C.v_graphics_native_layout(101, 26), 'sapp_desc.win32 offset'
		assert i64(__offsetof(C.sapp_desc, html5)) == C.v_graphics_native_layout(101, 27), 'sapp_desc.html5 offset'
		assert i64(__offsetof(C.sapp_desc, ios)) == C.v_graphics_native_layout(101, 28), 'sapp_desc.ios offset'
		assert i64(__offsetof(C.sapp_desc, __v_native_render)) == C.v_graphics_native_layout(101, 29), 'sapp_desc.__v_native_render offset'
		assert i64(__offsetof(C.sapp_desc, min_width)) == C.v_graphics_native_layout(101, 30), 'sapp_desc.min_width offset'
		assert i64(__offsetof(C.sapp_desc, min_height)) == C.v_graphics_native_layout(101, 31), 'sapp_desc.min_height offset'
		assert i64(__offsetof(C.sapp_desc, borderless_window)) == C.v_graphics_native_layout(101, 32), 'sapp_desc.borderless_window offset'
	}
}

fn test_native_layout_sapp_event() {
	$if macos {
		assert i64(sizeof(C.sapp_event)) == C.v_graphics_native_layout(102, -1), 'sapp_event size'
		assert i64(__offsetof(C.sapp_event, frame_count)) == C.v_graphics_native_layout(102, 0), 'sapp_event.frame_count offset'
		assert i64(__offsetof(C.sapp_event, type)) == C.v_graphics_native_layout(102, 1), 'sapp_event.type offset'
		assert i64(__offsetof(C.sapp_event, key_code)) == C.v_graphics_native_layout(102, 2), 'sapp_event.key_code offset'
		assert i64(__offsetof(C.sapp_event, char_code)) == C.v_graphics_native_layout(102, 3), 'sapp_event.char_code offset'
		assert i64(__offsetof(C.sapp_event, key_repeat)) == C.v_graphics_native_layout(102, 4), 'sapp_event.key_repeat offset'
		assert i64(__offsetof(C.sapp_event, modifiers)) == C.v_graphics_native_layout(102, 5), 'sapp_event.modifiers offset'
		assert i64(__offsetof(C.sapp_event, mouse_button)) == C.v_graphics_native_layout(102, 6), 'sapp_event.mouse_button offset'
		assert i64(__offsetof(C.sapp_event, mouse_x)) == C.v_graphics_native_layout(102, 7), 'sapp_event.mouse_x offset'
		assert i64(__offsetof(C.sapp_event, mouse_y)) == C.v_graphics_native_layout(102, 8), 'sapp_event.mouse_y offset'
		assert i64(__offsetof(C.sapp_event, mouse_dx)) == C.v_graphics_native_layout(102, 9), 'sapp_event.mouse_dx offset'
		assert i64(__offsetof(C.sapp_event, mouse_dy)) == C.v_graphics_native_layout(102, 10), 'sapp_event.mouse_dy offset'
		assert i64(__offsetof(C.sapp_event, scroll_x)) == C.v_graphics_native_layout(102, 11), 'sapp_event.scroll_x offset'
		assert i64(__offsetof(C.sapp_event, scroll_y)) == C.v_graphics_native_layout(102, 12), 'sapp_event.scroll_y offset'
		assert i64(__offsetof(C.sapp_event, num_touches)) == C.v_graphics_native_layout(102, 13), 'sapp_event.num_touches offset'
		assert i64(__offsetof(C.sapp_event, touches)) == C.v_graphics_native_layout(102, 14), 'sapp_event.touches offset'
		assert i64(__offsetof(C.sapp_event, window_width)) == C.v_graphics_native_layout(102, 15), 'sapp_event.window_width offset'
		assert i64(__offsetof(C.sapp_event, window_height)) == C.v_graphics_native_layout(102, 16), 'sapp_event.window_height offset'
		assert i64(__offsetof(C.sapp_event, framebuffer_width)) == C.v_graphics_native_layout(102, 17), 'sapp_event.framebuffer_width offset'
		assert i64(__offsetof(C.sapp_event, framebuffer_height)) == C.v_graphics_native_layout(102, 18), 'sapp_event.framebuffer_height offset'
	}
}

fn test_native_layout_sapp_touchpoint() {
	$if macos {
		assert i64(sizeof(C.sapp_touchpoint)) == C.v_graphics_native_layout(103, -1), 'sapp_touchpoint size'
		assert i64(__offsetof(C.sapp_touchpoint, identifier)) == C.v_graphics_native_layout(103, 0), 'sapp_touchpoint.identifier offset'
		assert i64(__offsetof(C.sapp_touchpoint, pos_x)) == C.v_graphics_native_layout(103, 1), 'sapp_touchpoint.pos_x offset'
		assert i64(__offsetof(C.sapp_touchpoint, pos_y)) == C.v_graphics_native_layout(103, 2), 'sapp_touchpoint.pos_y offset'
		assert i64(__offsetof(C.sapp_touchpoint, android_tooltype)) == C.v_graphics_native_layout(103, 3), 'sapp_touchpoint.android_tooltype offset'
		assert i64(__offsetof(C.sapp_touchpoint, changed)) == C.v_graphics_native_layout(103, 4), 'sapp_touchpoint.changed offset'
	}
}

fn test_native_layout_sgl_pipeline() {
	$if macos {
		assert i64(sizeof(C.sgl_pipeline)) == C.v_graphics_native_layout(104, -1), 'sgl_pipeline size'
		assert i64(__offsetof(C.sgl_pipeline, id)) == C.v_graphics_native_layout(104, 0), 'sgl_pipeline.id offset'
	}
}

fn test_native_layout_sgl_context() {
	$if macos {
		assert i64(sizeof(C.sgl_context)) == C.v_graphics_native_layout(105, -1), 'sgl_context size'
		assert i64(__offsetof(C.sgl_context, id)) == C.v_graphics_native_layout(105, 0), 'sgl_context.id offset'
	}
}

fn test_native_layout_sgl_context_desc_t() {
	$if macos {
		assert i64(sizeof(C.sgl_context_desc_t)) == C.v_graphics_native_layout(106, -1), 'sgl_context_desc_t size'
		assert i64(__offsetof(C.sgl_context_desc_t, max_vertices)) == C.v_graphics_native_layout(106, 0), 'sgl_context_desc_t.max_vertices offset'
		assert i64(__offsetof(C.sgl_context_desc_t, max_commands)) == C.v_graphics_native_layout(106, 1), 'sgl_context_desc_t.max_commands offset'
		assert i64(__offsetof(C.sgl_context_desc_t, color_format)) == C.v_graphics_native_layout(106, 2), 'sgl_context_desc_t.color_format offset'
		assert i64(__offsetof(C.sgl_context_desc_t, depth_format)) == C.v_graphics_native_layout(106, 3), 'sgl_context_desc_t.depth_format offset'
		assert i64(__offsetof(C.sgl_context_desc_t, sample_count)) == C.v_graphics_native_layout(106, 4), 'sgl_context_desc_t.sample_count offset'
	}
}

fn test_native_layout_sgl_desc_t() {
	$if macos {
		assert i64(sizeof(C.sgl_desc_t)) == C.v_graphics_native_layout(107, -1), 'sgl_desc_t size'
		assert i64(__offsetof(C.sgl_desc_t, max_vertices)) == C.v_graphics_native_layout(107, 0), 'sgl_desc_t.max_vertices offset'
		assert i64(__offsetof(C.sgl_desc_t, max_commands)) == C.v_graphics_native_layout(107, 1), 'sgl_desc_t.max_commands offset'
		assert i64(__offsetof(C.sgl_desc_t, context_pool_size)) == C.v_graphics_native_layout(107, 2), 'sgl_desc_t.context_pool_size offset'
		assert i64(__offsetof(C.sgl_desc_t, pipeline_pool_size)) == C.v_graphics_native_layout(107, 3), 'sgl_desc_t.pipeline_pool_size offset'
		assert i64(__offsetof(C.sgl_desc_t, color_format)) == C.v_graphics_native_layout(107, 4), 'sgl_desc_t.color_format offset'
		assert i64(__offsetof(C.sgl_desc_t, depth_format)) == C.v_graphics_native_layout(107, 5), 'sgl_desc_t.depth_format offset'
		assert i64(__offsetof(C.sgl_desc_t, sample_count)) == C.v_graphics_native_layout(107, 6), 'sgl_desc_t.sample_count offset'
		assert i64(__offsetof(C.sgl_desc_t, face_winding)) == C.v_graphics_native_layout(107, 7), 'sgl_desc_t.face_winding offset'
		assert i64(__offsetof(C.sgl_desc_t, allocator)) == C.v_graphics_native_layout(107, 8), 'sgl_desc_t.allocator offset'
		assert i64(__offsetof(C.sgl_desc_t, logger)) == C.v_graphics_native_layout(107, 9), 'sgl_desc_t.logger offset'
	}
}

fn test_native_layout_sgl_allocator_t() {
	$if macos {
		assert i64(sizeof(C.sgl_allocator_t)) == C.v_graphics_native_layout(108, -1), 'sgl_allocator_t size'
		assert i64(__offsetof(C.sgl_allocator_t, alloc_fn)) == C.v_graphics_native_layout(108, 0), 'sgl_allocator_t.alloc_fn offset'
		assert i64(__offsetof(C.sgl_allocator_t, free_fn)) == C.v_graphics_native_layout(108, 1), 'sgl_allocator_t.free_fn offset'
		assert i64(__offsetof(C.sgl_allocator_t, user_data)) == C.v_graphics_native_layout(108, 2), 'sgl_allocator_t.user_data offset'
	}
}

fn test_native_layout_sgl_logger_t() {
	$if macos {
		assert i64(sizeof(C.sgl_logger_t)) == C.v_graphics_native_layout(109, -1), 'sgl_logger_t size'
		assert i64(__offsetof(C.sgl_logger_t, func)) == C.v_graphics_native_layout(109, 0), 'sgl_logger_t.func offset'
		assert i64(__offsetof(C.sgl_logger_t, user_data)) == C.v_graphics_native_layout(109, 1), 'sgl_logger_t.user_data offset'
	}
}

fn test_native_layout_sfons_allocator_t() {
	$if macos {
		assert i64(sizeof(C.sfons_allocator_t)) == C.v_graphics_native_layout(110, -1), 'sfons_allocator_t size'
		assert i64(__offsetof(C.sfons_allocator_t, alloc_fn)) == C.v_graphics_native_layout(110, 0), 'sfons_allocator_t.alloc_fn offset'
		assert i64(__offsetof(C.sfons_allocator_t, free_fn)) == C.v_graphics_native_layout(110, 1), 'sfons_allocator_t.free_fn offset'
		assert i64(__offsetof(C.sfons_allocator_t, user_data)) == C.v_graphics_native_layout(110, 2), 'sfons_allocator_t.user_data offset'
	}
}

fn test_native_layout_sfons_desc_t() {
	$if macos {
		assert i64(sizeof(C.sfons_desc_t)) == C.v_graphics_native_layout(111, -1), 'sfons_desc_t size'
		assert i64(__offsetof(C.sfons_desc_t, width)) == C.v_graphics_native_layout(111, 0), 'sfons_desc_t.width offset'
		assert i64(__offsetof(C.sfons_desc_t, height)) == C.v_graphics_native_layout(111, 1), 'sfons_desc_t.height offset'
		assert i64(__offsetof(C.sfons_desc_t, allocator)) == C.v_graphics_native_layout(111, 2), 'sfons_desc_t.allocator offset'
	}
}

fn test_native_layout_saudio_allocator() {
	$if macos {
		assert i64(sizeof(C.saudio_allocator)) == C.v_graphics_native_layout(112, -1), 'saudio_allocator size'
		assert i64(__offsetof(C.saudio_allocator, alloc_fn)) == C.v_graphics_native_layout(112, 0), 'saudio_allocator.alloc_fn offset'
		assert i64(__offsetof(C.saudio_allocator, free_fn)) == C.v_graphics_native_layout(112, 1), 'saudio_allocator.free_fn offset'
		assert i64(__offsetof(C.saudio_allocator, user_data)) == C.v_graphics_native_layout(112, 2), 'saudio_allocator.user_data offset'
	}
}

fn test_native_layout_saudio_logger() {
	$if macos {
		assert i64(sizeof(C.saudio_logger)) == C.v_graphics_native_layout(113, -1), 'saudio_logger size'
		assert i64(__offsetof(C.saudio_logger, func)) == C.v_graphics_native_layout(113, 0), 'saudio_logger.func offset'
		assert i64(__offsetof(C.saudio_logger, user_data)) == C.v_graphics_native_layout(113, 1), 'saudio_logger.user_data offset'
	}
}

fn test_native_layout_saudio_desc() {
	$if macos {
		assert i64(sizeof(C.saudio_desc)) == C.v_graphics_native_layout(114, -1), 'saudio_desc size'
		assert i64(__offsetof(C.saudio_desc, sample_rate)) == C.v_graphics_native_layout(114, 0), 'saudio_desc.sample_rate offset'
		assert i64(__offsetof(C.saudio_desc, num_channels)) == C.v_graphics_native_layout(114, 1), 'saudio_desc.num_channels offset'
		assert i64(__offsetof(C.saudio_desc, buffer_frames)) == C.v_graphics_native_layout(114, 2), 'saudio_desc.buffer_frames offset'
		assert i64(__offsetof(C.saudio_desc, packet_frames)) == C.v_graphics_native_layout(114, 3), 'saudio_desc.packet_frames offset'
		assert i64(__offsetof(C.saudio_desc, num_packets)) == C.v_graphics_native_layout(114, 4), 'saudio_desc.num_packets offset'
		assert i64(__offsetof(C.saudio_desc, stream_cb)) == C.v_graphics_native_layout(114, 5), 'saudio_desc.stream_cb offset'
		assert i64(__offsetof(C.saudio_desc, stream_userdata_cb)) == C.v_graphics_native_layout(114, 6), 'saudio_desc.stream_userdata_cb offset'
		assert i64(__offsetof(C.saudio_desc, user_data)) == C.v_graphics_native_layout(114, 7), 'saudio_desc.user_data offset'
		assert i64(__offsetof(C.saudio_desc, allocator)) == C.v_graphics_native_layout(114, 8), 'saudio_desc.allocator offset'
		assert i64(__offsetof(C.saudio_desc, logger)) == C.v_graphics_native_layout(114, 9), 'saudio_desc.logger offset'
	}
}

fn test_native_layout_fonsparams() {
	$if macos {
		assert i64(sizeof(C.FONSparams)) == C.v_graphics_native_layout(115, -1), 'FONSparams size'
		assert i64(__offsetof(C.FONSparams, width)) == C.v_graphics_native_layout(115, 0), 'FONSparams.width offset'
		assert i64(__offsetof(C.FONSparams, height)) == C.v_graphics_native_layout(115, 1), 'FONSparams.height offset'
		assert i64(__offsetof(C.FONSparams, flags)) == C.v_graphics_native_layout(115, 2), 'FONSparams.flags offset'
		assert i64(__offsetof(C.FONSparams, userPtr)) == C.v_graphics_native_layout(115, 3), 'FONSparams.userPtr offset'
		assert i64(__offsetof(C.FONSparams, renderCreate)) == C.v_graphics_native_layout(115, 4), 'FONSparams.renderCreate offset'
		assert i64(__offsetof(C.FONSparams, renderResize)) == C.v_graphics_native_layout(115, 5), 'FONSparams.renderResize offset'
		assert i64(__offsetof(C.FONSparams, renderUpdate)) == C.v_graphics_native_layout(115, 6), 'FONSparams.renderUpdate offset'
		assert i64(__offsetof(C.FONSparams, renderDraw)) == C.v_graphics_native_layout(115, 7), 'FONSparams.renderDraw offset'
		assert i64(__offsetof(C.FONSparams, renderDelete)) == C.v_graphics_native_layout(115, 8), 'FONSparams.renderDelete offset'
	}
}

fn test_native_layout_fonsquad() {
	$if macos {
		assert i64(sizeof(C.FONSquad)) == C.v_graphics_native_layout(116, -1), 'FONSquad size'
		assert i64(__offsetof(C.FONSquad, x0)) == C.v_graphics_native_layout(116, 0), 'FONSquad.x0 offset'
		assert i64(__offsetof(C.FONSquad, y0)) == C.v_graphics_native_layout(116, 1), 'FONSquad.y0 offset'
		assert i64(__offsetof(C.FONSquad, s0)) == C.v_graphics_native_layout(116, 2), 'FONSquad.s0 offset'
		assert i64(__offsetof(C.FONSquad, t0)) == C.v_graphics_native_layout(116, 3), 'FONSquad.t0 offset'
		assert i64(__offsetof(C.FONSquad, x1)) == C.v_graphics_native_layout(116, 4), 'FONSquad.x1 offset'
		assert i64(__offsetof(C.FONSquad, y1)) == C.v_graphics_native_layout(116, 5), 'FONSquad.y1 offset'
		assert i64(__offsetof(C.FONSquad, s1)) == C.v_graphics_native_layout(116, 6), 'FONSquad.s1 offset'
		assert i64(__offsetof(C.FONSquad, t1)) == C.v_graphics_native_layout(116, 7), 'FONSquad.t1 offset'
	}
}

fn test_native_layout_fonstextiter() {
	$if macos {
		assert i64(sizeof(C.FONStextIter)) == C.v_graphics_native_layout(117, -1), 'FONStextIter size'
		assert i64(__offsetof(C.FONStextIter, x)) == C.v_graphics_native_layout(117, 0), 'FONStextIter.x offset'
		assert i64(__offsetof(C.FONStextIter, y)) == C.v_graphics_native_layout(117, 1), 'FONStextIter.y offset'
		assert i64(__offsetof(C.FONStextIter, nextx)) == C.v_graphics_native_layout(117, 2), 'FONStextIter.nextx offset'
		assert i64(__offsetof(C.FONStextIter, nexty)) == C.v_graphics_native_layout(117, 3), 'FONStextIter.nexty offset'
		assert i64(__offsetof(C.FONStextIter, scale)) == C.v_graphics_native_layout(117, 4), 'FONStextIter.scale offset'
		assert i64(__offsetof(C.FONStextIter, spacing)) == C.v_graphics_native_layout(117, 5), 'FONStextIter.spacing offset'
		assert i64(__offsetof(C.FONStextIter, codepoint)) == C.v_graphics_native_layout(117, 6), 'FONStextIter.codepoint offset'
		assert i64(__offsetof(C.FONStextIter, isize)) == C.v_graphics_native_layout(117, 7), 'FONStextIter.isize offset'
		assert i64(__offsetof(C.FONStextIter, iblur)) == C.v_graphics_native_layout(117, 8), 'FONStextIter.iblur offset'
		assert i64(__offsetof(C.FONStextIter, font)) == C.v_graphics_native_layout(117, 9), 'FONStextIter.font offset'
		assert i64(__offsetof(C.FONStextIter, prevGlyphIndex)) == C.v_graphics_native_layout(117, 10), 'FONStextIter.prevGlyphIndex offset'
		assert i64(__offsetof(C.FONStextIter, str)) == C.v_graphics_native_layout(117, 11), 'FONStextIter.str offset'
		assert i64(__offsetof(C.FONStextIter, next)) == C.v_graphics_native_layout(117, 12), 'FONStextIter.next offset'
		assert i64(__offsetof(C.FONStextIter, end)) == C.v_graphics_native_layout(117, 13), 'FONStextIter.end offset'
		assert i64(__offsetof(C.FONStextIter, utf8state)) == C.v_graphics_native_layout(117, 14), 'FONStextIter.utf8state offset'
	}
}

fn test_native_layout_fonsstate() {
	$if macos {
		assert i64(sizeof(C.FONSstate)) == C.v_graphics_native_layout(118, -1), 'FONSstate size'
		assert i64(__offsetof(C.FONSstate, font)) == C.v_graphics_native_layout(118, 0), 'FONSstate.font offset'
		assert i64(__offsetof(C.FONSstate, align)) == C.v_graphics_native_layout(118, 1), 'FONSstate.align offset'
		assert i64(__offsetof(C.FONSstate, size)) == C.v_graphics_native_layout(118, 2), 'FONSstate.size offset'
		assert i64(__offsetof(C.FONSstate, color)) == C.v_graphics_native_layout(118, 3), 'FONSstate.color offset'
		assert i64(__offsetof(C.FONSstate, blur)) == C.v_graphics_native_layout(118, 4), 'FONSstate.blur offset'
		assert i64(__offsetof(C.FONSstate, spacing)) == C.v_graphics_native_layout(118, 5), 'FONSstate.spacing offset'
	}
}

fn test_native_layout_fonscontext() {
	$if macos {
		assert i64(sizeof(C.FONScontext)) == C.v_graphics_native_layout(119, -1), 'FONScontext size'
		assert i64(__offsetof(C.FONScontext, params)) == C.v_graphics_native_layout(119, 0), 'FONScontext.params offset'
		assert i64(__offsetof(C.FONScontext, itw)) == C.v_graphics_native_layout(119, 1), 'FONScontext.itw offset'
		assert i64(__offsetof(C.FONScontext, ith)) == C.v_graphics_native_layout(119, 2), 'FONScontext.ith offset'
		assert i64(__offsetof(C.FONScontext, texData)) == C.v_graphics_native_layout(119, 3), 'FONScontext.texData offset'
		assert i64(__offsetof(C.FONScontext, dirtyRect)) == C.v_graphics_native_layout(119, 4), 'FONScontext.dirtyRect offset'
		assert i64(__offsetof(C.FONScontext, fonts)) == C.v_graphics_native_layout(119, 5), 'FONScontext.fonts offset'
		assert i64(__offsetof(C.FONScontext, atlas)) == C.v_graphics_native_layout(119, 6), 'FONScontext.atlas offset'
		assert i64(__offsetof(C.FONScontext, cfonts)) == C.v_graphics_native_layout(119, 7), 'FONScontext.cfonts offset'
		assert i64(__offsetof(C.FONScontext, nfonts)) == C.v_graphics_native_layout(119, 8), 'FONScontext.nfonts offset'
		assert i64(__offsetof(C.FONScontext, verts)) == C.v_graphics_native_layout(119, 9), 'FONScontext.verts offset'
		assert i64(__offsetof(C.FONScontext, tcoords)) == C.v_graphics_native_layout(119, 10), 'FONScontext.tcoords offset'
		assert i64(__offsetof(C.FONScontext, colors)) == C.v_graphics_native_layout(119, 11), 'FONScontext.colors offset'
		assert i64(__offsetof(C.FONScontext, nverts)) == C.v_graphics_native_layout(119, 12), 'FONScontext.nverts offset'
		assert i64(__offsetof(C.FONScontext, scratch)) == C.v_graphics_native_layout(119, 13), 'FONScontext.scratch offset'
		assert i64(__offsetof(C.FONScontext, nscratch)) == C.v_graphics_native_layout(119, 14), 'FONScontext.nscratch offset'
		assert i64(__offsetof(C.FONScontext, states)) == C.v_graphics_native_layout(119, 15), 'FONScontext.states offset'
		assert i64(__offsetof(C.FONScontext, nstates)) == C.v_graphics_native_layout(119, 16), 'FONScontext.nstates offset'
		assert i64(__offsetof(C.FONScontext, handleError)) == C.v_graphics_native_layout(119, 17), 'FONScontext.handleError offset'
		assert i64(__offsetof(C.FONScontext, errorUptr)) == C.v_graphics_native_layout(119, 18), 'FONScontext.errorUptr offset'
	}
}

fn test_native_layout_stb_vorbis_alloc() {
	$if macos {
		assert i64(sizeof(C.stb_vorbis_alloc)) == C.v_graphics_native_layout(120, -1), 'stb_vorbis_alloc size'
		assert i64(__offsetof(C.stb_vorbis_alloc, alloc_buffer)) == C.v_graphics_native_layout(120, 0), 'stb_vorbis_alloc.alloc_buffer offset'
		assert i64(__offsetof(C.stb_vorbis_alloc, alloc_buffer_length_in_bytes)) == C.v_graphics_native_layout(120, 1), 'stb_vorbis_alloc.alloc_buffer_length_in_bytes offset'
	}
}

fn test_native_layout_stb_vorbis_info() {
	$if macos {
		assert i64(sizeof(C.stb_vorbis_info)) == C.v_graphics_native_layout(121, -1), 'stb_vorbis_info size'
		assert i64(__offsetof(C.stb_vorbis_info, sample_rate)) == C.v_graphics_native_layout(121, 0), 'stb_vorbis_info.sample_rate offset'
		assert i64(__offsetof(C.stb_vorbis_info, channels)) == C.v_graphics_native_layout(121, 1), 'stb_vorbis_info.channels offset'
		assert i64(__offsetof(C.stb_vorbis_info, setup_memory_required)) == C.v_graphics_native_layout(121, 2), 'stb_vorbis_info.setup_memory_required offset'
		assert i64(__offsetof(C.stb_vorbis_info, setup_temp_memory_required)) == C.v_graphics_native_layout(121, 3), 'stb_vorbis_info.setup_temp_memory_required offset'
		assert i64(__offsetof(C.stb_vorbis_info, temp_memory_required)) == C.v_graphics_native_layout(121, 4), 'stb_vorbis_info.temp_memory_required offset'
		assert i64(__offsetof(C.stb_vorbis_info, max_frame_size)) == C.v_graphics_native_layout(121, 5), 'stb_vorbis_info.max_frame_size offset'
	}
}

fn test_native_layout_stb_vorbis_comment() {
	$if macos {
		assert i64(sizeof(C.stb_vorbis_comment)) == C.v_graphics_native_layout(122, -1), 'stb_vorbis_comment size'
		assert i64(__offsetof(C.stb_vorbis_comment, vendor)) == C.v_graphics_native_layout(122, 0), 'stb_vorbis_comment.vendor offset'
		assert i64(__offsetof(C.stb_vorbis_comment, comment_list_length)) == C.v_graphics_native_layout(122, 1), 'stb_vorbis_comment.comment_list_length offset'
		assert i64(__offsetof(C.stb_vorbis_comment, comment_list)) == C.v_graphics_native_layout(122, 2), 'stb_vorbis_comment.comment_list offset'
	}
}

fn test_native_layout_v_gg_color() {
	$if macos {
		assert i64(sizeof(C.v_gg_color)) == C.v_graphics_native_layout(123, -1), 'v_gg_color size'
		assert i64(__offsetof(C.v_gg_color, r)) == C.v_graphics_native_layout(123, 0), 'v_gg_color.r offset'
		assert i64(__offsetof(C.v_gg_color, g)) == C.v_graphics_native_layout(123, 1), 'v_gg_color.g offset'
		assert i64(__offsetof(C.v_gg_color, b)) == C.v_graphics_native_layout(123, 2), 'v_gg_color.b offset'
		assert i64(__offsetof(C.v_gg_color, a)) == C.v_graphics_native_layout(123, 3), 'v_gg_color.a offset'
	}
}

fn test_native_layout_v_gg_string() {
	$if macos {
		assert i64(sizeof(C.v_gg_string)) == C.v_graphics_native_layout(124, -1), 'v_gg_string size'
		assert i64(__offsetof(C.v_gg_string, str)) == C.v_graphics_native_layout(124, 0), 'v_gg_string.str offset'
		assert i64(__offsetof(C.v_gg_string, len)) == C.v_graphics_native_layout(124, 1), 'v_gg_string.len offset'
	}
}

fn test_native_layout_v_gg_text_cfg() {
	$if macos {
		assert i64(sizeof(C.v_gg_text_cfg)) == C.v_graphics_native_layout(125, -1), 'v_gg_text_cfg size'
		assert i64(__offsetof(C.v_gg_text_cfg, color)) == C.v_graphics_native_layout(125, 0), 'v_gg_text_cfg.color offset'
		assert i64(__offsetof(C.v_gg_text_cfg, size)) == C.v_graphics_native_layout(125, 1), 'v_gg_text_cfg.size offset'
		assert i64(__offsetof(C.v_gg_text_cfg, bold)) == C.v_graphics_native_layout(125, 2), 'v_gg_text_cfg.bold offset'
		assert i64(__offsetof(C.v_gg_text_cfg, mono)) == C.v_graphics_native_layout(125, 3), 'v_gg_text_cfg.mono offset'
	}
}

fn test_native_layout_v_gg_size() {
	$if macos {
		assert i64(sizeof(C.v_gg_size)) == C.v_graphics_native_layout(126, -1), 'v_gg_size size'
		assert i64(__offsetof(C.v_gg_size, width)) == C.v_graphics_native_layout(126, 0), 'v_gg_size.width offset'
		assert i64(__offsetof(C.v_gg_size, height)) == C.v_graphics_native_layout(126, 1), 'v_gg_size.height offset'
	}
}

fn test_native_layout_v_gg_image() {
	$if macos {
		assert i64(sizeof(C.v_gg_image)) == C.v_graphics_native_layout(127, -1), 'v_gg_image size'
		assert i64(__offsetof(C.v_gg_image, width)) == C.v_graphics_native_layout(127, 0), 'v_gg_image.width offset'
		assert i64(__offsetof(C.v_gg_image, height)) == C.v_graphics_native_layout(127, 1), 'v_gg_image.height offset'
		assert i64(__offsetof(C.v_gg_image, ok)) == C.v_graphics_native_layout(127, 2), 'v_gg_image.ok offset'
		assert i64(__offsetof(C.v_gg_image, data)) == C.v_graphics_native_layout(127, 3), 'v_gg_image.data offset'
	}
}
