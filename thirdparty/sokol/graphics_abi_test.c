// Native layout oracle, compiled with the shipped SDK headers.
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#define SOKOL_NO_DEPRECATED
#define FONS_STATIC
#define FONTSTASH_IMPLEMENTATION
#include "../fontstash/fontstash.h"
#include "../stb_vorbis/stb_vorbis.h"
#include "sokol_app.h"
#include "sokol_gfx.h"
#include "util/sokol_gl.h"
#include "sokol_audio.h"
#include "util/sokol_fontstash.h"
#include "../../vlib/gg/gg_darwin_abi.h"
int64_t v_graphics_native_layout(int32_t type, int32_t member) {
    switch (type) {
    case 0: switch (member) {
        case -1: return sizeof(sg_desc);
        case 0: return offsetof(sg_desc, _start_canary);
        case 1: return offsetof(sg_desc, buffer_pool_size);
        case 2: return offsetof(sg_desc, image_pool_size);
        case 3: return offsetof(sg_desc, sampler_pool_size);
        case 4: return offsetof(sg_desc, shader_pool_size);
        case 5: return offsetof(sg_desc, pipeline_pool_size);
        case 6: return offsetof(sg_desc, attachments_pool_size);
        case 7: return offsetof(sg_desc, uniform_buffer_size);
        case 8: return offsetof(sg_desc, max_commit_listeners);
        case 9: return offsetof(sg_desc, disable_validation);
        case 10: return offsetof(sg_desc, mtl_force_managed_storage_mode);
        case 11: return offsetof(sg_desc, mtl_use_command_buffer_with_retained_references);
        case 12: return offsetof(sg_desc, wgpu_disable_bindgroups_cache);
        case 13: return offsetof(sg_desc, wgpu_bindgroups_cache_size);
        case 14: return offsetof(sg_desc, allocator);
        case 15: return offsetof(sg_desc, logger);
        case 16: return offsetof(sg_desc, environment);
        case 17: return offsetof(sg_desc, _end_canary);
    } break;
    case 1: switch (member) {
        case -1: return sizeof(sg_color_target_state);
        case 0: return offsetof(sg_color_target_state, pixel_format);
        case 1: return offsetof(sg_color_target_state, write_mask);
        case 2: return offsetof(sg_color_target_state, blend);
    } break;
    case 2: switch (member) {
        case -1: return sizeof(sg_pipeline_desc);
        case 0: return offsetof(sg_pipeline_desc, _start_canary);
        case 1: return offsetof(sg_pipeline_desc, shader);
        case 2: return offsetof(sg_pipeline_desc, layout);
        case 3: return offsetof(sg_pipeline_desc, depth);
        case 4: return offsetof(sg_pipeline_desc, stencil);
        case 5: return offsetof(sg_pipeline_desc, color_count);
        case 6: return offsetof(sg_pipeline_desc, colors);
        case 7: return offsetof(sg_pipeline_desc, primitive_type);
        case 8: return offsetof(sg_pipeline_desc, index_type);
        case 9: return offsetof(sg_pipeline_desc, cull_mode);
        case 10: return offsetof(sg_pipeline_desc, face_winding);
        case 11: return offsetof(sg_pipeline_desc, sample_count);
        case 12: return offsetof(sg_pipeline_desc, blend_color);
        case 13: return offsetof(sg_pipeline_desc, alpha_to_coverage_enabled);
        case 14: return offsetof(sg_pipeline_desc, label);
        case 15: return offsetof(sg_pipeline_desc, _end_canary);
    } break;
    case 3: switch (member) {
        case -1: return sizeof(sg_pipeline_info);
        case 0: return offsetof(sg_pipeline_info, slot);
    } break;
    case 4: switch (member) {
        case -1: return sizeof(sg_pipeline);
        case 0: return offsetof(sg_pipeline, id);
    } break;
    case 5: switch (member) {
        case -1: return sizeof(sg_attachments);
        case 0: return offsetof(sg_attachments, id);
    } break;
    case 6: switch (member) {
        case -1: return sizeof(sg_attachment_desc);
        case 0: return offsetof(sg_attachment_desc, image);
        case 1: return offsetof(sg_attachment_desc, mip_level);
        case 2: return offsetof(sg_attachment_desc, slice);
    } break;
    case 7: switch (member) {
        case -1: return sizeof(sg_attachments_desc);
        case 0: return offsetof(sg_attachments_desc, _start_canary);
        case 1: return offsetof(sg_attachments_desc, colors);
        case 2: return offsetof(sg_attachments_desc, resolves);
        case 3: return offsetof(sg_attachments_desc, depth_stencil);
        case 4: return offsetof(sg_attachments_desc, label);
        case 5: return offsetof(sg_attachments_desc, _end_canary);
    } break;
    case 8: switch (member) {
        case -1: return sizeof(sg_attachments_info);
        case 0: return offsetof(sg_attachments_info, slot);
    } break;
    case 9: switch (member) {
        case -1: return sizeof(sg_bindings);
        case 0: return offsetof(sg_bindings, _start_canary);
        case 1: return offsetof(sg_bindings, vertex_buffers);
        case 2: return offsetof(sg_bindings, vertex_buffer_offsets);
        case 3: return offsetof(sg_bindings, index_buffer);
        case 4: return offsetof(sg_bindings, index_buffer_offset);
        case 5: return offsetof(sg_bindings, vs);
        case 6: return offsetof(sg_bindings, fs);
        case 7: return offsetof(sg_bindings, _end_canary);
    } break;
    case 10: switch (member) {
        case -1: return sizeof(sg_stage_bindings);
        case 0: return offsetof(sg_stage_bindings, images);
        case 1: return offsetof(sg_stage_bindings, samplers);
        case 2: return offsetof(sg_stage_bindings, storage_buffers);
    } break;
    case 11: switch (member) {
        case -1: return sizeof(sg_shader_desc);
        case 0: return offsetof(sg_shader_desc, _start_canary);
        case 1: return offsetof(sg_shader_desc, attrs);
        case 2: return offsetof(sg_shader_desc, vs);
        case 3: return offsetof(sg_shader_desc, fs);
        case 4: return offsetof(sg_shader_desc, label);
        case 5: return offsetof(sg_shader_desc, _end_canary);
    } break;
    case 12: switch (member) {
        case -1: return sizeof(sg_shader_attr_desc);
        case 0: return offsetof(sg_shader_attr_desc, name);
        case 1: return offsetof(sg_shader_attr_desc, sem_name);
        case 2: return offsetof(sg_shader_attr_desc, sem_index);
    } break;
    case 13: switch (member) {
        case -1: return sizeof(sg_shader_stage_desc);
        case 0: return offsetof(sg_shader_stage_desc, source);
        case 1: return offsetof(sg_shader_stage_desc, bytecode);
        case 2: return offsetof(sg_shader_stage_desc, entry);
        case 3: return offsetof(sg_shader_stage_desc, d3d11_target);
        case 4: return offsetof(sg_shader_stage_desc, uniform_blocks);
        case 5: return offsetof(sg_shader_stage_desc, storage_buffers);
        case 6: return offsetof(sg_shader_stage_desc, images);
        case 7: return offsetof(sg_shader_stage_desc, samplers);
        case 8: return offsetof(sg_shader_stage_desc, image_sampler_pairs);
    } break;
    case 14: switch (member) {
        case -1: return sizeof(sg_shader_uniform_block_desc);
        case 0: return offsetof(sg_shader_uniform_block_desc, size);
        case 1: return offsetof(sg_shader_uniform_block_desc, layout);
        case 2: return offsetof(sg_shader_uniform_block_desc, uniforms);
    } break;
    case 15: switch (member) {
        case -1: return sizeof(sg_shader_uniform_desc);
        case 0: return offsetof(sg_shader_uniform_desc, name);
        case 1: return offsetof(sg_shader_uniform_desc, type);
        case 2: return offsetof(sg_shader_uniform_desc, array_count);
    } break;
    case 16: switch (member) {
        case -1: return sizeof(sg_shader_image_desc);
        case 0: return offsetof(sg_shader_image_desc, used);
        case 1: return offsetof(sg_shader_image_desc, multisampled);
        case 2: return offsetof(sg_shader_image_desc, image_type);
        case 3: return offsetof(sg_shader_image_desc, sample_type);
    } break;
    case 17: switch (member) {
        case -1: return sizeof(sg_shader_storage_buffer_desc);
        case 0: return offsetof(sg_shader_storage_buffer_desc, used);
        case 1: return offsetof(sg_shader_storage_buffer_desc, readonly);
    } break;
    case 18: switch (member) {
        case -1: return sizeof(sg_shader_sampler_desc);
        case 0: return offsetof(sg_shader_sampler_desc, used);
        case 1: return offsetof(sg_shader_sampler_desc, sampler_type);
    } break;
    case 19: switch (member) {
        case -1: return sizeof(sg_shader_image_sampler_pair_desc);
        case 0: return offsetof(sg_shader_image_sampler_pair_desc, used);
        case 1: return offsetof(sg_shader_image_sampler_pair_desc, image_slot);
        case 2: return offsetof(sg_shader_image_sampler_pair_desc, sampler_slot);
        case 3: return offsetof(sg_shader_image_sampler_pair_desc, glsl_name);
    } break;
    case 20: switch (member) {
        case -1: return sizeof(sg_shader_info);
        case 0: return offsetof(sg_shader_info, slot);
    } break;
    case 21: switch (member) {
        case -1: return sizeof(sg_range);
        case 0: return offsetof(sg_range, ptr);
        case 1: return offsetof(sg_range, size);
    } break;
    case 22: switch (member) {
        case -1: return sizeof(sg_color);
        case 0: return offsetof(sg_color, r);
        case 1: return offsetof(sg_color, g);
        case 2: return offsetof(sg_color, b);
        case 3: return offsetof(sg_color, a);
    } break;
    case 23: switch (member) {
        case -1: return sizeof(sg_shader);
        case 0: return offsetof(sg_shader, id);
    } break;
    case 24: switch (member) {
        case -1: return sizeof(sg_frame_stats_gl);
        case 0: return offsetof(sg_frame_stats_gl, num_bind_buffer);
        case 1: return offsetof(sg_frame_stats_gl, num_active_texture);
        case 2: return offsetof(sg_frame_stats_gl, num_bind_texture);
        case 3: return offsetof(sg_frame_stats_gl, num_bind_sampler);
        case 4: return offsetof(sg_frame_stats_gl, num_use_program);
        case 5: return offsetof(sg_frame_stats_gl, num_render_state);
        case 6: return offsetof(sg_frame_stats_gl, num_vertex_attrib_pointer);
        case 7: return offsetof(sg_frame_stats_gl, num_vertex_attrib_divisor);
        case 8: return offsetof(sg_frame_stats_gl, num_enable_vertex_attrib_array);
        case 9: return offsetof(sg_frame_stats_gl, num_disable_vertex_attrib_array);
        case 10: return offsetof(sg_frame_stats_gl, num_uniform);
    } break;
    case 25: switch (member) {
        case -1: return sizeof(sg_frame_stats_d3d11_pass);
        case 0: return offsetof(sg_frame_stats_d3d11_pass, num_om_set_render_targets);
        case 1: return offsetof(sg_frame_stats_d3d11_pass, num_clear_render_target_view);
        case 2: return offsetof(sg_frame_stats_d3d11_pass, num_clear_depth_stencil_view);
        case 3: return offsetof(sg_frame_stats_d3d11_pass, num_resolve_subresource);
    } break;
    case 26: switch (member) {
        case -1: return sizeof(sg_frame_stats_d3d11_pipeline);
        case 0: return offsetof(sg_frame_stats_d3d11_pipeline, num_rs_set_state);
        case 1: return offsetof(sg_frame_stats_d3d11_pipeline, num_om_set_depth_stencil_state);
        case 2: return offsetof(sg_frame_stats_d3d11_pipeline, num_om_set_blend_state);
        case 3: return offsetof(sg_frame_stats_d3d11_pipeline, num_ia_set_primitive_topology);
        case 4: return offsetof(sg_frame_stats_d3d11_pipeline, num_ia_set_input_layout);
        case 5: return offsetof(sg_frame_stats_d3d11_pipeline, num_vs_set_shader);
        case 6: return offsetof(sg_frame_stats_d3d11_pipeline, num_vs_set_constant_buffers);
        case 7: return offsetof(sg_frame_stats_d3d11_pipeline, num_ps_set_shader);
        case 8: return offsetof(sg_frame_stats_d3d11_pipeline, num_ps_set_constant_buffers);
    } break;
    case 27: switch (member) {
        case -1: return sizeof(sg_frame_stats_d3d11_bindings);
        case 0: return offsetof(sg_frame_stats_d3d11_bindings, num_ia_set_vertex_buffers);
        case 1: return offsetof(sg_frame_stats_d3d11_bindings, num_ia_set_index_buffer);
        case 2: return offsetof(sg_frame_stats_d3d11_bindings, num_vs_set_shader_resources);
        case 3: return offsetof(sg_frame_stats_d3d11_bindings, num_ps_set_shader_resources);
        case 4: return offsetof(sg_frame_stats_d3d11_bindings, num_vs_set_samplers);
        case 5: return offsetof(sg_frame_stats_d3d11_bindings, num_ps_set_samplers);
    } break;
    case 28: switch (member) {
        case -1: return sizeof(sg_frame_stats_d3d11_uniforms);
        case 0: return offsetof(sg_frame_stats_d3d11_uniforms, num_update_subresource);
    } break;
    case 29: switch (member) {
        case -1: return sizeof(sg_frame_stats_d3d11_draw);
        case 0: return offsetof(sg_frame_stats_d3d11_draw, num_draw_indexed_instanced);
        case 1: return offsetof(sg_frame_stats_d3d11_draw, num_draw_indexed);
        case 2: return offsetof(sg_frame_stats_d3d11_draw, num_draw_instanced);
        case 3: return offsetof(sg_frame_stats_d3d11_draw, num_draw);
    } break;
    case 30: switch (member) {
        case -1: return sizeof(sg_frame_stats_d3d11);
        case 0: return offsetof(sg_frame_stats_d3d11, pass);
        case 1: return offsetof(sg_frame_stats_d3d11, pipeline);
        case 2: return offsetof(sg_frame_stats_d3d11, bindings);
        case 3: return offsetof(sg_frame_stats_d3d11, uniforms);
        case 4: return offsetof(sg_frame_stats_d3d11, draw);
        case 5: return offsetof(sg_frame_stats_d3d11, num_map);
        case 6: return offsetof(sg_frame_stats_d3d11, num_unmap);
    } break;
    case 31: switch (member) {
        case -1: return sizeof(sg_frame_stats_metal_idpool);
        case 0: return offsetof(sg_frame_stats_metal_idpool, num_added);
        case 1: return offsetof(sg_frame_stats_metal_idpool, num_released);
        case 2: return offsetof(sg_frame_stats_metal_idpool, num_garbage_collected);
    } break;
    case 32: switch (member) {
        case -1: return sizeof(sg_frame_stats_metal_pipeline);
        case 0: return offsetof(sg_frame_stats_metal_pipeline, num_set_blend_color);
        case 1: return offsetof(sg_frame_stats_metal_pipeline, num_set_cull_mode);
        case 2: return offsetof(sg_frame_stats_metal_pipeline, num_set_front_facing_winding);
        case 3: return offsetof(sg_frame_stats_metal_pipeline, num_set_stencil_reference_value);
        case 4: return offsetof(sg_frame_stats_metal_pipeline, num_set_depth_bias);
        case 5: return offsetof(sg_frame_stats_metal_pipeline, num_set_render_pipeline_state);
        case 6: return offsetof(sg_frame_stats_metal_pipeline, num_set_depth_stencil_state);
    } break;
    case 33: switch (member) {
        case -1: return sizeof(sg_frame_stats_metal_bindings);
        case 0: return offsetof(sg_frame_stats_metal_bindings, num_set_vertex_buffer);
        case 1: return offsetof(sg_frame_stats_metal_bindings, num_set_vertex_texture);
        case 2: return offsetof(sg_frame_stats_metal_bindings, num_set_vertex_sampler_state);
        case 3: return offsetof(sg_frame_stats_metal_bindings, num_set_fragment_buffer);
        case 4: return offsetof(sg_frame_stats_metal_bindings, num_set_fragment_texture);
        case 5: return offsetof(sg_frame_stats_metal_bindings, num_set_fragment_sampler_state);
    } break;
    case 34: switch (member) {
        case -1: return sizeof(sg_frame_stats_metal_uniforms);
        case 0: return offsetof(sg_frame_stats_metal_uniforms, num_set_vertex_buffer_offset);
        case 1: return offsetof(sg_frame_stats_metal_uniforms, num_set_fragment_buffer_offset);
    } break;
    case 35: switch (member) {
        case -1: return sizeof(sg_frame_stats_metal);
        case 0: return offsetof(sg_frame_stats_metal, idpool);
        case 1: return offsetof(sg_frame_stats_metal, pipeline);
        case 2: return offsetof(sg_frame_stats_metal, bindings);
        case 3: return offsetof(sg_frame_stats_metal, uniforms);
    } break;
    case 36: switch (member) {
        case -1: return sizeof(sg_frame_stats_wgpu_uniforms);
        case 0: return offsetof(sg_frame_stats_wgpu_uniforms, num_set_bindgroup);
        case 1: return offsetof(sg_frame_stats_wgpu_uniforms, size_write_buffer);
    } break;
    case 37: switch (member) {
        case -1: return sizeof(sg_frame_stats_wgpu_bindings);
        case 0: return offsetof(sg_frame_stats_wgpu_bindings, num_set_vertex_buffer);
        case 1: return offsetof(sg_frame_stats_wgpu_bindings, num_skip_redundant_vertex_buffer);
        case 2: return offsetof(sg_frame_stats_wgpu_bindings, num_set_index_buffer);
        case 3: return offsetof(sg_frame_stats_wgpu_bindings, num_skip_redundant_index_buffer);
        case 4: return offsetof(sg_frame_stats_wgpu_bindings, num_create_bindgroup);
        case 5: return offsetof(sg_frame_stats_wgpu_bindings, num_discard_bindgroup);
        case 6: return offsetof(sg_frame_stats_wgpu_bindings, num_set_bindgroup);
        case 7: return offsetof(sg_frame_stats_wgpu_bindings, num_skip_redundant_bindgroup);
        case 8: return offsetof(sg_frame_stats_wgpu_bindings, num_bindgroup_cache_hits);
        case 9: return offsetof(sg_frame_stats_wgpu_bindings, num_bindgroup_cache_misses);
        case 10: return offsetof(sg_frame_stats_wgpu_bindings, num_bindgroup_cache_collisions);
        case 11: return offsetof(sg_frame_stats_wgpu_bindings, num_bindgroup_cache_hash_vs_key_mismatch);
    } break;
    case 38: switch (member) {
        case -1: return sizeof(sg_frame_stats_wgpu);
        case 0: return offsetof(sg_frame_stats_wgpu, uniforms);
        case 1: return offsetof(sg_frame_stats_wgpu, bindings);
    } break;
    case 39: switch (member) {
        case -1: return sizeof(sg_frame_stats);
        case 0: return offsetof(sg_frame_stats, frame_index);
        case 1: return offsetof(sg_frame_stats, num_passes);
        case 2: return offsetof(sg_frame_stats, num_apply_viewport);
        case 3: return offsetof(sg_frame_stats, num_apply_scissor_rect);
        case 4: return offsetof(sg_frame_stats, num_apply_pipeline);
        case 5: return offsetof(sg_frame_stats, num_apply_bindings);
        case 6: return offsetof(sg_frame_stats, num_apply_uniforms);
        case 7: return offsetof(sg_frame_stats, num_draw);
        case 8: return offsetof(sg_frame_stats, num_update_buffer);
        case 9: return offsetof(sg_frame_stats, num_append_buffer);
        case 10: return offsetof(sg_frame_stats, num_update_image);
        case 11: return offsetof(sg_frame_stats, size_apply_uniforms);
        case 12: return offsetof(sg_frame_stats, size_update_buffer);
        case 13: return offsetof(sg_frame_stats, size_append_buffer);
        case 14: return offsetof(sg_frame_stats, size_update_image);
        case 15: return offsetof(sg_frame_stats, gl);
        case 16: return offsetof(sg_frame_stats, d3d11);
        case 17: return offsetof(sg_frame_stats, metal);
        case 18: return offsetof(sg_frame_stats, wgpu);
    } break;
    case 40: switch (member) {
        case -1: return sizeof(sg_pass_action);
        case 0: return offsetof(sg_pass_action, colors);
        case 1: return offsetof(sg_pass_action, depth);
        case 2: return offsetof(sg_pass_action, stencil);
    } break;
    case 41: switch (member) {
        case -1: return sizeof(sg_metal_swapchain);
        case 0: return offsetof(sg_metal_swapchain, current_drawable);
        case 1: return offsetof(sg_metal_swapchain, depth_stencil_texture);
        case 2: return offsetof(sg_metal_swapchain, msaa_color_texture);
    } break;
    case 42: switch (member) {
        case -1: return sizeof(sg_d3d11_swapchain);
        case 0: return offsetof(sg_d3d11_swapchain, render_view);
        case 1: return offsetof(sg_d3d11_swapchain, resolve_view);
        case 2: return offsetof(sg_d3d11_swapchain, depth_stencil_view);
    } break;
    case 43: switch (member) {
        case -1: return sizeof(sg_wgpu_swapchain);
        case 0: return offsetof(sg_wgpu_swapchain, render_view);
        case 1: return offsetof(sg_wgpu_swapchain, resolve_view);
        case 2: return offsetof(sg_wgpu_swapchain, depth_stencil_view);
    } break;
    case 44: switch (member) {
        case -1: return sizeof(sg_gl_swapchain);
        case 0: return offsetof(sg_gl_swapchain, framebuffer);
    } break;
    case 45: switch (member) {
        case -1: return sizeof(sg_swapchain);
        case 0: return offsetof(sg_swapchain, width);
        case 1: return offsetof(sg_swapchain, height);
        case 2: return offsetof(sg_swapchain, sample_count);
        case 3: return offsetof(sg_swapchain, color_format);
        case 4: return offsetof(sg_swapchain, depth_format);
        case 5: return offsetof(sg_swapchain, metal);
        case 6: return offsetof(sg_swapchain, d3d11);
        case 7: return offsetof(sg_swapchain, wgpu);
        case 8: return offsetof(sg_swapchain, gl);
    } break;
    case 46: switch (member) {
        case -1: return sizeof(sg_pass);
        case 0: return offsetof(sg_pass, _start_canary);
        case 1: return offsetof(sg_pass, action);
        case 2: return offsetof(sg_pass, attachments);
        case 3: return offsetof(sg_pass, swapchain);
        case 4: return offsetof(sg_pass, label);
        case 5: return offsetof(sg_pass, _end_canary);
    } break;
    case 47: switch (member) {
        case -1: return sizeof(sg_buffer_desc);
        case 0: return offsetof(sg_buffer_desc, _start_canary);
        case 1: return offsetof(sg_buffer_desc, size);
        case 2: return offsetof(sg_buffer_desc, type);
        case 3: return offsetof(sg_buffer_desc, usage);
        case 4: return offsetof(sg_buffer_desc, data);
        case 5: return offsetof(sg_buffer_desc, label);
        case 6: return offsetof(sg_buffer_desc, gl_buffers);
        case 7: return offsetof(sg_buffer_desc, mtl_buffers);
        case 8: return offsetof(sg_buffer_desc, d3d11_buffer);
        case 9: return offsetof(sg_buffer_desc, wgpu_buffer);
        case 10: return offsetof(sg_buffer_desc, _end_canary);
    } break;
    case 48: switch (member) {
        case -1: return sizeof(sg_slot_info);
        case 0: return offsetof(sg_slot_info, state);
        case 1: return offsetof(sg_slot_info, res_id);
    } break;
    case 49: switch (member) {
        case -1: return sizeof(sg_buffer_info);
        case 0: return offsetof(sg_buffer_info, slot);
        case 1: return offsetof(sg_buffer_info, update_frame_index);
        case 2: return offsetof(sg_buffer_info, append_frame_index);
        case 3: return offsetof(sg_buffer_info, append_pos);
        case 4: return offsetof(sg_buffer_info, append_overflow);
        case 5: return offsetof(sg_buffer_info, num_slots);
        case 6: return offsetof(sg_buffer_info, active_slot);
    } break;
    case 50: switch (member) {
        case -1: return sizeof(sg_buffer);
        case 0: return offsetof(sg_buffer, id);
    } break;
    case 51: switch (member) {
        case -1: return sizeof(sg_image_desc);
        case 0: return offsetof(sg_image_desc, _start_canary);
        case 1: return offsetof(sg_image_desc, type);
        case 2: return offsetof(sg_image_desc, render_target);
        case 3: return offsetof(sg_image_desc, width);
        case 4: return offsetof(sg_image_desc, height);
        case 5: return offsetof(sg_image_desc, num_slices);
        case 6: return offsetof(sg_image_desc, num_mipmaps);
        case 7: return offsetof(sg_image_desc, usage);
        case 8: return offsetof(sg_image_desc, pixel_format);
        case 9: return offsetof(sg_image_desc, sample_count);
        case 10: return offsetof(sg_image_desc, data);
        case 11: return offsetof(sg_image_desc, label);
        case 12: return offsetof(sg_image_desc, gl_textures);
        case 13: return offsetof(sg_image_desc, gl_texture_target);
        case 14: return offsetof(sg_image_desc, mtl_textures);
        case 15: return offsetof(sg_image_desc, d3d11_texture);
        case 16: return offsetof(sg_image_desc, d3d11_shader_resource_view);
        case 17: return offsetof(sg_image_desc, wgpu_texture);
        case 18: return offsetof(sg_image_desc, wgpu_texture_view);
        case 19: return offsetof(sg_image_desc, _end_canary);
    } break;
    case 52: switch (member) {
        case -1: return sizeof(sg_sampler_desc);
        case 0: return offsetof(sg_sampler_desc, _start_canary);
        case 1: return offsetof(sg_sampler_desc, min_filter);
        case 2: return offsetof(sg_sampler_desc, mag_filter);
        case 3: return offsetof(sg_sampler_desc, mipmap_filter);
        case 4: return offsetof(sg_sampler_desc, wrap_u);
        case 5: return offsetof(sg_sampler_desc, wrap_v);
        case 6: return offsetof(sg_sampler_desc, wrap_w);
        case 7: return offsetof(sg_sampler_desc, min_lod);
        case 8: return offsetof(sg_sampler_desc, max_lod);
        case 9: return offsetof(sg_sampler_desc, border_color);
        case 10: return offsetof(sg_sampler_desc, compare);
        case 11: return offsetof(sg_sampler_desc, max_anisotropy);
        case 12: return offsetof(sg_sampler_desc, label);
        case 13: return offsetof(sg_sampler_desc, gl_sampler);
        case 14: return offsetof(sg_sampler_desc, mtl_sampler);
        case 15: return offsetof(sg_sampler_desc, d3d11_sampler);
        case 16: return offsetof(sg_sampler_desc, wgpu_sampler);
        case 17: return offsetof(sg_sampler_desc, _end_canary);
    } break;
    case 53: switch (member) {
        case -1: return sizeof(sg_image_info);
        case 0: return offsetof(sg_image_info, slot);
        case 1: return offsetof(sg_image_info, upd_frame_index);
        case 2: return offsetof(sg_image_info, num_slots);
        case 3: return offsetof(sg_image_info, active_slot);
    } break;
    case 54: switch (member) {
        case -1: return sizeof(sg_image);
        case 0: return offsetof(sg_image, id);
    } break;
    case 55: switch (member) {
        case -1: return sizeof(sg_sampler);
        case 0: return offsetof(sg_sampler, id);
    } break;
    case 56: switch (member) {
        case -1: return sizeof(sg_image_data);
        case 0: return offsetof(sg_image_data, subimage);
    } break;
    case 57: switch (member) {
        case -1: return sizeof(sg_features);
        case 0: return offsetof(sg_features, origin_top_left);
        case 1: return offsetof(sg_features, image_clamp_to_border);
        case 2: return offsetof(sg_features, mrt_independent_blend_state);
        case 3: return offsetof(sg_features, mrt_independent_write_mask);
        case 4: return offsetof(sg_features, storage_buffer);
    } break;
    case 58: switch (member) {
        case -1: return sizeof(sg_limits);
        case 0: return offsetof(sg_limits, max_image_size_2d);
        case 1: return offsetof(sg_limits, max_image_size_cube);
        case 2: return offsetof(sg_limits, max_image_size_3d);
        case 3: return offsetof(sg_limits, max_image_size_array);
        case 4: return offsetof(sg_limits, max_image_array_layers);
        case 5: return offsetof(sg_limits, max_vertex_attrs);
        case 6: return offsetof(sg_limits, gl_max_vertex_uniform_components);
        case 7: return offsetof(sg_limits, gl_max_combined_texture_image_units);
    } break;
    case 59: switch (member) {
        case -1: return sizeof(sg_vertex_layout_state);
        case 0: return offsetof(sg_vertex_layout_state, buffers);
        case 1: return offsetof(sg_vertex_layout_state, attrs);
    } break;
    case 60: switch (member) {
        case -1: return sizeof(sg_vertex_buffer_layout_state);
        case 0: return offsetof(sg_vertex_buffer_layout_state, stride);
        case 1: return offsetof(sg_vertex_buffer_layout_state, step_func);
        case 2: return offsetof(sg_vertex_buffer_layout_state, step_rate);
    } break;
    case 61: switch (member) {
        case -1: return sizeof(sg_vertex_attr_state);
        case 0: return offsetof(sg_vertex_attr_state, buffer_index);
        case 1: return offsetof(sg_vertex_attr_state, offset);
        case 2: return offsetof(sg_vertex_attr_state, format);
    } break;
    case 62: switch (member) {
        case -1: return sizeof(sg_stencil_state);
        case 0: return offsetof(sg_stencil_state, enabled);
        case 1: return offsetof(sg_stencil_state, front);
        case 2: return offsetof(sg_stencil_state, back);
        case 3: return offsetof(sg_stencil_state, read_mask);
        case 4: return offsetof(sg_stencil_state, write_mask);
        case 5: return offsetof(sg_stencil_state, ref);
    } break;
    case 63: switch (member) {
        case -1: return sizeof(sg_depth_state);
        case 0: return offsetof(sg_depth_state, pixel_format);
        case 1: return offsetof(sg_depth_state, compare);
        case 2: return offsetof(sg_depth_state, write_enabled);
        case 3: return offsetof(sg_depth_state, bias);
        case 4: return offsetof(sg_depth_state, bias_slope_scale);
        case 5: return offsetof(sg_depth_state, bias_clamp);
    } break;
    case 64: switch (member) {
        case -1: return sizeof(sg_stencil_face_state);
        case 0: return offsetof(sg_stencil_face_state, compare);
        case 1: return offsetof(sg_stencil_face_state, fail_op);
        case 2: return offsetof(sg_stencil_face_state, depth_fail_op);
        case 3: return offsetof(sg_stencil_face_state, pass_op);
    } break;
    case 65: switch (member) {
        case -1: return sizeof(sg_blend_state);
        case 0: return offsetof(sg_blend_state, enabled);
        case 1: return offsetof(sg_blend_state, src_factor_rgb);
        case 2: return offsetof(sg_blend_state, dst_factor_rgb);
        case 3: return offsetof(sg_blend_state, op_rgb);
        case 4: return offsetof(sg_blend_state, src_factor_alpha);
        case 5: return offsetof(sg_blend_state, dst_factor_alpha);
        case 6: return offsetof(sg_blend_state, op_alpha);
    } break;
    case 66: switch (member) {
        case -1: return sizeof(sg_color_attachment_action);
        case 0: return offsetof(sg_color_attachment_action, load_action);
        case 1: return offsetof(sg_color_attachment_action, store_action);
        case 2: return offsetof(sg_color_attachment_action, clear_value);
    } break;
    case 67: switch (member) {
        case -1: return sizeof(sg_depth_attachment_action);
        case 0: return offsetof(sg_depth_attachment_action, load_action);
        case 1: return offsetof(sg_depth_attachment_action, store_action);
        case 2: return offsetof(sg_depth_attachment_action, clear_value);
    } break;
    case 68: switch (member) {
        case -1: return sizeof(sg_stencil_attachment_action);
        case 0: return offsetof(sg_stencil_attachment_action, load_action);
        case 1: return offsetof(sg_stencil_attachment_action, store_action);
        case 2: return offsetof(sg_stencil_attachment_action, clear_value);
    } break;
    case 69: switch (member) {
        case -1: return sizeof(sg_pixelformat_info);
        case 0: return offsetof(sg_pixelformat_info, sample);
        case 1: return offsetof(sg_pixelformat_info, filter);
        case 2: return offsetof(sg_pixelformat_info, render);
        case 3: return offsetof(sg_pixelformat_info, blend);
        case 4: return offsetof(sg_pixelformat_info, msaa);
        case 5: return offsetof(sg_pixelformat_info, depth);
        case 6: return offsetof(sg_pixelformat_info, compressed);
        case 7: return offsetof(sg_pixelformat_info, bytes_per_pixel);
    } break;
    case 70: switch (member) {
        case -1: return sizeof(sg_environment_defaults);
        case 0: return offsetof(sg_environment_defaults, color_format);
        case 1: return offsetof(sg_environment_defaults, depth_format);
        case 2: return offsetof(sg_environment_defaults, sample_count);
    } break;
    case 71: switch (member) {
        case -1: return sizeof(sg_metal_environment);
        case 0: return offsetof(sg_metal_environment, device);
    } break;
    case 72: switch (member) {
        case -1: return sizeof(sg_d3d11_environment);
        case 0: return offsetof(sg_d3d11_environment, device);
        case 1: return offsetof(sg_d3d11_environment, device_context);
    } break;
    case 73: switch (member) {
        case -1: return sizeof(sg_wgpu_environment);
        case 0: return offsetof(sg_wgpu_environment, device);
    } break;
    case 74: switch (member) {
        case -1: return sizeof(sg_environment);
        case 0: return offsetof(sg_environment, defaults);
        case 1: return offsetof(sg_environment, metal);
        case 2: return offsetof(sg_environment, d3d11);
        case 3: return offsetof(sg_environment, wgpu);
    } break;
    case 75: switch (member) {
        case -1: return sizeof(sg_commit_listener);
        case 0: return offsetof(sg_commit_listener, func);
        case 1: return offsetof(sg_commit_listener, user_data);
    } break;
    case 76: switch (member) {
        case -1: return sizeof(sg_trace_hooks);
        case 0: return offsetof(sg_trace_hooks, user_data);
        case 1: return offsetof(sg_trace_hooks, reset_state_cache);
        case 2: return offsetof(sg_trace_hooks, make_buffer);
        case 3: return offsetof(sg_trace_hooks, make_image);
        case 4: return offsetof(sg_trace_hooks, make_sampler);
        case 5: return offsetof(sg_trace_hooks, make_shader);
        case 6: return offsetof(sg_trace_hooks, make_pipeline);
        case 7: return offsetof(sg_trace_hooks, make_attachments);
        case 8: return offsetof(sg_trace_hooks, destroy_buffer);
        case 9: return offsetof(sg_trace_hooks, destroy_image);
        case 10: return offsetof(sg_trace_hooks, destroy_sampler);
        case 11: return offsetof(sg_trace_hooks, destroy_shader);
        case 12: return offsetof(sg_trace_hooks, destroy_pipeline);
        case 13: return offsetof(sg_trace_hooks, destroy_attachments);
        case 14: return offsetof(sg_trace_hooks, update_buffer);
        case 15: return offsetof(sg_trace_hooks, update_image);
        case 16: return offsetof(sg_trace_hooks, append_buffer);
        case 17: return offsetof(sg_trace_hooks, begin_pass);
        case 18: return offsetof(sg_trace_hooks, apply_viewport);
        case 19: return offsetof(sg_trace_hooks, apply_scissor_rect);
        case 20: return offsetof(sg_trace_hooks, apply_pipeline);
        case 21: return offsetof(sg_trace_hooks, apply_bindings);
        case 22: return offsetof(sg_trace_hooks, apply_uniforms);
        case 23: return offsetof(sg_trace_hooks, draw);
        case 24: return offsetof(sg_trace_hooks, end_pass);
        case 25: return offsetof(sg_trace_hooks, commit);
        case 26: return offsetof(sg_trace_hooks, alloc_buffer);
        case 27: return offsetof(sg_trace_hooks, alloc_image);
        case 28: return offsetof(sg_trace_hooks, alloc_sampler);
        case 29: return offsetof(sg_trace_hooks, alloc_shader);
        case 30: return offsetof(sg_trace_hooks, alloc_pipeline);
        case 31: return offsetof(sg_trace_hooks, alloc_attachments);
        case 32: return offsetof(sg_trace_hooks, dealloc_buffer);
        case 33: return offsetof(sg_trace_hooks, dealloc_image);
        case 34: return offsetof(sg_trace_hooks, dealloc_sampler);
        case 35: return offsetof(sg_trace_hooks, dealloc_shader);
        case 36: return offsetof(sg_trace_hooks, dealloc_pipeline);
        case 37: return offsetof(sg_trace_hooks, dealloc_attachments);
        case 38: return offsetof(sg_trace_hooks, init_buffer);
        case 39: return offsetof(sg_trace_hooks, init_image);
        case 40: return offsetof(sg_trace_hooks, init_sampler);
        case 41: return offsetof(sg_trace_hooks, init_shader);
        case 42: return offsetof(sg_trace_hooks, init_pipeline);
        case 43: return offsetof(sg_trace_hooks, init_attachments);
        case 44: return offsetof(sg_trace_hooks, uninit_buffer);
        case 45: return offsetof(sg_trace_hooks, uninit_image);
        case 46: return offsetof(sg_trace_hooks, uninit_sampler);
        case 47: return offsetof(sg_trace_hooks, uninit_shader);
        case 48: return offsetof(sg_trace_hooks, uninit_pipeline);
        case 49: return offsetof(sg_trace_hooks, uninit_attachments);
        case 50: return offsetof(sg_trace_hooks, fail_buffer);
        case 51: return offsetof(sg_trace_hooks, fail_image);
        case 52: return offsetof(sg_trace_hooks, fail_sampler);
        case 53: return offsetof(sg_trace_hooks, fail_shader);
        case 54: return offsetof(sg_trace_hooks, fail_pipeline);
        case 55: return offsetof(sg_trace_hooks, fail_attachments);
        case 56: return offsetof(sg_trace_hooks, push_debug_group);
        case 57: return offsetof(sg_trace_hooks, pop_debug_group);
    } break;
    case 77: switch (member) {
        case -1: return sizeof(sg_sampler_info);
        case 0: return offsetof(sg_sampler_info, slot);
    } break;
    case 78: switch (member) {
        case -1: return sizeof(sg_allocator);
        case 0: return offsetof(sg_allocator, alloc_fn);
        case 1: return offsetof(sg_allocator, free_fn);
        case 2: return offsetof(sg_allocator, user_data);
    } break;
    case 79: switch (member) {
        case -1: return sizeof(sg_logger);
        case 0: return offsetof(sg_logger, func);
        case 1: return offsetof(sg_logger, user_data);
    } break;
    case 80: switch (member) {
        case -1: return sizeof(sapp_allocator);
        case 0: return offsetof(sapp_allocator, alloc_fn);
        case 1: return offsetof(sapp_allocator, free_fn);
        case 2: return offsetof(sapp_allocator, user_data);
    } break;
    case 81: switch (member) {
        case -1: return sizeof(sapp_logger);
        case 0: return offsetof(sapp_logger, func);
        case 1: return offsetof(sapp_logger, user_data);
    } break;
    case 82: switch (member) {
        case -1: return sizeof(sapp_range);
        case 0: return offsetof(sapp_range, ptr);
        case 1: return offsetof(sapp_range, size);
    } break;
    case 83: switch (member) {
        case -1: return sizeof(sapp_image_desc);
        case 0: return offsetof(sapp_image_desc, width);
        case 1: return offsetof(sapp_image_desc, height);
        case 2: return offsetof(sapp_image_desc, cursor_hotspot_x);
        case 3: return offsetof(sapp_image_desc, cursor_hotspot_y);
        case 4: return offsetof(sapp_image_desc, pixels);
    } break;
    case 84: switch (member) {
        case -1: return sizeof(sapp_icon_desc);
        case 0: return offsetof(sapp_icon_desc, sokol_default);
        case 1: return offsetof(sapp_icon_desc, images);
    } break;
    case 85: switch (member) {
        case -1: return sizeof(sapp_gl_desc);
        case 0: return offsetof(sapp_gl_desc, major_version);
        case 1: return offsetof(sapp_gl_desc, minor_version);
    } break;
    case 86: switch (member) {
        case -1: return sizeof(sapp_win32_desc);
        case 0: return offsetof(sapp_win32_desc, console_utf8);
        case 1: return offsetof(sapp_win32_desc, console_create);
        case 2: return offsetof(sapp_win32_desc, console_attach);
    } break;
    case 87: switch (member) {
        case -1: return sizeof(sapp_html5_desc);
        case 0: return offsetof(sapp_html5_desc, canvas_selector);
        case 1: return offsetof(sapp_html5_desc, canvas_resize);
        case 2: return offsetof(sapp_html5_desc, preserve_drawing_buffer);
        case 3: return offsetof(sapp_html5_desc, premultiplied_alpha);
        case 4: return offsetof(sapp_html5_desc, ask_leave_site);
        case 5: return offsetof(sapp_html5_desc, update_document_title);
        case 6: return offsetof(sapp_html5_desc, bubble_mouse_events);
        case 7: return offsetof(sapp_html5_desc, bubble_touch_events);
        case 8: return offsetof(sapp_html5_desc, bubble_wheel_events);
        case 9: return offsetof(sapp_html5_desc, bubble_key_events);
        case 10: return offsetof(sapp_html5_desc, bubble_char_events);
        case 11: return offsetof(sapp_html5_desc, use_emsc_set_main_loop);
        case 12: return offsetof(sapp_html5_desc, emsc_set_main_loop_simulate_infinite_loop);
    } break;
    case 88: switch (member) {
        case -1: return sizeof(sapp_ios_desc);
        case 0: return offsetof(sapp_ios_desc, keyboard_resizes_canvas);
    } break;
    case 89: switch (member) {
        case -1: return sizeof(sapp_environment_defaults);
        case 0: return offsetof(sapp_environment_defaults, color_format);
        case 1: return offsetof(sapp_environment_defaults, depth_format);
        case 2: return offsetof(sapp_environment_defaults, sample_count);
    } break;
    case 90: switch (member) {
        case -1: return sizeof(sapp_metal_environment);
        case 0: return offsetof(sapp_metal_environment, device);
    } break;
    case 91: switch (member) {
        case -1: return sizeof(sapp_d3d11_environment);
        case 0: return offsetof(sapp_d3d11_environment, device);
        case 1: return offsetof(sapp_d3d11_environment, device_context);
    } break;
    case 92: switch (member) {
        case -1: return sizeof(sapp_wgpu_environment);
        case 0: return offsetof(sapp_wgpu_environment, device);
    } break;
    case 93: switch (member) {
        case -1: return sizeof(sapp_vulkan_environment);
        case 0: return offsetof(sapp_vulkan_environment, instance);
        case 1: return offsetof(sapp_vulkan_environment, physical_device);
        case 2: return offsetof(sapp_vulkan_environment, device);
        case 3: return offsetof(sapp_vulkan_environment, queue);
        case 4: return offsetof(sapp_vulkan_environment, queue_family_index);
    } break;
    case 94: switch (member) {
        case -1: return sizeof(sapp_environment);
        case 0: return offsetof(sapp_environment, defaults);
        case 1: return offsetof(sapp_environment, metal);
        case 2: return offsetof(sapp_environment, d3d11);
        case 3: return offsetof(sapp_environment, wgpu);
        case 4: return offsetof(sapp_environment, vulkan);
    } break;
    case 95: switch (member) {
        case -1: return sizeof(sapp_metal_swapchain);
        case 0: return offsetof(sapp_metal_swapchain, current_drawable);
        case 1: return offsetof(sapp_metal_swapchain, depth_stencil_texture);
        case 2: return offsetof(sapp_metal_swapchain, msaa_color_texture);
    } break;
    case 96: switch (member) {
        case -1: return sizeof(sapp_d3d11_swapchain);
        case 0: return offsetof(sapp_d3d11_swapchain, render_view);
        case 1: return offsetof(sapp_d3d11_swapchain, resolve_view);
        case 2: return offsetof(sapp_d3d11_swapchain, depth_stencil_view);
    } break;
    case 97: switch (member) {
        case -1: return sizeof(sapp_wgpu_swapchain);
        case 0: return offsetof(sapp_wgpu_swapchain, render_view);
        case 1: return offsetof(sapp_wgpu_swapchain, resolve_view);
        case 2: return offsetof(sapp_wgpu_swapchain, depth_stencil_view);
    } break;
    case 98: switch (member) {
        case -1: return sizeof(sapp_vulkan_swapchain);
        case 0: return offsetof(sapp_vulkan_swapchain, render_image);
        case 1: return offsetof(sapp_vulkan_swapchain, render_view);
        case 2: return offsetof(sapp_vulkan_swapchain, resolve_image);
        case 3: return offsetof(sapp_vulkan_swapchain, resolve_view);
        case 4: return offsetof(sapp_vulkan_swapchain, depth_stencil_image);
        case 5: return offsetof(sapp_vulkan_swapchain, depth_stencil_view);
        case 6: return offsetof(sapp_vulkan_swapchain, render_finished_semaphore);
        case 7: return offsetof(sapp_vulkan_swapchain, present_complete_semaphore);
    } break;
    case 99: switch (member) {
        case -1: return sizeof(sapp_gl_swapchain);
        case 0: return offsetof(sapp_gl_swapchain, framebuffer);
    } break;
    case 100: switch (member) {
        case -1: return sizeof(sapp_swapchain);
        case 0: return offsetof(sapp_swapchain, width);
        case 1: return offsetof(sapp_swapchain, height);
        case 2: return offsetof(sapp_swapchain, sample_count);
        case 3: return offsetof(sapp_swapchain, color_format);
        case 4: return offsetof(sapp_swapchain, depth_format);
        case 5: return offsetof(sapp_swapchain, metal);
        case 6: return offsetof(sapp_swapchain, d3d11);
        case 7: return offsetof(sapp_swapchain, wgpu);
        case 8: return offsetof(sapp_swapchain, vulkan);
        case 9: return offsetof(sapp_swapchain, gl);
    } break;
    case 101: switch (member) {
        case -1: return sizeof(sapp_desc);
        case 0: return offsetof(sapp_desc, init_cb);
        case 1: return offsetof(sapp_desc, frame_cb);
        case 2: return offsetof(sapp_desc, cleanup_cb);
        case 3: return offsetof(sapp_desc, event_cb);
        case 4: return offsetof(sapp_desc, user_data);
        case 5: return offsetof(sapp_desc, init_userdata_cb);
        case 6: return offsetof(sapp_desc, frame_userdata_cb);
        case 7: return offsetof(sapp_desc, cleanup_userdata_cb);
        case 8: return offsetof(sapp_desc, event_userdata_cb);
        case 9: return offsetof(sapp_desc, width);
        case 10: return offsetof(sapp_desc, height);
        case 11: return offsetof(sapp_desc, sample_count);
        case 12: return offsetof(sapp_desc, swap_interval);
        case 13: return offsetof(sapp_desc, high_dpi);
        case 14: return offsetof(sapp_desc, fullscreen);
        case 15: return offsetof(sapp_desc, alpha);
        case 16: return offsetof(sapp_desc, window_title);
        case 17: return offsetof(sapp_desc, enable_clipboard);
        case 18: return offsetof(sapp_desc, clipboard_size);
        case 19: return offsetof(sapp_desc, enable_dragndrop);
        case 20: return offsetof(sapp_desc, max_dropped_files);
        case 21: return offsetof(sapp_desc, max_dropped_file_path_length);
        case 22: return offsetof(sapp_desc, icon);
        case 23: return offsetof(sapp_desc, allocator);
        case 24: return offsetof(sapp_desc, logger);
        case 25: return offsetof(sapp_desc, gl);
        case 26: return offsetof(sapp_desc, win32);
        case 27: return offsetof(sapp_desc, html5);
        case 28: return offsetof(sapp_desc, ios);
        case 29: return offsetof(sapp_desc, __v_native_render);
        case 30: return offsetof(sapp_desc, min_width);
        case 31: return offsetof(sapp_desc, min_height);
        case 32: return offsetof(sapp_desc, borderless_window);
    } break;
    case 102: switch (member) {
        case -1: return sizeof(sapp_event);
        case 0: return offsetof(sapp_event, frame_count);
        case 1: return offsetof(sapp_event, type);
        case 2: return offsetof(sapp_event, key_code);
        case 3: return offsetof(sapp_event, char_code);
        case 4: return offsetof(sapp_event, key_repeat);
        case 5: return offsetof(sapp_event, modifiers);
        case 6: return offsetof(sapp_event, mouse_button);
        case 7: return offsetof(sapp_event, mouse_x);
        case 8: return offsetof(sapp_event, mouse_y);
        case 9: return offsetof(sapp_event, mouse_dx);
        case 10: return offsetof(sapp_event, mouse_dy);
        case 11: return offsetof(sapp_event, scroll_x);
        case 12: return offsetof(sapp_event, scroll_y);
        case 13: return offsetof(sapp_event, num_touches);
        case 14: return offsetof(sapp_event, touches);
        case 15: return offsetof(sapp_event, window_width);
        case 16: return offsetof(sapp_event, window_height);
        case 17: return offsetof(sapp_event, framebuffer_width);
        case 18: return offsetof(sapp_event, framebuffer_height);
    } break;
    case 103: switch (member) {
        case -1: return sizeof(sapp_touchpoint);
        case 0: return offsetof(sapp_touchpoint, identifier);
        case 1: return offsetof(sapp_touchpoint, pos_x);
        case 2: return offsetof(sapp_touchpoint, pos_y);
        case 3: return offsetof(sapp_touchpoint, android_tooltype);
        case 4: return offsetof(sapp_touchpoint, changed);
    } break;
    case 104: switch (member) {
        case -1: return sizeof(sgl_pipeline);
        case 0: return offsetof(sgl_pipeline, id);
    } break;
    case 105: switch (member) {
        case -1: return sizeof(sgl_context);
        case 0: return offsetof(sgl_context, id);
    } break;
    case 106: switch (member) {
        case -1: return sizeof(sgl_context_desc_t);
        case 0: return offsetof(sgl_context_desc_t, max_vertices);
        case 1: return offsetof(sgl_context_desc_t, max_commands);
        case 2: return offsetof(sgl_context_desc_t, color_format);
        case 3: return offsetof(sgl_context_desc_t, depth_format);
        case 4: return offsetof(sgl_context_desc_t, sample_count);
    } break;
    case 107: switch (member) {
        case -1: return sizeof(sgl_desc_t);
        case 0: return offsetof(sgl_desc_t, max_vertices);
        case 1: return offsetof(sgl_desc_t, max_commands);
        case 2: return offsetof(sgl_desc_t, context_pool_size);
        case 3: return offsetof(sgl_desc_t, pipeline_pool_size);
        case 4: return offsetof(sgl_desc_t, color_format);
        case 5: return offsetof(sgl_desc_t, depth_format);
        case 6: return offsetof(sgl_desc_t, sample_count);
        case 7: return offsetof(sgl_desc_t, face_winding);
        case 8: return offsetof(sgl_desc_t, allocator);
        case 9: return offsetof(sgl_desc_t, logger);
    } break;
    case 108: switch (member) {
        case -1: return sizeof(sgl_allocator_t);
        case 0: return offsetof(sgl_allocator_t, alloc_fn);
        case 1: return offsetof(sgl_allocator_t, free_fn);
        case 2: return offsetof(sgl_allocator_t, user_data);
    } break;
    case 109: switch (member) {
        case -1: return sizeof(sgl_logger_t);
        case 0: return offsetof(sgl_logger_t, func);
        case 1: return offsetof(sgl_logger_t, user_data);
    } break;
    case 110: switch (member) {
        case -1: return sizeof(sfons_allocator_t);
        case 0: return offsetof(sfons_allocator_t, alloc_fn);
        case 1: return offsetof(sfons_allocator_t, free_fn);
        case 2: return offsetof(sfons_allocator_t, user_data);
    } break;
    case 111: switch (member) {
        case -1: return sizeof(sfons_desc_t);
        case 0: return offsetof(sfons_desc_t, width);
        case 1: return offsetof(sfons_desc_t, height);
        case 2: return offsetof(sfons_desc_t, allocator);
    } break;
    case 112: switch (member) {
        case -1: return sizeof(saudio_allocator);
        case 0: return offsetof(saudio_allocator, alloc_fn);
        case 1: return offsetof(saudio_allocator, free_fn);
        case 2: return offsetof(saudio_allocator, user_data);
    } break;
    case 113: switch (member) {
        case -1: return sizeof(saudio_logger);
        case 0: return offsetof(saudio_logger, func);
        case 1: return offsetof(saudio_logger, user_data);
    } break;
    case 114: switch (member) {
        case -1: return sizeof(saudio_desc);
        case 0: return offsetof(saudio_desc, sample_rate);
        case 1: return offsetof(saudio_desc, num_channels);
        case 2: return offsetof(saudio_desc, buffer_frames);
        case 3: return offsetof(saudio_desc, packet_frames);
        case 4: return offsetof(saudio_desc, num_packets);
        case 5: return offsetof(saudio_desc, stream_cb);
        case 6: return offsetof(saudio_desc, stream_userdata_cb);
        case 7: return offsetof(saudio_desc, user_data);
        case 8: return offsetof(saudio_desc, allocator);
        case 9: return offsetof(saudio_desc, logger);
    } break;
    case 115: switch (member) {
        case -1: return sizeof(FONSparams);
        case 0: return offsetof(FONSparams, width);
        case 1: return offsetof(FONSparams, height);
        case 2: return offsetof(FONSparams, flags);
        case 3: return offsetof(FONSparams, userPtr);
        case 4: return offsetof(FONSparams, renderCreate);
        case 5: return offsetof(FONSparams, renderResize);
        case 6: return offsetof(FONSparams, renderUpdate);
        case 7: return offsetof(FONSparams, renderDraw);
        case 8: return offsetof(FONSparams, renderDelete);
    } break;
    case 116: switch (member) {
        case -1: return sizeof(FONSquad);
        case 0: return offsetof(FONSquad, x0);
        case 1: return offsetof(FONSquad, y0);
        case 2: return offsetof(FONSquad, s0);
        case 3: return offsetof(FONSquad, t0);
        case 4: return offsetof(FONSquad, x1);
        case 5: return offsetof(FONSquad, y1);
        case 6: return offsetof(FONSquad, s1);
        case 7: return offsetof(FONSquad, t1);
    } break;
    case 117: switch (member) {
        case -1: return sizeof(FONStextIter);
        case 0: return offsetof(FONStextIter, x);
        case 1: return offsetof(FONStextIter, y);
        case 2: return offsetof(FONStextIter, nextx);
        case 3: return offsetof(FONStextIter, nexty);
        case 4: return offsetof(FONStextIter, scale);
        case 5: return offsetof(FONStextIter, spacing);
        case 6: return offsetof(FONStextIter, codepoint);
        case 7: return offsetof(FONStextIter, isize);
        case 8: return offsetof(FONStextIter, iblur);
        case 9: return offsetof(FONStextIter, font);
        case 10: return offsetof(FONStextIter, prevGlyphIndex);
        case 11: return offsetof(FONStextIter, str);
        case 12: return offsetof(FONStextIter, next);
        case 13: return offsetof(FONStextIter, end);
        case 14: return offsetof(FONStextIter, utf8state);
    } break;
    case 118: switch (member) {
        case -1: return sizeof(FONSstate);
        case 0: return offsetof(FONSstate, font);
        case 1: return offsetof(FONSstate, align);
        case 2: return offsetof(FONSstate, size);
        case 3: return offsetof(FONSstate, color);
        case 4: return offsetof(FONSstate, blur);
        case 5: return offsetof(FONSstate, spacing);
    } break;
    case 119: switch (member) {
        case -1: return sizeof(FONScontext);
        case 0: return offsetof(FONScontext, params);
        case 1: return offsetof(FONScontext, itw);
        case 2: return offsetof(FONScontext, ith);
        case 3: return offsetof(FONScontext, texData);
        case 4: return offsetof(FONScontext, dirtyRect);
        case 5: return offsetof(FONScontext, fonts);
        case 6: return offsetof(FONScontext, atlas);
        case 7: return offsetof(FONScontext, cfonts);
        case 8: return offsetof(FONScontext, nfonts);
        case 9: return offsetof(FONScontext, verts);
        case 10: return offsetof(FONScontext, tcoords);
        case 11: return offsetof(FONScontext, colors);
        case 12: return offsetof(FONScontext, nverts);
        case 13: return offsetof(FONScontext, scratch);
        case 14: return offsetof(FONScontext, nscratch);
        case 15: return offsetof(FONScontext, states);
        case 16: return offsetof(FONScontext, nstates);
        case 17: return offsetof(FONScontext, handleError);
        case 18: return offsetof(FONScontext, errorUptr);
    } break;
    case 120: switch (member) {
        case -1: return sizeof(stb_vorbis_alloc);
        case 0: return offsetof(stb_vorbis_alloc, alloc_buffer);
        case 1: return offsetof(stb_vorbis_alloc, alloc_buffer_length_in_bytes);
    } break;
    case 121: switch (member) {
        case -1: return sizeof(stb_vorbis_info);
        case 0: return offsetof(stb_vorbis_info, sample_rate);
        case 1: return offsetof(stb_vorbis_info, channels);
        case 2: return offsetof(stb_vorbis_info, setup_memory_required);
        case 3: return offsetof(stb_vorbis_info, setup_temp_memory_required);
        case 4: return offsetof(stb_vorbis_info, temp_memory_required);
        case 5: return offsetof(stb_vorbis_info, max_frame_size);
    } break;
    case 122: switch (member) {
        case -1: return sizeof(stb_vorbis_comment);
        case 0: return offsetof(stb_vorbis_comment, vendor);
        case 1: return offsetof(stb_vorbis_comment, comment_list_length);
        case 2: return offsetof(stb_vorbis_comment, comment_list);
    } break;
    case 123: switch (member) {
        case -1: return sizeof(v_gg_color);
        case 0: return offsetof(v_gg_color, r);
        case 1: return offsetof(v_gg_color, g);
        case 2: return offsetof(v_gg_color, b);
        case 3: return offsetof(v_gg_color, a);
    } break;
    case 124: switch (member) {
        case -1: return sizeof(v_gg_string);
        case 0: return offsetof(v_gg_string, str);
        case 1: return offsetof(v_gg_string, len);
    } break;
    case 125: switch (member) {
        case -1: return sizeof(v_gg_text_cfg);
        case 0: return offsetof(v_gg_text_cfg, color);
        case 1: return offsetof(v_gg_text_cfg, size);
        case 2: return offsetof(v_gg_text_cfg, bold);
        case 3: return offsetof(v_gg_text_cfg, mono);
    } break;
    case 126: switch (member) {
        case -1: return sizeof(v_gg_size);
        case 0: return offsetof(v_gg_size, width);
        case 1: return offsetof(v_gg_size, height);
    } break;
    case 127: switch (member) {
        case -1: return sizeof(v_gg_image);
        case 0: return offsetof(v_gg_image, width);
        case 1: return offsetof(v_gg_image, height);
        case 2: return offsetof(v_gg_image, ok);
        case 3: return offsetof(v_gg_image, data);
    } break;
    }
    return -1;
}
