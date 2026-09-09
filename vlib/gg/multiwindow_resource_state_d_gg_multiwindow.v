module gg

import sokol.gfx
import sokol.sgl

enum MultiWindowResourceKind {
	buffer
	image
	sampler
	shader
	pipeline
	attachments
	sgl_pipeline
}

enum MultiWindowResourceStatus {
	invalid
	alive
	retiring
	exhausted
}

enum MultiWindowBufferMutationMode {
	none
	update
	append
}

struct MultiWindowResourceKey {
	app_instance u64
	slot         int
	generation   u32
	window       WindowId
}

struct MultiWindowAttachmentsRecipe {
	colors        []MultiWindowResourceKey
	resolves      []MultiWindowResourceKey
	depth_stencil ?MultiWindowResourceKey
}

struct MultiWindowSglMaterialization {
	pipeline    sgl.Pipeline
	context_key string
}

struct MultiWindowResourceSlot {
mut:
	app_instance         u64
	generation           u32
	kind                 MultiWindowResourceKind
	status               MultiWindowResourceStatus
	window               WindowId
	app_scoped           bool
	label                string
	dependencies         []MultiWindowResourceKey
	dependents           []MultiWindowResourceKey
	last_mutation_batch  u64
	buffer_mutation_mode MultiWindowBufferMutationMode
	buffer_capacity      usize
	buffer_usage         gfx.Usage
	buffer_desc          gfx.BufferDesc
	buffer               gfx.Buffer
	image_desc           gfx.ImageDesc
	image                gfx.Image
	sampler_desc         gfx.SamplerDesc
	sampler              gfx.Sampler
	shader               gfx.Shader
	shader_desc          gfx.ShaderDesc
	pipeline_desc        gfx.PipelineDesc
	pipeline             gfx.Pipeline
	attachments_recipe   MultiWindowAttachmentsRecipe
	attachments          gfx.Attachments
	target_identity      u64
	sgl_recipe           gfx.PipelineDesc
	sgl_shader           ?MultiWindowResourceKey
	materialized_sgl     map[string]MultiWindowSglMaterialization
}

struct MultiWindowRetiredResource {
	key          ?MultiWindowResourceKey
	kind         MultiWindowResourceKind
	retire_batch u64
	buffer       gfx.Buffer
	image        gfx.Image
	sampler      gfx.Sampler
	shader       gfx.Shader
	pipeline     gfx.Pipeline
	attachments  gfx.Attachments
mut:
	materialized_sgl map[string]MultiWindowSglMaterialization
}

struct MultiWindowResourceRegistry {
mut:
	app_instance u64
	slots        []MultiWindowResourceSlot
	deferred     []MultiWindowRetiredResource
}

struct MultiWindowBufferMutationPlan {
	index        int
	batch        u64
	mode         MultiWindowBufferMutationMode
	starts_batch bool
	buffer       gfx.Buffer
}

struct MultiWindowImageMutationPlan {
	index int
	batch u64
	image gfx.Image
}

struct MultiWindowRetirementPlan {
	index          int
	batch          u64
	needs_boundary bool
}

fn new_multiwindow_resource_registry(app_instance u64) MultiWindowResourceRegistry {
	return MultiWindowResourceRegistry{
		app_instance: app_instance
	}
}

fn (mut registry MultiWindowResourceRegistry) reserve(kind MultiWindowResourceKind, window WindowId, app_scoped bool) !MultiWindowResourceKey {
	for i, slot in registry.slots {
		if slot.status == .exhausted || slot.status == .alive || slot.status == .retiring {
			continue
		}
		if slot.generation == u32(0xffffffff) {
			mut exhausted_slot := &registry.slots[i]
			exhausted_slot.status = .exhausted
			continue
		}
		generation := slot.generation + 1
		key := MultiWindowResourceKey{
			app_instance: registry.app_instance
			slot:         i
			generation:   generation
			window:       window
		}
		registry.slots[i] = new_resource_slot(key, kind, app_scoped)
		return key
	}
	key := MultiWindowResourceKey{
		app_instance: registry.app_instance
		slot:         registry.slots.len
		generation:   1
		window:       window
	}
	registry.slots << new_resource_slot(key, kind, app_scoped)
	return key
}

fn new_resource_slot(key MultiWindowResourceKey, kind MultiWindowResourceKind, app_scoped bool) MultiWindowResourceSlot {
	return MultiWindowResourceSlot{
		app_instance:     key.app_instance
		generation:       key.generation
		kind:             kind
		status:           .alive
		window:           key.window
		app_scoped:       app_scoped
		materialized_sgl: map[string]MultiWindowSglMaterialization{}
	}
}

fn (registry &MultiWindowResourceRegistry) validate(key MultiWindowResourceKey, kind MultiWindowResourceKind, window WindowId, scope MultiWindowResourceScope) !int {
	if key.app_instance != registry.app_instance {
		return error(err_multiwindow_render_resource_scope)
	}
	if key.slot < 0 || key.slot >= registry.slots.len {
		return error(err_multiwindow_render_stale_resource)
	}
	slot := registry.slots[key.slot]
	if slot.status != .alive || slot.app_instance != key.app_instance
		|| slot.generation != key.generation || slot.kind != kind || slot.window != key.window {
		return error(err_multiwindow_render_stale_resource)
	}
	if scope == .app {
		if !slot.app_scoped {
			return error(err_multiwindow_render_resource_scope)
		}
	} else if !slot.app_scoped && slot.window != window {
		return error(err_multiwindow_render_resource_scope)
	}
	return key.slot
}

fn (mut registry MultiWindowResourceRegistry) add_dependencies(index int, dependencies []MultiWindowResourceKey) ! {
	if index < 0 || index >= registry.slots.len || registry.slots[index].status != .alive {
		return error(err_multiwindow_render_stale_resource)
	}
	owner_key := registry.key_for_slot(index)
	owner := registry.slots[index]
	for dependency in dependencies {
		dependency_index := registry.validate_any(dependency)!
		dependency_slot := registry.slots[dependency_index]
		if owner.app_scoped && !dependency_slot.app_scoped {
			return error(err_multiwindow_render_resource_scope)
		}
		if !owner.app_scoped && !dependency_slot.app_scoped
			&& dependency_slot.window != owner.window {
			return error(err_multiwindow_render_resource_scope)
		}
		mut owner_slot := &registry.slots[index]
		if dependency !in owner_slot.dependencies {
			owner_slot.dependencies << dependency
		}
		mut dependency_owner := &registry.slots[dependency_index]
		if owner_key !in dependency_owner.dependents {
			dependency_owner.dependents << owner_key
		}
	}
}

fn (registry &MultiWindowResourceRegistry) validate_any(key MultiWindowResourceKey) !int {
	if key.app_instance != registry.app_instance || key.slot < 0 || key.slot >= registry.slots.len {
		return error(err_multiwindow_render_stale_resource)
	}
	slot := registry.slots[key.slot]
	if slot.status != .alive || slot.app_instance != key.app_instance
		|| slot.generation != key.generation || slot.window != key.window {
		return error(err_multiwindow_render_stale_resource)
	}
	return key.slot
}

fn (registry &MultiWindowResourceRegistry) plan_buffer_mutation(key MultiWindowResourceKey, window WindowId, scope MultiWindowResourceScope, batch u64, mode MultiWindowBufferMutationMode, data_size usize) !MultiWindowBufferMutationPlan {
	index := registry.validate(key, .buffer, window, scope)!
	registry.validate_mutation_scope(index, scope)!
	if batch == 0 {
		return error(err_multiwindow_render_wrong_phase)
	}
	slot := registry.slots[index]
	if slot.buffer_usage == .immutable {
		return error(err_multiwindow_render_mutation_mode)
	}
	if data_size > slot.buffer_capacity {
		return error(err_multiwindow_render_buffer_overflow)
	}
	if slot.last_mutation_batch == batch {
		if slot.buffer_mutation_mode != mode {
			return error(err_multiwindow_render_mutation_mode)
		}
		if mode == .update {
			return error(err_multiwindow_render_update_twice)
		}
	}
	return MultiWindowBufferMutationPlan{
		index:        index
		batch:        batch
		mode:         mode
		starts_batch: slot.last_mutation_batch != batch
		buffer:       slot.buffer
	}
}

fn (mut registry MultiWindowResourceRegistry) commit_buffer_mutation(plan MultiWindowBufferMutationPlan) {
	if plan.starts_batch {
		mut slot := &registry.slots[plan.index]
		slot.last_mutation_batch = plan.batch
		slot.buffer_mutation_mode = plan.mode
	}
}

fn (registry &MultiWindowResourceRegistry) plan_image_update(key MultiWindowResourceKey, window WindowId, scope MultiWindowResourceScope, batch u64) !MultiWindowImageMutationPlan {
	index := registry.validate(key, .image, window, scope)!
	registry.validate_mutation_scope(index, scope)!
	slot := registry.slots[index]
	if slot.image_desc.usage == .immutable || slot.image_desc.render_target {
		return error(err_multiwindow_render_wrong_phase)
	}
	if batch == 0 || slot.last_mutation_batch == batch {
		return error(err_multiwindow_render_update_twice)
	}
	return MultiWindowImageMutationPlan{
		index: index
		batch: batch
		image: slot.image
	}
}

fn (mut registry MultiWindowResourceRegistry) commit_image_mutation(plan MultiWindowImageMutationPlan) {
	mut slot := &registry.slots[plan.index]
	slot.last_mutation_batch = plan.batch
}

fn (registry &MultiWindowResourceRegistry) plan_retirement(key MultiWindowResourceKey, kind MultiWindowResourceKind, window WindowId, scope MultiWindowResourceScope, batch u64) !MultiWindowRetirementPlan {
	index := registry.validate(key, kind, window, scope)!
	registry.validate_mutation_scope(index, scope)!
	if registry.live_dependents(index).len > 0 {
		return error(err_multiwindow_render_resource_has_dependents)
	}
	return MultiWindowRetirementPlan{
		index:          index
		batch:          batch
		needs_boundary: resource_slot_requires_boundary(registry.slots[index])
	}
}

fn (mut registry MultiWindowResourceRegistry) commit_retirement(plan MultiWindowRetirementPlan) {
	registry.retire_slot(plan.index, plan.batch)
}

fn (registry &MultiWindowResourceRegistry) validate_mutation_scope(index int, scope MultiWindowResourceScope) ! {
	if index < 0 || index >= registry.slots.len {
		return error(err_multiwindow_render_stale_resource)
	}
	if (scope == .app) != registry.slots[index].app_scoped {
		return error(err_multiwindow_render_resource_scope)
	}
}

fn (mut registry MultiWindowResourceRegistry) retire_window_at_batch(window WindowId, batch u64) {
	registry.retire_scope_at_batch(false, window, batch)
}

fn (mut registry MultiWindowResourceRegistry) retire_app_at_batch(batch u64) {
	registry.retire_scope_at_batch(true, WindowId{}, batch)
}

fn (registry &MultiWindowResourceRegistry) scope_requires_boundary(app_scoped bool, window WindowId) bool {
	for slot in registry.slots {
		if slot.status == .alive && slot.app_scoped == app_scoped
			&& (app_scoped || slot.window == window) && resource_slot_requires_boundary(slot) {
			return true
		}
	}
	return false
}

fn resource_slot_requires_boundary(slot MultiWindowResourceSlot) bool {
	return match slot.kind {
		.buffer { slot.buffer.id != 0 }
		.image { slot.image.id != 0 }
		.sampler { slot.sampler.id != 0 }
		.shader { slot.shader.id != 0 }
		.pipeline { slot.pipeline.id != 0 }
		.attachments { slot.attachments.id != 0 }
		.sgl_pipeline { slot.materialized_sgl.len > 0 }
	}
}

fn (mut registry MultiWindowResourceRegistry) retire_scope_at_batch(app_scoped bool, window WindowId, batch u64) {
	mut pending := []int{}
	for i, slot in registry.slots {
		if slot.status == .alive && slot.app_scoped == app_scoped
			&& (app_scoped || slot.window == window) {
			pending << i
		}
	}
	for pending.len > 0 {
		mut retired_one := false
		for position, index in pending {
			mut has_pending_dependent := false
			for dependent in registry.live_dependents(index) {
				if dependent.slot in pending {
					has_pending_dependent = true
					break
				}
			}
			if has_pending_dependent {
				continue
			}
			registry.retire_slot(index, batch)
			pending.delete(position)
			retired_one = true
			break
		}
		if !retired_one {
			// Cycles are invalid, but terminal cleanup must still make progress.
			registry.retire_slot(pending[0], batch)
			pending.delete(0)
		}
	}
}

fn (mut registry MultiWindowResourceRegistry) retire_slot(index int, batch u64) {
	if index < 0 || index >= registry.slots.len || registry.slots[index].status != .alive {
		return
	}
	key := registry.key_for_slot(index)
	for dependency in registry.slots[index].dependencies {
		dependency_index := registry.validate_any(dependency) or { continue }
		mut dependency_slot := &registry.slots[dependency_index]
		dependency_slot.dependents = dependency_slot.dependents.filter(it != key)
	}
	registry.deferred << retired_resource_from_slot(key, registry.slots[index], batch)
	mut slot := &registry.slots[index]
	slot.status = .retiring
	slot.dependencies.clear()
	slot.dependents.clear()
	slot.materialized_sgl.clear()
}

fn (registry &MultiWindowResourceRegistry) live_dependents(index int) []MultiWindowResourceKey {
	mut result := []MultiWindowResourceKey{}
	if index < 0 || index >= registry.slots.len {
		return result
	}
	for dependent in registry.slots[index].dependents {
		registry.validate_any(dependent) or { continue }
		result << dependent
	}
	return result
}

fn retired_resource_from_slot(key MultiWindowResourceKey, slot MultiWindowResourceSlot, batch u64) MultiWindowRetiredResource {
	return MultiWindowRetiredResource{
		key:              key
		kind:             slot.kind
		retire_batch:     batch
		buffer:           slot.buffer
		image:            slot.image
		sampler:          slot.sampler
		shader:           slot.shader
		pipeline:         slot.pipeline
		attachments:      slot.attachments
		materialized_sgl: slot.materialized_sgl.clone()
	}
}

fn (mut registry MultiWindowResourceRegistry) defer_replaced(kind MultiWindowResourceKind, image gfx.Image, attachments gfx.Attachments, batch u64) {
	registry.deferred << MultiWindowRetiredResource{
		kind:         kind
		retire_batch: batch
		image:        image
		attachments:  attachments
	}
}

fn (mut registry MultiWindowResourceRegistry) flush_retired(completed_batch u64) {
	mut remaining := []MultiWindowRetiredResource{}
	for retired in registry.deferred {
		if retired.retire_batch > completed_batch {
			remaining << retired
			continue
		}
		destroy_retired_resource(retired)
		if key := retired.key {
			if key.slot >= 0 && key.slot < registry.slots.len {
				mut slot := &registry.slots[key.slot]
				if slot.status == .retiring && slot.app_instance == key.app_instance
					&& slot.generation == key.generation && slot.window == key.window {
					slot.status = .invalid
				}
			}
		}
	}
	registry.deferred = remaining
}

// abandon_all invalidates managed ownership without touching Sokol. It is used
// only when global SGL/gfx shutdown will release a valid device, or when a
// confirmed device/context loss makes individual destruction unsafe.
fn (mut registry MultiWindowResourceRegistry) abandon_all() {
	for i, slot in registry.slots {
		status := if slot.status == .exhausted {
			MultiWindowResourceStatus.exhausted
		} else {
			MultiWindowResourceStatus.invalid
		}
		registry.slots[i] = MultiWindowResourceSlot{
			app_instance:     slot.app_instance
			generation:       slot.generation
			status:           status
			window:           slot.window
			app_scoped:       slot.app_scoped
			materialized_sgl: map[string]MultiWindowSglMaterialization{}
		}
	}
	registry.deferred.clear()
}

fn destroy_retired_resource(resource MultiWindowRetiredResource) {
	match resource.kind {
		.buffer {
			gfx.destroy_buffer(resource.buffer)
		}
		.image {
			gfx.destroy_image(resource.image)
		}
		.sampler {
			gfx.destroy_sampler(resource.sampler)
		}
		.shader {
			gfx.destroy_shader(resource.shader)
		}
		.pipeline {
			gfx.destroy_pipeline(resource.pipeline)
		}
		.attachments {
			gfx.destroy_attachments(resource.attachments)
		}
		.sgl_pipeline {
			for _, materialization in resource.materialized_sgl {
				sgl.destroy_pipeline(materialization.pipeline)
			}
		}
	}
}

fn (mut registry MultiWindowResourceRegistry) take_sgl_materialization_for_target(target_key string) ?MultiWindowSglMaterialization {
	for i in 0 .. registry.slots.len {
		mut slot := &registry.slots[i]
		if slot.kind != .sgl_pipeline || target_key !in slot.materialized_sgl {
			continue
		}
		materialization := slot.materialized_sgl[target_key]
		slot.materialized_sgl.delete(target_key)
		return materialization
	}
	for i in 0 .. registry.deferred.len {
		mut retired := &registry.deferred[i]
		if target_key !in retired.materialized_sgl {
			continue
		}
		materialization := retired.materialized_sgl[target_key]
		retired.materialized_sgl.delete(target_key)
		return materialization
	}
	return none
}

fn (mut registry MultiWindowResourceRegistry) abandon_sgl_materializations_for_target(target_key string) {
	for i in 0 .. registry.slots.len {
		mut slot := &registry.slots[i]
		if slot.kind == .sgl_pipeline && target_key in slot.materialized_sgl {
			slot.materialized_sgl.delete(target_key)
		}
	}
	for i in 0 .. registry.deferred.len {
		mut retired := &registry.deferred[i]
		if target_key in retired.materialized_sgl {
			retired.materialized_sgl.delete(target_key)
		}
	}
}

fn (registry &MultiWindowResourceRegistry) key_for_slot(index int) MultiWindowResourceKey {
	slot := registry.slots[index]
	return MultiWindowResourceKey{
		app_instance: slot.app_instance
		slot:         index
		generation:   slot.generation
		window:       slot.window
	}
}

fn buffer_resource_key(id WindowBufferId) MultiWindowResourceKey {
	return MultiWindowResourceKey{id.app_instance, id.slot, id.generation, id.window}
}

fn image_resource_key(id WindowImageId) MultiWindowResourceKey {
	return MultiWindowResourceKey{id.app_instance, id.slot, id.generation, id.window}
}

fn sampler_resource_key(id WindowSamplerId) MultiWindowResourceKey {
	return MultiWindowResourceKey{id.app_instance, id.slot, id.generation, id.window}
}

fn shader_resource_key(id WindowShaderId) MultiWindowResourceKey {
	return MultiWindowResourceKey{id.app_instance, id.slot, id.generation, id.window}
}

fn pipeline_resource_key(id WindowPipelineId) MultiWindowResourceKey {
	return MultiWindowResourceKey{id.app_instance, id.slot, id.generation, id.window}
}

fn attachments_resource_key(id WindowAttachmentsId) MultiWindowResourceKey {
	return MultiWindowResourceKey{id.app_instance, id.slot, id.generation, id.window}
}

fn sgl_pipeline_resource_key(id WindowSglPipelineId) MultiWindowResourceKey {
	return MultiWindowResourceKey{id.app_instance, id.slot, id.generation, id.window}
}

fn window_buffer_id(key MultiWindowResourceKey) WindowBufferId {
	return WindowBufferId{key.app_instance, key.slot, key.generation, key.window}
}

fn window_image_id(key MultiWindowResourceKey) WindowImageId {
	return WindowImageId{key.app_instance, key.slot, key.generation, key.window}
}

fn window_sampler_id(key MultiWindowResourceKey) WindowSamplerId {
	return WindowSamplerId{key.app_instance, key.slot, key.generation, key.window}
}

fn window_shader_id(key MultiWindowResourceKey) WindowShaderId {
	return WindowShaderId{key.app_instance, key.slot, key.generation, key.window}
}

fn window_pipeline_id(key MultiWindowResourceKey) WindowPipelineId {
	return WindowPipelineId{key.app_instance, key.slot, key.generation, key.window}
}

fn window_attachments_id(key MultiWindowResourceKey) WindowAttachmentsId {
	return WindowAttachmentsId{key.app_instance, key.slot, key.generation, key.window}
}

fn window_sgl_pipeline_id(key MultiWindowResourceKey) WindowSglPipelineId {
	return WindowSglPipelineId{key.app_instance, key.slot, key.generation, key.window}
}

fn next_gg_target_identity(value u64) !u64 {
	if value == 0 || value == u64(0xffffffffffffffff) {
		return error(err_multiwindow_render_stopped)
	}
	return value + 1
}
