module gg

import sokol.gfx
import sokol.sgl

struct ManagedBufferDescCopy {
mut:
	desc  gfx.BufferDesc
	data  []u8
	label string
}

struct ManagedImageDescCopy {
mut:
	desc  gfx.ImageDesc
	data  [][]u8
	label string
}

struct ManagedSamplerDescCopy {
mut:
	desc  gfx.SamplerDesc
	label string
}

struct ManagedPipelineDescCopy {
mut:
	desc  gfx.PipelineDesc
	label string
}

struct ManagedShaderDescCopy {
mut:
	desc    gfx.ShaderDesc
	strings []string
	bytes   [][]u8
	label   string
}

struct ManagedAttachmentReplacement {
	key                  MultiWindowResourceKey
	index                int
	recipe               MultiWindowAttachmentsRecipe
	old                  gfx.Attachments
	target_identity      u64
	next_target_identity u64
}

struct ManagedRetiredAttachmentTarget {
	key             MultiWindowResourceKey
	target_identity u64
}

fn (mut resources WindowResourceContext) make_buffer_managed(desc &gfx.BufferDesc) !WindowBufferId {
	resources.validate_managed_resource_context(.create)!
	validate_raw_buffer_desc(desc)!
	mut copied := copy_managed_buffer_desc(desc)!
	effective := gfx.query_buffer_desc_defaults(&copied.desc)
	validate_effective_buffer_desc(effective)!
	resources.fail_internal_resource_stage(.resource_make_buffer)!
	resources.app.note_managed_gpu_work(resources.batch_epoch)!
	buffer := gfx.make_buffer(&effective)
	if gfx.query_buffer_state(buffer) != .valid {
		gfx.destroy_buffer(buffer)
		return error(err_multiwindow_render_resource_failed)
	}
	mut runtime := resources.app.render_runtime
	runtime.mutex.lock()
	key := runtime.resources.reserve(.buffer, resources.window, resources.scope == .app) or {
		runtime.mutex.unlock()
		gfx.destroy_buffer(buffer)
		return err
	}
	mut slot := &runtime.resources.slots[key.slot]
	slot.buffer = buffer
	slot.buffer_capacity = effective.size
	slot.buffer_usage = effective.usage
	slot.buffer_desc = pointer_free_buffer_desc(effective)
	slot.label = copied.label
	runtime.mutex.unlock()
	return window_buffer_id(key)
}

fn (mut resources WindowResourceContext) make_image_managed(desc &gfx.ImageDesc) !WindowImageId {
	resources.validate_managed_resource_context(.create)!
	mut copied := copy_managed_image_desc(desc)!
	effective := gfx.query_image_desc_defaults(&copied.desc)
	validate_effective_image_desc(effective)!
	resources.fail_internal_resource_stage(.resource_make_image)!
	resources.app.note_managed_gpu_work(resources.batch_epoch)!
	image := gfx.make_image(&effective)
	if gfx.query_image_state(image) != .valid {
		gfx.destroy_image(image)
		return error(err_multiwindow_render_resource_failed)
	}
	mut runtime := resources.app.render_runtime
	runtime.mutex.lock()
	key := runtime.resources.reserve(.image, resources.window, resources.scope == .app) or {
		runtime.mutex.unlock()
		gfx.destroy_image(image)
		return err
	}
	mut slot := &runtime.resources.slots[key.slot]
	slot.image = image
	slot.image_desc = pointer_free_image_desc(effective)
	slot.label = copied.label
	runtime.mutex.unlock()
	return window_image_id(key)
}

fn (mut resources WindowResourceContext) make_sampler_managed(desc &gfx.SamplerDesc) !WindowSamplerId {
	resources.validate_managed_resource_context(.create)!
	mut copied := copy_managed_sampler_desc(desc)!
	effective := gfx.query_sampler_defaults(&copied.desc)
	resources.fail_internal_resource_stage(.resource_make_sampler)!
	resources.app.note_managed_gpu_work(resources.batch_epoch)!
	sampler := gfx.make_sampler(&effective)
	if gfx.query_sampler_state(sampler) != .valid {
		gfx.destroy_sampler(sampler)
		return error(err_multiwindow_render_resource_failed)
	}
	mut runtime := resources.app.render_runtime
	runtime.mutex.lock()
	key := runtime.resources.reserve(.sampler, resources.window, resources.scope == .app) or {
		runtime.mutex.unlock()
		gfx.destroy_sampler(sampler)
		return err
	}
	mut slot := &runtime.resources.slots[key.slot]
	slot.sampler = sampler
	slot.sampler_desc = pointer_free_sampler_desc(effective)
	slot.label = copied.label
	runtime.mutex.unlock()
	return window_sampler_id(key)
}

fn (mut resources WindowResourceContext) make_shader_managed(desc &gfx.ShaderDesc) !WindowShaderId {
	resources.validate_managed_resource_context(.create)!
	mut copied := copy_managed_shader_desc(desc)!
	effective := gfx.query_shader_desc_defaults(&copied.desc)
	resources.fail_internal_resource_stage(.resource_make_shader)!
	resources.app.note_managed_gpu_work(resources.batch_epoch)!
	shader := gfx.make_shader(&effective)
	if gfx.query_shader_state(shader) != .valid {
		gfx.destroy_shader(shader)
		return error(err_multiwindow_render_resource_failed)
	}
	mut runtime := resources.app.render_runtime
	runtime.mutex.lock()
	key := runtime.resources.reserve(.shader, resources.window, resources.scope == .app) or {
		runtime.mutex.unlock()
		gfx.destroy_shader(shader)
		return err
	}
	mut slot := &runtime.resources.slots[key.slot]
	slot.shader = shader
	slot.shader_desc = pointer_free_shader_desc(effective)
	slot.label = copied.label
	runtime.mutex.unlock()
	return window_shader_id(key)
}

fn (mut resources WindowResourceContext) make_pipeline_managed(desc &gfx.PipelineDesc, shader_id WindowShaderId) !WindowPipelineId {
	resources.validate_managed_resource_context(.create)!
	mut copied := copy_managed_pipeline_desc(desc)!
	clean := copied.desc
	mut runtime := resources.app.render_runtime
	runtime.mutex.lock()
	shader_key := shader_resource_key(shader_id)
	shader_index := runtime.resources.validate(shader_key, .shader, resources.window,
		resources.scope) or {
		runtime.mutex.unlock()
		return err
	}
	shader := runtime.resources.slots[shader_index].shader
	runtime.mutex.unlock()
	mut effective_input := clean
	effective_input.shader = shader
	effective := gfx.query_pipeline_desc_defaults(&effective_input)
	resources.fail_internal_resource_stage(.resource_make_pipeline)!
	resources.app.note_managed_gpu_work(resources.batch_epoch)!
	pipeline := gfx.make_pipeline(&effective)
	if gfx.query_pipeline_state(pipeline) != .valid {
		gfx.destroy_pipeline(pipeline)
		return error(err_multiwindow_render_resource_failed)
	}
	runtime.mutex.lock()
	key := runtime.resources.reserve(.pipeline, resources.window, resources.scope == .app) or {
		runtime.mutex.unlock()
		gfx.destroy_pipeline(pipeline)
		return err
	}
	mut slot := &runtime.resources.slots[key.slot]
	slot.pipeline = pipeline
	slot.pipeline_desc = pointer_free_pipeline_desc(effective)
	slot.label = copied.label
	runtime.resources.add_dependencies(key.slot, [shader_key]) or {
		slot.status = .invalid
		runtime.mutex.unlock()
		gfx.destroy_pipeline(pipeline)
		return err
	}
	runtime.mutex.unlock()
	return window_pipeline_id(key)
}

fn (mut resources WindowResourceContext) make_attachments_managed(config WindowAttachmentsConfig) !WindowAttachmentsId {
	resources.validate_managed_resource_context(.create)!
	recipe := attachments_recipe_from_config(config)
	mut runtime := resources.app.render_runtime
	runtime.mutex.lock()
	desc := runtime.resources.build_attachments_desc(recipe, none, gfx.Image{}, none,
		resources.window, resources.scope) or {
		runtime.mutex.unlock()
		return err
	}
	dependencies := attachment_recipe_dependencies(recipe)
	runtime.mutex.unlock()
	resources.fail_internal_resource_stage(.resource_make_attachments)!
	resources.app.note_managed_gpu_work(resources.batch_epoch)!
	attachments := gfx.make_attachments(&desc)
	if gfx.query_attachments_state(attachments) != .valid {
		gfx.destroy_attachments(attachments)
		return error(err_multiwindow_render_resource_failed)
	}
	runtime.mutex.lock()
	key := runtime.resources.reserve(.attachments, resources.window, resources.scope == .app) or {
		runtime.mutex.unlock()
		gfx.destroy_attachments(attachments)
		return err
	}
	mut slot := &runtime.resources.slots[key.slot]
	slot.attachments = attachments
	slot.attachments_recipe = recipe
	slot.target_identity = 1
	runtime.resources.add_dependencies(key.slot, dependencies) or {
		slot.status = .invalid
		runtime.mutex.unlock()
		gfx.destroy_attachments(attachments)
		return err
	}
	runtime.mutex.unlock()
	return window_attachments_id(key)
}

fn (mut resources WindowResourceContext) make_sgl_pipeline_managed(desc &gfx.PipelineDesc) !WindowSglPipelineId {
	return resources.make_sgl_pipeline_recipe(desc, none)
}

fn (mut resources WindowResourceContext) make_sgl_pipeline_with_shader_managed(desc &gfx.PipelineDesc, shader WindowShaderId) !WindowSglPipelineId {
	return resources.make_sgl_pipeline_recipe(desc, shader_resource_key(shader))
}

fn (mut resources WindowResourceContext) make_sgl_pipeline_recipe(desc &gfx.PipelineDesc, shader ?MultiWindowResourceKey) !WindowSglPipelineId {
	resources.validate_managed_resource_context(.create)!
	mut copied := copy_managed_pipeline_desc(desc)!
	recipe := copied.desc
	mut runtime := resources.app.render_runtime
	runtime.mutex.lock()
	mut dependencies := []MultiWindowResourceKey{}
	if shader_key := shader {
		runtime.resources.validate(shader_key, .shader, resources.window, resources.scope) or {
			runtime.mutex.unlock()
			return err
		}
		dependencies << shader_key
	}
	if message := runtime.take_internal_fault_locked(.resource_make_sgl_recipe) {
		runtime.mutex.unlock()
		return error(message)
	}
	key := runtime.resources.reserve(.sgl_pipeline, resources.window, resources.scope == .app) or {
		runtime.mutex.unlock()
		return err
	}
	mut slot := &runtime.resources.slots[key.slot]
	slot.sgl_recipe = recipe
	slot.label = copied.label
	if copied.label != '' {
		slot.sgl_recipe.label = &char(slot.label.str)
	}
	slot.sgl_shader = shader
	runtime.resources.add_dependencies(key.slot, dependencies) or {
		slot.status = .invalid
		runtime.mutex.unlock()
		return err
	}
	runtime.mutex.unlock()
	return window_sgl_pipeline_id(key)
}

fn (mut resources WindowResourceContext) update_buffer_managed(id WindowBufferId, data &gfx.Range) ! {
	resources.validate_managed_resource_context(.update)!
	validate_managed_range(data)!
	mut runtime := resources.app.render_runtime
	runtime.mutex.lock()
	key := buffer_resource_key(id)
	plan := runtime.resources.plan_buffer_mutation(key, resources.window, resources.scope,
		resources.batch_epoch, .update, data.size) or {
		runtime.mutex.unlock()
		return err
	}
	runtime.mutex.unlock()
	resources.fail_internal_resource_stage(.resource_update_buffer)!
	resources.app.note_managed_gpu_work(resources.batch_epoch)!
	runtime.mutex.lock()
	runtime.resources.commit_buffer_mutation(plan)
	runtime.mutex.unlock()
	gfx.update_buffer(plan.buffer, data)
}

fn (mut resources WindowResourceContext) append_buffer_managed(id WindowBufferId, data &gfx.Range) !int {
	resources.validate_managed_resource_context(.append)!
	validate_managed_range(data)!
	mut runtime := resources.app.render_runtime
	runtime.mutex.lock()
	key := buffer_resource_key(id)
	plan := runtime.resources.plan_buffer_mutation(key, resources.window, resources.scope,
		resources.batch_epoch, .append, data.size) or {
		runtime.mutex.unlock()
		return err
	}
	runtime.mutex.unlock()
	if gfx.query_buffer_will_overflow(plan.buffer, data.size) {
		return error(err_multiwindow_render_buffer_overflow)
	}
	resources.fail_internal_resource_stage(.resource_append_buffer)!
	resources.app.note_managed_gpu_work(resources.batch_epoch)!
	offset := gfx.append_buffer(plan.buffer, data)
	runtime.mutex.lock()
	runtime.resources.commit_buffer_mutation(plan)
	runtime.mutex.unlock()
	return offset
}

fn (mut resources WindowResourceContext) update_image_managed(id WindowImageId, data &gfx.ImageData) ! {
	resources.validate_managed_resource_context(.update)!
	validate_image_data(data, true)!
	mut runtime := resources.app.render_runtime
	runtime.mutex.lock()
	key := image_resource_key(id)
	plan := runtime.resources.plan_image_update(key, resources.window, resources.scope,
		resources.batch_epoch) or {
		runtime.mutex.unlock()
		return err
	}
	runtime.mutex.unlock()
	resources.fail_internal_resource_stage(.resource_update_image)!
	resources.app.note_managed_gpu_work(resources.batch_epoch)!
	runtime.mutex.lock()
	runtime.resources.commit_image_mutation(plan)
	runtime.mutex.unlock()
	gfx.update_image(plan.image, data)
}

fn (mut resources WindowResourceContext) replace_image_managed(id WindowImageId, desc &gfx.ImageDesc) !WindowImageId {
	resources.validate_managed_resource_context(.replace)!
	mut app := resources.app
	mut copied := copy_managed_image_desc(desc)!
	effective := gfx.query_image_desc_defaults(&copied.desc)
	validate_effective_image_desc(effective)!
	mut runtime := app.render_runtime
	runtime.mutex.lock()
	image_key := image_resource_key(id)
	image_index, old_image, replacement_plans := runtime.resources.plan_image_replacement(image_key,
		resources.window, resources.scope, resources.batch_epoch, effective) or {
		runtime.mutex.unlock()
		return err
	}
	runtime.mutex.unlock()
	resources.fail_internal_resource_stage(.resource_replace_image)!
	app.note_managed_gpu_work(resources.batch_epoch)!
	replacement := gfx.make_image(&effective)
	if !app.core.renderer_device_available_for_gg() {
		return error(err_multiwindow_render_backend_unavailable)
	}
	if gfx.query_image_state(replacement) != .valid {
		cleanup_unpublished_image_replacement(app, replacement, [])
		return error(err_multiwindow_render_resource_failed)
	}

	mut new_attachments := []gfx.Attachments{cap: replacement_plans.len}
	for plan in replacement_plans {
		runtime.mutex.lock()
		attachment_desc := runtime.resources.build_unpublished_attachment_desc(image_key,
			image_index, old_image, plan, replacement) or {
			runtime.mutex.unlock()
			cleanup_unpublished_image_replacement(app, replacement, new_attachments)
			return err
		}
		runtime.mutex.unlock()
		if !app.core.renderer_device_available_for_gg() {
			return error(err_multiwindow_render_backend_unavailable)
		}
		if message := runtime.take_internal_fault(.resource_rebuild_attachment) {
			cleanup_unpublished_image_replacement(app, replacement, new_attachments)
			return error(message)
		}
		created := gfx.make_attachments(&attachment_desc)
		if !app.core.renderer_device_available_for_gg() {
			return error(err_multiwindow_render_backend_unavailable)
		}
		if gfx.query_attachments_state(created) != .valid {
			new_attachments << created
			cleanup_unpublished_image_replacement(app, replacement, new_attachments)
			return error(err_multiwindow_render_resource_failed)
		}
		new_attachments << created
	}
	runtime.mutex.lock()
	runtime.resources.validate_image_replacement_publish(image_key, image_index, old_image,
		replacement_plans, resources.window, resources.scope, resources.batch_epoch) or {
		runtime.mutex.unlock()
		cleanup_unpublished_image_replacement(app, replacement, new_attachments)
		return err
	}
	if replacement_plans.len != new_attachments.len {
		runtime.mutex.unlock()
		cleanup_unpublished_image_replacement(app, replacement, new_attachments)
		return error(err_multiwindow_render_resource_failed)
	}
	mut retired_attachment_targets := []ManagedRetiredAttachmentTarget{cap: replacement_plans.len}
	for plan in replacement_plans {
		retired_attachment_targets << ManagedRetiredAttachmentTarget{
			key:             plan.key
			target_identity: plan.target_identity
		}
	}
	runtime.resources.commit_image_replacement(image_index, old_image, replacement, effective,
		copied.label, replacement_plans, new_attachments, resources.batch_epoch)
	runtime.mutex.unlock()
	for retired in retired_attachment_targets {
		app.defer_attachment_sgl_target_retirement(retired.key, retired.target_identity,
			resources.batch_epoch)
	}
	return id
}

fn (registry &MultiWindowResourceRegistry) plan_image_replacement(image_key MultiWindowResourceKey, window WindowId, scope MultiWindowResourceScope, batch u64, replacement_desc gfx.ImageDesc) !(int, gfx.Image, []ManagedAttachmentReplacement) {
	image_index := registry.validate(image_key, .image, window, scope)!
	registry.validate_mutation_scope(image_index, scope)!
	image_slot := registry.slots[image_index]
	if batch == 0 || image_slot.last_mutation_batch == batch {
		return error(err_multiwindow_render_update_twice)
	}
	mut plans := []ManagedAttachmentReplacement{cap: image_slot.dependents.len}
	for dependent_key in image_slot.dependents {
		dependent_index := registry.validate_any(dependent_key)!
		dependent := registry.slots[dependent_index]
		if dependent.kind != .attachments {
			return error(err_multiwindow_render_resource_has_dependents)
		}
		registry.validate_attachments_recipe(dependent.attachments_recipe, image_key,
			replacement_desc)!
		next_identity := next_gg_target_identity(dependent.target_identity)!
		plans << ManagedAttachmentReplacement{
			key:                  dependent_key
			index:                dependent_index
			recipe:               dependent.attachments_recipe
			old:                  dependent.attachments
			target_identity:      dependent.target_identity
			next_target_identity: next_identity
		}
	}
	return image_index, image_slot.image, plans
}

fn (registry &MultiWindowResourceRegistry) build_unpublished_attachment_desc(image_key MultiWindowResourceKey, image_index int, old_image gfx.Image, plan ManagedAttachmentReplacement, replacement gfx.Image) !gfx.AttachmentsDesc {
	validated_image := registry.validate_any(image_key)!
	if validated_image != image_index || registry.slots[image_index].image.id != old_image.id {
		return error(err_multiwindow_render_stale_resource)
	}
	validated_attachment := registry.validate_any(plan.key)!
	if validated_attachment != plan.index || registry.slots[plan.index].kind != .attachments
		|| registry.slots[plan.index].attachments.id != plan.old.id
		|| registry.slots[plan.index].target_identity != plan.target_identity {
		return error(err_multiwindow_render_stale_resource)
	}
	return registry.build_prevalidated_replacement_attachments_desc(plan.recipe, image_key,
		replacement)
}

fn (registry &MultiWindowResourceRegistry) build_prevalidated_replacement_attachments_desc(recipe MultiWindowAttachmentsRecipe, image_key MultiWindowResourceKey, replacement gfx.Image) !gfx.AttachmentsDesc {
	mut desc := gfx.AttachmentsDesc{}
	for i, key in recipe.colors {
		index := registry.validate_any(key)!
		desc.colors[i].image = if key == image_key {
			replacement
		} else {
			registry.slots[index].image
		}
	}
	for i, key in recipe.resolves {
		index := registry.validate_any(key)!
		desc.resolves[i].image = if key == image_key {
			replacement
		} else {
			registry.slots[index].image
		}
	}
	if key := recipe.depth_stencil {
		index := registry.validate_any(key)!
		desc.depth_stencil.image = if key == image_key {
			replacement
		} else {
			registry.slots[index].image
		}
	}
	return desc
}

fn (registry &MultiWindowResourceRegistry) validate_image_replacement_publish(image_key MultiWindowResourceKey, image_index int, old_image gfx.Image, plans []ManagedAttachmentReplacement, window WindowId, scope MultiWindowResourceScope, batch u64) ! {
	validated_image := registry.validate(image_key, .image, window, scope)!
	if validated_image != image_index || registry.slots[image_index].image.id != old_image.id
		|| registry.slots[image_index].last_mutation_batch == batch
		|| registry.slots[image_index].dependents.len != plans.len {
		return error(err_multiwindow_render_stale_resource)
	}
	for plan in plans {
		if plan.key !in registry.slots[image_index].dependents {
			return error(err_multiwindow_render_stale_resource)
		}
		validated_attachment := registry.validate_any(plan.key)!
		if validated_attachment != plan.index || registry.slots[plan.index].kind != .attachments
			|| registry.slots[plan.index].attachments.id != plan.old.id
			|| registry.slots[plan.index].target_identity != plan.target_identity {
			return error(err_multiwindow_render_stale_resource)
		}
	}
}

fn (mut registry MultiWindowResourceRegistry) commit_image_replacement(image_index int, old_image gfx.Image, replacement gfx.Image, desc gfx.ImageDesc, label string, plans []ManagedAttachmentReplacement, new_attachments []gfx.Attachments, batch u64) {
	mut image_slot := &registry.slots[image_index]
	image_slot.last_mutation_batch = batch
	for i, plan in plans {
		mut attachment_slot := &registry.slots[plan.index]
		attachment_slot.attachments = new_attachments[i]
		attachment_slot.target_identity = plan.next_target_identity
		registry.defer_replaced(.attachments, gfx.Image{}, plan.old, batch)
	}
	image_slot.image = replacement
	image_slot.image_desc = pointer_free_image_desc(desc)
	image_slot.label = label
	registry.defer_replaced(.image, old_image, gfx.Attachments{}, batch)
}

fn cleanup_unpublished_image_replacement(app &App, image gfx.Image, attachments []gfx.Attachments) {
	if !app.core.renderer_device_available_for_gg() {
		return
	}
	for attachment in attachments {
		gfx.destroy_attachments(attachment)
	}
	if image.id != 0 {
		gfx.destroy_image(image)
	}
}

fn (mut resources WindowResourceContext) retire_managed(key MultiWindowResourceKey, kind MultiWindowResourceKind) ! {
	resources.validate_managed_retire_context()!
	mut runtime := resources.app.render_runtime
	runtime.mutex.lock()
	plan := runtime.resources.plan_retirement(key, kind, resources.window, resources.scope,
		resources.batch_epoch) or {
		runtime.mutex.unlock()
		return err
	}
	runtime.mutex.unlock()
	if plan.needs_boundary {
		resources.app.note_managed_retirement_boundary(resources.batch_epoch)!
	}
	runtime.mutex.lock()
	runtime.resources.commit_retirement(plan)
	runtime.mutex.unlock()
}

fn (resources &WindowResourceContext) validate_managed_retire_context() ! {
	if resources.app == unsafe { nil } || resources.app_instance != resources.app.app_instance {
		return error(err_multiwindow_render_stale_lease)
	}
	resources.app.assert_owner_thread() or { return error(err_multiwindow_render_owner_thread) }
	resources.app.render_runtime.validate_resource_context(resources, .retire)!
}

fn (resources &WindowResourceContext) validate_managed_resource_context(operation MultiWindowResourceOperation) ! {
	if resources.app == unsafe { nil } || resources.app_instance != resources.app.app_instance {
		return error(err_multiwindow_render_stale_lease)
	}
	resources.app.assert_owner_thread() or { return error(err_multiwindow_render_owner_thread) }
	resources.app.render_runtime.validate_resource_context(resources, operation)!
	if !resources.app.gfx_started || !resources.app.core.renderer_is_usable() {
		return error(err_multiwindow_render_backend_unavailable)
	}
}

fn (resources &WindowResourceContext) fail_internal_resource_stage(stage MultiWindowInternalFaultStage) ! {
	mut runtime := resources.app.render_runtime
	if message := runtime.take_internal_fault(stage) {
		return error(message)
	}
}

fn (app &App) note_managed_gpu_work(batch_epoch u64) ! {
	if app.active_batch_epoch == 0 || app.active_batch_epoch != batch_epoch {
		return error(err_multiwindow_render_stale_lease)
	}
	mut core := app.core
	core.note_render_gpu_work(app.active_batch_lease)!
}

fn (app &App) note_managed_retirement_boundary(batch_epoch u64) ! {
	if app.gfx_started && app.core.renderer_is_usable() && app.active_batch_epoch != 0 {
		app.note_managed_gpu_work(batch_epoch)!
	}
}

fn validate_managed_range(data &gfx.Range) ! {
	if data == unsafe { nil } || data.ptr == unsafe { nil } || data.size == 0 {
		return error(err_multiwindow_render_invalid_descriptor)
	}
}

fn (app &App) managed_pipeline(id WindowPipelineId, window WindowId) !gfx.Pipeline {
	mut runtime := app.render_runtime
	runtime.mutex.lock()
	index := runtime.resources.validate(pipeline_resource_key(id), .pipeline, window, .window) or {
		runtime.mutex.unlock()
		return err
	}
	pipeline := runtime.resources.slots[index].pipeline
	runtime.mutex.unlock()
	return pipeline
}

fn (app &App) managed_attachments(id WindowAttachmentsId, window WindowId) !(gfx.Attachments, WindowRenderTargetInfo, string) {
	mut runtime := app.render_runtime
	runtime.mutex.lock()
	index := runtime.resources.validate(attachments_resource_key(id), .attachments, window, .window) or {
		runtime.mutex.unlock()
		return err
	}
	slot := runtime.resources.slots[index]
	compatibility := runtime.resources.attachments_compatibility(slot.attachments_recipe) or {
		runtime.mutex.unlock()
		return err
	}
	key := '${app.app_instance}:${window.str()}:offscreen:${id.app_instance}:${id.slot}:${id.generation}:${slot.target_identity}:${int(compatibility.color_format)}:${int(compatibility.depth_format)}:${compatibility.sample_count}'
	runtime.mutex.unlock()
	return slot.attachments, compatibility, key
}

fn (app &App) managed_sgl_pipeline(id WindowSglPipelineId, window WindowId, context sgl.Context, target_key string) !sgl.Pipeline {
	if !app.gfx_started || !app.core.renderer_device_available_for_gg() {
		return error(err_multiwindow_render_backend_unavailable)
	}
	if target_key !in app.sgl_contexts || app.sgl_contexts[target_key] != context
		|| target_key !in app.sgl_context_targets {
		return error(err_multiwindow_render_stale_lease)
	}
	mut runtime := app.render_runtime
	runtime.mutex.lock()
	index := runtime.resources.validate(sgl_pipeline_resource_key(id), .sgl_pipeline, window,
		.window) or {
		runtime.mutex.unlock()
		return err
	}
	mut slot := &runtime.resources.slots[index]
	if target_key in slot.materialized_sgl {
		pipeline := slot.materialized_sgl[target_key].pipeline
		runtime.mutex.unlock()
		return pipeline
	}
	mut recipe := slot.sgl_recipe
	if shader_key := slot.sgl_shader {
		shader_index := runtime.resources.validate_any(shader_key) or {
			runtime.mutex.unlock()
			return err
		}
		recipe.shader = runtime.resources.slots[shader_index].shader
	}
	runtime.mutex.unlock()
	if !app.core.renderer_device_available_for_gg() {
		return error(err_multiwindow_render_backend_unavailable)
	}
	if message := runtime.take_internal_fault(.resource_sgl_materialization) {
		return error(message)
	}
	app.note_managed_gpu_work(app.active_batch_epoch)!
	previous := sgl.get_context()
	sgl.set_context(context)
	pipeline := sgl.context_make_pipeline(context, &recipe)
	sgl.set_context(previous)
	if !app.core.renderer_device_available_for_gg() {
		return error(err_multiwindow_render_backend_unavailable)
	}
	if pipeline.id == 0 {
		return error(err_multiwindow_render_resource_failed)
	}
	runtime.mutex.lock()
	validated := runtime.resources.validate(sgl_pipeline_resource_key(id), .sgl_pipeline, window,
		.window) or {
		runtime.mutex.unlock()
		if pipeline.id != 0 && app.core.renderer_device_available_for_gg() {
			sgl.destroy_pipeline(pipeline)
		}
		return err
	}
	runtime.resources.slots[validated].materialized_sgl[target_key] = MultiWindowSglMaterialization{
		pipeline:    pipeline
		context_key: target_key
	}
	runtime.mutex.unlock()
	return pipeline
}

fn (app &App) managed_image_sampler(image_id WindowImageId, sampler_id WindowSamplerId, window WindowId) !(gfx.Image, gfx.Sampler) {
	mut runtime := app.render_runtime
	runtime.mutex.lock()
	image_index := runtime.resources.validate(image_resource_key(image_id), .image, window, .window) or {
		runtime.mutex.unlock()
		return err
	}
	sampler_index := runtime.resources.validate(sampler_resource_key(sampler_id), .sampler, window,
		.window) or {
		runtime.mutex.unlock()
		return err
	}
	image := runtime.resources.slots[image_index].image
	sampler := runtime.resources.slots[sampler_index].sampler
	runtime.mutex.unlock()
	return image, sampler
}

fn copy_managed_buffer_desc(desc &gfx.BufferDesc) !ManagedBufferDescCopy {
	if desc == unsafe { nil } {
		return error(err_multiwindow_render_invalid_descriptor)
	}
	if desc.gl_buffers != [2]u32{} || desc.mtl_buffers[0] != unsafe { nil }
		|| desc.mtl_buffers[1] != unsafe { nil } || desc.d3d11_buffer != unsafe { nil }
		|| desc.wgpu_buffer != unsafe { nil } {
		return error(err_multiwindow_render_native_injection)
	}
	mut result := ManagedBufferDescCopy{
		desc: *desc
	}
	if desc._start_canary != 0 || desc._end_canary != 0 {
		return error(err_multiwindow_render_invalid_descriptor)
	}
	if desc.label != unsafe { nil } {
		result.label = unsafe { desc.label.vstring() }.clone()
		result.desc.label = &char(result.label.str)
	}
	if desc.data.size > 0 {
		if desc.data.ptr == unsafe { nil } || desc.data.size > usize(0x7fffffff) {
			return error(err_multiwindow_render_invalid_descriptor)
		}
		result.data = unsafe { desc.data.ptr.vbytes(int(desc.data.size)).clone() }
		result.desc.data = gfx.Range{
			ptr:  result.data.data
			size: usize(result.data.len)
		}
	} else if desc.data.ptr != unsafe { nil } {
		return error(err_multiwindow_render_invalid_descriptor)
	}
	return result
}

fn validate_raw_buffer_desc(desc &gfx.BufferDesc) ! {
	if desc == unsafe { nil } {
		return error(err_multiwindow_render_invalid_descriptor)
	}
	if desc.size > usize(0x7fffffff) || desc.data.size > usize(0x7fffffff) {
		return error(err_multiwindow_render_invalid_descriptor)
	}
	match desc.usage {
		._default, .immutable {
			if desc.data.ptr == unsafe { nil } || desc.data.size == 0 {
				return error(err_multiwindow_render_invalid_descriptor)
			}
			if desc.size != 0 && desc.size != desc.data.size {
				return error(err_multiwindow_render_invalid_descriptor)
			}
		}
		.dynamic, .stream {
			if desc.size == 0 || desc.data.ptr != unsafe { nil } || desc.data.size != 0 {
				return error(err_multiwindow_render_invalid_descriptor)
			}
		}
		else {
			return error(err_multiwindow_render_invalid_descriptor)
		}
	}
}

fn validate_effective_buffer_desc(desc gfx.BufferDesc) ! {
	if desc.size == 0 || desc.size > usize(0x7fffffff) || desc.data.size > usize(0x7fffffff) {
		return error(err_multiwindow_render_invalid_descriptor)
	}
	if desc.type != .vertexbuffer && desc.type != .indexbuffer && desc.type != .storagebuffer {
		return error(err_multiwindow_render_invalid_descriptor)
	}
	match desc.usage {
		.immutable {
			if desc.data.ptr == unsafe { nil } || desc.data.size == 0 || desc.size != desc.data.size {
				return error(err_multiwindow_render_invalid_descriptor)
			}
		}
		.dynamic, .stream {
			if desc.data.ptr != unsafe { nil } || desc.data.size != desc.size {
				return error(err_multiwindow_render_invalid_descriptor)
			}
		}
		else {
			return error(err_multiwindow_render_invalid_descriptor)
		}
	}
}

fn copy_managed_image_desc(desc &gfx.ImageDesc) !ManagedImageDescCopy {
	if desc == unsafe { nil } {
		return error(err_multiwindow_render_invalid_descriptor)
	}
	if desc.gl_textures != [2]u32{} || desc.gl_texture_target != 0
		|| desc.mtl_textures[0] != unsafe { nil } || desc.mtl_textures[1] != unsafe { nil }
		|| desc.d3d11_texture != unsafe { nil } || desc.d3d11_shader_resource_view != unsafe { nil }
		|| desc.wgpu_texture != unsafe { nil } || desc.wgpu_texture_view != unsafe { nil } {
		return error(err_multiwindow_render_native_injection)
	}
	mut result := ManagedImageDescCopy{
		desc: *desc
	}
	if desc._start_canary != 0 || desc._end_canary != 0 {
		return error(err_multiwindow_render_invalid_descriptor)
	}
	if desc.label != unsafe { nil } {
		result.label = unsafe { desc.label.vstring() }.clone()
		result.desc.label = &char(result.label.str)
	}
	for face in 0 .. gfx.sg_cubeface_num {
		for mip in 0 .. gfx.sg_max_mipmaps {
			range := desc.data.subimage[face][mip]
			if range.size == 0 {
				if range.ptr != unsafe { nil } {
					return error(err_multiwindow_render_invalid_descriptor)
				}
				continue
			}
			if range.ptr == unsafe { nil } || range.size > usize(0x7fffffff) {
				return error(err_multiwindow_render_invalid_descriptor)
			}
			owned := unsafe { range.ptr.vbytes(int(range.size)).clone() }
			result.data << owned
			result.desc.data.subimage[face][mip] = gfx.Range{
				ptr:  result.data[result.data.len - 1].data
				size: usize(owned.len)
			}
		}
	}
	return result
}

fn validate_effective_image_desc(desc gfx.ImageDesc) ! {
	if desc.width <= 0 || desc.height <= 0 || desc.sample_count <= 0 {
		return error(err_multiwindow_render_invalid_descriptor)
	}
	has_data := desc.data.subimage[0][0].ptr != unsafe { nil }
	if desc.render_target {
		if has_data || !gfx.query_pixelformat(desc.pixel_format).render {
			return error(err_multiwindow_render_invalid_descriptor)
		}
	} else if desc.usage == .immutable {
		if !has_data {
			return error(err_multiwindow_render_invalid_descriptor)
		}
	} else if has_data {
		return error(err_multiwindow_render_invalid_descriptor)
	}
}

fn copy_managed_sampler_desc(desc &gfx.SamplerDesc) !ManagedSamplerDescCopy {
	if desc == unsafe { nil } {
		return error(err_multiwindow_render_invalid_descriptor)
	}
	if desc.gl_sampler != 0 || desc.mtl_sampler != unsafe { nil }
		|| desc.d3d11_sampler != unsafe { nil } || desc.wgpu_sampler != unsafe { nil } {
		return error(err_multiwindow_render_native_injection)
	}
	mut result := ManagedSamplerDescCopy{
		desc: *desc
	}
	if desc._start_canary != 0 || desc._end_canary != 0 {
		return error(err_multiwindow_render_invalid_descriptor)
	}
	if desc.label != unsafe { nil } {
		result.label = unsafe { desc.label.vstring() }.clone()
		result.desc.label = &char(result.label.str)
	}
	return result
}

fn copy_managed_pipeline_desc(desc &gfx.PipelineDesc) !ManagedPipelineDescCopy {
	if desc == unsafe { nil } {
		return error(err_multiwindow_render_invalid_descriptor)
	}
	if desc.shader.id != 0 {
		return error(err_multiwindow_render_native_injection)
	}
	mut result := ManagedPipelineDescCopy{
		desc: *desc
	}
	if desc._start_canary != 0 || desc._end_canary != 0 {
		return error(err_multiwindow_render_invalid_descriptor)
	}
	result.desc.shader = gfx.Shader{}
	if desc.label != unsafe { nil } {
		result.label = unsafe { desc.label.vstring() }.clone()
		result.desc.label = &char(result.label.str)
	}
	return result
}

fn copy_managed_shader_desc(desc &gfx.ShaderDesc) !ManagedShaderDescCopy {
	if desc == unsafe { nil } {
		return error(err_multiwindow_render_invalid_descriptor)
	}
	mut result := ManagedShaderDescCopy{
		desc: *desc
	}
	if desc._start_canary != 0 || desc._end_canary != 0 {
		return error(err_multiwindow_render_invalid_descriptor)
	}
	result.desc.label = copy_managed_cstring(result.desc.label, mut result.strings)!
	if desc.label != unsafe { nil } {
		result.label = unsafe { desc.label.vstring() }.clone()
	}
	for i in 0 .. result.desc.attrs.len {
		result.desc.attrs[i].name = copy_managed_cstring(result.desc.attrs[i].name, mut
			result.strings)!
		result.desc.attrs[i].sem_name = copy_managed_cstring(result.desc.attrs[i].sem_name, mut
			result.strings)!
	}
	copy_managed_shader_stage(mut result.desc.vs, mut result.strings, mut result.bytes)!
	copy_managed_shader_stage(mut result.desc.fs, mut result.strings, mut result.bytes)!
	return result
}

fn copy_managed_shader_stage(mut stage gfx.ShaderStageDesc, mut strings []string, mut bytes [][]u8) ! {
	stage.source = copy_managed_cstring(stage.source, mut strings)!
	stage.entry = copy_managed_cstring(stage.entry, mut strings)!
	stage.d3d11_target = copy_managed_cstring(stage.d3d11_target, mut strings)!
	if stage.bytecode.size > 0 {
		if stage.bytecode.ptr == unsafe { nil } || stage.bytecode.size > usize(0x7fffffff) {
			return error(err_multiwindow_render_invalid_descriptor)
		}
		owned := unsafe { stage.bytecode.ptr.vbytes(int(stage.bytecode.size)).clone() }
		bytes << owned
		stage.bytecode = gfx.Range{
			ptr:  bytes[bytes.len - 1].data
			size: usize(owned.len)
		}
	} else if stage.bytecode.ptr != unsafe { nil } {
		return error(err_multiwindow_render_invalid_descriptor)
	}
	for block in 0 .. stage.uniform_blocks.len {
		for uniform in 0 .. stage.uniform_blocks[block].uniforms.len {
			stage.uniform_blocks[block].uniforms[uniform].name = copy_managed_cstring(stage.uniform_blocks[block].uniforms[uniform].name, mut
				strings)!
		}
	}
	for i in 0 .. stage.image_sampler_pairs.len {
		stage.image_sampler_pairs[i].glsl_name = copy_managed_cstring(stage.image_sampler_pairs[i].glsl_name, mut
			strings)!
	}
}

fn copy_managed_cstring(value &char, mut owned []string) !&char {
	if value == unsafe { nil } {
		return unsafe { nil }
	}
	text := unsafe { value.vstring() }.clone()
	owned << text
	return &char(owned[owned.len - 1].str)
}

fn pointer_free_image_desc(desc gfx.ImageDesc) gfx.ImageDesc {
	return gfx.ImageDesc{
		type:          desc.type
		render_target: desc.render_target
		width:         desc.width
		height:        desc.height
		num_slices:    desc.num_slices
		num_mipmaps:   desc.num_mipmaps
		usage:         desc.usage
		pixel_format:  desc.pixel_format
		sample_count:  desc.sample_count
	}
}

fn pointer_free_buffer_desc(desc gfx.BufferDesc) gfx.BufferDesc {
	return gfx.BufferDesc{
		size:  desc.size
		type:  desc.type
		usage: desc.usage
	}
}

fn pointer_free_shader_desc(desc gfx.ShaderDesc) gfx.ShaderDesc {
	mut clean := desc
	clean.label = unsafe { nil }
	for i in 0 .. clean.attrs.len {
		clean.attrs[i].name = unsafe { nil }
		clean.attrs[i].sem_name = unsafe { nil }
	}
	pointer_free_shader_stage(mut clean.vs)
	pointer_free_shader_stage(mut clean.fs)
	return clean
}

fn pointer_free_shader_stage(mut stage gfx.ShaderStageDesc) {
	stage.source = unsafe { nil }
	stage.bytecode = gfx.Range{}
	stage.entry = unsafe { nil }
	stage.d3d11_target = unsafe { nil }
	for block in 0 .. stage.uniform_blocks.len {
		for uniform in 0 .. stage.uniform_blocks[block].uniforms.len {
			stage.uniform_blocks[block].uniforms[uniform].name = unsafe { nil }
		}
	}
	for i in 0 .. stage.image_sampler_pairs.len {
		stage.image_sampler_pairs[i].glsl_name = unsafe { nil }
	}
}

fn pointer_free_pipeline_desc(desc gfx.PipelineDesc) gfx.PipelineDesc {
	mut clean := desc
	clean.shader = gfx.Shader{}
	clean.label = unsafe { nil }
	return clean
}

fn pointer_free_sampler_desc(desc gfx.SamplerDesc) gfx.SamplerDesc {
	return gfx.SamplerDesc{
		min_filter:     desc.min_filter
		mag_filter:     desc.mag_filter
		mipmap_filter:  desc.mipmap_filter
		wrap_u:         desc.wrap_u
		wrap_v:         desc.wrap_v
		wrap_w:         desc.wrap_w
		min_lod:        desc.min_lod
		max_lod:        desc.max_lod
		border_color:   desc.border_color
		compare:        desc.compare
		max_anisotropy: desc.max_anisotropy
	}
}

fn validate_image_data(data &gfx.ImageData, require_data bool) ! {
	if data == unsafe { nil } {
		return error(err_multiwindow_render_invalid_descriptor)
	}
	mut found := false
	for face in 0 .. gfx.sg_cubeface_num {
		for mip in 0 .. gfx.sg_max_mipmaps {
			range := data.subimage[face][mip]
			if (range.ptr == unsafe { nil }) != (range.size == 0) {
				return error(err_multiwindow_render_invalid_descriptor)
			}
			found = found || range.size > 0
		}
	}
	if require_data && !found {
		return error(err_multiwindow_render_invalid_descriptor)
	}
}

fn attachments_recipe_from_config(config WindowAttachmentsConfig) MultiWindowAttachmentsRecipe {
	mut colors := []MultiWindowResourceKey{cap: config.colors.len}
	mut resolves := []MultiWindowResourceKey{cap: config.resolves.len}
	for id in config.colors {
		colors << image_resource_key(id)
	}
	for id in config.resolves {
		resolves << image_resource_key(id)
	}
	return MultiWindowAttachmentsRecipe{
		colors:        colors
		resolves:      resolves
		depth_stencil: if depth := config.depth_stencil {
			image_resource_key(depth)
		} else {
			none
		}
	}
}

fn attachment_recipe_dependencies(recipe MultiWindowAttachmentsRecipe) []MultiWindowResourceKey {
	mut dependencies := recipe.colors.clone()
	dependencies << recipe.resolves
	if depth := recipe.depth_stencil {
		dependencies << depth
	}
	return dependencies
}

fn (registry &MultiWindowResourceRegistry) build_attachments_desc(recipe MultiWindowAttachmentsRecipe, replacement_key ?MultiWindowResourceKey, replacement gfx.Image, replacement_desc ?gfx.ImageDesc, window WindowId, scope MultiWindowResourceScope) !gfx.AttachmentsDesc {
	if recipe.colors.len == 0 || recipe.colors.len > 4 || recipe.resolves.len > 4
		|| (recipe.resolves.len > 0 && recipe.resolves.len != recipe.colors.len) {
		return error(err_multiwindow_render_invalid_descriptor)
	}
	mut desc := gfx.AttachmentsDesc{}
	for i, key in recipe.colors {
		index := registry.validate(key, .image, window, scope)!
		use_replacement := if candidate := replacement_key { candidate == key } else { false }
		desc.colors[i].image = if use_replacement {
			replacement
		} else {
			registry.slots[index].image
		}
	}
	for i, key in recipe.resolves {
		index := registry.validate(key, .image, window, scope)!
		use_replacement := if candidate := replacement_key { candidate == key } else { false }
		desc.resolves[i].image = if use_replacement {
			replacement
		} else {
			registry.slots[index].image
		}
	}
	if key := recipe.depth_stencil {
		index := registry.validate(key, .image, window, scope)!
		use_replacement := if candidate := replacement_key { candidate == key } else { false }
		desc.depth_stencil.image = if use_replacement {
			replacement
		} else {
			registry.slots[index].image
		}
	}
	registry.validate_attachments_recipe(recipe, replacement_key, replacement_desc)!
	return desc
}

fn (registry &MultiWindowResourceRegistry) validate_attachments_recipe(recipe MultiWindowAttachmentsRecipe, replacement_key ?MultiWindowResourceKey, replacement_desc ?gfx.ImageDesc) ! {
	mut width := 0
	mut height := 0
	mut samples := 0
	for key in recipe.colors {
		image_desc := registry.attachment_image_desc(key, replacement_key, replacement_desc)!
		format := gfx.query_pixelformat(image_desc.pixel_format)
		if !image_desc.render_target || !format.render || format.depth {
			return error(err_multiwindow_render_invalid_descriptor)
		}
		if width == 0 {
			width = image_desc.width
			height = image_desc.height
			samples = image_desc.sample_count
		} else if width != image_desc.width || height != image_desc.height
			|| samples != image_desc.sample_count {
			return error(err_multiwindow_render_invalid_descriptor)
		}
	}
	for i, key in recipe.resolves {
		image_desc := registry.attachment_image_desc(key, replacement_key, replacement_desc)!
		color_desc := registry.attachment_image_desc(recipe.colors[i], replacement_key,
			replacement_desc)!
		if !image_desc.render_target || image_desc.sample_count != 1 || color_desc.sample_count <= 1
			|| image_desc.pixel_format != color_desc.pixel_format || image_desc.width != width
			|| image_desc.height != height {
			return error(err_multiwindow_render_invalid_descriptor)
		}
	}
	if key := recipe.depth_stencil {
		image_desc := registry.attachment_image_desc(key, replacement_key, replacement_desc)!
		if !image_desc.render_target || !gfx.query_pixelformat(image_desc.pixel_format).depth
			|| image_desc.width != width || image_desc.height != height
			|| image_desc.sample_count != samples {
			return error(err_multiwindow_render_invalid_descriptor)
		}
	}
}

fn (registry &MultiWindowResourceRegistry) attachments_compatibility(recipe MultiWindowAttachmentsRecipe) !WindowRenderTargetInfo {
	registry.validate_attachments_recipe(recipe, none, none)!
	color := registry.slots[registry.validate_any(recipe.colors[0])!].image_desc
	mut depth_format := gfx.PixelFormat.none
	if depth := recipe.depth_stencil {
		depth_format = registry.slots[registry.validate_any(depth)!].image_desc.pixel_format
	}
	return WindowRenderTargetInfo{
		color_format: color.pixel_format
		depth_format: depth_format
		sample_count: color.sample_count
	}
}

fn (registry &MultiWindowResourceRegistry) attachment_image_desc(key MultiWindowResourceKey, replacement_key ?MultiWindowResourceKey, replacement_desc ?gfx.ImageDesc) !gfx.ImageDesc {
	if candidate := replacement_key {
		if candidate == key {
			if desc := replacement_desc {
				return pointer_free_image_desc(desc)
			}
			return error(err_multiwindow_render_invalid_descriptor)
		}
	}
	return registry.slots[registry.validate_any(key)!].image_desc
}
