module gg

// MultiWindowInternalFaultStage is deliberately private and always compiled
// with the managed runtime. Same-module tests can make failure ordering
// deterministic without exposing a public or test-only control surface.
enum MultiWindowInternalFaultStage {
	none
	renderer_sgl_setup
	resource_make_buffer
	resource_make_image
	resource_make_sampler
	resource_make_shader
	resource_make_pipeline
	resource_make_attachments
	resource_make_sgl_recipe
	resource_update_buffer
	resource_append_buffer
	resource_update_image
	resource_replace_image
	resource_rebuild_attachment
	resource_sgl_materialization
}

struct MultiWindowInternalFaultPlan {
mut:
	stage               MultiWindowInternalFaultStage
	hits_before_failure int
	message             string
}

fn (mut runtime MultiWindowRenderRuntime) set_internal_fault(stage MultiWindowInternalFaultStage, hits_before_failure int, message string) ! {
	if stage == .none || hits_before_failure < 0 || message == '' {
		return error(err_multiwindow_render_fault_config_invalid)
	}
	runtime.mutex.lock()
	runtime.internal_fault = MultiWindowInternalFaultPlan{
		stage:               stage
		hits_before_failure: hits_before_failure
		message:             message
	}
	runtime.mutex.unlock()
}

fn (mut runtime MultiWindowRenderRuntime) clear_internal_fault() {
	runtime.mutex.lock()
	runtime.internal_fault = MultiWindowInternalFaultPlan{}
	runtime.mutex.unlock()
}

fn (mut runtime MultiWindowRenderRuntime) take_internal_fault(stage MultiWindowInternalFaultStage) ?string {
	runtime.mutex.lock()
	defer {
		runtime.mutex.unlock()
	}
	return runtime.take_internal_fault_locked(stage)
}

fn (mut runtime MultiWindowRenderRuntime) take_internal_fault_locked(stage MultiWindowInternalFaultStage) ?string {
	if stage == .none || runtime.internal_fault.stage != stage {
		return none
	}
	if runtime.internal_fault.hits_before_failure > 0 {
		runtime.internal_fault.hits_before_failure--
		return none
	}
	message := runtime.internal_fault.message
	runtime.internal_fault = MultiWindowInternalFaultPlan{}
	return message
}
