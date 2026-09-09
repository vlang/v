// vtest build: !musl? && !self_ubuntu_musl_ci?
module gg

import os

fn test_multiwindow_service_public_types_are_available_without_opt_in() {
	assert WindowOperation.native_borrow.str() == 'native_borrow'
	assert WindowOperation.mouse_lock.str() == 'mouse_lock'
	assert WindowOperation.titlebar_appearance.str() == 'titlebar_appearance'
	assert WindowOperation.image_readback.str() == 'image_readback'
	assert WindowOperation.window_capture.str() == 'window_capture'
	assert WindowTitlebarAppearance.system.str() == 'system'
	assert WindowMappingState.unknown.str() == 'unknown'
	assert WindowState{}.mapping == .unknown
	assert !WindowState{}.monitor_membership_observed
	assert WindowOperationCapability{}.support == .unsupported
	assert WindowMonitorId{}.str() == 'WindowMonitorId(0:0:0)'
	assert ClipboardRequestId{}.str() == 'ClipboardRequestId(0:0)'
	assert PortalParentRequestId{}.str() == 'PortalParentRequestId(0:0)'
	assert PortalParentLeaseId{}.str() == 'PortalParentLeaseId(0:0)'
}

fn test_multiwindow_service_enabled_and_disabled_facades_compile_same_consumer() {
	fixture := os.join_path(@DIR, 'testdata', 'multiwindow_service_consumer.v')
	vlib_dir := os.dir(@DIR)
	for enabled in [false, true] {
		mode := if enabled { 'enabled' } else { 'disabled' }
		c_output := os.join_path(os.temp_dir(), 'gg_multiwindow_service_${mode}.c')
		binary := os.join_path(os.temp_dir(), 'gg_multiwindow_service_${mode}')
		defer {
			os.rm(c_output) or {}
			os.rm(binary) or {}
		}
		define := if enabled { ' -d gg_multiwindow' } else { '' }
		c_cmd := '${os.quoted_path(@VEXE)} -gc none${define} -b c -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(c_output)} ${os.quoted_path(fixture)}'
		c_result := os.execute(c_cmd)
		assert c_result.exit_code == 0, '${mode} service facade failed structural compilation:\n${c_result.output}'
		if !enabled {
			generated := os.read_file(c_output) or { panic(err) }
			for forbidden in ['x.multiwindow', 'multiwindow__App', 'multiwindow__Backend',
				'v_multiwindow_', 'x11_backend.c.v', 'wayland_backend.c.v', 'appkit_backend.c.v',
				'win32_backend.c.v'] {
				assert !generated.contains(forbidden), 'disabled service facade leaked `${forbidden}` into generated C'
			}
		}
		link_cmd := '${os.quoted_path(@VEXE)} -gc none${define} -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(binary)} ${os.quoted_path(fixture)}'
		link_result := os.execute(link_cmd)
		assert link_result.exit_code == 0, '${mode} service facade failed link:\n${link_result.output}'
		run_result := os.execute(os.quoted_path(binary))
		assert run_result.exit_code == 0, '${mode} service facade failed run:\n${run_result.output}'
	}
}

fn test_multiwindow_service_disabled_facade_keeps_clear_opt_in_error() {
	mut app := App{}
	app.monitor_ids() or {
		assert err.msg() == err_multiwindow_not_enabled
		return
	}
	assert false, 'disabled service facade unexpectedly succeeded'
}
