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
	vlib_path := os.dir(@DIR) + '|@vlib|@vmodules'
	dir := os.join_path(os.vtmp_dir(), 'gg_service_contract_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	for enabled in [false, true] {
		mode := if enabled { 'enabled' } else { 'disabled' }
		output := os.join_path(dir, mode)
		options := if enabled { ['-d', 'gg_multiwindow'] } else { ['-b', 'c'] }
		compiler := [@VEXE, '-gc', 'none', '-path', vlib_path, ...options]
		link_result := os.exec([...compiler, '-o', output, fixture])
		assert link_result.exit_code == 0, '${mode} service facade failed link:\n${link_result.output}'
		if !enabled {
			c_output := output + '.c'
			if !os.is_file(c_output) {
				// Older compilers need an explicit C output path to retain the source.
				c_result := os.exec([...compiler, '-o', c_output, fixture])
				assert c_result.exit_code == 0, 'disabled service facade failed structural compilation:\n${c_result.output}'
			}
			generated := os.read_file(c_output)!
			for forbidden in ['x.multiwindow', 'multiwindow__App', 'multiwindow__Backend',
				'v_multiwindow_', 'x11_backend.c.v', 'wayland_backend.c.v', 'appkit_backend.c.v',
				'win32_backend.c.v'] {
				assert !generated.contains(forbidden), 'disabled service facade leaked `${forbidden}` into generated C'
			}
		}
		binary := output + $if windows { '.exe' } $else { '' }
		run_result := os.exec([binary])
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
