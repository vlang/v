module main

import macos

#flag darwin -framework Cocoa

// on_action is the Objective-C IMP for NSMenuItem clicks.
@[unsafe]
fn on_action(_ macos.Id, _ macos.Sel, sender macos.Id) {
	controller := macos.msg_id(sender, 'target')
	params_ptr := macos.get_associated_object(controller, controller)
	if params_ptr == unsafe { nil } {
		return
	}
	params := unsafe { &TrayParams(params_ptr) }
	idx := int(macos.msg_i64(sender, 'tag'))
	if idx >= 0 && idx < params.items.len {
		params.on_click(params.items[idx])
	}
}

@[unsafe]
fn ensure_controller_class() macos.Id {
	cls := macos.get_class('VTrayController')
	if cls != unsafe { nil } {
		return cls
	}
	new_cls := macos.allocate_class_pair(macos.get_class('NSObject'), 'VTrayController')
	macos.add_method(new_cls, 'onAction:', on_action, 'v@:@')
	macos.register_class_pair(new_cls)
	return new_cls
}

fn build_menu(controller macos.Id, params TrayParams) macos.Id {
	menu := macos.msg_id(macos.get_class('NSMenu'), 'new')
	macos.msg_void(menu, 'autorelease')
	macos.msg_void_bool(menu, 'setAutoenablesItems:', false)

	for i, mi in params.items {
		title := macos.nsstring(mi.text)
		item := macos.msg_id3(menu, 'addItemWithTitle:action:keyEquivalent:', title, macos.sel('onAction:'), macos.nsstring(''))
		macos.msg_void_i64(item, 'setTag:', i64(i))
		macos.msg_void1(item, 'setTarget:', controller)
		macos.msg_void(item, 'autorelease')
		macos.msg_void_bool(item, 'setEnabled:', true)
	}
	return menu
}

// tray_app_init initializes NSApplication and the system tray status item.
pub fn tray_app_init(params TrayParams) &TrayInfo {
	app := macos.msg_id(macos.get_class('NSApplication'), 'sharedApplication')

	ensure_controller_class()
	controller := macos.msg_id(macos.alloc('VTrayController'), 'init')

	// Store params on the controller for the click handler.
	params_heap := &TrayParams{
		items:    params.items
		on_click: params.on_click
	}
	macos.set_associated_object(controller, controller, macos.Id(params_heap), macos.assoc_assign)

	// Hide icon from the dock.
	macos.msg_void_i64(app, 'setActivationPolicy:', 2)
	macos.msg_void1(app, 'setDelegate:', controller)

	status_bar := macos.msg_id(macos.get_class('NSStatusBar'), 'systemStatusBar')
	status_item := macos.msg_id_f64(status_bar, 'statusItemWithLength:', -1.0)
	macos.msg_void_bool(status_item, 'setVisible:', true)

	button := macos.msg_id(status_item, 'button')
	img := macos.msg_id1(macos.get_class('NSImage'), 'imageNamed:', macos.nsstring('icon.png'))
	macos.msg_void1(button, 'setImage:', img)

	menu := build_menu(controller, params)
	macos.msg_void1(status_item, 'setMenu:', menu)

	macos.retain(status_item)
	macos.retain(controller)

	return &TrayInfo{
		app:          app
		app_delegate: controller
	}
}

// tray_app_loop processes a single NSEvent, blocking until one arrives.
@[unsafe]
pub fn tray_app_loop(info &TrayInfo) {
	app := macos.Id(info.app)
	until := macos.msg_id(macos.get_class('NSDate'), 'distantFuture')
	mode := macos.nsstring(macos.run_loop_default_mode)
	mask := u64(18446744073709551615)
	event := macos.msg_id4(app, 'nextEventMatchingMask:untilDate:inMode:dequeue:', macos.Id(mask), until, mode, macos.Id(1))
	if event != unsafe { nil } {
		macos.msg_void1(app, 'sendEvent:', event)
	}
}

// tray_app_run blocks and runs the application.
pub fn tray_app_run(info &TrayInfo) {
	app := macos.Id(info.app)
	macos.msg_void(app, 'run')
}

// tray_app_exit terminates the app.
pub fn tray_app_exit(info &TrayInfo) {
	app := macos.Id(info.app)
	macos.msg_void1(app, 'terminate:', app)
}
