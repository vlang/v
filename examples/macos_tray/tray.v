// vtest build: macos
// Simple windows-less application that shows a icon button on Mac OS tray.
// Tested on Mac OS Monterey (12.3).

module main

struct TrayMenuItem {
	id   string @[required] // Unique ID.
	text string @[required] // Text to display.
}

// Parameters to configure the tray button.
struct TrayParams {
	items    []TrayMenuItem         @[required]
	on_click fn (item TrayMenuItem) @[required]
}

// Internal Cocoa application state.
struct TrayInfo {
	app          voidptr // pointer to NSApplication
	app_delegate voidptr // pointer to AppDelegate
}

@[heap]
struct MyApp {
mut:
	tray_info &TrayInfo = unsafe { nil }
}

fn (app &MyApp) on_menu_item_click(item TrayMenuItem) {
	println('click ${item.id}')
	if item.id == 'quit' {
		tray_app_exit(app.tray_info)
	}
}

@[unsafe]
fn main() {
	mut my_app := &MyApp{
		tray_info: unsafe { nil }
	}

	my_app.tray_info = tray_app_init(TrayParams{
		items:    [TrayMenuItem{
			id:   'hello'
			text: 'Hello'
		}, TrayMenuItem{
			id:   'quit'
			text: 'Quit!'
		}]
		on_click: my_app.on_menu_item_click
	})

	//// Use this:
	// for {
	// 	tray_app_loop(my_app.tray_info)
	//  	// println("loop")
	// }

	//// Or this:
	tray_app_run(my_app.tray_info)
}
