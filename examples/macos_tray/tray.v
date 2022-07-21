// Simple windows-less application that shows a icon button
// on Mac OS tray.
//
// Tested on Mac OS Monterey (12.3).

module main

#include <Cocoa/Cocoa.h>
#flag -framework Cocoa

#include "@VMODROOT/tray.m"

fn C.tray_app_init(TrayParams) &TrayInfo
fn C.tray_app_loop(&TrayInfo)
fn C.tray_app_run(&TrayInfo)
fn C.tray_app_exit(&TrayInfo)

struct TrayMenuItem {
	id   string [required] // Unique ID.
	text string [required] // Text to display.
}

// Parameters to configure the tray button.
struct TrayParams {
	items    []TrayMenuItem         [required]
	on_click fn (item TrayMenuItem)
}

// Internal Cocoa application state.
struct TrayInfo {
	app          voidptr // pointer to NSApplication
	app_delegate voidptr // pointer to AppDelegate
}

[heap]
struct MyApp {
mut:
	tray_info &TrayInfo
}

fn (app &MyApp) on_menu_item_click(item TrayMenuItem) {
	println('click $item.id')
	if item.id == 'quit' {
		C.tray_app_exit(app.tray_info)
	}
}

fn main() {
	mut my_app := &MyApp{
		tray_info: 0
	}

	my_app.tray_info = C.tray_app_init(TrayParams{
		items: [TrayMenuItem{
			id: 'hello'
			text: 'Hello'
		}, TrayMenuItem{
			id: 'quit'
			text: 'Quit!'
		}]
		on_click: my_app.on_menu_item_click
	})

	//// Use this:
	// for {
	// 	C.tray_app_loop(my_app.tray_info)
	//  	// println("loop")
	// }

	//// Or this:
	C.tray_app_run(my_app.tray_info)
}
