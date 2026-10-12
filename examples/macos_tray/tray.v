// vtest build: macos
// Simple windows-less application that shows a icon button on Mac OS tray.
// Tested on Mac OS Monterey (12.3).

module main

#flag -framework Cocoa

#flag @VMODROOT/tray.o

fn C.tray_app_init(titles &&char, count i32, on_click fn (voidptr, i32), user_data voidptr) voidptr
fn C.tray_app_loop(voidptr)
fn C.tray_app_run(voidptr)
fn C.tray_app_exit(voidptr)

struct TrayMenuItem {
	id   string @[required] // Unique ID.
	text string @[required] // Text to display.
}

@[heap]
struct MyApp {
mut:
	tray_info voidptr = unsafe { nil }
	items     []TrayMenuItem
}

fn (app &MyApp) on_menu_item_click(item TrayMenuItem) {
	println('click ${item.id}')
	if item.id == 'quit' {
		C.tray_app_exit(app.tray_info)
	}
}

fn tray_menu_click(user_data voidptr, index i32) {
	app := unsafe { &MyApp(user_data) }
	if index >= 0 && int(index) < app.items.len {
		app.on_menu_item_click(app.items[int(index)])
	}
}

fn main() {
	mut my_app := &MyApp{
		tray_info: unsafe { nil }
	}

	my_app.items = [TrayMenuItem{
		id:   'hello'
		text: 'Hello'
	}, TrayMenuItem{
		id:   'quit'
		text: 'Quit!'
	}]
	mut titles := []&char{cap: my_app.items.len}
	for item in my_app.items {
		titles << &char(item.text.str)
	}
	my_app.tray_info = C.tray_app_init(titles.data, i32(titles.len), tray_menu_click, my_app)

	//// Use this:
	// for {
	// 	C.tray_app_loop(my_app.tray_info)
	//  	// println("loop")
	// }

	//// Or this:
	C.tray_app_run(my_app.tray_info)
}
