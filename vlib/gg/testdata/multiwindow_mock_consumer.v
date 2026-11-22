module main

import gg
import os

fn main() {
	if os.args.len != 2 {
		panic('expected one mock consumer case')
	}
	match os.args[1] {
		'window_info' { check_window_info()! }
		'enumeration' { check_enumeration()! }
		'event_callback' { check_event_callback()! }
		'queued_events' { check_queued_events()! }
		else { panic('unknown mock consumer case: ${os.args[1]}') }
	}
}

fn check_window_info() ! {
	mut app := gg.new_app(backend: .mock, queue_size: 2)!
	win := app.create_window(
		title:      'Initial'
		width:      320
		height:     200
		min_width:  100
		min_height: 80
		resizable:  false
		visible:    false
		high_dpi:   false
		borderless: true
	)!

	info := app.window_info(win)!
	assert info.id == win
	assert info.title == 'Initial'
	assert info.width == 320
	assert info.height == 200
	assert info.min_width == 100
	assert info.min_height == 80
	assert !info.resizable
	assert !info.visible
	assert !info.high_dpi
	assert info.borderless
	assert !info.fullscreen

	app.set_window_title(win, 'Updated')!
	title_info := app.window_info(win)!
	assert title_info.title == 'Updated'
	assert title_info.width == 320
	assert title_info.height == 200

	app.resize_window(win, 640, 360)!
	resized_info := app.window_info(win)!
	assert resized_info.title == 'Updated'
	assert resized_info.width == 640
	assert resized_info.height == 360
	assert resized_info.min_width == 100
	assert resized_info.min_height == 80

	app.destroy_window(win)!
	assert !app.window_exists(win)

	mut rejected_info := false
	app.window_info(win) or {
		assert err.msg() == 'multiwindow: window handle is stale'
		rejected_info = true
	}
	assert rejected_info

	mut rejected_title := false
	app.set_window_title(win, 'Destroyed') or {
		assert err.msg() == 'multiwindow: window handle is stale'
		rejected_title = true
	}
	assert rejected_title

	mut rejected_resize := false
	app.resize_window(win, 100, 100) or {
		assert err.msg() == 'multiwindow: window handle is stale'
		rejected_resize = true
	}
	assert rejected_resize

	app.stop()!
}

fn check_enumeration() ! {
	close_kind := gg.WindowEventKind.window_close_requested
	assert close_kind == .window_close_requested

	mut app := gg.new_app(backend: .mock, queue_size: 2)!
	assert app.window_ids()!.len == 0
	assert app.window_infos()!.len == 0

	first := app.create_window(title: 'First', width: 100, height: 80)!
	second := app.create_window(title: 'Second', width: 200, height: 120)!
	ids := app.window_ids()!
	assert ids == [first, second]

	infos := app.window_infos()!
	assert infos.len == 2
	assert infos[0].id == first
	assert infos[0].title == 'First'
	assert infos[0].width == 100
	assert infos[0].height == 80
	assert infos[1].id == second
	assert infos[1].title == 'Second'
	assert infos[1].width == 200
	assert infos[1].height == 120

	created_events := app.drain_events()!
	assert created_events.len == 2
	assert created_events[0].kind == .window_created
	assert created_events[0].window == first
	assert created_events[1].kind == .window_created
	assert created_events[1].window == second

	app.resize_window(second, 640, 360)!
	resize_events := app.drain_events()!
	assert resize_events.len == 1
	assert resize_events[0].kind == .window_resized
	assert resize_events[0].window == second
	assert resize_events[0].width == 640
	assert resize_events[0].height == 360

	resized := app.window_infos()!
	assert resized[1].id == second
	assert resized[1].width == 640
	assert resized[1].height == 360

	assert app.poll_events()! == 0
	app.destroy_window(first)!
	assert app.window_ids()! == [second]
	app.stop()!
}

struct SeenResizeEvents {
mut:
	created       int
	resized       int
	resize_width  int
	resize_height int
}

fn check_event_callback() ! {
	mut seen := &SeenResizeEvents{}
	mut app := gg.new_app(backend: .mock, queue_size: 2)!
	win := app.create_window(title: 'Event target', width: 160, height: 90)!
	app.resize_window(win, 320, 180)!

	app.run(
		event_fn: fn [win, mut seen] (event gg.WindowEvent, mut app gg.App) ! {
			match event.kind {
				.window_created {
					assert event.window == win
					seen.created++
				}
				.window_resized {
					assert event.window == win
					assert event.width == 320
					assert event.height == 180
					seen.resized++
					seen.resize_width = event.width
					seen.resize_height = event.height
					app.stop()!
				}
				else {}
			}
		}
	)!

	assert seen.created == 1
	assert seen.resized == 1
	assert seen.resize_width == 320
	assert seen.resize_height == 180
}

struct SeenQueuedEvents {
mut:
	created bool
	resized bool
}

fn check_queued_events() ! {
	mut seen := &SeenQueuedEvents{}
	created := chan gg.WindowId{cap: 1}
	mut app := gg.new_app(backend: .mock, queue_size: 2)!
	app.post(fn [created] (mut app gg.App) ! {
		win := app.create_window(title: 'Queued window', width: 120, height: 80)!
		app.resize_window(win, 240, 160)!
		created <- win
	})!

	app.run(
		event_fn: fn [created, mut seen] (event gg.WindowEvent, mut app gg.App) ! {
			win := <-created
			match event.kind {
				.window_created {
					assert event.window == win
					seen.created = true
					created <- win
				}
				.window_resized {
					assert seen.created
					assert event.window == win
					assert event.width == 240
					assert event.height == 160
					seen.resized = true
					created <- win
					app.stop()!
				}
				else {
					created <- win
				}
			}
		}
	)!

	assert seen.created
	assert seen.resized
}
