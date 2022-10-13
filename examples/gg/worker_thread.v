// Copyright(C) 2019 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license file distributed with this software package.
module main

// Example of how to send a value through a channel from a worker thread to the main/rendering thread.
// This can be useful to do long running computations while keeping your framerate high (60 fps in this example).
import gg
import gx
import math
import time

const (
	win_width   = 600
	win_height  = 700
	bg_color    = gx.white
	count_color = gx.black
)

struct App {
mut:
	gg      &gg.Context = unsafe { nil }
	ch      chan i64
	counter i64
}

fn main() {
	mut app := &App{
		gg: 0
	}
	app.gg = gg.new_context(
		width: win_width
		height: win_height
		create_window: true
		window_title: 'Counter'
		user_data: app
		bg_color: bg_color
		frame_fn: frame
		init_fn: init
	)
	app.gg.run()
}

fn init(mut app App) {
	// Spawn a new worker thread.
	go worker(mut app)
}

// worker simulates a workload. This should be run in a separate thread.
fn worker(mut app App) {
	stopwatch := time.new_stopwatch()
	mut elapsed := stopwatch.elapsed()
	// Do heavy operations here - like invoking a path finding algorithm, load an image or similar.
	for {
		now := stopwatch.elapsed()
		// When done - send the result through a channel to the main/rendering thread.
		app.ch <- (now - elapsed)
		elapsed = now
		time.sleep(1 * time.second)
	}
}

fn frame(mut app App) {
	app.gg.begin()
	size := gg.window_size()
	mut scale_factor := math.round(f32(size.width) / win_width)
	if scale_factor <= 0 {
		scale_factor = 1
	}
	text_cfg := gx.TextCfg{
		size: 64 * int(scale_factor)
	}

	// Try a pop from the channel
	mut count := i64(0)
	if app.ch.try_pop(mut count) == .success {
		// A value was assigned - increase the counter
		app.counter += i64(f64(count) / time.second)
	}

	label := '$app.counter'
	label_width := (f64(label.len * text_cfg.size) / 4.0)
	label_height := (f64(1 * text_cfg.size) / 2.0)
	mut x := f32(size.width) * 0.5 - label_width
	mut y := f32(size.height) * 0.5 - label_height

	app.gg.draw_text(int(x), int(y), label, text_cfg)

	app.gg.end()
}
