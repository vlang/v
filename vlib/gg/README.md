## Description

`gg` is V's simple graphics module.
It is currently implemented using `sokol`, and makes easy creating
apps that just need a way to draw simple 2D shapes, and to react to
user's keyboard/mouse input.

## Example

```v cgen
module main

import gg

fn main() {
	mut context := gg.new_context(
		bg_color:     gg.rgb(174, 198, 255)
		width:        600
		height:       400
		window_title: 'Polygons'
		frame_fn:     frame
	)
	context.run()
}

fn frame(mut ctx gg.Context) {
	ctx.begin()
	ctx.draw_convex_poly([f32(100.0), 100.0, 200.0, 100.0, 300.0, 200.0, 200.0, 300.0, 100.0, 300.0],
		gg.blue)
	ctx.draw_poly_empty([f32(50.0), 50.0, 70.0, 60.0, 90.0, 80.0, 70.0, 110.0], gg.black)
	ctx.draw_triangle_filled(450, 142, 530, 280, 370, 280, gg.red)
	ctx.end()
}
```

## Multi-Window Applications

`gg.App` is an additive multi-window facade for programs that need to manage
more than one native window from the same `gg` application. It is opt-in and is
compiled only when the program is built with `-d gg_multiwindow`:

```sh
v -d gg_multiwindow run examples/gg/multiwindow.v
```

The existing single-window `gg.Context` API and behavior are unchanged. A normal
`import gg` program that uses `gg.new_context()` does not load the native
multi-window implementation. Without `-d gg_multiwindow`, the opt-in API surface
is kept as a lightweight compatibility stub so accidental `gg.App` calls report
a clear "compile with `-d gg_multiwindow`" error instead of pulling in
`x.multiwindow` or native backend code.

Users normally import only `gg`. [`x.multiwindow`](../x/multiwindow/README.md)
is the lower-level lifecycle, window and render-surface layer used by the facade,
and is available for backend or direct-control callers. Use `backend: .auto` for
native applications. It selects the appropriate platform backend at
runtime/build time, falling back only when a native backend is unavailable.
On Linux, X11 native windows are opt-in with `-d x_multiwindow_x11`; Wayland
remains opt-in with `-d sokol_wayland`. Tests and headless tools can request
`backend: .mock` explicitly; the lower-level `.mock` path remains
dependency-light and does not link X11/EGL/OpenGL by default.

The basic lifecycle is:

```v
import gg

fn main() {
	mut app := gg.new_app(backend: .auto)!
	defer {
		app.stop() or {}
	}

	main_window := app.create_window(
		title:  'Main'
		width:  800
		height: 600
	)!

	app.run(
		event_fn: fn (event gg.WindowEvent, mut app gg.App) ! {
			match event.kind {
				.window_close_requested {
					app.destroy_window(event.window)!
				}
				.window_destroyed {
					if app.window_ids()!.len == 0 {
						app.stop()!
					}
				}
				else {}
			}
		}
	)!

	_ = main_window
}
```

Lifecycle-only applications can run with just `event_fn`; they do not require a
renderer. `frame_fn` and `draw_window()` require an already render-capable app;
they do not re-run `.auto` backend selection. Programs that plan to render
should use `gg.new_app(require_renderer: true)` or verify
`app.capabilities().explicit_swapchain` before rendering. Each `draw_window()`
call records and commits work for that window while its native surface is
current. Linux X11 rendering, including under Xvfb, needs both flags:

```sh
xvfb-run -a v -d gg_multiwindow -d x_multiwindow_x11 run examples/gg/multiwindow.v
```

`gg.App` manages native windows through `x.multiwindow`. The lower-level
`x.multiwindow` layer owns native lifetimes and the owner queue; `gg.App` owns
`sokol.gfx`/`sokol.sgl` renderer state only after rendering is initialized.
Create, run, stop and render from the owner thread. Background threads should
schedule owner-side work with `app.post()` or `app.try_post()` and let the run
loop drain it. A `gg.App` render owner cannot coexist with an active legacy
`gg.Context` renderer owner in the same process, but the legacy `gg.Context` API
remains available for normal single-window programs.

## Troubleshooting

A common problem, if you draw a lot of primitive elements in the same
frame, is that there is a chance that your program can exceed the maximum
allowed amount of vertices and commands, imposed by `sokol`.
The symptom is that your frame will be suddenly black, after it becomes more complex.
Sokol's default for vertices is 131072.
Sokol's default for commands is 32768.

To solve that, you can try adding these lines at the top of your program:
`#flag -D_SGL_DEFAULT_MAX_VERTICES=4194304`
`#flag -D_SGL_DEFAULT_MAX_COMMANDS=65536`
You can see an example of that in:
https://github.com/vlang/v/blob/master/examples/gg/many_thousands_of_circles_overriding_max_vertices.v

Another approach is to use several draw passes, and limit the amount
of draw calls that you make in each, demonstrated in:
https://github.com/vlang/v/blob/master/examples/gg/many_thousands_of_circles.v

Another approach to that problem, is to draw everything yourself in a streaming
texture, then upload that streaming texture as a single draw command to the GPU.
You can see an example of that done in:
https://github.com/vlang/v/blob/master/examples/gg/random.v
and in:
https://github.com/vlang/v/blob/master/examples/gg/random_stars.v

A third approach, is to only upload your changing inputs to the GPU, and do all
the calculations and drawing there in shaders.
