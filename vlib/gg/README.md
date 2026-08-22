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
compiled only when the program is built with `-d gg_multiwindow`.
`examples/gg/multiwindow.v` is the interactive example:

```sh
v -d gg_multiwindow run examples/gg/multiwindow.v
```

The existing single-window `gg.Context` API and behavior are unchanged. A normal
`import gg` program that uses `gg.new_context()` does not load the native
multi-window implementation. Without `-d gg_multiwindow`, the opt-in API surface
is a non-render compatibility stub: accidental `gg.App` calls report a clear
"compile with `-d gg_multiwindow`" error instead of pulling in `x.multiwindow`
or native backend code.

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
`app.capabilities().explicit_swapchain` before rendering. Linux X11 rendering,
including under Xvfb, needs both flags:

```sh
xvfb-run -a v -d gg_multiwindow -d x_multiwindow_x11 run examples/gg/multiwindow.v
```

`AppConfig.app_id` supplies the native application identity (currently the
Wayland `xdg_toplevel` app id). A modal `WindowConfig` must name a live `owner`
from the same app; ownerless modal windows are rejected before native
allocation, whether hidden or visible. Destroying an owner destroys its complete
owned-window tree child-first; `stop()` uses the same child-before-owner order
for every remaining window. Destroying the final window does not stop the app
automatically, so event loops should call `app.stop()` explicitly.
For every destroyed descendant, pending clipboard and portal requests are
cancelled and portal leases are invalidated during sealing, before any teardown
result can be delivered. Their service cancellations precede readback
cancellation and the final `window_destroyed` event in the canonical queue;
teardown replay does not duplicate those terminals.
After a window generation is sealed, state/capability queries and new service,
readback, and native-borrow admissions fail as stale. Already admitted work
follows the terminal or cancellation flow, and queued terminals remain
deliverable.

`examples/gg/multiwindow_render_runtime.v` is an unattended CI probe, not an
interactive launch target. Backend lanes compile it with `-d gg_multiwindow`
plus `-d x_multiwindow_x11`, `-d sokol_wayland`, `-d sokol_metal`, or
`-d sokol_d3d11` as appropriate, and select the matching backend through
`V_MULTIWINDOW_PROBE_BACKEND`. A process-tree watchdog supplies the private
parent gate, enforces the deadline, and checks process cleanup. After all window
and renderer cleanup succeeds, the probe emits
`{"example":"multiwindow_render_runtime","status":"PASS","cleanup":"complete"}`.

### Multi-Window Events

`gg.App.run()` dispatches all four ordered event families: lifecycle through
`event_fn`, input through `input_fn`, native service results through
`window_service_fn`, and readback terminals through `readback_fn`. Lifecycle
events use `gg.WindowEvent` and cover created, resized, close-requested and
destroyed windows. Input events use `gg.WindowInputEvent`, which adds the target
`gg.WindowId` to the normal `gg.Event` payload so existing key, mouse, scroll,
focus and window-state event fields keep the same gg-facing types.
If any of the four queue callbacks returns an error, `run()` reinserts the
current event and every untouched suffix event in their original order. All
four handlers must therefore be idempotent, not only readback handlers.
For native multi-window events, `gg.Event.frame_count` is assigned by the
underlying multi-window owner poll cycle so events collected by the same
`app.poll_events()` call share a frame count.

`input_fn` has the `gg.AppInputFn` shape:

```v ignore
fn (event gg.WindowInputEvent, mut app gg.App) !
```

For manual owner loops, call `app.poll_events()` and then either
`app.drain_window_queued_events()` or a specialized drain. The canonical drain
returns lifecycle, input, service, and readback envelopes in exact global
acceptance order. `drain_events()`, `drain_input_events()`, and
`drain_window_service_events()` each consume only a contiguous prefix of their
own family; if another family is at the head, they return empty without skipping
it. There is deliberately no separate gg readback drain: consume readbacks via
`readback_fn` in `run()` or `.readback` entries from the canonical drain.

Input support is capability-driven. Check `app.capabilities()` before relying
on a class of native events: `input_events`, `mouse_events`, `keyboard_events`,
`text_events`, `focus_events`, `drop_events` and `touch_events` report what the
selected backend can actually deliver. `cursor_shapes` reports whether
`app.set_window_cursor(id, shape)` can update native hover cursor feedback, and
is independent from interactive move/resize support. `interactive_move_resize`
reports whether the runtime backend has the native handles needed for
`app.begin_window_move(id)` and `app.begin_window_resize(id, edge)`;
individual calls can still fail when the platform requires a recent user-action
serial. `native_decorations` reports whether native/server-side window
decorations are effective for the running backend. Plain capability probes do
not necessarily open a display, so runtime globals are authoritative only after
`gg.new_app()` via `app.capabilities()`; on Wayland that includes `wl_touch` for
touch, `wl_data_device` for drops, seats for interactive move/resize, and
xdg-decoration negotiation for native decorations. Wayland cursor-shape
reporting is stricter: `cursor_shapes` is true only after a
`wp_cursor_shape_device_v1` has been created for the active `wl_pointer`.
Wayland requests server-side decorations through xdg-decoration when available;
the compositor's `configure(mode)` decides the effective `server_side` or
`client_side` mode. If `server_side` is refused or xdg-decoration is
unavailable, apps and examples may draw a client-side fallback. Wayland cursor
shape feedback uses `wp_cursor_shape_manager_v1` when the compositor exposes it
and the seat has a pointer; cursor theme selection remains compositor-side.
`wl_cursor_theme` client-side fallback is not implemented, so
`app.capabilities()` reports `cursor_shapes == false` on Wayland compositors
that do not advertise cursor-shape-v1. Wayland uses fractional-scale-v1 only
when viewporter is also present; otherwise framebuffer metrics follow integer
`wl_output` scale.
Backends must leave unsupported
classes false instead of emulating partial support. Current native backends route
window-scoped mouse, keyboard, focus, resize and iconified/restored events where
the platform implementation supports them. `drop_events` is true on native
backends that clone dropped file paths into `WindowInputEvent.dropped_files`;
`touch_events` is true only where native touch input is wired. Win32 reports the
`WM_TOUCH` began/moved/ended states; AppKit also reports cancelled touches from
`touchesCancelledWithEvent:`. Clipboard paste is reported as an event signal;
clipboard contents are not carried by `WindowInputEvent`. X11 text uses
XIM/XIC with `Xutf8LookupString`. X11 file drops accept inline or bounded
1 MiB ICCCM INCR XDND `text/uri-list` transfers, refresh their timeout only on
progress, never publish a partial drop, and safely finish even if the source
window disappears.
Wayland text uses xkb keymap/state for key-press characters, and Wayland file
drops use `wl_data_device`/`wl_data_offer` `text/uri-list`. A drop exactly at
the byte limit is accepted only after EOF confirms that no extra byte remains;
neither Linux text path implements full IME/composed text yet.

### Window Services and Native Borrows

Services are capability-first. Query `window_operation_capability(window,
operation)` on the running app immediately before an optional call. The runtime
answer is authoritative: `.conditional` can still require compositor support,
window configuration, or a recent user action. `.asynchronous` means the call
is not synchronously authoritative; it does not promise a later queued result.
Check `.state_observable` before waiting for a state observation. Wayland
minimize is asynchronous with `state_observable == false`, so no resulting
minimized-state observation is guaranteed. Use `window_state()` for the latest
observed state and `monitor_ids()` plus `monitor_info()` for generation-checked
monitor snapshots. Full observations can authoritatively clear membership;
partial state observations preserve the last known ids. Monitor names are
descriptive, not stable identities. X11 root work-area/current-desktop changes
refresh the complete monitor projection while failed refreshes preserve the
last snapshot. Win32 keeps the app-level monitor projection current even while
the app has no managed windows and refreshes it before the next first window.
Window membership observations contain only ids in the currently available
public monitor snapshot; staged native monitor and metrics updates become
visible together.

Clipboard reads and writes return `ClipboardRequestId`; match it with the
terminal `.clipboard` `WindowServiceEvent`. Portal export returns
`PortalParentRequestId`; a ready event contains both an opaque identifier and a
`PortalParentLeaseId`. Keep the lease alive for the external consumer and call
`release_portal_parent()` explicitly afterward. Native X11 identifiers start
with `x11:` and Wayland xdg-foreign-v2 identifiers start with `wayland:`. Treat
everything after the prefix as opaque. Wayland clipboard uses the seat data
device, writes require a recent input serial, and portal export requires
xdg-foreign-v2. If preparing a replacement Wayland clipboard source fails
before selection submission, the previously published clipboard value remains
unchanged. A compositor clipboard send already accepted by Wayland uses its own
bounded text snapshot and can finish after replacement or source cancellation.
Each active X11 clipboard read has an isolated native conversion requestor, so
late inline, failure, or INCR replies cannot complete a later request; failure
to start a queued conversion is delivered as its terminal failure.
Replies to external X11 requestors, including INCR chunks, use a checked
connection so an expired requestor fails that transfer without affecting later
clipboard work.
For X11 INCR reads, the advertised length is a lower bound; actual growth is
accepted only within the per-request and aggregate clipboard byte limits.
Queued clipboard terminal payloads share a 16 MiB, 64-operation bound across
backends and remain charged until their service events are delivered or
discarded.

`with_native_window()` is callback-only. Inside its callback, invoke exactly
the accessor matching `app.capabilities().backend`:

- `with_win32`: HWND;
- `with_appkit`: NSWindow pointer;
- `with_x11`: Display pointer and X11 Window;
- `with_wayland`: wl_display and wl_surface pointers.

`NativeWindowLease` expires when the outer `with_native_window()` callback
returns. A backend handle expires sooner, when its nested `lease.with_*`
callback returns. Never store, return, or use either authority after its own
callback lifetime.

Window and managed-image readbacks are asynchronous. `WindowReadbackConfig{}`
captures the full target; `rect` requests a positive, fully contained region in
framebuffer coordinates. Every request admits and enqueues one terminal result
as `.ready`, `.cancelled`, or `.failed`, but a callback failure can replay that
same queued result until acknowledgment; handlers must be idempotent. Ready
results own top-left RGBA8 bytes, an explicit stride, dimensions, and the
producing `submitted_frame`; cancellation/failure does not carry pixels.
Pending and queued readbacks share a 256 MiB, 64-operation bound. A producer
reserves its tight RGBA8 size before allocation or capture, and that storage
remains charged until the terminal event is delivered or discarded.
Pending requests are cancelled during window/app teardown.
`app.capabilities().readback` is only the backend-wide availability summary for
the current renderer (Mock has its deterministic window path; AppKit requires a
ready Metal renderer). `window_readback_capabilities()` reports per-window path
availability. The request still validates app/window ownership, same-window
image scope, single-sample 2D render-target eligibility, and rectangle bounds.

The compact end-to-end example is:

```sh
v -d gg_multiwindow run examples/gg/multiwindow_services.v
```

It gates operations on runtime capabilities, queries state and monitors,
correlates clipboard/portal request ids, releases portal leases, uses a scoped
native borrow, and requests readback only when available.

Runtime support differs by backend:

| Backend | Service summary |
| --- | --- |
| Mock | Deterministic state, monitors, clipboard, portal, and readback for tests; native borrow is unsupported. |
| X11 | Native state/monitors, clipboard, portal (`x11:`), scoped borrow, and native window capture; focus is available only when the live server advertises EWMH `_NET_ACTIVE_WINDOW`, its request is asynchronous, and authoritative state comes from `FocusIn`/`FocusOut`. Position and supported window-manager minimize/maximize/fullscreen/restore requests are also asynchronous; root-coordinate observations triggered by `ConfigureNotify` and native WM-state property events are authoritative. Mouse-lock centers are refreshed after resize. Other EWMH, mouse-lock, and rendered-image support also depend on the live server/renderer. |
| Wayland | Runtime-global-driven state/monitors, clipboard, portal (`wayland:`), scoped borrow, and mouse lock; focus/raise/position are unsupported. Hide/show remapping preserves configured metadata, ownership, constraints, decorations, and maximize/fullscreen intent; show fails hidden and retryable when no fresh compositor configure is available. Show/minimize/maximize/restore/fullscreen/mouse-lock are asynchronous, but minimize is not state-observable, so callers are not guaranteed a resulting minimized-state observation. Rendered readback requires the active GL path. |
| AppKit | Native state/monitors, scoped borrow, clipboard, window operations, and titlebar appearance as reported by the live bridge; portal is unsupported and readback requires active Metal. |
| Win32 | Native state/monitors (including zero-window observation), scoped borrow, clipboard, and standard window operations; focus/mouse lock are conditional. Focus loss releases mouse lock transactionally, retaining an error and retrying without a false unlocked observation if cleanup fails. Maximize depends on window configuration; fullscreen and restore become unsupported if the native fullscreen state is unknown. Portal/readback are currently unsupported. |

This table is orientation only. Always prefer the live per-window capability
over backend-name assumptions. Relative mouse lock on Wayland, for example,
requires both relative-pointer and pointer-constraints globals.

The multi-window event queue is separate from legacy `gg.Context` callbacks.
Normal single-window applications keep using the existing `event_fn`,
`keydown_fn`, `move_fn`, `scroll_fn` and related callbacks on `gg.Context`, and
do not import or initialize `x.multiwindow`.

`gg.App` manages native windows through `x.multiwindow`. The lower-level
`x.multiwindow` layer owns native lifetimes and the owner queue; `gg.App` owns
`sokol.gfx`/`sokol.sgl` renderer state only after rendering is initialized.
Create, run, stop and render from the owner thread. Background threads should
schedule owner-side work with `app.post()` or `app.try_post()` and let the run
loop drain it. A `gg.App` render owner cannot coexist with an active legacy
`gg.Context` renderer owner in the same process, but the legacy `gg.Context` API
remains available for normal single-window programs.

The public facade maps `gg.App`/`WindowId`, `WindowEvent`, `WindowInputEvent`,
`WindowServiceEvent`, `WindowReadbackResult`, and `WindowQueuedEvent` to the
corresponding `x.multiwindow` app/id, lifecycle, input, service, readback, and
queued-envelope types. Likewise, `window_state`, `monitor_ids`,
`window_operation_capability`, and `drain_window_queued_events` map to the
lower-level `service_*` queries and `drain_queued_events`. Keep application code
on the gg side unless it deliberately needs the low-level backend layer; opaque
ids, leases, and native handles are not interchangeable across the facade.

### Per-Window Render API

With `-d gg_multiwindow`, `WindowConfig` provides per-window clear color,
redraw mode, sample count, and init/frame/cleanup callbacks. Callback contexts
provide immutable metrics and target snapshots, bounded frame/pass authority,
app- and window-scoped managed resource IDs, pass methods, and the recording
subset of `WindowSglContext`. `RunConfig` also provides app-resource lifecycle
callbacks for resources shared by multiple windows.

Multi-window render targets support exactly `sample_count: 1`. A different
sample count is rejected when a renderer is required or active. With an X11 or
Wayland GL renderer active, `request_window_capture()` reads the framebuffer
owned by `gg` after drawing and publishes it only after the producing frame is
submitted. `request_image_readback()` reads a managed single-sample 2D render
target. Without an active X11 renderer, window capture falls back to the native
`XGetImage` path; that path reflects the X server drawable and cannot guarantee
frame-exact compositor presentation under XWayland. Neither path is desktop or
compositor capture. Results are owned top-left RGBA8 values delivered through
`RunConfig.readback_fn` and support bounded pixel regions. Query
`window_readback_capabilities()` before either operation. AppKit exposes the
same asynchronous contract when its Metal renderer and private pre-present
hook are active; GLCore33 and rendererless AppKit builds report unsupported.
Win32 readback remains unsupported in this tranche.

Managed IDs are scoped to their app and, where applicable, their window. Stale,
foreign, or expired IDs and callback leases return errors instead of exposing
raw `gfx.Environment`, `gfx.Swapchain`, native drawable, command-buffer, or
present authority. Rendering uses owner-thread batches, backend-issued ready
credits, late target acquisition, ordered finalization, and one global commit
per submitted batch.

Renderer behavior is exercised in the dedicated X11, Wayland, AppKit, and Win32
CI lanes. Those lanes set `VGG_MULTIWINDOW_RUNTIME_PROBES=1`,
`VGG_MULTIWINDOW_RUNTIME_BACKEND`, and `V_MULTIWINDOW_PROBE_BACKEND`; compile
with the matching native flag; and run tests and probes through the process-tree
watchdog. The checks cover multi-window submission, resource cleanup and
replacement, stale leases, callback-driven teardown, recovery, and native fault
paths. A normal legacy `gg.Context` import remains isolated from
`x.multiwindow` and native multi-window backend dependencies. The implementation
follows the V-vendored Sokol revision and matching pinned
[sokol_gfx.h](https://raw.githubusercontent.com/floooh/sokol/c0e0563/sokol_gfx.h)
and [sokol_gl.h](https://raw.githubusercontent.com/floooh/sokol/c0e0563/util/sokol_gl.h)
contracts.

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
