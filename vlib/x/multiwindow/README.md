# x.multiwindow

`x.multiwindow` is the low-level multi-window layer used by the experimental
`gg` multi-window facade. It owns native window lifetimes, backend selection,
per-window events, owner-thread dispatch, and explicit render swapchains.

Most application code should use `gg` with `-d gg_multiwindow`. This module is
intended for the `gg` facade, backend work, and callers that need direct control
over native windows and rendering setup.

## Scope

The module provides:

- an `App` registry for multiple native windows;
- generation-checked `WindowId` handles;
- backend capability reporting and backend selection;
- lifecycle events routed to a specific window;
- backend-neutral input events routed to a specific window;
- an owner-thread job queue for cross-thread work;
- explicit swapchain handoff for `sokol.gfx` rendering.

It does not provide layout, widgets, text rendering, high-level input semantics,
or a default event loop. The `gg` facade supplies the higher-level loop, `gg.Event`
mapping, and drawing API.

## Creating an App

```v
import x.multiwindow

mut app := multiwindow.new_app(backend: .mock, queue_size: 128)!
defer {
	app.stop() or {}
}

win := app.create_window(title: 'Tool', width: 320, height: 200)!
info := app.window_info(win)!
println('${info.title}: ${info.width}x${info.height}')
```

`multiwindow.new_app()` uses the values in `Config`. The low-level default
backend is `.mock`; `.auto` must be requested explicitly. The `gg` facade has
its own configuration and defaults to `.auto`.

`Config.require_renderer: true` asks the selected backend to initialize its
renderer during `new_app()`. The render API requires that creation-time request
and `Capabilities.explicit_swapchain`; `x.multiwindow` does not lazily
initialize a renderer later.

## Backend Selection

`BackendKind` values are:

- `.mock`: deterministic in-process backend for tests and event-only code;
- `.x11`: Linux X11 backend, compiled only with `-d x_multiwindow_x11`;
- `.wayland`: Linux Wayland backend, compiled only with `-d sokol_wayland`;
- `.appkit`: macOS AppKit backend;
- `.win32`: Windows backend;
- `.auto`: resolve to a concrete backend.

The `.auto` policy is platform and environment dependent:

- Windows selects `.win32`.
- macOS selects `.appkit`.
- Linux with `require_renderer: true` prefers X11 only when compiled with
  `-d x_multiwindow_x11` and `DISPLAY` is set, then Wayland when compiled with
  `-d sokol_wayland` and `WAYLAND_DISPLAY` is set.
- Linux without `require_renderer` prefers Wayland when compiled with
  `-d sokol_wayland` and `WAYLAND_DISPLAY` is set, then X11 only when compiled
  with `-d x_multiwindow_x11` and `DISPLAY` is set.
- If no native backend is selected, `.auto` falls back to `.mock`.

Plain capability probes do not necessarily open a display or create a device.
Renderer capability probes and `new_app(require_renderer: true)` may fail if
the display server, graphics device, or platform API is unavailable.

## Backend Capabilities

`Capabilities` describes the selected backend contract:

- `multi_window`: backend can manage more than one window;
- `owner_queue`: the owner-thread queue is available;
- `explicit_swapchain`: render calls can return a `gfx.Swapchain`;
- `mock`, `native`, `x11`, `wayland`, `win32`: selected platform flags;
- `gl`, `metal`, `d3d11`: active renderer API flags;
- `input_events`, `mouse_events`, `keyboard_events`, `text_events`,
  `focus_events`, `drop_events`, `touch_events`: native input classes the
  backend can actually deliver;
- `cursor_shapes`: native hover cursor shape updates are supported via
  `set_window_cursor(id, shape)`. This is independent from native interactive
  move/resize support;
- `interactive_move_resize`: native user-driven move/resize can be requested
  when the running backend has the required handles and current user action;
- `native_decorations`: native/server-side decorations are effective for the
  running backend;
- `readback`: reserved for backends that expose readback support.

Plain capability probes do not necessarily connect to the display server, so
runtime optional globals can be unknown before startup and most of those probes
report implementation support. For Wayland, use `app.capabilities()` after
`new_app()` for the authoritative runtime state: `drop_events` requires a
`wl_data_device`, `touch_events` requires `wl_touch`, and interactive
move/resize requires a seat. Wayland cursor-shape reporting is stricter:
`cursor_shapes` is true only after a `wp_cursor_shape_device_v1` has been
created for the active `wl_pointer`. Wayland requests server-side decorations
through xdg-decoration when the protocol is available; the compositor's
`configure(mode)` decides the effective `server_side` or `client_side` mode. If
`server_side` is refused or xdg-decoration is unavailable, apps and examples may
draw a client-side fallback. Wayland cursor-shape feedback uses
`wp_cursor_shape_manager_v1` when the compositor exposes it and the seat has a
pointer; this keeps cursor theme selection compositor-side. `wl_cursor_theme`
client-side fallback is not implemented, so `app.capabilities()` reports
`cursor_shapes == false` on Wayland compositors that do not advertise
cursor-shape-v1.

Backend notes:

- Mock supports lifecycle, events, min-size clamping, and the owner queue, but
  it has no renderer.
- X11 is Linux-only and exists only in builds compiled with
  `-d x_multiwindow_x11`. It supports native lifecycle, title updates, X11 size
  hints, borderless/fullscreen hints, optional EGL/OpenGL rendering, and native
  size queries after create/resize. Programmatic resize is rejected for
  non-resizable windows.
- Wayland is Linux-only and exists only in builds compiled with
  `-d sokol_wayland`. It requires `wl_compositor` and `xdg_wm_base`, rejects
  `visible: false`, and currently rejects programmatic resize. Rendering uses
  Wayland EGL/OpenGL when initialized.
- AppKit is macOS-only. It must start on the main thread and uses Metal when
  rendering is required.
- Win32 is Windows-only and supports native lifecycle and min-size enforcement.
  D3D11 rendering requires a Windows build with `-d sokol_d3d11`; without that
  flag, lifecycle works but renderer/swapchain calls are unsupported. Renderer
  startup can still fail if D3D11 device or swapchain creation is unavailable,
  and DXGI occlusion during present is treated as a skipped frame.

## Window Lifecycle

`create_window()` creates the native/backend window and returns a
generation-checked `WindowId`. The stored `WindowInfo` uses the actual size
reported by the backend after clamping or native size queries, not just the
requested `WindowConfig`.

`destroy_window()` destroys one live window and emits a destroy event. Destroying
the last window does not stop the app. `stop()` destroys all remaining live
windows, marks the app stopped, stops the backend, and closes owner-queue
admission.

Window handles are generation checked. A handle for a destroyed slot becomes
stale if that slot is later reused.

## Owner-Thread Rule

The thread that calls `new_app()` is the App owner thread. Mutating operations,
event draining, registry enumeration, owner-queue draining, and rendering must
run on that thread. Calls from another thread fail with:

```text
multiwindow: operation requires the owner thread
```

Use `post()` or `try_post()` to enqueue short callbacks from other threads, then
call `drain_pending()` on the owner thread. `drain_pending()` runs at most the
requested number of jobs and rechecks app status between jobs; if a job stops the
app, later queued jobs are not run.

The simple read helpers `status()`, `capabilities()`, `window_exists()`, and
`window_status()` do not enforce the owner-thread check.

## Events

Events are explicit. Native lifecycle and input events are not delivered to user
code until the owner thread calls:

- `poll_events()` to collect backend/native events into the App queue;
- `drain_events()` to retrieve and clear queued lifecycle events;
- `drain_input_events()` to retrieve and clear queued input events without
  consuming lifecycle events;
- `drain_queued_events()` to retrieve lifecycle and input events together in the
  exact order accepted by `App`.

Lifecycle event kinds are:

- `.window_created`: emitted by `create_window()` with the actual initial size;
- `.window_destroyed`: emitted by `destroy_window()`, `stop()`, or accepted
  backend destroy notifications;
- `.window_close_requested`: emitted when the backend reports a close request;
- `.window_resized`: emitted after `resize_window()` or accepted backend resize
  notifications with the actual size.

Backend events for stale or already-destroyed window handles are filtered.

`InputEvent` is the low-level backend-neutral payload used by the `gg` facade to
rebuild `gg.Event` for a specific window. Input kinds include key down/up, char,
mouse down/up/move/scroll/enter/leave, resize, iconified/restored,
focus/unfocus, clipboard paste, file drop, and touch families. Backends report
which families are implemented through the capability booleans above;
unsupported input classes must remain false rather than being partially
emulated.

Native input events that do not already carry a frame counter are stamped by
`App.poll_events()`; all input events accepted in the same poll cycle share that
`frame_count`. Mock/test events can provide an explicit non-zero `frame_count`,
which is preserved.

Current native input support is intentionally capability-scoped:

- Mock can synthesize every input family for deterministic tests.
- Win32 routes mouse, keyboard, text/char, focus, resize, iconified/restored,
  clipboard paste signals, file drops via `WM_DROPFILES`, and `WM_TOUCH`
  down/move/up touch input. `WM_TOUCH` handles are read and closed in the
  window procedure; `WM_TOUCH` does not expose a cancelled state, so
  `touches_cancelled` is not emitted by the Win32 backend unless a future
  Pointer Input path owns `POINTER_FLAG_CANCELED`.
- AppKit routes mouse, keyboard, text/char, focus, resize, iconified/restored,
  clipboard paste signals, and file drops through `NSDraggingDestination` file
  URLs. It also routes AppKit `NSResponder` touch phases; positions come from
  `NSTouch.normalizedPosition` mapped into the current framebuffer, with
  `touches_cancelled` emitted only for `touchesCancelledWithEvent:`.
- X11 routes mouse, keyboard, text/char, focus, resize, iconified/restored, and
  clipboard paste signal input events. Text uses Xlib XIM/XIC with
  `Xutf8LookupString`; this covers committed UTF-8 text from the active input
  method without exposing Xlib objects through the public API.
- X11 receives file drops with XDND `text/uri-list` selection conversion. It
  decodes local `file://` URIs and queues `.files_dropped` with routed
  `dropped_files`.
- Wayland routes pointer, keyboard, text/char through xkb keymap/state, focus,
  clipboard paste signal, resize input events, touch when the seat exposes
  `wl_touch`, and file drops when `wl_data_device`/`wl_data_offer`
  `text/uri-list` is available.
  Data-offer payloads are received through a non-blocking fd and drained from
  the owner poll path; the backend only sends `wl_data_offer.finish` after a
  valid `copy` or `move` action has been received. Pending drops whose source
  never closes the transfer fd are rejected and cleaned up after a bounded
  number of owner poll cycles.
  Wayland text follows the existing `sapp` `xkb_state_key_get_utf8` model for
  key presses; full IME/composed text is not implemented. Wayland synthesizes
  key-repeat from compositor repeat_info/xkb; pointer frame batching is not
  synthesized yet; event callbacks are
  routed as the compositor delivers them.
- Native drop and touch input are false unless a backend explicitly reports the
  corresponding capability. Clipboard paste is an input signal; clipboard
  contents are not stored on `InputEvent`.

`QueuedEvent` is the ordered queue entry used when lifecycle and input events
must be consumed as a single stream. Its `kind` field tells whether the entry
contains a lifecycle `Event` or an `InputEvent`. Use `drain_queued_events()` when
relative ordering matters, for example input -> close-request -> input; use the
separate `drain_events()` and `drain_input_events()` helpers only when that
cross-family ordering is not needed.

## Rendering

Rendering is explicit and optional. The render-facing API is compiled when the
program uses the `gg` facade with `-d gg_multiwindow`, or when direct
`x.multiwindow` callers opt in with `-d x_multiwindow_render`. Plain lifecycle
and `.mock` imports remain dependency-light and do not pull `sokol.gfx`, X11,
EGL, or OpenGL by default.

Backends that have initialized a renderer set `Capabilities.explicit_swapchain`
and implement:

- `render_environment(window)` for the `gfx.setup()` environment;
- `begin_render(window)` to make the window current and return a `RenderFrame`;
- `end_render(frame)` to present the frame;
- `abort_render(frame)` for error paths before presentation.

The low-level module does not call `gfx.setup()`, does not build passes, and does
not draw. A direct caller is responsible for using `RenderFrame.swapchain` in a
`gfx.Pass`, committing the frame, and then calling `end_render()`. The `gg`
facade handles this for normal gg applications.

## Relationship With gg

`gg.App` is the user-facing multi-window facade and is enabled with
`-d gg_multiwindow`. It maps `gg` types to `x.multiwindow`, owns the
`sokol.gfx` and `sokol.sgl` setup, maintains per-window draw contexts, runs the
event/frame loop, and exposes `draw_window()` and `run()`.

The checked-in example is:

```sh
./v -d gg_multiwindow run examples/gg/multiwindow.v
```

For Linux X11 native rendering, including Xvfb runs, add the X11 backend flag:

```sh
xvfb-run -a ./v -d gg_multiwindow -d x_multiwindow_x11 run examples/gg/multiwindow.v
```

For Wayland, build with `-d sokol_wayland`. The default build can still fall
back to `.mock` when no enabled native backend is available. The example creates
two gg windows, handles lifecycle events, and tolerates backends that reject
programmatic resize.

## Limitations

- X11 support is compiled only with `-d x_multiwindow_x11`; without that flag,
  the X11 backend is unsupported and X11/EGL/OpenGL libraries are not linked by
  low-level lifecycle or `.mock` imports.
- Wayland support is compiled only with `-d sokol_wayland`; without that flag,
  the Wayland backend is unsupported and Wayland libraries are not linked.
- Wayland hidden window creation (`visible: false`) is rejected.
- Wayland programmatic resize is currently unsupported.
- X11 programmatic resize is rejected for non-resizable windows.
- Native app creation can still fail even when plain capabilities report that a
  backend is supported, for example when a display cannot be opened.
- The mock backend is not a renderer and cannot produce swapchains.
- The module has no layout, widget, text rendering, or drawing abstraction.

## Validation

Useful checks while working on this module:

```sh
./v test vlib/x/multiwindow/multiwindow_test.v
./v -d gg_multiwindow test vlib/x/multiwindow
./v -d gg_multiwindow -d x_multiwindow_x11 test vlib/x/multiwindow
```
