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
- an owner-thread job queue for cross-thread work;
- explicit swapchain handoff for `sokol.gfx` rendering.

It does not provide high-level input handling, layout, widgets, text rendering,
or a default event loop. The `gg` facade supplies the higher-level loop and
drawing API.

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
- `readback`: reserved for backends that expose readback support.

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

Events are explicit. Native events are not delivered to user code until the
owner thread calls:

- `poll_events()` to collect backend/native events into the App queue;
- `drain_events()` to retrieve and clear queued events.

Event kinds are:

- `.window_created`: emitted by `create_window()` with the actual initial size;
- `.window_destroyed`: emitted by `destroy_window()`, `stop()`, or accepted
  backend destroy notifications;
- `.window_close_requested`: emitted when the backend reports a close request;
- `.window_resized`: emitted after `resize_window()` or accepted backend resize
  notifications with the actual size.

Backend events for stale or already-destroyed window handles are filtered.

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
- The module has no high-level input, layout, or drawing abstraction.

## Validation

Useful checks while working on this module:

```sh
./v test vlib/x/multiwindow/multiwindow_test.v
./v -d gg_multiwindow test vlib/x/multiwindow
./v -d gg_multiwindow -d x_multiwindow_x11 test vlib/x/multiwindow
```
