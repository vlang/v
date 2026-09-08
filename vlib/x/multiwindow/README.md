# x.multiwindow

`x.multiwindow` is the low-level multi-window layer used by the `gg`
multi-window facade. It owns native window lifetimes, backend selection,
per-window events, owner-thread dispatch, and optional render scheduling.

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
- an opt-in render scheduler and opaque transaction declarations.

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

`Config.app_id` supplies the native application identity. It is currently
marshalled to Wayland as the `xdg_toplevel` app id; an empty value uses
`v.x.multiwindow`. Other backends currently ignore it.

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
- `explicit_swapchain`: the backend can participate in managed explicit-target
  rendering; it does not expose a public `gfx.Swapchain`;
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
- `readback`: whether the backend exposes at least one readback path. X11
  exposes native window capture without a renderer. X11 and Wayland managed
  image readback require an active GL renderer; with that renderer, window
  capture is managed by `gg` from its owned framebuffer. Mock exposes its
  deterministic window path, while AppKit reports readback only with a ready
  Metal renderer. Confirm individual operations per window through the `gg`
  readback capability query.

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
cursor-shape-v1. Fractional framebuffer scaling is used only when both
fractional-scale-v1 and viewporter are present; otherwise the backend keeps the
integer `wl_output` scale path. Clipboard requests require a seat and data
device, clipboard writes additionally require a current input serial, and
portal-parent identifiers require xdg-foreign-v2.

Backend notes:

- Mock supports lifecycle, events, min-size clamping, and the owner queue, but
  it has no renderer.
- X11 is Linux-only and exists only in builds compiled with
  `-d x_multiwindow_x11`. It supports native lifecycle, title updates, X11 size
  hints, borderless/fullscreen hints, optional EGL/OpenGL rendering, and native
  size queries after create/resize. Programmatic resize is rejected for
  non-resizable windows.
- Wayland is Linux-only and exists only in builds compiled with
  `-d sokol_wayland`. It requires `wl_compositor` and `xdg_wm_base`, supports
  initially hidden windows through an explicit remap/configure cycle, and
  replays the window title, app id, owner relation, size constraints,
  decoration preference, and requested maximize/fullscreen state when a hidden
  toplevel is shown again. If the compositor or transport does not supply a
  fresh configure for that show request, the request fails and the window stays
  hidden and retryable. It currently rejects programmatic resize. Rendering uses
  Wayland EGL/OpenGL when initialized.
- AppKit is macOS-only. It must start on the main thread and uses Metal when
  rendering is required.
- Win32 is Windows-only and supports native lifecycle and min-size enforcement.
  D3D11 rendering requires a Windows build with `-d sokol_d3d11`; without that
  flag, lifecycle works but managed renderer calls are unsupported. Renderer
  startup can still fail if D3D11 device or swapchain creation is unavailable,
  and DXGI occlusion during present is treated as a skipped frame.

## Window Lifecycle

`create_window()` creates the native/backend window and returns a
generation-checked `WindowId`. The stored `WindowInfo` uses the actual size
reported by the backend after clamping or native size queries, not just the
requested `WindowConfig`.

A modal window must name a live owner from the same app. Ownerless modal
configurations are rejected before native creation, whether initially visible
or hidden. Destroying an owner destroys its complete owned-window tree in
child-first order, so no child outlives the native owner it references.

`destroy_window()` destroys one live window, or the child-first owner cascade
rooted at that window, and emits a destroy event for each window. Destroying the
last window does not stop the app. `stop()` destroys all remaining live windows,
marks the app stopped, stops the backend, and closes owner-queue admission.

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

Events are explicit. One canonical queue preserves acceptance order across four
families: lifecycle, input, service, and readback. Native events are not
delivered to user code until the owner thread calls `poll_events()`.

The owner thread can then call:

- `poll_events()` to collect backend/native events into the App queue;
- `drain_events()` for lifecycle events;
- `drain_input_events()` for input events;
- `drain_service_events()` for service events;
- `drain_readback_events()` for readback results;
- `drain_queued_events()` for all four families in their exact global order.

Each specialized drain consumes only the contiguous prefix of its own family.
If a different family is at the head of the queue it returns an empty slice and
leaves that event, and everything after it, untouched. This prevents a
specialized consumer from silently reordering the stream. Use
`drain_queued_events()` whenever cross-family order matters.

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
- X11 receives XDND `text/uri-list` file drops through inline or bounded ICCCM
  INCR transfers (1 MiB maximum). It refreshes the inactivity deadline only on
  transfer progress, never delivers a partial drop, and sends checked
  `XdndFinished` replies without turning a vanished source window into a fatal
  X error. Valid local `file://` URIs are queued as routed `.files_dropped`
  events with cloned `dropped_files`.
- Wayland routes pointer, keyboard, text/char through xkb keymap/state, focus,
  clipboard paste signal, resize input events, touch when the seat exposes
  `wl_touch`, and file drops when `wl_data_device`/`wl_data_offer`
  `text/uri-list` is available.
  Data-offer payloads are received through a non-blocking fd and drained from
  the owner poll path; the backend only sends `wl_data_offer.finish` after a
  valid `copy` or `move` action has been received. Pending drops whose source
  never closes the transfer fd are rejected and cleaned up after a bounded
  number of owner poll cycles. A payload exactly at the byte limit is accepted
  only after EOF confirms that no extra byte remains.
  Wayland text follows the existing `sapp` `xkb_state_key_get_utf8` model for
  key presses; full IME/composed text is not implemented. Wayland synthesizes
  key-repeat from compositor repeat_info/xkb; pointer frame batching is not
  synthesized yet; event callbacks are
  routed as the compositor delivers them.
- Native drop and touch input are false unless a backend explicitly reports the
  corresponding capability. Clipboard paste is an input signal; clipboard
  contents are not stored on `InputEvent`.

`QueuedEvent` is the ordered envelope. Its `kind` selects exactly one of
`lifecycle`, `input`, `service`, or `readback`, and `sequence` is the admitted
global delivery sequence.

## Window Services

Window services are capability-first. Query
`service_operation_capability(window, operation)` on the live app before each
optional operation; the running backend result is authoritative. `available`
means the operation can be attempted now, `conditional` means a compositor,
window configuration, or recent user action can still decide it, and
`unsupported` must be handled without calling the operation. The
`asynchronous` bit means the call is not synchronously authoritative; it does
not promise a later canonical-queue result. Check `state_observable` before
waiting for a state observation. Wayland minimize is asynchronous with
`state_observable == false`, so no resulting minimized-state observation is
guaranteed.

A prepared destroy ticket remains live and can still admit service work, but a
new owned window cannot name that closing window until the ticket is rolled
back. Once the ticket is sealed, state/capability queries and new service,
readback, and native-borrow admissions for that window fail as stale. Already
admitted work follows the terminal or cancellation flow, and queued terminals
remain deliverable.

Use `service_window_state()` for the latest observed mapping, visibility,
focus, minimized/maximized/fullscreen, mouse-lock, position, and monitor
membership state. Unknown fields are explicit. `service_monitor_ids()` returns
currently available generation-checked monitor ids, and
`service_monitor_info()` returns geometry, work area, scale, primary state, and
the observation sequence. A removed monitor can become unavailable and a later
replacement receives a new generation; monitor names are descriptive and are
not identities. A full backend observation can authoritatively report an empty
membership and clears older ids; partial state observations preserve the last
known membership. X11 refreshes monitor work areas when the root
`_NET_WORKAREA` or `_NET_CURRENT_DESKTOP` property changes and retains the last
complete snapshot when refresh fails. Win32 continues observing complete native
monitor snapshots while no managed windows exist and refreshes that snapshot
before the next first-window creation. Window membership observations expose
only ids from the currently available public monitor snapshot; a staged native
refresh becomes visible atomically with its monitor and metrics events.

Clipboard reads and writes return a `ServiceRequestId`. Completion is a
terminal `.clipboard` service event with `.ready`, `.cancelled`, or `.failed`.
Portal-parent export follows the same request-id-to-event flow, but a ready
result also owns a `ServicePortalLeaseId`. Keep that lease alive while the
identifier is used and release it explicitly with
`service_release_portal_parent()`. X11 identifiers start with `x11:`; Wayland
xdg-foreign-v2 identifiers start with `wayland:`. Treat the remainder as opaque.
If preparing a replacement Wayland clipboard source fails before selection
submission, the previously published clipboard value remains unchanged.
Each Wayland clipboard send already accepted by the compositor owns a bounded
snapshot of the offered text and can finish independently after selection
replacement or source cancellation.
Each active X11 clipboard conversion uses an isolated native requestor; late
inline, failure, or INCR replies from an earlier conversion cannot terminalize
the next request, and failure to start that next conversion is itself terminal.
Replies to external X11 requestors, including INCR chunks, use a checked
connection so an expired requestor fails that transfer without poisoning later
clipboard work.
For X11 INCR reads, the advertised length is a lower bound; actual growth is
accepted only within the per-request and aggregate clipboard byte limits.
Queued clipboard terminal payloads share a 16 MiB, 64-operation bound across
backends and remain charged until their service events are delivered or
discarded.
Destroying an owner processes its descendants child-first. For each destroyed
window, pending clipboard and portal requests are cancelled and portal leases
are invalidated during sealing, before teardown results can be delivered.
Service cancellations precede readback cancellation and the final lifecycle
event in the canonical queue; replay does not create another terminal.

Native window handles are not owned by callers. The `gg` facade exposes them
only through a synchronous callback-bounded borrow; the pointer or integer
handle must not be stored, returned, or used after that callback. Backend handle
shapes are HWND on Win32, NSWindow on AppKit, Display plus Window on X11, and
wl_display plus wl_surface on Wayland.

Readback is asynchronous and terminal. The low-level
`service_request_window_readback()` reads the supplied width and height from the
framebuffer origin; `service_request_window_readback_region()` accepts
non-negative coordinates and a positive rectangle fully contained in the
target. Ready results own top-left RGBA8 pixels with an explicit stride and
producing `submitted_frame`; cancellation or failure has no pixel payload.
Pending and queued readbacks share a 256 MiB, 64-operation bound. A producer
reserves its tight RGBA8 size before allocation or capture, and that storage
remains charged until the terminal event is delivered or discarded.
Window destruction and app stop cancel pending work. The user-facing `gg`
facade publishes readbacks through its run callback and the canonical queue
rather than a separate gg readback drain. Aggregate `Capabilities.readback` and
per-window capability queries report availability; each request still validates
identity, ownership, render-target/sample constraints, and rectangle bounds.

Runtime support differs by backend:

| Backend | Service summary |
| --- | --- |
| Mock | Deterministic state, monitors, clipboard, portal, and readback for tests; no native-window borrow. |
| X11 | Native state/monitors, clipboard, portal (`x11:`), borrow, and native window capture; focus is available only when the live server advertises EWMH `_NET_ACTIVE_WINDOW`, its request is asynchronous, and authoritative state comes from `FocusIn`/`FocusOut`. Position and supported window-manager minimize/maximize/fullscreen/restore requests are also asynchronous; root-coordinate observations triggered by `ConfigureNotify` and native WM-state property events are authoritative. Mouse-lock centers are refreshed after resize. Other EWMH, mouse-lock, and rendered image support also depend on live runtime support. |
| Wayland | Runtime-global-driven state/monitors, clipboard, portal (`wayland:`), borrow, and mouse lock; focus/raise/position are unsupported. Show fails hidden and retryable when no fresh compositor configure is available. Show/minimize/maximize/restore/fullscreen/mouse-lock are asynchronous, but minimize is not state-observable, so callers are not guaranteed a resulting minimized-state observation. Rendered readback requires the active gg GL path. |
| AppKit | Native state/monitors, borrow, clipboard, window operations, and titlebar appearance as reported by the live bridge; portal export is unsupported and readback requires active Metal. |
| Win32 | Native state/monitors (including zero-window observation), borrow, clipboard, and standard window operations; focus and mouse lock are conditional. Focus loss releases mouse lock transactionally, retaining an error and retrying without a false unlocked observation if native cleanup fails. Maximize depends on window configuration; fullscreen and restore become unsupported if the native fullscreen state is unknown. Portal/readback are currently unsupported. |

This table is orientation, not a substitute for the per-window runtime query.
Optional compositor protocols, EWMH atoms, renderer state, user-action tokens,
and window configuration can change an operation's effective support.

## Rendering

Rendering is optional. The render-facing source is selected by
`-d gg_multiwindow` or `-d x_multiwindow_render`; plain lifecycle and `.mock`
imports remain the no-flag isolation case.

The render contract does not expose `gfx.Environment`, `gfx.Swapchain`, native
drawables, command buffers, `RenderFrame`, or present authority. It provides
owner-thread opaque batch and target leases, backend-issued ready credits,
immutable metrics and target snapshots, late target acquisition, one global
commit, ordered finalization, and a private recovery anchor. Each acquired target
and its stored slot share a nonzero per-window lease epoch; zero, mismatched,
rotated, copied, or expired epochs are rejected.

Render-capable windows support exactly `sample_count: 1`; other sample counts
are rejected. For X11 and Wayland GL renderers, `gg` captures its currently
owned framebuffer before presentation and publishes it only after the producing
frame is submitted. X11 without a renderer retains native `XGetImage` window
capture, which observes the X server drawable but is not frame-exact compositor
capture under XWayland. Both backends also provide managed single-sample GL
image readback. These paths produce owned top-left RGBA8 data in the canonical
readback queue. Capability queries
remain authoritative because support is backend- and renderer-specific;
unsupported paths return `gg.multiwindow: requested readback is not supported`.
AppKit provides the same canonical asynchronous delivery when the active
renderer is Metal and its private pre-present hook is installed. Rendererless
and GLCore33 AppKit builds report both readback operations as unsupported.

The normative graphics references are the V-vendored Sokol API and matching
pinned upstream [sokol_gfx.h](https://raw.githubusercontent.com/floooh/sokol/c0e0563/sokol_gfx.h).
Backend lifetime and sequencing must also follow
[DXGI Present](https://learn.microsoft.com/en-us/windows/win32/api/dxgi/nf-dxgi-idxgiswapchain-present),
[DXGI ResizeBuffers](https://learn.microsoft.com/en-us/windows/win32/api/dxgi/nf-dxgi-idxgiswapchain-resizebuffers),
[D3D11 threading](https://learn.microsoft.com/en-us/windows/win32/direct3d11/overviews-direct3d-11-render-multi-thread-intro),
[CAMetalLayer](https://developer.apple.com/documentation/quartzcore/cametallayer),
[EGL make-current](https://registry.khronos.org/EGL/sdk/docs/man/html/eglMakeCurrent.xhtml),
and [EGL swap](https://registry.khronos.org/EGL/sdk/docs/man/html/eglSwapBuffers.xhtml)
contracts. Local precedent does not override those lifetime and threading rules.

## Relationship With gg

`gg.App` is the user-facing multi-window facade and is enabled with
`-d gg_multiwindow`. It maps `gg` types to `x.multiwindow` and declares the
managed `sokol.gfx`/`sokol.sgl` surface.

The main facade mapping is:

| `gg` | `x.multiwindow` |
| --- | --- |
| `gg.App`, `gg.WindowId` | `multiwindow.App`, `multiwindow.WindowId` |
| `gg.WindowEvent`, `gg.WindowInputEvent` | `multiwindow.Event`, `multiwindow.InputEvent` |
| `gg.WindowServiceEvent` | `multiwindow.ServiceEvent` |
| `gg.WindowReadbackResult` | `multiwindow.ServiceReadbackResult` |
| `gg.WindowQueuedEvent` | `multiwindow.QueuedEvent` |
| `window_state`, `monitor_ids`, `window_operation_capability` | `service_window_state`, `service_monitor_ids`, `service_operation_capability` |
| `drain_window_queued_events` | `drain_queued_events` |

Application code should stay on one side of this mapping. The facade converts
opaque ids and snapshots; it does not transfer ownership of low-level native or
render objects.

`examples/gg/multiwindow.v` is the interactive rendering example, and
`examples/gg/multiwindow_services.v` is the compact capability-first services,
queue, borrow, portal, and optional-readback example:

```sh
./v -d gg_multiwindow run examples/gg/multiwindow.v
./v -d gg_multiwindow run examples/gg/multiwindow_services.v
```

For Linux X11 native rendering, including Xvfb runs, add the X11 backend flag:

```sh
xvfb-run -a ./v -d gg_multiwindow -d x_multiwindow_x11 run examples/gg/multiwindow.v
```

For Wayland, build with `-d sokol_wayland`. The default build can still fall
back to `.mock` when no enabled native backend is available. The example creates
two gg windows, handles lifecycle events, and tolerates backends that reject
programmatic resize.

`examples/gg/multiwindow_render_runtime.v` is the unattended renderer probe used
by CI. The backend lanes compile it with `-d gg_multiwindow` and the matching
native flag (`-d x_multiwindow_x11`, `-d sokol_wayland`, `-d sokol_metal`, or
`-d sokol_d3d11`), select the backend with `V_MULTIWINDOW_PROBE_BACKEND`, and
launch it through the process-tree watchdog that owns its parent gate. The probe
emits
`{"example":"multiwindow_render_runtime","status":"PASS","cleanup":"complete"}`
only after renderer and window cleanup completes.

## Limitations

- X11 support is compiled only with `-d x_multiwindow_x11`; without that flag,
  the X11 backend is unsupported and X11/EGL/OpenGL libraries are not linked by
  low-level lifecycle or `.mock` imports. Enabled X11 builds link Xlib, XCB,
  and the same-client Xlib/XCB bridge; on Debian-family systems install
  `libx11-dev` and `libx11-xcb-dev` (which supply the XCB development
  dependency).
- Wayland support is compiled only with `-d sokol_wayland`; without that flag,
  the Wayland backend is unsupported and Wayland libraries are not linked.
- Wayland programmatic resize is currently unsupported.
- X11 programmatic resize is rejected for non-resizable windows.
- Native app creation can still fail even when plain capabilities report that a
  backend is supported, for example when a display cannot be opened.
- The mock backend is not a renderer and cannot produce render targets.
- Multi-window render targets support only `sample_count: 1`.
- X11 native window capture is available without a renderer through
  `XGetImage`, with the native XWayland presentation limitation described
  above. Managed window/image readback on X11 and Wayland requires an active GL
  renderer and is limited to the framebuffer owned by `gg`; it is not
  compositor or desktop capture. AppKit readback requires its active Metal
  renderer and private pre-present hook. Win32 readback remains unsupported in
  this tranche.
- The module has no layout, widget, text rendering, or drawing abstraction.

## Validation

The no-flag lifecycle/source check is:

```sh
./v test vlib/x/multiwindow/multiwindow_test.v
```

This command is a non-render isolation check; it does not establish native
renderer behavior. Renderer proofs run in the dedicated X11, Wayland, AppKit,
and Win32 CI lanes. Each lane sets `VGG_MULTIWINDOW_RUNTIME_PROBES=1`,
`VGG_MULTIWINDOW_RUNTIME_BACKEND`, and `V_MULTIWINDOW_PROBE_BACKEND`, compiles
with its native renderer flags, and executes each test and runtime probe through
the process-tree watchdog. The watchdog supplies the private parent gate,
enforces the deadline, reaps child processes, and checks the final cleanup JSON.

Real RandR or Wayland output removal/reconnection and external portal-parent
consumption by another toolkit or desktop portal require an interactive desktop
session and remain manual integration checks.
