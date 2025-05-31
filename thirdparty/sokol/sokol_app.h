#if defined(SOKOL_IMPL) && !defined(SOKOL_APP_IMPL)
#define SOKOL_APP_IMPL
#endif
#ifndef SOKOL_APP_INCLUDED

/*
    V language IMPORTANT NOTE: all the V patches in this code, are marked with:
    // __v_ start
    // __v_ end
*/

/*
    sokol_app.h -- cross-platform application wrapper

    Project URL: https://github.com/floooh/sokol

    Do this:
        #define SOKOL_IMPL or
        #define SOKOL_APP_IMPL
    before you include this file in *one* C or C++ file to create the
    implementation.

    In the same place define one of the following to select the 3D-API
    which should be initialized by sokol_app.h (this must also match
    the backend selected for sokol_gfx.h if both are used in the same
    project):

        #define SOKOL_GLCORE
        #define SOKOL_GLES3
        #define SOKOL_D3D11
        #define SOKOL_METAL
        #define SOKOL_WGPU
        #define SOKOL_NOAPI

    Optionally provide the following defines with your own implementations:

        SOKOL_ASSERT(c)     - your own assert macro (default: assert(c))
        SOKOL_UNREACHABLE() - a guard macro for unreachable code (default: assert(false))
        SOKOL_WIN32_FORCE_MAIN  - define this on Win32 to use a main() entry point instead of WinMain
        SOKOL_NO_ENTRY      - define this if sokol_app.h shouldn't "hijack" the main() function
        SOKOL_APP_API_DECL  - public function declaration prefix (default: extern)
        SOKOL_API_DECL      - same as SOKOL_APP_API_DECL
        SOKOL_API_IMPL      - public function implementation prefix (default: -)

    Optionally define the following to force debug checks and validations
    even in release mode:

        SOKOL_DEBUG         - by default this is defined if _DEBUG is defined

    If sokol_app.h is compiled as a DLL, define the following before
    including the declaration or implementation:

        SOKOL_DLL

    On Windows, SOKOL_DLL will define SOKOL_APP_API_DECL as __declspec(dllexport)
    or __declspec(dllimport) as needed.

    On Linux, SOKOL_GLCORE can use either GLX or EGL.
    GLX is default, set SOKOL_FORCE_EGL to override.

    For example code, see https://github.com/floooh/sokol-samples/tree/master/sapp

    Portions of the Windows and Linux GL initialization, event-, icon- etc... code
    have been taken from GLFW (http://www.glfw.org/).

    iOS onscreen keyboard support 'inspired' by libgdx.

    Link with the following system libraries:

    - on macOS with Metal: Cocoa, QuartzCore, Metal, MetalKit
    - on macOS with GL: Cocoa, QuartzCore, OpenGL
    - on iOS with Metal: Foundation, UIKit, Metal, MetalKit
    - on iOS with GL: Foundation, UIKit, OpenGLES, GLKit
    - on Linux with EGL: X11, Xi, Xcursor, EGL, GL (or GLESv2), dl, pthread, m(?)
    - on Linux with GLX: X11, Xi, Xcursor, GL, dl, pthread, m(?)
    - on Android: GLESv3, EGL, log, android
    - on Windows with the MSVC or Clang toolchains: no action needed, libs are defined in-source via pragma-comment-lib
    - on Windows with MINGW/MSYS2 gcc: compile with '-mwin32' so that _WIN32 is defined
        - link with the following libs: -lkernel32 -luser32 -lshell32
        - additionally with the GL backend: -lgdi32
        - additionally with the D3D11 backend: -ld3d11 -ldxgi

    On Linux, you also need to use the -pthread compiler and linker option, otherwise weird
    things will happen, see here for details: https://github.com/floooh/sokol/issues/376

    On macOS and iOS, the implementation must be compiled as Objective-C.

    FEATURE OVERVIEW
    ================
    sokol_app.h provides a minimalistic cross-platform API which
    implements the 'application-wrapper' parts of a 3D application:

    - a common application entry function
    - creates a window and 3D-API context/device with a 'default framebuffer'
    - makes the rendered frame visible
    - provides keyboard-, mouse- and low-level touch-events
    - platforms: MacOS, iOS, HTML5, Win32, Linux/RaspberryPi, Android
    - 3D-APIs: Metal, D3D11, GL4.1, GL4.3, GLES3, WebGL, WebGL2, NOAPI

    FEATURE/PLATFORM MATRIX
    =======================
                        | Windows | macOS | Linux |  iOS  | Android |  HTML5
    --------------------+---------+-------+-------+-------+---------+--------
    gl 3.x              | YES     | YES   | YES   | ---   | ---     |  ---
    gles3/webgl2        | ---     | ---   | YES(2)| YES   | YES     |  YES
    metal               | ---     | YES   | ---   | YES   | ---     |  ---
    d3d11               | YES     | ---   | ---   | ---   | ---     |  ---
    noapi               | YES     | TODO  | TODO  | ---   | TODO    |  ---
    KEY_DOWN            | YES     | YES   | YES   | SOME  | TODO    |  YES
    KEY_UP              | YES     | YES   | YES   | SOME  | TODO    |  YES
    CHAR                | YES     | YES   | YES   | YES   | TODO    |  YES
    MOUSE_DOWN          | YES     | YES   | YES   | ---   | ---     |  YES
    MOUSE_UP            | YES     | YES   | YES   | ---   | ---     |  YES
    MOUSE_SCROLL        | YES     | YES   | YES   | ---   | ---     |  YES
    MOUSE_MOVE          | YES     | YES   | YES   | ---   | ---     |  YES
    MOUSE_ENTER         | YES     | YES   | YES   | ---   | ---     |  YES
    MOUSE_LEAVE         | YES     | YES   | YES   | ---   | ---     |  YES
    TOUCHES_BEGAN       | ---     | ---   | ---   | YES   | YES     |  YES
    TOUCHES_MOVED       | ---     | ---   | ---   | YES   | YES     |  YES
    TOUCHES_ENDED       | ---     | ---   | ---   | YES   | YES     |  YES
    TOUCHES_CANCELLED   | ---     | ---   | ---   | YES   | YES     |  YES
    RESIZED             | YES     | YES   | YES   | YES   | YES     |  YES
    ICONIFIED           | YES     | YES   | YES   | ---   | ---     |  ---
    RESTORED            | YES     | YES   | YES   | ---   | ---     |  ---
    FOCUSED             | YES     | YES   | YES   | ---   | ---     |  YES
    UNFOCUSED           | YES     | YES   | YES   | ---   | ---     |  YES
    SUSPENDED           | ---     | ---   | ---   | YES   | YES     |  TODO
    RESUMED             | ---     | ---   | ---   | YES   | YES     |  TODO
    QUIT_REQUESTED      | YES     | YES   | YES   | ---   | ---     |  YES
    IME                 | TODO    | TODO? | TODO  | ???   | TODO    |  ???
    key repeat flag     | YES     | YES   | YES   | ---   | ---     |  YES
    windowed            | YES     | YES   | YES   | ---   | ---     |  YES
    fullscreen          | YES     | YES   | YES   | YES   | YES     |  ---
    mouse hide          | YES     | YES   | YES   | ---   | ---     |  YES
    mouse lock          | YES     | YES   | YES   | ---   | ---     |  YES
    set cursor type     | YES     | YES   | YES   | ---   | ---     |  YES
    screen keyboard     | ---     | ---   | ---   | YES   | TODO    |  YES
    swap interval       | YES     | YES   | YES   | YES   | TODO    |  YES
    high-dpi            | YES     | YES   | TODO  | YES   | YES     |  YES
    clipboard           | YES     | YES   | TODO  | ---   | ---     |  YES
    MSAA                | YES     | YES   | YES   | YES   | YES     |  YES
    drag'n'drop         | YES     | YES   | YES   | ---   | ---     |  YES
    window icon         | YES     | YES(1)| YES   | ---   | ---     |  YES

    (1) macOS has no regular window icons, instead the dock icon is changed
    (2) supported with EGL only (not GLX)

    STEP BY STEP
    ============
    --- Add a sokol_main() function to your code which returns a sapp_desc structure
        with initialization parameters and callback function pointers. This
        function is called very early, usually at the start of the
        platform's entry function (e.g. main or WinMain). You should do as
        little as possible here, since the rest of your code might be called
        from another thread (this depends on the platform):

            sapp_desc sokol_main(int argc, char* argv[]) {
                return (sapp_desc) {
                    .width = 640,
                    .height = 480,
                    .init_cb = my_init_func,
                    .frame_cb = my_frame_func,
                    .cleanup_cb = my_cleanup_func,
                    .event_cb = my_event_func,
                    ...
                };
            }

        To get any logging output in case of errors you need to provide a log
        callback. The easiest way is via sokol_log.h:

            #include "sokol_log.h"

            sapp_desc sokol_main(int argc, char* argv[]) {
                return (sapp_desc) {
                    ...
                    .logger.func = slog_func,
                };
            }

        There are many more setup parameters, but these are the most important.
        For a complete list search for the sapp_desc structure declaration
        below.

        DO NOT call any sokol-app function from inside sokol_main(), since
        sokol-app will not be initialized at this point.

        The .width and .height parameters are the preferred size of the 3D
        rendering canvas. The actual size may differ from this depending on
        platform and other circumstances. Also the canvas size may change at
        any time (for instance when the user resizes the application window,
        or rotates the mobile device). You can just keep .width and .height
        zero-initialized to open a default-sized window (what "default-size"
        exactly means is platform-specific, but usually it's a size that covers
        most of, but not all, of the display).

        All provided function callbacks will be called from the same thread,
        but this may be different from the thread where sokol_main() was called.

        .init_cb (void (*)(void))
            This function is called once after the application window,
            3D rendering context and swap chain have been created. The
            function takes no arguments and has no return value.
        .frame_cb (void (*)(void))
            This is the per-frame callback, which is usually called 60
            times per second. This is where your application would update
            most of its state and perform all rendering.
        .cleanup_cb (void (*)(void))
            The cleanup callback is called once right before the application
            quits.
        .event_cb (void (*)(const sapp_event* event))
            The event callback is mainly for input handling, but is also
            used to communicate other types of events to the application. Keep the
            event_cb struct member zero-initialized if your application doesn't require
            event handling.

        As you can see, those 'standard callbacks' don't have a user_data
        argument, so any data that needs to be preserved between callbacks
        must live in global variables. If keeping state in global variables
        is not an option, there's an alternative set of callbacks with
        an additional user_data pointer argument:

        .user_data (void*)
            The user-data argument for the callbacks below
        .init_userdata_cb (void (*)(void* user_data))
        .frame_userdata_cb (void (*)(void* user_data))
        .cleanup_userdata_cb (void (*)(void* user_data))
        .event_userdata_cb (void(*)(const sapp_event* event, void* user_data))

        The function sapp_userdata() can be used to query the user_data
        pointer provided in the sapp_desc struct.

        You can also call sapp_query_desc() to get a copy of the
        original sapp_desc structure.

        NOTE that there's also an alternative compile mode where sokol_app.h
        doesn't "hijack" the main() function. Search below for SOKOL_NO_ENTRY.

    --- Implement the initialization callback function (init_cb), this is called
        once after the rendering surface, 3D API and swap chain have been
        initialized by sokol_app. All sokol-app functions can be called
        from inside the initialization callback, the most useful functions
        at this point are:

        int sapp_width(void)
        int sapp_height(void)
            Returns the current width and height of the default framebuffer in pixels,
            this may change from one frame to the next, and it may be different
            from the initial size provided in the sapp_desc struct.

        float sapp_widthf(void)
        float sapp_heightf(void)
            These are alternatives to sapp_width() and sapp_height() which return
            the default framebuffer size as float values instead of integer. This
            may help to prevent casting back and forth between int and float
            in more strongly typed languages than C and C++.

        double sapp_frame_duration(void)
            Returns the frame duration in seconds averaged over a number of
            frames to smooth out any jittering spikes.

        int sapp_color_format(void)
        int sapp_depth_format(void)
            The color and depth-stencil pixelformats of the default framebuffer,
            as integer values which are compatible with sokol-gfx's
            sg_pixel_format enum (so that they can be plugged directly in places
            where sg_pixel_format is expected). Possible values are:

                23 == SG_PIXELFORMAT_RGBA8
                28 == SG_PIXELFORMAT_BGRA8
                42 == SG_PIXELFORMAT_DEPTH
                43 == SG_PIXELFORMAT_DEPTH_STENCIL

        int sapp_sample_count(void)
            Return the MSAA sample count of the default framebuffer.

        const void* sapp_metal_get_device(void)
        const void* sapp_metal_get_current_drawable(void)
        const void* sapp_metal_get_depth_stencil_texture(void)
        const void* sapp_metal_get_msaa_color_texture(void)
            If the Metal backend has been selected, these functions return pointers
            to various Metal API objects required for rendering, otherwise
            they return a null pointer. These void pointers are actually
            Objective-C ids converted with a (ARC) __bridge cast so that
            the ids can be tunnel through C code. Also note that the returned
            pointers to the renderpass-descriptor and drawable may change from one
            frame to the next, only the Metal device object is guaranteed to
            stay the same.

        const void* sapp_macos_get_window(void)
            On macOS, get the NSWindow object pointer, otherwise a null pointer.
            Before being used as Objective-C object, the void* must be converted
            back with a (ARC) __bridge cast.

        const void* sapp_ios_get_window(void)
            On iOS, get the UIWindow object pointer, otherwise a null pointer.
            Before being used as Objective-C object, the void* must be converted
            back with a (ARC) __bridge cast.

        const void* sapp_d3d11_get_device(void)
        const void* sapp_d3d11_get_device_context(void)
        const void* sapp_d3d11_get_render_view(void)
        const void* sapp_d3d11_get_resolve_view(void);
        const void* sapp_d3d11_get_depth_stencil_view(void)
            Similar to the sapp_metal_* functions, the sapp_d3d11_* functions
            return pointers to D3D11 API objects required for rendering,
            only if the D3D11 backend has been selected. Otherwise they
            return a null pointer. Note that the returned pointers to the
            render-target-view and depth-stencil-view may change from one
            frame to the next!

        const void* sapp_win32_get_hwnd(void)
            On Windows, get the window's HWND, otherwise a null pointer. The
            HWND has been cast to a void pointer in order to be tunneled
            through code which doesn't include Windows.h.

        const void* sapp_wgpu_get_device(void)
        const void* sapp_wgpu_get_render_view(void)
        const void* sapp_wgpu_get_resolve_view(void)
        const void* sapp_wgpu_get_depth_stencil_view(void)
            These are the WebGPU-specific functions to get the WebGPU
            objects and values required for rendering. If sokol_app.h
            is not compiled with SOKOL_WGPU, these functions return null.

        uint32_t sapp_gl_get_framebuffer(void)
            This returns the 'default framebuffer' of the GL context.
            Typically this will be zero.

        int sapp_gl_get_major_version(void)
        int sapp_gl_get_minor_version(void)
            Returns the major and minor version of the GL context
            (only for SOKOL_GLCORE, all other backends return zero here, including SOKOL_GLES3)

        const void* sapp_android_get_native_activity(void);
            On Android, get the native activity ANativeActivity pointer, otherwise
            a null pointer.

    --- Implement the frame-callback function, this function will be called
        on the same thread as the init callback, but might be on a different
        thread than the sokol_main() function. Note that the size of
        the rendering framebuffer might have changed since the frame callback
        was called last. Call the functions sapp_width() and sapp_height()
        each frame to get the current size.

    --- Optionally implement the event-callback to handle input events.
        sokol-app provides the following type of input events:
            - a 'virtual key' was pressed down or released
            - a single text character was entered (provided as UTF-32 code point)
            - a mouse button was pressed down or released (left, right, middle)
            - mouse-wheel or 2D scrolling events
            - the mouse was moved
            - the mouse has entered or left the application window boundaries
            - low-level, portable multi-touch events (began, moved, ended, cancelled)
            - the application window was resized, iconified or restored
            - the application was suspended or restored (on mobile platforms)
            - the user or application code has asked to quit the application
            - a string was pasted to the system clipboard
            - one or more files have been dropped onto the application window

        To explicitly 'consume' an event and prevent that the event is
        forwarded for further handling to the operating system, call
        sapp_consume_event() from inside the event handler (NOTE that
        this behaviour is currently only implemented for some HTML5
        events, support for other platforms and event types will
        be added as needed, please open a GitHub ticket and/or provide
        a PR if needed).

        NOTE: Do *not* call any 3D API rendering functions in the event
        callback function, since the 3D API context may not be active when the
        event callback is called (it may work on some platforms and 3D APIs,
        but not others, and the exact behaviour may change between
        sokol-app versions).

    --- Implement the cleanup-callback function, this is called once
        after the user quits the application (see the section
        "APPLICATION QUIT" for detailed information on quitting
        behaviour, and how to intercept a pending quit - for instance to show a
        "Really Quit?" dialog box). Note that the cleanup-callback isn't
        guaranteed to be called on the web and mobile platforms.

    MOUSE CURSOR TYPE AND VISIBILITY
    ================================
    You can show and hide the mouse cursor with

        void sapp_show_mouse(bool show)

    And to get the current shown status:

        bool sapp_mouse_shown(void)

    NOTE that hiding the mouse cursor is different and independent from
    the MOUSE/POINTER LOCK feature which will also hide the mouse pointer when
    active (MOUSE LOCK is described below).

    To change the mouse cursor to one of several predefined types, call
    the function:

        void sapp_set_mouse_cursor(sapp_mouse_cursor cursor)

    Setting the default mouse cursor SAPP_MOUSECURSOR_DEFAULT will restore
    the standard look.

    To get the currently active mouse cursor type, call:

        sapp_mouse_cursor sapp_get_mouse_cursor(void)

    MOUSE LOCK (AKA POINTER LOCK, AKA MOUSE CAPTURE)
    ================================================
    In normal mouse mode, no mouse movement events are reported when the
    mouse leaves the windows client area or hits the screen border (whether
    it's one or the other depends on the platform), and the mouse move events
    (SAPP_EVENTTYPE_MOUSE_MOVE) contain absolute mouse positions in
    framebuffer pixels in the sapp_event items mouse_x and mouse_y, and
    relative movement in framebuffer pixels in the sapp_event items mouse_dx
    and mouse_dy.

    To get continuous mouse movement (also when the mouse leaves the window
    client area or hits the screen border), activate mouse-lock mode
    by calling:

        sapp_lock_mouse(true)

    When mouse lock is activated, the mouse pointer is hidden, the
    reported absolute mouse position (sapp_event.mouse_x/y) appears
    frozen, and the relative mouse movement in sapp_event.mouse_dx/dy
    no longer has a direct relation to framebuffer pixels but instead
    uses "raw mouse input" (what "raw mouse input" exactly means also
    differs by platform).

    To deactivate mouse lock and return to normal mouse mode, call

        sapp_lock_mouse(false)

    And finally, to check if mouse lock is currently active, call

        if (sapp_mouse_locked()) { ... }

    On native platforms, the sapp_lock_mouse() and sapp_mouse_locked()
    functions work as expected (mouse lock is activated or deactivated
    immediately when sapp_lock_mouse() is called, and sapp_mouse_locked()
    also immediately returns the new state after sapp_lock_mouse()
    is called.

    On the web platform, sapp_lock_mouse() and sapp_mouse_locked() behave
    differently, as dictated by the limitations of the HTML5 Pointer Lock API:

        - sapp_lock_mouse(true) can be called at any time, but it will
          only take effect in a 'short-lived input event handler of a specific
          type', meaning when one of the following events happens:
            - SAPP_EVENTTYPE_MOUSE_DOWN
            - SAPP_EVENTTYPE_MOUSE_UP
            - SAPP_EVENTTYPE_MOUSE_SCROLL
            - SAPP_EVENTTYPE_KEY_UP
            - SAPP_EVENTTYPE_KEY_DOWN
        - The mouse lock/unlock action on the web platform is asynchronous,
          this means that sapp_mouse_locked() won't immediately return
          the new status after calling sapp_lock_mouse(), instead the
          reported status will only change when the pointer lock has actually
          been activated or deactivated in the browser.
        - On the web, mouse lock can be deactivated by the user at any time
          by pressing the Esc key. When this happens, sokol_app.h behaves
          the same as if sapp_lock_mouse(false) is called.

    For things like camera manipulation it's most straightforward to lock
    and unlock the mouse right from the sokol_app.h event handler, for
    instance the following code enters and leaves mouse lock when the
    left mouse button is pressed and released, and then uses the relative
    movement information to manipulate a camera (taken from the
    cgltf-sapp.c sample in the sokol-samples repository
    at https://github.com/floooh/sokol-samples):

        static void input(const sapp_event* ev) {
            switch (ev->type) {
                case SAPP_EVENTTYPE_MOUSE_DOWN:
                    if (ev->mouse_button == SAPP_MOUSEBUTTON_LEFT) {
                        sapp_lock_mouse(true);
                    }
                    break;

                case SAPP_EVENTTYPE_MOUSE_UP:
                    if (ev->mouse_button == SAPP_MOUSEBUTTON_LEFT) {
                        sapp_lock_mouse(false);
                    }
                    break;

                case SAPP_EVENTTYPE_MOUSE_MOVE:
                    if (sapp_mouse_locked()) {
                        cam_orbit(&state.camera, ev->mouse_dx * 0.25f, ev->mouse_dy * 0.25f);
                    }
                    break;

                default:
                    break;
            }
        }

    CLIPBOARD SUPPORT
    =================
    Applications can send and receive UTF-8 encoded text data from and to the
    system clipboard. By default, clipboard support is disabled and
    must be enabled at startup via the following sapp_desc struct
    members:

        sapp_desc.enable_clipboard  - set to true to enable clipboard support
        sapp_desc.clipboard_size    - size of the internal clipboard buffer in bytes

    Enabling the clipboard will dynamically allocate a clipboard buffer
    for UTF-8 encoded text data of the requested size in bytes, the default
    size is 8 KBytes. Strings that don't fit into the clipboard buffer
    (including the terminating zero) will be silently clipped, so it's
    important that you provide a big enough clipboard size for your
    use case.

    To send data to the clipboard, call sapp_set_clipboard_string() with
    a pointer to an UTF-8 encoded, null-terminated C-string.

    NOTE that on the HTML5 platform, sapp_set_clipboard_string() must be
    called from inside a 'short-lived event handler', and there are a few
    other HTML5-specific caveats to workaround. You'll basically have to
    tinker until it works in all browsers :/ (maybe the situation will
    improve when all browsers agree on and implement the new
    HTML5 navigator.clipboard API).

    To get data from the clipboard, check for the SAPP_EVENTTYPE_CLIPBOARD_PASTED
    event in your event handler function, and then call sapp_get_clipboard_string()
    to obtain the pasted UTF-8 encoded text.

    NOTE that behaviour of sapp_get_clipboard_string() is slightly different
    depending on platform:

        - on the HTML5 platform, the internal clipboard buffer will only be updated
          right before the SAPP_EVENTTYPE_CLIPBOARD_PASTED event is sent,
          and sapp_get_clipboard_string() will simply return the current content
          of the clipboard buffer
        - on 'native' platforms, the call to sapp_get_clipboard_string() will
          update the internal clipboard buffer with the most recent data
          from the system clipboard

    Portable code should check for the SAPP_EVENTTYPE_CLIPBOARD_PASTED event,
    and then call sapp_get_clipboard_string() right in the event handler.

    The SAPP_EVENTTYPE_CLIPBOARD_PASTED event will be generated by sokol-app
    as follows:

        - on macOS: when the Cmd+V key is pressed down
        - on HTML5: when the browser sends a 'paste' event to the global 'window' object
        - on all other platforms: when the Ctrl+V key is pressed down

    DRAG AND DROP SUPPORT
    =====================
    PLEASE NOTE: the drag'n'drop feature works differently on WASM/HTML5
    and on the native desktop platforms (Win32, Linux and macOS) because
    of security-related restrictions in the HTML5 drag'n'drop API. The
    WASM/HTML5 specifics are described at the end of this documentation
    section:

    Like clipboard support, drag'n'drop support must be explicitly enabled
    at startup in the sapp_desc struct.

        sapp_desc sokol_main(void) {
            return (sapp_desc) {
                .enable_dragndrop = true,   // default is false
                ...
            };
        }

    You can also adjust the maximum number of files that are accepted
    in a drop operation, and the maximum path length in bytes if needed:

        sapp_desc sokol_main(void) {
            return (sapp_desc) {
                .enable_dragndrop = true,               // default is false
                .max_dropped_files = 8,                 // default is 1
                .max_dropped_file_path_length = 8192,   // in bytes, default is 2048
                ...
            };
        }

    When drag'n'drop is enabled, the event callback will be invoked with an
    event of type SAPP_EVENTTYPE_FILES_DROPPED whenever the user drops files on
    the application window.

    After the SAPP_EVENTTYPE_FILES_DROPPED is received, you can query the
    number of dropped files, and their absolute paths by calling separate
    functions:

        void on_event(const sapp_event* ev) {
            if (ev->type == SAPP_EVENTTYPE_FILES_DROPPED) {

                // the mouse position where the drop happened
                float x = ev->mouse_x;
                float y = ev->mouse_y;

                // get the number of files and their paths like this:
                const int num_dropped_files = sapp_get_num_dropped_files();
                for (int i = 0; i < num_dropped_files; i++) {
                    const char* path = sapp_get_dropped_file_path(i);
                    ...
                }
            }
        }

    The returned file paths are UTF-8 encoded strings.

    You can call sapp_get_num_dropped_files() and sapp_get_dropped_file_path()
    anywhere, also outside the event handler callback, but be aware that the
    file path strings will be overwritten with the next drop operation.

    In any case, sapp_get_dropped_file_path() will never return a null pointer,
    instead an empty string "" will be returned if the drag'n'drop feature
    hasn't been enabled, the last drop-operation failed, or the file path index
    is out of range.

    Drag'n'drop caveats:

        - if more files are dropped in a single drop-action
          than sapp_desc.max_dropped_files, the additional
          files will be silently ignored
        - if any of the file paths is longer than
          sapp_desc.max_dropped_file_path_length (in number of bytes, after UTF-8
          encoding) the entire drop operation will be silently ignored (this
          needs some sort of error feedback in the future)
        - no mouse positions are reported while the drag is in
          process, this may change in the future

    Drag'n'drop on HTML5/WASM:

    The HTML5 drag'n'drop API doesn't return file paths, but instead
    black-box 'file objects' which must be used to load the content
    of dropped files. This is the reason why sokol_app.h adds two
    HTML5-specific functions to the drag'n'drop API:

        uint32_t sapp_html5_get_dropped_file_size(int index)
            Returns the size in bytes of a dropped file.

        void sapp_html5_fetch_dropped_file(const sapp_html5_fetch_request* request)
            Asynchronously loads the content of a dropped file into a
            provided memory buffer (which must be big enough to hold
            the file content)

    To start loading the first dropped file after an SAPP_EVENTTYPE_FILES_DROPPED
    event is received:

        sapp_html5_fetch_dropped_file(&(sapp_html5_fetch_request){
            .dropped_file_index = 0,
            .callback = fetch_cb
            .buffer = {
                .ptr = buf,
                .size = sizeof(buf)
            },
            .user_data = ...
        });

    Make sure that the memory pointed to by 'buf' stays valid until the
    callback function is called!

    As result of the asynchronous loading operation (no matter if succeeded or
    failed) the 'fetch_cb' function will be called:

        void fetch_cb(const sapp_html5_fetch_response* response) {
            // IMPORTANT: check if the loading operation actually succeeded:
            if (response->succeeded) {
                // the size of the loaded file:
                const size_t num_bytes = response->data.size;
                // and the pointer to the data (same as 'buf' in the fetch-call):
                const void* ptr = response->data.ptr;
            }
            else {
                // on error check the error code:
                switch (response->error_code) {
                    case SAPP_HTML5_FETCH_ERROR_BUFFER_TOO_SMALL:
                        ...
                        break;
                    case SAPP_HTML5_FETCH_ERROR_OTHER:
                        ...
                        break;
                }
            }
        }

    Check the droptest-sapp example for a real-world example which works
    both on native platforms and the web:

    https://github.com/floooh/sokol-samples/blob/master/sapp/droptest-sapp.c

    HIGH-DPI RENDERING
    ==================
    You can set the sapp_desc.high_dpi flag during initialization to request
    a full-resolution framebuffer on HighDPI displays. The default behaviour
    is sapp_desc.high_dpi=false, this means that the application will
    render to a lower-resolution framebuffer on HighDPI displays and the
    rendered content will be upscaled by the window system composer.

    In a HighDPI scenario, you still request the same window size during
    sokol_main(), but the framebuffer sizes returned by sapp_width()
    and sapp_height() will be scaled up according to the DPI scaling
    ratio.

    Note that on some platforms the DPI scaling factor may change at any
    time (for instance when a window is moved from a high-dpi display
    to a low-dpi display).

    To query the current DPI scaling factor, call the function:

    float sapp_dpi_scale(void);

    For instance on a Retina Mac, returning the following sapp_desc
    struct from sokol_main():

    sapp_desc sokol_main(void) {
        return (sapp_desc) {
            .width = 640,
            .height = 480,
            .high_dpi = true,
            ...
        };
    }

    ...the functions the functions sapp_width(), sapp_height()
    and sapp_dpi_scale() will return the following values:

    sapp_width:     1280
    sapp_height:    960
    sapp_dpi_scale: 2.0

    If the high_dpi flag is false, or you're not running on a Retina display,
    the values would be:

    sapp_width:     640
    sapp_height:    480
    sapp_dpi_scale: 1.0

    If the window is moved from the Retina display to a low-dpi external display,
    the values would change as follows:

    sapp_width:     1280 => 640
    sapp_height:    960  => 480
    sapp_dpi_scale: 2.0  => 1.0

    Currently there is no event associated with a DPI change, but an
    SAPP_EVENTTYPE_RESIZED will be sent as a side effect of the
    framebuffer size changing.

    Per-monitor DPI is currently supported on macOS and Windows.

    APPLICATION QUIT
    ================
    Without special quit handling, a sokol_app.h application will quit
    'gracefully' when the user clicks the window close-button unless a
    platform's application model prevents this (e.g. on web or mobile).
    'Graceful exit' means that the application-provided cleanup callback will
    be called before the application quits.

    On native desktop platforms sokol_app.h provides more control over the
    application-quit-process. It's possible to initiate a 'programmatic quit'
    from the application code, and a quit initiated by the application user can
    be intercepted (for instance to show a custom dialog box).

    This 'programmatic quit protocol' is implemented through 3 functions
    and 1 event:

        - sapp_quit(): This function simply quits the application without
          giving the user a chance to intervene. Usually this might
          be called when the user clicks the 'Ok' button in a 'Really Quit?'
          dialog box
        - sapp_request_quit(): Calling sapp_request_quit() will send the
          event SAPP_EVENTTYPE_QUIT_REQUESTED to the applications event handler
          callback, giving the user code a chance to intervene and cancel the
          pending quit process (for instance to show a 'Really Quit?' dialog
          box). If the event handler callback does nothing, the application
          will be quit as usual. To prevent this, call the function
          sapp_cancel_quit() from inside the event handler.
        - sapp_cancel_quit(): Cancels a pending quit request, either initiated
          by the user clicking the window close button, or programmatically
          by calling sapp_request_quit(). The only place where calling this
          function makes sense is from inside the event handler callback when
          the SAPP_EVENTTYPE_QUIT_REQUESTED event has been received.
        - SAPP_EVENTTYPE_QUIT_REQUESTED: this event is sent when the user
          clicks the window's close button or application code calls the
          sapp_request_quit() function. The event handler callback code can handle
          this event by calling sapp_cancel_quit() to cancel the quit.
          If the event is ignored, the application will quit as usual.

    On the web platform, the quit behaviour differs from native platforms,
    because of web-specific restrictions:

    A `programmatic quit` initiated by calling sapp_quit() or
    sapp_request_quit() will work as described above: the cleanup callback is
    called, platform-specific cleanup is performed (on the web
    this means that JS event handlers are unregistered), and then
    the request-animation-loop will be exited. However that's all. The
    web page itself will continue to exist (e.g. it's not possible to
    programmatically close the browser tab).

    On the web it's also not possible to run custom code when the user
    closes a browser tab, so it's not possible to prevent this with a
    fancy custom dialog box.

    Instead the standard "Leave Site?" dialog box can be activated (or
    deactivated) with the following function:

        sapp_html5_ask_leave_site(bool ask);

    The initial state of the associated internal flag can be provided
    at startup via sapp_desc.html5_ask_leave_site.

    This feature should only be used sparingly in critical situations - for
    instance when the user would loose data - since popping up modal dialog
    boxes is considered quite rude in the web world. Note that there's no way
    to customize the content of this dialog box or run any code as a result
    of the user's decision. Also note that the user must have interacted with
    the site before the dialog box will appear. These are all security measures
    to prevent fishing.

    The Dear ImGui HighDPI sample contains example code of how to
    implement a 'Really Quit?' dialog box with Dear ImGui (native desktop
    platforms only), and for showing the hardwired "Leave Site?" dialog box
    when running on the web platform:

        https://floooh.github.io/sokol-html5/wasm/imgui-highdpi-sapp.html

    FULLSCREEN
    ==========
    If the sapp_desc.fullscreen flag is true, sokol-app will try to create
    a fullscreen window on platforms with a 'proper' window system
    (mobile devices will always use fullscreen). The implementation details
    depend on the target platform, in general sokol-app will use a
    'soft approach' which doesn't interfere too much with the platform's
    window system (for instance borderless fullscreen window instead of
    a 'real' fullscreen mode). Such details might change over time
    as sokol-app is adapted for different needs.

    The most important effect of fullscreen mode to keep in mind is that
    the requested canvas width and height will be ignored for the initial
    window size, calling sapp_width() and sapp_height() will instead return
    the resolution of the fullscreen canvas (however the provided size
    might still be used for the non-fullscreen window, in case the user can
    switch back from fullscreen- to windowed-mode).

    To toggle fullscreen mode programmatically, call sapp_toggle_fullscreen().

    To check if the application window is currently in fullscreen mode,
    call sapp_is_fullscreen().

    WINDOW ICON SUPPORT
    ===================
    Some sokol_app.h backends allow to change the window icon programmatically:

        - on Win32: the small icon in the window's title bar, and the
          bigger icon in the task bar
        - on Linux: highly dependent on the used window manager, but usually
          the window's title bar icon and/or the task bar icon
        - on HTML5: the favicon shown in the page's browser tab

    NOTE that it is not possible to set the actual application icon which is
    displayed by the operating system on the desktop or 'home screen'. Those
    icons must be provided 'traditionally' through operating-system-specific
    resources which are associated with the application (sokol_app.h might
    later support setting the window icon from platform specific resource data
    though).

    There are two ways to set the window icon:

        - at application start in the sokol_main() function by initializing
          the sapp_desc.icon nested struct
        - or later by calling the function sapp_set_icon()

    As a convenient shortcut, sokol_app.h comes with a builtin default-icon
    (a rainbow-colored 'S', which at least looks a bit better than the Windows
    default icon for applications), which can be activated like this:

    At startup in sokol_main():

        sapp_desc sokol_main(...) {
            return (sapp_desc){
                ...
                icon.sokol_default = true
            };
        }

    Or later by calling:

        sapp_set_icon(&(sapp_icon_desc){ .sokol_default = true });

    NOTE that a completely zero-initialized sapp_icon_desc struct will not
    update the window icon in any way. This is an 'escape hatch' so that you
    can handle the window icon update yourself (or if you do this already,
    sokol_app.h won't get in your way, in this case just leave the
    sapp_desc.icon struct zero-initialized).

    Providing your own icon images works exactly like in GLFW (down to the
    data format):

    You provide one or more 'candidate images' in different sizes, and the
    sokol_app.h platform backends pick the best match for the specific backend
    and icon type.

    For each candidate image, you need to provide:

        - the width in pixels
        - the height in pixels
        - and the actual pixel data in RGBA8 pixel format (e.g. 0xFFCC8844
          on a little-endian CPU means: alpha=0xFF, blue=0xCC, green=0x88, red=0x44)

    For instance, if you have 3 candidate images (small, medium, big) of
    sizes 16x16, 32x32 and 64x64 the corresponding sapp_icon_desc struct is setup
    like this:

        // the actual pixel data (RGBA8, origin top-left)
        const uint32_t small[16][16]  = { ... };
        const uint32_t medium[32][32] = { ... };
        const uint32_t big[64][64]    = { ... };

        const sapp_icon_desc icon_desc = {
            .images = {
                { .width = 16, .height = 16, .pixels = SAPP_RANGE(small) },
                { .width = 32, .height = 32, .pixels = SAPP_RANGE(medium) },
                // ...or without the SAPP_RANGE helper macro:
                { .width = 64, .height = 64, .pixels = { .ptr=big, .size=sizeof(big) } }
            }
        };

    An sapp_icon_desc struct initialized like this can then either be applied
    at application start in sokol_main:

        sapp_desc sokol_main(...) {
            return (sapp_desc){
                ...
                icon = icon_desc
            };
        }

    ...or later by calling sapp_set_icon():

        sapp_set_icon(&icon_desc);

    Some window icon caveats:

        - once the window icon has been updated, there's no way to go back to
          the platform's default icon, this is because some platforms (Linux
          and HTML5) don't switch the icon visual back to the default even if
          the custom icon is deleted or removed
        - on HTML5, if the sokol_app.h icon doesn't show up in the browser
          tab, check that there's no traditional favicon 'link' element
          is defined in the page's index.html, sokol_app.h will only
          append a new favicon link element, but not delete any manually
          defined favicon in the page

    For an example and test of the window icon feature, check out the
    'icon-sapp' sample on the sokol-samples git repository.

    ONSCREEN KEYBOARD
    =================
    On some platforms which don't provide a physical keyboard, sokol-app
    can display the platform's integrated onscreen keyboard for text
    input. To request that the onscreen keyboard is shown, call

        sapp_show_keyboard(true);

    Likewise, to hide the keyboard call:

        sapp_show_keyboard(false);

    Note that onscreen keyboard functionality is no longer supported
    on the browser platform (the previous hacks and workarounds to make browser
    keyboards work for on web applications that don't use HTML UIs
    never really worked across browsers).

    INPUT EVENT BUBBLING ON THE WEB PLATFORM
    ========================================
    By default, input event bubbling on the web platform is configured in
    a way that makes the most sense for 'full-canvas' apps that cover the
    entire browser client window area:

    - mouse, touch and wheel events do not bubble up, this prevents various
      ugly side events, like:
        - HTML text overlays being selected on double- or triple-click into
          the canvas
        - 'scroll bumping' even when the canvas covers the entire client area
    - key_up/down events for 'character keys' *do* bubble up (otherwise
      the browser will not generate UNICODE character events)
    - all other key events *do not* bubble up by default (this prevents side effects
      like F1 opening help, or F7 starting 'caret browsing')
    - character events do no bubble up (although I haven't noticed any side effects
      otherwise)

    Event bubbling can be enabled for input event categories during initialization
    in the sapp_desc struct:

        sapp_desc sokol_main(int argc, char* argv[]) {
            return (sapp_desc){
                //...
                .html5_bubble_mouse_events = true,
                .html5_bubble_touch_events = true,
                .html5_bubble_wheel_events = true,
                .html5_bubble_key_events = true,
                .html5_bubble_char_events = true,
            };
        }

    This basically opens the floodgates lets *all* input events bubble up to the browser.
    To prevent individual events from bubbling, call sapp_consume_event() from within
    the sokol_app.h event callback.

    OPTIONAL: DON'T HIJACK main() (#define SOKOL_NO_ENTRY)
    ======================================================
    NOTE: SOKOL_NO_ENTRY and sapp_run() is currently not supported on Android.

    In its default configuration, sokol_app.h "hijacks" the platform's
    standard main() function. This was done because different platforms
    have different entry point conventions which are not compatible with
    C's main() (for instance WinMain on Windows has completely different
    arguments). However, this "main hijacking" posed a problem for
    usage scenarios like integrating sokol_app.h with other languages than
    C or C++, so an alternative SOKOL_NO_ENTRY mode has been added
    in which the user code provides the platform's main function:

    - define SOKOL_NO_ENTRY before including the sokol_app.h implementation
    - do *not* provide a sokol_main() function
    - instead provide the standard main() function of the platform
    - from the main function, call the function ```sapp_run()``` which
      takes a pointer to an ```sapp_desc``` structure.
    - from here on```sapp_run()``` takes over control and calls the provided
      init-, frame-, event- and cleanup-callbacks just like in the default model.

    sapp_run() behaves differently across platforms:

        - on some platforms, sapp_run() will return when the application quits
        - on other platforms, sapp_run() will never return, even when the
          application quits (the operating system is free to simply terminate
          the application at any time)
        - on Emscripten specifically, sapp_run() will return immediately while
          the frame callback keeps being called

    This different behaviour of sapp_run() essentially means that there shouldn't
    be any code *after* sapp_run(), because that may either never be called, or in
    case of Emscripten will be called at an unexpected time (at application start).

    An application also should not depend on the cleanup-callback being called
    when cross-platform compatibility is required.

    Since sapp_run() returns immediately on Emscripten you shouldn't activate
    the 'EXIT_RUNTIME' linker option (this is disabled by default when compiling
    for the browser target), since the C/C++ exit runtime would be called immediately at
    application start, causing any global objects to be destroyed and global
    variables to be zeroed.

    WINDOWS CONSOLE OUTPUT
    ======================
    On Windows, regular windowed applications don't show any stdout/stderr text
    output, which can be a bit of a hassle for printf() debugging or generally
    logging text to the console. Also, console output by default uses a local
    codepage setting and thus international UTF-8 encoded text is printed
    as garbage.

    To help with these issues, sokol_app.h can be configured at startup
    via the following Windows-specific sapp_desc flags:

        sapp_desc.win32_console_utf8 (default: false)
            When set to true, the output console codepage will be switched
            to UTF-8 (and restored to the original codepage on exit)

        sapp_desc.win32_console_attach (default: false)
            When set to true, stdout and stderr will be attached to the
            console of the parent process (if the parent process actually
            has a console). This means that if the application was started
            in a command line window, stdout and stderr output will be printed
            to the terminal, just like a regular command line program. But if
            the application is started via double-click, it will behave like
            a regular UI application, and stdout/stderr will not be visible.

        sapp_desc.win32_console_create (default: false)
            When set to true, a new console window will be created and
            stdout/stderr will be redirected to that console window. It
            doesn't matter if the application is started from the command
            line or via double-click.

    MEMORY ALLOCATION OVERRIDE
    ==========================
    You can override the memory allocation functions at initialization time
    like this:

        void* my_alloc(size_t size, void* user_data) {
            return malloc(size);
        }

        void my_free(void* ptr, void* user_data) {
            free(ptr);
        }

        sapp_desc sokol_main(int argc, char* argv[]) {
            return (sapp_desc){
                // ...
                .allocator = {
                    .alloc_fn = my_alloc,
                    .free_fn = my_free,
                    .user_data = ...,
                }
            };
        }

    If no overrides are provided, malloc and free will be used.

    This only affects memory allocation calls done by sokol_app.h
    itself though, not any allocations in OS libraries.


    ERROR REPORTING AND LOGGING
    ===========================
    To get any logging information at all you need to provide a logging callback in the setup call
    the easiest way is to use sokol_log.h:

        #include "sokol_log.h"

        sapp_desc sokol_main(int argc, char* argv[]) {
            return (sapp_desc) {
                ...
                .logger.func = slog_func,
            };
        }

    To override logging with your own callback, first write a logging function like this:

        void my_log(const char* tag,                // e.g. 'sapp'
                    uint32_t log_level,             // 0=panic, 1=error, 2=warn, 3=info
                    uint32_t log_item_id,           // SAPP_LOGITEM_*
                    const char* message_or_null,    // a message string, may be nullptr in release mode
                    uint32_t line_nr,               // line number in sokol_app.h
                    const char* filename_or_null,   // source filename, may be nullptr in release mode
                    void* user_data)
        {
            ...
        }

    ...and then setup sokol-app like this:

        sapp_desc sokol_main(int argc, char* argv[]) {
            return (sapp_desc) {
                ...
                .logger = {
                    .func = my_log,
                    .user_data = my_user_data,
                }
            };
        }

    The provided logging function must be reentrant (e.g. be callable from
    different threads).

    If you don't want to provide your own custom logger it is highly recommended to use
    the standard logger in sokol_log.h instead, otherwise you won't see any warnings or
    errors.


    TEMP NOTE DUMP
    ==============
    - sapp_desc needs a bool whether to initialize depth-stencil surface
    - the Android implementation calls cleanup_cb() and destroys the egl context in onDestroy
      at the latest but should do it earlier, in onStop, as an app is "killable" after onStop
      on Android Honeycomb and later (it can't be done at the moment as the app may be started
      again after onStop and the sokol lifecycle does not yet handle context teardown/bringup)


    LICENSE
    =======
    zlib/libpng license

    Copyright (c) 2018 Andre Weissflog

    This software is provided 'as-is', without any express or implied warranty.
    In no event will the authors be held liable for any damages arising from the
    use of this software.

    Permission is granted to anyone to use this software for any purpose,
    including commercial applications, and to alter it and redistribute it
    freely, subject to the following restrictions:

        1. The origin of this software must not be misrepresented; you must not
        claim that you wrote the original software. If you use this software in a
        product, an acknowledgment in the product documentation would be
        appreciated but is not required.

        2. Altered source versions must be plainly marked as such, and must not
        be misrepresented as being the original software.

        3. This notice may not be removed or altered from any source
        distribution.
*/
#define SOKOL_APP_INCLUDED (1)
#include <stddef.h> // size_t
#include <stdint.h>
#include <stdbool.h>

#if defined(SOKOL_API_DECL) && !defined(SOKOL_APP_API_DECL)
#define SOKOL_APP_API_DECL SOKOL_API_DECL
#endif
#ifndef SOKOL_APP_API_DECL
#if defined(_WIN32) && defined(SOKOL_DLL) && defined(SOKOL_APP_IMPL)
#define SOKOL_APP_API_DECL __declspec(dllexport)
#elif defined(_WIN32) && defined(SOKOL_DLL)
#define SOKOL_APP_API_DECL __declspec(dllimport)
#else
#define SOKOL_APP_API_DECL extern
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* misc constants */
enum {
    SAPP_MAX_TOUCHPOINTS = 8,
    SAPP_MAX_MOUSEBUTTONS = 3,
    SAPP_MAX_KEYCODES = 512,
    SAPP_MAX_ICONIMAGES = 8,
};

/*
    sapp_event_type

    The type of event that's passed to the event handler callback
    in the sapp_event.type field. These are not just "traditional"
    input events, but also notify the application about state changes
    or other user-invoked actions.
*/
typedef enum sapp_event_type {
    SAPP_EVENTTYPE_INVALID,
    SAPP_EVENTTYPE_KEY_DOWN,
    SAPP_EVENTTYPE_KEY_UP,
    SAPP_EVENTTYPE_CHAR,
    SAPP_EVENTTYPE_MOUSE_DOWN,
    SAPP_EVENTTYPE_MOUSE_UP,
    SAPP_EVENTTYPE_MOUSE_SCROLL,
    SAPP_EVENTTYPE_MOUSE_MOVE,
    SAPP_EVENTTYPE_MOUSE_ENTER,
    SAPP_EVENTTYPE_MOUSE_LEAVE,
    SAPP_EVENTTYPE_TOUCHES_BEGAN,
    SAPP_EVENTTYPE_TOUCHES_MOVED,
    SAPP_EVENTTYPE_TOUCHES_ENDED,
    SAPP_EVENTTYPE_TOUCHES_CANCELLED,
    SAPP_EVENTTYPE_RESIZED,
    SAPP_EVENTTYPE_ICONIFIED,
    SAPP_EVENTTYPE_RESTORED,
    SAPP_EVENTTYPE_FOCUSED,
    SAPP_EVENTTYPE_UNFOCUSED,
    SAPP_EVENTTYPE_SUSPENDED,
    SAPP_EVENTTYPE_RESUMED,
    SAPP_EVENTTYPE_QUIT_REQUESTED,
    SAPP_EVENTTYPE_CLIPBOARD_PASTED,
    SAPP_EVENTTYPE_FILES_DROPPED,
    _SAPP_EVENTTYPE_NUM,
    _SAPP_EVENTTYPE_FORCE_U32 = 0x7FFFFFFF
} sapp_event_type;

/*
    sapp_keycode

    The 'virtual keycode' of a KEY_DOWN or KEY_UP event in the
    struct field sapp_event.key_code.

    Note that the keycode values are identical with GLFW.
*/
typedef enum sapp_keycode {
    SAPP_KEYCODE_INVALID          = 0,
    SAPP_KEYCODE_SPACE            = 32,
    SAPP_KEYCODE_APOSTROPHE       = 39,  /* ' */
    SAPP_KEYCODE_COMMA            = 44,  /* , */
    SAPP_KEYCODE_MINUS            = 45,  /* - */
    SAPP_KEYCODE_PERIOD           = 46,  /* . */
    SAPP_KEYCODE_SLASH            = 47,  /* / */
    SAPP_KEYCODE_0                = 48,
    SAPP_KEYCODE_1                = 49,
    SAPP_KEYCODE_2                = 50,
    SAPP_KEYCODE_3                = 51,
    SAPP_KEYCODE_4                = 52,
    SAPP_KEYCODE_5                = 53,
    SAPP_KEYCODE_6                = 54,
    SAPP_KEYCODE_7                = 55,
    SAPP_KEYCODE_8                = 56,
    SAPP_KEYCODE_9                = 57,
    SAPP_KEYCODE_SEMICOLON        = 59,  /* ; */
    SAPP_KEYCODE_EQUAL            = 61,  /* = */
    SAPP_KEYCODE_A                = 65,
    SAPP_KEYCODE_B                = 66,
    SAPP_KEYCODE_C                = 67,
    SAPP_KEYCODE_D                = 68,
    SAPP_KEYCODE_E                = 69,
    SAPP_KEYCODE_F                = 70,
    SAPP_KEYCODE_G                = 71,
    SAPP_KEYCODE_H                = 72,
    SAPP_KEYCODE_I                = 73,
    SAPP_KEYCODE_J                = 74,
    SAPP_KEYCODE_K                = 75,
    SAPP_KEYCODE_L                = 76,
    SAPP_KEYCODE_M                = 77,
    SAPP_KEYCODE_N                = 78,
    SAPP_KEYCODE_O                = 79,
    SAPP_KEYCODE_P                = 80,
    SAPP_KEYCODE_Q                = 81,
    SAPP_KEYCODE_R                = 82,
    SAPP_KEYCODE_S                = 83,
    SAPP_KEYCODE_T                = 84,
    SAPP_KEYCODE_U                = 85,
    SAPP_KEYCODE_V                = 86,
    SAPP_KEYCODE_W                = 87,
    SAPP_KEYCODE_X                = 88,
    SAPP_KEYCODE_Y                = 89,
    SAPP_KEYCODE_Z                = 90,
    SAPP_KEYCODE_LEFT_BRACKET     = 91,  /* [ */
    SAPP_KEYCODE_BACKSLASH        = 92,  /* \ */
    SAPP_KEYCODE_RIGHT_BRACKET    = 93,  /* ] */
    SAPP_KEYCODE_GRAVE_ACCENT     = 96,  /* ` */
    SAPP_KEYCODE_WORLD_1          = 161, /* non-US #1 */
    SAPP_KEYCODE_WORLD_2          = 162, /* non-US #2 */
    SAPP_KEYCODE_ESCAPE           = 256,
    SAPP_KEYCODE_ENTER            = 257,
    SAPP_KEYCODE_TAB              = 258,
    SAPP_KEYCODE_BACKSPACE        = 259,
    SAPP_KEYCODE_INSERT           = 260,
    SAPP_KEYCODE_DELETE           = 261,
    SAPP_KEYCODE_RIGHT            = 262,
    SAPP_KEYCODE_LEFT             = 263,
    SAPP_KEYCODE_DOWN             = 264,
    SAPP_KEYCODE_UP               = 265,
    SAPP_KEYCODE_PAGE_UP          = 266,
    SAPP_KEYCODE_PAGE_DOWN        = 267,
    SAPP_KEYCODE_HOME             = 268,
    SAPP_KEYCODE_END              = 269,
    SAPP_KEYCODE_CAPS_LOCK        = 280,
    SAPP_KEYCODE_SCROLL_LOCK      = 281,
    SAPP_KEYCODE_NUM_LOCK         = 282,
    SAPP_KEYCODE_PRINT_SCREEN     = 283,
    SAPP_KEYCODE_PAUSE            = 284,
    SAPP_KEYCODE_F1               = 290,
    SAPP_KEYCODE_F2               = 291,
    SAPP_KEYCODE_F3               = 292,
    SAPP_KEYCODE_F4               = 293,
    SAPP_KEYCODE_F5               = 294,
    SAPP_KEYCODE_F6               = 295,
    SAPP_KEYCODE_F7               = 296,
    SAPP_KEYCODE_F8               = 297,
    SAPP_KEYCODE_F9               = 298,
    SAPP_KEYCODE_F10              = 299,
    SAPP_KEYCODE_F11              = 300,
    SAPP_KEYCODE_F12              = 301,
    SAPP_KEYCODE_F13              = 302,
    SAPP_KEYCODE_F14              = 303,
    SAPP_KEYCODE_F15              = 304,
    SAPP_KEYCODE_F16              = 305,
    SAPP_KEYCODE_F17              = 306,
    SAPP_KEYCODE_F18              = 307,
    SAPP_KEYCODE_F19              = 308,
    SAPP_KEYCODE_F20              = 309,
    SAPP_KEYCODE_F21              = 310,
    SAPP_KEYCODE_F22              = 311,
    SAPP_KEYCODE_F23              = 312,
    SAPP_KEYCODE_F24              = 313,
    SAPP_KEYCODE_F25              = 314,
    SAPP_KEYCODE_KP_0             = 320,
    SAPP_KEYCODE_KP_1             = 321,
    SAPP_KEYCODE_KP_2             = 322,
    SAPP_KEYCODE_KP_3             = 323,
    SAPP_KEYCODE_KP_4             = 324,
    SAPP_KEYCODE_KP_5             = 325,
    SAPP_KEYCODE_KP_6             = 326,
    SAPP_KEYCODE_KP_7             = 327,
    SAPP_KEYCODE_KP_8             = 328,
    SAPP_KEYCODE_KP_9             = 329,
    SAPP_KEYCODE_KP_DECIMAL       = 330,
    SAPP_KEYCODE_KP_DIVIDE        = 331,
    SAPP_KEYCODE_KP_MULTIPLY      = 332,
    SAPP_KEYCODE_KP_SUBTRACT      = 333,
    SAPP_KEYCODE_KP_ADD           = 334,
    SAPP_KEYCODE_KP_ENTER         = 335,
    SAPP_KEYCODE_KP_EQUAL         = 336,
    SAPP_KEYCODE_LEFT_SHIFT       = 340,
    SAPP_KEYCODE_LEFT_CONTROL     = 341,
    SAPP_KEYCODE_LEFT_ALT         = 342,
    SAPP_KEYCODE_LEFT_SUPER       = 343,
    SAPP_KEYCODE_RIGHT_SHIFT      = 344,
    SAPP_KEYCODE_RIGHT_CONTROL    = 345,
    SAPP_KEYCODE_RIGHT_ALT        = 346,
    SAPP_KEYCODE_RIGHT_SUPER      = 347,
    SAPP_KEYCODE_MENU             = 348,
} sapp_keycode;

/*
    Android specific 'tool type' enum for touch events. This lets the
    application check what type of input device was used for
    touch events.

    NOTE: the values must remain in sync with the corresponding
    Android SDK type, so don't change those.

    See https://developer.android.com/reference/android/view/MotionEvent#TOOL_TYPE_UNKNOWN
*/
typedef enum sapp_android_tooltype {
    SAPP_ANDROIDTOOLTYPE_UNKNOWN = 0,   // TOOL_TYPE_UNKNOWN
    SAPP_ANDROIDTOOLTYPE_FINGER = 1,    // TOOL_TYPE_FINGER
    SAPP_ANDROIDTOOLTYPE_STYLUS = 2,    // TOOL_TYPE_STYLUS
    SAPP_ANDROIDTOOLTYPE_MOUSE = 3,     // TOOL_TYPE_MOUSE
} sapp_android_tooltype;

/*
    sapp_touchpoint

    Describes a single touchpoint in a multitouch event (TOUCHES_BEGAN,
    TOUCHES_MOVED, TOUCHES_ENDED).

    Touch points are stored in the nested array sapp_event.touches[],
    and the number of touches is stored in sapp_event.num_touches.
*/
typedef struct sapp_touchpoint {
    uintptr_t identifier;
    float pos_x;
    float pos_y;
    sapp_android_tooltype android_tooltype; // only valid on Android
    bool changed;
} sapp_touchpoint;

/*
    sapp_mousebutton

    The currently pressed mouse button in the events MOUSE_DOWN
    and MOUSE_UP, stored in the struct field sapp_event.mouse_button.
*/
typedef enum sapp_mousebutton {
    SAPP_MOUSEBUTTON_LEFT = 0x0,
    SAPP_MOUSEBUTTON_RIGHT = 0x1,
    SAPP_MOUSEBUTTON_MIDDLE = 0x2,
    SAPP_MOUSEBUTTON_INVALID = 0x100,
} sapp_mousebutton;

/*
    These are currently pressed modifier keys (and mouse buttons) which are
    passed in the event struct field sapp_event.modifiers.
*/
enum {
    SAPP_MODIFIER_SHIFT = 0x1,      // left or right shift key
    SAPP_MODIFIER_CTRL  = 0x2,      // left or right control key
    SAPP_MODIFIER_ALT   = 0x4,      // left or right alt key
    SAPP_MODIFIER_SUPER = 0x8,      // left or right 'super' key
    SAPP_MODIFIER_LMB   = 0x100,    // left mouse button
    SAPP_MODIFIER_RMB   = 0x200,    // right mouse button
    SAPP_MODIFIER_MMB   = 0x400,    // middle mouse button
};

/*
    sapp_event

    This is an all-in-one event struct passed to the event handler
    user callback function. Note that it depends on the event
    type what struct fields actually contain useful values, so you
    should first check the event type before reading other struct
    fields.
*/
typedef struct sapp_event {
    uint64_t frame_count;               // current frame counter, always valid, useful for checking if two events were issued in the same frame
    sapp_event_type type;               // the event type, always valid
    sapp_keycode key_code;              // the virtual key code, only valid in KEY_UP, KEY_DOWN
    uint32_t char_code;                 // the UTF-32 character code, only valid in CHAR events
    bool key_repeat;                    // true if this is a key-repeat event, valid in KEY_UP, KEY_DOWN and CHAR
    uint32_t modifiers;                 // current modifier keys, valid in all key-, char- and mouse-events
    sapp_mousebutton mouse_button;      // mouse button that was pressed or released, valid in MOUSE_DOWN, MOUSE_UP
    float mouse_x;                      // current horizontal mouse position in pixels, always valid except during mouse lock
    float mouse_y;                      // current vertical mouse position in pixels, always valid except during mouse lock
    float mouse_dx;                     // relative horizontal mouse movement since last frame, always valid
    float mouse_dy;                     // relative vertical mouse movement since last frame, always valid
    float scroll_x;                     // horizontal mouse wheel scroll distance, valid in MOUSE_SCROLL events
    float scroll_y;                     // vertical mouse wheel scroll distance, valid in MOUSE_SCROLL events
    int num_touches;                    // number of valid items in the touches[] array
    sapp_touchpoint touches[SAPP_MAX_TOUCHPOINTS];  // current touch points, valid in TOUCHES_BEGIN, TOUCHES_MOVED, TOUCHES_ENDED
    int window_width;                   // current window- and framebuffer sizes in pixels, always valid
    int window_height;
    int framebuffer_width;              // = window_width * dpi_scale
    int framebuffer_height;             // = window_height * dpi_scale
} sapp_event;

/*
    sg_range

    A general pointer/size-pair struct and constructor macros for passing binary blobs
    into sokol_app.h.
*/
typedef struct sapp_range {
    const void* ptr;
    size_t size;
} sapp_range;
// disabling this for every includer isn't great, but the warnings are also quite pointless
#if defined(_MSC_VER)
#pragma warning(disable:4221)   /* /W4 only: nonstandard extension used: 'x': cannot be initialized using address of automatic variable 'y' */
#pragma warning(disable:4204)   /* VS2015: nonstandard extension used: non-constant aggregate initializer */
#endif
#if defined(__cplusplus)
#define SAPP_RANGE(x) sapp_range{ &x, sizeof(x) }
#else
#define SAPP_RANGE(x) (sapp_range){ &x, sizeof(x) }
#endif

/*
    sapp_image_desc

    This is used to describe image data to sokol_app.h (at first, window
    icons, later maybe cursor images).

    Note that the actual image pixel format depends on the use case:

    - window icon pixels are RGBA8
*/
typedef struct sapp_image_desc {
    int width;
    int height;
    sapp_range pixels;
} sapp_image_desc;

/*
    sapp_icon_desc

    An icon description structure for use in sapp_desc.icon and
    sapp_set_icon().

    When setting a custom image, the application can provide a number of
    candidates differing in size, and sokol_app.h will pick the image(s)
    closest to the size expected by the platform's window system.

    To set sokol-app's default icon, set .sokol_default to true.

    Otherwise provide candidate images of different sizes in the
    images[] array.

    If both the sokol_default flag is set to true, any image candidates
    will be ignored and the sokol_app.h default icon will be set.
*/
typedef struct sapp_icon_desc {
    bool sokol_default;
    sapp_image_desc images[SAPP_MAX_ICONIMAGES];
} sapp_icon_desc;

/*
    sapp_allocator

    Used in sapp_desc to provide custom memory-alloc and -free functions
    to sokol_app.h. If memory management should be overridden, both the
    alloc_fn and free_fn function must be provided (e.g. it's not valid to
    override one function but not the other).
*/
typedef struct sapp_allocator {
    void* (*alloc_fn)(size_t size, void* user_data);
    void (*free_fn)(void* ptr, void* user_data);
    void* user_data;
} sapp_allocator;

/*
    sapp_log_item

    Log items are defined via X-Macros and expanded to an enum
    'sapp_log_item', and in debug mode to corresponding
    human readable error messages.
*/
#define _SAPP_LOG_ITEMS \
    _SAPP_LOGITEM_XMACRO(OK, "Ok") \
    _SAPP_LOGITEM_XMACRO(MALLOC_FAILED, "memory allocation failed") \
    _SAPP_LOGITEM_XMACRO(MACOS_INVALID_NSOPENGL_PROFILE, "macos: invalid NSOpenGLProfile (valid choices are 1.0 and 4.1)") \
    _SAPP_LOGITEM_XMACRO(WIN32_LOAD_OPENGL32_DLL_FAILED, "failed loading opengl32.dll") \
    _SAPP_LOGITEM_XMACRO(WIN32_CREATE_HELPER_WINDOW_FAILED, "failed to create helper window") \
    _SAPP_LOGITEM_XMACRO(WIN32_HELPER_WINDOW_GETDC_FAILED, "failed to get helper window DC") \
    _SAPP_LOGITEM_XMACRO(WIN32_DUMMY_CONTEXT_SET_PIXELFORMAT_FAILED, "failed to set pixel format for dummy GL context") \
    _SAPP_LOGITEM_XMACRO(WIN32_CREATE_DUMMY_CONTEXT_FAILED, "failed to create dummy GL context") \
    _SAPP_LOGITEM_XMACRO(WIN32_DUMMY_CONTEXT_MAKE_CURRENT_FAILED, "failed to make dummy GL context current") \
    _SAPP_LOGITEM_XMACRO(WIN32_GET_PIXELFORMAT_ATTRIB_FAILED, "failed to get WGL pixel format attribute") \
    _SAPP_LOGITEM_XMACRO(WIN32_WGL_FIND_PIXELFORMAT_FAILED, "failed to find matching WGL pixel format") \
    _SAPP_LOGITEM_XMACRO(WIN32_WGL_DESCRIBE_PIXELFORMAT_FAILED, "failed to get pixel format descriptor") \
    _SAPP_LOGITEM_XMACRO(WIN32_WGL_SET_PIXELFORMAT_FAILED, "failed to set selected pixel format") \
    _SAPP_LOGITEM_XMACRO(WIN32_WGL_ARB_CREATE_CONTEXT_REQUIRED, "ARB_create_context required") \
    _SAPP_LOGITEM_XMACRO(WIN32_WGL_ARB_CREATE_CONTEXT_PROFILE_REQUIRED, "ARB_create_context_profile required") \
    _SAPP_LOGITEM_XMACRO(WIN32_WGL_OPENGL_VERSION_NOT_SUPPORTED, "requested OpenGL version not supported by GL driver (ERROR_INVALID_VERSION_ARB)") \
    _SAPP_LOGITEM_XMACRO(WIN32_WGL_OPENGL_PROFILE_NOT_SUPPORTED, "requested OpenGL profile not support by GL driver (ERROR_INVALID_PROFILE_ARB)") \
    _SAPP_LOGITEM_XMACRO(WIN32_WGL_INCOMPATIBLE_DEVICE_CONTEXT, "CreateContextAttribsARB failed with ERROR_INCOMPATIBLE_DEVICE_CONTEXTS_ARB") \
    _SAPP_LOGITEM_XMACRO(WIN32_WGL_CREATE_CONTEXT_ATTRIBS_FAILED_OTHER, "CreateContextAttribsARB failed for other reason") \
    _SAPP_LOGITEM_XMACRO(WIN32_D3D11_CREATE_DEVICE_AND_SWAPCHAIN_WITH_DEBUG_FAILED, "D3D11CreateDeviceAndSwapChain() with D3D11_CREATE_DEVICE_DEBUG failed, retrying without debug flag.") \
    _SAPP_LOGITEM_XMACRO(WIN32_D3D11_GET_IDXGIFACTORY_FAILED, "could not obtain IDXGIFactory object") \
    _SAPP_LOGITEM_XMACRO(WIN32_D3D11_GET_IDXGIADAPTER_FAILED, "could not obtain IDXGIAdapter object") \
    _SAPP_LOGITEM_XMACRO(WIN32_D3D11_QUERY_INTERFACE_IDXGIDEVICE1_FAILED, "could not obtain IDXGIDevice1 interface") \
    _SAPP_LOGITEM_XMACRO(WIN32_REGISTER_RAW_INPUT_DEVICES_FAILED_MOUSE_LOCK, "RegisterRawInputDevices() failed (on mouse lock)") \
    _SAPP_LOGITEM_XMACRO(WIN32_REGISTER_RAW_INPUT_DEVICES_FAILED_MOUSE_UNLOCK, "RegisterRawInputDevices() failed (on mouse unlock)") \
    _SAPP_LOGITEM_XMACRO(WIN32_GET_RAW_INPUT_DATA_FAILED, "GetRawInputData() failed") \
    _SAPP_LOGITEM_XMACRO(LINUX_GLX_LOAD_LIBGL_FAILED, "failed to load libGL") \
    _SAPP_LOGITEM_XMACRO(LINUX_GLX_LOAD_ENTRY_POINTS_FAILED, "failed to load GLX entry points") \
    _SAPP_LOGITEM_XMACRO(LINUX_GLX_EXTENSION_NOT_FOUND, "GLX extension not found") \
    _SAPP_LOGITEM_XMACRO(LINUX_GLX_QUERY_VERSION_FAILED, "failed to query GLX version") \
    _SAPP_LOGITEM_XMACRO(LINUX_GLX_VERSION_TOO_LOW, "GLX version too low (need at least 1.3)") \
    _SAPP_LOGITEM_XMACRO(LINUX_GLX_NO_GLXFBCONFIGS, "glXGetFBConfigs() returned no configs") \
    _SAPP_LOGITEM_XMACRO(LINUX_GLX_NO_SUITABLE_GLXFBCONFIG, "failed to find a suitable GLXFBConfig") \
    _SAPP_LOGITEM_XMACRO(LINUX_GLX_GET_VISUAL_FROM_FBCONFIG_FAILED, "glXGetVisualFromFBConfig failed") \
    _SAPP_LOGITEM_XMACRO(LINUX_GLX_REQUIRED_EXTENSIONS_MISSING, "GLX extensions ARB_create_context and ARB_create_context_profile missing") \
    _SAPP_LOGITEM_XMACRO(LINUX_GLX_CREATE_CONTEXT_FAILED, "Failed to create GL context via glXCreateContextAttribsARB") \
    _SAPP_LOGITEM_XMACRO(LINUX_GLX_CREATE_WINDOW_FAILED, "glXCreateWindow() failed") \
    _SAPP_LOGITEM_XMACRO(LINUX_X11_CREATE_WINDOW_FAILED, "XCreateWindow() failed") \
    _SAPP_LOGITEM_XMACRO(LINUX_EGL_BIND_OPENGL_API_FAILED, "eglBindAPI(EGL_OPENGL_API) failed") \
    _SAPP_LOGITEM_XMACRO(LINUX_EGL_BIND_OPENGL_ES_API_FAILED, "eglBindAPI(EGL_OPENGL_ES_API) failed") \
    _SAPP_LOGITEM_XMACRO(LINUX_EGL_GET_DISPLAY_FAILED, "eglGetDisplay() failed") \
    _SAPP_LOGITEM_XMACRO(LINUX_EGL_INITIALIZE_FAILED, "eglInitialize() failed") \
    _SAPP_LOGITEM_XMACRO(LINUX_EGL_NO_CONFIGS, "eglChooseConfig() returned no configs") \
    _SAPP_LOGITEM_XMACRO(LINUX_EGL_NO_NATIVE_VISUAL, "eglGetConfigAttrib() for EGL_NATIVE_VISUAL_ID failed") \
    _SAPP_LOGITEM_XMACRO(LINUX_EGL_GET_VISUAL_INFO_FAILED, "XGetVisualInfo() failed") \
    _SAPP_LOGITEM_XMACRO(LINUX_EGL_CREATE_WINDOW_SURFACE_FAILED, "eglCreateWindowSurface() failed") \
    _SAPP_LOGITEM_XMACRO(LINUX_EGL_CREATE_CONTEXT_FAILED, "eglCreateContext() failed") \
    _SAPP_LOGITEM_XMACRO(LINUX_EGL_MAKE_CURRENT_FAILED, "eglMakeCurrent() failed") \
    _SAPP_LOGITEM_XMACRO(LINUX_X11_OPEN_DISPLAY_FAILED, "XOpenDisplay() failed") \
    _SAPP_LOGITEM_XMACRO(LINUX_X11_QUERY_SYSTEM_DPI_FAILED, "failed to query system dpi value, assuming default 96.0") \
    _SAPP_LOGITEM_XMACRO(LINUX_X11_DROPPED_FILE_URI_WRONG_SCHEME, "dropped file URL doesn't start with 'file://'") \
    _SAPP_LOGITEM_XMACRO(ANDROID_UNSUPPORTED_INPUT_EVENT_INPUT_CB, "unsupported input event encountered in _sapp_android_input_cb()") \
    _SAPP_LOGITEM_XMACRO(ANDROID_UNSUPPORTED_INPUT_EVENT_MAIN_CB, "unsupported input event encountered in _sapp_android_main_cb()") \
    _SAPP_LOGITEM_XMACRO(ANDROID_READ_MSG_FAILED, "failed to read message in _sapp_android_main_cb()") \
    _SAPP_LOGITEM_XMACRO(ANDROID_WRITE_MSG_FAILED, "failed to write message in _sapp_android_msg") \
    _SAPP_LOGITEM_XMACRO(ANDROID_MSG_CREATE, "MSG_CREATE") \
    _SAPP_LOGITEM_XMACRO(ANDROID_MSG_RESUME, "MSG_RESUME") \
    _SAPP_LOGITEM_XMACRO(ANDROID_MSG_PAUSE, "MSG_PAUSE") \
    _SAPP_LOGITEM_XMACRO(ANDROID_MSG_FOCUS, "MSG_FOCUS") \
    _SAPP_LOGITEM_XMACRO(ANDROID_MSG_NO_FOCUS, "MSG_NO_FOCUS") \
    _SAPP_LOGITEM_XMACRO(ANDROID_MSG_SET_NATIVE_WINDOW, "MSG_SET_NATIVE_WINDOW") \
    _SAPP_LOGITEM_XMACRO(ANDROID_MSG_SET_INPUT_QUEUE, "MSG_SET_INPUT_QUEUE") \
    _SAPP_LOGITEM_XMACRO(ANDROID_MSG_DESTROY, "MSG_DESTROY") \
    _SAPP_LOGITEM_XMACRO(ANDROID_UNKNOWN_MSG, "unknown msg type received") \
    _SAPP_LOGITEM_XMACRO(ANDROID_LOOP_THREAD_STARTED, "loop thread started") \
    _SAPP_LOGITEM_XMACRO(ANDROID_LOOP_THREAD_DONE, "loop thread done") \
    _SAPP_LOGITEM_XMACRO(ANDROID_NATIVE_ACTIVITY_ONSTART, "NativeActivity onStart()") \
    _SAPP_LOGITEM_XMACRO(ANDROID_NATIVE_ACTIVITY_ONRESUME, "NativeActivity onResume") \
    _SAPP_LOGITEM_XMACRO(ANDROID_NATIVE_ACTIVITY_ONSAVEINSTANCESTATE, "NativeActivity onSaveInstanceState") \
    _SAPP_LOGITEM_XMACRO(ANDROID_NATIVE_ACTIVITY_ONWINDOWFOCUSCHANGED, "NativeActivity onWindowFocusChanged") \
    _SAPP_LOGITEM_XMACRO(ANDROID_NATIVE_ACTIVITY_ONPAUSE, "NativeActivity onPause") \
    _SAPP_LOGITEM_XMACRO(ANDROID_NATIVE_ACTIVITY_ONSTOP, "NativeActivity onStop()") \
    _SAPP_LOGITEM_XMACRO(ANDROID_NATIVE_ACTIVITY_ONNATIVEWINDOWCREATED, "NativeActivity onNativeWindowCreated") \
    _SAPP_LOGITEM_XMACRO(ANDROID_NATIVE_ACTIVITY_ONNATIVEWINDOWDESTROYED, "NativeActivity onNativeWindowDestroyed") \
    _SAPP_LOGITEM_XMACRO(ANDROID_NATIVE_ACTIVITY_ONINPUTQUEUECREATED, "NativeActivity onInputQueueCreated") \
    _SAPP_LOGITEM_XMACRO(ANDROID_NATIVE_ACTIVITY_ONINPUTQUEUEDESTROYED, "NativeActivity onInputQueueDestroyed") \
    _SAPP_LOGITEM_XMACRO(ANDROID_NATIVE_ACTIVITY_ONCONFIGURATIONCHANGED, "NativeActivity onConfigurationChanged") \
    _SAPP_LOGITEM_XMACRO(ANDROID_NATIVE_ACTIVITY_ONLOWMEMORY, "NativeActivity onLowMemory") \
    _SAPP_LOGITEM_XMACRO(ANDROID_NATIVE_ACTIVITY_ONDESTROY, "NativeActivity onDestroy") \
    _SAPP_LOGITEM_XMACRO(ANDROID_NATIVE_ACTIVITY_DONE, "NativeActivity done") \
    _SAPP_LOGITEM_XMACRO(ANDROID_NATIVE_ACTIVITY_ONCREATE, "NativeActivity onCreate") \
    _SAPP_LOGITEM_XMACRO(ANDROID_CREATE_THREAD_PIPE_FAILED, "failed to create thread pipe") \
    _SAPP_LOGITEM_XMACRO(ANDROID_NATIVE_ACTIVITY_CREATE_SUCCESS, "NativeActivity successfully created") \
    _SAPP_LOGITEM_XMACRO(WGPU_SWAPCHAIN_CREATE_SURFACE_FAILED, "wgpu: failed to create surface for swapchain") \
    _SAPP_LOGITEM_XMACRO(WGPU_SWAPCHAIN_CREATE_SWAPCHAIN_FAILED, "wgpu: failed to create swapchain object") \
    _SAPP_LOGITEM_XMACRO(WGPU_SWAPCHAIN_CREATE_DEPTH_STENCIL_TEXTURE_FAILED, "wgpu: failed to create depth-stencil texture for swapchain") \
    _SAPP_LOGITEM_XMACRO(WGPU_SWAPCHAIN_CREATE_DEPTH_STENCIL_VIEW_FAILED, "wgpu: failed to create view object for swapchain depth-stencil texture") \
    _SAPP_LOGITEM_XMACRO(WGPU_SWAPCHAIN_CREATE_MSAA_TEXTURE_FAILED, "wgpu: failed to create msaa texture for swapchain") \
    _SAPP_LOGITEM_XMACRO(WGPU_SWAPCHAIN_CREATE_MSAA_VIEW_FAILED, "wgpu: failed to create view object for swapchain msaa texture") \
    _SAPP_LOGITEM_XMACRO(WGPU_REQUEST_DEVICE_STATUS_ERROR, "wgpu: requesting device failed with status 'error'") \
    _SAPP_LOGITEM_XMACRO(WGPU_REQUEST_DEVICE_STATUS_UNKNOWN, "wgpu: requesting device failed with status 'unknown'") \
    _SAPP_LOGITEM_XMACRO(WGPU_REQUEST_ADAPTER_STATUS_UNAVAILABLE, "wgpu: requesting adapter failed with 'unavailable'") \
    _SAPP_LOGITEM_XMACRO(WGPU_REQUEST_ADAPTER_STATUS_ERROR, "wgpu: requesting adapter failed with status 'error'") \
    _SAPP_LOGITEM_XMACRO(WGPU_REQUEST_ADAPTER_STATUS_UNKNOWN, "wgpu: requesting adapter failed with status 'unknown'") \
    _SAPP_LOGITEM_XMACRO(WGPU_CREATE_INSTANCE_FAILED, "wgpu: failed to create instance") \
    _SAPP_LOGITEM_XMACRO(IMAGE_DATA_SIZE_MISMATCH, "image data size mismatch (must be width*height*4 bytes)") \
    _SAPP_LOGITEM_XMACRO(DROPPED_FILE_PATH_TOO_LONG, "dropped file path too long (sapp_desc.max_dropped_filed_path_length)") \
    _SAPP_LOGITEM_XMACRO(CLIPBOARD_STRING_TOO_BIG, "clipboard string didn't fit into clipboard buffer") \

#define _SAPP_LOGITEM_XMACRO(item,msg) SAPP_LOGITEM_##item,
typedef enum sapp_log_item {
    _SAPP_LOG_ITEMS
} sapp_log_item;
#undef _SAPP_LOGITEM_XMACRO

/*
    sapp_logger

    Used in sapp_desc to provide a logging function. Please be aware that
    without logging function, sokol-app will be completely silent, e.g. it will
    not report errors or warnings. For maximum error verbosity, compile in
    debug mode (e.g. NDEBUG *not* defined) and install a logger (for instance
    the standard logging function from sokol_log.h).
*/
typedef struct sapp_logger {
    void (*func)(
        const char* tag,                // always "sapp"
        uint32_t log_level,             // 0=panic, 1=error, 2=warning, 3=info
        uint32_t log_item_id,           // SAPP_LOGITEM_*
        const char* message_or_null,    // a message string, may be nullptr in release mode
        uint32_t line_nr,               // line number in sokol_app.h
        const char* filename_or_null,   // source filename, may be nullptr in release mode
        void* user_data);
    void* user_data;
} sapp_logger;

typedef struct sapp_desc {
    void (*init_cb)(void);                  // these are the user-provided callbacks without user data
    void (*frame_cb)(void);
    void (*cleanup_cb)(void);
    void (*event_cb)(const sapp_event*);

    void* user_data;                        // these are the user-provided callbacks with user data
    void (*init_userdata_cb)(void*);
    void (*frame_userdata_cb)(void*);
    void (*cleanup_userdata_cb)(void*);
    void (*event_userdata_cb)(const sapp_event*, void*);

    int width;                          // the preferred width of the window / canvas
    int height;                         // the preferred height of the window / canvas
    int sample_count;                   // MSAA sample count
    int swap_interval;                  // the preferred swap interval (ignored on some platforms)
    bool high_dpi;                      // whether the rendering canvas is full-resolution on HighDPI displays
    bool fullscreen;                    // whether the window should be created in fullscreen mode
    bool alpha;                         // whether the framebuffer should have an alpha channel (ignored on some platforms)
    const char* window_title;           // the window title as UTF-8 encoded string
    bool enable_clipboard;              // enable clipboard access, default is false
    int clipboard_size;                 // max size of clipboard content in bytes
    bool enable_dragndrop;              // enable file dropping (drag'n'drop), default is false
    int max_dropped_files;              // max number of dropped files to process (default: 1)
    int max_dropped_file_path_length;   // max length in bytes of a dropped UTF-8 file path (default: 2048)
    sapp_icon_desc icon;                // the initial window icon to set
    sapp_allocator allocator;           // optional memory allocation overrides (default: malloc/free)
    sapp_logger logger;                 // logging callback override (default: NO LOGGING!)

    // backend-specific options
    int gl_major_version;               // override GL major and minor version (the default GL version is 4.1 on macOS, 4.3 elsewhere)
    int gl_minor_version;
    bool win32_console_utf8;            // if true, set the output console codepage to UTF-8
    bool win32_console_create;          // if true, attach stdout/stderr to a new console window
    bool win32_console_attach;          // if true, attach stdout/stderr to parent process
    const char* html5_canvas_name;      // the name (id) of the HTML5 canvas element, default is "canvas"
    bool html5_canvas_resize;           // if true, the HTML5 canvas size is set to sapp_desc.width/height, otherwise canvas size is tracked
    bool html5_preserve_drawing_buffer; // HTML5 only: whether to preserve default framebuffer content between frames
    bool html5_premultiplied_alpha;     // HTML5 only: whether the rendered pixels use premultiplied alpha convention
    bool html5_ask_leave_site;          // initial state of the internal html5_ask_leave_site flag (see sapp_html5_ask_leave_site())
    bool html5_bubble_mouse_events;     // if true, mouse events will bubble up to the web page
    bool html5_bubble_touch_events;     // same for touch events
    bool html5_bubble_wheel_events;     // same for wheel events
    bool html5_bubble_key_events;       // if true, bubble up *all* key events to browser, not just key events that represent characters
    bool html5_bubble_char_events;      // if true, bubble up character events to browser
    bool html5_use_emsc_set_main_loop;  // if true, use emscripten_set_main_loop() instead of emscripten_request_animation_frame_loop()
    bool html5_emsc_set_main_loop_simulate_infinite_loop;   // this will be passed as the simulate_infinite_loop arg to emscripten_set_main_loop()
    bool ios_keyboard_resizes_canvas;   // if true, showing the iOS keyboard shrinks the canvas
    // __v_ start
    bool __v_native_render;             // V patch to allow for native rendering
    int min_width;
    int min_height;
    // __v_ end
} sapp_desc;

/* HTML5 specific: request and response structs for
   asynchronously loading dropped-file content.
*/
typedef enum sapp_html5_fetch_error {
    SAPP_HTML5_FETCH_ERROR_NO_ERROR,
    SAPP_HTML5_FETCH_ERROR_BUFFER_TOO_SMALL,
    SAPP_HTML5_FETCH_ERROR_OTHER,
} sapp_html5_fetch_error;

typedef struct sapp_html5_fetch_response {
    bool succeeded;         // true if the loading operation has succeeded
    sapp_html5_fetch_error error_code;
    int file_index;         // index of the dropped file (0..sapp_get_num_dropped_filed()-1)
    sapp_range data;        // pointer and size of the fetched data (data.ptr == buffer.ptr, data.size <= buffer.size)
    sapp_range buffer;      // the user-provided buffer ptr/size pair (buffer.ptr == data.ptr, buffer.size >= data.size)
    void* user_data;        // user-provided user data pointer
} sapp_html5_fetch_response;

typedef struct sapp_html5_fetch_request {
    int dropped_file_index; // 0..sapp_get_num_dropped_files()-1
    void (*callback)(const sapp_html5_fetch_response*);     // response callback function pointer (required)
    sapp_range buffer;      // ptr/size of a memory buffer to load the data into
    void* user_data;        // optional userdata pointer
} sapp_html5_fetch_request;

/*
    sapp_mouse_cursor

    Predefined cursor image definitions, set with sapp_set_mouse_cursor(sapp_mouse_cursor cursor)
*/
typedef enum sapp_mouse_cursor {
    SAPP_MOUSECURSOR_DEFAULT = 0,   // equivalent with system default cursor
    SAPP_MOUSECURSOR_ARROW,
    SAPP_MOUSECURSOR_IBEAM,
    SAPP_MOUSECURSOR_CROSSHAIR,
    SAPP_MOUSECURSOR_POINTING_HAND,
    SAPP_MOUSECURSOR_RESIZE_EW,
    SAPP_MOUSECURSOR_RESIZE_NS,
    SAPP_MOUSECURSOR_RESIZE_NWSE,
    SAPP_MOUSECURSOR_RESIZE_NESW,
    SAPP_MOUSECURSOR_RESIZE_ALL,
    SAPP_MOUSECURSOR_NOT_ALLOWED,
    _SAPP_MOUSECURSOR_NUM,
} sapp_mouse_cursor;

/* user-provided functions */
extern sapp_desc sokol_main(int argc, char* argv[]);

/* returns true after sokol-app has been initialized */
SOKOL_APP_API_DECL bool sapp_isvalid(void);
/* returns the current framebuffer width in pixels */
SOKOL_APP_API_DECL int sapp_width(void);
/* same as sapp_width(), but returns float */
SOKOL_APP_API_DECL float sapp_widthf(void);
/* returns the current framebuffer height in pixels */
SOKOL_APP_API_DECL int sapp_height(void);
/* same as sapp_height(), but returns float */
SOKOL_APP_API_DECL float sapp_heightf(void);
/* get default framebuffer color pixel format */
SOKOL_APP_API_DECL int sapp_color_format(void);
/* get default framebuffer depth pixel format */
SOKOL_APP_API_DECL int sapp_depth_format(void);
/* get default framebuffer sample count */
SOKOL_APP_API_DECL int sapp_sample_count(void);
/* returns true when high_dpi was requested and actually running in a high-dpi scenario */
SOKOL_APP_API_DECL bool sapp_high_dpi(void);
/* returns the dpi scaling factor (window pixels to framebuffer pixels) */
SOKOL_APP_API_DECL float sapp_dpi_scale(void);
/* show or hide the mobile device onscreen keyboard */
SOKOL_APP_API_DECL void sapp_show_keyboard(bool show);
/* return true if the mobile device onscreen keyboard is currently shown */
SOKOL_APP_API_DECL bool sapp_keyboard_shown(void);
/* query fullscreen mode */
SOKOL_APP_API_DECL bool sapp_is_fullscreen(void);
/* toggle fullscreen mode */
SOKOL_APP_API_DECL void sapp_toggle_fullscreen(void);
/* show or hide the mouse cursor */
SOKOL_APP_API_DECL void sapp_show_mouse(bool show);
/* show or hide the mouse cursor */
SOKOL_APP_API_DECL bool sapp_mouse_shown(void);
/* enable/disable mouse-pointer-lock mode */
SOKOL_APP_API_DECL void sapp_lock_mouse(bool lock);
/* return true if in mouse-pointer-lock mode (this may toggle a few frames later) */
SOKOL_APP_API_DECL bool sapp_mouse_locked(void);
/* set mouse cursor type */
SOKOL_APP_API_DECL void sapp_set_mouse_cursor(sapp_mouse_cursor cursor);
/* get current mouse cursor type */
SOKOL_APP_API_DECL sapp_mouse_cursor sapp_get_mouse_cursor(void);
/* return the userdata pointer optionally provided in sapp_desc */
SOKOL_APP_API_DECL void* sapp_userdata(void);
/* return a copy of the sapp_desc structure */
SOKOL_APP_API_DECL sapp_desc sapp_query_desc(void);
/* initiate a "soft quit" (sends SAPP_EVENTTYPE_QUIT_REQUESTED) */
SOKOL_APP_API_DECL void sapp_request_quit(void);
/* cancel a pending quit (when SAPP_EVENTTYPE_QUIT_REQUESTED has been received) */
SOKOL_APP_API_DECL void sapp_cancel_quit(void);
/* initiate a "hard quit" (quit application without sending SAPP_EVENTTYPE_QUIT_REQUESTED) */
SOKOL_APP_API_DECL void sapp_quit(void);
/* call from inside event callback to consume the current event (don't forward to platform) */
SOKOL_APP_API_DECL void sapp_consume_event(void);
/* get the current frame counter (for comparison with sapp_event.frame_count) */
SOKOL_APP_API_DECL uint64_t sapp_frame_count(void);
/* get an averaged/smoothed frame duration in seconds */
SOKOL_APP_API_DECL double sapp_frame_duration(void);
/* write string into clipboard */
SOKOL_APP_API_DECL void sapp_set_clipboard_string(const char* str);
/* read string from clipboard (usually during SAPP_EVENTTYPE_CLIPBOARD_PASTED) */
SOKOL_APP_API_DECL const char* sapp_get_clipboard_string(void);
/* set the window title (only on desktop platforms) */
SOKOL_APP_API_DECL void sapp_set_window_title(const char* str);
/* set the window icon (only on Windows and Linux) */
SOKOL_APP_API_DECL void sapp_set_icon(const sapp_icon_desc* icon_desc);
/* gets the total number of dropped files (after an SAPP_EVENTTYPE_FILES_DROPPED event) */
SOKOL_APP_API_DECL int sapp_get_num_dropped_files(void);
/* gets the dropped file paths */
SOKOL_APP_API_DECL const char* sapp_get_dropped_file_path(int index);

/* special run-function for SOKOL_NO_ENTRY (in standard mode this is an empty stub) */
SOKOL_APP_API_DECL void sapp_run(const sapp_desc* desc);

/* EGL: get EGLDisplay object */
SOKOL_APP_API_DECL const void* sapp_egl_get_display(void);
/* EGL: get EGLContext object */
SOKOL_APP_API_DECL const void* sapp_egl_get_context(void);

/* HTML5: enable or disable the hardwired "Leave Site?" dialog box */
SOKOL_APP_API_DECL void sapp_html5_ask_leave_site(bool ask);
/* HTML5: get byte size of a dropped file */
SOKOL_APP_API_DECL uint32_t sapp_html5_get_dropped_file_size(int index);
/* HTML5: asynchronously load the content of a dropped file */
SOKOL_APP_API_DECL void sapp_html5_fetch_dropped_file(const sapp_html5_fetch_request* request);

/* Metal: get bridged pointer to Metal device object */
SOKOL_APP_API_DECL const void* sapp_metal_get_device(void);
/* Metal: get bridged pointer to MTKView's current drawable of type CAMetalDrawable */
SOKOL_APP_API_DECL const void* sapp_metal_get_current_drawable(void);
/* Metal: get bridged pointer to MTKView's depth-stencil texture of type MTLTexture */
SOKOL_APP_API_DECL const void* sapp_metal_get_depth_stencil_texture(void);
/* Metal: get bridged pointer to MTKView's msaa-color-texture of type MTLTexture (may be null) */
SOKOL_APP_API_DECL const void* sapp_metal_get_msaa_color_texture(void);
/* macOS: get bridged pointer to macOS NSWindow */
SOKOL_APP_API_DECL const void* sapp_macos_get_window(void);
/* iOS: get bridged pointer to iOS UIWindow */
SOKOL_APP_API_DECL const void* sapp_ios_get_window(void);

/* D3D11: get pointer to ID3D11Device object */
SOKOL_APP_API_DECL const void* sapp_d3d11_get_device(void);
/* D3D11: get pointer to ID3D11DeviceContext object */
SOKOL_APP_API_DECL const void* sapp_d3d11_get_device_context(void);
/* D3D11: get pointer to IDXGISwapChain object */
SOKOL_APP_API_DECL const void* sapp_d3d11_get_swap_chain(void);
/* D3D11: get pointer to ID3D11RenderTargetView object for rendering */
SOKOL_APP_API_DECL const void* sapp_d3d11_get_render_view(void);
/* D3D11: get pointer ID3D11RenderTargetView object for msaa-resolve (may return null) */
SOKOL_APP_API_DECL const void* sapp_d3d11_get_resolve_view(void);
/* D3D11: get pointer ID3D11DepthStencilView */
SOKOL_APP_API_DECL const void* sapp_d3d11_get_depth_stencil_view(void);
/* Win32: get the HWND window handle */
SOKOL_APP_API_DECL const void* sapp_win32_get_hwnd(void);

/* WebGPU: get WGPUDevice handle */
SOKOL_APP_API_DECL const void* sapp_wgpu_get_device(void);
/* WebGPU: get swapchain's WGPUTextureView handle for rendering */
SOKOL_APP_API_DECL const void* sapp_wgpu_get_render_view(void);
/* WebGPU: get swapchain's MSAA-resolve WGPUTextureView (may return null) */
SOKOL_APP_API_DECL const void* sapp_wgpu_get_resolve_view(void);
/* WebGPU: get swapchain's WGPUTextureView for the depth-stencil surface */
SOKOL_APP_API_DECL const void* sapp_wgpu_get_depth_stencil_view(void);

/* GL: get framebuffer object */
SOKOL_APP_API_DECL uint32_t sapp_gl_get_framebuffer(void);
/* GL: get major version (only valid for desktop GL) */
SOKOL_APP_API_DECL int sapp_gl_get_major_version(void);
/* GL: get minor version (only valid for desktop GL) */
SOKOL_APP_API_DECL int sapp_gl_get_minor_version(void);

/* Android: get native activity handle */
SOKOL_APP_API_DECL const void* sapp_android_get_native_activity(void);

#ifdef __cplusplus
} /* extern "C" */

/* reference-based equivalents for C++ */
inline void sapp_run(const sapp_desc& desc) { return sapp_run(&desc); }

#endif

// this WinRT specific hack is required when wWinMain is in a static library
#if defined(_MSC_VER) && defined(UNICODE)
#include <winapifamily.h>
#if defined(WINAPI_FAMILY_PARTITION) && !WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
#pragma comment(linker, "/include:wWinMain")
#endif
#endif

#endif // SOKOL_APP_INCLUDED

// ██ ███    ███ ██████  ██      ███████ ███    ███ ███████ ███    ██ ████████  █████  ████████ ██  ██████  ███    ██
// ██ ████  ████ ██   ██ ██      ██      ████  ████ ██      ████   ██    ██    ██   ██    ██    ██ ██    ██ ████   ██
// ██ ██ ████ ██ ██████  ██      █████   ██ ████ ██ █████   ██ ██  ██    ██    ███████    ██    ██ ██    ██ ██ ██  ██
// ██ ██  ██  ██ ██      ██      ██      ██  ██  ██ ██      ██  ██ ██    ██    ██   ██    ██    ██ ██    ██ ██  ██ ██
// ██ ██      ██ ██      ███████ ███████ ██      ██ ███████ ██   ████    ██    ██   ██    ██    ██  ██████  ██   ████
//
// >>implementation
#ifdef SOKOL_APP_IMPL
#define SOKOL_APP_IMPL_INCLUDED (1)

#if defined(SOKOL_MALLOC) || defined(SOKOL_CALLOC) || defined(SOKOL_FREE)
#error "SOKOL_MALLOC/CALLOC/FREE macros are no longer supported, please use sapp_desc.allocator to override memory allocation functions"
#endif

#include <stdlib.h> // malloc, free
#include <string.h> // memset, strncmp
#include <stddef.h> // size_t
#include <math.h>   // roundf

// helper macros
#define _sapp_def(val, def) (((val) == 0) ? (def) : (val))
#define _sapp_absf(a) (((a)<0.0f)?-(a):(a))

#define _SAPP_MAX_TITLE_LENGTH (128)
#define _SAPP_FALLBACK_DEFAULT_WINDOW_WIDTH (640)
#define _SAPP_FALLBACK_DEFAULT_WINDOW_HEIGHT (480)
// NOTE: the pixel format values *must* be compatible with sg_pixel_format
#define _SAPP_PIXELFORMAT_RGBA8 (23)
#define _SAPP_PIXELFORMAT_BGRA8 (28)
#define _SAPP_PIXELFORMAT_DEPTH (43)
#define _SAPP_PIXELFORMAT_DEPTH_STENCIL (44)

// check if the config defines are alright
#if defined(__APPLE__)
    // see https://clang.llvm.org/docs/LanguageExtensions.html#automatic-reference-counting
    #if !defined(__cplusplus)
        #if __has_feature(objc_arc) && !__has_feature(objc_arc_fields)
            #error "sokol_app.h requires __has_feature(objc_arc_field) if ARC is enabled (use a more recent compiler version)"
        #endif
    #endif
    #define _SAPP_APPLE (1)
    #include <TargetConditionals.h>
    #if defined(TARGET_OS_IPHONE) && !TARGET_OS_IPHONE
        /* MacOS */
        #define _SAPP_MACOS (1)
        #if !defined(SOKOL_METAL) && !defined(SOKOL_GLCORE)
        #error("sokol_app.h: unknown 3D API selected for MacOS, must be SOKOL_METAL or SOKOL_GLCORE")
        #endif
    #else
        /* iOS or iOS Simulator */
        #define _SAPP_IOS (1)
        #if !defined(SOKOL_METAL) && !defined(SOKOL_GLES3)
        #error("sokol_app.h: unknown 3D API selected for iOS, must be SOKOL_METAL or SOKOL_GLES3")
        #endif
    #endif
#elif defined(__EMSCRIPTEN__)
    /* emscripten (asm.js or wasm) */
    #define _SAPP_EMSCRIPTEN (1)
    #if !defined(SOKOL_GLES3) && !defined(SOKOL_WGPU)
    #error("sokol_app.h: unknown 3D API selected for emscripten, must be SOKOL_GLES3 or SOKOL_WGPU")
    #endif
#elif defined(_WIN32)
    /* Windows (D3D11 or GL) */
    #define _SAPP_WIN32 (1)
    #if !defined(SOKOL_D3D11) && !defined(SOKOL_GLCORE) && !defined(SOKOL_NOAPI)
    #error("sokol_app.h: unknown 3D API selected for Win32, must be SOKOL_D3D11, SOKOL_GLCORE or SOKOL_NOAPI")
    #endif
#elif defined(__ANDROID__)
    /* Android */
    #define _SAPP_ANDROID (1)
    #if !defined(SOKOL_GLES3)
    #error("sokol_app.h: unknown 3D API selected for Android, must be SOKOL_GLES3")
    #endif
    #if defined(SOKOL_NO_ENTRY)
    #error("sokol_app.h: SOKOL_NO_ENTRY is not supported on Android")
    #endif
#elif defined(__linux__) || defined(__unix__)
    /* Linux */
    #define _SAPP_LINUX (1)
    #if defined(SOKOL_GLCORE)
        #if !defined(SOKOL_FORCE_EGL)
            #define _SAPP_GLX (1)
        #endif
        #define GL_GLEXT_PROTOTYPES
        #include <GL/gl.h>
    #elif defined(SOKOL_GLES3)
        #include <GLES3/gl3.h>
        #include <GLES3/gl3ext.h>
    #else
        #error("sokol_app.h: unknown 3D API selected for Linux, must be SOKOL_GLCORE, SOKOL_GLES3")
    #endif
#else
#error "sokol_app.h: Unknown platform"
#endif

#if defined(SOKOL_GLCORE) || defined(SOKOL_GLES3)
    #define _SAPP_ANY_GL (1)
#endif

#ifndef SOKOL_API_IMPL
    #define SOKOL_API_IMPL
#endif
#ifndef SOKOL_DEBUG
    #ifndef NDEBUG
        #define SOKOL_DEBUG
    #endif
#endif
#ifndef SOKOL_ASSERT
    #include <assert.h>
    #define SOKOL_ASSERT(c) assert(c)
#endif
#ifndef SOKOL_UNREACHABLE
    #define SOKOL_UNREACHABLE SOKOL_ASSERT(false)
#endif

#ifndef _SOKOL_PRIVATE
    #if defined(__GNUC__) || defined(__clang__)
        #define _SOKOL_PRIVATE __attribute__((unused)) static
    #else
        #define _SOKOL_PRIVATE static
    #endif
#endif
#ifndef _SOKOL_UNUSED
    #define _SOKOL_UNUSED(x) (void)(x)
#endif

#if defined(_SAPP_APPLE)
    #if defined(SOKOL_METAL)
        #import <Metal/Metal.h>
        #import <MetalKit/MetalKit.h>
    #endif
    #if defined(_SAPP_MACOS)
        #if defined(_SAPP_ANY_GL)
            #ifndef GL_SILENCE_DEPRECATION
            #define GL_SILENCE_DEPRECATION
            #endif
            #include <Cocoa/Cocoa.h>
            #include <OpenGL/gl3.h>
        #endif
    #elif defined(_SAPP_IOS)
        #import <UIKit/UIKit.h>
        #if defined(_SAPP_ANY_GL)
            #import <GLKit/GLKit.h>
            #include <OpenGLES/ES3/gl.h>
        #endif
    #endif
    #include <AvailabilityMacros.h>
    #include <mach/mach_time.h>
#elif defined(_SAPP_EMSCRIPTEN)
    #if defined(SOKOL_WGPU)
        #include <webgpu/webgpu.h>
    #endif
    #if defined(SOKOL_GLES3)
        #include <GLES3/gl3.h>
    #endif
    #include <emscripten/emscripten.h>
    #include <emscripten/html5.h>
#elif defined(_SAPP_WIN32)
    #ifdef _MSC_VER
        #pragma warning(push)
        #pragma warning(disable:4201)   /* nonstandard extension used: nameless struct/union */
        #pragma warning(disable:4204)   /* nonstandard extension used: non-constant aggregate initializer */
        #pragma warning(disable:4054)   /* 'type cast': from function pointer */
        #pragma warning(disable:4055)   /* 'type cast': from data pointer */
        #pragma warning(disable:4505)   /* unreferenced local function has been removed */
        #pragma warning(disable:4115)   /* /W4: 'ID3D11ModuleInstance': named type definition in parentheses (in d3d11.h) */
    #endif
    #ifndef WIN32_LEAN_AND_MEAN
        #define WIN32_LEAN_AND_MEAN
    #endif
    #ifndef NOMINMAX
        #define NOMINMAX
    #endif
    #include <windows.h>
    #include <windowsx.h>
    #include <shellapi.h>
    #if !defined(SOKOL_NO_ENTRY)    // if SOKOL_NO_ENTRY is defined, it's the applications' responsibility to use the right subsystem
        #if defined(SOKOL_WIN32_FORCE_MAIN)
            #pragma comment (linker, "/subsystem:console")
        #else
            #pragma comment (linker, "/subsystem:windows")
        #endif
    #endif
    #include <stdio.h>  /* freopen_s() */
    #include <wchar.h>  /* wcslen() */

    #pragma comment (lib, "kernel32")
    #pragma comment (lib, "user32")
    #pragma comment (lib, "shell32")    /* CommandLineToArgvW, DragQueryFileW, DragFinished */
    #pragma comment (lib, "gdi32")
    #if defined(SOKOL_D3D11)
        #pragma comment (lib, "dxgi")
        #pragma comment (lib, "d3d11")
    #endif

    #if defined(SOKOL_D3D11)
        #ifndef D3D11_NO_HELPERS
            #define D3D11_NO_HELPERS
        #endif
        #include <d3d11.h>
        #include <dxgi.h>
        // DXGI_SWAP_EFFECT_FLIP_DISCARD is only defined in newer Windows SDKs, so don't depend on it
        #define _SAPP_DXGI_SWAP_EFFECT_FLIP_DISCARD (4)
    #endif
    #ifndef WM_MOUSEHWHEEL /* see https://github.com/floooh/sokol/issues/138 */
        #define WM_MOUSEHWHEEL (0x020E)
    #endif
    #ifndef WM_DPICHANGED
        #define WM_DPICHANGED (0x02E0)
    #endif
#elif defined(_SAPP_ANDROID)
    #include <pthread.h>
    #include <unistd.h>
    #include <time.h>
    #include <android/native_activity.h>
    #include <android/looper.h>
    #include <EGL/egl.h>
    #include <GLES3/gl3.h>
#elif defined(_SAPP_LINUX)
    #define GL_GLEXT_PROTOTYPES
    #include <X11/Xlib.h>
    #include <X11/Xutil.h>
    #include <X11/XKBlib.h>
    #include <X11/keysym.h>
    #include <X11/Xresource.h>
    #include <X11/Xatom.h>
    #include <X11/extensions/XInput2.h>
    #include <X11/Xcursor/Xcursor.h>
    #include <X11/cursorfont.h> /* XC_* font cursors */
    #include <X11/Xmd.h> /* CARD32 */
    #if !defined(_SAPP_GLX)
        #include <EGL/egl.h>
    #endif
    #include <dlfcn.h> /* dlopen, dlsym, dlclose */
    #include <limits.h> /* LONG_MAX */
    #include <pthread.h>    /* only used a linker-guard, search for _sapp_linux_run() and see first comment */
    #include <time.h>
#endif

#if defined(_SAPP_APPLE)
    // this is ARC compatible
    #if defined(__cplusplus)
        #define _SAPP_CLEAR_ARC_STRUCT(type, item) { item = type(); }
    #else
        #define _SAPP_CLEAR_ARC_STRUCT(type, item) { item = (type) { 0 }; }
    #endif
#else
    #define _SAPP_CLEAR_ARC_STRUCT(type, item) { _sapp_clear(&item, sizeof(item)); }
#endif


// ███████ ██████   █████  ███    ███ ███████     ████████ ██ ███    ███ ██ ███    ██  ██████
// ██      ██   ██ ██   ██ ████  ████ ██             ██    ██ ████  ████ ██ ████   ██ ██
// █████   ██████  ███████ ██ ████ ██ █████          ██    ██ ██ ████ ██ ██ ██ ██  ██ ██   ███
// ██      ██   ██ ██   ██ ██  ██  ██ ██             ██    ██ ██  ██  ██ ██ ██  ██ ██ ██    ██
// ██      ██   ██ ██   ██ ██      ██ ███████        ██    ██ ██      ██ ██ ██   ████  ██████
//
// >>frame timing
#define _SAPP_RING_NUM_SLOTS (256)
typedef struct {
    int head;
    int tail;
    double buf[_SAPP_RING_NUM_SLOTS];
} _sapp_ring_t;

_SOKOL_PRIVATE int _sapp_ring_idx(int i) {
    return i % _SAPP_RING_NUM_SLOTS;
}

_SOKOL_PRIVATE void _sapp_ring_init(_sapp_ring_t* ring) {
    ring->head = 0;
    ring->tail = 0;
}

_SOKOL_PRIVATE bool _sapp_ring_full(_sapp_ring_t* ring) {
    return _sapp_ring_idx(ring->head + 1) == ring->tail;
}

_SOKOL_PRIVATE bool _sapp_ring_empty(_sapp_ring_t* ring) {
    return ring->head == ring->tail;
}

_SOKOL_PRIVATE int _sapp_ring_count(_sapp_ring_t* ring) {
    int count;
    if (ring->head >= ring->tail) {
        count = ring->head - ring->tail;
    }
    else {
        count = (ring->head + _SAPP_RING_NUM_SLOTS) - ring->tail;
    }
    SOKOL_ASSERT((count >= 0) && (count < _SAPP_RING_NUM_SLOTS));
    return count;
}

_SOKOL_PRIVATE void _sapp_ring_enqueue(_sapp_ring_t* ring, double val) {
    SOKOL_ASSERT(!_sapp_ring_full(ring));
    ring->buf[ring->head] = val;
    ring->head = _sapp_ring_idx(ring->head + 1);
}

_SOKOL_PRIVATE double _sapp_ring_dequeue(_sapp_ring_t* ring) {
    SOKOL_ASSERT(!_sapp_ring_empty(ring));
    double val = ring->buf[ring->tail];
    ring->tail = _sapp_ring_idx(ring->tail + 1);
    return val;
}

/*
    NOTE:

    Q: Why not use CAMetalDrawable.presentedTime on macOS and iOS?
    A: The value appears to be highly unstable during the first few
    seconds, sometimes several frames are dropped in sequence, or
    switch between 120 and 60 Hz for a few frames. Simply measuring
    and averaging the frame time yielded a more stable frame duration.
    Maybe switching to CVDisplayLink would yield better results.
    Until then just measure the time.
*/
typedef struct {
    #if defined(_SAPP_APPLE)
        struct {
            mach_timebase_info_data_t timebase;
            uint64_t start;
        } mach;
    #elif defined(_SAPP_EMSCRIPTEN)
        // empty
    #elif defined(_SAPP_WIN32)
        struct {
            LARGE_INTEGER freq;
            LARGE_INTEGER start;
        } win;
    #else // Linux, Android, ...
        #ifdef CLOCK_MONOTONIC
        #define _SAPP_CLOCK_MONOTONIC CLOCK_MONOTONIC
        #else
        // on some embedded platforms, CLOCK_MONOTONIC isn't defined
        #define _SAPP_CLOCK_MONOTONIC (1)
        #endif
        struct {
            uint64_t start;
        } posix;
    #endif
} _sapp_timestamp_t;

_SOKOL_PRIVATE int64_t _sapp_int64_muldiv(int64_t value, int64_t numer, int64_t denom) {
    int64_t q = value / denom;
    int64_t r = value % denom;
    return q * numer + r * numer / denom;
}

_SOKOL_PRIVATE void _sapp_timestamp_init(_sapp_timestamp_t* ts) {
    #if defined(_SAPP_APPLE)
        mach_timebase_info(&ts->mach.timebase);
        ts->mach.start = mach_absolute_time();
    #elif defined(_SAPP_EMSCRIPTEN)
        (void)ts;
    #elif defined(_SAPP_WIN32)
        QueryPerformanceFrequency(&ts->win.freq);
        QueryPerformanceCounter(&ts->win.start);
    #else
        struct timespec tspec;
        clock_gettime(_SAPP_CLOCK_MONOTONIC, &tspec);
        ts->posix.start = (uint64_t)tspec.tv_sec*1000000000 + (uint64_t)tspec.tv_nsec;
    #endif
}

_SOKOL_PRIVATE double _sapp_timestamp_now(_sapp_timestamp_t* ts) {
    #if defined(_SAPP_APPLE)
        const uint64_t traw = mach_absolute_time() - ts->mach.start;
        const uint64_t now = (uint64_t) _sapp_int64_muldiv((int64_t)traw, (int64_t)ts->mach.timebase.numer, (int64_t)ts->mach.timebase.denom);
        return (double)now / 1000000000.0;
    #elif defined(_SAPP_EMSCRIPTEN)
        (void)ts;
        SOKOL_ASSERT(false);
        return 0.0;
    #elif defined(_SAPP_WIN32)
        LARGE_INTEGER qpc;
        QueryPerformanceCounter(&qpc);
        const uint64_t now = (uint64_t)_sapp_int64_muldiv(qpc.QuadPart - ts->win.start.QuadPart, 1000000000, ts->win.freq.QuadPart);
        return (double)now / 1000000000.0;
    #else
        struct timespec tspec;
        clock_gettime(_SAPP_CLOCK_MONOTONIC, &tspec);
        const uint64_t now = ((uint64_t)tspec.tv_sec*1000000000 + (uint64_t)tspec.tv_nsec) - ts->posix.start;
        return (double)now / 1000000000.0;
    #endif
}

typedef struct {
    double last;
    double accum;
    double avg;
    int spike_count;
    int num;
    _sapp_timestamp_t timestamp;
    _sapp_ring_t ring;
} _sapp_timing_t;

_SOKOL_PRIVATE void _sapp_timing_reset(_sapp_timing_t* t) {
    t->last = 0.0;
    t->accum = 0.0;
    t->spike_count = 0;
    t->num = 0;
    _sapp_ring_init(&t->ring);
}

_SOKOL_PRIVATE void _sapp_timing_init(_sapp_timing_t* t) {
    t->avg = 1.0 / 60.0;    // dummy value until first actual value is available
    _sapp_timing_reset(t);
    _sapp_timestamp_init(&t->timestamp);
}

_SOKOL_PRIVATE void _sapp_timing_put(_sapp_timing_t* t, double dur) {
    // arbitrary upper limit to ignore outliers (e.g. during window resizing, or debugging)
    double min_dur = 0.0;
    double max_dur = 0.1;
    // if we have enough samples for a useful average, use a much tighter 'valid window'
    if (_sapp_ring_full(&t->ring)) {
        min_dur = t->avg * 0.8;
        max_dur = t->avg * 1.2;
    }
    if ((dur < min_dur) || (dur > max_dur)) {
        t->spike_count++;
        // if there have been many spikes in a row, the display refresh rate
        // might have changed, so a timing reset is needed
        if (t->spike_count > 20) {
            _sapp_timing_reset(t);
        }
        return;
    }
    if (_sapp_ring_full(&t->ring)) {
        double old_val = _sapp_ring_dequeue(&t->ring);
        t->accum -= old_val;
        t->num -= 1;
    }
    _sapp_ring_enqueue(&t->ring, dur);
    t->accum += dur;
    t->num += 1;
    SOKOL_ASSERT(t->num > 0);
    t->avg = t->accum / t->num;
    t->spike_count = 0;
}

_SOKOL_PRIVATE void _sapp_timing_discontinuity(_sapp_timing_t* t) {
    t->last = 0.0;
}

_SOKOL_PRIVATE void _sapp_timing_measure(_sapp_timing_t* t) {
    const double now = _sapp_timestamp_now(&t->timestamp);
    if (t->last > 0.0) {
        double dur = now - t->last;
        _sapp_timing_put(t, dur);
    }
    t->last = now;
}

_SOKOL_PRIVATE void _sapp_timing_external(_sapp_timing_t* t, double now) {
    if (t->last > 0.0) {
        double dur = now - t->last;
        _sapp_timing_put(t, dur);
    }
    t->last = now;
}

_SOKOL_PRIVATE double _sapp_timing_get_avg(_sapp_timing_t* t) {
    return t->avg;
}

// ███████ ████████ ██████  ██    ██  ██████ ████████ ███████
// ██         ██    ██   ██ ██    ██ ██         ██    ██
// ███████    ██    ██████  ██    ██ ██         ██    ███████
//      ██    ██    ██   ██ ██    ██ ██         ██         ██
// ███████    ██    ██   ██  ██████   ██████    ██    ███████
//
// >> structs
#if defined(_SAPP_MACOS)


// __v_ start
@interface SokolWindow : NSWindow {
}
@end
@interface MyView2 : NSView
@end

MyView2* g_view;

// A custom NSWindow interface to handle events in borderless windows.
@implementation SokolWindow
- (BOOL)canBecomeKeyWindow { return YES; } // needed for NSWindowStyleMaskBorderless
- (BOOL)canBecomeMainWindow { return YES; }
@end
// __v_ end


@interface _sapp_macos_app_delegate : NSObject<NSApplicationDelegate>
@end
@interface _sapp_macos_window : NSWindow
@end
@interface _sapp_macos_window_delegate : NSObject<NSWindowDelegate>
@end
#if defined(SOKOL_METAL)
    @interface _sapp_macos_view : MTKView
    @end
#elif defined(SOKOL_GLCORE)
    @interface _sapp_macos_view : NSOpenGLView
    - (void)timerFired:(id)sender;
    @end
#endif // SOKOL_GLCORE

typedef struct {
    uint32_t flags_changed_store;
    uint8_t mouse_buttons;
    // __v_ start
    //NSWindow* window; // __v_ removed
    _sapp_macos_window* window; // __v_ added
    // __v_ end
    NSTrackingArea* tracking_area;
    id keyup_monitor;
    _sapp_macos_app_delegate* app_dlg;
    _sapp_macos_window_delegate* win_dlg;
    _sapp_macos_view* view;
    NSCursor* cursors[_SAPP_MOUSECURSOR_NUM];
    #if defined(SOKOL_METAL)
        id<MTLDevice> mtl_device;
    #endif
} _sapp_macos_t;

#endif // _SAPP_MACOS

#if defined(_SAPP_IOS)

@interface _sapp_app_delegate : NSObject<UIApplicationDelegate>
@end
@interface _sapp_textfield_dlg : NSObject<UITextFieldDelegate>
- (void)keyboardWasShown:(NSNotification*)notif;
- (void)keyboardWillBeHidden:(NSNotification*)notif;
- (void)keyboardDidChangeFrame:(NSNotification*)notif;
@end
#if defined(SOKOL_METAL)
    @interface _sapp_ios_view : MTKView;
    @end
#else
    @interface _sapp_ios_view : GLKView
    @end
#endif

typedef struct {
    UIWindow* window;
    _sapp_ios_view* view;
    UITextField* textfield;
    _sapp_textfield_dlg* textfield_dlg;
    #if defined(SOKOL_METAL)
        UIViewController* view_ctrl;
        id<MTLDevice> mtl_device;
    #else
        GLKViewController* view_ctrl;
        EAGLContext* eagl_ctx;
    #endif
    bool suspended;
} _sapp_ios_t;

#endif // _SAPP_IOS

#if defined(_SAPP_EMSCRIPTEN)

#if defined(SOKOL_WGPU)
typedef struct {
    WGPUInstance instance;
    WGPUAdapter adapter;
    WGPUDevice device;
    WGPUTextureFormat render_format;
    WGPUSurface surface;
    WGPUSwapChain swapchain;
    WGPUTexture msaa_tex;
    WGPUTextureView msaa_view;
    WGPUTexture depth_stencil_tex;
    WGPUTextureView depth_stencil_view;
    WGPUTextureView swapchain_view;
    bool async_init_done;
} _sapp_wgpu_t;
#endif

typedef struct {
    bool mouse_lock_requested;
    uint16_t mouse_buttons;
} _sapp_emsc_t;
#endif // _SAPP_EMSCRIPTEN

#if defined(SOKOL_D3D11) && defined(_SAPP_WIN32)
typedef struct {
    ID3D11Device* device;
    ID3D11DeviceContext* device_context;
    ID3D11Texture2D* rt;
    ID3D11RenderTargetView* rtv;
    ID3D11Texture2D* msaa_rt;
    ID3D11RenderTargetView* msaa_rtv;
    ID3D11Texture2D* ds;
    ID3D11DepthStencilView* dsv;
    DXGI_SWAP_CHAIN_DESC swap_chain_desc;
    IDXGISwapChain* swap_chain;
    IDXGIDevice1* dxgi_device;
    bool use_dxgi_frame_stats;
    UINT sync_refresh_count;
} _sapp_d3d11_t;
#endif

#if defined(_SAPP_WIN32)

#ifndef DPI_ENUMS_DECLARED
typedef enum PROCESS_DPI_AWARENESS
{
    PROCESS_DPI_UNAWARE = 0,
    PROCESS_SYSTEM_DPI_AWARE = 1,
    PROCESS_PER_MONITOR_DPI_AWARE = 2
} PROCESS_DPI_AWARENESS;
typedef enum MONITOR_DPI_TYPE {
    MDT_EFFECTIVE_DPI = 0,
    MDT_ANGULAR_DPI = 1,
    MDT_RAW_DPI = 2,
    MDT_DEFAULT = MDT_EFFECTIVE_DPI
} MONITOR_DPI_TYPE;
#endif // DPI_ENUMS_DECLARED

typedef struct {
    bool aware;
    float content_scale;
    float window_scale;
    float mouse_scale;
} _sapp_win32_dpi_t;

typedef struct {
    HWND hwnd;
    HMONITOR hmonitor;
    HDC dc;
    HICON big_icon;
    HICON small_icon;
    HCURSOR cursors[_SAPP_MOUSECURSOR_NUM];
    UINT orig_codepage;
    LONG mouse_locked_x, mouse_locked_y;
    RECT stored_window_rect;    // used to restore window pos/size when toggling fullscreen => windowed
    bool is_win10_or_greater;
    bool in_create_window;
    bool iconified;
    bool mouse_tracked;
    uint8_t mouse_capture_mask;
    _sapp_win32_dpi_t dpi;
    bool raw_input_mousepos_valid;
    LONG raw_input_mousepos_x;
    LONG raw_input_mousepos_y;
    uint8_t raw_input_data[256];
} _sapp_win32_t;

#if defined(SOKOL_GLCORE)
#define WGL_NUMBER_PIXEL_FORMATS_ARB 0x2000
#define WGL_SUPPORT_OPENGL_ARB 0x2010
#define WGL_DRAW_TO_WINDOW_ARB 0x2001
#define WGL_PIXEL_TYPE_ARB 0x2013
#define WGL_TYPE_RGBA_ARB 0x202b
#define WGL_ACCELERATION_ARB 0x2003
#define WGL_NO_ACCELERATION_ARB 0x2025
#define WGL_RED_BITS_ARB 0x2015
#define WGL_GREEN_BITS_ARB 0x2017
#define WGL_BLUE_BITS_ARB 0x2019
#define WGL_ALPHA_BITS_ARB 0x201b
#define WGL_DEPTH_BITS_ARB 0x2022
#define WGL_STENCIL_BITS_ARB 0x2023
#define WGL_DOUBLE_BUFFER_ARB 0x2011
#define WGL_SAMPLES_ARB 0x2042
#define WGL_CONTEXT_DEBUG_BIT_ARB 0x00000001
#define WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB 0x00000002
#define WGL_CONTEXT_PROFILE_MASK_ARB 0x9126
#define WGL_CONTEXT_CORE_PROFILE_BIT_ARB 0x00000001
#define WGL_CONTEXT_MAJOR_VERSION_ARB 0x2091
#define WGL_CONTEXT_MINOR_VERSION_ARB 0x2092
#define WGL_CONTEXT_FLAGS_ARB 0x2094
#define ERROR_INVALID_VERSION_ARB 0x2095
#define ERROR_INVALID_PROFILE_ARB 0x2096
#define ERROR_INCOMPATIBLE_DEVICE_CONTEXTS_ARB 0x2054
typedef BOOL (WINAPI * PFNWGLSWAPINTERVALEXTPROC)(int);
typedef BOOL (WINAPI * PFNWGLGETPIXELFORMATATTRIBIVARBPROC)(HDC,int,int,UINT,const int*,int*);
typedef const char* (WINAPI * PFNWGLGETEXTENSIONSSTRINGEXTPROC)(void);
typedef const char* (WINAPI * PFNWGLGETEXTENSIONSSTRINGARBPROC)(HDC);
typedef HGLRC (WINAPI * PFNWGLCREATECONTEXTATTRIBSARBPROC)(HDC,HGLRC,const int*);
typedef HGLRC (WINAPI * PFN_wglCreateContext)(HDC);
typedef BOOL (WINAPI * PFN_wglDeleteContext)(HGLRC);
typedef PROC (WINAPI * PFN_wglGetProcAddress)(LPCSTR);
typedef HDC (WINAPI * PFN_wglGetCurrentDC)(void);
typedef BOOL (WINAPI * PFN_wglMakeCurrent)(HDC,HGLRC);

typedef struct {
    HINSTANCE opengl32;
    HGLRC gl_ctx;
    PFN_wglCreateContext CreateContext;
    PFN_wglDeleteContext DeleteContext;
    PFN_wglGetProcAddress GetProcAddress;
    PFN_wglGetCurrentDC GetCurrentDC;
    PFN_wglMakeCurrent MakeCurrent;
    PFNWGLSWAPINTERVALEXTPROC SwapIntervalEXT;
    PFNWGLGETPIXELFORMATATTRIBIVARBPROC GetPixelFormatAttribivARB;
    PFNWGLGETEXTENSIONSSTRINGEXTPROC GetExtensionsStringEXT;
    PFNWGLGETEXTENSIONSSTRINGARBPROC GetExtensionsStringARB;
    PFNWGLCREATECONTEXTATTRIBSARBPROC CreateContextAttribsARB;
    // special case glGetIntegerv
    void (WINAPI *GetIntegerv)(uint32_t pname, int32_t* data);
    bool ext_swap_control;
    bool arb_multisample;
    bool arb_pixel_format;
    bool arb_create_context;
    bool arb_create_context_profile;
    HWND msg_hwnd;
    HDC msg_dc;
} _sapp_wgl_t;
#endif // SOKOL_GLCORE

#endif // _SAPP_WIN32

#if defined(_SAPP_ANDROID)
typedef enum {
    _SOKOL_ANDROID_MSG_CREATE,
    _SOKOL_ANDROID_MSG_RESUME,
    _SOKOL_ANDROID_MSG_PAUSE,
    _SOKOL_ANDROID_MSG_FOCUS,
    _SOKOL_ANDROID_MSG_NO_FOCUS,
    _SOKOL_ANDROID_MSG_SET_NATIVE_WINDOW,
    _SOKOL_ANDROID_MSG_SET_INPUT_QUEUE,
    _SOKOL_ANDROID_MSG_DESTROY,
} _sapp_android_msg_t;

typedef struct {
    pthread_t thread;
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    int read_from_main_fd;
    int write_from_main_fd;
} _sapp_android_pt_t;

typedef struct {
    ANativeWindow* window;
    AInputQueue* input;
} _sapp_android_resources_t;

typedef struct {
    ANativeActivity* activity;
    _sapp_android_pt_t pt;
    _sapp_android_resources_t pending;
    _sapp_android_resources_t current;
    ALooper* looper;
    bool is_thread_started;
    bool is_thread_stopping;
    bool is_thread_stopped;
    bool has_created;
    bool has_resumed;
    bool has_focus;
    EGLConfig config;
    EGLDisplay display;
    EGLContext context;
    EGLSurface surface;
} _sapp_android_t;

#endif // _SAPP_ANDROID

#if defined(_SAPP_LINUX)

#define _SAPP_X11_XDND_VERSION (5)
#define _SAPP_X11_MAX_X11_KEYCODES (256)

#define GLX_VENDOR 1
#define GLX_RGBA_BIT 0x00000001
#define GLX_WINDOW_BIT 0x00000001
#define GLX_DRAWABLE_TYPE 0x8010
#define GLX_RENDER_TYPE	0x8011
#define GLX_DOUBLEBUFFER 5
#define GLX_RED_SIZE 8
#define GLX_GREEN_SIZE 9
#define GLX_BLUE_SIZE 10
#define GLX_ALPHA_SIZE 11
#define GLX_DEPTH_SIZE 12
#define GLX_STENCIL_SIZE 13
#define GLX_SAMPLES 0x186a1
#define GLX_CONTEXT_CORE_PROFILE_BIT_ARB 0x00000001
#define GLX_CONTEXT_PROFILE_MASK_ARB 0x9126
#define GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB 0x00000002
#define GLX_CONTEXT_MAJOR_VERSION_ARB 0x2091
#define GLX_CONTEXT_MINOR_VERSION_ARB 0x2092
#define GLX_CONTEXT_FLAGS_ARB 0x2094

typedef XID GLXWindow;
typedef XID GLXDrawable;
typedef struct __GLXFBConfig* GLXFBConfig;
typedef struct __GLXcontext* GLXContext;
typedef void (*__GLXextproc)(void);

typedef int (*PFNGLXGETFBCONFIGATTRIBPROC)(Display*,GLXFBConfig,int,int*);
typedef const char* (*PFNGLXGETCLIENTSTRINGPROC)(Display*,int);
typedef Bool (*PFNGLXQUERYEXTENSIONPROC)(Display*,int*,int*);
typedef Bool (*PFNGLXQUERYVERSIONPROC)(Display*,int*,int*);
typedef void (*PFNGLXDESTROYCONTEXTPROC)(Display*,GLXContext);
typedef Bool (*PFNGLXMAKECURRENTPROC)(Display*,GLXDrawable,GLXContext);
typedef void (*PFNGLXSWAPBUFFERSPROC)(Display*,GLXDrawable);
typedef const char* (*PFNGLXQUERYEXTENSIONSSTRINGPROC)(Display*,int);
typedef GLXFBConfig* (*PFNGLXGETFBCONFIGSPROC)(Display*,int,int*);
typedef __GLXextproc (* PFNGLXGETPROCADDRESSPROC)(const char *procName);
typedef void (*PFNGLXSWAPINTERVALEXTPROC)(Display*,GLXDrawable,int);
typedef XVisualInfo* (*PFNGLXGETVISUALFROMFBCONFIGPROC)(Display*,GLXFBConfig);
typedef GLXWindow (*PFNGLXCREATEWINDOWPROC)(Display*,GLXFBConfig,Window,const int*);
typedef void (*PFNGLXDESTROYWINDOWPROC)(Display*,GLXWindow);

typedef int (*PFNGLXSWAPINTERVALMESAPROC)(int);
typedef GLXContext (*PFNGLXCREATECONTEXTATTRIBSARBPROC)(Display*,GLXFBConfig,GLXContext,Bool,const int*);

typedef struct {
    bool available;
    int major_opcode;
    int event_base;
    int error_base;
    int major;
    int minor;
} _sapp_xi_t;

typedef struct {
    int version;
    Window source;
    Atom format;
    Atom XdndAware;
    Atom XdndEnter;
    Atom XdndPosition;
    Atom XdndStatus;
    Atom XdndActionCopy;
    Atom XdndDrop;
    Atom XdndFinished;
    Atom XdndSelection;
    Atom XdndTypeList;
    Atom text_uri_list;
} _sapp_xdnd_t;

typedef struct {
    uint8_t mouse_buttons;
    Display* display;
    int screen;
    Window root;
    Colormap colormap;
    Window window;
    Cursor hidden_cursor;
    Cursor cursors[_SAPP_MOUSECURSOR_NUM];
    int window_state;
    float dpi;
    unsigned char error_code;
    Atom UTF8_STRING;
    Atom WM_PROTOCOLS;
    Atom WM_DELETE_WINDOW;
    Atom WM_STATE;
    Atom NET_WM_NAME;
    Atom NET_WM_ICON_NAME;
    Atom NET_WM_ICON;
    Atom NET_WM_STATE;
    Atom NET_WM_STATE_FULLSCREEN;
    _sapp_xi_t xi;
    _sapp_xdnd_t xdnd;
    // XLib manual says keycodes are in the range [8, 255] inclusive.
    // https://tronche.com/gui/x/xlib/input/keyboard-encoding.html
    bool key_repeat[_SAPP_X11_MAX_X11_KEYCODES];
} _sapp_x11_t;

#if defined(_SAPP_GLX)

typedef struct {
    void* libgl;
    int major;
    int minor;
    int event_base;
    int error_base;
    GLXContext ctx;
    GLXWindow window;

    // GLX 1.3 functions
    PFNGLXGETFBCONFIGSPROC GetFBConfigs;
    PFNGLXGETFBCONFIGATTRIBPROC GetFBConfigAttrib;
    PFNGLXGETCLIENTSTRINGPROC GetClientString;
    PFNGLXQUERYEXTENSIONPROC QueryExtension;
    PFNGLXQUERYVERSIONPROC QueryVersion;
    PFNGLXDESTROYCONTEXTPROC DestroyContext;
    PFNGLXMAKECURRENTPROC MakeCurrent;
    PFNGLXSWAPBUFFERSPROC SwapBuffers;
    PFNGLXQUERYEXTENSIONSSTRINGPROC QueryExtensionsString;
    PFNGLXGETVISUALFROMFBCONFIGPROC GetVisualFromFBConfig;
    PFNGLXCREATEWINDOWPROC CreateWindow;
    PFNGLXDESTROYWINDOWPROC DestroyWindow;

    // GLX 1.4 and extension functions
    PFNGLXGETPROCADDRESSPROC GetProcAddress;
    PFNGLXGETPROCADDRESSPROC GetProcAddressARB;
    PFNGLXSWAPINTERVALEXTPROC SwapIntervalEXT;
    PFNGLXSWAPINTERVALMESAPROC SwapIntervalMESA;
    PFNGLXCREATECONTEXTATTRIBSARBPROC CreateContextAttribsARB;

    // special case glGetIntegerv
    void (*GetIntegerv)(uint32_t pname, int32_t* data);

    // extension availability
    bool EXT_swap_control;
    bool MESA_swap_control;
    bool ARB_multisample;
    bool ARB_create_context;
    bool ARB_create_context_profile;
} _sapp_glx_t;

#else

typedef struct {
    EGLDisplay display;
    EGLContext context;
    EGLSurface surface;
} _sapp_egl_t;

#endif // _SAPP_GLX
#endif // _SAPP_LINUX

#if defined(_SAPP_ANY_GL)
typedef struct {
    uint32_t framebuffer;
} _sapp_gl_t;
#endif

typedef struct {
    bool enabled;
    int buf_size;
    char* buffer;
} _sapp_clipboard_t;

typedef struct {
    bool enabled;
    int max_files;
    int max_path_length;
    int num_files;
    int buf_size;
    char* buffer;
} _sapp_drop_t;

typedef struct {
    float x, y;
    float dx, dy;
    bool shown;
    bool locked;
    bool pos_valid;
    sapp_mouse_cursor current_cursor;
} _sapp_mouse_t;

typedef struct {
    sapp_desc desc;
    bool valid;
    bool fullscreen;
    bool first_frame;
    bool init_called;
    bool cleanup_called;
    bool quit_requested;
    bool quit_ordered;
    bool event_consumed;
    bool html5_ask_leave_site;
    bool onscreen_keyboard_shown;
    int window_width;
    int window_height;
    int framebuffer_width;
    int framebuffer_height;
    int sample_count;
    int swap_interval;
    float dpi_scale;
    uint64_t frame_count;
    _sapp_timing_t timing;
    sapp_event event;
    _sapp_mouse_t mouse;
    _sapp_clipboard_t clipboard;
    _sapp_drop_t drop;
    sapp_icon_desc default_icon_desc;
    uint32_t* default_icon_pixels;
    #if defined(_SAPP_MACOS)
        _sapp_macos_t macos;
    #elif defined(_SAPP_IOS)
        _sapp_ios_t ios;
    #elif defined(_SAPP_EMSCRIPTEN)
        _sapp_emsc_t emsc;
        #if defined(SOKOL_WGPU)
            _sapp_wgpu_t wgpu;
        #endif
    #elif defined(_SAPP_WIN32)
        _sapp_win32_t win32;
        #if defined(SOKOL_D3D11)
            _sapp_d3d11_t d3d11;
        #elif defined(SOKOL_GLCORE)
            _sapp_wgl_t wgl;
        #endif
    #elif defined(_SAPP_ANDROID)
        _sapp_android_t android;
    #elif defined(_SAPP_LINUX)
        _sapp_x11_t x11;
        #if defined(_SAPP_GLX)
            _sapp_glx_t glx;
        #else
            _sapp_egl_t egl;
        #endif
    #endif
    #if defined(_SAPP_ANY_GL)
        _sapp_gl_t gl;
    #endif
    char html5_canvas_selector[_SAPP_MAX_TITLE_LENGTH];
    char window_title[_SAPP_MAX_TITLE_LENGTH];      // UTF-8
    wchar_t window_title_wide[_SAPP_MAX_TITLE_LENGTH];   // UTF-32 or UCS-2 */
    sapp_keycode keycodes[SAPP_MAX_KEYCODES];
    // __v_ start
    bool __v_native_render;             /* V patch to allow for native rendering */
    // __v_ end
} _sapp_t;
static _sapp_t _sapp;

// ██       ██████   ██████   ██████  ██ ███    ██  ██████
// ██      ██    ██ ██       ██       ██ ████   ██ ██
// ██      ██    ██ ██   ███ ██   ███ ██ ██ ██  ██ ██   ███
// ██      ██    ██ ██    ██ ██    ██ ██ ██  ██ ██ ██    ██
// ███████  ██████   ██████   ██████  ██ ██   ████  ██████
//
// >>logging
#if defined(SOKOL_DEBUG)
#define _SAPP_LOGITEM_XMACRO(item,msg) #item ": " msg,
static const char* _sapp_log_messages[] = {
    _SAPP_LOG_ITEMS
};
#undef _SAPP_LOGITEM_XMACRO
#endif // SOKOL_DEBUG

#define _SAPP_PANIC(code) _sapp_log(SAPP_LOGITEM_ ##code, 0, 0, __LINE__)
#define _SAPP_ERROR(code) _sapp_log(SAPP_LOGITEM_ ##code, 1, 0, __LINE__)
#define _SAPP_WARN(code) _sapp_log(SAPP_LOGITEM_ ##code, 2, 0, __LINE__)
#define _SAPP_INFO(code) _sapp_log(SAPP_LOGITEM_ ##code, 3, 0, __LINE__)

static void _sapp_log(sapp_log_item log_item, uint32_t log_level, const char* msg, uint32_t line_nr) {
    if (_sapp.desc.logger.func) {
        const char* filename = 0;
        #if defined(SOKOL_DEBUG)
            filename = __FILE__;
            if (0 == msg) {
                msg = _sapp_log_messages[log_item];
            }
        #endif
        _sapp.desc.logger.func("sapp", log_level, log_item, msg, line_nr, filename, _sapp.desc.logger.user_data);
    }
    else {
        // for log level PANIC it would be 'undefined behaviour' to continue
        if (log_level == 0) {
            abort();
        }
    }
}

// ███    ███ ███████ ███    ███  ██████  ██████  ██    ██
// ████  ████ ██      ████  ████ ██    ██ ██   ██  ██  ██
// ██ ████ ██ █████   ██ ████ ██ ██    ██ ██████    ████
// ██  ██  ██ ██      ██  ██  ██ ██    ██ ██   ██    ██
// ██      ██ ███████ ██      ██  ██████  ██   ██    ██
//
// >>memory
_SOKOL_PRIVATE void _sapp_clear(void* ptr, size_t size) {
    SOKOL_ASSERT(ptr && (size > 0));
    memset(ptr, 0, size);
}

_SOKOL_PRIVATE void* _sapp_malloc(size_t size) {
    SOKOL_ASSERT(size > 0);
    void* ptr;
    if (_sapp.desc.allocator.alloc_fn) {
        ptr = _sapp.desc.allocator.alloc_fn(size, _sapp.desc.allocator.user_data);
    } else {
        ptr = malloc(size);
    }
    if (0 == ptr) {
        _SAPP_PANIC(MALLOC_FAILED);
    }
    return ptr;
}

_SOKOL_PRIVATE void* _sapp_malloc_clear(size_t size) {
    void* ptr = _sapp_malloc(size);
    _sapp_clear(ptr, size);
    return ptr;
}

_SOKOL_PRIVATE void _sapp_free(void* ptr) {
    if (_sapp.desc.allocator.free_fn) {
        _sapp.desc.allocator.free_fn(ptr, _sapp.desc.allocator.user_data);
    }
    else {
        free(ptr);
    }
}

// ██   ██ ███████ ██      ██████  ███████ ██████  ███████
// ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
// ███████ █████   ██      ██████  █████   ██████  ███████
// ██   ██ ██      ██      ██      ██      ██   ██      ██
// ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████
//
// >>helpers
_SOKOL_PRIVATE void _sapp_call_init(void) {
    if (_sapp.desc.init_cb) {
        _sapp.desc.init_cb();
    }
    else if (_sapp.desc.init_userdata_cb) {
        _sapp.desc.init_userdata_cb(_sapp.desc.user_data);
    }
    _sapp.init_called = true;
}

_SOKOL_PRIVATE void _sapp_call_frame(void) {
    // __v_ start
    if (_sapp.__v_native_render) {
        return;
    }
    if (_sapp.init_called && !_sapp.cleanup_called) {
        if (_sapp.desc.frame_cb) {
            _sapp.desc.frame_cb();
        }
        else if (_sapp.desc.frame_userdata_cb) {
            _sapp.desc.frame_userdata_cb(_sapp.desc.user_data);
        }
    }
}

// __v_ start
_SOKOL_PRIVATE void _sapp_call_frame_native(void) {
    //puts("_sapp_call_frame_native()");
    //printf("init called=%d cleanup_called=%d\n", _sapp.init_called,_sapp.cleanup_called);
    if (_sapp.init_called && !_sapp.cleanup_called) {
        if (_sapp.desc.frame_cb) {
            _sapp.desc.frame_cb();
        }
        else if (_sapp.desc.frame_userdata_cb) {
            _sapp.desc.frame_userdata_cb(_sapp.desc.user_data);
        }
   }
}
// __v_ end

_SOKOL_PRIVATE void _sapp_call_cleanup(void) {
    if (!_sapp.cleanup_called) {
        if (_sapp.desc.cleanup_cb) {
            _sapp.desc.cleanup_cb();
        }
        else if (_sapp.desc.cleanup_userdata_cb) {
            _sapp.desc.cleanup_userdata_cb(_sapp.desc.user_data);
        }
        _sapp.cleanup_called = true;
    }
}

_SOKOL_PRIVATE bool _sapp_call_event(const sapp_event* e) {
    if (!_sapp.cleanup_called) {
        if (_sapp.desc.event_cb) {
            _sapp.desc.event_cb(e);
        }
        else if (_sapp.desc.event_userdata_cb) {
            _sapp.desc.event_userdata_cb(e, _sapp.desc.user_data);
        }
    }
    if (_sapp.event_consumed) {
        _sapp.event_consumed = false;
        return true;
    }
    else {
        return false;
    }
}

_SOKOL_PRIVATE char* _sapp_dropped_file_path_ptr(int index) {
    SOKOL_ASSERT(_sapp.drop.buffer);
    SOKOL_ASSERT((index >= 0) && (index <= _sapp.drop.max_files));
    int offset = index * _sapp.drop.max_path_length;
    SOKOL_ASSERT(offset < _sapp.drop.buf_size);
    return &_sapp.drop.buffer[offset];
}

/* Copy a string into a fixed size buffer with guaranteed zero-
   termination.

   Return false if the string didn't fit into the buffer and had to be clamped.

   FIXME: Currently UTF-8 strings might become invalid if the string
   is clamped, because the last zero-byte might be written into
   the middle of a multi-byte sequence.
*/
_SOKOL_PRIVATE bool _sapp_strcpy(const char* src, char* dst, int max_len) {
    SOKOL_ASSERT(src && dst && (max_len > 0));
    char* const end = &(dst[max_len-1]);
    char c = 0;
    for (int i = 0; i < max_len; i++) {
        c = *src;
        if (c != 0) {
            src++;
        }
        *dst++ = c;
    }
    /* truncated? */
    if (c != 0) {
        *end = 0;
        return false;
    }
    else {
        return true;
    }
}

_SOKOL_PRIVATE sapp_desc _sapp_desc_defaults(const sapp_desc* desc) {
    SOKOL_ASSERT((desc->allocator.alloc_fn && desc->allocator.free_fn) || (!desc->allocator.alloc_fn && !desc->allocator.free_fn));
    sapp_desc res = *desc;
    res.sample_count = _sapp_def(res.sample_count, 1);
    res.swap_interval = _sapp_def(res.swap_interval, 1);
    // NOTE: can't patch the default for gl_major_version and gl_minor_version
    // independently, because a desired version 4.0 would be patched to 4.2
    // (or expressed differently: zero is a valid value for gl_minor_version
    // and can't be used to indicate 'default')
    if (0 == res.gl_major_version) {
        #if defined(_SAPP_APPLE)
            res.gl_major_version = 4;
            res.gl_minor_version = 1;
        #else
            res.gl_major_version = 4;
            res.gl_minor_version = 3;
            // __v_ start
            res.gl_minor_version = 1; // keep backwards compatibility
            // __v_ end
        #endif
    }
    res.html5_canvas_name = _sapp_def(res.html5_canvas_name, "canvas");
    res.clipboard_size = _sapp_def(res.clipboard_size, 8192);
    res.max_dropped_files = _sapp_def(res.max_dropped_files, 1);
    res.max_dropped_file_path_length = _sapp_def(res.max_dropped_file_path_length, 2048);
    res.window_title = _sapp_def(res.window_title, "sokol_app");
    return res;
}

_SOKOL_PRIVATE void _sapp_init_state(const sapp_desc* desc) {
    SOKOL_ASSERT(desc);
    SOKOL_ASSERT(desc->width >= 0);
    SOKOL_ASSERT(desc->height >= 0);
    SOKOL_ASSERT(desc->sample_count >= 0);
    SOKOL_ASSERT(desc->swap_interval >= 0);
    SOKOL_ASSERT(desc->clipboard_size >= 0);
    SOKOL_ASSERT(desc->max_dropped_files >= 0);
    SOKOL_ASSERT(desc->max_dropped_file_path_length >= 0);
    _SAPP_CLEAR_ARC_STRUCT(_sapp_t, _sapp);
    _sapp.desc = _sapp_desc_defaults(desc);
    _sapp.first_frame = true;
    // NOTE: _sapp.desc.width/height may be 0! Platform backends need to deal with this
    _sapp.window_width = _sapp.desc.width;
    _sapp.window_height = _sapp.desc.height;
    _sapp.framebuffer_width = _sapp.window_width;
    _sapp.framebuffer_height = _sapp.window_height;
    _sapp.sample_count = _sapp.desc.sample_count;
    _sapp.swap_interval = _sapp.desc.swap_interval;
    _sapp.html5_canvas_selector[0] = '#';
    _sapp_strcpy(_sapp.desc.html5_canvas_name, &_sapp.html5_canvas_selector[1], sizeof(_sapp.html5_canvas_selector) - 1);
    _sapp.desc.html5_canvas_name = &_sapp.html5_canvas_selector[1];
    _sapp.html5_ask_leave_site = _sapp.desc.html5_ask_leave_site;
    _sapp.clipboard.enabled = _sapp.desc.enable_clipboard;
    if (_sapp.clipboard.enabled) {
        _sapp.clipboard.buf_size = _sapp.desc.clipboard_size;
        _sapp.clipboard.buffer = (char*) _sapp_malloc_clear((size_t)_sapp.clipboard.buf_size);
    }
    _sapp.drop.enabled = _sapp.desc.enable_dragndrop;
    if (_sapp.drop.enabled) {
        _sapp.drop.max_files = _sapp.desc.max_dropped_files;
        _sapp.drop.max_path_length = _sapp.desc.max_dropped_file_path_length;
        _sapp.drop.buf_size = _sapp.drop.max_files * _sapp.drop.max_path_length;
        _sapp.drop.buffer = (char*) _sapp_malloc_clear((size_t)_sapp.drop.buf_size);
    }
    _sapp_strcpy(_sapp.desc.window_title, _sapp.window_title, sizeof(_sapp.window_title));
    _sapp.desc.window_title = _sapp.window_title;
    _sapp.dpi_scale = 1.0f;
    _sapp.fullscreen = _sapp.desc.fullscreen;
    _sapp.mouse.shown = true;
    _sapp_timing_init(&_sapp.timing);
    // __v_ start
    _sapp.__v_native_render = _sapp.desc.__v_native_render;
    // __v_end
}

_SOKOL_PRIVATE void _sapp_discard_state(void) {
    if (_sapp.clipboard.enabled) {
        SOKOL_ASSERT(_sapp.clipboard.buffer);
        _sapp_free((void*)_sapp.clipboard.buffer);
    }
    if (_sapp.drop.enabled) {
        SOKOL_ASSERT(_sapp.drop.buffer);
        _sapp_free((void*)_sapp.drop.buffer);
    }
    if (_sapp.default_icon_pixels) {
        _sapp_free((void*)_sapp.default_icon_pixels);
    }
    _SAPP_CLEAR_ARC_STRUCT(_sapp_t, _sapp);
}

_SOKOL_PRIVATE void _sapp_init_event(sapp_event_type type) {
    _sapp_clear(&_sapp.event, sizeof(_sapp.event));
    _sapp.event.type = type;
    _sapp.event.frame_count = _sapp.frame_count;
    _sapp.event.mouse_button = SAPP_MOUSEBUTTON_INVALID;
    _sapp.event.window_width = _sapp.window_width;
    _sapp.event.window_height = _sapp.window_height;
    _sapp.event.framebuffer_width = _sapp.framebuffer_width;
    _sapp.event.framebuffer_height = _sapp.framebuffer_height;
    _sapp.event.mouse_x = _sapp.mouse.x;
    _sapp.event.mouse_y = _sapp.mouse.y;
    _sapp.event.mouse_dx = _sapp.mouse.dx;
    _sapp.event.mouse_dy = _sapp.mouse.dy;
}

_SOKOL_PRIVATE bool _sapp_events_enabled(void) {
    /* only send events when an event callback is set, and the init function was called */
    return (_sapp.desc.event_cb || _sapp.desc.event_userdata_cb) && _sapp.init_called;
}

_SOKOL_PRIVATE sapp_keycode _sapp_translate_key(int scan_code) {
    if ((scan_code >= 0) && (scan_code < SAPP_MAX_KEYCODES)) {
        return _sapp.keycodes[scan_code];
    }
    else {
        return SAPP_KEYCODE_INVALID;
    }
}

_SOKOL_PRIVATE void _sapp_clear_drop_buffer(void) {
    if (_sapp.drop.enabled) {
        SOKOL_ASSERT(_sapp.drop.buffer);
        _sapp_clear(_sapp.drop.buffer, (size_t)_sapp.drop.buf_size);
    }
}

_SOKOL_PRIVATE void _sapp_frame(void) {
    if (_sapp.first_frame) {
        _sapp.first_frame = false;
        _sapp_call_init();
    }
    _sapp_call_frame();
    _sapp.frame_count++;
}

_SOKOL_PRIVATE bool _sapp_image_validate(const sapp_image_desc* desc) {
    SOKOL_ASSERT(desc->width > 0);
    SOKOL_ASSERT(desc->height > 0);
    SOKOL_ASSERT(desc->pixels.ptr != 0);
    SOKOL_ASSERT(desc->pixels.size > 0);
    const size_t wh_size = (size_t)(desc->width * desc->height) * sizeof(uint32_t);
    if (wh_size != desc->pixels.size) {
        _SAPP_ERROR(IMAGE_DATA_SIZE_MISMATCH);
        return false;
    }
    return true;
}

_SOKOL_PRIVATE int _sapp_image_bestmatch(const sapp_image_desc image_descs[], int num_images, int width, int height) {
    int least_diff = 0x7FFFFFFF;
    int least_index = 0;
    for (int i = 0; i < num_images; i++) {
        int diff = (image_descs[i].width * image_descs[i].height) - (width * height);
        if (diff < 0) {
            diff = -diff;
        }
        if (diff < least_diff) {
            least_diff = diff;
            least_index = i;
        }
    }
    return least_index;
}

_SOKOL_PRIVATE int _sapp_icon_num_images(const sapp_icon_desc* desc) {
    int index = 0;
    for (; index < SAPP_MAX_ICONIMAGES; index++) {
        if (0 == desc->images[index].pixels.ptr) {
            break;
        }
    }
    return index;
}

_SOKOL_PRIVATE bool _sapp_validate_icon_desc(const sapp_icon_desc* desc, int num_images) {
    SOKOL_ASSERT(num_images <= SAPP_MAX_ICONIMAGES);
    for (int i = 0; i < num_images; i++) {
        const sapp_image_desc* img_desc = &desc->images[i];
        if (!_sapp_image_validate(img_desc)) {
            return false;
        }
    }
    return true;
}

_SOKOL_PRIVATE void _sapp_setup_default_icon(void) {
    SOKOL_ASSERT(0 == _sapp.default_icon_pixels);

    const int num_icons = 3;
    const int icon_sizes[3] = { 16, 32, 64 };   // must be multiple of 8!

    // allocate a pixel buffer for all icon pixels
    int all_num_pixels = 0;
    for (int i = 0; i < num_icons; i++) {
        all_num_pixels += icon_sizes[i] * icon_sizes[i];
    }
    _sapp.default_icon_pixels = (uint32_t*) _sapp_malloc_clear((size_t)all_num_pixels * sizeof(uint32_t));

    // initialize default_icon_desc struct
    uint32_t* dst = _sapp.default_icon_pixels;
    const uint32_t* dst_end = dst + all_num_pixels;
    (void)dst_end; // silence unused warning in release mode
    for (int i = 0; i < num_icons; i++) {
        const int dim = (int) icon_sizes[i];
        const int num_pixels = dim * dim;
        sapp_image_desc* img_desc = &_sapp.default_icon_desc.images[i];
        img_desc->width = dim;
        img_desc->height = dim;
        img_desc->pixels.ptr = dst;
        img_desc->pixels.size = (size_t)num_pixels * sizeof(uint32_t);
        dst += num_pixels;
    }
    SOKOL_ASSERT(dst == dst_end);

    // Amstrad CPC font 'S'
    const uint8_t tile[8] = {
        0x3C,
        0x66,
        0x60,
        0x3C,
        0x06,
        0x66,
        0x3C,
        0x00,
    };
    // rainbow colors
    const uint32_t colors[8] = {
        0xFF4370FF,
        0xFF26A7FF,
        0xFF58EEFF,
        0xFF57E1D4,
        0xFF65CC9C,
        0xFF6ABB66,
        0xFFF5A542,
        0xFFC2577E,
    };
    dst = _sapp.default_icon_pixels;
    const uint32_t blank = 0x00FFFFFF;
    const uint32_t shadow = 0xFF000000;
    for (int i = 0; i < num_icons; i++) {
        const int dim = icon_sizes[i];
        SOKOL_ASSERT((dim % 8) == 0);
        const int scale = dim / 8;
        for (int ty = 0, y = 0; ty < 8; ty++) {
            const uint32_t color = colors[ty];
            for (int sy = 0; sy < scale; sy++, y++) {
                uint8_t bits = tile[ty];
                for (int tx = 0, x = 0; tx < 8; tx++, bits<<=1) {
                    uint32_t pixel = (0 == (bits & 0x80)) ? blank : color;
                    for (int sx = 0; sx < scale; sx++, x++) {
                        SOKOL_ASSERT(dst < dst_end);
                        *dst++ = pixel;
                    }
                }
            }
        }
    }
    SOKOL_ASSERT(dst == dst_end);

    // right shadow
    dst = _sapp.default_icon_pixels;
    for (int i = 0; i < num_icons; i++) {
        const int dim = icon_sizes[i];
        for (int y = 0; y < dim; y++) {
            uint32_t prev_color = blank;
            for (int x = 0; x < dim; x++) {
                const int dst_index = y * dim + x;
                const uint32_t cur_color = dst[dst_index];
                if ((cur_color == blank) && (prev_color != blank)) {
                    dst[dst_index] = shadow;
                }
                prev_color = cur_color;
            }
        }
        dst += dim * dim;
    }
    SOKOL_ASSERT(dst == dst_end);

    // bottom shadow
    dst = _sapp.default_icon_pixels;
    for (int i = 0; i < num_icons; i++) {
        const int dim = icon_sizes[i];
        for (int x = 0; x < dim; x++) {
            uint32_t prev_color = blank;
            for (int y = 0; y < dim; y++) {
                const int dst_index = y * dim + x;
                const uint32_t cur_color = dst[dst_index];
                if ((cur_color == blank) && (prev_color != blank)) {
                    dst[dst_index] = shadow;
                }
                prev_color = cur_color;
            }
        }
        dst += dim * dim;
    }
    SOKOL_ASSERT(dst == dst_end);
}

//  █████  ██████  ██████  ██      ███████
// ██   ██ ██   ██ ██   ██ ██      ██
// ███████ ██████  ██████  ██      █████
// ██   ██ ██      ██      ██      ██
// ██   ██ ██      ██      ███████ ███████
//
// >>apple
#if defined(_SAPP_APPLE)

#if __has_feature(objc_arc)
#define _SAPP_OBJC_RELEASE(obj) { obj = nil; }
#else
#define _SAPP_OBJC_RELEASE(obj) { [obj release]; obj = nil; }
#endif

// ███    ███  █████   ██████  ██████  ███████
// ████  ████ ██   ██ ██      ██    ██ ██
// ██ ████ ██ ███████ ██      ██    ██ ███████
// ██  ██  ██ ██   ██ ██      ██    ██      ██
// ██      ██ ██   ██  ██████  ██████  ███████
//
// >>macos
#if defined(_SAPP_MACOS)

_SOKOL_PRIVATE void _sapp_macos_init_keytable(void) {
    _sapp.keycodes[0x1D] = SAPP_KEYCODE_0;
    _sapp.keycodes[0x12] = SAPP_KEYCODE_1;
    _sapp.keycodes[0x13] = SAPP_KEYCODE_2;
    _sapp.keycodes[0x14] = SAPP_KEYCODE_3;
    _sapp.keycodes[0x15] = SAPP_KEYCODE_4;
    _sapp.keycodes[0x17] = SAPP_KEYCODE_5;
    _sapp.keycodes[0x16] = SAPP_KEYCODE_6;
    _sapp.keycodes[0x1A] = SAPP_KEYCODE_7;
    _sapp.keycodes[0x1C] = SAPP_KEYCODE_8;
    _sapp.keycodes[0x19] = SAPP_KEYCODE_9;
    _sapp.keycodes[0x00] = SAPP_KEYCODE_A;
    _sapp.keycodes[0x0B] = SAPP_KEYCODE_B;
    _sapp.keycodes[0x08] = SAPP_KEYCODE_C;
    _sapp.keycodes[0x02] = SAPP_KEYCODE_D;
    _sapp.keycodes[0x0E] = SAPP_KEYCODE_E;
    _sapp.keycodes[0x03] = SAPP_KEYCODE_F;
    _sapp.keycodes[0x05] = SAPP_KEYCODE_G;
    _sapp.keycodes[0x04] = SAPP_KEYCODE_H;
    _sapp.keycodes[0x22] = SAPP_KEYCODE_I;
    _sapp.keycodes[0x26] = SAPP_KEYCODE_J;
    _sapp.keycodes[0x28] = SAPP_KEYCODE_K;
    _sapp.keycodes[0x25] = SAPP_KEYCODE_L;
    _sapp.keycodes[0x2E] = SAPP_KEYCODE_M;
    _sapp.keycodes[0x2D] = SAPP_KEYCODE_N;
    _sapp.keycodes[0x1F] = SAPP_KEYCODE_O;
    _sapp.keycodes[0x23] = SAPP_KEYCODE_P;
    _sapp.keycodes[0x0C] = SAPP_KEYCODE_Q;
    _sapp.keycodes[0x0F] = SAPP_KEYCODE_R;
    _sapp.keycodes[0x01] = SAPP_KEYCODE_S;
    _sapp.keycodes[0x11] = SAPP_KEYCODE_T;
    _sapp.keycodes[0x20] = SAPP_KEYCODE_U;
    _sapp.keycodes[0x09] = SAPP_KEYCODE_V;
    _sapp.keycodes[0x0D] = SAPP_KEYCODE_W;
    _sapp.keycodes[0x07] = SAPP_KEYCODE_X;
    _sapp.keycodes[0x10] = SAPP_KEYCODE_Y;
    _sapp.keycodes[0x06] = SAPP_KEYCODE_Z;
    _sapp.keycodes[0x27] = SAPP_KEYCODE_APOSTROPHE;
    _sapp.keycodes[0x2A] = SAPP_KEYCODE_BACKSLASH;
    _sapp.keycodes[0x2B] = SAPP_KEYCODE_COMMA;
    _sapp.keycodes[0x18] = SAPP_KEYCODE_EQUAL;
    _sapp.keycodes[0x32] = SAPP_KEYCODE_GRAVE_ACCENT;
    _sapp.keycodes[0x21] = SAPP_KEYCODE_LEFT_BRACKET;
    _sapp.keycodes[0x1B] = SAPP_KEYCODE_MINUS;
    _sapp.keycodes[0x2F] = SAPP_KEYCODE_PERIOD;
    _sapp.keycodes[0x1E] = SAPP_KEYCODE_RIGHT_BRACKET;
    _sapp.keycodes[0x29] = SAPP_KEYCODE_SEMICOLON;
    _sapp.keycodes[0x2C] = SAPP_KEYCODE_SLASH;
    _sapp.keycodes[0x0A] = SAPP_KEYCODE_WORLD_1;
    _sapp.keycodes[0x33] = SAPP_KEYCODE_BACKSPACE;
    _sapp.keycodes[0x39] = SAPP_KEYCODE_CAPS_LOCK;
    _sapp.keycodes[0x75] = SAPP_KEYCODE_DELETE;
    _sapp.keycodes[0x7D] = SAPP_KEYCODE_DOWN;
    _sapp.keycodes[0x77] = SAPP_KEYCODE_END;
    _sapp.keycodes[0x24] = SAPP_KEYCODE_ENTER;
    _sapp.keycodes[0x35] = SAPP_KEYCODE_ESCAPE;
    _sapp.keycodes[0x7A] = SAPP_KEYCODE_F1;
    _sapp.keycodes[0x78] = SAPP_KEYCODE_F2;
    _sapp.keycodes[0x63] = SAPP_KEYCODE_F3;
    _sapp.keycodes[0x76] = SAPP_KEYCODE_F4;
    _sapp.keycodes[0x60] = SAPP_KEYCODE_F5;
    _sapp.keycodes[0x61] = SAPP_KEYCODE_F6;
    _sapp.keycodes[0x62] = SAPP_KEYCODE_F7;
    _sapp.keycodes[0x64] = SAPP_KEYCODE_F8;
    _sapp.keycodes[0x65] = SAPP_KEYCODE_F9;
    _sapp.keycodes[0x6D] = SAPP_KEYCODE_F10;
    _sapp.keycodes[0x67] = SAPP_KEYCODE_F11;
    _sapp.keycodes[0x6F] = SAPP_KEYCODE_F12;
    _sapp.keycodes[0x69] = SAPP_KEYCODE_F13;
    _sapp.keycodes[0x6B] = SAPP_KEYCODE_F14;
    _sapp.keycodes[0x71] = SAPP_KEYCODE_F15;
    _sapp.keycodes[0x6A] = SAPP_KEYCODE_F16;
    _sapp.keycodes[0x40] = SAPP_KEYCODE_F17;
    _sapp.keycodes[0x4F] = SAPP_KEYCODE_F18;
    _sapp.keycodes[0x50] = SAPP_KEYCODE_F19;
    _sapp.keycodes[0x5A] = SAPP_KEYCODE_F20;
    _sapp.keycodes[0x73] = SAPP_KEYCODE_HOME;
    _sapp.keycodes[0x72] = SAPP_KEYCODE_INSERT;
    _sapp.keycodes[0x7B] = SAPP_KEYCODE_LEFT;
    _sapp.keycodes[0x3A] = SAPP_KEYCODE_LEFT_ALT;
    _sapp.keycodes[0x3B] = SAPP_KEYCODE_LEFT_CONTROL;
    _sapp.keycodes[0x38] = SAPP_KEYCODE_LEFT_SHIFT;
    _sapp.keycodes[0x37] = SAPP_KEYCODE_LEFT_SUPER;
    _sapp.keycodes[0x6E] = SAPP_KEYCODE_MENU;
    _sapp.keycodes[0x47] = SAPP_KEYCODE_NUM_LOCK;
    _sapp.keycodes[0x79] = SAPP_KEYCODE_PAGE_DOWN;
    _sapp.keycodes[0x74] = SAPP_KEYCODE_PAGE_UP;
    _sapp.keycodes[0x7C] = SAPP_KEYCODE_RIGHT;
    _sapp.keycodes[0x3D] = SAPP_KEYCODE_RIGHT_ALT;
    _sapp.keycodes[0x3E] = SAPP_KEYCODE_RIGHT_CONTROL;
    _sapp.keycodes[0x3C] = SAPP_KEYCODE_RIGHT_SHIFT;
    _sapp.keycodes[0x36] = SAPP_KEYCODE_RIGHT_SUPER;
    _sapp.keycodes[0x31] = SAPP_KEYCODE_SPACE;
    _sapp.keycodes[0x30] = SAPP_KEYCODE_TAB;
    _sapp.keycodes[0x7E] = SAPP_KEYCODE_UP;
    _sapp.keycodes[0x52] = SAPP_KEYCODE_KP_0;
    _sapp.keycodes[0x53] = SAPP_KEYCODE_KP_1;
    _sapp.keycodes[0x54] = SAPP_KEYCODE_KP_2;
    _sapp.keycodes[0x55] = SAPP_KEYCODE_KP_3;
    _sapp.keycodes[0x56] = SAPP_KEYCODE_KP_4;
    _sapp.keycodes[0x57] = SAPP_KEYCODE_KP_5;
    _sapp.keycodes[0x58] = SAPP_KEYCODE_KP_6;
    _sapp.keycodes[0x59] = SAPP_KEYCODE_KP_7;
    _sapp.keycodes[0x5B] = SAPP_KEYCODE_KP_8;
    _sapp.keycodes[0x5C] = SAPP_KEYCODE_KP_9;
    _sapp.keycodes[0x45] = SAPP_KEYCODE_KP_ADD;
    _sapp.keycodes[0x41] = SAPP_KEYCODE_KP_DECIMAL;
    _sapp.keycodes[0x4B] = SAPP_KEYCODE_KP_DIVIDE;
    _sapp.keycodes[0x4C] = SAPP_KEYCODE_KP_ENTER;
    _sapp.keycodes[0x51] = SAPP_KEYCODE_KP_EQUAL;
    _sapp.keycodes[0x43] = SAPP_KEYCODE_KP_MULTIPLY;
    _sapp.keycodes[0x4E] = SAPP_KEYCODE_KP_SUBTRACT;
}

_SOKOL_PRIVATE void _sapp_macos_discard_state(void) {
    // NOTE: it's safe to call [release] on a nil object
    if (_sapp.macos.keyup_monitor != nil) {
        [NSEvent removeMonitor:_sapp.macos.keyup_monitor];
        // NOTE: removeMonitor also releases the object
        _sapp.macos.keyup_monitor = nil;
    }
    _SAPP_OBJC_RELEASE(_sapp.macos.tracking_area);
    _SAPP_OBJC_RELEASE(_sapp.macos.app_dlg);
    _SAPP_OBJC_RELEASE(_sapp.macos.win_dlg);
    _SAPP_OBJC_RELEASE(_sapp.macos.view);
    #if defined(SOKOL_METAL)
        _SAPP_OBJC_RELEASE(_sapp.macos.mtl_device);
    #endif
    _SAPP_OBJC_RELEASE(_sapp.macos.window);
}

// undocumented methods for creating cursors (see GLFW 3.4 and imgui_impl_osx.mm)
@interface NSCursor()
+ (id)_windowResizeNorthWestSouthEastCursor;
+ (id)_windowResizeNorthEastSouthWestCursor;
+ (id)_windowResizeNorthSouthCursor;
+ (id)_windowResizeEastWestCursor;
@end

_SOKOL_PRIVATE void _sapp_macos_init_cursors(void) {
    _sapp.macos.cursors[SAPP_MOUSECURSOR_DEFAULT] = nil; // not a bug
    _sapp.macos.cursors[SAPP_MOUSECURSOR_ARROW] = [NSCursor arrowCursor];
    _sapp.macos.cursors[SAPP_MOUSECURSOR_IBEAM] = [NSCursor IBeamCursor];
    _sapp.macos.cursors[SAPP_MOUSECURSOR_CROSSHAIR] = [NSCursor crosshairCursor];
    _sapp.macos.cursors[SAPP_MOUSECURSOR_POINTING_HAND] = [NSCursor pointingHandCursor];
    _sapp.macos.cursors[SAPP_MOUSECURSOR_RESIZE_EW] = [NSCursor respondsToSelector:@selector(_windowResizeEastWestCursor)] ? [NSCursor _windowResizeEastWestCursor] : [NSCursor resizeLeftRightCursor];
    _sapp.macos.cursors[SAPP_MOUSECURSOR_RESIZE_NS] = [NSCursor respondsToSelector:@selector(_windowResizeNorthSouthCursor)] ? [NSCursor _windowResizeNorthSouthCursor] : [NSCursor resizeUpDownCursor];
    _sapp.macos.cursors[SAPP_MOUSECURSOR_RESIZE_NWSE] = [NSCursor respondsToSelector:@selector(_windowResizeNorthWestSouthEastCursor)] ? [NSCursor _windowResizeNorthWestSouthEastCursor] : [NSCursor closedHandCursor];
    _sapp.macos.cursors[SAPP_MOUSECURSOR_RESIZE_NESW] = [NSCursor respondsToSelector:@selector(_windowResizeNorthEastSouthWestCursor)] ? [NSCursor _windowResizeNorthEastSouthWestCursor] : [NSCursor closedHandCursor];
    _sapp.macos.cursors[SAPP_MOUSECURSOR_RESIZE_ALL] = [NSCursor closedHandCursor];
    _sapp.macos.cursors[SAPP_MOUSECURSOR_NOT_ALLOWED] = [NSCursor operationNotAllowedCursor];
}

_SOKOL_PRIVATE void _sapp_macos_run(const sapp_desc* desc) {
    _sapp_init_state(desc);
    _sapp_macos_init_keytable();
    [NSApplication sharedApplication];

    // set the application dock icon as early as possible, otherwise
    // the dummy icon will be visible for a short time
    sapp_set_icon(&_sapp.desc.icon);
    _sapp.macos.app_dlg = [[_sapp_macos_app_delegate alloc] init];
    NSApp.delegate = _sapp.macos.app_dlg;

    // workaround for "no key-up sent while Cmd is pressed" taken from GLFW:
    NSEvent* (^keyup_monitor)(NSEvent*) = ^NSEvent* (NSEvent* event) {
        if ([event modifierFlags] & NSEventModifierFlagCommand) {
            [[NSApp keyWindow] sendEvent:event];
        }
        return event;
    };
    _sapp.macos.keyup_monitor = [NSEvent addLocalMonitorForEventsMatchingMask:NSEventMaskKeyUp handler:keyup_monitor];

    [NSApp run];
    // NOTE: [NSApp run] never returns, instead cleanup code
    // must be put into applicationWillTerminate
}

/* MacOS entry function */
#if !defined(SOKOL_NO_ENTRY)
int main(int argc, char* argv[]) {
    sapp_desc desc = sokol_main(argc, argv);
    _sapp_macos_run(&desc);
    return 0;
}
#endif /* SOKOL_NO_ENTRY */

_SOKOL_PRIVATE uint32_t _sapp_macos_mods(NSEvent* ev) {
    const NSEventModifierFlags f = (ev == nil) ? NSEvent.modifierFlags : ev.modifierFlags;
    const NSUInteger b = NSEvent.pressedMouseButtons;
    uint32_t m = 0;
    if (f & NSEventModifierFlagShift) {
        m |= SAPP_MODIFIER_SHIFT;
    }
    if (f & NSEventModifierFlagControl) {
        m |= SAPP_MODIFIER_CTRL;
    }
    if (f & NSEventModifierFlagOption) {
        m |= SAPP_MODIFIER_ALT;
    }
    if (f & NSEventModifierFlagCommand) {
        m |= SAPP_MODIFIER_SUPER;
    }
    if (0 != (b & (1<<0))) {
        m |= SAPP_MODIFIER_LMB;
    }
    if (0 != (b & (1<<1))) {
        m |= SAPP_MODIFIER_RMB;
    }
    if (0 != (b & (1<<2))) {
        m |= SAPP_MODIFIER_MMB;
    }
    return m;
}

_SOKOL_PRIVATE void _sapp_macos_mouse_event(sapp_event_type type, sapp_mousebutton btn, uint32_t mod) {
    if (_sapp_events_enabled()) {
        _sapp_init_event(type);
        _sapp.event.mouse_button = btn;
        _sapp.event.modifiers = mod;
        _sapp_call_event(&_sapp.event);
    }
}

_SOKOL_PRIVATE void _sapp_macos_key_event(sapp_event_type type, sapp_keycode key, bool repeat, uint32_t mod) {
    if (_sapp_events_enabled()) {
        _sapp_init_event(type);
        _sapp.event.key_code = key;
        _sapp.event.key_repeat = repeat;
        _sapp.event.modifiers = mod;
        _sapp_call_event(&_sapp.event);
    }
}

_SOKOL_PRIVATE void _sapp_macos_app_event(sapp_event_type type) {
    if (_sapp_events_enabled()) {
        _sapp_init_event(type);
        _sapp_call_event(&_sapp.event);
    }
}

/* NOTE: unlike the iOS version of this function, the macOS version
    can dynamically update the DPI scaling factor when a window is moved
    between HighDPI / LowDPI screens.
*/
_SOKOL_PRIVATE void _sapp_macos_update_dimensions(void) {
    if (_sapp.desc.high_dpi) {
        _sapp.dpi_scale = [_sapp.macos.window screen].backingScaleFactor;
    }
    else {
        _sapp.dpi_scale = 1.0f;
    }
    _sapp.macos.view.layer.contentsScale = _sapp.dpi_scale; // NOTE: needed because we set layerContentsPlacement to a non-scaling value in windowWillStartLiveResize.
    const NSRect bounds = [_sapp.macos.view bounds];
    _sapp.window_width = (int)roundf(bounds.size.width);
    _sapp.window_height = (int)roundf(bounds.size.height);
    #if defined(SOKOL_METAL)
        _sapp.framebuffer_width = (int)roundf(bounds.size.width * _sapp.dpi_scale);
        _sapp.framebuffer_height = (int)roundf(bounds.size.height * _sapp.dpi_scale);
        const CGSize fb_size = _sapp.macos.view.drawableSize;
        const int cur_fb_width = (int)roundf(fb_size.width);
        const int cur_fb_height = (int)roundf(fb_size.height);
        const bool dim_changed = (_sapp.framebuffer_width != cur_fb_width) ||
                                 (_sapp.framebuffer_height != cur_fb_height);
    #elif defined(SOKOL_GLCORE)
        const int cur_fb_width = (int)roundf(bounds.size.width * _sapp.dpi_scale);
        const int cur_fb_height = (int)roundf(bounds.size.height * _sapp.dpi_scale);
        const bool dim_changed = (_sapp.framebuffer_width != cur_fb_width) ||
                                 (_sapp.framebuffer_height != cur_fb_height);
        _sapp.framebuffer_width = cur_fb_width;
        _sapp.framebuffer_height = cur_fb_height;
    #endif
    if (_sapp.framebuffer_width == 0) {
        _sapp.framebuffer_width = 1;
    }
    if (_sapp.framebuffer_height == 0) {
        _sapp.framebuffer_height = 1;
    }
    if (_sapp.window_width == 0) {
        _sapp.window_width = 1;
    }
    if (_sapp.window_height == 0) {
        _sapp.window_height = 1;
    }
    if (dim_changed) {
        #if defined(SOKOL_METAL)
            CGSize drawable_size = { (CGFloat) _sapp.framebuffer_width, (CGFloat) _sapp.framebuffer_height };
            _sapp.macos.view.drawableSize = drawable_size;
        #else
            // nothing to do for GL?
        #endif
        if (!_sapp.first_frame) {
            _sapp_macos_app_event(SAPP_EVENTTYPE_RESIZED);
        }
    }
}

_SOKOL_PRIVATE void _sapp_macos_toggle_fullscreen(void) {
    /* NOTE: the _sapp.fullscreen flag is also notified by the
       windowDidEnterFullscreen / windowDidExitFullscreen
       event handlers
    */
    _sapp.fullscreen = !_sapp.fullscreen;
    [_sapp.macos.window toggleFullScreen:nil];
}

_SOKOL_PRIVATE void _sapp_macos_set_clipboard_string(const char* str) {
    @autoreleasepool {
        NSPasteboard* pasteboard = [NSPasteboard generalPasteboard];
        [pasteboard declareTypes:@[NSPasteboardTypeString] owner:nil];
        [pasteboard setString:@(str) forType:NSPasteboardTypeString];
    }
}

_SOKOL_PRIVATE const char* _sapp_macos_get_clipboard_string(void) {
    SOKOL_ASSERT(_sapp.clipboard.buffer);
    @autoreleasepool {
        _sapp.clipboard.buffer[0] = 0;
        NSPasteboard* pasteboard = [NSPasteboard generalPasteboard];
        if (![[pasteboard types] containsObject:NSPasteboardTypeString]) {
            return _sapp.clipboard.buffer;
        }
        NSString* str = [pasteboard stringForType:NSPasteboardTypeString];
        if (!str) {
            return _sapp.clipboard.buffer;
        }
        _sapp_strcpy([str UTF8String], _sapp.clipboard.buffer, _sapp.clipboard.buf_size);
    }
    return _sapp.clipboard.buffer;
}

_SOKOL_PRIVATE void _sapp_macos_update_window_title(void) {
    [_sapp.macos.window setTitle: [NSString stringWithUTF8String:_sapp.window_title]];
}

_SOKOL_PRIVATE void _sapp_macos_mouse_update_from_nspoint(NSPoint mouse_pos, bool clear_dxdy) {
    if (!_sapp.mouse.locked) {
        float new_x = mouse_pos.x * _sapp.dpi_scale;
        float new_y = _sapp.framebuffer_height - (mouse_pos.y * _sapp.dpi_scale) - 1;
        if (clear_dxdy) {
            _sapp.mouse.dx = 0.0f;
            _sapp.mouse.dy = 0.0f;
        }
        else if (_sapp.mouse.pos_valid) {
            // don't update dx/dy in the very first update
            _sapp.mouse.dx = new_x - _sapp.mouse.x;
            _sapp.mouse.dy = new_y - _sapp.mouse.y;
        }
        _sapp.mouse.x = new_x;
        _sapp.mouse.y = new_y;
        _sapp.mouse.pos_valid = true;
    }
}

_SOKOL_PRIVATE void _sapp_macos_mouse_update_from_nsevent(NSEvent* event, bool clear_dxdy) {
    _sapp_macos_mouse_update_from_nspoint(event.locationInWindow, clear_dxdy);
}

_SOKOL_PRIVATE void _sapp_macos_show_mouse(bool visible) {
    /* NOTE: this function is only called when the mouse visibility actually changes */
    if (visible) {
        CGDisplayShowCursor(kCGDirectMainDisplay);
    }
    else {
        CGDisplayHideCursor(kCGDirectMainDisplay);
    }
}

_SOKOL_PRIVATE void _sapp_macos_lock_mouse(bool lock) {
    if (lock == _sapp.mouse.locked) {
        return;
    }
    _sapp.mouse.dx = 0.0f;
    _sapp.mouse.dy = 0.0f;
    _sapp.mouse.locked = lock;
    /*
        NOTE that this code doesn't warp the mouse cursor to the window
        center as everybody else does it. This lead to a spike in the
        *second* mouse-moved event after the warp happened. The
        mouse centering doesn't seem to be required (mouse-moved events
        are reported correctly even when the cursor is at an edge of the screen).

        NOTE also that the hide/show of the mouse cursor should properly
        stack with calls to sapp_show_mouse()
    */
    if (_sapp.mouse.locked) {
        CGAssociateMouseAndMouseCursorPosition(NO);
        [NSCursor hide];
    }
    else {
        [NSCursor unhide];
        CGAssociateMouseAndMouseCursorPosition(YES);
    }
}

_SOKOL_PRIVATE void _sapp_macos_update_cursor(sapp_mouse_cursor cursor, bool shown) {
    // show/hide cursor only if visibility status has changed (required because show/hide stacks)
    if (shown != _sapp.mouse.shown) {
        if (shown) {
            [NSCursor unhide];
        }
        else {
            [NSCursor hide];
        }
    }
    // update cursor type
    SOKOL_ASSERT((cursor >= 0) && (cursor < _SAPP_MOUSECURSOR_NUM));
    if (_sapp.macos.cursors[cursor]) {
        [_sapp.macos.cursors[cursor] set];
    }
    else {
        [[NSCursor arrowCursor] set];
    }
}

_SOKOL_PRIVATE void _sapp_macos_set_icon(const sapp_icon_desc* icon_desc, int num_images) {
    NSDockTile* dock_tile = NSApp.dockTile;
    const int wanted_width = (int) dock_tile.size.width;
    const int wanted_height = (int) dock_tile.size.height;
    const int img_index = _sapp_image_bestmatch(icon_desc->images, num_images, wanted_width, wanted_height);
    const sapp_image_desc* img_desc = &icon_desc->images[img_index];

    CGColorSpaceRef cg_color_space = CGColorSpaceCreateDeviceRGB();
    CFDataRef cf_data = CFDataCreate(kCFAllocatorDefault, (const UInt8*)img_desc->pixels.ptr, (CFIndex)img_desc->pixels.size);
    CGDataProviderRef cg_data_provider = CGDataProviderCreateWithCFData(cf_data);
    CGImageRef cg_img = CGImageCreate(
        (size_t)img_desc->width,    // width
        (size_t)img_desc->height,   // height
        8,                          // bitsPerComponent
        32,                         // bitsPerPixel
        (size_t)img_desc->width * 4,// bytesPerRow
        cg_color_space,             // space
        kCGImageAlphaLast | kCGImageByteOrderDefault,  // bitmapInfo
        cg_data_provider,           // provider
        NULL,                       // decode
        false,                      // shouldInterpolate
        kCGRenderingIntentDefault);
    CFRelease(cf_data);
    CGDataProviderRelease(cg_data_provider);
    CGColorSpaceRelease(cg_color_space);

    NSImage* ns_image = [[NSImage alloc] initWithCGImage:cg_img size:dock_tile.size];
    dock_tile.contentView = [NSImageView imageViewWithImage:ns_image];
    [dock_tile display];
    _SAPP_OBJC_RELEASE(ns_image);
    CGImageRelease(cg_img);
}

_SOKOL_PRIVATE void _sapp_macos_frame(void) {
    _sapp_frame();
    if (_sapp.quit_requested || _sapp.quit_ordered) {
        [_sapp.macos.window performClose:nil];
    }
}

// __v_ start
#include "sokol_app2.h" // __v_
// __v_ end

@implementation _sapp_macos_app_delegate
- (void)applicationDidFinishLaunching:(NSNotification*)aNotification {
    _SOKOL_UNUSED(aNotification);
    _sapp_macos_init_cursors();
    if ((_sapp.window_width == 0) || (_sapp.window_height == 0)) {
        // use 4/5 of screen size as default size
        NSRect screen_rect = NSScreen.mainScreen.frame;
        if (_sapp.window_width == 0) {
            _sapp.window_width = (int)roundf((screen_rect.size.width * 4.0f) / 5.0f);
        }
        if (_sapp.window_height == 0) {
            _sapp.window_height = (int)roundf((screen_rect.size.height * 4.0f) / 5.0f);
        }
    }
    const NSUInteger style =
        // __v_ start
        _sapp.desc.fullscreen ? NSWindowStyleMaskBorderless : // __v_
        // __v_ end
        NSWindowStyleMaskTitled |
        NSWindowStyleMaskClosable |
        NSWindowStyleMaskMiniaturizable |
        NSWindowStyleMaskResizable;
    NSRect window_rect = NSMakeRect(0, 0, _sapp.window_width, _sapp.window_height);
    _sapp.macos.window = [[_sapp_macos_window alloc]
        initWithContentRect:window_rect
        styleMask:style
        backing:NSBackingStoreBuffered
        defer:NO];
    _sapp.macos.window.releasedWhenClosed = NO; // this is necessary for proper cleanup in applicationWillTerminate
    _sapp.macos.window.title = [NSString stringWithUTF8String:_sapp.window_title];
    _sapp.macos.window.acceptsMouseMovedEvents = YES;
    _sapp.macos.window.restorable = YES;

    // __v_ start
   // Set min width/height
    [_sapp.macos.window setMinSize:NSMakeSize(_sapp.desc.min_width, _sapp.desc.min_height)];
          // Quit menu
       NSMenu* menu_bar = [[NSMenu alloc] init];
		NSMenuItem* app_menu_item = [[NSMenuItem alloc] init];
		[menu_bar addItem:app_menu_item];
		NSApp.mainMenu = menu_bar;
		NSMenu* app_menu = [[NSMenu alloc] init];
		NSString* window_title_as_nsstring = [NSString stringWithUTF8String:_sapp.window_title];
		// `quit_title` memory will be owned by the NSMenuItem, so no need to release it ourselves
		NSString* quit_title =  [@"Quit " stringByAppendingString:window_title_as_nsstring];
		NSMenuItem* quit_menu_item = [[NSMenuItem alloc]
		       initWithTitle:quit_title
		       action:@selector(terminate:)
		       keyEquivalent:@"q"];
		[app_menu addItem:quit_menu_item];
		// Paste menu
		/*
NSMenuItem *paste_menu_item = [[NSMenuItem alloc] initWithTitle:@"Paste" action:@selector(paste:)
keyEquivalent:@"v"];
//[paste_menu_item setKeyEquivalentModifierMask:NSEventModifierFlagCommand];
//[paste_menu_item setTarget:self];
 [paste_menu_item setEnabled:true];
[app_menu addItem:paste_menu_item];
*/

		app_menu_item.submenu = app_menu;
		_SAPP_OBJC_RELEASE( window_title_as_nsstring );
		_SAPP_OBJC_RELEASE( app_menu );
		_SAPP_OBJC_RELEASE( app_menu_item );
		_SAPP_OBJC_RELEASE( menu_bar );
    // __v_ end

    _sapp.macos.win_dlg = [[_sapp_macos_window_delegate alloc] init];
    _sapp.macos.window.delegate = _sapp.macos.win_dlg;
    #if defined(SOKOL_METAL)
        NSInteger max_fps = 60;
        #if (__MAC_OS_X_VERSION_MAX_ALLOWED >= 120000)
        if (@available(macOS 12.0, *)) {
            max_fps = [NSScreen.mainScreen maximumFramesPerSecond];
        }
        #endif
        _sapp.macos.mtl_device = MTLCreateSystemDefaultDevice();
        _sapp.macos.view = [[_sapp_macos_view alloc] init];
        [_sapp.macos.view updateTrackingAreas];
        _sapp.macos.view.preferredFramesPerSecond = max_fps / _sapp.swap_interval;
        _sapp.macos.view.device = _sapp.macos.mtl_device;
        _sapp.macos.view.colorPixelFormat = MTLPixelFormatBGRA8Unorm;
        _sapp.macos.view.depthStencilPixelFormat = MTLPixelFormatDepth32Float_Stencil8;
        _sapp.macos.view.sampleCount = (NSUInteger) _sapp.sample_count;
        _sapp.macos.view.autoResizeDrawable = false;
        _sapp.macos.window.contentView = _sapp.macos.view;
        [_sapp.macos.window makeFirstResponder:_sapp.macos.view];
        _sapp.macos.view.layer.magnificationFilter = kCAFilterNearest;
    #elif defined(SOKOL_GLCORE)
        NSOpenGLPixelFormatAttribute attrs[32];
        int i = 0;
        attrs[i++] = NSOpenGLPFAAccelerated;
        attrs[i++] = NSOpenGLPFADoubleBuffer;
        attrs[i++] = NSOpenGLPFAOpenGLProfile;
        const int glVersion = _sapp.desc.gl_major_version * 10 + _sapp.desc.gl_minor_version;
        switch(glVersion) {
            case 10: attrs[i++] = NSOpenGLProfileVersionLegacy;  break;
            case 32: attrs[i++] = NSOpenGLProfileVersion3_2Core; break;
            case 41: attrs[i++] = NSOpenGLProfileVersion4_1Core; break;
            default:
                _SAPP_PANIC(MACOS_INVALID_NSOPENGL_PROFILE);
        }
        attrs[i++] = NSOpenGLPFAColorSize; attrs[i++] = 24;
        attrs[i++] = NSOpenGLPFAAlphaSize; attrs[i++] = 8;
        attrs[i++] = NSOpenGLPFADepthSize; attrs[i++] = 24;
        attrs[i++] = NSOpenGLPFAStencilSize; attrs[i++] = 8;
        if (_sapp.sample_count > 1) {
            attrs[i++] = NSOpenGLPFAMultisample;
            attrs[i++] = NSOpenGLPFASampleBuffers; attrs[i++] = 1;
            attrs[i++] = NSOpenGLPFASamples; attrs[i++] = (NSOpenGLPixelFormatAttribute)_sapp.sample_count;
        }
        else {
            attrs[i++] = NSOpenGLPFASampleBuffers; attrs[i++] = 0;
        }
        attrs[i++] = 0;
        NSOpenGLPixelFormat* glpixelformat_obj = [[NSOpenGLPixelFormat alloc] initWithAttributes:attrs];
        SOKOL_ASSERT(glpixelformat_obj != nil);

        _sapp.macos.view = [[_sapp_macos_view alloc]
            initWithFrame:window_rect
            pixelFormat:glpixelformat_obj];
        _SAPP_OBJC_RELEASE(glpixelformat_obj);
        [_sapp.macos.view updateTrackingAreas];
        if (_sapp.desc.high_dpi) {
            [_sapp.macos.view setWantsBestResolutionOpenGLSurface:YES];
        }
        else {
            [_sapp.macos.view setWantsBestResolutionOpenGLSurface:NO];
        }

        _sapp.macos.window.contentView = _sapp.macos.view;
        [_sapp.macos.window makeFirstResponder:_sapp.macos.view];

        NSTimer* timer_obj = [NSTimer timerWithTimeInterval:0.001
            target:_sapp.macos.view
            selector:@selector(timerFired:)
            userInfo:nil
            repeats:YES];
        [[NSRunLoop currentRunLoop] addTimer:timer_obj forMode:NSDefaultRunLoopMode];
        timer_obj = nil;
    #endif
    [_sapp.macos.window center];
    _sapp.valid = true;


    // __v_ start
    if (!_sapp.__v_native_render) { // __v_

    if (_sapp.fullscreen) {
        /* ^^^ on GL, this already toggles a rendered frame, so set the valid flag before */
        [_sapp.macos.window toggleFullScreen:self];
    }

    else {
        [_sapp.macos.window center];
	}
	} // __v_
    // __v_ end


    NSApp.activationPolicy = NSApplicationActivationPolicyRegular;
    [NSApp activateIgnoringOtherApps:YES];



    // __v start
    ///////////////////////////////////////////////////////
    // Create a child view for native rendering
    if (_sapp.__v_native_render) {

        CGRect wRect = _sapp.macos.window.frame;
        NSView *contentView  =_sapp.macos.window.contentView;
        CGRect cRect = contentView.frame;

        CGRect rect = CGRectMake(wRect.origin.x, wRect.origin.y, cRect.size.width, cRect.size.height);
        NSWindow *overlayWindow = [[NSWindow alloc]initWithContentRect:rect
                                                             styleMask:NSBorderlessWindowMask
                                                               backing:NSBackingStoreBuffered
                                                                 defer:NO];
        //overlayWindow.backgroundColor = [NSColor whiteColor];

        //overlayWindow.backgroundColor = [[NSColor whiteColor] colorWithAlphaComponent:0];
        [overlayWindow setOpaque:YES];
        [_sapp.macos.window setIgnoresMouseEvents:NO];
        g_view = [[MyView2 alloc] init];
        g_view.autoresizingMask = NSViewWidthSizable | NSViewHeightSizable;
        overlayWindow.contentView = g_view;

        [   contentView addSubview:g_view];
        //[    _sapp.macos.window addChildWindow:overlayWindow ordered:NSWindowAbove];
        [_sapp.macos.window center];
    }
    //////////////////////////////////
    // __v_ end


    [_sapp.macos.window makeKeyAndOrderFront:nil];
    _sapp_macos_update_dimensions();
    [NSEvent setMouseCoalescingEnabled:NO];


    // __v_ start
/*
       NSApplicationPresentationOptions options = (NSApplicationPresentationAutoHideMenuBar |
                                                NSApplicationPresentationAutoHideDock |
                                                NSApplicationPresentationFullScreen |
                                                NSApplicationPresentationHideDock);

    [NSApp setPresentationOptions:options];
   */

    //[NSEvent setMouseCoalescingEnabled:NO];
    // __v_ end
}

- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication*)sender {
    _SOKOL_UNUSED(sender);
    return YES;
}

- (void)applicationWillTerminate:(NSNotification*)notification {
    _SOKOL_UNUSED(notification);
    _sapp_call_cleanup();
    _sapp_macos_discard_state();
    _sapp_discard_state();
}
@end

@implementation _sapp_macos_window_delegate
- (BOOL)windowShouldClose:(id)sender {
    _SOKOL_UNUSED(sender);
    /* only give user-code a chance to intervene when sapp_quit() wasn't already called */
    if (!_sapp.quit_ordered) {
        /* if window should be closed and event handling is enabled, give user code
           a chance to intervene via sapp_cancel_quit()
        */
        _sapp.quit_requested = true;
        _sapp_macos_app_event(SAPP_EVENTTYPE_QUIT_REQUESTED);
        /* user code hasn't intervened, quit the app */
        if (_sapp.quit_requested) {
            _sapp.quit_ordered = true;
        }
    }
    if (_sapp.quit_ordered) {
        return YES;
    }
    else {
        return NO;
    }
}

#if defined(SOKOL_METAL)
- (void)windowWillStartLiveResize:(NSNotification *)notification {
    // Work around the MTKView resizing glitch by "anchoring" the layer to the window corner opposite
    // to the currently manipulated corner (or edge). This prevents the content stretching back and
    // forth during resizing. This is a workaround for this issue: https://github.com/floooh/sokol/issues/700
    // Can be removed if/when migrating to CAMetalLayer: https://github.com/floooh/sokol/issues/727
    bool resizing_from_left = _sapp.mouse.x < _sapp.window_width/2;
    bool resizing_from_top = _sapp.mouse.y < _sapp.window_height/2;
    NSViewLayerContentsPlacement placement;
    if (resizing_from_left) {
        placement = resizing_from_top ? NSViewLayerContentsPlacementBottomRight : NSViewLayerContentsPlacementTopRight;
    } else {
        placement = resizing_from_top ? NSViewLayerContentsPlacementBottomLeft : NSViewLayerContentsPlacementTopLeft;
    }
    _sapp.macos.view.layerContentsPlacement = placement;
}
#endif

- (void)windowDidResize:(NSNotification*)notification {
    _SOKOL_UNUSED(notification);
    _sapp_macos_update_dimensions();
}

- (void)windowDidChangeScreen:(NSNotification*)notification {
    _SOKOL_UNUSED(notification);
    _sapp_timing_reset(&_sapp.timing);
    _sapp_macos_update_dimensions();
}

- (void)windowDidMiniaturize:(NSNotification*)notification {
    _SOKOL_UNUSED(notification);
    _sapp_macos_app_event(SAPP_EVENTTYPE_ICONIFIED);
}

- (void)windowDidDeminiaturize:(NSNotification*)notification {
    _SOKOL_UNUSED(notification);
    _sapp_macos_app_event(SAPP_EVENTTYPE_RESTORED);
}

- (void)windowDidBecomeKey:(NSNotification*)notification {
    _SOKOL_UNUSED(notification);
    _sapp_macos_app_event(SAPP_EVENTTYPE_FOCUSED);
}

- (void)windowDidResignKey:(NSNotification*)notification {
    _SOKOL_UNUSED(notification);
    _sapp_macos_app_event(SAPP_EVENTTYPE_UNFOCUSED);
}

- (void)windowDidEnterFullScreen:(NSNotification*)notification {
    _SOKOL_UNUSED(notification);
    _sapp.fullscreen = true;
}

- (void)windowDidExitFullScreen:(NSNotification*)notification {
    _SOKOL_UNUSED(notification);
    _sapp.fullscreen = false;
}
@end

@implementation _sapp_macos_window

// __v_ start
- (BOOL)canBecomeKeyWindow { return YES; } // needed for NSWindowStyleMaskBorderless
- (BOOL)canBecomeMainWindow { return YES; }
// __v_ end

- (instancetype)initWithContentRect:(NSRect)contentRect
                          styleMask:(NSWindowStyleMask)style
                            backing:(NSBackingStoreType)backingStoreType
                              defer:(BOOL)flag {
    if (self = [super initWithContentRect:contentRect styleMask:style backing:backingStoreType defer:flag]) {
        #if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101300
            [self registerForDraggedTypes:[NSArray arrayWithObject:NSPasteboardTypeFileURL]];
        #endif
    }
    return self;
}

- (NSDragOperation)draggingEntered:(id<NSDraggingInfo>)sender {
    return NSDragOperationCopy;
}

- (NSDragOperation)draggingUpdated:(id<NSDraggingInfo>)sender {
    return NSDragOperationCopy;
}

- (BOOL)performDragOperation:(id<NSDraggingInfo>)sender {
    #if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101300
    NSPasteboard *pboard = [sender draggingPasteboard];
    if ([pboard.types containsObject:NSPasteboardTypeFileURL]) {
        _sapp_clear_drop_buffer();
        _sapp.drop.num_files = ((int)pboard.pasteboardItems.count > _sapp.drop.max_files) ? _sapp.drop.max_files : (int)pboard.pasteboardItems.count;
        bool drop_failed = false;
        for (int i = 0; i < _sapp.drop.num_files; i++) {
            NSURL *fileUrl = [NSURL fileURLWithPath:[pboard.pasteboardItems[(NSUInteger)i] stringForType:NSPasteboardTypeFileURL]];
            if (!_sapp_strcpy(fileUrl.standardizedURL.path.UTF8String, _sapp_dropped_file_path_ptr(i), _sapp.drop.max_path_length)) {
                _SAPP_ERROR(DROPPED_FILE_PATH_TOO_LONG);
                drop_failed = true;
                break;
            }
        }
        if (!drop_failed) {
            if (_sapp_events_enabled()) {
                _sapp_macos_mouse_update_from_nspoint(sender.draggingLocation, true);
                _sapp_init_event(SAPP_EVENTTYPE_FILES_DROPPED);
                _sapp.event.modifiers = _sapp_macos_mods(nil);
                _sapp_call_event(&_sapp.event);
            }
        }
        else {
            _sapp_clear_drop_buffer();
            _sapp.drop.num_files = 0;
        }
        return YES;
    }
    #endif
    return NO;
}
@end

@implementation _sapp_macos_view
#if defined(SOKOL_GLCORE)
- (void)timerFired:(id)sender {
    _SOKOL_UNUSED(sender);
    [self setNeedsDisplay:YES];
}
- (void)prepareOpenGL {
    [super prepareOpenGL];
    GLint swapInt = 1;
    NSOpenGLContext* ctx = [_sapp.macos.view openGLContext];
    [ctx setValues:&swapInt forParameter:NSOpenGLContextParameterSwapInterval];
    [ctx makeCurrentContext];
}
#endif

_SOKOL_PRIVATE void _sapp_macos_poll_input_events(void) {
    /*

    NOTE: late event polling temporarily out-commented to check if this
    causes infrequent and almost impossible to reproduce problems with the
    window close events, see:
    https://github.com/floooh/sokol/pull/483#issuecomment-805148815


    const NSEventMask mask = NSEventMaskLeftMouseDown |
                             NSEventMaskLeftMouseUp|
                             NSEventMaskRightMouseDown |
                             NSEventMaskRightMouseUp |
                             NSEventMaskMouseMoved |
                             NSEventMaskLeftMouseDragged |
                             NSEventMaskRightMouseDragged |
                             NSEventMaskMouseEntered |
                             NSEventMaskMouseExited |
                             NSEventMaskKeyDown |
                             NSEventMaskKeyUp |
                             NSEventMaskCursorUpdate |
                             NSEventMaskScrollWheel |
                             NSEventMaskTabletPoint |
                             NSEventMaskTabletProximity |
                             NSEventMaskOtherMouseDown |
                             NSEventMaskOtherMouseUp |
                             NSEventMaskOtherMouseDragged |
                             NSEventMaskPressure |
                             NSEventMaskDirectTouch;
    @autoreleasepool {
        for (;;) {
            // NOTE: using NSDefaultRunLoopMode here causes stuttering in the GL backend,
            // see: https://github.com/floooh/sokol/issues/486
            NSEvent* event = [NSApp nextEventMatchingMask:mask untilDate:nil inMode:NSEventTrackingRunLoopMode dequeue:YES];
            if (event == nil) {
                break;
            }
            [NSApp sendEvent:event];
        }
    }
    */
}

- (void)drawRect:(NSRect)rect {
    _SOKOL_UNUSED(rect);
    #if defined(_SAPP_ANY_GL)
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, (GLint*)&_sapp.gl.framebuffer);
    #endif
    _sapp_timing_measure(&_sapp.timing);
    /* Catch any last-moment input events */
    _sapp_macos_poll_input_events();
    @autoreleasepool {
        _sapp_macos_frame();
    }
    #if defined(_SAPP_ANY_GL)
    [[_sapp.macos.view openGLContext] flushBuffer];
    #endif
}

- (BOOL)isOpaque {
    return YES;
}
- (BOOL)canBecomeKeyView {
    return YES;
}
- (BOOL)acceptsFirstResponder {
    return YES;
}
- (void)updateTrackingAreas {
    if (_sapp.macos.tracking_area != nil) {
        [self removeTrackingArea:_sapp.macos.tracking_area];
        _SAPP_OBJC_RELEASE(_sapp.macos.tracking_area);
    }
    const NSTrackingAreaOptions options = NSTrackingMouseEnteredAndExited |
                                          NSTrackingActiveInKeyWindow |
                                          NSTrackingEnabledDuringMouseDrag |
                                          NSTrackingCursorUpdate |
                                          NSTrackingInVisibleRect |
                                          NSTrackingAssumeInside;
    _sapp.macos.tracking_area = [[NSTrackingArea alloc] initWithRect:[self bounds] options:options owner:self userInfo:nil];
    [self addTrackingArea:_sapp.macos.tracking_area];
    [super updateTrackingAreas];
}

// helper function to make GL context active
static void _sapp_gl_make_current(void) {
    #if defined(SOKOL_GLCORE)
    [[_sapp.macos.view openGLContext] makeCurrentContext];
    #endif
}

- (void)mouseEntered:(NSEvent*)event {
    _sapp_gl_make_current();
    _sapp_macos_mouse_update_from_nsevent(event, true);
    /* don't send mouse enter/leave while dragging (so that it behaves the same as
       on Windows while SetCapture is active
    */
    if (0 == _sapp.macos.mouse_buttons) {
        _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_ENTER, SAPP_MOUSEBUTTON_INVALID, _sapp_macos_mods(event));
    }
}
- (void)mouseExited:(NSEvent*)event {
    _sapp_gl_make_current();
    _sapp_macos_mouse_update_from_nsevent(event, true);
    if (0 == _sapp.macos.mouse_buttons) {
        _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_LEAVE, SAPP_MOUSEBUTTON_INVALID, _sapp_macos_mods(event));
    }
}
- (void)mouseDown:(NSEvent*)event {
    _sapp_gl_make_current();
    _sapp_macos_mouse_update_from_nsevent(event, false);
    _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_DOWN, SAPP_MOUSEBUTTON_LEFT, _sapp_macos_mods(event));
    _sapp.macos.mouse_buttons |= (1<<SAPP_MOUSEBUTTON_LEFT);
}
- (void)mouseUp:(NSEvent*)event {
    _sapp_gl_make_current();
    _sapp_macos_mouse_update_from_nsevent(event, false);
    _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_UP, SAPP_MOUSEBUTTON_LEFT, _sapp_macos_mods(event));
    _sapp.macos.mouse_buttons &= ~(1<<SAPP_MOUSEBUTTON_LEFT);
}
- (void)rightMouseDown:(NSEvent*)event {
    _sapp_gl_make_current();
    _sapp_macos_mouse_update_from_nsevent(event, false);
    _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_DOWN, SAPP_MOUSEBUTTON_RIGHT, _sapp_macos_mods(event));
    _sapp.macos.mouse_buttons |= (1<<SAPP_MOUSEBUTTON_RIGHT);
}
- (void)rightMouseUp:(NSEvent*)event {
    _sapp_gl_make_current();
    _sapp_macos_mouse_update_from_nsevent(event, false);
    _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_UP, SAPP_MOUSEBUTTON_RIGHT, _sapp_macos_mods(event));
    _sapp.macos.mouse_buttons &= ~(1<<SAPP_MOUSEBUTTON_RIGHT);
}
- (void)otherMouseDown:(NSEvent*)event {
    _sapp_gl_make_current();
    _sapp_macos_mouse_update_from_nsevent(event, false);
    if (2 == event.buttonNumber) {
        _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_DOWN, SAPP_MOUSEBUTTON_MIDDLE, _sapp_macos_mods(event));
        _sapp.macos.mouse_buttons |= (1<<SAPP_MOUSEBUTTON_MIDDLE);
    }
}
- (void)otherMouseUp:(NSEvent*)event {
    _sapp_gl_make_current();
    _sapp_macos_mouse_update_from_nsevent(event, false);
    if (2 == event.buttonNumber) {
        _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_UP, SAPP_MOUSEBUTTON_MIDDLE, _sapp_macos_mods(event));
        _sapp.macos.mouse_buttons &= (1<<SAPP_MOUSEBUTTON_MIDDLE);
    }
}
- (void)otherMouseDragged:(NSEvent*)event {
    _sapp_gl_make_current();
    _sapp_macos_mouse_update_from_nsevent(event, false);
    if (2 == event.buttonNumber) {
        if (_sapp.mouse.locked) {
            _sapp.mouse.dx = [event deltaX];
            _sapp.mouse.dy = [event deltaY];
        }
        _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_MOVE, SAPP_MOUSEBUTTON_INVALID, _sapp_macos_mods(event));
    }
}
- (void)mouseMoved:(NSEvent*)event {
    _sapp_gl_make_current();
    _sapp_macos_mouse_update_from_nsevent(event, false);
    if (_sapp.mouse.locked) {
        _sapp.mouse.dx = [event deltaX];
        _sapp.mouse.dy = [event deltaY];
    }
    _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_MOVE, SAPP_MOUSEBUTTON_INVALID , _sapp_macos_mods(event));
}
- (void)mouseDragged:(NSEvent*)event {
    _sapp_gl_make_current();
    _sapp_macos_mouse_update_from_nsevent(event, false);
    if (_sapp.mouse.locked) {
        _sapp.mouse.dx = [event deltaX];
        _sapp.mouse.dy = [event deltaY];
    }
    _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_MOVE, SAPP_MOUSEBUTTON_INVALID , _sapp_macos_mods(event));
}
- (void)rightMouseDragged:(NSEvent*)event {
    _sapp_gl_make_current();
    _sapp_macos_mouse_update_from_nsevent(event, false);
    if (_sapp.mouse.locked) {
        _sapp.mouse.dx = [event deltaX];
        _sapp.mouse.dy = [event deltaY];
    }
    _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_MOVE, SAPP_MOUSEBUTTON_INVALID, _sapp_macos_mods(event));
}
- (void)scrollWheel:(NSEvent*)event {
    _sapp_gl_make_current();
    _sapp_macos_mouse_update_from_nsevent(event, true);
    if (_sapp_events_enabled()) {
        float dx = (float) event.scrollingDeltaX;
        float dy = (float) event.scrollingDeltaY;
        if (event.hasPreciseScrollingDeltas) {
            dx *= 0.1;
            dy *= 0.1;
        }
        if ((_sapp_absf(dx) > 0.0f) || (_sapp_absf(dy) > 0.0f)) {
            _sapp_init_event(SAPP_EVENTTYPE_MOUSE_SCROLL);
            _sapp.event.modifiers = _sapp_macos_mods(event);
            _sapp.event.scroll_x = dx;
            _sapp.event.scroll_y = dy;
            _sapp_call_event(&_sapp.event);
        }
    }
}
- (void)keyDown:(NSEvent*)event {
    if (_sapp_events_enabled()) {
        _sapp_gl_make_current();
        const uint32_t mods = _sapp_macos_mods(event);
        const sapp_keycode key_code = _sapp_translate_key(event.keyCode);
        _sapp_macos_key_event(SAPP_EVENTTYPE_KEY_DOWN, key_code, event.isARepeat, mods);
        const NSString* chars = event.characters;
        const NSUInteger len = chars.length;
        if (len > 0) {
            _sapp_init_event(SAPP_EVENTTYPE_CHAR);
            _sapp.event.modifiers = mods;
            for (NSUInteger i = 0; i < len; i++) {
                const unichar codepoint = [chars characterAtIndex:i];
                if ((codepoint & 0xFF00) == 0xF700) {
                    continue;
                }
                _sapp.event.char_code = codepoint;
                _sapp.event.key_repeat = event.isARepeat;
                _sapp_call_event(&_sapp.event);
            }
        }
        /* if this is a Cmd+V (paste), also send a CLIPBOARD_PASTE event */
        if (_sapp.clipboard.enabled && (mods == SAPP_MODIFIER_SUPER) && (key_code == SAPP_KEYCODE_V)) {
            _sapp_init_event(SAPP_EVENTTYPE_CLIPBOARD_PASTED);
            _sapp_call_event(&_sapp.event);
        }
    }
}
- (void)keyUp:(NSEvent*)event {
    _sapp_gl_make_current();
    _sapp_macos_key_event(SAPP_EVENTTYPE_KEY_UP,
        _sapp_translate_key(event.keyCode),
        event.isARepeat,
        _sapp_macos_mods(event));
}
- (void)flagsChanged:(NSEvent*)event {
    const uint32_t old_f = _sapp.macos.flags_changed_store;
    const uint32_t new_f = (uint32_t)event.modifierFlags;
    _sapp.macos.flags_changed_store = new_f;
    sapp_keycode key_code = SAPP_KEYCODE_INVALID;
    bool down = false;
    if ((new_f ^ old_f) & NSEventModifierFlagShift) {
        key_code = SAPP_KEYCODE_LEFT_SHIFT;
        down = 0 != (new_f & NSEventModifierFlagShift);
    }
    if ((new_f ^ old_f) & NSEventModifierFlagControl) {
        key_code = SAPP_KEYCODE_LEFT_CONTROL;
        down = 0 != (new_f & NSEventModifierFlagControl);
    }
    if ((new_f ^ old_f) & NSEventModifierFlagOption) {
        key_code = SAPP_KEYCODE_LEFT_ALT;
        down = 0 != (new_f & NSEventModifierFlagOption);
    }
    if ((new_f ^ old_f) & NSEventModifierFlagCommand) {
        key_code = SAPP_KEYCODE_LEFT_SUPER;
        down = 0 != (new_f & NSEventModifierFlagCommand);
    }
    if (key_code != SAPP_KEYCODE_INVALID) {
        _sapp_macos_key_event(down ? SAPP_EVENTTYPE_KEY_DOWN : SAPP_EVENTTYPE_KEY_UP,
            key_code,
            false,
            _sapp_macos_mods(event));
    }
}
@end

#endif // macOS

// ██  ██████  ███████
// ██ ██    ██ ██
// ██ ██    ██ ███████
// ██ ██    ██      ██
// ██  ██████  ███████
//
// >>ios
#if defined(_SAPP_IOS)

_SOKOL_PRIVATE void _sapp_ios_discard_state(void) {
    // NOTE: it's safe to call [release] on a nil object
    _SAPP_OBJC_RELEASE(_sapp.ios.textfield_dlg);
    _SAPP_OBJC_RELEASE(_sapp.ios.textfield);
    #if defined(SOKOL_METAL)
        _SAPP_OBJC_RELEASE(_sapp.ios.view_ctrl);
        _SAPP_OBJC_RELEASE(_sapp.ios.mtl_device);
    #else
        _SAPP_OBJC_RELEASE(_sapp.ios.view_ctrl);
        _SAPP_OBJC_RELEASE(_sapp.ios.eagl_ctx);
    #endif
    _SAPP_OBJC_RELEASE(_sapp.ios.view);
    _SAPP_OBJC_RELEASE(_sapp.ios.window);
}

_SOKOL_PRIVATE void _sapp_ios_run(const sapp_desc* desc) {
    _sapp_init_state(desc);
    static int argc = 1;
    static char* argv[] = { (char*)"sokol_app" };
    UIApplicationMain(argc, argv, nil, NSStringFromClass([_sapp_app_delegate class]));
}

/* iOS entry function */
#if !defined(SOKOL_NO_ENTRY)
int main(int argc, char* argv[]) {
    sapp_desc desc = sokol_main(argc, argv);
    _sapp_ios_run(&desc);
    return 0;
}
#endif /* SOKOL_NO_ENTRY */

_SOKOL_PRIVATE void _sapp_ios_app_event(sapp_event_type type) {
    if (_sapp_events_enabled()) {
        _sapp_init_event(type);
        _sapp_call_event(&_sapp.event);
    }
}

_SOKOL_PRIVATE void _sapp_ios_touch_event(sapp_event_type type, NSSet<UITouch *>* touches, UIEvent* event) {
    if (_sapp_events_enabled()) {
        _sapp_init_event(type);
        NSEnumerator* enumerator = event.allTouches.objectEnumerator;
        UITouch* ios_touch;
        while ((ios_touch = [enumerator nextObject])) {
            if ((_sapp.event.num_touches + 1) < SAPP_MAX_TOUCHPOINTS) {
                CGPoint ios_pos = [ios_touch locationInView:_sapp.ios.view];
                sapp_touchpoint* cur_point = &_sapp.event.touches[_sapp.event.num_touches++];
                cur_point->identifier = (uintptr_t) ios_touch;
                cur_point->pos_x = ios_pos.x * _sapp.dpi_scale;
                cur_point->pos_y = ios_pos.y * _sapp.dpi_scale;
                cur_point->changed = [touches containsObject:ios_touch];
            }
        }
        if (_sapp.event.num_touches > 0) {
            _sapp_call_event(&_sapp.event);
        }
    }
}

_SOKOL_PRIVATE void _sapp_ios_update_dimensions(void) {
    CGRect screen_rect = UIScreen.mainScreen.bounds;
    _sapp.framebuffer_width = (int)roundf(screen_rect.size.width * _sapp.dpi_scale);
    _sapp.framebuffer_height = (int)roundf(screen_rect.size.height * _sapp.dpi_scale);
    _sapp.window_width = (int)roundf(screen_rect.size.width);
    _sapp.window_height = (int)roundf(screen_rect.size.height);
    int cur_fb_width, cur_fb_height;
    #if defined(SOKOL_METAL)
        const CGSize fb_size = _sapp.ios.view.drawableSize;
        cur_fb_width = (int)roundf(fb_size.width);
        cur_fb_height = (int)roundf(fb_size.height);
    #else
        cur_fb_width = (int)roundf(_sapp.ios.view.drawableWidth);
        cur_fb_height = (int)roundf(_sapp.ios.view.drawableHeight);
    #endif
    const bool dim_changed = (_sapp.framebuffer_width != cur_fb_width) ||
                             (_sapp.framebuffer_height != cur_fb_height);
    if (dim_changed) {
        #if defined(SOKOL_METAL)
            const CGSize drawable_size = { (CGFloat) _sapp.framebuffer_width, (CGFloat) _sapp.framebuffer_height };
            _sapp.ios.view.drawableSize = drawable_size;
        #else
            // nothing to do here, GLKView correctly respects the view's contentScaleFactor
        #endif
        if (!_sapp.first_frame) {
            _sapp_ios_app_event(SAPP_EVENTTYPE_RESIZED);
        }
    }
}

_SOKOL_PRIVATE void _sapp_ios_frame(void) {
    _sapp_ios_update_dimensions();
    _sapp_frame();
}

_SOKOL_PRIVATE void _sapp_ios_show_keyboard(bool shown) {
    /* if not happened yet, create an invisible text field */
    if (nil == _sapp.ios.textfield) {
        _sapp.ios.textfield_dlg = [[_sapp_textfield_dlg alloc] init];
        _sapp.ios.textfield = [[UITextField alloc] initWithFrame:CGRectMake(10, 10, 100, 50)];
        _sapp.ios.textfield.keyboardType = UIKeyboardTypeDefault;
        _sapp.ios.textfield.returnKeyType = UIReturnKeyDefault;
        _sapp.ios.textfield.autocapitalizationType = UITextAutocapitalizationTypeNone;
        _sapp.ios.textfield.autocorrectionType = UITextAutocorrectionTypeNo;
        _sapp.ios.textfield.spellCheckingType = UITextSpellCheckingTypeNo;
        _sapp.ios.textfield.hidden = YES;
        _sapp.ios.textfield.text = @"x";
        _sapp.ios.textfield.delegate = _sapp.ios.textfield_dlg;
        [_sapp.ios.view_ctrl.view addSubview:_sapp.ios.textfield];

        [[NSNotificationCenter defaultCenter] addObserver:_sapp.ios.textfield_dlg
            selector:@selector(keyboardWasShown:)
            name:UIKeyboardDidShowNotification object:nil];
        [[NSNotificationCenter defaultCenter] addObserver:_sapp.ios.textfield_dlg
            selector:@selector(keyboardWillBeHidden:)
            name:UIKeyboardWillHideNotification object:nil];
        [[NSNotificationCenter defaultCenter] addObserver:_sapp.ios.textfield_dlg
            selector:@selector(keyboardDidChangeFrame:)
            name:UIKeyboardDidChangeFrameNotification object:nil];
    }
    if (shown) {
        /* setting the text field as first responder brings up the onscreen keyboard */
        [_sapp.ios.textfield becomeFirstResponder];
    }
    else {
        [_sapp.ios.textfield resignFirstResponder];
    }
}

@implementation _sapp_app_delegate
- (BOOL)application:(UIApplication*)application didFinishLaunchingWithOptions:(NSDictionary*)launchOptions {
    CGRect screen_rect = UIScreen.mainScreen.bounds;
    _sapp.ios.window = [[UIWindow alloc] initWithFrame:screen_rect];
    _sapp.window_width = (int)roundf(screen_rect.size.width);
    _sapp.window_height = (int)roundf(screen_rect.size.height);
    if (_sapp.desc.high_dpi) {
        _sapp.dpi_scale = (float) UIScreen.mainScreen.nativeScale;
    }
    else {
        _sapp.dpi_scale = 1.0f;
    }
    _sapp.framebuffer_width = (int)roundf(_sapp.window_width * _sapp.dpi_scale);
    _sapp.framebuffer_height = (int)roundf(_sapp.window_height * _sapp.dpi_scale);
    NSInteger max_fps = UIScreen.mainScreen.maximumFramesPerSecond;
    #if defined(SOKOL_METAL)
        _sapp.ios.mtl_device = MTLCreateSystemDefaultDevice();
        _sapp.ios.view = [[_sapp_ios_view alloc] init];
        _sapp.ios.view.preferredFramesPerSecond = max_fps / _sapp.swap_interval;
        _sapp.ios.view.device = _sapp.ios.mtl_device;
        _sapp.ios.view.colorPixelFormat = MTLPixelFormatBGRA8Unorm;
        _sapp.ios.view.depthStencilPixelFormat = MTLPixelFormatDepth32Float_Stencil8;
        _sapp.ios.view.sampleCount = (NSUInteger)_sapp.sample_count;
        /* NOTE: iOS MTKView seems to ignore thew view's contentScaleFactor
            and automatically renders at Retina resolution. We'll disable
            autoResize and instead do the resizing in _sapp_ios_update_dimensions()
        */
        _sapp.ios.view.autoResizeDrawable = false;
        _sapp.ios.view.userInteractionEnabled = YES;
        _sapp.ios.view.multipleTouchEnabled = YES;
        _sapp.ios.view_ctrl = [[UIViewController alloc] init];
        _sapp.ios.view_ctrl.modalPresentationStyle = UIModalPresentationFullScreen;
        _sapp.ios.view_ctrl.view = _sapp.ios.view;
        _sapp.ios.window.rootViewController = _sapp.ios.view_ctrl;
    #else
        _sapp.ios.eagl_ctx = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES3];
        _sapp.ios.view = [[_sapp_ios_view alloc] initWithFrame:screen_rect];
        _sapp.ios.view.drawableColorFormat = GLKViewDrawableColorFormatRGBA8888;
        _sapp.ios.view.drawableDepthFormat = GLKViewDrawableDepthFormat24;
        _sapp.ios.view.drawableStencilFormat = GLKViewDrawableStencilFormatNone;
        GLKViewDrawableMultisample msaa = _sapp.sample_count > 1 ? GLKViewDrawableMultisample4X : GLKViewDrawableMultisampleNone;
        _sapp.ios.view.drawableMultisample = msaa;
        _sapp.ios.view.context = _sapp.ios.eagl_ctx;
        _sapp.ios.view.enableSetNeedsDisplay = NO;
        _sapp.ios.view.userInteractionEnabled = YES;
        _sapp.ios.view.multipleTouchEnabled = YES;
        // on GLKView, contentScaleFactor appears to work just fine!
        if (_sapp.desc.high_dpi) {
            _sapp.ios.view.contentScaleFactor = _sapp.dpi_scale;
        }
        else {
            _sapp.ios.view.contentScaleFactor = 1.0;
        }
        _sapp.ios.view_ctrl = [[GLKViewController alloc] init];
        _sapp.ios.view_ctrl.view = _sapp.ios.view;
        _sapp.ios.view_ctrl.preferredFramesPerSecond = max_fps / _sapp.swap_interval;
        _sapp.ios.window.rootViewController = _sapp.ios.view_ctrl;
    #endif
    [_sapp.ios.window makeKeyAndVisible];

    _sapp.valid = true;
    return YES;
}

- (void)applicationWillResignActive:(UIApplication *)application {
    if (!_sapp.ios.suspended) {
        _sapp.ios.suspended = true;
        _sapp_ios_app_event(SAPP_EVENTTYPE_SUSPENDED);
    }
}

- (void)applicationDidBecomeActive:(UIApplication *)application {
    if (_sapp.ios.suspended) {
        _sapp.ios.suspended = false;
        _sapp_ios_app_event(SAPP_EVENTTYPE_RESUMED);
    }
}

/* NOTE: this method will rarely ever be called, iOS application
    which are terminated by the user are usually killed via signal 9
    by the operating system.
*/
- (void)applicationWillTerminate:(UIApplication *)application {
    _SOKOL_UNUSED(application);
    _sapp_call_cleanup();
    _sapp_ios_discard_state();
    _sapp_discard_state();
}
@end

@implementation _sapp_textfield_dlg
- (void)keyboardWasShown:(NSNotification*)notif {
    _sapp.onscreen_keyboard_shown = true;
    /* query the keyboard's size, and modify the content view's size */
    if (_sapp.desc.ios_keyboard_resizes_canvas) {
        NSDictionary* info = notif.userInfo;
        CGFloat kbd_h = [[info objectForKey:UIKeyboardFrameEndUserInfoKey] CGRectValue].size.height;
        CGRect view_frame = UIScreen.mainScreen.bounds;
        view_frame.size.height -= kbd_h;
        _sapp.ios.view.frame = view_frame;
    }
}
- (void)keyboardWillBeHidden:(NSNotification*)notif {
    _sapp.onscreen_keyboard_shown = false;
    if (_sapp.desc.ios_keyboard_resizes_canvas) {
        _sapp.ios.view.frame = UIScreen.mainScreen.bounds;
    }
}
- (void)keyboardDidChangeFrame:(NSNotification*)notif {
    /* this is for the case when the screen rotation changes while the keyboard is open */
    if (_sapp.onscreen_keyboard_shown && _sapp.desc.ios_keyboard_resizes_canvas) {
        NSDictionary* info = notif.userInfo;
        CGFloat kbd_h = [[info objectForKey:UIKeyboardFrameEndUserInfoKey] CGRectValue].size.height;
        CGRect view_frame = UIScreen.mainScreen.bounds;
        view_frame.size.height -= kbd_h;
        _sapp.ios.view.frame = view_frame;
    }
}
- (BOOL)textField:(UITextField*)textField shouldChangeCharactersInRange:(NSRange)range replacementString:(NSString*)string {
    if (_sapp_events_enabled()) {
        const NSUInteger len = string.length;
        if (len > 0) {
            for (NSUInteger i = 0; i < len; i++) {
                unichar c = [string characterAtIndex:i];
                if (c >= 32) {
                    /* ignore surrogates for now */
                    if ((c < 0xD800) || (c > 0xDFFF)) {
                        _sapp_init_event(SAPP_EVENTTYPE_CHAR);
                        _sapp.event.char_code = c;
                        _sapp_call_event(&_sapp.event);
                    }
                }
                if (c <= 32) {
                    sapp_keycode k = SAPP_KEYCODE_INVALID;
                    switch (c) {
                        case 10: k = SAPP_KEYCODE_ENTER; break;
                        case 32: k = SAPP_KEYCODE_SPACE; break;
                        default: break;
                    }
                    if (k != SAPP_KEYCODE_INVALID) {
                        _sapp_init_event(SAPP_EVENTTYPE_KEY_DOWN);
                        _sapp.event.key_code = k;
                        _sapp_call_event(&_sapp.event);
                        _sapp_init_event(SAPP_EVENTTYPE_KEY_UP);
                        _sapp.event.key_code = k;
                        _sapp_call_event(&_sapp.event);
                    }
                }
            }
        }
        else {
            /* this was a backspace */
            _sapp_init_event(SAPP_EVENTTYPE_KEY_DOWN);
            _sapp.event.key_code = SAPP_KEYCODE_BACKSPACE;
            _sapp_call_event(&_sapp.event);
            _sapp_init_event(SAPP_EVENTTYPE_KEY_UP);
            _sapp.event.key_code = SAPP_KEYCODE_BACKSPACE;
            _sapp_call_event(&_sapp.event);
        }
    }
    return NO;
}
@end

@implementation _sapp_ios_view
- (void)drawRect:(CGRect)rect {
    _SOKOL_UNUSED(rect);
    #if defined(_SAPP_ANY_GL)
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, (GLint*)&_sapp.gl.framebuffer);
    #endif
    _sapp_timing_measure(&_sapp.timing);
    @autoreleasepool {
        _sapp_ios_frame();
    }
}
- (BOOL)isOpaque {
    return YES;
}
- (void)touchesBegan:(NSSet<UITouch *> *)touches withEvent:(UIEvent*)event {
    _sapp_ios_touch_event(SAPP_EVENTTYPE_TOUCHES_BEGAN, touches, event);
}
- (void)touchesMoved:(NSSet<UITouch *> *)touches withEvent:(UIEvent*)event {
    _sapp_ios_touch_event(SAPP_EVENTTYPE_TOUCHES_MOVED, touches, event);
}
- (void)touchesEnded:(NSSet<UITouch *> *)touches withEvent:(UIEvent*)event {
    _sapp_ios_touch_event(SAPP_EVENTTYPE_TOUCHES_ENDED, touches, event);
}
- (void)touchesCancelled:(NSSet<UITouch *> *)touches withEvent:(UIEvent*)event {
    _sapp_ios_touch_event(SAPP_EVENTTYPE_TOUCHES_CANCELLED, touches, event);
}
@end
#endif /* TARGET_OS_IPHONE */

#endif /* _SAPP_APPLE */

// ███████ ███    ███ ███████  ██████ ██████  ██ ██████  ████████ ███████ ███    ██
// ██      ████  ████ ██      ██      ██   ██ ██ ██   ██    ██    ██      ████   ██
// █████   ██ ████ ██ ███████ ██      ██████  ██ ██████     ██    █████   ██ ██  ██
// ██      ██  ██  ██      ██ ██      ██   ██ ██ ██         ██    ██      ██  ██ ██
// ███████ ██      ██ ███████  ██████ ██   ██ ██ ██         ██    ███████ ██   ████
//
// >>emscripten
#if defined(_SAPP_EMSCRIPTEN)

#if defined(EM_JS_DEPS)
EM_JS_DEPS(sokol_app, "$withStackSave,$stringToUTF8OnStack");
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef void (*_sapp_html5_fetch_callback) (const sapp_html5_fetch_response*);

EMSCRIPTEN_KEEPALIVE void _sapp_emsc_onpaste(const char* str) {
    if (_sapp.clipboard.enabled) {
        _sapp_strcpy(str, _sapp.clipboard.buffer, _sapp.clipboard.buf_size);
        if (_sapp_events_enabled()) {
            _sapp_init_event(SAPP_EVENTTYPE_CLIPBOARD_PASTED);
            _sapp_call_event(&_sapp.event);
        }
    }
}

/*  https://developer.mozilla.org/en-US/docs/Web/API/WindowEventHandlers/onbeforeunload */
EMSCRIPTEN_KEEPALIVE int _sapp_html5_get_ask_leave_site(void) {
    return _sapp.html5_ask_leave_site ? 1 : 0;
}

EMSCRIPTEN_KEEPALIVE void _sapp_emsc_begin_drop(int num) {
    if (!_sapp.drop.enabled) {
        return;
    }
    if (num < 0) {
        num = 0;
    }
    if (num > _sapp.drop.max_files) {
        num = _sapp.drop.max_files;
    }
    _sapp.drop.num_files = num;
    _sapp_clear_drop_buffer();
}

EMSCRIPTEN_KEEPALIVE void _sapp_emsc_drop(int i, const char* name) {
    /* NOTE: name is only the filename part, not a path */
    if (!_sapp.drop.enabled) {
        return;
    }
    if (0 == name) {
        return;
    }
    SOKOL_ASSERT(_sapp.drop.num_files <= _sapp.drop.max_files);
    if ((i < 0) || (i >= _sapp.drop.num_files)) {
        return;
    }
    if (!_sapp_strcpy(name, _sapp_dropped_file_path_ptr(i), _sapp.drop.max_path_length)) {
        _SAPP_ERROR(DROPPED_FILE_PATH_TOO_LONG);
        _sapp.drop.num_files = 0;
    }
}

EMSCRIPTEN_KEEPALIVE void _sapp_emsc_end_drop(int x, int y, int mods) {
    if (!_sapp.drop.enabled) {
        return;
    }
    if (0 == _sapp.drop.num_files) {
        /* there was an error copying the filenames */
        _sapp_clear_drop_buffer();
        return;

    }
    if (_sapp_events_enabled()) {
        _sapp.mouse.x = (float)x * _sapp.dpi_scale;
        _sapp.mouse.y = (float)y * _sapp.dpi_scale;
        _sapp.mouse.dx = 0.0f;
        _sapp.mouse.dy = 0.0f;
        _sapp_init_event(SAPP_EVENTTYPE_FILES_DROPPED);
        // see sapp_js_add_dragndrop_listeners for mods constants
        if (mods & 1) { _sapp.event.modifiers |= SAPP_MODIFIER_SHIFT; }
        if (mods & 2) { _sapp.event.modifiers |= SAPP_MODIFIER_CTRL; }
        if (mods & 4) { _sapp.event.modifiers |= SAPP_MODIFIER_ALT; }
        if (mods & 8) { _sapp.event.modifiers |= SAPP_MODIFIER_SUPER; }
        _sapp_call_event(&_sapp.event);
    }
}

EMSCRIPTEN_KEEPALIVE void _sapp_emsc_invoke_fetch_cb(int index, int success, int error_code, _sapp_html5_fetch_callback callback, uint32_t fetched_size, void* buf_ptr, uint32_t buf_size, void* user_data) {
    sapp_html5_fetch_response response;
    _sapp_clear(&response, sizeof(response));
    response.succeeded = (0 != success);
    response.error_code = (sapp_html5_fetch_error) error_code;
    response.file_index = index;
    response.data.ptr = buf_ptr;
    response.data.size = fetched_size;
    response.buffer.ptr = buf_ptr;
    response.buffer.size = buf_size;
    response.user_data = user_data;
    callback(&response);
}

#ifdef __cplusplus
} /* extern "C" */
#endif

EM_JS(void, sapp_js_add_beforeunload_listener, (void), {
    Module.sokol_beforeunload = (event) => {
        if (__sapp_html5_get_ask_leave_site() != 0) {
            event.preventDefault();
            event.returnValue = ' ';
        }
    };
    window.addEventListener('beforeunload', Module.sokol_beforeunload);
});

EM_JS(void, sapp_js_remove_beforeunload_listener, (void), {
    window.removeEventListener('beforeunload', Module.sokol_beforeunload);
});

EM_JS(void, sapp_js_add_clipboard_listener, (void), {
    Module.sokol_paste = (event) => {
        const pasted_str = event.clipboardData.getData('text');
        withStackSave(() => {
            const cstr = stringToUTF8OnStack(pasted_str);
            __sapp_emsc_onpaste(cstr);
        });
    };
    window.addEventListener('paste', Module.sokol_paste);
});

EM_JS(void, sapp_js_remove_clipboard_listener, (void), {
    window.removeEventListener('paste', Module.sokol_paste);
});

EM_JS(void, sapp_js_write_clipboard, (const char* c_str), {
    const str = UTF8ToString(c_str);
    const ta = document.createElement('textarea');
    ta.setAttribute('autocomplete', 'off');
    ta.setAttribute('autocorrect', 'off');
    ta.setAttribute('autocapitalize', 'off');
    ta.setAttribute('spellcheck', 'false');
    ta.style.left = -100 + 'px';
    ta.style.top = -100 + 'px';
    ta.style.height = 1;
    ta.style.width = 1;
    ta.value = str;
    document.body.appendChild(ta);
    ta.select();
    document.execCommand('copy');
    document.body.removeChild(ta);
});

_SOKOL_PRIVATE void _sapp_emsc_set_clipboard_string(const char* str) {
    sapp_js_write_clipboard(str);
}

EM_JS(void, sapp_js_add_dragndrop_listeners, (const char* canvas_name_cstr), {
    Module.sokol_drop_files = [];
    const canvas_name = UTF8ToString(canvas_name_cstr);
    const canvas = document.getElementById(canvas_name);
    Module.sokol_dragenter = (event) => {
        event.stopPropagation();
        event.preventDefault();
    };
    Module.sokol_dragleave = (event) => {
        event.stopPropagation();
        event.preventDefault();
    };
    Module.sokol_dragover = (event) => {
        event.stopPropagation();
        event.preventDefault();
    };
    Module.sokol_drop = (event) => {
        event.stopPropagation();
        event.preventDefault();
        const files = event.dataTransfer.files;
        Module.sokol_dropped_files = files;
        __sapp_emsc_begin_drop(files.length);
        for (let i = 0; i < files.length; i++) {
            withStackSave(() => {
                const cstr = stringToUTF8OnStack(files[i].name);
                __sapp_emsc_drop(i, cstr);
            });
        }
        let mods = 0;
        if (event.shiftKey) { mods |= 1; }
        if (event.ctrlKey) { mods |= 2; }
        if (event.altKey) { mods |= 4; }
        if (event.metaKey) { mods |= 8; }
        // FIXME? see computation of targetX/targetY in emscripten via getClientBoundingRect
        __sapp_emsc_end_drop(event.clientX, event.clientY, mods);
    };
    canvas.addEventListener('dragenter', Module.sokol_dragenter, false);
    canvas.addEventListener('dragleave', Module.sokol_dragleave, false);
    canvas.addEventListener('dragover',  Module.sokol_dragover, false);
    canvas.addEventListener('drop',      Module.sokol_drop, false);
});

EM_JS(uint32_t, sapp_js_dropped_file_size, (int index), {
    \x2F\x2A\x2A @suppress {missingProperties} \x2A\x2F
    const files = Module.sokol_dropped_files;
    if ((index < 0) || (index >= files.length)) {
        return 0;
    }
    else {
        return files[index].size;
    }
});

EM_JS(void, sapp_js_fetch_dropped_file, (int index, _sapp_html5_fetch_callback callback, void* buf_ptr, uint32_t buf_size, void* user_data), {
    const reader = new FileReader();
    reader.onload = (loadEvent) => {
        const content = loadEvent.target.result;
        if (content.byteLength > buf_size) {
            // SAPP_HTML5_FETCH_ERROR_BUFFER_TOO_SMALL
            __sapp_emsc_invoke_fetch_cb(index, 0, 1, callback, 0, buf_ptr, buf_size, user_data);
        }
        else {
            HEAPU8.set(new Uint8Array(content), buf_ptr);
            __sapp_emsc_invoke_fetch_cb(index, 1, 0, callback, content.byteLength, buf_ptr, buf_size, user_data);
        }
    };
    reader.onerror = () => {
        // SAPP_HTML5_FETCH_ERROR_OTHER
        __sapp_emsc_invoke_fetch_cb(index, 0, 2, callback, 0, buf_ptr, buf_size, user_data);
    };
    \x2F\x2A\x2A @suppress {missingProperties} \x2A\x2F
    const files = Module.sokol_dropped_files;
    reader.readAsArrayBuffer(files[index]);
});

EM_JS(void, sapp_js_remove_dragndrop_listeners, (const char* canvas_name_cstr), {
    const canvas_name = UTF8ToString(canvas_name_cstr);
    const canvas = document.getElementById(canvas_name);
    canvas.removeEventListener('dragenter', Module.sokol_dragenter);
    canvas.removeEventListener('dragleave', Module.sokol_dragleave);
    canvas.removeEventListener('dragover',  Module.sokol_dragover);
    canvas.removeEventListener('drop',      Module.sokol_drop);
});

EM_JS(void, sapp_js_init, (const char* c_str_target), {
    // lookup and store canvas object by name
    const target_str = UTF8ToString(c_str_target);
    Module.sapp_emsc_target = document.getElementById(target_str);
    if (!Module.sapp_emsc_target) {
        console.log("sokol_app.h: invalid target:" + target_str);
    }
    if (!Module.sapp_emsc_target.requestPointerLock) {
        console.log("sokol_app.h: target doesn't support requestPointerLock:" + target_str);
    }
});

_SOKOL_PRIVATE EM_BOOL _sapp_emsc_pointerlockchange_cb(int emsc_type, const EmscriptenPointerlockChangeEvent* emsc_event, void* user_data) {
    _SOKOL_UNUSED(emsc_type);
    _SOKOL_UNUSED(user_data);
    _sapp.mouse.locked = emsc_event->isActive;
    return EM_TRUE;
}

_SOKOL_PRIVATE EM_BOOL _sapp_emsc_pointerlockerror_cb(int emsc_type, const void* reserved, void* user_data) {
    _SOKOL_UNUSED(emsc_type);
    _SOKOL_UNUSED(reserved);
    _SOKOL_UNUSED(user_data);
    _sapp.mouse.locked = false;
    _sapp.emsc.mouse_lock_requested = false;
    return true;
}

EM_JS(void, sapp_js_request_pointerlock, (void), {
    if (Module.sapp_emsc_target) {
        if (Module.sapp_emsc_target.requestPointerLock) {
            Module.sapp_emsc_target.requestPointerLock();
        }
    }
});

EM_JS(void, sapp_js_exit_pointerlock, (void), {
    if (document.exitPointerLock) {
        document.exitPointerLock();
    }
});

_SOKOL_PRIVATE void _sapp_emsc_lock_mouse(bool lock) {
    if (lock) {
        /* request mouse-lock during event handler invocation (see _sapp_emsc_update_mouse_lock_state) */
        _sapp.emsc.mouse_lock_requested = true;
    }
    else {
        /* NOTE: the _sapp.mouse_locked state will be set in the pointerlockchange callback */
        _sapp.emsc.mouse_lock_requested = false;
        sapp_js_exit_pointerlock();
    }
}

/* called from inside event handlers to check if mouse lock had been requested,
   and if yes, actually enter mouse lock.
*/
_SOKOL_PRIVATE void _sapp_emsc_update_mouse_lock_state(void) {
    if (_sapp.emsc.mouse_lock_requested) {
        _sapp.emsc.mouse_lock_requested = false;
        sapp_js_request_pointerlock();
    }
}

// set mouse cursor type
EM_JS(void, sapp_js_set_cursor, (int cursor_type, int shown), {
    if (Module.sapp_emsc_target) {
        let cursor;
        if (shown === 0) {
            cursor = "none";
        }
        else switch (cursor_type) {
            case 0: cursor = "auto"; break;         // SAPP_MOUSECURSOR_DEFAULT
            case 1: cursor = "default"; break;      // SAPP_MOUSECURSOR_ARROW
            case 2: cursor = "text"; break;         // SAPP_MOUSECURSOR_IBEAM
            case 3: cursor = "crosshair"; break;    // SAPP_MOUSECURSOR_CROSSHAIR
            case 4: cursor = "pointer"; break;      // SAPP_MOUSECURSOR_POINTING_HAND
            case 5: cursor = "ew-resize"; break;    // SAPP_MOUSECURSOR_RESIZE_EW
            case 6: cursor = "ns-resize"; break;    // SAPP_MOUSECURSOR_RESIZE_NS
            case 7: cursor = "nwse-resize"; break;  // SAPP_MOUSECURSOR_RESIZE_NWSE
            case 8: cursor = "nesw-resize"; break;  // SAPP_MOUSECURSOR_RESIZE_NESW
            case 9: cursor = "all-scroll"; break;   // SAPP_MOUSECURSOR_RESIZE_ALL
            case 10: cursor = "not-allowed"; break; // SAPP_MOUSECURSOR_NOT_ALLOWED
            default: cursor = "auto"; break;
        }
        Module.sapp_emsc_target.style.cursor = cursor;
    }
});

_SOKOL_PRIVATE void _sapp_emsc_update_cursor(sapp_mouse_cursor cursor, bool shown) {
    SOKOL_ASSERT((cursor >= 0) && (cursor < _SAPP_MOUSECURSOR_NUM));
    sapp_js_set_cursor((int)cursor, shown ? 1 : 0);
}

/* JS helper functions to update browser tab favicon */
EM_JS(void, sapp_js_clear_favicon, (void), {
    const link = document.getElementById('sokol-app-favicon');
    if (link) {
        document.head.removeChild(link);
    }
});

EM_JS(void, sapp_js_set_favicon, (int w, int h, const uint8_t* pixels), {
    const canvas = document.createElement('canvas');
    canvas.width = w;
    canvas.height = h;
    const ctx = canvas.getContext('2d');
    const img_data = ctx.createImageData(w, h);
    img_data.data.set(HEAPU8.subarray(pixels, pixels + w*h*4));
    ctx.putImageData(img_data, 0, 0);
    const new_link = document.createElement('link');
    new_link.id = 'sokol-app-favicon';
    new_link.rel = 'shortcut icon';
    new_link.href = canvas.toDataURL();
    document.head.appendChild(new_link);
});

_SOKOL_PRIVATE void _sapp_emsc_set_icon(const sapp_icon_desc* icon_desc, int num_images) {
    SOKOL_ASSERT((num_images > 0) && (num_images <= SAPP_MAX_ICONIMAGES));
    sapp_js_clear_favicon();
    // find the best matching image candidate for 16x16 pixels
    int img_index = _sapp_image_bestmatch(icon_desc->images, num_images, 16, 16);
    const sapp_image_desc* img_desc = &icon_desc->images[img_index];
    sapp_js_set_favicon(img_desc->width, img_desc->height, (const uint8_t*) img_desc->pixels.ptr);
}

_SOKOL_PRIVATE uint32_t _sapp_emsc_mouse_button_mods(uint16_t buttons) {
    uint32_t m = 0;
    if (0 != (buttons & (1<<0))) { m |= SAPP_MODIFIER_LMB; }
    if (0 != (buttons & (1<<1))) { m |= SAPP_MODIFIER_RMB; } // not a bug
    if (0 != (buttons & (1<<2))) { m |= SAPP_MODIFIER_MMB; } // not a bug
    return m;
}

_SOKOL_PRIVATE uint32_t _sapp_emsc_mouse_event_mods(const EmscriptenMouseEvent* ev) {
    uint32_t m = 0;
    if (ev->ctrlKey)    { m |= SAPP_MODIFIER_CTRL; }
    if (ev->shiftKey)   { m |= SAPP_MODIFIER_SHIFT; }
    if (ev->altKey)     { m |= SAPP_MODIFIER_ALT; }
    if (ev->metaKey)    { m |= SAPP_MODIFIER_SUPER; }
    m |= _sapp_emsc_mouse_button_mods(_sapp.emsc.mouse_buttons);
    return m;
}

_SOKOL_PRIVATE uint32_t _sapp_emsc_key_event_mods(const EmscriptenKeyboardEvent* ev) {
    uint32_t m = 0;
    if (ev->ctrlKey)    { m |= SAPP_MODIFIER_CTRL; }
    if (ev->shiftKey)   { m |= SAPP_MODIFIER_SHIFT; }
    if (ev->altKey)     { m |= SAPP_MODIFIER_ALT; }
    if (ev->metaKey)    { m |= SAPP_MODIFIER_SUPER; }
    m |= _sapp_emsc_mouse_button_mods(_sapp.emsc.mouse_buttons);
    return m;
}

_SOKOL_PRIVATE uint32_t _sapp_emsc_touch_event_mods(const EmscriptenTouchEvent* ev) {
    uint32_t m = 0;
    if (ev->ctrlKey)    { m |= SAPP_MODIFIER_CTRL; }
    if (ev->shiftKey)   { m |= SAPP_MODIFIER_SHIFT; }
    if (ev->altKey)     { m |= SAPP_MODIFIER_ALT; }
    if (ev->metaKey)    { m |= SAPP_MODIFIER_SUPER; }
    m |= _sapp_emsc_mouse_button_mods(_sapp.emsc.mouse_buttons);
    return m;
}

#if defined(SOKOL_WGPU)
_SOKOL_PRIVATE void _sapp_emsc_wgpu_size_changed(void);
#endif

_SOKOL_PRIVATE EM_BOOL _sapp_emsc_size_changed(int event_type, const EmscriptenUiEvent* ui_event, void* user_data) {
    _SOKOL_UNUSED(event_type);
    _SOKOL_UNUSED(user_data);
    double w, h;
    emscripten_get_element_css_size(_sapp.html5_canvas_selector, &w, &h);
    /* The above method might report zero when toggling HTML5 fullscreen,
       in that case use the window's inner width reported by the
       emscripten event. This works ok when toggling *into* fullscreen
       but doesn't properly restore the previous canvas size when switching
       back from fullscreen.

       In general, due to the HTML5's fullscreen API's flaky nature it is
       recommended to use 'soft fullscreen' (stretching the WebGL canvas
       over the browser windows client rect) with a CSS definition like this:

            position: absolute;
            top: 0px;
            left: 0px;
            margin: 0px;
            border: 0;
            width: 100%;
            height: 100%;
            overflow: hidden;
            display: block;
    */
    if (w < 1.0) {
        w = ui_event->windowInnerWidth;
    }
    else {
        _sapp.window_width = (int)roundf(w);
    }
    if (h < 1.0) {
        h = ui_event->windowInnerHeight;
    }
    else {
        _sapp.window_height = (int)roundf(h);
    }
    if (_sapp.desc.high_dpi) {
        _sapp.dpi_scale = emscripten_get_device_pixel_ratio();
    }
    _sapp.framebuffer_width = (int)roundf(w * _sapp.dpi_scale);
    _sapp.framebuffer_height = (int)roundf(h * _sapp.dpi_scale);
    SOKOL_ASSERT((_sapp.framebuffer_width > 0) && (_sapp.framebuffer_height > 0));
    emscripten_set_canvas_element_size(_sapp.html5_canvas_selector, _sapp.framebuffer_width, _sapp.framebuffer_height);
    #if defined(SOKOL_WGPU)
        // on WebGPU: recreate size-dependent rendering surfaces
        _sapp_emsc_wgpu_size_changed();
    #endif
    if (_sapp_events_enabled()) {
        _sapp_init_event(SAPP_EVENTTYPE_RESIZED);
        _sapp_call_event(&_sapp.event);
    }
    return true;
}

_SOKOL_PRIVATE EM_BOOL _sapp_emsc_mouse_cb(int emsc_type, const EmscriptenMouseEvent* emsc_event, void* user_data) {
    _SOKOL_UNUSED(user_data);
    bool consume_event = !_sapp.desc.html5_bubble_mouse_events;
    _sapp.emsc.mouse_buttons = emsc_event->buttons;
    if (_sapp.mouse.locked) {
        _sapp.mouse.dx = (float) emsc_event->movementX;
        _sapp.mouse.dy = (float) emsc_event->movementY;
    } else {
        float new_x = emsc_event->targetX * _sapp.dpi_scale;
        float new_y = emsc_event->targetY * _sapp.dpi_scale;
        if (_sapp.mouse.pos_valid) {
            _sapp.mouse.dx = new_x - _sapp.mouse.x;
            _sapp.mouse.dy = new_y - _sapp.mouse.y;
        }
        _sapp.mouse.x = new_x;
        _sapp.mouse.y = new_y;
        _sapp.mouse.pos_valid = true;
    }
    if (_sapp_events_enabled() && (emsc_event->button >= 0) && (emsc_event->button < SAPP_MAX_MOUSEBUTTONS)) {
        sapp_event_type type;
        bool is_button_event = false;
        bool clear_dxdy = false;
        switch (emsc_type) {
            case EMSCRIPTEN_EVENT_MOUSEDOWN:
                type = SAPP_EVENTTYPE_MOUSE_DOWN;
                is_button_event = true;
                break;
            case EMSCRIPTEN_EVENT_MOUSEUP:
                type = SAPP_EVENTTYPE_MOUSE_UP;
                is_button_event = true;
                break;
            case EMSCRIPTEN_EVENT_MOUSEMOVE:
                type = SAPP_EVENTTYPE_MOUSE_MOVE;
                break;
            case EMSCRIPTEN_EVENT_MOUSEENTER:
                type = SAPP_EVENTTYPE_MOUSE_ENTER;
                clear_dxdy = true;
                break;
            case EMSCRIPTEN_EVENT_MOUSELEAVE:
                type = SAPP_EVENTTYPE_MOUSE_LEAVE;
                clear_dxdy = true;
                break;
            default:
                type = SAPP_EVENTTYPE_INVALID;
                break;
        }
        if (clear_dxdy) {
            _sapp.mouse.dx = 0.0f;
            _sapp.mouse.dy = 0.0f;
        }
        if (type != SAPP_EVENTTYPE_INVALID) {
            _sapp_init_event(type);
            _sapp.event.modifiers = _sapp_emsc_mouse_event_mods(emsc_event);
            if (is_button_event) {
                switch (emsc_event->button) {
                    case 0: _sapp.event.mouse_button = SAPP_MOUSEBUTTON_LEFT; break;
                    case 1: _sapp.event.mouse_button = SAPP_MOUSEBUTTON_MIDDLE; break;
                    case 2: _sapp.event.mouse_button = SAPP_MOUSEBUTTON_RIGHT; break;
                    default: _sapp.event.mouse_button = (sapp_mousebutton)emsc_event->button; break;
                }
            } else {
                _sapp.event.mouse_button = SAPP_MOUSEBUTTON_INVALID;
            }
            consume_event |= _sapp_call_event(&_sapp.event);
        }
        // mouse lock can only be activated in mouse button events (not in move, enter or leave)
        if (is_button_event) {
            _sapp_emsc_update_mouse_lock_state();
        }
    }
    return consume_event;
}

_SOKOL_PRIVATE EM_BOOL _sapp_emsc_wheel_cb(int emsc_type, const EmscriptenWheelEvent* emsc_event, void* user_data) {
    _SOKOL_UNUSED(emsc_type);
    _SOKOL_UNUSED(user_data);
    bool consume_event = !_sapp.desc.html5_bubble_wheel_events;
    _sapp.emsc.mouse_buttons = emsc_event->mouse.buttons;
    if (_sapp_events_enabled()) {
        _sapp_init_event(SAPP_EVENTTYPE_MOUSE_SCROLL);
        _sapp.event.modifiers = _sapp_emsc_mouse_event_mods(&emsc_event->mouse);
        /* see https://github.com/floooh/sokol/issues/339 */
        float scale;
        switch (emsc_event->deltaMode) {
            case DOM_DELTA_PIXEL: scale = -0.04f; break;
            case DOM_DELTA_LINE:  scale = -1.33f; break;
            case DOM_DELTA_PAGE:  scale = -10.0f; break; // FIXME: this is a guess
            default:              scale = -0.1f; break;  // shouldn't happen
        }
        _sapp.event.scroll_x = scale * (float)emsc_event->deltaX;
        _sapp.event.scroll_y = scale * (float)emsc_event->deltaY;
        consume_event |= _sapp_call_event(&_sapp.event);
    }
    _sapp_emsc_update_mouse_lock_state();
    return consume_event;
}

static struct {
    const char* str;
    sapp_keycode code;
} _sapp_emsc_keymap[] = {
    { "Backspace",      SAPP_KEYCODE_BACKSPACE },
    { "Tab",            SAPP_KEYCODE_TAB },
    { "Enter",          SAPP_KEYCODE_ENTER },
    { "ShiftLeft",      SAPP_KEYCODE_LEFT_SHIFT },
    { "ShiftRight",     SAPP_KEYCODE_RIGHT_SHIFT },
    { "ControlLeft",    SAPP_KEYCODE_LEFT_CONTROL },
    { "ControlRight",   SAPP_KEYCODE_RIGHT_CONTROL },
    { "AltLeft",        SAPP_KEYCODE_LEFT_ALT },
    { "AltRight",       SAPP_KEYCODE_RIGHT_ALT },
    { "Pause",          SAPP_KEYCODE_PAUSE },
    { "CapsLock",       SAPP_KEYCODE_CAPS_LOCK },
    { "Escape",         SAPP_KEYCODE_ESCAPE },
    { "Space",          SAPP_KEYCODE_SPACE },
    { "PageUp",         SAPP_KEYCODE_PAGE_UP },
    { "PageDown",       SAPP_KEYCODE_PAGE_DOWN },
    { "End",            SAPP_KEYCODE_END },
    { "Home",           SAPP_KEYCODE_HOME },
    { "ArrowLeft",      SAPP_KEYCODE_LEFT },
    { "ArrowUp",        SAPP_KEYCODE_UP },
    { "ArrowRight",     SAPP_KEYCODE_RIGHT },
    { "ArrowDown",      SAPP_KEYCODE_DOWN },
    { "PrintScreen",    SAPP_KEYCODE_PRINT_SCREEN },
    { "Insert",         SAPP_KEYCODE_INSERT },
    { "Delete",         SAPP_KEYCODE_DELETE },
    { "Digit0",         SAPP_KEYCODE_0 },
    { "Digit1",         SAPP_KEYCODE_1 },
    { "Digit2",         SAPP_KEYCODE_2 },
    { "Digit3",         SAPP_KEYCODE_3 },
    { "Digit4",         SAPP_KEYCODE_4 },
    { "Digit5",         SAPP_KEYCODE_5 },
    { "Digit6",         SAPP_KEYCODE_6 },
    { "Digit7",         SAPP_KEYCODE_7 },
    { "Digit8",         SAPP_KEYCODE_8 },
    { "Digit9",         SAPP_KEYCODE_9 },
    { "KeyA",           SAPP_KEYCODE_A },
    { "KeyB",           SAPP_KEYCODE_B },
    { "KeyC",           SAPP_KEYCODE_C },
    { "KeyD",           SAPP_KEYCODE_D },
    { "KeyE",           SAPP_KEYCODE_E },
    { "KeyF",           SAPP_KEYCODE_F },
    { "KeyG",           SAPP_KEYCODE_G },
    { "KeyH",           SAPP_KEYCODE_H },
    { "KeyI",           SAPP_KEYCODE_I },
    { "KeyJ",           SAPP_KEYCODE_J },
    { "KeyK",           SAPP_KEYCODE_K },
    { "KeyL",           SAPP_KEYCODE_L },
    { "KeyM",           SAPP_KEYCODE_M },
    { "KeyN",           SAPP_KEYCODE_N },
    { "KeyO",           SAPP_KEYCODE_O },
    { "KeyP",           SAPP_KEYCODE_P },
    { "KeyQ",           SAPP_KEYCODE_Q },
    { "KeyR",           SAPP_KEYCODE_R },
    { "KeyS",           SAPP_KEYCODE_S },
    { "KeyT",           SAPP_KEYCODE_T },
    { "KeyU",           SAPP_KEYCODE_U },
    { "KeyV",           SAPP_KEYCODE_V },
    { "KeyW",           SAPP_KEYCODE_W },
    { "KeyX",           SAPP_KEYCODE_X },
    { "KeyY",           SAPP_KEYCODE_Y },
    { "KeyZ",           SAPP_KEYCODE_Z },
    { "MetaLeft",       SAPP_KEYCODE_LEFT_SUPER },
    { "MetaRight",      SAPP_KEYCODE_RIGHT_SUPER },
    { "Numpad0",        SAPP_KEYCODE_KP_0 },
    { "Numpad1",        SAPP_KEYCODE_KP_1 },
    { "Numpad2",        SAPP_KEYCODE_KP_2 },
    { "Numpad3",        SAPP_KEYCODE_KP_3 },
    { "Numpad4",        SAPP_KEYCODE_KP_4 },
    { "Numpad5",        SAPP_KEYCODE_KP_5 },
    { "Numpad6",        SAPP_KEYCODE_KP_6 },
    { "Numpad7",        SAPP_KEYCODE_KP_7 },
    { "Numpad8",        SAPP_KEYCODE_KP_8 },
    { "Numpad9",        SAPP_KEYCODE_KP_9 },
    { "NumpadMultiply", SAPP_KEYCODE_KP_MULTIPLY },
    { "NumpadAdd",      SAPP_KEYCODE_KP_ADD },
    { "NumpadSubtract", SAPP_KEYCODE_KP_SUBTRACT },
    { "NumpadDecimal",  SAPP_KEYCODE_KP_DECIMAL },
    { "NumpadDivide",   SAPP_KEYCODE_KP_DIVIDE },
    { "F1",             SAPP_KEYCODE_F1 },
    { "F2",             SAPP_KEYCODE_F2 },
    { "F3",             SAPP_KEYCODE_F3 },
    { "F4",             SAPP_KEYCODE_F4 },
    { "F5",             SAPP_KEYCODE_F5 },
    { "F6",             SAPP_KEYCODE_F6 },
    { "F7",             SAPP_KEYCODE_F7 },
    { "F8",             SAPP_KEYCODE_F8 },
    { "F9",             SAPP_KEYCODE_F9 },
    { "F10",            SAPP_KEYCODE_F10 },
    { "F11",            SAPP_KEYCODE_F11 },
    { "F12",            SAPP_KEYCODE_F12 },
    { "NumLock",        SAPP_KEYCODE_NUM_LOCK },
    { "ScrollLock",     SAPP_KEYCODE_SCROLL_LOCK },
    { "Semicolon",      SAPP_KEYCODE_SEMICOLON },
    { "Equal",          SAPP_KEYCODE_EQUAL },
    { "Comma",          SAPP_KEYCODE_COMMA },
    { "Minus",          SAPP_KEYCODE_MINUS },
    { "Period",         SAPP_KEYCODE_PERIOD },
    { "Slash",          SAPP_KEYCODE_SLASH },
    { "Backquote",      SAPP_KEYCODE_GRAVE_ACCENT },
    { "BracketLeft",    SAPP_KEYCODE_LEFT_BRACKET },
    { "Backslash",      SAPP_KEYCODE_BACKSLASH },
    { "BracketRight",   SAPP_KEYCODE_RIGHT_BRACKET },
    { "Quote",          SAPP_KEYCODE_GRAVE_ACCENT },    // FIXME: ???
    { 0, SAPP_KEYCODE_INVALID },
};

_SOKOL_PRIVATE sapp_keycode _sapp_emsc_translate_key(const char* str) {
    int i = 0;
    const char* keystr;
    while (( keystr = _sapp_emsc_keymap[i].str )) {
        if (0 == strcmp(str, keystr)) {
            return _sapp_emsc_keymap[i].code;
        }
        i += 1;
    }
    return SAPP_KEYCODE_INVALID;
}

// returns true if the key code is a 'character key', this is used to decide
// if a key event needs to bubble up to create a char event
_SOKOL_PRIVATE bool _sapp_emsc_is_char_key(sapp_keycode key_code) {
    return key_code < SAPP_KEYCODE_WORLD_1;
}

_SOKOL_PRIVATE EM_BOOL _sapp_emsc_key_cb(int emsc_type, const EmscriptenKeyboardEvent* emsc_event, void* user_data) {
    _SOKOL_UNUSED(user_data);
    bool consume_event = false;
    if (_sapp_events_enabled()) {
        sapp_event_type type;
        switch (emsc_type) {
            case EMSCRIPTEN_EVENT_KEYDOWN:
                type = SAPP_EVENTTYPE_KEY_DOWN;
                break;
            case EMSCRIPTEN_EVENT_KEYUP:
                type = SAPP_EVENTTYPE_KEY_UP;
                break;
            case EMSCRIPTEN_EVENT_KEYPRESS:
                type = SAPP_EVENTTYPE_CHAR;
                break;
            default:
                type = SAPP_EVENTTYPE_INVALID;
                break;
        }
        if (type != SAPP_EVENTTYPE_INVALID) {
            bool send_keyup_followup = false;
            _sapp_init_event(type);
            _sapp.event.key_repeat = emsc_event->repeat;
            _sapp.event.modifiers = _sapp_emsc_key_event_mods(emsc_event);
            if (type == SAPP_EVENTTYPE_CHAR) {
                // NOTE: charCode doesn't appear to be supported on Android Chrome
                _sapp.event.char_code = emsc_event->charCode;
                consume_event |= !_sapp.desc.html5_bubble_char_events;
            } else {
                if (0 != emsc_event->code[0]) {
                    // This code path is for desktop browsers which send untranslated 'physical' key code strings
                    // (which is what we actually want for key events)
                    _sapp.event.key_code = _sapp_emsc_translate_key(emsc_event->code);
                } else {
                    // This code path is for mobile browsers which only send localized key code
                    // strings. Note that the translation will only work for a small subset
                    // of localization-agnostic keys (like Enter, arrow keys, etc...), but
                    // regular alpha-numeric keys will all result in an SAPP_KEYCODE_INVALID)
                    _sapp.event.key_code = _sapp_emsc_translate_key(emsc_event->key);
                }

                // Special hack for macOS: if the Super key is pressed, macOS doesn't
                //  send keyUp events. As a workaround, to prevent keys from
                //  "sticking", we'll send a keyup event following a keydown
                //  when the SUPER key is pressed
                if ((type == SAPP_EVENTTYPE_KEY_DOWN) &&
                    (_sapp.event.key_code != SAPP_KEYCODE_LEFT_SUPER) &&
                    (_sapp.event.key_code != SAPP_KEYCODE_RIGHT_SUPER) &&
                    (_sapp.event.modifiers & SAPP_MODIFIER_SUPER))
                {
                    send_keyup_followup = true;
                }

                // 'character key events' will always need to bubble up, otherwise the browser
                // wouldn't be able to generate character events.
                if (!_sapp_emsc_is_char_key(_sapp.event.key_code)) {
                    consume_event |= !_sapp.desc.html5_bubble_key_events;
                }
            }
            consume_event |= _sapp_call_event(&_sapp.event);
            if (send_keyup_followup) {
                _sapp.event.type = SAPP_EVENTTYPE_KEY_UP;
                consume_event |= _sapp_call_event(&_sapp.event);
            }
        }
    }
    _sapp_emsc_update_mouse_lock_state();
    return consume_event;
}

_SOKOL_PRIVATE EM_BOOL _sapp_emsc_touch_cb(int emsc_type, const EmscriptenTouchEvent* emsc_event, void* user_data) {
    _SOKOL_UNUSED(user_data);
    bool consume_event = !_sapp.desc.html5_bubble_touch_events;
    if (_sapp_events_enabled()) {
        sapp_event_type type;
        switch (emsc_type) {
            case EMSCRIPTEN_EVENT_TOUCHSTART:
                type = SAPP_EVENTTYPE_TOUCHES_BEGAN;
                break;
            case EMSCRIPTEN_EVENT_TOUCHMOVE:
                type = SAPP_EVENTTYPE_TOUCHES_MOVED;
                break;
            case EMSCRIPTEN_EVENT_TOUCHEND:
                type = SAPP_EVENTTYPE_TOUCHES_ENDED;
                break;
            case EMSCRIPTEN_EVENT_TOUCHCANCEL:
                type = SAPP_EVENTTYPE_TOUCHES_CANCELLED;
                break;
            default:
                type = SAPP_EVENTTYPE_INVALID;
                break;
        }
        if (type != SAPP_EVENTTYPE_INVALID) {
            _sapp_init_event(type);
            _sapp.event.modifiers = _sapp_emsc_touch_event_mods(emsc_event);
            _sapp.event.num_touches = emsc_event->numTouches;
            if (_sapp.event.num_touches > SAPP_MAX_TOUCHPOINTS) {
                _sapp.event.num_touches = SAPP_MAX_TOUCHPOINTS;
            }
            for (int i = 0; i < _sapp.event.num_touches; i++) {
                const EmscriptenTouchPoint* src = &emsc_event->touches[i];
                sapp_touchpoint* dst = &_sapp.event.touches[i];
                dst->identifier = (uintptr_t)src->identifier;
                dst->pos_x = src->targetX * _sapp.dpi_scale;
                dst->pos_y = src->targetY * _sapp.dpi_scale;
                dst->changed = src->isChanged;
            }
            consume_event |= _sapp_call_event(&_sapp.event);
        }
    }
    return consume_event;
}

_SOKOL_PRIVATE EM_BOOL _sapp_emsc_focus_cb(int emsc_type, const EmscriptenFocusEvent* emsc_event, void* user_data) {
    _SOKOL_UNUSED(emsc_type);
    _SOKOL_UNUSED(emsc_event);
    _SOKOL_UNUSED(user_data);
    if (_sapp_events_enabled()) {
        _sapp_init_event(SAPP_EVENTTYPE_FOCUSED);
        _sapp_call_event(&_sapp.event);
    }
    return true;
}

_SOKOL_PRIVATE EM_BOOL _sapp_emsc_blur_cb(int emsc_type, const EmscriptenFocusEvent* emsc_event, void* user_data) {
    _SOKOL_UNUSED(emsc_type);
    _SOKOL_UNUSED(emsc_event);
    _SOKOL_UNUSED(user_data);
    if (_sapp_events_enabled()) {
        _sapp_init_event(SAPP_EVENTTYPE_UNFOCUSED);
        _sapp_call_event(&_sapp.event);
    }
    return true;
}

#if defined(SOKOL_GLES3)
_SOKOL_PRIVATE EM_BOOL _sapp_emsc_webgl_context_cb(int emsc_type, const void* reserved, void* user_data) {
    _SOKOL_UNUSED(reserved);
    _SOKOL_UNUSED(user_data);
    sapp_event_type type;
    switch (emsc_type) {
        case EMSCRIPTEN_EVENT_WEBGLCONTEXTLOST:     type = SAPP_EVENTTYPE_SUSPENDED; break;
        case EMSCRIPTEN_EVENT_WEBGLCONTEXTRESTORED: type = SAPP_EVENTTYPE_RESUMED; break;
        default:                                    type = SAPP_EVENTTYPE_INVALID; break;
    }
    if (_sapp_events_enabled() && (SAPP_EVENTTYPE_INVALID != type)) {
        _sapp_init_event(type);
        _sapp_call_event(&_sapp.event);
    }
    return true;
}

_SOKOL_PRIVATE void _sapp_emsc_webgl_init(void) {
    EmscriptenWebGLContextAttributes attrs;
    emscripten_webgl_init_context_attributes(&attrs);
    attrs.alpha = _sapp.desc.alpha;
    attrs.depth = true;
    attrs.stencil = true;
    attrs.antialias = _sapp.sample_count > 1;
    attrs.premultipliedAlpha = _sapp.desc.html5_premultiplied_alpha;
    attrs.preserveDrawingBuffer = _sapp.desc.html5_preserve_drawing_buffer;
    attrs.enableExtensionsByDefault = true;
    attrs.majorVersion = 2;
    EMSCRIPTEN_WEBGL_CONTEXT_HANDLE ctx = emscripten_webgl_create_context(_sapp.html5_canvas_selector, &attrs);
    // FIXME: error message?
    emscripten_webgl_make_context_current(ctx);
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, (GLint*)&_sapp.gl.framebuffer);

    // FIXME: remove PVRTC support here and in sokol-gfx at some point
    // some WebGL extension are not enabled automatically by emscripten
    emscripten_webgl_enable_extension(ctx, "WEBKIT_WEBGL_compressed_texture_pvrtc");
}
#endif

#if defined(SOKOL_WGPU)

_SOKOL_PRIVATE void _sapp_emsc_wgpu_create_swapchain(void) {
    SOKOL_ASSERT(_sapp.wgpu.instance);
    SOKOL_ASSERT(_sapp.wgpu.device);
    SOKOL_ASSERT(0 == _sapp.wgpu.surface);
    SOKOL_ASSERT(0 == _sapp.wgpu.swapchain);
    SOKOL_ASSERT(0 == _sapp.wgpu.msaa_tex);
    SOKOL_ASSERT(0 == _sapp.wgpu.msaa_view);
    SOKOL_ASSERT(0 == _sapp.wgpu.depth_stencil_tex);
    SOKOL_ASSERT(0 == _sapp.wgpu.depth_stencil_view);
    SOKOL_ASSERT(0 == _sapp.wgpu.swapchain_view);

    WGPUSurfaceDescriptorFromCanvasHTMLSelector canvas_desc;
    _sapp_clear(&canvas_desc, sizeof(canvas_desc));
    canvas_desc.chain.sType = WGPUSType_SurfaceDescriptorFromCanvasHTMLSelector;
    canvas_desc.selector = _sapp.html5_canvas_selector;
    WGPUSurfaceDescriptor surf_desc;
    _sapp_clear(&surf_desc, sizeof(surf_desc));
    surf_desc.nextInChain = &canvas_desc.chain;
    _sapp.wgpu.surface = wgpuInstanceCreateSurface(_sapp.wgpu.instance, &surf_desc);
    if (0 == _sapp.wgpu.surface) {
        _SAPP_PANIC(WGPU_SWAPCHAIN_CREATE_SURFACE_FAILED);
    }
    _sapp.wgpu.render_format = wgpuSurfaceGetPreferredFormat(_sapp.wgpu.surface, _sapp.wgpu.adapter);

    WGPUSwapChainDescriptor sc_desc;
    _sapp_clear(&sc_desc, sizeof(sc_desc));
    sc_desc.usage = WGPUTextureUsage_RenderAttachment;
    sc_desc.format = _sapp.wgpu.render_format;
    sc_desc.width = (uint32_t)_sapp.framebuffer_width;
    sc_desc.height = (uint32_t)_sapp.framebuffer_height;
    sc_desc.presentMode = WGPUPresentMode_Fifo;
    _sapp.wgpu.swapchain = wgpuDeviceCreateSwapChain(_sapp.wgpu.device, _sapp.wgpu.surface, &sc_desc);
    if (0 == _sapp.wgpu.swapchain) {
        _SAPP_PANIC(WGPU_SWAPCHAIN_CREATE_SWAPCHAIN_FAILED);
    }

    WGPUTextureDescriptor ds_desc;
    _sapp_clear(&ds_desc, sizeof(ds_desc));
    ds_desc.usage = WGPUTextureUsage_RenderAttachment;
    ds_desc.dimension = WGPUTextureDimension_2D;
    ds_desc.size.width = (uint32_t)_sapp.framebuffer_width;
    ds_desc.size.height = (uint32_t)_sapp.framebuffer_height;
    ds_desc.size.depthOrArrayLayers = 1;
    ds_desc.format = WGPUTextureFormat_Depth32FloatStencil8;
    ds_desc.mipLevelCount = 1;
    ds_desc.sampleCount = (uint32_t)_sapp.sample_count;
    _sapp.wgpu.depth_stencil_tex = wgpuDeviceCreateTexture(_sapp.wgpu.device, &ds_desc);
    if (0 == _sapp.wgpu.depth_stencil_tex) {
        _SAPP_PANIC(WGPU_SWAPCHAIN_CREATE_DEPTH_STENCIL_TEXTURE_FAILED);
    }
    _sapp.wgpu.depth_stencil_view = wgpuTextureCreateView(_sapp.wgpu.depth_stencil_tex, 0);
    if (0 == _sapp.wgpu.depth_stencil_view) {
        _SAPP_PANIC(WGPU_SWAPCHAIN_CREATE_DEPTH_STENCIL_VIEW_FAILED);
    }

    if (_sapp.sample_count > 1) {
        WGPUTextureDescriptor msaa_desc;
        _sapp_clear(&msaa_desc, sizeof(msaa_desc));
        msaa_desc.usage = WGPUTextureUsage_RenderAttachment;
        msaa_desc.dimension = WGPUTextureDimension_2D;
        msaa_desc.size.width = (uint32_t)_sapp.framebuffer_width;
        msaa_desc.size.height = (uint32_t)_sapp.framebuffer_height;
        msaa_desc.size.depthOrArrayLayers = 1;
        msaa_desc.format = _sapp.wgpu.render_format;
        msaa_desc.mipLevelCount = 1;
        msaa_desc.sampleCount = (uint32_t)_sapp.sample_count;
        _sapp.wgpu.msaa_tex = wgpuDeviceCreateTexture(_sapp.wgpu.device, &msaa_desc);
        if (0 == _sapp.wgpu.msaa_tex) {
            _SAPP_PANIC(WGPU_SWAPCHAIN_CREATE_MSAA_TEXTURE_FAILED);
        }
        _sapp.wgpu.msaa_view = wgpuTextureCreateView(_sapp.wgpu.msaa_tex, 0);
        if (0 == _sapp.wgpu.msaa_view) {
            _SAPP_PANIC(WGPU_SWAPCHAIN_CREATE_MSAA_VIEW_FAILED);
        }
    }
}

_SOKOL_PRIVATE void _sapp_emsc_wgpu_discard_swapchain(void) {
    if (_sapp.wgpu.msaa_view) {
        wgpuTextureViewRelease(_sapp.wgpu.msaa_view);
        _sapp.wgpu.msaa_view = 0;
    }
    if (_sapp.wgpu.msaa_tex) {
        wgpuTextureRelease(_sapp.wgpu.msaa_tex);
        _sapp.wgpu.msaa_tex = 0;
    }
    if (_sapp.wgpu.depth_stencil_view) {
        wgpuTextureViewRelease(_sapp.wgpu.depth_stencil_view);
        _sapp.wgpu.depth_stencil_view = 0;
    }
    if (_sapp.wgpu.depth_stencil_tex) {
        wgpuTextureRelease(_sapp.wgpu.depth_stencil_tex);
        _sapp.wgpu.depth_stencil_tex = 0;
    }
    if (_sapp.wgpu.swapchain) {
        wgpuSwapChainRelease(_sapp.wgpu.swapchain);
        _sapp.wgpu.swapchain = 0;
    }
    if (_sapp.wgpu.surface) {
        wgpuSurfaceRelease(_sapp.wgpu.surface);
        _sapp.wgpu.surface = 0;
    }
}

_SOKOL_PRIVATE void _sapp_emsc_wgpu_size_changed(void) {
    _sapp_emsc_wgpu_discard_swapchain();
    _sapp_emsc_wgpu_create_swapchain();
}

_SOKOL_PRIVATE void _sapp_emsc_wgpu_request_device_cb(WGPURequestDeviceStatus status, WGPUDevice device, const char* msg, void* userdata) {
    _SOKOL_UNUSED(msg);
    _SOKOL_UNUSED(userdata);
    SOKOL_ASSERT(!_sapp.wgpu.async_init_done);
    if (status != WGPURequestDeviceStatus_Success) {
        if (status == WGPURequestDeviceStatus_Error) {
            _SAPP_PANIC(WGPU_REQUEST_DEVICE_STATUS_ERROR);
        } else {
            _SAPP_PANIC(WGPU_REQUEST_DEVICE_STATUS_UNKNOWN);
        }
    }
    SOKOL_ASSERT(device);
    _sapp.wgpu.device = device;
    _sapp_emsc_wgpu_create_swapchain();
    _sapp.wgpu.async_init_done = true;
}

_SOKOL_PRIVATE void _sapp_emsc_wgpu_request_adapter_cb(WGPURequestAdapterStatus status, WGPUAdapter adapter, const char* msg, void* userdata) {
    _SOKOL_UNUSED(msg);
    _SOKOL_UNUSED(userdata);
    if (status != WGPURequestAdapterStatus_Success) {
        switch (status) {
            case WGPURequestAdapterStatus_Unavailable: _SAPP_PANIC(WGPU_REQUEST_ADAPTER_STATUS_UNAVAILABLE); break;
            case WGPURequestAdapterStatus_Error: _SAPP_PANIC(WGPU_REQUEST_ADAPTER_STATUS_ERROR); break;
            default: _SAPP_PANIC(WGPU_REQUEST_ADAPTER_STATUS_UNKNOWN); break;
        }
    }
    SOKOL_ASSERT(adapter);
    _sapp.wgpu.adapter = adapter;
    size_t cur_feature_index = 1;
    #define _SAPP_WGPU_MAX_REQUESTED_FEATURES (8)
    WGPUFeatureName requiredFeatures[_SAPP_WGPU_MAX_REQUESTED_FEATURES] = {
        WGPUFeatureName_Depth32FloatStencil8,
    };
    // check for optional features we're interested in
    if (wgpuAdapterHasFeature(adapter, WGPUFeatureName_TextureCompressionBC)) {
        SOKOL_ASSERT(cur_feature_index < _SAPP_WGPU_MAX_REQUESTED_FEATURES);
        requiredFeatures[cur_feature_index++] = WGPUFeatureName_TextureCompressionBC;
    }
    if (wgpuAdapterHasFeature(adapter, WGPUFeatureName_TextureCompressionETC2)) {
        SOKOL_ASSERT(cur_feature_index < _SAPP_WGPU_MAX_REQUESTED_FEATURES);
        requiredFeatures[cur_feature_index++] = WGPUFeatureName_TextureCompressionETC2;
    }
    if (wgpuAdapterHasFeature(adapter, WGPUFeatureName_TextureCompressionASTC)) {
        SOKOL_ASSERT(cur_feature_index < _SAPP_WGPU_MAX_REQUESTED_FEATURES);
        requiredFeatures[cur_feature_index++] = WGPUFeatureName_TextureCompressionASTC;
    }
    if (wgpuAdapterHasFeature(adapter, WGPUFeatureName_Float32Filterable)) {
        SOKOL_ASSERT(cur_feature_index < _SAPP_WGPU_MAX_REQUESTED_FEATURES);
        requiredFeatures[cur_feature_index++] = WGPUFeatureName_Float32Filterable;
    }
    #undef _SAPP_WGPU_MAX_REQUESTED_FEATURES

    WGPUDeviceDescriptor dev_desc;
    _sapp_clear(&dev_desc, sizeof(dev_desc));
    dev_desc.requiredFeatureCount = cur_feature_index;
    dev_desc.requiredFeatures = requiredFeatures,
    wgpuAdapterRequestDevice(adapter, &dev_desc, _sapp_emsc_wgpu_request_device_cb, 0);
}

_SOKOL_PRIVATE void _sapp_emsc_wgpu_init(void) {
    SOKOL_ASSERT(0 == _sapp.wgpu.instance);
    SOKOL_ASSERT(!_sapp.wgpu.async_init_done);
    _sapp.wgpu.instance = wgpuCreateInstance(0);
    if (0 == _sapp.wgpu.instance) {
        _SAPP_PANIC(WGPU_CREATE_INSTANCE_FAILED);
    }
    // FIXME: power preference?
    wgpuInstanceRequestAdapter(_sapp.wgpu.instance, 0, _sapp_emsc_wgpu_request_adapter_cb, 0);
}

_SOKOL_PRIVATE void _sapp_emsc_wgpu_frame(void) {
    if (_sapp.wgpu.async_init_done) {
        _sapp.wgpu.swapchain_view = wgpuSwapChainGetCurrentTextureView(_sapp.wgpu.swapchain);
        _sapp_frame();
        wgpuTextureViewRelease(_sapp.wgpu.swapchain_view);
        _sapp.wgpu.swapchain_view = 0;
    }
}
#endif // SOKOL_WGPU

_SOKOL_PRIVATE void _sapp_emsc_register_eventhandlers(void) {
    // NOTE: HTML canvas doesn't receive input focus, this is why key event handlers are added
    // to the window object (this could be worked around by adding a "tab index" to the
    // canvas)
    emscripten_set_mousedown_callback(_sapp.html5_canvas_selector, 0, true, _sapp_emsc_mouse_cb);
    emscripten_set_mouseup_callback(_sapp.html5_canvas_selector, 0, true, _sapp_emsc_mouse_cb);
    emscripten_set_mousemove_callback(_sapp.html5_canvas_selector, 0, true, _sapp_emsc_mouse_cb);
    emscripten_set_mouseenter_callback(_sapp.html5_canvas_selector, 0, true, _sapp_emsc_mouse_cb);
    emscripten_set_mouseleave_callback(_sapp.html5_canvas_selector, 0, true, _sapp_emsc_mouse_cb);
    emscripten_set_wheel_callback(_sapp.html5_canvas_selector, 0, true, _sapp_emsc_wheel_cb);
    emscripten_set_keydown_callback(EMSCRIPTEN_EVENT_TARGET_WINDOW, 0, true, _sapp_emsc_key_cb);
    emscripten_set_keyup_callback(EMSCRIPTEN_EVENT_TARGET_WINDOW, 0, true, _sapp_emsc_key_cb);
    emscripten_set_keypress_callback(EMSCRIPTEN_EVENT_TARGET_WINDOW, 0, true, _sapp_emsc_key_cb);
    emscripten_set_touchstart_callback(_sapp.html5_canvas_selector, 0, true, _sapp_emsc_touch_cb);
    emscripten_set_touchmove_callback(_sapp.html5_canvas_selector, 0, true, _sapp_emsc_touch_cb);
    emscripten_set_touchend_callback(_sapp.html5_canvas_selector, 0, true, _sapp_emsc_touch_cb);
    emscripten_set_touchcancel_callback(_sapp.html5_canvas_selector, 0, true, _sapp_emsc_touch_cb);
    emscripten_set_pointerlockchange_callback(EMSCRIPTEN_EVENT_TARGET_DOCUMENT, 0, true, _sapp_emsc_pointerlockchange_cb);
    emscripten_set_pointerlockerror_callback(EMSCRIPTEN_EVENT_TARGET_DOCUMENT, 0, true, _sapp_emsc_pointerlockerror_cb);
    emscripten_set_focus_callback(EMSCRIPTEN_EVENT_TARGET_WINDOW, 0, true, _sapp_emsc_focus_cb);
    emscripten_set_blur_callback(EMSCRIPTEN_EVENT_TARGET_WINDOW, 0, true, _sapp_emsc_blur_cb);
    sapp_js_add_beforeunload_listener();
    if (_sapp.clipboard.enabled) {
        sapp_js_add_clipboard_listener();
    }
    if (_sapp.drop.enabled) {
        sapp_js_add_dragndrop_listeners(&_sapp.html5_canvas_selector[1]);
    }
    #if defined(SOKOL_GLES3)
        emscripten_set_webglcontextlost_callback(_sapp.html5_canvas_selector, 0, true, _sapp_emsc_webgl_context_cb);
        emscripten_set_webglcontextrestored_callback(_sapp.html5_canvas_selector, 0, true, _sapp_emsc_webgl_context_cb);
    #endif
}

_SOKOL_PRIVATE void _sapp_emsc_unregister_eventhandlers(void) {
    emscripten_set_mousedown_callback(_sapp.html5_canvas_selector, 0, true, 0);
    emscripten_set_mouseup_callback(_sapp.html5_canvas_selector, 0, true, 0);
    emscripten_set_mousemove_callback(_sapp.html5_canvas_selector, 0, true, 0);
    emscripten_set_mouseenter_callback(_sapp.html5_canvas_selector, 0, true, 0);
    emscripten_set_mouseleave_callback(_sapp.html5_canvas_selector, 0, true, 0);
    emscripten_set_wheel_callback(_sapp.html5_canvas_selector, 0, true, 0);
    emscripten_set_keydown_callback(EMSCRIPTEN_EVENT_TARGET_WINDOW, 0, true, 0);
    emscripten_set_keyup_callback(EMSCRIPTEN_EVENT_TARGET_WINDOW, 0, true, 0);
    emscripten_set_keypress_callback(EMSCRIPTEN_EVENT_TARGET_WINDOW, 0, true, 0);
    emscripten_set_touchstart_callback(_sapp.html5_canvas_selector, 0, true, 0);
    emscripten_set_touchmove_callback(_sapp.html5_canvas_selector, 0, true, 0);
    emscripten_set_touchend_callback(_sapp.html5_canvas_selector, 0, true, 0);
    emscripten_set_touchcancel_callback(_sapp.html5_canvas_selector, 0, true, 0);
    emscripten_set_pointerlockchange_callback(EMSCRIPTEN_EVENT_TARGET_DOCUMENT, 0, true, 0);
    emscripten_set_pointerlockerror_callback(EMSCRIPTEN_EVENT_TARGET_DOCUMENT, 0, true, 0);
    emscripten_set_focus_callback(EMSCRIPTEN_EVENT_TARGET_WINDOW, 0, true, 0);
    emscripten_set_blur_callback(EMSCRIPTEN_EVENT_TARGET_WINDOW, 0, true, 0);
    if (!_sapp.desc.html5_canvas_resize) {
        emscripten_set_resize_callback(EMSCRIPTEN_EVENT_TARGET_WINDOW, 0, true, 0);
    }
    sapp_js_remove_beforeunload_listener();
    if (_sapp.clipboard.enabled) {
        sapp_js_remove_clipboard_listener();
    }
    if (_sapp.drop.enabled) {
        sapp_js_remove_dragndrop_listeners(&_sapp.html5_canvas_selector[1]);
    }
    #if defined(SOKOL_GLES3)
        emscripten_set_webglcontextlost_callback(_sapp.html5_canvas_selector, 0, true, 0);
        emscripten_set_webglcontextrestored_callback(_sapp.html5_canvas_selector, 0, true, 0);
    #endif
}

_SOKOL_PRIVATE EM_BOOL _sapp_emsc_frame_animation_loop(double time, void* userData) {
    _SOKOL_UNUSED(userData);
    _sapp_timing_external(&_sapp.timing, time / 1000.0);

    #if defined(SOKOL_WGPU)
        _sapp_emsc_wgpu_frame();
    #else
        _sapp_frame();
    #endif

    // quit-handling
    if (_sapp.quit_requested) {
        _sapp_init_event(SAPP_EVENTTYPE_QUIT_REQUESTED);
        _sapp_call_event(&_sapp.event);
        if (_sapp.quit_requested) {
            _sapp.quit_ordered = true;
        }
    }
    if (_sapp.quit_ordered) {
        _sapp_emsc_unregister_eventhandlers();
        _sapp_call_cleanup();
        _sapp_discard_state();
        return EM_FALSE;
    }
    return EM_TRUE;
}

_SOKOL_PRIVATE void _sapp_emsc_frame_main_loop(void) {
    const double time = emscripten_performance_now();
    if (!_sapp_emsc_frame_animation_loop(time, 0)) {
        emscripten_cancel_main_loop();
    }
}

_SOKOL_PRIVATE void _sapp_emsc_run(const sapp_desc* desc) {
    _sapp_init_state(desc);
    sapp_js_init(&_sapp.html5_canvas_selector[1]);
    double w, h;
    if (_sapp.desc.html5_canvas_resize) {
        w = (double) _sapp_def(_sapp.desc.width, _SAPP_FALLBACK_DEFAULT_WINDOW_WIDTH);
        h = (double) _sapp_def(_sapp.desc.height, _SAPP_FALLBACK_DEFAULT_WINDOW_HEIGHT);
    }
    else {
        emscripten_get_element_css_size(_sapp.html5_canvas_selector, &w, &h);
        emscripten_set_resize_callback(EMSCRIPTEN_EVENT_TARGET_WINDOW, 0, false, _sapp_emsc_size_changed);
    }
    if (_sapp.desc.high_dpi) {
        _sapp.dpi_scale = emscripten_get_device_pixel_ratio();
    }
    _sapp.window_width = (int)roundf(w);
    _sapp.window_height = (int)roundf(h);
    _sapp.framebuffer_width = (int)roundf(w * _sapp.dpi_scale);
    _sapp.framebuffer_height = (int)roundf(h * _sapp.dpi_scale);
    emscripten_set_canvas_element_size(_sapp.html5_canvas_selector, _sapp.framebuffer_width, _sapp.framebuffer_height);
    #if defined(SOKOL_GLES3)
        _sapp_emsc_webgl_init();
    #elif defined(SOKOL_WGPU)
        _sapp_emsc_wgpu_init();
    #endif
    _sapp.valid = true;
    _sapp_emsc_register_eventhandlers();
    sapp_set_icon(&desc->icon);

    // start the frame loop
    if (_sapp.desc.html5_use_emsc_set_main_loop) {
        emscripten_set_main_loop(_sapp_emsc_frame_main_loop, 0, _sapp.desc.html5_emsc_set_main_loop_simulate_infinite_loop);
    } else {
        emscripten_request_animation_frame_loop(_sapp_emsc_frame_animation_loop, 0);
    }
    // NOT A BUG: do not call _sapp_discard_state() here, instead this is
    // called in _sapp_emsc_frame() when the application is ordered to quit
}

#if !defined(SOKOL_NO_ENTRY)
int main(int argc, char* argv[]) {
    sapp_desc desc = sokol_main(argc, argv);
    _sapp_emsc_run(&desc);
    return 0;
}
#endif /* SOKOL_NO_ENTRY */
#endif /* _SAPP_EMSCRIPTEN */

//  ██████  ██          ██   ██ ███████ ██      ██████  ███████ ██████  ███████
// ██       ██          ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
// ██   ███ ██          ███████ █████   ██      ██████  █████   ██████  ███████
// ██    ██ ██          ██   ██ ██      ██      ██      ██      ██   ██      ██
//  ██████  ███████     ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████
//
// >>gl helpers
#if defined(SOKOL_GLCORE)
typedef struct {
    int         red_bits;
    int         green_bits;
    int         blue_bits;
    int         alpha_bits;
    int         depth_bits;
    int         stencil_bits;
    int         samples;
    bool        doublebuffer;
    uintptr_t   handle;
} _sapp_gl_fbconfig;

_SOKOL_PRIVATE void _sapp_gl_init_fbconfig(_sapp_gl_fbconfig* fbconfig) {
    _sapp_clear(fbconfig, sizeof(_sapp_gl_fbconfig));
    /* -1 means "don't care" */
    fbconfig->red_bits = -1;
    fbconfig->green_bits = -1;
    fbconfig->blue_bits = -1;
    fbconfig->alpha_bits = -1;
    fbconfig->depth_bits = -1;
    fbconfig->stencil_bits = -1;
    fbconfig->samples = -1;
}

typedef struct {
    int least_missing;
    int least_color_diff;
    int least_extra_diff;
    bool best_match;
} _sapp_gl_fbselect;

_SOKOL_PRIVATE void _sapp_gl_init_fbselect(_sapp_gl_fbselect* fbselect) {
    _sapp_clear(fbselect, sizeof(_sapp_gl_fbselect));
    fbselect->least_missing = 1000000;
    fbselect->least_color_diff = 10000000;
    fbselect->least_extra_diff = 10000000;
    fbselect->best_match = false;
}

// NOTE: this is used only in the WGL code path
_SOKOL_PRIVATE bool _sapp_gl_select_fbconfig(_sapp_gl_fbselect* fbselect, const _sapp_gl_fbconfig* desired, const _sapp_gl_fbconfig* current) {
    int missing = 0;
    if (desired->doublebuffer != current->doublebuffer) {
        return false;
    }

    if ((desired->alpha_bits > 0) && (current->alpha_bits == 0)) {
        missing++;
    }
    if ((desired->depth_bits > 0) && (current->depth_bits == 0)) {
        missing++;
    }
    if ((desired->stencil_bits > 0) && (current->stencil_bits == 0)) {
        missing++;
    }
    if ((desired->samples > 0) && (current->samples == 0)) {
        /* Technically, several multisampling buffers could be
            involved, but that's a lower level implementation detail and
            not important to us here, so we count them as one
        */
        missing++;
    }

    /* These polynomials make many small channel size differences matter
        less than one large channel size difference
        Calculate color channel size difference value
    */
    int color_diff = 0;
    if (desired->red_bits != -1) {
        color_diff += (desired->red_bits - current->red_bits) * (desired->red_bits - current->red_bits);
    }
    if (desired->green_bits != -1) {
        color_diff += (desired->green_bits - current->green_bits) * (desired->green_bits - current->green_bits);
    }
    if (desired->blue_bits != -1) {
        color_diff += (desired->blue_bits - current->blue_bits) * (desired->blue_bits - current->blue_bits);
    }

    /* Calculate non-color channel size difference value */
    int extra_diff = 0;
    if (desired->alpha_bits != -1) {
        extra_diff += (desired->alpha_bits - current->alpha_bits) * (desired->alpha_bits - current->alpha_bits);
    }
    if (desired->depth_bits != -1) {
        extra_diff += (desired->depth_bits - current->depth_bits) * (desired->depth_bits - current->depth_bits);
    }
    if (desired->stencil_bits != -1) {
        extra_diff += (desired->stencil_bits - current->stencil_bits) * (desired->stencil_bits - current->stencil_bits);
    }
    if (desired->samples != -1) {
        extra_diff += (desired->samples - current->samples) * (desired->samples - current->samples);
    }

    /* Figure out if the current one is better than the best one found so far
        Least number of missing buffers is the most important heuristic,
        then color buffer size match and lastly size match for other buffers
    */
    bool new_closest = false;
    if (missing < fbselect->least_missing) {
        new_closest = true;
    } else if (missing == fbselect->least_missing) {
        if ((color_diff < fbselect->least_color_diff) ||
            ((color_diff == fbselect->least_color_diff) && (extra_diff < fbselect->least_extra_diff)))
        {
            new_closest = true;
        }
    }
    if (new_closest) {
        fbselect->least_missing = missing;
        fbselect->least_color_diff = color_diff;
        fbselect->least_extra_diff = extra_diff;
        fbselect->best_match = (missing | color_diff | extra_diff) == 0;
    }
    return new_closest;
}

// NOTE: this is used only in the GLX code path
_SOKOL_PRIVATE const _sapp_gl_fbconfig* _sapp_gl_choose_fbconfig(const _sapp_gl_fbconfig* desired, const _sapp_gl_fbconfig* alternatives, int count) {
    int missing, least_missing = 1000000;
    int color_diff, least_color_diff = 10000000;
    int extra_diff, least_extra_diff = 10000000;
    const _sapp_gl_fbconfig* current;
    const _sapp_gl_fbconfig* closest = 0;
    for (int i = 0;  i < count;  i++) {
        current = alternatives + i;
        if (desired->doublebuffer != current->doublebuffer) {
            continue;
        }
        missing = 0;
        if (desired->alpha_bits > 0 && current->alpha_bits == 0) {
            missing++;
        }
        if (desired->depth_bits > 0 && current->depth_bits == 0) {
            missing++;
        }
        if (desired->stencil_bits > 0 && current->stencil_bits == 0) {
            missing++;
        }
        if (desired->samples > 0 && current->samples == 0) {
            /* Technically, several multisampling buffers could be
                involved, but that's a lower level implementation detail and
                not important to us here, so we count them as one
            */
            missing++;
        }

        /* These polynomials make many small channel size differences matter
            less than one large channel size difference
            Calculate color channel size difference value
        */
        color_diff = 0;
        if (desired->red_bits != -1) {
            color_diff += (desired->red_bits - current->red_bits) * (desired->red_bits - current->red_bits);
        }
        if (desired->green_bits != -1) {
            color_diff += (desired->green_bits - current->green_bits) * (desired->green_bits - current->green_bits);
        }
        if (desired->blue_bits != -1) {
            color_diff += (desired->blue_bits - current->blue_bits) * (desired->blue_bits - current->blue_bits);
        }

        /* Calculate non-color channel size difference value */
        extra_diff = 0;
        if (desired->alpha_bits != -1) {
            extra_diff += (desired->alpha_bits - current->alpha_bits) * (desired->alpha_bits - current->alpha_bits);
        }
        if (desired->depth_bits != -1) {
            extra_diff += (desired->depth_bits - current->depth_bits) * (desired->depth_bits - current->depth_bits);
        }
        if (desired->stencil_bits != -1) {
            extra_diff += (desired->stencil_bits - current->stencil_bits) * (desired->stencil_bits - current->stencil_bits);
        }
        if (desired->samples != -1) {
            extra_diff += (desired->samples - current->samples) * (desired->samples - current->samples);
        }

        /* Figure out if the current one is better than the best one found so far
            Least number of missing buffers is the most important heuristic,
            then color buffer size match and lastly size match for other buffers
        */
        if (missing < least_missing) {
            closest = current;
        }
        else if (missing == least_missing) {
            if ((color_diff < least_color_diff) ||
                (color_diff == least_color_diff && extra_diff < least_extra_diff))
            {
                closest = current;
            }
        }
        if (current == closest) {
            least_missing = missing;
            least_color_diff = color_diff;
            least_extra_diff = extra_diff;
        }
    }
    return closest;
}
#endif

// ██     ██ ██ ███    ██ ██████   ██████  ██     ██ ███████
// ██     ██ ██ ████   ██ ██   ██ ██    ██ ██     ██ ██
// ██  █  ██ ██ ██ ██  ██ ██   ██ ██    ██ ██  █  ██ ███████
// ██ ███ ██ ██ ██  ██ ██ ██   ██ ██    ██ ██ ███ ██      ██
//  ███ ███  ██ ██   ████ ██████   ██████   ███ ███  ███████
//
// >>windows
#if defined(_SAPP_WIN32)
_SOKOL_PRIVATE bool _sapp_win32_utf8_to_wide(const char* src, wchar_t* dst, int dst_num_bytes) {
    SOKOL_ASSERT(src && dst && (dst_num_bytes > 1));
    _sapp_clear(dst, (size_t)dst_num_bytes);
    const int dst_chars = dst_num_bytes / (int)sizeof(wchar_t);
    const int dst_needed = MultiByteToWideChar(CP_UTF8, 0, src, -1, 0, 0);
    if ((dst_needed > 0) && (dst_needed < dst_chars)) {
        MultiByteToWideChar(CP_UTF8, 0, src, -1, dst, dst_chars);
        return true;
    }
    else {
        /* input string doesn't fit into destination buffer */
        return false;
    }
}

_SOKOL_PRIVATE void _sapp_win32_app_event(sapp_event_type type) {
    if (_sapp_events_enabled()) {
        _sapp_init_event(type);
        _sapp_call_event(&_sapp.event);
    }
}

_SOKOL_PRIVATE void _sapp_win32_init_keytable(void) {
    /* same as GLFW */
    _sapp.keycodes[0x00B] = SAPP_KEYCODE_0;
    _sapp.keycodes[0x002] = SAPP_KEYCODE_1;
    _sapp.keycodes[0x003] = SAPP_KEYCODE_2;
    _sapp.keycodes[0x004] = SAPP_KEYCODE_3;
    _sapp.keycodes[0x005] = SAPP_KEYCODE_4;
    _sapp.keycodes[0x006] = SAPP_KEYCODE_5;
    _sapp.keycodes[0x007] = SAPP_KEYCODE_6;
    _sapp.keycodes[0x008] = SAPP_KEYCODE_7;
    _sapp.keycodes[0x009] = SAPP_KEYCODE_8;
    _sapp.keycodes[0x00A] = SAPP_KEYCODE_9;
    _sapp.keycodes[0x01E] = SAPP_KEYCODE_A;
    _sapp.keycodes[0x030] = SAPP_KEYCODE_B;
    _sapp.keycodes[0x02E] = SAPP_KEYCODE_C;
    _sapp.keycodes[0x020] = SAPP_KEYCODE_D;
    _sapp.keycodes[0x012] = SAPP_KEYCODE_E;
    _sapp.keycodes[0x021] = SAPP_KEYCODE_F;
    _sapp.keycodes[0x022] = SAPP_KEYCODE_G;
    _sapp.keycodes[0x023] = SAPP_KEYCODE_H;
    _sapp.keycodes[0x017] = SAPP_KEYCODE_I;
    _sapp.keycodes[0x024] = SAPP_KEYCODE_J;
    _sapp.keycodes[0x025] = SAPP_KEYCODE_K;
    _sapp.keycodes[0x026] = SAPP_KEYCODE_L;
    _sapp.keycodes[0x032] = SAPP_KEYCODE_M;
    _sapp.keycodes[0x031] = SAPP_KEYCODE_N;
    _sapp.keycodes[0x018] = SAPP_KEYCODE_O;
    _sapp.keycodes[0x019] = SAPP_KEYCODE_P;
    _sapp.keycodes[0x010] = SAPP_KEYCODE_Q;
    _sapp.keycodes[0x013] = SAPP_KEYCODE_R;
    _sapp.keycodes[0x01F] = SAPP_KEYCODE_S;
    _sapp.keycodes[0x014] = SAPP_KEYCODE_T;
    _sapp.keycodes[0x016] = SAPP_KEYCODE_U;
    _sapp.keycodes[0x02F] = SAPP_KEYCODE_V;
    _sapp.keycodes[0x011] = SAPP_KEYCODE_W;
    _sapp.keycodes[0x02D] = SAPP_KEYCODE_X;
    _sapp.keycodes[0x015] = SAPP_KEYCODE_Y;
    _sapp.keycodes[0x02C] = SAPP_KEYCODE_Z;
    _sapp.keycodes[0x028] = SAPP_KEYCODE_APOSTROPHE;
    _sapp.keycodes[0x02B] = SAPP_KEYCODE_BACKSLASH;
    _sapp.keycodes[0x033] = SAPP_KEYCODE_COMMA;
    _sapp.keycodes[0x00D] = SAPP_KEYCODE_EQUAL;
    _sapp.keycodes[0x029] = SAPP_KEYCODE_GRAVE_ACCENT;
    _sapp.keycodes[0x01A] = SAPP_KEYCODE_LEFT_BRACKET;
    _sapp.keycodes[0x00C] = SAPP_KEYCODE_MINUS;
    _sapp.keycodes[0x034] = SAPP_KEYCODE_PERIOD;
    _sapp.keycodes[0x01B] = SAPP_KEYCODE_RIGHT_BRACKET;
    _sapp.keycodes[0x027] = SAPP_KEYCODE_SEMICOLON;
    _sapp.keycodes[0x035] = SAPP_KEYCODE_SLASH;
    _sapp.keycodes[0x056] = SAPP_KEYCODE_WORLD_2;
    _sapp.keycodes[0x00E] = SAPP_KEYCODE_BACKSPACE;
    _sapp.keycodes[0x153] = SAPP_KEYCODE_DELETE;
    _sapp.keycodes[0x14F] = SAPP_KEYCODE_END;
    _sapp.keycodes[0x01C] = SAPP_KEYCODE_ENTER;
    _sapp.keycodes[0x001] = SAPP_KEYCODE_ESCAPE;
    _sapp.keycodes[0x147] = SAPP_KEYCODE_HOME;
    _sapp.keycodes[0x152] = SAPP_KEYCODE_INSERT;
    _sapp.keycodes[0x15D] = SAPP_KEYCODE_MENU;
    _sapp.keycodes[0x151] = SAPP_KEYCODE_PAGE_DOWN;
    _sapp.keycodes[0x149] = SAPP_KEYCODE_PAGE_UP;
    _sapp.keycodes[0x045] = SAPP_KEYCODE_PAUSE;
    _sapp.keycodes[0x146] = SAPP_KEYCODE_PAUSE;
    _sapp.keycodes[0x039] = SAPP_KEYCODE_SPACE;
    _sapp.keycodes[0x00F] = SAPP_KEYCODE_TAB;
    _sapp.keycodes[0x03A] = SAPP_KEYCODE_CAPS_LOCK;
    _sapp.keycodes[0x145] = SAPP_KEYCODE_NUM_LOCK;
    _sapp.keycodes[0x046] = SAPP_KEYCODE_SCROLL_LOCK;
    _sapp.keycodes[0x03B] = SAPP_KEYCODE_F1;
    _sapp.keycodes[0x03C] = SAPP_KEYCODE_F2;
    _sapp.keycodes[0x03D] = SAPP_KEYCODE_F3;
    _sapp.keycodes[0x03E] = SAPP_KEYCODE_F4;
    _sapp.keycodes[0x03F] = SAPP_KEYCODE_F5;
    _sapp.keycodes[0x040] = SAPP_KEYCODE_F6;
    _sapp.keycodes[0x041] = SAPP_KEYCODE_F7;
    _sapp.keycodes[0x042] = SAPP_KEYCODE_F8;
    _sapp.keycodes[0x043] = SAPP_KEYCODE_F9;
    _sapp.keycodes[0x044] = SAPP_KEYCODE_F10;
    _sapp.keycodes[0x057] = SAPP_KEYCODE_F11;
    _sapp.keycodes[0x058] = SAPP_KEYCODE_F12;
    _sapp.keycodes[0x064] = SAPP_KEYCODE_F13;
    _sapp.keycodes[0x065] = SAPP_KEYCODE_F14;
    _sapp.keycodes[0x066] = SAPP_KEYCODE_F15;
    _sapp.keycodes[0x067] = SAPP_KEYCODE_F16;
    _sapp.keycodes[0x068] = SAPP_KEYCODE_F17;
    _sapp.keycodes[0x069] = SAPP_KEYCODE_F18;
    _sapp.keycodes[0x06A] = SAPP_KEYCODE_F19;
    _sapp.keycodes[0x06B] = SAPP_KEYCODE_F20;
    _sapp.keycodes[0x06C] = SAPP_KEYCODE_F21;
    _sapp.keycodes[0x06D] = SAPP_KEYCODE_F22;
    _sapp.keycodes[0x06E] = SAPP_KEYCODE_F23;
    _sapp.keycodes[0x076] = SAPP_KEYCODE_F24;
    _sapp.keycodes[0x038] = SAPP_KEYCODE_LEFT_ALT;
    _sapp.keycodes[0x01D] = SAPP_KEYCODE_LEFT_CONTROL;
    _sapp.keycodes[0x02A] = SAPP_KEYCODE_LEFT_SHIFT;
    _sapp.keycodes[0x15B] = SAPP_KEYCODE_LEFT_SUPER;
    _sapp.keycodes[0x137] = SAPP_KEYCODE_PRINT_SCREEN;
    _sapp.keycodes[0x138] = SAPP_KEYCODE_RIGHT_ALT;
    _sapp.keycodes[0x11D] = SAPP_KEYCODE_RIGHT_CONTROL;
    _sapp.keycodes[0x036] = SAPP_KEYCODE_RIGHT_SHIFT;
    _sapp.keycodes[0x136] = SAPP_KEYCODE_RIGHT_SHIFT;
    _sapp.keycodes[0x15C] = SAPP_KEYCODE_RIGHT_SUPER;
    _sapp.keycodes[0x150] = SAPP_KEYCODE_DOWN;
    _sapp.keycodes[0x14B] = SAPP_KEYCODE_LEFT;
    _sapp.keycodes[0x14D] = SAPP_KEYCODE_RIGHT;
    _sapp.keycodes[0x148] = SAPP_KEYCODE_UP;
    _sapp.keycodes[0x052] = SAPP_KEYCODE_KP_0;
    _sapp.keycodes[0x04F] = SAPP_KEYCODE_KP_1;
    _sapp.keycodes[0x050] = SAPP_KEYCODE_KP_2;
    _sapp.keycodes[0x051] = SAPP_KEYCODE_KP_3;
    _sapp.keycodes[0x04B] = SAPP_KEYCODE_KP_4;
    _sapp.keycodes[0x04C] = SAPP_KEYCODE_KP_5;
    _sapp.keycodes[0x04D] = SAPP_KEYCODE_KP_6;
    _sapp.keycodes[0x047] = SAPP_KEYCODE_KP_7;
    _sapp.keycodes[0x048] = SAPP_KEYCODE_KP_8;
    _sapp.keycodes[0x049] = SAPP_KEYCODE_KP_9;
    _sapp.keycodes[0x04E] = SAPP_KEYCODE_KP_ADD;
    _sapp.keycodes[0x053] = SAPP_KEYCODE_KP_DECIMAL;
    _sapp.keycodes[0x135] = SAPP_KEYCODE_KP_DIVIDE;
    _sapp.keycodes[0x11C] = SAPP_KEYCODE_KP_ENTER;
    _sapp.keycodes[0x037] = SAPP_KEYCODE_KP_MULTIPLY;
    _sapp.keycodes[0x04A] = SAPP_KEYCODE_KP_SUBTRACT;
}
#endif // _SAPP_WIN32

#if defined(_SAPP_WIN32)

#if defined(SOKOL_D3D11)

#if defined(__cplusplus)
#define _sapp_d3d11_Release(self) (self)->Release()
#define _sapp_win32_refiid(iid) iid
#else
#define _sapp_d3d11_Release(self) (self)->lpVtbl->Release(self)
#define _sapp_win32_refiid(iid) &iid
#endif

#define _SAPP_SAFE_RELEASE(obj) if (obj) { _sapp_d3d11_Release(obj); obj=0; }


static const IID _sapp_IID_ID3D11Texture2D = { 0x6f15aaf2,0xd208,0x4e89, {0x9a,0xb4,0x48,0x95,0x35,0xd3,0x4f,0x9c} };
static const IID _sapp_IID_IDXGIDevice1    = { 0x77db970f,0x6276,0x48ba, {0xba,0x28,0x07,0x01,0x43,0xb4,0x39,0x2c} };
static const IID _sapp_IID_IDXGIFactory    = { 0x7b7166ec,0x21c7,0x44ae, {0xb2,0x1a,0xc9,0xae,0x32,0x1a,0xe3,0x69} };

static inline HRESULT _sapp_dxgi_GetBuffer(IDXGISwapChain* self, UINT Buffer, REFIID riid, void** ppSurface) {
    #if defined(__cplusplus)
        return self->GetBuffer(Buffer, riid, ppSurface);
    #else
        return self->lpVtbl->GetBuffer(self, Buffer, riid, ppSurface);
    #endif
}

static inline HRESULT _sapp_d3d11_QueryInterface(ID3D11Device* self, REFIID riid, void** ppvObject) {
    #if defined(__cplusplus)
        return self->QueryInterface(riid, ppvObject);
    #else
        return self->lpVtbl->QueryInterface(self, riid, ppvObject);
    #endif
}

static inline HRESULT _sapp_d3d11_CreateRenderTargetView(ID3D11Device* self, ID3D11Resource *pResource, const D3D11_RENDER_TARGET_VIEW_DESC* pDesc, ID3D11RenderTargetView** ppRTView) {
    #if defined(__cplusplus)
        return self->CreateRenderTargetView(pResource, pDesc, ppRTView);
    #else
        return self->lpVtbl->CreateRenderTargetView(self, pResource, pDesc, ppRTView);
    #endif
}

static inline HRESULT _sapp_d3d11_CreateTexture2D(ID3D11Device* self, const D3D11_TEXTURE2D_DESC* pDesc, const D3D11_SUBRESOURCE_DATA* pInitialData, ID3D11Texture2D** ppTexture2D) {
    #if defined(__cplusplus)
        return self->CreateTexture2D(pDesc, pInitialData, ppTexture2D);
    #else
        return self->lpVtbl->CreateTexture2D(self, pDesc, pInitialData, ppTexture2D);
    #endif
}

static inline HRESULT _sapp_d3d11_CreateDepthStencilView(ID3D11Device* self, ID3D11Resource* pResource, const D3D11_DEPTH_STENCIL_VIEW_DESC* pDesc, ID3D11DepthStencilView** ppDepthStencilView) {
    #if defined(__cplusplus)
        return self->CreateDepthStencilView(pResource, pDesc, ppDepthStencilView);
    #else
        return self->lpVtbl->CreateDepthStencilView(self, pResource, pDesc, ppDepthStencilView);
    #endif
}

static inline HRESULT _sapp_dxgi_ResizeBuffers(IDXGISwapChain* self, UINT BufferCount, UINT Width, UINT Height, DXGI_FORMAT NewFormat, UINT SwapChainFlags) {
    #if defined(__cplusplus)
        return self->ResizeBuffers(BufferCount, Width, Height, NewFormat, SwapChainFlags);
    #else
        return self->lpVtbl->ResizeBuffers(self, BufferCount, Width, Height, NewFormat, SwapChainFlags);
    #endif
}

static inline HRESULT _sapp_dxgi_Present(IDXGISwapChain* self, UINT SyncInterval, UINT Flags) {
    #if defined(__cplusplus)
        return self->Present(SyncInterval, Flags);
    #else
        return self->lpVtbl->Present(self, SyncInterval, Flags);
    #endif
}

static inline HRESULT _sapp_dxgi_GetFrameStatistics(IDXGISwapChain* self, DXGI_FRAME_STATISTICS* pStats) {
    #if defined(__cplusplus)
        return self->GetFrameStatistics(pStats);
    #else
        return self->lpVtbl->GetFrameStatistics(self, pStats);
    #endif
}

static inline HRESULT _sapp_dxgi_SetMaximumFrameLatency(IDXGIDevice1* self, UINT MaxLatency) {
    #if defined(__cplusplus)
        return self->SetMaximumFrameLatency(MaxLatency);
    #else
        return self->lpVtbl->SetMaximumFrameLatency(self, MaxLatency);
    #endif
}

static inline HRESULT _sapp_dxgi_GetAdapter(IDXGIDevice1* self, IDXGIAdapter** pAdapter) {
    #if defined(__cplusplus)
        return self->GetAdapter(pAdapter);
    #else
        return self->lpVtbl->GetAdapter(self, pAdapter);
    #endif
}

static inline HRESULT _sapp_dxgi_GetParent(IDXGIObject* self, REFIID riid, void** ppParent) {
    #if defined(__cplusplus)
        return self->GetParent(riid, ppParent);
    #else
        return self->lpVtbl->GetParent(self, riid, ppParent);
    #endif
}

static inline HRESULT _sapp_dxgi_MakeWindowAssociation(IDXGIFactory* self, HWND WindowHandle, UINT Flags) {
    #if defined(__cplusplus)
        return self->MakeWindowAssociation(WindowHandle, Flags);
    #else
        return self->lpVtbl->MakeWindowAssociation(self, WindowHandle, Flags);
    #endif
}

_SOKOL_PRIVATE void _sapp_d3d11_create_device_and_swapchain(void) {
    DXGI_SWAP_CHAIN_DESC* sc_desc = &_sapp.d3d11.swap_chain_desc;
    sc_desc->BufferDesc.Width = (UINT)_sapp.framebuffer_width;
    sc_desc->BufferDesc.Height = (UINT)_sapp.framebuffer_height;
    sc_desc->BufferDesc.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
    sc_desc->BufferDesc.RefreshRate.Numerator = 60;
    sc_desc->BufferDesc.RefreshRate.Denominator = 1;
    sc_desc->OutputWindow = _sapp.win32.hwnd;
    sc_desc->Windowed = true;
    if (_sapp.win32.is_win10_or_greater) {
        sc_desc->BufferCount = 2;
        sc_desc->SwapEffect = (DXGI_SWAP_EFFECT) _SAPP_DXGI_SWAP_EFFECT_FLIP_DISCARD;
        _sapp.d3d11.use_dxgi_frame_stats = true;
    }
    else {
        sc_desc->BufferCount = 1;
        sc_desc->SwapEffect = DXGI_SWAP_EFFECT_DISCARD;
        _sapp.d3d11.use_dxgi_frame_stats = false;
    }
    sc_desc->SampleDesc.Count = 1;
    sc_desc->SampleDesc.Quality = 0;
    sc_desc->BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
    UINT create_flags = D3D11_CREATE_DEVICE_SINGLETHREADED | D3D11_CREATE_DEVICE_BGRA_SUPPORT;
    #if defined(SOKOL_DEBUG)
        create_flags |= D3D11_CREATE_DEVICE_DEBUG;
    #endif
    D3D_FEATURE_LEVEL feature_level;
    HRESULT hr = D3D11CreateDeviceAndSwapChain(
        NULL,                           /* pAdapter (use default) */
        D3D_DRIVER_TYPE_HARDWARE,       /* DriverType */
        NULL,                           /* Software */
        create_flags,                   /* Flags */
        NULL,                           /* pFeatureLevels */
        0,                              /* FeatureLevels */
        D3D11_SDK_VERSION,              /* SDKVersion */
        sc_desc,                        /* pSwapChainDesc */
        &_sapp.d3d11.swap_chain,        /* ppSwapChain */
        &_sapp.d3d11.device,            /* ppDevice */
        &feature_level,                 /* pFeatureLevel */
        &_sapp.d3d11.device_context);   /* ppImmediateContext */
    _SOKOL_UNUSED(hr);
    #if defined(SOKOL_DEBUG)
    if (!SUCCEEDED(hr)) {
        // if initialization with D3D11_CREATE_DEVICE_DEBUG fails, this could be because the
        // 'D3D11 debug layer' stopped working, indicated by the error message:
        // ===
        // D3D11CreateDevice: Flags (0x2) were specified which require the D3D11 SDK Layers for Windows 10, but they are not present on the system.
        // These flags must be removed, or the Windows 10 SDK must be installed.
        // Flags include: D3D11_CREATE_DEVICE_DEBUG
        // ===
        //
        // ...just retry with the DEBUG flag switched off
        _SAPP_ERROR(WIN32_D3D11_CREATE_DEVICE_AND_SWAPCHAIN_WITH_DEBUG_FAILED);
        create_flags &= ~D3D11_CREATE_DEVICE_DEBUG;
        hr = D3D11CreateDeviceAndSwapChain(
            NULL,                           /* pAdapter (use default) */
            D3D_DRIVER_TYPE_HARDWARE,       /* DriverType */
            NULL,                           /* Software */
            create_flags,                   /* Flags */
            NULL,                           /* pFeatureLevels */
            0,                              /* FeatureLevels */
            D3D11_SDK_VERSION,              /* SDKVersion */
            sc_desc,                        /* pSwapChainDesc */
            &_sapp.d3d11.swap_chain,        /* ppSwapChain */
            &_sapp.d3d11.device,            /* ppDevice */
            &feature_level,                 /* pFeatureLevel */
            &_sapp.d3d11.device_context);   /* ppImmediateContext */
    }
    #endif
    SOKOL_ASSERT(SUCCEEDED(hr) && _sapp.d3d11.swap_chain && _sapp.d3d11.device && _sapp.d3d11.device_context);

    // minimize frame latency, disable Alt-Enter
    hr = _sapp_d3d11_QueryInterface(_sapp.d3d11.device, _sapp_win32_refiid(_sapp_IID_IDXGIDevice1), (void**)&_sapp.d3d11.dxgi_device);
    if (SUCCEEDED(hr) && _sapp.d3d11.dxgi_device) {
        _sapp_dxgi_SetMaximumFrameLatency(_sapp.d3d11.dxgi_device, 1);
        IDXGIAdapter* dxgi_adapter = 0;
        hr = _sapp_dxgi_GetAdapter(_sapp.d3d11.dxgi_device, &dxgi_adapter);
        if (SUCCEEDED(hr) && dxgi_adapter) {
            IDXGIFactory* dxgi_factory = 0;
            hr = _sapp_dxgi_GetParent((IDXGIObject*)dxgi_adapter, _sapp_win32_refiid(_sapp_IID_IDXGIFactory), (void**)&dxgi_factory);
            if (SUCCEEDED(hr)) {
                _sapp_dxgi_MakeWindowAssociation(dxgi_factory, _sapp.win32.hwnd, DXGI_MWA_NO_ALT_ENTER|DXGI_MWA_NO_PRINT_SCREEN);
                _SAPP_SAFE_RELEASE(dxgi_factory);
            }
            else {
                _SAPP_ERROR(WIN32_D3D11_GET_IDXGIFACTORY_FAILED);
            }
            _SAPP_SAFE_RELEASE(dxgi_adapter);
        }
        else {
            _SAPP_ERROR(WIN32_D3D11_GET_IDXGIADAPTER_FAILED);
        }
    }
    else {
        _SAPP_PANIC(WIN32_D3D11_QUERY_INTERFACE_IDXGIDEVICE1_FAILED);
    }
}

_SOKOL_PRIVATE void _sapp_d3d11_destroy_device_and_swapchain(void) {
    _SAPP_SAFE_RELEASE(_sapp.d3d11.swap_chain);
    _SAPP_SAFE_RELEASE(_sapp.d3d11.dxgi_device);
    _SAPP_SAFE_RELEASE(_sapp.d3d11.device_context);
    _SAPP_SAFE_RELEASE(_sapp.d3d11.device);
}

_SOKOL_PRIVATE void _sapp_d3d11_create_default_render_target(void) {
    SOKOL_ASSERT(0 == _sapp.d3d11.rt);
    SOKOL_ASSERT(0 == _sapp.d3d11.rtv);
    SOKOL_ASSERT(0 == _sapp.d3d11.msaa_rt);
    SOKOL_ASSERT(0 == _sapp.d3d11.msaa_rtv);
    SOKOL_ASSERT(0 == _sapp.d3d11.ds);
    SOKOL_ASSERT(0 == _sapp.d3d11.dsv);

    HRESULT hr;

    /* view for the swapchain-created framebuffer */
    hr = _sapp_dxgi_GetBuffer(_sapp.d3d11.swap_chain, 0, _sapp_win32_refiid(_sapp_IID_ID3D11Texture2D), (void**)&_sapp.d3d11.rt);
    SOKOL_ASSERT(SUCCEEDED(hr) && _sapp.d3d11.rt);
    hr = _sapp_d3d11_CreateRenderTargetView(_sapp.d3d11.device, (ID3D11Resource*)_sapp.d3d11.rt, NULL, &_sapp.d3d11.rtv);
    SOKOL_ASSERT(SUCCEEDED(hr) && _sapp.d3d11.rtv);

    /* common desc for MSAA and depth-stencil texture */
    D3D11_TEXTURE2D_DESC tex_desc;
    _sapp_clear(&tex_desc, sizeof(tex_desc));
    tex_desc.Width = (UINT)_sapp.framebuffer_width;
    tex_desc.Height = (UINT)_sapp.framebuffer_height;
    tex_desc.MipLevels = 1;
    tex_desc.ArraySize = 1;
    tex_desc.Usage = D3D11_USAGE_DEFAULT;
    tex_desc.BindFlags = D3D11_BIND_RENDER_TARGET;
    tex_desc.SampleDesc.Count = (UINT) _sapp.sample_count;
    tex_desc.SampleDesc.Quality = (UINT) (_sapp.sample_count > 1 ? D3D11_STANDARD_MULTISAMPLE_PATTERN : 0);

    /* create MSAA texture and view if antialiasing requested */
    if (_sapp.sample_count > 1) {
        tex_desc.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
        hr = _sapp_d3d11_CreateTexture2D(_sapp.d3d11.device, &tex_desc, NULL, &_sapp.d3d11.msaa_rt);
        SOKOL_ASSERT(SUCCEEDED(hr) && _sapp.d3d11.msaa_rt);
        hr = _sapp_d3d11_CreateRenderTargetView(_sapp.d3d11.device, (ID3D11Resource*)_sapp.d3d11.msaa_rt, NULL, &_sapp.d3d11.msaa_rtv);
        SOKOL_ASSERT(SUCCEEDED(hr) && _sapp.d3d11.msaa_rtv);
    }

    /* texture and view for the depth-stencil-surface */
    tex_desc.Format = DXGI_FORMAT_D24_UNORM_S8_UINT;
    tex_desc.BindFlags = D3D11_BIND_DEPTH_STENCIL;
    hr = _sapp_d3d11_CreateTexture2D(_sapp.d3d11.device, &tex_desc, NULL, &_sapp.d3d11.ds);
    SOKOL_ASSERT(SUCCEEDED(hr) && _sapp.d3d11.ds);
    hr = _sapp_d3d11_CreateDepthStencilView(_sapp.d3d11.device, (ID3D11Resource*)_sapp.d3d11.ds, NULL, &_sapp.d3d11.dsv);
    SOKOL_ASSERT(SUCCEEDED(hr) && _sapp.d3d11.dsv);
}

_SOKOL_PRIVATE void _sapp_d3d11_destroy_default_render_target(void) {
    _SAPP_SAFE_RELEASE(_sapp.d3d11.rt);
    _SAPP_SAFE_RELEASE(_sapp.d3d11.rtv);
    _SAPP_SAFE_RELEASE(_sapp.d3d11.msaa_rt);
    _SAPP_SAFE_RELEASE(_sapp.d3d11.msaa_rtv);
    _SAPP_SAFE_RELEASE(_sapp.d3d11.ds);
    _SAPP_SAFE_RELEASE(_sapp.d3d11.dsv);
}

_SOKOL_PRIVATE void _sapp_d3d11_resize_default_render_target(void) {
    if (_sapp.d3d11.swap_chain) {
        _sapp_d3d11_destroy_default_render_target();
        _sapp_dxgi_ResizeBuffers(_sapp.d3d11.swap_chain, _sapp.d3d11.swap_chain_desc.BufferCount, (UINT)_sapp.framebuffer_width, (UINT)_sapp.framebuffer_height, DXGI_FORMAT_B8G8R8A8_UNORM, 0);
        _sapp_d3d11_create_default_render_target();
    }
}

_SOKOL_PRIVATE void _sapp_d3d11_present(bool do_not_wait) {
    UINT flags = 0;
    if (_sapp.win32.is_win10_or_greater && do_not_wait) {
        /* this hack/workaround somewhat improves window-movement and -sizing
            responsiveness when rendering is controlled via WM_TIMER during window
            move and resize on NVIDIA cards on Win10 with recent drivers.
        */
        flags = DXGI_PRESENT_DO_NOT_WAIT;
    }
    _sapp_dxgi_Present(_sapp.d3d11.swap_chain, (UINT)_sapp.swap_interval, flags);
}

#endif /* SOKOL_D3D11 */

#if defined(SOKOL_GLCORE)
_SOKOL_PRIVATE void _sapp_wgl_init(void) {
    _sapp.wgl.opengl32 = LoadLibraryA("opengl32.dll");
    if (!_sapp.wgl.opengl32) {
        _SAPP_PANIC(WIN32_LOAD_OPENGL32_DLL_FAILED);
    }
    SOKOL_ASSERT(_sapp.wgl.opengl32);
    _sapp.wgl.CreateContext = (PFN_wglCreateContext)(void*) GetProcAddress(_sapp.wgl.opengl32, "wglCreateContext");
    SOKOL_ASSERT(_sapp.wgl.CreateContext);
    _sapp.wgl.DeleteContext = (PFN_wglDeleteContext)(void*) GetProcAddress(_sapp.wgl.opengl32, "wglDeleteContext");
    SOKOL_ASSERT(_sapp.wgl.DeleteContext);
    _sapp.wgl.GetProcAddress = (PFN_wglGetProcAddress)(void*) GetProcAddress(_sapp.wgl.opengl32, "wglGetProcAddress");
    SOKOL_ASSERT(_sapp.wgl.GetProcAddress);
    _sapp.wgl.GetCurrentDC = (PFN_wglGetCurrentDC)(void*) GetProcAddress(_sapp.wgl.opengl32, "wglGetCurrentDC");
    SOKOL_ASSERT(_sapp.wgl.GetCurrentDC);
    _sapp.wgl.MakeCurrent = (PFN_wglMakeCurrent)(void*) GetProcAddress(_sapp.wgl.opengl32, "wglMakeCurrent");
    SOKOL_ASSERT(_sapp.wgl.MakeCurrent);
    _sapp.wgl.GetIntegerv = (void(WINAPI*)(uint32_t, int32_t*)) GetProcAddress(_sapp.wgl.opengl32, "glGetIntegerv");
    SOKOL_ASSERT(_sapp.wgl.GetIntegerv);

    _sapp.wgl.msg_hwnd = CreateWindowExW(WS_EX_OVERLAPPEDWINDOW,
        L"SOKOLAPP",
        L"sokol-app message window",
        WS_CLIPSIBLINGS|WS_CLIPCHILDREN,
        0, 0, 1, 1,
        NULL, NULL,
        GetModuleHandleW(NULL),
        NULL);
    if (!_sapp.wgl.msg_hwnd) {
        _SAPP_PANIC(WIN32_CREATE_HELPER_WINDOW_FAILED);
    }
    SOKOL_ASSERT(_sapp.wgl.msg_hwnd);
    ShowWindow(_sapp.wgl.msg_hwnd, SW_HIDE);
    MSG msg;
    while (PeekMessageW(&msg, _sapp.wgl.msg_hwnd, 0, 0, PM_REMOVE)) {
        TranslateMessage(&msg);
        DispatchMessageW(&msg);
    }
    _sapp.wgl.msg_dc = GetDC(_sapp.wgl.msg_hwnd);
    if (!_sapp.wgl.msg_dc) {
        _SAPP_PANIC(WIN32_HELPER_WINDOW_GETDC_FAILED);
    }
}

_SOKOL_PRIVATE void _sapp_wgl_shutdown(void) {
    SOKOL_ASSERT(_sapp.wgl.opengl32 && _sapp.wgl.msg_hwnd);
    DestroyWindow(_sapp.wgl.msg_hwnd); _sapp.wgl.msg_hwnd = 0;
    FreeLibrary(_sapp.wgl.opengl32); _sapp.wgl.opengl32 = 0;
}

_SOKOL_PRIVATE bool _sapp_wgl_has_ext(const char* ext, const char* extensions) {
    SOKOL_ASSERT(ext && extensions);
    const char* start = extensions;
    while (true) {
        const char* where = strstr(start, ext);
        if (!where) {
            return false;
        }
        const char* terminator = where + strlen(ext);
        if ((where == start) || (*(where - 1) == ' ')) {
            if (*terminator == ' ' || *terminator == '\0') {
                break;
            }
        }
        start = terminator;
    }
    return true;
}

_SOKOL_PRIVATE bool _sapp_wgl_ext_supported(const char* ext) {
    SOKOL_ASSERT(ext);
    if (_sapp.wgl.GetExtensionsStringEXT) {
        const char* extensions = _sapp.wgl.GetExtensionsStringEXT();
        if (extensions) {
            if (_sapp_wgl_has_ext(ext, extensions)) {
                return true;
            }
        }
    }
    if (_sapp.wgl.GetExtensionsStringARB) {
        const char* extensions = _sapp.wgl.GetExtensionsStringARB(_sapp.wgl.GetCurrentDC());
        if (extensions) {
            if (_sapp_wgl_has_ext(ext, extensions)) {
                return true;
            }
        }
    }
    return false;
}

_SOKOL_PRIVATE void _sapp_wgl_load_extensions(void) {
    SOKOL_ASSERT(_sapp.wgl.msg_dc);
    PIXELFORMATDESCRIPTOR pfd;
    _sapp_clear(&pfd, sizeof(pfd));
    pfd.nSize = sizeof(pfd);
    pfd.nVersion = 1;
    pfd.dwFlags = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER;
    pfd.iPixelType = PFD_TYPE_RGBA;
    pfd.cColorBits = 24;
    if (!SetPixelFormat(_sapp.wgl.msg_dc, ChoosePixelFormat(_sapp.wgl.msg_dc, &pfd), &pfd)) {
        _SAPP_PANIC(WIN32_DUMMY_CONTEXT_SET_PIXELFORMAT_FAILED);
    }
    HGLRC rc = _sapp.wgl.CreateContext(_sapp.wgl.msg_dc);
    if (!rc) {
        _SAPP_PANIC(WIN32_CREATE_DUMMY_CONTEXT_FAILED);
    }
    if (!_sapp.wgl.MakeCurrent(_sapp.wgl.msg_dc, rc)) {
        _SAPP_PANIC(WIN32_DUMMY_CONTEXT_MAKE_CURRENT_FAILED);
    }
    _sapp.wgl.GetExtensionsStringEXT = (PFNWGLGETEXTENSIONSSTRINGEXTPROC)(void*) _sapp.wgl.GetProcAddress("wglGetExtensionsStringEXT");
    _sapp.wgl.GetExtensionsStringARB = (PFNWGLGETEXTENSIONSSTRINGARBPROC)(void*) _sapp.wgl.GetProcAddress("wglGetExtensionsStringARB");
    _sapp.wgl.CreateContextAttribsARB = (PFNWGLCREATECONTEXTATTRIBSARBPROC)(void*) _sapp.wgl.GetProcAddress("wglCreateContextAttribsARB");
    _sapp.wgl.SwapIntervalEXT = (PFNWGLSWAPINTERVALEXTPROC)(void*) _sapp.wgl.GetProcAddress("wglSwapIntervalEXT");
    _sapp.wgl.GetPixelFormatAttribivARB = (PFNWGLGETPIXELFORMATATTRIBIVARBPROC)(void*) _sapp.wgl.GetProcAddress("wglGetPixelFormatAttribivARB");
    _sapp.wgl.arb_multisample = _sapp_wgl_ext_supported("WGL_ARB_multisample");
    _sapp.wgl.arb_create_context = _sapp_wgl_ext_supported("WGL_ARB_create_context");
    _sapp.wgl.arb_create_context_profile = _sapp_wgl_ext_supported("WGL_ARB_create_context_profile");
    _sapp.wgl.ext_swap_control = _sapp_wgl_ext_supported("WGL_EXT_swap_control");
    _sapp.wgl.arb_pixel_format = _sapp_wgl_ext_supported("WGL_ARB_pixel_format");
    _sapp.wgl.MakeCurrent(_sapp.wgl.msg_dc, 0);
    _sapp.wgl.DeleteContext(rc);
}

_SOKOL_PRIVATE int _sapp_wgl_attrib(int pixel_format, int attrib) {
    SOKOL_ASSERT(_sapp.wgl.arb_pixel_format);
    int value = 0;
    if (!_sapp.wgl.GetPixelFormatAttribivARB(_sapp.win32.dc, pixel_format, 0, 1, &attrib, &value)) {
        _SAPP_PANIC(WIN32_GET_PIXELFORMAT_ATTRIB_FAILED);
    }
    return value;
}

_SOKOL_PRIVATE void _sapp_wgl_attribiv(int pixel_format, int num_attribs, const int* attribs, int* results) {
    SOKOL_ASSERT(_sapp.wgl.arb_pixel_format);
    if (!_sapp.wgl.GetPixelFormatAttribivARB(_sapp.win32.dc, pixel_format, 0, num_attribs, attribs, results)) {
        _SAPP_PANIC(WIN32_GET_PIXELFORMAT_ATTRIB_FAILED);
    }
}

_SOKOL_PRIVATE int _sapp_wgl_find_pixel_format(void) {
    SOKOL_ASSERT(_sapp.win32.dc);
    SOKOL_ASSERT(_sapp.wgl.arb_pixel_format);

    #define _sapp_wgl_num_query_tags (12)
    const int query_tags[_sapp_wgl_num_query_tags] = {
        WGL_SUPPORT_OPENGL_ARB,
        WGL_DRAW_TO_WINDOW_ARB,
        WGL_PIXEL_TYPE_ARB,
        WGL_ACCELERATION_ARB,
        WGL_DOUBLE_BUFFER_ARB,
        WGL_RED_BITS_ARB,
        WGL_GREEN_BITS_ARB,
        WGL_BLUE_BITS_ARB,
        WGL_ALPHA_BITS_ARB,
        WGL_DEPTH_BITS_ARB,
        WGL_STENCIL_BITS_ARB,
        WGL_SAMPLES_ARB,
    };
    const int result_support_opengl_index = 0;
    const int result_draw_to_window_index = 1;
    const int result_pixel_type_index = 2;
    const int result_acceleration_index = 3;
    const int result_double_buffer_index = 4;
    const int result_red_bits_index = 5;
    const int result_green_bits_index = 6;
    const int result_blue_bits_index = 7;
    const int result_alpha_bits_index = 8;
    const int result_depth_bits_index = 9;
    const int result_stencil_bits_index = 10;
    const int result_samples_index = 11;

    int query_results[_sapp_wgl_num_query_tags] = {0};
    // Drop the last item if multisample extension is not supported.
    //  If in future querying with multiple extensions, will have to shuffle index values to have active extensions on the end.
    int query_count = _sapp_wgl_num_query_tags;
    if (!_sapp.wgl.arb_multisample) {
        query_count = _sapp_wgl_num_query_tags - 1;
    }

    int native_count = _sapp_wgl_attrib(1, WGL_NUMBER_PIXEL_FORMATS_ARB);

    _sapp_gl_fbconfig desired;
    _sapp_gl_init_fbconfig(&desired);
    desired.red_bits = 8;
    desired.green_bits = 8;
    desired.blue_bits = 8;
    desired.alpha_bits = 8;
    desired.depth_bits = 24;
    desired.stencil_bits = 8;
    desired.doublebuffer = true;
    desired.samples = (_sapp.sample_count > 1) ? _sapp.sample_count : 0;

    int pixel_format = 0;

    _sapp_gl_fbselect fbselect;
    _sapp_gl_init_fbselect(&fbselect);
    for (int i = 0; i < native_count; i++) {
        const int n = i + 1;
        _sapp_wgl_attribiv(n, query_count, query_tags, query_results);

        if (query_results[result_support_opengl_index] == 0
            || query_results[result_draw_to_window_index] == 0
            || query_results[result_pixel_type_index] != WGL_TYPE_RGBA_ARB
            || query_results[result_acceleration_index] == WGL_NO_ACCELERATION_ARB)
        {
            continue;
        }

        _sapp_gl_fbconfig u;
        _sapp_clear(&u, sizeof(u));
        u.red_bits     = query_results[result_red_bits_index];
        u.green_bits   = query_results[result_green_bits_index];
        u.blue_bits    = query_results[result_blue_bits_index];
        u.alpha_bits   = query_results[result_alpha_bits_index];
        u.depth_bits   = query_results[result_depth_bits_index];
        u.stencil_bits = query_results[result_stencil_bits_index];
        u.doublebuffer = 0 != query_results[result_double_buffer_index];
        u.samples = query_results[result_samples_index]; // NOTE: If arb_multisample is not supported  - just takes the default 0

        // Test if this pixel format is better than the previous one
        if (_sapp_gl_select_fbconfig(&fbselect, &desired, &u)) {
            pixel_format = (uintptr_t)n;

            // Early exit if matching as good as possible
            if (fbselect.best_match) {
                break;
            }
        }
    }

    return pixel_format;
}

_SOKOL_PRIVATE void _sapp_wgl_create_context(void) {
    int pixel_format = _sapp_wgl_find_pixel_format();
    if (0 == pixel_format) {
        _SAPP_PANIC(WIN32_WGL_FIND_PIXELFORMAT_FAILED);
    }
    PIXELFORMATDESCRIPTOR pfd;
    if (!DescribePixelFormat(_sapp.win32.dc, pixel_format, sizeof(pfd), &pfd)) {
        _SAPP_PANIC(WIN32_WGL_DESCRIBE_PIXELFORMAT_FAILED);
    }
    if (!SetPixelFormat(_sapp.win32.dc, pixel_format, &pfd)) {
        _SAPP_PANIC(WIN32_WGL_SET_PIXELFORMAT_FAILED);
    }
    if (!_sapp.wgl.arb_create_context) {
        _SAPP_PANIC(WIN32_WGL_ARB_CREATE_CONTEXT_REQUIRED);
    }
    if (!_sapp.wgl.arb_create_context_profile) {
        _SAPP_PANIC(WIN32_WGL_ARB_CREATE_CONTEXT_PROFILE_REQUIRED);
    }
    const int attrs[] = {
        WGL_CONTEXT_MAJOR_VERSION_ARB, _sapp.desc.gl_major_version,
        WGL_CONTEXT_MINOR_VERSION_ARB, _sapp.desc.gl_minor_version,
#if defined(SOKOL_DEBUG)
        WGL_CONTEXT_FLAGS_ARB, WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB | WGL_CONTEXT_DEBUG_BIT_ARB,
#else
        WGL_CONTEXT_FLAGS_ARB, WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB,
#endif
        WGL_CONTEXT_PROFILE_MASK_ARB, WGL_CONTEXT_CORE_PROFILE_BIT_ARB,
        0, 0
    };
    _sapp.wgl.gl_ctx = _sapp.wgl.CreateContextAttribsARB(_sapp.win32.dc, 0, attrs);
    if (!_sapp.wgl.gl_ctx) {
        const DWORD err = GetLastError();
        if (err == (0xc0070000 | ERROR_INVALID_VERSION_ARB)) {
            _SAPP_PANIC(WIN32_WGL_OPENGL_VERSION_NOT_SUPPORTED);
        }
        else if (err == (0xc0070000 | ERROR_INVALID_PROFILE_ARB)) {
            _SAPP_PANIC(WIN32_WGL_OPENGL_PROFILE_NOT_SUPPORTED);
        }
        else if (err == (0xc0070000 | ERROR_INCOMPATIBLE_DEVICE_CONTEXTS_ARB)) {
            _SAPP_PANIC(WIN32_WGL_INCOMPATIBLE_DEVICE_CONTEXT);
        }
        else {
            _SAPP_PANIC(WIN32_WGL_CREATE_CONTEXT_ATTRIBS_FAILED_OTHER);
        }
    }
    _sapp.wgl.MakeCurrent(_sapp.win32.dc, _sapp.wgl.gl_ctx);
    if (_sapp.wgl.ext_swap_control) {
        /* FIXME: DwmIsCompositionEnabled() (see GLFW) */
        _sapp.wgl.SwapIntervalEXT(_sapp.swap_interval);
    }
    const uint32_t gl_framebuffer_binding = 0x8CA6;
    _sapp.wgl.GetIntegerv(gl_framebuffer_binding, (int32_t*)&_sapp.gl.framebuffer);
}

_SOKOL_PRIVATE void _sapp_wgl_destroy_context(void) {
    SOKOL_ASSERT(_sapp.wgl.gl_ctx);
    _sapp.wgl.DeleteContext(_sapp.wgl.gl_ctx);
    _sapp.wgl.gl_ctx = 0;
}

_SOKOL_PRIVATE void _sapp_wgl_swap_buffers(void) {
    SOKOL_ASSERT(_sapp.win32.dc);
    /* FIXME: DwmIsCompositionEnabled? (see GLFW) */
    SwapBuffers(_sapp.win32.dc);
}
#endif /* SOKOL_GLCORE */

_SOKOL_PRIVATE bool _sapp_win32_wide_to_utf8(const wchar_t* src, char* dst, int dst_num_bytes) {
    SOKOL_ASSERT(src && dst && (dst_num_bytes > 1));
    _sapp_clear(dst, (size_t)dst_num_bytes);
    const int bytes_needed = WideCharToMultiByte(CP_UTF8, 0, src, -1, NULL, 0, NULL, NULL);
    if (bytes_needed <= dst_num_bytes) {
        WideCharToMultiByte(CP_UTF8, 0, src, -1, dst, dst_num_bytes, NULL, NULL);
        return true;
    }
    else {
        return false;
    }
}

/* updates current window and framebuffer size from the window's client rect, returns true if size has changed */
_SOKOL_PRIVATE bool _sapp_win32_update_dimensions(void) {
    RECT rect;
    if (GetClientRect(_sapp.win32.hwnd, &rect)) {
        float window_width = (float)(rect.right - rect.left) / _sapp.win32.dpi.window_scale;
        float window_height = (float)(rect.bottom - rect.top) / _sapp.win32.dpi.window_scale;
        _sapp.window_width = (int)roundf(window_width);
        _sapp.window_height = (int)roundf(window_height);
        int fb_width = (int)roundf(window_width * _sapp.win32.dpi.content_scale);
        int fb_height = (int)roundf(window_height * _sapp.win32.dpi.content_scale);
        /* prevent a framebuffer size of 0 when window is minimized */
        if (0 == fb_width) {
            fb_width = 1;
        }
        if (0 == fb_height) {
            fb_height = 1;
        }
        if ((fb_width != _sapp.framebuffer_width) || (fb_height != _sapp.framebuffer_height)) {
            _sapp.framebuffer_width = fb_width;
            _sapp.framebuffer_height = fb_height;
            return true;
        }
    }
    else {
        _sapp.window_width = _sapp.window_height = 1;
        _sapp.framebuffer_width = _sapp.framebuffer_height = 1;
    }
    return false;
}

_SOKOL_PRIVATE void _sapp_win32_set_fullscreen(bool fullscreen, UINT swp_flags) {
    HMONITOR monitor = MonitorFromWindow(_sapp.win32.hwnd, MONITOR_DEFAULTTONEAREST);
    MONITORINFO minfo;
    _sapp_clear(&minfo, sizeof(minfo));
    minfo.cbSize = sizeof(MONITORINFO);
    GetMonitorInfo(monitor, &minfo);
    const RECT mr = minfo.rcMonitor;
    const int monitor_w = mr.right - mr.left;
    const int monitor_h = mr.bottom - mr.top;

    const DWORD win_ex_style = WS_EX_APPWINDOW | WS_EX_WINDOWEDGE;
    DWORD win_style;
    RECT rect = { 0, 0, 0, 0 };

    _sapp.fullscreen = fullscreen;
    if (!_sapp.fullscreen) {
        win_style = WS_CLIPSIBLINGS | WS_CLIPCHILDREN | WS_CAPTION | WS_SYSMENU | WS_MINIMIZEBOX | WS_MAXIMIZEBOX | WS_SIZEBOX;
        rect = _sapp.win32.stored_window_rect;
    }
    else {
        GetWindowRect(_sapp.win32.hwnd, &_sapp.win32.stored_window_rect);
        win_style = WS_POPUP | WS_SYSMENU | WS_VISIBLE;
        rect.left = mr.left;
        rect.top = mr.top;
        rect.right = rect.left + monitor_w;
        rect.bottom = rect.top + monitor_h;
        AdjustWindowRectEx(&rect, win_style, FALSE, win_ex_style);
    }
    const int win_w = rect.right - rect.left;
    const int win_h = rect.bottom - rect.top;
    const int win_x = rect.left;
    const int win_y = rect.top;
    SetWindowLongPtr(_sapp.win32.hwnd, GWL_STYLE, win_style);
    SetWindowPos(_sapp.win32.hwnd, HWND_TOP, win_x, win_y, win_w, win_h, swp_flags | SWP_FRAMECHANGED);
}

_SOKOL_PRIVATE void _sapp_win32_toggle_fullscreen(void) {
    _sapp_win32_set_fullscreen(!_sapp.fullscreen, SWP_SHOWWINDOW);
}

_SOKOL_PRIVATE void _sapp_win32_init_cursor(sapp_mouse_cursor cursor) {
    SOKOL_ASSERT((cursor >= 0) && (cursor < _SAPP_MOUSECURSOR_NUM));
    // NOTE: the OCR_* constants are only defined if OEMRESOURCE is defined
    // before windows.h is included, but we can't guarantee that because
    // the sokol_app.h implementation may be included with other implementations
    // in the same compilation unit
    int id = 0;
    switch (cursor) {
        case SAPP_MOUSECURSOR_ARROW:            id = 32512; break;  // OCR_NORMAL
        case SAPP_MOUSECURSOR_IBEAM:            id = 32513; break;  // OCR_IBEAM
        case SAPP_MOUSECURSOR_CROSSHAIR:        id = 32515; break;  // OCR_CROSS
        case SAPP_MOUSECURSOR_POINTING_HAND:    id = 32649; break;  // OCR_HAND
        case SAPP_MOUSECURSOR_RESIZE_EW:        id = 32644; break;  // OCR_SIZEWE
        case SAPP_MOUSECURSOR_RESIZE_NS:        id = 32645; break;  // OCR_SIZENS
        case SAPP_MOUSECURSOR_RESIZE_NWSE:      id = 32642; break;  // OCR_SIZENWSE
        case SAPP_MOUSECURSOR_RESIZE_NESW:      id = 32643; break;  // OCR_SIZENESW
        case SAPP_MOUSECURSOR_RESIZE_ALL:       id = 32646; break;  // OCR_SIZEALL
        case SAPP_MOUSECURSOR_NOT_ALLOWED:      id = 32648; break;  // OCR_NO
        default: break;
    }
    if (id != 0) {
        _sapp.win32.cursors[cursor] = (HCURSOR)LoadImageW(NULL, MAKEINTRESOURCEW(id), IMAGE_CURSOR, 0, 0, LR_DEFAULTSIZE|LR_SHARED);
    }
    // fallback: default cursor
    if (0 == _sapp.win32.cursors[cursor]) {
        // 32512 => IDC_ARROW
        _sapp.win32.cursors[cursor] = LoadCursorW(NULL, MAKEINTRESOURCEW(32512));
    }
    SOKOL_ASSERT(0 != _sapp.win32.cursors[cursor]);
}

_SOKOL_PRIVATE void _sapp_win32_init_cursors(void) {
    for (int i = 0; i < _SAPP_MOUSECURSOR_NUM; i++) {
        _sapp_win32_init_cursor((sapp_mouse_cursor)i);
    }
}

_SOKOL_PRIVATE bool _sapp_win32_cursor_in_content_area(void) {
    POINT pos;
    if (!GetCursorPos(&pos)) {
        return false;
    }
    if (WindowFromPoint(pos) != _sapp.win32.hwnd) {
        return false;
    }
    RECT area;
    GetClientRect(_sapp.win32.hwnd, &area);
    ClientToScreen(_sapp.win32.hwnd, (POINT*)&area.left);
    ClientToScreen(_sapp.win32.hwnd, (POINT*)&area.right);
    return PtInRect(&area, pos) == TRUE;
}

_SOKOL_PRIVATE void _sapp_win32_update_cursor(sapp_mouse_cursor cursor, bool shown, bool skip_area_test) {
    // NOTE: when called from WM_SETCURSOR, the area test would be redundant
    if (!skip_area_test) {
        if (!_sapp_win32_cursor_in_content_area()) {
            return;
        }
    }
    if (!shown) {
        SetCursor(NULL);
    }
    else {
        SOKOL_ASSERT((cursor >= 0) && (cursor < _SAPP_MOUSECURSOR_NUM));
        SOKOL_ASSERT(0 != _sapp.win32.cursors[cursor]);
        SetCursor(_sapp.win32.cursors[cursor]);
    }
}

_SOKOL_PRIVATE void _sapp_win32_capture_mouse(uint8_t btn_mask) {
    if (0 == _sapp.win32.mouse_capture_mask) {
        SetCapture(_sapp.win32.hwnd);
    }
    _sapp.win32.mouse_capture_mask |= btn_mask;
}

_SOKOL_PRIVATE void _sapp_win32_release_mouse(uint8_t btn_mask) {
    if (0 != _sapp.win32.mouse_capture_mask) {
        _sapp.win32.mouse_capture_mask &= ~btn_mask;
        if (0 == _sapp.win32.mouse_capture_mask) {
            ReleaseCapture();
        }
    }
}

_SOKOL_PRIVATE void _sapp_win32_lock_mouse(bool lock) {
    if (lock == _sapp.mouse.locked) {
        return;
    }
    _sapp.mouse.dx = 0.0f;
    _sapp.mouse.dy = 0.0f;
    _sapp.mouse.locked = lock;
    _sapp_win32_release_mouse(0xFF);
    if (_sapp.mouse.locked) {
        /* store the current mouse position, so it can be restored when unlocked */
        POINT pos;
        BOOL res = GetCursorPos(&pos);
        SOKOL_ASSERT(res); _SOKOL_UNUSED(res);
        _sapp.win32.mouse_locked_x = pos.x;
        _sapp.win32.mouse_locked_y = pos.y;

        /* while the mouse is locked, make the mouse cursor invisible and
           confine the mouse movement to a small rectangle inside our window
           (so that we don't miss any mouse up events)
        */
        RECT client_rect = {
            _sapp.win32.mouse_locked_x,
            _sapp.win32.mouse_locked_y,
            _sapp.win32.mouse_locked_x,
            _sapp.win32.mouse_locked_y
        };
        ClipCursor(&client_rect);

        /* make the mouse cursor invisible, this will stack with sapp_show_mouse() */
        ShowCursor(FALSE);

        /* enable raw input for mouse, starts sending WM_INPUT messages to WinProc (see GLFW) */
        const RAWINPUTDEVICE rid = {
            0x01,   // usUsagePage: HID_USAGE_PAGE_GENERIC
            0x02,   // usUsage: HID_USAGE_GENERIC_MOUSE
            0,      // dwFlags
            _sapp.win32.hwnd    // hwndTarget
        };
        if (!RegisterRawInputDevices(&rid, 1, sizeof(rid))) {
            _SAPP_ERROR(WIN32_REGISTER_RAW_INPUT_DEVICES_FAILED_MOUSE_LOCK);
        }
        /* in case the raw mouse device only supports absolute position reporting,
           we need to skip the dx/dy compution for the first WM_INPUT event
        */
        _sapp.win32.raw_input_mousepos_valid = false;
    }
    else {
        /* disable raw input for mouse */
        const RAWINPUTDEVICE rid = { 0x01, 0x02, RIDEV_REMOVE, NULL };
        if (!RegisterRawInputDevices(&rid, 1, sizeof(rid))) {
            _SAPP_ERROR(WIN32_REGISTER_RAW_INPUT_DEVICES_FAILED_MOUSE_UNLOCK);
        }

        /* let the mouse roam freely again */
        ClipCursor(NULL);
        ShowCursor(TRUE);

        /* restore the 'pre-locked' mouse position */
        BOOL res = SetCursorPos(_sapp.win32.mouse_locked_x, _sapp.win32.mouse_locked_y);
        SOKOL_ASSERT(res); _SOKOL_UNUSED(res);
    }
}

_SOKOL_PRIVATE bool _sapp_win32_update_monitor(void) {
    const HMONITOR cur_monitor = MonitorFromWindow(_sapp.win32.hwnd, MONITOR_DEFAULTTONULL);
    if (cur_monitor != _sapp.win32.hmonitor) {
        _sapp.win32.hmonitor = cur_monitor;
        return true;
    }
    else {
        return false;
    }
}

_SOKOL_PRIVATE uint32_t _sapp_win32_mods(void) {
    uint32_t mods = 0;
    if (GetKeyState(VK_SHIFT) & (1<<15)) {
        mods |= SAPP_MODIFIER_SHIFT;
    }
    if (GetKeyState(VK_CONTROL) & (1<<15)) {
        mods |= SAPP_MODIFIER_CTRL;
    }
    if (GetKeyState(VK_MENU) & (1<<15)) {
        mods |= SAPP_MODIFIER_ALT;
    }
    if ((GetKeyState(VK_LWIN) | GetKeyState(VK_RWIN)) & (1<<15)) {
        mods |= SAPP_MODIFIER_SUPER;
    }
    const bool swapped = (TRUE == GetSystemMetrics(SM_SWAPBUTTON));
    if (GetAsyncKeyState(VK_LBUTTON)) {
        mods |= swapped ? SAPP_MODIFIER_RMB : SAPP_MODIFIER_LMB;
    }
    if (GetAsyncKeyState(VK_RBUTTON)) {
        mods |= swapped ? SAPP_MODIFIER_LMB : SAPP_MODIFIER_RMB;
    }
    if (GetAsyncKeyState(VK_MBUTTON)) {
        mods |= SAPP_MODIFIER_MMB;
    }
    return mods;
}

_SOKOL_PRIVATE void _sapp_win32_mouse_update(LPARAM lParam) {
    if (!_sapp.mouse.locked) {
        const float new_x  = (float)GET_X_LPARAM(lParam) * _sapp.win32.dpi.mouse_scale;
        const float new_y = (float)GET_Y_LPARAM(lParam) * _sapp.win32.dpi.mouse_scale;
        if (_sapp.mouse.pos_valid) {
            // don't update dx/dy in the very first event
            _sapp.mouse.dx = new_x - _sapp.mouse.x;
            _sapp.mouse.dy = new_y - _sapp.mouse.y;
        }
        _sapp.mouse.x = new_x;
        _sapp.mouse.y = new_y;
        _sapp.mouse.pos_valid = true;
    }
}

_SOKOL_PRIVATE void _sapp_win32_mouse_event(sapp_event_type type, sapp_mousebutton btn) {
    if (_sapp_events_enabled()) {
        _sapp_init_event(type);
        _sapp.event.modifiers = _sapp_win32_mods();
        _sapp.event.mouse_button = btn;
        _sapp_call_event(&_sapp.event);
    }
}

_SOKOL_PRIVATE void _sapp_win32_scroll_event(float x, float y) {
    if (_sapp_events_enabled()) {
        _sapp_init_event(SAPP_EVENTTYPE_MOUSE_SCROLL);
        _sapp.event.modifiers = _sapp_win32_mods();
        _sapp.event.scroll_x = -x / 30.0f;
        _sapp.event.scroll_y = y / 30.0f;
        _sapp_call_event(&_sapp.event);
    }
}

_SOKOL_PRIVATE void _sapp_win32_key_event(sapp_event_type type, int vk, bool repeat) {
    if (_sapp_events_enabled() && (vk < SAPP_MAX_KEYCODES)) {
        _sapp_init_event(type);
        _sapp.event.modifiers = _sapp_win32_mods();
        _sapp.event.key_code = _sapp.keycodes[vk];
        _sapp.event.key_repeat = repeat;
        _sapp_call_event(&_sapp.event);
        /* check if a CLIPBOARD_PASTED event must be sent too */
        if (_sapp.clipboard.enabled &&
            (type == SAPP_EVENTTYPE_KEY_DOWN) &&
            (_sapp.event.modifiers == SAPP_MODIFIER_CTRL) &&
            (_sapp.event.key_code == SAPP_KEYCODE_V))
        {
            _sapp_init_event(SAPP_EVENTTYPE_CLIPBOARD_PASTED);
            _sapp_call_event(&_sapp.event);
        }
    }
}

_SOKOL_PRIVATE void _sapp_win32_char_event(uint32_t c, bool repeat) {
    if (_sapp_events_enabled() && (c >= 32)) {
        _sapp_init_event(SAPP_EVENTTYPE_CHAR);
        _sapp.event.modifiers = _sapp_win32_mods();
        _sapp.event.char_code = c;
        _sapp.event.key_repeat = repeat;
        _sapp_call_event(&_sapp.event);
    }
}

_SOKOL_PRIVATE void _sapp_win32_dpi_changed(HWND hWnd, LPRECT proposed_win_rect) {
    /* called on WM_DPICHANGED, which will only be sent to the application
        if sapp_desc.high_dpi is true and the Windows version is recent enough
        to support DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2
    */
    SOKOL_ASSERT(_sapp.desc.high_dpi);
    HINSTANCE user32 = LoadLibraryA("user32.dll");
    if (!user32) {
        return;
    }
    typedef UINT(WINAPI * GETDPIFORWINDOW_T)(HWND hwnd);
    GETDPIFORWINDOW_T fn_getdpiforwindow = (GETDPIFORWINDOW_T)(void*)GetProcAddress(user32, "GetDpiForWindow");
    if (fn_getdpiforwindow) {
        UINT dpix = fn_getdpiforwindow(_sapp.win32.hwnd);
        // NOTE: for high-dpi apps, mouse_scale remains one
        _sapp.win32.dpi.window_scale = (float)dpix / 96.0f;
        _sapp.win32.dpi.content_scale = _sapp.win32.dpi.window_scale;
        _sapp.dpi_scale = _sapp.win32.dpi.window_scale;
        SetWindowPos(hWnd, 0,
            proposed_win_rect->left,
            proposed_win_rect->top,
            proposed_win_rect->right - proposed_win_rect->left,
            proposed_win_rect->bottom - proposed_win_rect->top,
            SWP_NOZORDER | SWP_NOACTIVATE);
    }
    FreeLibrary(user32);
}

_SOKOL_PRIVATE void _sapp_win32_files_dropped(HDROP hdrop) {
    if (!_sapp.drop.enabled) {
        return;
    }
    _sapp_clear_drop_buffer();
    bool drop_failed = false;
    const int count = (int) DragQueryFileW(hdrop, 0xffffffff, NULL, 0);
    _sapp.drop.num_files = (count > _sapp.drop.max_files) ? _sapp.drop.max_files : count;
    for (UINT i = 0;  i < (UINT)_sapp.drop.num_files;  i++) {
        const UINT num_chars = DragQueryFileW(hdrop, i, NULL, 0) + 1;
        WCHAR* buffer = (WCHAR*) _sapp_malloc_clear(num_chars * sizeof(WCHAR));
        DragQueryFileW(hdrop, i, buffer, num_chars);
        if (!_sapp_win32_wide_to_utf8(buffer, _sapp_dropped_file_path_ptr((int)i), _sapp.drop.max_path_length)) {
            _SAPP_ERROR(DROPPED_FILE_PATH_TOO_LONG);
            drop_failed = true;
        }
        _sapp_free(buffer);
    }
    DragFinish(hdrop);
    if (!drop_failed) {
        if (_sapp_events_enabled()) {
            _sapp_init_event(SAPP_EVENTTYPE_FILES_DROPPED);
            _sapp.event.modifiers = _sapp_win32_mods();
            _sapp_call_event(&_sapp.event);
        }
    }
    else {
        _sapp_clear_drop_buffer();
        _sapp.drop.num_files = 0;
    }
}

_SOKOL_PRIVATE void _sapp_win32_timing_measure(void) {
    #if defined(SOKOL_D3D11)
        // on D3D11, use the more precise DXGI timestamp
        if (_sapp.d3d11.use_dxgi_frame_stats) {
            DXGI_FRAME_STATISTICS dxgi_stats;
            _sapp_clear(&dxgi_stats, sizeof(dxgi_stats));
            HRESULT hr = _sapp_dxgi_GetFrameStatistics(_sapp.d3d11.swap_chain, &dxgi_stats);
            if (SUCCEEDED(hr)) {
                if (dxgi_stats.SyncRefreshCount != _sapp.d3d11.sync_refresh_count) {
                    if ((_sapp.d3d11.sync_refresh_count + 1) != dxgi_stats.SyncRefreshCount) {
                        _sapp_timing_discontinuity(&_sapp.timing);
                    }
                    _sapp.d3d11.sync_refresh_count = dxgi_stats.SyncRefreshCount;
                    LARGE_INTEGER qpc = dxgi_stats.SyncQPCTime;
                    const uint64_t now = (uint64_t)_sapp_int64_muldiv(qpc.QuadPart - _sapp.timing.timestamp.win.start.QuadPart, 1000000000, _sapp.timing.timestamp.win.freq.QuadPart);
                    _sapp_timing_external(&_sapp.timing, (double)now / 1000000000.0);
                }
                return;
            }
        }
        // fallback if swap model isn't "flip-discard" or GetFrameStatistics failed for another reason
        _sapp_timing_measure(&_sapp.timing);
    #endif
    #if defined(SOKOL_GLCORE)
        _sapp_timing_measure(&_sapp.timing);
    #endif
    #if defined(SOKOL_NOAPI)
        _sapp_timing_measure(&_sapp.timing);
    #endif
}

_SOKOL_PRIVATE LRESULT CALLBACK _sapp_win32_wndproc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam) {
    if (!_sapp.win32.in_create_window) {
        switch (uMsg) {
            case WM_CLOSE:
                /* only give user a chance to intervene when sapp_quit() wasn't already called */
                if (!_sapp.quit_ordered) {
                    /* if window should be closed and event handling is enabled, give user code
                        a change to intervene via sapp_cancel_quit()
                    */
                    _sapp.quit_requested = true;
                    _sapp_win32_app_event(SAPP_EVENTTYPE_QUIT_REQUESTED);
                    /* if user code hasn't intervened, quit the app */
                    if (_sapp.quit_requested) {
                        _sapp.quit_ordered = true;
                    }
                }
                if (_sapp.quit_ordered) {
                    PostQuitMessage(0);
                }
                return 0;
            case WM_SYSCOMMAND:
                switch (wParam & 0xFFF0) {
                    case SC_SCREENSAVE:
                    case SC_MONITORPOWER:
                        if (_sapp.fullscreen) {
                            /* disable screen saver and blanking in fullscreen mode */
                            return 0;
                        }
                        break;
                    case SC_KEYMENU:
                        /* user trying to access menu via ALT */
                        return 0;
                }
                break;
            case WM_ERASEBKGND:
                return 1;
            case WM_SIZE:
                {
                    const bool iconified = wParam == SIZE_MINIMIZED;
                    if (iconified != _sapp.win32.iconified) {
                        _sapp.win32.iconified = iconified;
                        if (iconified) {
                            _sapp_win32_app_event(SAPP_EVENTTYPE_ICONIFIED);
                        }
                        else {
                            _sapp_win32_app_event(SAPP_EVENTTYPE_RESTORED);
                        }
                    }
                }
                break;
            case WM_SETFOCUS:
                _sapp_win32_app_event(SAPP_EVENTTYPE_FOCUSED);
                break;
            case WM_KILLFOCUS:
                /* if focus is lost for any reason, and we're in mouse locked mode, disable mouse lock */
                if (_sapp.mouse.locked) {
                    _sapp_win32_lock_mouse(false);
                }
                _sapp_win32_app_event(SAPP_EVENTTYPE_UNFOCUSED);
                break;
            case WM_SETCURSOR:
                if (LOWORD(lParam) == HTCLIENT) {
                    _sapp_win32_update_cursor(_sapp.mouse.current_cursor, _sapp.mouse.shown, true);
                    return TRUE;
                }
                break;
            case WM_DPICHANGED:
            {
                /* Update window's DPI and size if its moved to another monitor with a different DPI
                   Only sent if DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2 is used.
                */
                _sapp_win32_dpi_changed(hWnd, (LPRECT)lParam);
                break;
            }
            case WM_LBUTTONDOWN:
                _sapp_win32_mouse_update(lParam);
                _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_DOWN, SAPP_MOUSEBUTTON_LEFT);
                _sapp_win32_capture_mouse(1<<SAPP_MOUSEBUTTON_LEFT);
                break;
            case WM_RBUTTONDOWN:
                _sapp_win32_mouse_update(lParam);
                _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_DOWN, SAPP_MOUSEBUTTON_RIGHT);
                _sapp_win32_capture_mouse(1<<SAPP_MOUSEBUTTON_RIGHT);
                break;
            case WM_MBUTTONDOWN:
                _sapp_win32_mouse_update(lParam);
                _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_DOWN, SAPP_MOUSEBUTTON_MIDDLE);
                _sapp_win32_capture_mouse(1<<SAPP_MOUSEBUTTON_MIDDLE);
                break;
            case WM_LBUTTONUP:
                _sapp_win32_mouse_update(lParam);
                _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_UP, SAPP_MOUSEBUTTON_LEFT);
                _sapp_win32_release_mouse(1<<SAPP_MOUSEBUTTON_LEFT);
                break;
            case WM_RBUTTONUP:
                _sapp_win32_mouse_update(lParam);
                _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_UP, SAPP_MOUSEBUTTON_RIGHT);
                _sapp_win32_release_mouse(1<<SAPP_MOUSEBUTTON_RIGHT);
                break;
            case WM_MBUTTONUP:
                _sapp_win32_mouse_update(lParam);
                _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_UP, SAPP_MOUSEBUTTON_MIDDLE);
                _sapp_win32_release_mouse(1<<SAPP_MOUSEBUTTON_MIDDLE);
                break;
            case WM_MOUSEMOVE:
                if (!_sapp.mouse.locked) {
                    _sapp_win32_mouse_update(lParam);
                    if (!_sapp.win32.mouse_tracked) {
                        _sapp.win32.mouse_tracked = true;
                        TRACKMOUSEEVENT tme;
                        _sapp_clear(&tme, sizeof(tme));
                        tme.cbSize = sizeof(tme);
                        tme.dwFlags = TME_LEAVE;
                        tme.hwndTrack = _sapp.win32.hwnd;
                        TrackMouseEvent(&tme);
                        _sapp.mouse.dx = 0.0f;
                        _sapp.mouse.dy = 0.0f;
                        _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_ENTER, SAPP_MOUSEBUTTON_INVALID);
                    }
                    _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_MOVE, SAPP_MOUSEBUTTON_INVALID);
                }
                break;
            case WM_INPUT:
                /* raw mouse input during mouse-lock */
                if (_sapp.mouse.locked) {
                    HRAWINPUT ri = (HRAWINPUT) lParam;
                    UINT size = sizeof(_sapp.win32.raw_input_data);
                    // see: https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-getrawinputdata
                    if ((UINT)-1 == GetRawInputData(ri, RID_INPUT, &_sapp.win32.raw_input_data, &size, sizeof(RAWINPUTHEADER))) {
                        _SAPP_ERROR(WIN32_GET_RAW_INPUT_DATA_FAILED);
                        break;
                    }
                    const RAWINPUT* raw_mouse_data = (const RAWINPUT*) &_sapp.win32.raw_input_data;
                    if (raw_mouse_data->data.mouse.usFlags & MOUSE_MOVE_ABSOLUTE) {
                        /* mouse only reports absolute position
                           NOTE: This code is untested and will most likely behave wrong in Remote Desktop sessions.
                           (such remote desktop sessions are setting the MOUSE_MOVE_ABSOLUTE flag).
                           See: https://github.com/floooh/sokol/issues/806 and
                           https://github.com/microsoft/DirectXTK/commit/ef56b63f3739381e451f7a5a5bd2c9779d2a7555)
                        */
                        LONG new_x = raw_mouse_data->data.mouse.lLastX;
                        LONG new_y = raw_mouse_data->data.mouse.lLastY;
                        if (_sapp.win32.raw_input_mousepos_valid) {
                            _sapp.mouse.dx = (float) (new_x - _sapp.win32.raw_input_mousepos_x);
                            _sapp.mouse.dy = (float) (new_y - _sapp.win32.raw_input_mousepos_y);
                        }
                        _sapp.win32.raw_input_mousepos_x = new_x;
                        _sapp.win32.raw_input_mousepos_y = new_y;
                        _sapp.win32.raw_input_mousepos_valid = true;
                    }
                    else {
                        /* mouse reports movement delta (this seems to be the common case) */
                        _sapp.mouse.dx = (float) raw_mouse_data->data.mouse.lLastX;
                        _sapp.mouse.dy = (float) raw_mouse_data->data.mouse.lLastY;
                    }
                    _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_MOVE, SAPP_MOUSEBUTTON_INVALID);
                }
                break;

            case WM_MOUSELEAVE:
                if (!_sapp.mouse.locked) {
                    _sapp.mouse.dx = 0.0f;
                    _sapp.mouse.dy = 0.0f;
                    _sapp.win32.mouse_tracked = false;
                    _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_LEAVE, SAPP_MOUSEBUTTON_INVALID);
                }
                break;
            case WM_MOUSEWHEEL:
                _sapp_win32_scroll_event(0.0f, (float)((SHORT)HIWORD(wParam)));
                break;
            case WM_MOUSEHWHEEL:
                _sapp_win32_scroll_event((float)((SHORT)HIWORD(wParam)), 0.0f);
                break;
            case WM_CHAR:
                _sapp_win32_char_event((uint32_t)wParam, !!(lParam&0x40000000));
                break;
            case WM_KEYDOWN:
            case WM_SYSKEYDOWN:
                _sapp_win32_key_event(SAPP_EVENTTYPE_KEY_DOWN, (int)(HIWORD(lParam)&0x1FF), !!(lParam&0x40000000));
                break;
            case WM_KEYUP:
            case WM_SYSKEYUP:
                _sapp_win32_key_event(SAPP_EVENTTYPE_KEY_UP, (int)(HIWORD(lParam)&0x1FF), false);
                break;
            case WM_ENTERSIZEMOVE:
                SetTimer(_sapp.win32.hwnd, 1, USER_TIMER_MINIMUM, NULL);
                break;
            case WM_EXITSIZEMOVE:
                KillTimer(_sapp.win32.hwnd, 1);
                break;
            case WM_TIMER:
                _sapp_win32_timing_measure();
                _sapp_frame();
                #if defined(SOKOL_D3D11)
                    // present with DXGI_PRESENT_DO_NOT_WAIT
                    _sapp_d3d11_present(true);
                #endif
                #if defined(SOKOL_GLCORE)
                    _sapp_wgl_swap_buffers();
                #endif
                /* NOTE: resizing the swap-chain during resize leads to a substantial
                   memory spike (hundreds of megabytes for a few seconds).

                if (_sapp_win32_update_dimensions()) {
                    #if defined(SOKOL_D3D11)
                    _sapp_d3d11_resize_default_render_target();
                    #endif
                    _sapp_win32_app_event(SAPP_EVENTTYPE_RESIZED);
                }
                */
                break;
            case WM_NCLBUTTONDOWN:
                /* workaround for half-second pause when starting to move window
                    see: https://gamedev.net/forums/topic/672094-keeping-things-moving-during-win32-moveresize-events/5254386/
                */
                if (SendMessage(_sapp.win32.hwnd, WM_NCHITTEST, wParam, lParam) == HTCAPTION) {
                    POINT point;
                    GetCursorPos(&point);
                    ScreenToClient(_sapp.win32.hwnd, &point);
                    PostMessage(_sapp.win32.hwnd, WM_MOUSEMOVE, 0, ((uint32_t)point.x)|(((uint32_t)point.y) << 16));
                }
                break;
            case WM_DROPFILES:
                _sapp_win32_files_dropped((HDROP)wParam);
                break;
            case WM_DISPLAYCHANGE:
                // refresh rate might have changed
                _sapp_timing_reset(&_sapp.timing);
                break;

            default:
                break;
        }
    }
    return DefWindowProcW(hWnd, uMsg, wParam, lParam);
}

_SOKOL_PRIVATE void _sapp_win32_create_window(void) {
    WNDCLASSW wndclassw;
    _sapp_clear(&wndclassw, sizeof(wndclassw));
    wndclassw.style = CS_HREDRAW | CS_VREDRAW | CS_OWNDC;
    wndclassw.lpfnWndProc = (WNDPROC) _sapp_win32_wndproc;
    wndclassw.hInstance = GetModuleHandleW(NULL);
    wndclassw.hCursor = LoadCursor(NULL, IDC_ARROW);
    wndclassw.hIcon = LoadIcon(NULL, IDI_WINLOGO);
    wndclassw.lpszClassName = L"SOKOLAPP";
    RegisterClassW(&wndclassw);

    /* NOTE: regardless whether fullscreen is requested or not, a regular
       windowed-mode window will always be created first (however in hidden
       mode, so that no windowed-mode window pops up before the fullscreen window)
    */
    const DWORD win_ex_style = WS_EX_APPWINDOW | WS_EX_WINDOWEDGE;
    RECT rect = { 0, 0, 0, 0 };
    DWORD win_style = WS_CLIPSIBLINGS | WS_CLIPCHILDREN | WS_CAPTION | WS_SYSMENU | WS_MINIMIZEBOX | WS_MAXIMIZEBOX | WS_SIZEBOX;
    rect.right = (int) ((float)_sapp.window_width * _sapp.win32.dpi.window_scale);
    rect.bottom = (int) ((float)_sapp.window_height * _sapp.win32.dpi.window_scale);
    const bool use_default_width = 0 == _sapp.window_width;
    const bool use_default_height = 0 == _sapp.window_height;
    AdjustWindowRectEx(&rect, win_style, FALSE, win_ex_style);
    const int win_width = rect.right - rect.left;
    const int win_height = rect.bottom - rect.top;
    _sapp.win32.in_create_window = true;
    _sapp.win32.hwnd = CreateWindowExW(
        win_ex_style,               // dwExStyle
        L"SOKOLAPP",                // lpClassName
        _sapp.window_title_wide,    // lpWindowName
        win_style,                  // dwStyle
        CW_USEDEFAULT,              // X
        SW_HIDE,                    // Y (NOTE: CW_USEDEFAULT is not used for position here, but internally calls ShowWindow!
        use_default_width ? CW_USEDEFAULT : win_width, // nWidth
        use_default_height ? CW_USEDEFAULT : win_height, // nHeight (NOTE: if width is CW_USEDEFAULT, height is actually ignored)
        NULL,                       // hWndParent
        NULL,                       // hMenu
        GetModuleHandle(NULL),      // hInstance
        NULL);                      // lParam
    _sapp.win32.in_create_window = false;
    _sapp.win32.dc = GetDC(_sapp.win32.hwnd);
    _sapp.win32.hmonitor = MonitorFromWindow(_sapp.win32.hwnd, MONITOR_DEFAULTTONULL);
    SOKOL_ASSERT(_sapp.win32.dc);

    /* this will get the actual windowed-mode window size, if fullscreen
       is requested, the set_fullscreen function will then capture the
       current window rectangle, which then might be used later to
       restore the window position when switching back to windowed
    */
    _sapp_win32_update_dimensions();
    if (_sapp.fullscreen) {
        _sapp_win32_set_fullscreen(_sapp.fullscreen, SWP_HIDEWINDOW);
        _sapp_win32_update_dimensions();
    }
    ShowWindow(_sapp.win32.hwnd, SW_SHOW);
    DragAcceptFiles(_sapp.win32.hwnd, 1);
}

_SOKOL_PRIVATE void _sapp_win32_destroy_window(void) {
    DestroyWindow(_sapp.win32.hwnd); _sapp.win32.hwnd = 0;
    UnregisterClassW(L"SOKOLAPP", GetModuleHandleW(NULL));
}

_SOKOL_PRIVATE void _sapp_win32_destroy_icons(void) {
    if (_sapp.win32.big_icon) {
        DestroyIcon(_sapp.win32.big_icon);
        _sapp.win32.big_icon = 0;
    }
    if (_sapp.win32.small_icon) {
        DestroyIcon(_sapp.win32.small_icon);
        _sapp.win32.small_icon = 0;
    }
}

_SOKOL_PRIVATE void _sapp_win32_init_console(void) {
    if (_sapp.desc.win32_console_create || _sapp.desc.win32_console_attach) {
        BOOL con_valid = FALSE;
        if (_sapp.desc.win32_console_create) {
            con_valid = AllocConsole();
        }
        else if (_sapp.desc.win32_console_attach) {
            con_valid = AttachConsole(ATTACH_PARENT_PROCESS);
        }
        if (con_valid) {
            FILE* res_fp = 0;
            errno_t err;
            err = freopen_s(&res_fp, "CON", "w", stdout);
            (void)err;
            err = freopen_s(&res_fp, "CON", "w", stderr);
            (void)err;
        }
    }
    if (_sapp.desc.win32_console_utf8) {
        _sapp.win32.orig_codepage = GetConsoleOutputCP();
        SetConsoleOutputCP(CP_UTF8);
    }
}

_SOKOL_PRIVATE void _sapp_win32_restore_console(void) {
    if (_sapp.desc.win32_console_utf8) {
        SetConsoleOutputCP(_sapp.win32.orig_codepage);
    }
}

_SOKOL_PRIVATE void _sapp_win32_init_dpi(void) {

    DECLARE_HANDLE(DPI_AWARENESS_CONTEXT_T);
    typedef BOOL(WINAPI * SETPROCESSDPIAWARE_T)(void);
    typedef bool (WINAPI * SETPROCESSDPIAWARENESSCONTEXT_T)(DPI_AWARENESS_CONTEXT_T); // since Windows 10, version 1703
    typedef HRESULT(WINAPI * SETPROCESSDPIAWARENESS_T)(PROCESS_DPI_AWARENESS);
    typedef HRESULT(WINAPI * GETDPIFORMONITOR_T)(HMONITOR, MONITOR_DPI_TYPE, UINT*, UINT*);

    SETPROCESSDPIAWARE_T fn_setprocessdpiaware = 0;
    SETPROCESSDPIAWARENESS_T fn_setprocessdpiawareness = 0;
    GETDPIFORMONITOR_T fn_getdpiformonitor = 0;
    SETPROCESSDPIAWARENESSCONTEXT_T fn_setprocessdpiawarenesscontext =0;

    HINSTANCE user32 = LoadLibraryA("user32.dll");
    if (user32) {
        fn_setprocessdpiaware = (SETPROCESSDPIAWARE_T)(void*) GetProcAddress(user32, "SetProcessDPIAware");
        fn_setprocessdpiawarenesscontext = (SETPROCESSDPIAWARENESSCONTEXT_T)(void*) GetProcAddress(user32, "SetProcessDpiAwarenessContext");
    }
    HINSTANCE shcore = LoadLibraryA("shcore.dll");
    if (shcore) {
        fn_setprocessdpiawareness = (SETPROCESSDPIAWARENESS_T)(void*) GetProcAddress(shcore, "SetProcessDpiAwareness");
        fn_getdpiformonitor = (GETDPIFORMONITOR_T)(void*) GetProcAddress(shcore, "GetDpiForMonitor");
    }
    /*
        NOTE on SetProcessDpiAware() vs SetProcessDpiAwareness() vs SetProcessDpiAwarenessContext():

        These are different attempts to get DPI handling on Windows right, from oldest
        to newest. SetProcessDpiAwarenessContext() is required for the new
        DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2 method.
    */
    if (fn_setprocessdpiawareness) {
        if (_sapp.desc.high_dpi) {
            /* app requests HighDPI rendering, first try the Win10 Creator Update per-monitor-dpi awareness,
               if that fails, fall back to system-dpi-awareness
            */
            _sapp.win32.dpi.aware = true;
            DPI_AWARENESS_CONTEXT_T per_monitor_aware_v2 = (DPI_AWARENESS_CONTEXT_T)-4;
            if (!(fn_setprocessdpiawarenesscontext && fn_setprocessdpiawarenesscontext(per_monitor_aware_v2))) {
                // fallback to system-dpi-aware
                fn_setprocessdpiawareness(PROCESS_SYSTEM_DPI_AWARE);
            }
        }
        else {
            /* if the app didn't request HighDPI rendering, let Windows do the upscaling */
            _sapp.win32.dpi.aware = false;
            fn_setprocessdpiawareness(PROCESS_DPI_UNAWARE);
        }
    }
    else if (fn_setprocessdpiaware) {
        // fallback for Windows 7
        _sapp.win32.dpi.aware = true;
        fn_setprocessdpiaware();
    }
    /* get dpi scale factor for main monitor */
    if (fn_getdpiformonitor && _sapp.win32.dpi.aware) {
        POINT pt = { 1, 1 };
        HMONITOR hm = MonitorFromPoint(pt, MONITOR_DEFAULTTONEAREST);
        UINT dpix, dpiy;
        HRESULT hr = fn_getdpiformonitor(hm, MDT_EFFECTIVE_DPI, &dpix, &dpiy);
        _SOKOL_UNUSED(hr);
        SOKOL_ASSERT(SUCCEEDED(hr));
        /* clamp window scale to an integer factor */
        _sapp.win32.dpi.window_scale = (float)dpix / 96.0f;
    }
    else {
        _sapp.win32.dpi.window_scale = 1.0f;
    }
    if (_sapp.desc.high_dpi) {
        _sapp.win32.dpi.content_scale = _sapp.win32.dpi.window_scale;
        _sapp.win32.dpi.mouse_scale = 1.0f;
    }
    else {
        _sapp.win32.dpi.content_scale = 1.0f;
        _sapp.win32.dpi.mouse_scale = 1.0f / _sapp.win32.dpi.window_scale;
    }
    _sapp.dpi_scale = _sapp.win32.dpi.content_scale;
    if (user32) {
        FreeLibrary(user32);
    }
    if (shcore) {
        FreeLibrary(shcore);
    }
}

_SOKOL_PRIVATE bool _sapp_win32_set_clipboard_string(const char* str) {
    SOKOL_ASSERT(str);
    SOKOL_ASSERT(_sapp.win32.hwnd);
    SOKOL_ASSERT(_sapp.clipboard.enabled && (_sapp.clipboard.buf_size > 0));

    if (!OpenClipboard(_sapp.win32.hwnd)) {
        return false;
    }

    HANDLE object = 0;
    wchar_t* wchar_buf = 0;

    const SIZE_T wchar_buf_size = (SIZE_T)_sapp.clipboard.buf_size * sizeof(wchar_t);
    object = GlobalAlloc(GMEM_MOVEABLE, wchar_buf_size);
    if (NULL == object) {
        goto error;
    }
    wchar_buf = (wchar_t*) GlobalLock(object);
    if (NULL == wchar_buf) {
        goto error;
    }
    if (!_sapp_win32_utf8_to_wide(str, wchar_buf, (int)wchar_buf_size)) {
        goto error;
    }
    GlobalUnlock(object);
    wchar_buf = 0;
    EmptyClipboard();
    // NOTE: when successful, SetClipboardData() takes ownership of memory object!
    if (NULL == SetClipboardData(CF_UNICODETEXT, object)) {
        goto error;
    }
    CloseClipboard();
    return true;

error:
    if (wchar_buf) {
        GlobalUnlock(object);
    }
    if (object) {
        GlobalFree(object);
    }
    CloseClipboard();
    return false;
}

_SOKOL_PRIVATE const char* _sapp_win32_get_clipboard_string(void) {
    SOKOL_ASSERT(_sapp.clipboard.enabled && _sapp.clipboard.buffer);
    SOKOL_ASSERT(_sapp.win32.hwnd);
    if (!OpenClipboard(_sapp.win32.hwnd)) {
        /* silently ignore any errors and just return the current
           content of the local clipboard buffer
        */
        return _sapp.clipboard.buffer;
    }
    HANDLE object = GetClipboardData(CF_UNICODETEXT);
    if (!object) {
        CloseClipboard();
        return _sapp.clipboard.buffer;
    }
    const wchar_t* wchar_buf = (const wchar_t*) GlobalLock(object);
    if (!wchar_buf) {
        CloseClipboard();
        return _sapp.clipboard.buffer;
    }
    if (!_sapp_win32_wide_to_utf8(wchar_buf, _sapp.clipboard.buffer, _sapp.clipboard.buf_size)) {
        _SAPP_ERROR(CLIPBOARD_STRING_TOO_BIG);
    }
    GlobalUnlock(object);
    CloseClipboard();
    return _sapp.clipboard.buffer;
}

_SOKOL_PRIVATE void _sapp_win32_update_window_title(void) {
    _sapp_win32_utf8_to_wide(_sapp.window_title, _sapp.window_title_wide, sizeof(_sapp.window_title_wide));
    SetWindowTextW(_sapp.win32.hwnd, _sapp.window_title_wide);
}

_SOKOL_PRIVATE HICON _sapp_win32_create_icon_from_image(const sapp_image_desc* desc) {
    BITMAPV5HEADER bi;
    _sapp_clear(&bi, sizeof(bi));
    bi.bV5Size = sizeof(bi);
    bi.bV5Width = desc->width;
    bi.bV5Height = -desc->height;   // NOTE the '-' here to indicate that origin is top-left
    bi.bV5Planes = 1;
    bi.bV5BitCount = 32;
    bi.bV5Compression = BI_BITFIELDS;
    bi.bV5RedMask = 0x00FF0000;
    bi.bV5GreenMask = 0x0000FF00;
    bi.bV5BlueMask = 0x000000FF;
    bi.bV5AlphaMask = 0xFF000000;

    uint8_t* target = 0;
    const uint8_t* source = (const uint8_t*)desc->pixels.ptr;

    HDC dc = GetDC(NULL);
    HBITMAP color = CreateDIBSection(dc, (BITMAPINFO*)&bi, DIB_RGB_COLORS, (void**)&target, NULL, (DWORD)0);
    ReleaseDC(NULL, dc);
    if (0 == color) {
        return NULL;
    }
    SOKOL_ASSERT(target);

    HBITMAP mask = CreateBitmap(desc->width, desc->height, 1, 1, NULL);
    if (0 == mask) {
        DeleteObject(color);
        return NULL;
    }

    for (int i = 0; i < (desc->width*desc->height); i++) {
        target[0] = source[2];
        target[1] = source[1];
        target[2] = source[0];
        target[3] = source[3];
        target += 4;
        source += 4;
    }

    ICONINFO icon_info;
    _sapp_clear(&icon_info, sizeof(icon_info));
    icon_info.fIcon = true;
    icon_info.xHotspot = 0;
    icon_info.yHotspot = 0;
    icon_info.hbmMask = mask;
    icon_info.hbmColor = color;
    HICON icon_handle = CreateIconIndirect(&icon_info);
    DeleteObject(color);
    DeleteObject(mask);

    return icon_handle;
}

_SOKOL_PRIVATE void _sapp_win32_set_icon(const sapp_icon_desc* icon_desc, int num_images) {
    SOKOL_ASSERT((num_images > 0) && (num_images <= SAPP_MAX_ICONIMAGES));

    int big_img_index = _sapp_image_bestmatch(icon_desc->images, num_images, GetSystemMetrics(SM_CXICON), GetSystemMetrics(SM_CYICON));
    int sml_img_index = _sapp_image_bestmatch(icon_desc->images, num_images, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON));
    HICON big_icon = _sapp_win32_create_icon_from_image(&icon_desc->images[big_img_index]);
    HICON sml_icon = _sapp_win32_create_icon_from_image(&icon_desc->images[sml_img_index]);

    // if icon creation or lookup has failed for some reason, leave the currently set icon untouched
    if (0 != big_icon) {
        SendMessage(_sapp.win32.hwnd, WM_SETICON, ICON_BIG, (LPARAM) big_icon);
        if (0 != _sapp.win32.big_icon) {
            DestroyIcon(_sapp.win32.big_icon);
        }
        _sapp.win32.big_icon = big_icon;
    }
    if (0 != sml_icon) {
        SendMessage(_sapp.win32.hwnd, WM_SETICON, ICON_SMALL, (LPARAM) sml_icon);
        if (0 != _sapp.win32.small_icon) {
            DestroyIcon(_sapp.win32.small_icon);
        }
        _sapp.win32.small_icon = sml_icon;
    }
}

/* don't laugh, but this seems to be the easiest and most robust
   way to check if we're running on Win10

   From: https://github.com/videolan/vlc/blob/232fb13b0d6110c4d1b683cde24cf9a7f2c5c2ea/modules/video_output/win32/d3d11_swapchain.c#L263
*/
_SOKOL_PRIVATE bool _sapp_win32_is_win10_or_greater(void) {
    HMODULE h = GetModuleHandleW(L"kernel32.dll");
    if (NULL != h) {
        return (NULL != GetProcAddress(h, "GetSystemCpuSetInformation"));
    }
    else {
        return false;
    }
}

_SOKOL_PRIVATE void _sapp_win32_run(const sapp_desc* desc) {
    _sapp_init_state(desc);
    _sapp_win32_init_console();
    _sapp.win32.is_win10_or_greater = _sapp_win32_is_win10_or_greater();
    _sapp_win32_init_keytable();
    _sapp_win32_utf8_to_wide(_sapp.window_title, _sapp.window_title_wide, sizeof(_sapp.window_title_wide));
    _sapp_win32_init_dpi();
    _sapp_win32_init_cursors();
    _sapp_win32_create_window();
    sapp_set_icon(&desc->icon);
    #if defined(SOKOL_D3D11)
        _sapp_d3d11_create_device_and_swapchain();
        _sapp_d3d11_create_default_render_target();
    #endif
    #if defined(SOKOL_GLCORE)
        _sapp_wgl_init();
        _sapp_wgl_load_extensions();
        _sapp_wgl_create_context();
    #endif
    _sapp.valid = true;

    bool done = false;
    while (!(done || _sapp.quit_ordered)) {
        _sapp_win32_timing_measure();
        MSG msg;
        while (PeekMessageW(&msg, NULL, 0, 0, PM_REMOVE)) {
            if (WM_QUIT == msg.message) {
                done = true;
                continue;
            }
            else {
                TranslateMessage(&msg);
                DispatchMessageW(&msg);
            }
        }
        _sapp_frame();
        #if defined(SOKOL_D3D11)
            _sapp_d3d11_present(false);
            if (IsIconic(_sapp.win32.hwnd)) {
                Sleep((DWORD)(16 * _sapp.swap_interval));
            }
        #endif
        #if defined(SOKOL_GLCORE)
            _sapp_wgl_swap_buffers();
        #endif
        /* check for window resized, this cannot happen in WM_SIZE as it explodes memory usage */
        if (_sapp_win32_update_dimensions()) {
            #if defined(SOKOL_D3D11)
            _sapp_d3d11_resize_default_render_target();
            #endif
            _sapp_win32_app_event(SAPP_EVENTTYPE_RESIZED);
        }
        /* check if the window monitor has changed, need to reset timing because
           the new monitor might have a different refresh rate
        */
        if (_sapp_win32_update_monitor()) {
            _sapp_timing_reset(&_sapp.timing);
        }
        if (_sapp.quit_requested) {
            PostMessage(_sapp.win32.hwnd, WM_CLOSE, 0, 0);
        }
    }
    _sapp_call_cleanup();

    #if defined(SOKOL_D3D11)
        _sapp_d3d11_destroy_default_render_target();
        _sapp_d3d11_destroy_device_and_swapchain();
    #else
        _sapp_wgl_destroy_context();
        _sapp_wgl_shutdown();
    #endif
    _sapp_win32_destroy_window();
    _sapp_win32_destroy_icons();
    _sapp_win32_restore_console();
    _sapp_discard_state();
}

_SOKOL_PRIVATE char** _sapp_win32_command_line_to_utf8_argv(LPWSTR w_command_line, int* o_argc) {
    int argc = 0;
    char** argv = 0;
    char* args;

    LPWSTR* w_argv = CommandLineToArgvW(w_command_line, &argc);
    if (w_argv == NULL) {
        // FIXME: chicken egg problem, can't report errors before sokol_main() is called!
    } else {
        size_t size = wcslen(w_command_line) * 4;
        argv = (char**) _sapp_malloc_clear(((size_t)argc + 1) * sizeof(char*) + size);
        SOKOL_ASSERT(argv);
        args = (char*) &argv[argc + 1];
        int n;
        for (int i = 0; i < argc; ++i) {
            n = WideCharToMultiByte(CP_UTF8, 0, w_argv[i], -1, args, (int)size, NULL, NULL);
            if (n == 0) {
                // FIXME: chicken egg problem, can't report errors before sokol_main() is called!
                break;
            }
            argv[i] = args;
            size -= (size_t)n;
            args += n;
        }
        LocalFree(w_argv);
    }
    *o_argc = argc;
    return argv;
}

#if !defined(SOKOL_NO_ENTRY)
#if defined(SOKOL_WIN32_FORCE_MAIN)
int main(int argc, char* argv[]) {
    sapp_desc desc = sokol_main(argc, argv);
    _sapp_win32_run(&desc);
    return 0;
}
#else
int WINAPI WinMain(_In_ HINSTANCE hInstance, _In_opt_ HINSTANCE hPrevInstance, _In_ LPSTR lpCmdLine, _In_ int nCmdShow) {
    _SOKOL_UNUSED(hInstance);
    _SOKOL_UNUSED(hPrevInstance);
    _SOKOL_UNUSED(lpCmdLine);
    _SOKOL_UNUSED(nCmdShow);
    int argc_utf8 = 0;
    char** argv_utf8 = _sapp_win32_command_line_to_utf8_argv(GetCommandLineW(), &argc_utf8);
    sapp_desc desc = sokol_main(argc_utf8, argv_utf8);
    _sapp_win32_run(&desc);
    _sapp_free(argv_utf8);
    return 0;
}
#endif /* SOKOL_WIN32_FORCE_MAIN */
#endif /* SOKOL_NO_ENTRY */

#ifdef _MSC_VER
    #pragma warning(pop)
#endif

#endif /* _SAPP_WIN32 */

//  █████  ███    ██ ██████  ██████   ██████  ██ ██████
// ██   ██ ████   ██ ██   ██ ██   ██ ██    ██ ██ ██   ██
// ███████ ██ ██  ██ ██   ██ ██████  ██    ██ ██ ██   ██
// ██   ██ ██  ██ ██ ██   ██ ██   ██ ██    ██ ██ ██   ██
// ██   ██ ██   ████ ██████  ██   ██  ██████  ██ ██████
//
// >>android
#if defined(_SAPP_ANDROID)

/* android loop thread */
_SOKOL_PRIVATE bool _sapp_android_init_egl(void) {
    SOKOL_ASSERT(_sapp.android.display == EGL_NO_DISPLAY);
    SOKOL_ASSERT(_sapp.android.context == EGL_NO_CONTEXT);

    EGLDisplay display = eglGetDisplay(EGL_DEFAULT_DISPLAY);
    if (display == EGL_NO_DISPLAY) {
        return false;
    }
    if (eglInitialize(display, NULL, NULL) == EGL_FALSE) {
        return false;
    }
    EGLint alpha_size = _sapp.desc.alpha ? 8 : 0;
    const EGLint cfg_attributes[] = {
        EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
        EGL_RENDERABLE_TYPE, EGL_OPENGL_ES3_BIT,
        EGL_RED_SIZE, 8,
        EGL_GREEN_SIZE, 8,
        EGL_BLUE_SIZE, 8,
        EGL_ALPHA_SIZE, alpha_size,
        EGL_DEPTH_SIZE, 16,
        EGL_STENCIL_SIZE, 0,
        EGL_NONE,
    };
    EGLConfig available_cfgs[32];
    EGLint cfg_count;
    eglChooseConfig(display, cfg_attributes, available_cfgs, 32, &cfg_count);
    SOKOL_ASSERT(cfg_count > 0);
    SOKOL_ASSERT(cfg_count <= 32);

    /* find config with 8-bit rgb buffer if available, ndk sample does not trust egl spec */
    EGLConfig config;
    bool exact_cfg_found = false;
    for (int i = 0; i < cfg_count; ++i) {
        EGLConfig c = available_cfgs[i];
        EGLint r, g, b, a, d;
        if (eglGetConfigAttrib(display, c, EGL_RED_SIZE, &r) == EGL_TRUE &&
            eglGetConfigAttrib(display, c, EGL_GREEN_SIZE, &g) == EGL_TRUE &&
            eglGetConfigAttrib(display, c, EGL_BLUE_SIZE, &b) == EGL_TRUE &&
            eglGetConfigAttrib(display, c, EGL_ALPHA_SIZE, &a) == EGL_TRUE &&
            eglGetConfigAttrib(display, c, EGL_DEPTH_SIZE, &d) == EGL_TRUE &&
            r == 8 && g == 8 && b == 8 && (alpha_size == 0 || a == alpha_size) && d == 16) {
            exact_cfg_found = true;
            config = c;
            break;
        }
    }
    if (!exact_cfg_found) {
        config = available_cfgs[0];
    }

    EGLint ctx_attributes[] = {
        EGL_CONTEXT_CLIENT_VERSION, 3,
        EGL_NONE,
    };
    EGLContext context = eglCreateContext(display, config, EGL_NO_CONTEXT, ctx_attributes);
    if (context == EGL_NO_CONTEXT) {
        return false;
    }

    _sapp.android.config = config;
    _sapp.android.display = display;
    _sapp.android.context = context;
    return true;
}

_SOKOL_PRIVATE void _sapp_android_cleanup_egl(void) {
    if (_sapp.android.display != EGL_NO_DISPLAY) {
        eglMakeCurrent(_sapp.android.display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
        if (_sapp.android.surface != EGL_NO_SURFACE) {
            eglDestroySurface(_sapp.android.display, _sapp.android.surface);
            _sapp.android.surface = EGL_NO_SURFACE;
        }
        if (_sapp.android.context != EGL_NO_CONTEXT) {
            eglDestroyContext(_sapp.android.display, _sapp.android.context);
            _sapp.android.context = EGL_NO_CONTEXT;
        }
        eglTerminate(_sapp.android.display);
        _sapp.android.display = EGL_NO_DISPLAY;
    }
}

_SOKOL_PRIVATE bool _sapp_android_init_egl_surface(ANativeWindow* window) {
    SOKOL_ASSERT(_sapp.android.display != EGL_NO_DISPLAY);
    SOKOL_ASSERT(_sapp.android.context != EGL_NO_CONTEXT);
    SOKOL_ASSERT(_sapp.android.surface == EGL_NO_SURFACE);
    SOKOL_ASSERT(window);

    /* TODO: set window flags */
    /* ANativeActivity_setWindowFlags(activity, AWINDOW_FLAG_KEEP_SCREEN_ON, 0); */

    /* create egl surface and make it current */
    EGLSurface surface = eglCreateWindowSurface(_sapp.android.display, _sapp.android.config, window, NULL);
    if (surface == EGL_NO_SURFACE) {
        return false;
    }
    if (eglMakeCurrent(_sapp.android.display, surface, surface, _sapp.android.context) == EGL_FALSE) {
        return false;
    }
    _sapp.android.surface = surface;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, (GLint*)&_sapp.gl.framebuffer);
    return true;
}

_SOKOL_PRIVATE void _sapp_android_cleanup_egl_surface(void) {
    if (_sapp.android.display == EGL_NO_DISPLAY) {
        return;
    }
    eglMakeCurrent(_sapp.android.display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
    if (_sapp.android.surface != EGL_NO_SURFACE) {
        eglDestroySurface(_sapp.android.display, _sapp.android.surface);
        _sapp.android.surface = EGL_NO_SURFACE;
    }
}

_SOKOL_PRIVATE void _sapp_android_app_event(sapp_event_type type) {
    if (_sapp_events_enabled()) {
        _sapp_init_event(type);
        _sapp_call_event(&_sapp.event);
    }
}

_SOKOL_PRIVATE void _sapp_android_update_dimensions(ANativeWindow* window, bool force_update) {
    SOKOL_ASSERT(_sapp.android.display != EGL_NO_DISPLAY);
    SOKOL_ASSERT(_sapp.android.context != EGL_NO_CONTEXT);
    SOKOL_ASSERT(_sapp.android.surface != EGL_NO_SURFACE);
    SOKOL_ASSERT(window);

    const int32_t win_w = ANativeWindow_getWidth(window);
    const int32_t win_h = ANativeWindow_getHeight(window);
    SOKOL_ASSERT(win_w >= 0 && win_h >= 0);
    const bool win_changed = (win_w != _sapp.window_width) || (win_h != _sapp.window_height);
    _sapp.window_width = win_w;
    _sapp.window_height = win_h;
    if (win_changed || force_update) {
        if (!_sapp.desc.high_dpi) {
            const int32_t buf_w = win_w / 2;
            const int32_t buf_h = win_h / 2;
            EGLint format;
            EGLBoolean egl_result = eglGetConfigAttrib(_sapp.android.display, _sapp.android.config, EGL_NATIVE_VISUAL_ID, &format);
            SOKOL_ASSERT(egl_result == EGL_TRUE); _SOKOL_UNUSED(egl_result);
            /* NOTE: calling ANativeWindow_setBuffersGeometry() with the same dimensions
                as the ANativeWindow size results in weird display artefacts, that's
                why it's only called when the buffer geometry is different from
                the window size
            */
            int32_t result = ANativeWindow_setBuffersGeometry(window, buf_w, buf_h, format);
            SOKOL_ASSERT(result == 0); _SOKOL_UNUSED(result);
        }
    }

    /* query surface size */
    EGLint fb_w, fb_h;
    EGLBoolean egl_result_w = eglQuerySurface(_sapp.android.display, _sapp.android.surface, EGL_WIDTH, &fb_w);
    EGLBoolean egl_result_h = eglQuerySurface(_sapp.android.display, _sapp.android.surface, EGL_HEIGHT, &fb_h);
    SOKOL_ASSERT(egl_result_w == EGL_TRUE); _SOKOL_UNUSED(egl_result_w);
    SOKOL_ASSERT(egl_result_h == EGL_TRUE); _SOKOL_UNUSED(egl_result_h);
    const bool fb_changed = (fb_w != _sapp.framebuffer_width) || (fb_h != _sapp.framebuffer_height);
    _sapp.framebuffer_width = fb_w;
    _sapp.framebuffer_height = fb_h;
    _sapp.dpi_scale = (float)_sapp.framebuffer_width / (float)_sapp.window_width;
    if (win_changed || fb_changed || force_update) {
        if (!_sapp.first_frame) {
            _sapp_android_app_event(SAPP_EVENTTYPE_RESIZED);
        }
    }
}

_SOKOL_PRIVATE void _sapp_android_cleanup(void) {
    if (_sapp.android.surface != EGL_NO_SURFACE) {
        /* egl context is bound, cleanup gracefully */
        if (_sapp.init_called && !_sapp.cleanup_called) {
            _sapp_call_cleanup();
        }
    }
    /* always try to cleanup by destroying egl context */
    _sapp_android_cleanup_egl();
}

_SOKOL_PRIVATE void _sapp_android_shutdown(void) {
    /* try to cleanup while we still have a surface and can call cleanup_cb() */
    _sapp_android_cleanup();
    /* request exit */
    ANativeActivity_finish(_sapp.android.activity);
}

_SOKOL_PRIVATE void _sapp_android_frame(void) {
    SOKOL_ASSERT(_sapp.android.display != EGL_NO_DISPLAY);
    SOKOL_ASSERT(_sapp.android.context != EGL_NO_CONTEXT);
    SOKOL_ASSERT(_sapp.android.surface != EGL_NO_SURFACE);
    _sapp_timing_measure(&_sapp.timing);
    _sapp_android_update_dimensions(_sapp.android.current.window, false);
    _sapp_frame();
    eglSwapBuffers(_sapp.android.display, _sapp.android.surface);
}

_SOKOL_PRIVATE bool _sapp_android_touch_event(const AInputEvent* e) {
    if (AInputEvent_getType(e) != AINPUT_EVENT_TYPE_MOTION) {
        return false;
    }
    if (!_sapp_events_enabled()) {
        return false;
    }
    int32_t action_idx = AMotionEvent_getAction(e);
    int32_t action = action_idx & AMOTION_EVENT_ACTION_MASK;
    sapp_event_type type = SAPP_EVENTTYPE_INVALID;
    switch (action) {
        case AMOTION_EVENT_ACTION_DOWN:
        case AMOTION_EVENT_ACTION_POINTER_DOWN:
            type = SAPP_EVENTTYPE_TOUCHES_BEGAN;
            break;
        case AMOTION_EVENT_ACTION_MOVE:
            type = SAPP_EVENTTYPE_TOUCHES_MOVED;
            break;
        case AMOTION_EVENT_ACTION_UP:
        case AMOTION_EVENT_ACTION_POINTER_UP:
            type = SAPP_EVENTTYPE_TOUCHES_ENDED;
            break;
        case AMOTION_EVENT_ACTION_CANCEL:
            type = SAPP_EVENTTYPE_TOUCHES_CANCELLED;
            break;
        default:
            break;
    }
    if (type == SAPP_EVENTTYPE_INVALID) {
        return false;
    }
    int32_t idx = action_idx >> AMOTION_EVENT_ACTION_POINTER_INDEX_SHIFT;
    _sapp_init_event(type);
    _sapp.event.num_touches = (int)AMotionEvent_getPointerCount(e);
    if (_sapp.event.num_touches > SAPP_MAX_TOUCHPOINTS) {
        _sapp.event.num_touches = SAPP_MAX_TOUCHPOINTS;
    }
    for (int32_t i = 0; i < _sapp.event.num_touches; i++) {
        sapp_touchpoint* dst = &_sapp.event.touches[i];
        dst->identifier = (uintptr_t)AMotionEvent_getPointerId(e, (size_t)i);
        dst->pos_x = (AMotionEvent_getX(e, (size_t)i) / _sapp.window_width) * _sapp.framebuffer_width;
        dst->pos_y = (AMotionEvent_getY(e, (size_t)i) / _sapp.window_height) * _sapp.framebuffer_height;
        dst->android_tooltype = (sapp_android_tooltype) AMotionEvent_getToolType(e, (size_t)i);
        if (action == AMOTION_EVENT_ACTION_POINTER_DOWN ||
            action == AMOTION_EVENT_ACTION_POINTER_UP) {
            dst->changed = (i == idx);
        } else {
            dst->changed = true;
        }
    }
    _sapp_call_event(&_sapp.event);
    return true;
}

_SOKOL_PRIVATE bool _sapp_android_key_event(const AInputEvent* e) {
    if (AInputEvent_getType(e) != AINPUT_EVENT_TYPE_KEY) {
        return false;
    }
    if (AKeyEvent_getKeyCode(e) == AKEYCODE_BACK) {
        /* FIXME: this should be hooked into a "really quit?" mechanism
           so the app can ask the user for confirmation, this is currently
           generally missing in sokol_app.h
        */
        _sapp_android_shutdown();
        return true;
    }
    return false;
}

_SOKOL_PRIVATE int _sapp_android_input_cb(int fd, int events, void* data) {
    _SOKOL_UNUSED(fd);
    _SOKOL_UNUSED(data);
    if ((events & ALOOPER_EVENT_INPUT) == 0) {
        _SAPP_ERROR(ANDROID_UNSUPPORTED_INPUT_EVENT_INPUT_CB);
        return 1;
    }
    SOKOL_ASSERT(_sapp.android.current.input);
    AInputEvent* event = NULL;
    while (AInputQueue_getEvent(_sapp.android.current.input, &event) >= 0) {
        if (AInputQueue_preDispatchEvent(_sapp.android.current.input, event) != 0) {
            continue;
        }
        int32_t handled = 0;
        if (_sapp_android_touch_event(event) || _sapp_android_key_event(event)) {
            handled = 1;
        }
        AInputQueue_finishEvent(_sapp.android.current.input, event, handled);
    }
    return 1;
}

_SOKOL_PRIVATE int _sapp_android_main_cb(int fd, int events, void* data) {
    _SOKOL_UNUSED(data);
    if ((events & ALOOPER_EVENT_INPUT) == 0) {
        _SAPP_ERROR(ANDROID_UNSUPPORTED_INPUT_EVENT_MAIN_CB);
        return 1;
    }

    _sapp_android_msg_t msg;
    if (read(fd, &msg, sizeof(msg)) != sizeof(msg)) {
        _SAPP_ERROR(ANDROID_READ_MSG_FAILED);
        return 1;
    }

    pthread_mutex_lock(&_sapp.android.pt.mutex);
    switch (msg) {
        case _SOKOL_ANDROID_MSG_CREATE:
            {
                _SAPP_INFO(ANDROID_MSG_CREATE);
                SOKOL_ASSERT(!_sapp.valid);
                bool result = _sapp_android_init_egl();
                SOKOL_ASSERT(result); _SOKOL_UNUSED(result);
                _sapp.valid = true;
                _sapp.android.has_created = true;
            }
            break;
        case _SOKOL_ANDROID_MSG_RESUME:
            _SAPP_INFO(ANDROID_MSG_RESUME);
            _sapp.android.has_resumed = true;
            _sapp_android_app_event(SAPP_EVENTTYPE_RESUMED);
            break;
        case _SOKOL_ANDROID_MSG_PAUSE:
            _SAPP_INFO(ANDROID_MSG_PAUSE);
            _sapp.android.has_resumed = false;
            _sapp_android_app_event(SAPP_EVENTTYPE_SUSPENDED);
            break;
        case _SOKOL_ANDROID_MSG_FOCUS:
            _SAPP_INFO(ANDROID_MSG_FOCUS);
            _sapp.android.has_focus = true;
            break;
        case _SOKOL_ANDROID_MSG_NO_FOCUS:
            _SAPP_INFO(ANDROID_MSG_NO_FOCUS);
            _sapp.android.has_focus = false;
            break;
        case _SOKOL_ANDROID_MSG_SET_NATIVE_WINDOW:
            _SAPP_INFO(ANDROID_MSG_SET_NATIVE_WINDOW);
            if (_sapp.android.current.window != _sapp.android.pending.window) {
                if (_sapp.android.current.window != NULL) {
                    _sapp_android_cleanup_egl_surface();
                }
                if (_sapp.android.pending.window != NULL) {
                    if (_sapp_android_init_egl_surface(_sapp.android.pending.window)) {
                        _sapp_android_update_dimensions(_sapp.android.pending.window, true);
                    } else {
                        _sapp_android_shutdown();
                    }
                }
            }
            _sapp.android.current.window = _sapp.android.pending.window;
            break;
        case _SOKOL_ANDROID_MSG_SET_INPUT_QUEUE:
            _SAPP_INFO(ANDROID_MSG_SET_INPUT_QUEUE);
            if (_sapp.android.current.input != _sapp.android.pending.input) {
                if (_sapp.android.current.input != NULL) {
                    AInputQueue_detachLooper(_sapp.android.current.input);
                }
                if (_sapp.android.pending.input != NULL) {
                    AInputQueue_attachLooper(
                        _sapp.android.pending.input,
                        _sapp.android.looper,
                        ALOOPER_POLL_CALLBACK,
                        _sapp_android_input_cb,
                        NULL); /* data */
                }
            }
            _sapp.android.current.input = _sapp.android.pending.input;
            break;
        case _SOKOL_ANDROID_MSG_DESTROY:
            _SAPP_INFO(ANDROID_MSG_DESTROY);
            _sapp_android_cleanup();
            _sapp.valid = false;
            _sapp.android.is_thread_stopping = true;
            break;
        default:
            _SAPP_WARN(ANDROID_UNKNOWN_MSG);
            break;
    }
    pthread_cond_broadcast(&_sapp.android.pt.cond); /* signal "received" */
    pthread_mutex_unlock(&_sapp.android.pt.mutex);
    return 1;
}

_SOKOL_PRIVATE bool _sapp_android_should_update(void) {
    bool is_in_front = _sapp.android.has_resumed && _sapp.android.has_focus;
    bool has_surface = _sapp.android.surface != EGL_NO_SURFACE;
    return is_in_front && has_surface;
}

_SOKOL_PRIVATE void _sapp_android_show_keyboard(bool shown) {
    SOKOL_ASSERT(_sapp.valid);
    /* This seems to be broken in the NDK, but there is (a very cumbersome) workaround... */
    if (shown) {
        ANativeActivity_showSoftInput(_sapp.android.activity, ANATIVEACTIVITY_SHOW_SOFT_INPUT_FORCED);
    } else {
        ANativeActivity_hideSoftInput(_sapp.android.activity, ANATIVEACTIVITY_HIDE_SOFT_INPUT_NOT_ALWAYS);
    }
}

_SOKOL_PRIVATE void* _sapp_android_loop(void* arg) {
    _SOKOL_UNUSED(arg);
    _SAPP_INFO(ANDROID_LOOP_THREAD_STARTED);

    _sapp.android.looper = ALooper_prepare(0 /* or ALOOPER_PREPARE_ALLOW_NON_CALLBACKS*/);
    ALooper_addFd(_sapp.android.looper,
        _sapp.android.pt.read_from_main_fd,
        ALOOPER_POLL_CALLBACK,
        ALOOPER_EVENT_INPUT,
        _sapp_android_main_cb,
        NULL); /* data */

    /* signal start to main thread */
    pthread_mutex_lock(&_sapp.android.pt.mutex);
    _sapp.android.is_thread_started = true;
    pthread_cond_broadcast(&_sapp.android.pt.cond);
    pthread_mutex_unlock(&_sapp.android.pt.mutex);

    /* main loop */
    while (!_sapp.android.is_thread_stopping) {
        /* sokol frame */
        if (_sapp_android_should_update()) {
            _sapp_android_frame();
        }

        /* process all events (or stop early if app is requested to quit) */
        bool process_events = true;
        while (process_events && !_sapp.android.is_thread_stopping) {
            bool block_until_event = !_sapp.android.is_thread_stopping && !_sapp_android_should_update();
            process_events = ALooper_pollOnce(block_until_event ? -1 : 0, NULL, NULL, NULL) == ALOOPER_POLL_CALLBACK;
        }
    }

    /* cleanup thread */
    if (_sapp.android.current.input != NULL) {
        AInputQueue_detachLooper(_sapp.android.current.input);
    }

    /* the following causes heap corruption on exit, why??
    ALooper_removeFd(_sapp.android.looper, _sapp.android.pt.read_from_main_fd);
    ALooper_release(_sapp.android.looper);*/

    /* signal "destroyed" */
    pthread_mutex_lock(&_sapp.android.pt.mutex);
    _sapp.android.is_thread_stopped = true;
    pthread_cond_broadcast(&_sapp.android.pt.cond);
    pthread_mutex_unlock(&_sapp.android.pt.mutex);

    _SAPP_INFO(ANDROID_LOOP_THREAD_DONE);
    return NULL;
}

/* android main/ui thread */
_SOKOL_PRIVATE void _sapp_android_msg(_sapp_android_msg_t msg) {
    if (write(_sapp.android.pt.write_from_main_fd, &msg, sizeof(msg)) != sizeof(msg)) {
        _SAPP_ERROR(ANDROID_WRITE_MSG_FAILED);
    }
}

_SOKOL_PRIVATE void _sapp_android_on_start(ANativeActivity* activity) {
    _SOKOL_UNUSED(activity);
    _SAPP_INFO(ANDROID_NATIVE_ACTIVITY_ONSTART);
}

_SOKOL_PRIVATE void _sapp_android_on_resume(ANativeActivity* activity) {
    _SOKOL_UNUSED(activity);
    _SAPP_INFO(ANDROID_NATIVE_ACTIVITY_ONRESUME);
    _sapp_android_msg(_SOKOL_ANDROID_MSG_RESUME);
}

_SOKOL_PRIVATE void* _sapp_android_on_save_instance_state(ANativeActivity* activity, size_t* out_size) {
    _SOKOL_UNUSED(activity);
    _SAPP_INFO(ANDROID_NATIVE_ACTIVITY_ONSAVEINSTANCESTATE);
    *out_size = 0;
    return NULL;
}

_SOKOL_PRIVATE void _sapp_android_on_window_focus_changed(ANativeActivity* activity, int has_focus) {
    _SOKOL_UNUSED(activity);
    _SAPP_INFO(ANDROID_NATIVE_ACTIVITY_ONWINDOWFOCUSCHANGED);
    if (has_focus) {
        _sapp_android_msg(_SOKOL_ANDROID_MSG_FOCUS);
    } else {
        _sapp_android_msg(_SOKOL_ANDROID_MSG_NO_FOCUS);
    }
}

_SOKOL_PRIVATE void _sapp_android_on_pause(ANativeActivity* activity) {
    _SOKOL_UNUSED(activity);
    _SAPP_INFO(ANDROID_NATIVE_ACTIVITY_ONPAUSE);
    _sapp_android_msg(_SOKOL_ANDROID_MSG_PAUSE);
}

_SOKOL_PRIVATE void _sapp_android_on_stop(ANativeActivity* activity) {
    _SOKOL_UNUSED(activity);
    _SAPP_INFO(ANDROID_NATIVE_ACTIVITY_ONSTOP);
}

_SOKOL_PRIVATE void _sapp_android_msg_set_native_window(ANativeWindow* window) {
    pthread_mutex_lock(&_sapp.android.pt.mutex);
    _sapp.android.pending.window = window;
    _sapp_android_msg(_SOKOL_ANDROID_MSG_SET_NATIVE_WINDOW);
    while (_sapp.android.current.window != window) {
        pthread_cond_wait(&_sapp.android.pt.cond, &_sapp.android.pt.mutex);
    }
    pthread_mutex_unlock(&_sapp.android.pt.mutex);
}

_SOKOL_PRIVATE void _sapp_android_on_native_window_created(ANativeActivity* activity, ANativeWindow* window) {
    _SOKOL_UNUSED(activity);
    _SAPP_INFO(ANDROID_NATIVE_ACTIVITY_ONNATIVEWINDOWCREATED);
    _sapp_android_msg_set_native_window(window);
}

_SOKOL_PRIVATE void _sapp_android_on_native_window_destroyed(ANativeActivity* activity, ANativeWindow* window) {
    _SOKOL_UNUSED(activity);
    _SOKOL_UNUSED(window);
    _SAPP_INFO(ANDROID_NATIVE_ACTIVITY_ONNATIVEWINDOWDESTROYED);
    _sapp_android_msg_set_native_window(NULL);
}

_SOKOL_PRIVATE void _sapp_android_msg_set_input_queue(AInputQueue* input) {
    pthread_mutex_lock(&_sapp.android.pt.mutex);
    _sapp.android.pending.input = input;
    _sapp_android_msg(_SOKOL_ANDROID_MSG_SET_INPUT_QUEUE);
    while (_sapp.android.current.input != input) {
        pthread_cond_wait(&_sapp.android.pt.cond, &_sapp.android.pt.mutex);
    }
    pthread_mutex_unlock(&_sapp.android.pt.mutex);
}

_SOKOL_PRIVATE void _sapp_android_on_input_queue_created(ANativeActivity* activity, AInputQueue* queue) {
    _SOKOL_UNUSED(activity);
    _SAPP_INFO(ANDROID_NATIVE_ACTIVITY_ONINPUTQUEUECREATED);
    _sapp_android_msg_set_input_queue(queue);
}

_SOKOL_PRIVATE void _sapp_android_on_input_queue_destroyed(ANativeActivity* activity, AInputQueue* queue) {
    _SOKOL_UNUSED(activity);
    _SOKOL_UNUSED(queue);
    _SAPP_INFO(ANDROID_NATIVE_ACTIVITY_ONINPUTQUEUEDESTROYED);
    _sapp_android_msg_set_input_queue(NULL);
}

_SOKOL_PRIVATE void _sapp_android_on_config_changed(ANativeActivity* activity) {
    _SOKOL_UNUSED(activity);
    _SAPP_INFO(ANDROID_NATIVE_ACTIVITY_ONCONFIGURATIONCHANGED);
    /* see android:configChanges in manifest */
}

_SOKOL_PRIVATE void _sapp_android_on_low_memory(ANativeActivity* activity) {
    _SOKOL_UNUSED(activity);
    _SAPP_INFO(ANDROID_NATIVE_ACTIVITY_ONLOWMEMORY);
}

_SOKOL_PRIVATE void _sapp_android_on_destroy(ANativeActivity* activity) {
    /*
     * For some reason even an empty app using nativeactivity.h will crash (WIN DEATH)
     * on my device (Moto X 2nd gen) when the app is removed from the task view
     * (TaskStackView: onTaskViewDismissed).
     *
     * However, if ANativeActivity_finish() is explicitly called from for example
     * _sapp_android_on_stop(), the crash disappears. Is this a bug in NativeActivity?
     */
    _SOKOL_UNUSED(activity);
    _SAPP_INFO(ANDROID_NATIVE_ACTIVITY_ONDESTROY);

    /* send destroy msg */
    pthread_mutex_lock(&_sapp.android.pt.mutex);
    _sapp_android_msg(_SOKOL_ANDROID_MSG_DESTROY);
    while (!_sapp.android.is_thread_stopped) {
        pthread_cond_wait(&_sapp.android.pt.cond, &_sapp.android.pt.mutex);
    }
    pthread_mutex_unlock(&_sapp.android.pt.mutex);

    /* clean up main thread */
    pthread_cond_destroy(&_sapp.android.pt.cond);
    pthread_mutex_destroy(&_sapp.android.pt.mutex);

    close(_sapp.android.pt.read_from_main_fd);
    close(_sapp.android.pt.write_from_main_fd);

    _SAPP_INFO(ANDROID_NATIVE_ACTIVITY_DONE);

    /* this is a bit naughty, but causes a clean restart of the app (static globals are reset) */
    exit(0);
}

JNIEXPORT
void ANativeActivity_onCreate(ANativeActivity* activity, void* saved_state, size_t saved_state_size) {
    _SOKOL_UNUSED(saved_state);
    _SOKOL_UNUSED(saved_state_size);
    _SAPP_INFO(ANDROID_NATIVE_ACTIVITY_ONCREATE);

    // the NativeActity pointer needs to be available inside sokol_main()
    // (see https://github.com/floooh/sokol/issues/708), however _sapp_init_state()
    // will clear the global _sapp_t struct, so we need to initialize the native
    // activity pointer twice, once before sokol_main() and once after _sapp_init_state()
    _sapp_clear(&_sapp, sizeof(_sapp));
    _sapp.android.activity = activity;
    sapp_desc desc = sokol_main(0, NULL);
    _sapp_init_state(&desc);
    _sapp.android.activity = activity;

    int pipe_fd[2];
    if (pipe(pipe_fd) != 0) {
        _SAPP_ERROR(ANDROID_CREATE_THREAD_PIPE_FAILED);
        return;
    }
    _sapp.android.pt.read_from_main_fd = pipe_fd[0];
    _sapp.android.pt.write_from_main_fd = pipe_fd[1];

    pthread_mutex_init(&_sapp.android.pt.mutex, NULL);
    pthread_cond_init(&_sapp.android.pt.cond, NULL);

    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    pthread_create(&_sapp.android.pt.thread, &attr, _sapp_android_loop, 0);
    pthread_attr_destroy(&attr);

    /* wait until main loop has started */
    pthread_mutex_lock(&_sapp.android.pt.mutex);
    while (!_sapp.android.is_thread_started) {
        pthread_cond_wait(&_sapp.android.pt.cond, &_sapp.android.pt.mutex);
    }
    pthread_mutex_unlock(&_sapp.android.pt.mutex);

    /* send create msg */
    pthread_mutex_lock(&_sapp.android.pt.mutex);
    _sapp_android_msg(_SOKOL_ANDROID_MSG_CREATE);
    while (!_sapp.android.has_created) {
        pthread_cond_wait(&_sapp.android.pt.cond, &_sapp.android.pt.mutex);
    }
    pthread_mutex_unlock(&_sapp.android.pt.mutex);

    /* register for callbacks */
    activity->callbacks->onStart = _sapp_android_on_start;
    activity->callbacks->onResume = _sapp_android_on_resume;
    activity->callbacks->onSaveInstanceState = _sapp_android_on_save_instance_state;
    activity->callbacks->onWindowFocusChanged = _sapp_android_on_window_focus_changed;
    activity->callbacks->onPause = _sapp_android_on_pause;
    activity->callbacks->onStop = _sapp_android_on_stop;
    activity->callbacks->onDestroy = _sapp_android_on_destroy;
    activity->callbacks->onNativeWindowCreated = _sapp_android_on_native_window_created;
    /* activity->callbacks->onNativeWindowResized = _sapp_android_on_native_window_resized; */
    /* activity->callbacks->onNativeWindowRedrawNeeded = _sapp_android_on_native_window_redraw_needed; */
    activity->callbacks->onNativeWindowDestroyed = _sapp_android_on_native_window_destroyed;
    activity->callbacks->onInputQueueCreated = _sapp_android_on_input_queue_created;
    activity->callbacks->onInputQueueDestroyed = _sapp_android_on_input_queue_destroyed;
    /* activity->callbacks->onContentRectChanged = _sapp_android_on_content_rect_changed; */
    activity->callbacks->onConfigurationChanged = _sapp_android_on_config_changed;
    activity->callbacks->onLowMemory = _sapp_android_on_low_memory;

    _SAPP_INFO(ANDROID_NATIVE_ACTIVITY_CREATE_SUCCESS);

    /* NOT A BUG: do NOT call sapp_discard_state() */
}

#endif /* _SAPP_ANDROID */

// ██      ██ ███    ██ ██    ██ ██   ██
// ██      ██ ████   ██ ██    ██  ██ ██
// ██      ██ ██ ██  ██ ██    ██   ███
// ██      ██ ██  ██ ██ ██    ██  ██ ██
// ███████ ██ ██   ████  ██████  ██   ██
//
// >>linux
#if defined(_SAPP_LINUX)

/* see GLFW's xkb_unicode.c */
static const struct _sapp_x11_codepair {
  uint16_t keysym;
  uint16_t ucs;
} _sapp_x11_keysymtab[] = {
  { 0x01a1, 0x0104 },
  { 0x01a2, 0x02d8 },
  { 0x01a3, 0x0141 },
  { 0x01a5, 0x013d },
  { 0x01a6, 0x015a },
  { 0x01a9, 0x0160 },
  { 0x01aa, 0x015e },
  { 0x01ab, 0x0164 },
  { 0x01ac, 0x0179 },
  { 0x01ae, 0x017d },
  { 0x01af, 0x017b },
  { 0x01b1, 0x0105 },
  { 0x01b2, 0x02db },
  { 0x01b3, 0x0142 },
  { 0x01b5, 0x013e },
  { 0x01b6, 0x015b },
  { 0x01b7, 0x02c7 },
  { 0x01b9, 0x0161 },
  { 0x01ba, 0x015f },
  { 0x01bb, 0x0165 },
  { 0x01bc, 0x017a },
  { 0x01bd, 0x02dd },
  { 0x01be, 0x017e },
  { 0x01bf, 0x017c },
  { 0x01c0, 0x0154 },
  { 0x01c3, 0x0102 },
  { 0x01c5, 0x0139 },
  { 0x01c6, 0x0106 },
  { 0x01c8, 0x010c },
  { 0x01ca, 0x0118 },
  { 0x01cc, 0x011a },
  { 0x01cf, 0x010e },
  { 0x01d0, 0x0110 },
  { 0x01d1, 0x0143 },
  { 0x01d2, 0x0147 },
  { 0x01d5, 0x0150 },
  { 0x01d8, 0x0158 },
  { 0x01d9, 0x016e },
  { 0x01db, 0x0170 },
  { 0x01de, 0x0162 },
  { 0x01e0, 0x0155 },
  { 0x01e3, 0x0103 },
  { 0x01e5, 0x013a },
  { 0x01e6, 0x0107 },
  { 0x01e8, 0x010d },
  { 0x01ea, 0x0119 },
  { 0x01ec, 0x011b },
  { 0x01ef, 0x010f },
  { 0x01f0, 0x0111 },
  { 0x01f1, 0x0144 },
  { 0x01f2, 0x0148 },
  { 0x01f5, 0x0151 },
  { 0x01f8, 0x0159 },
  { 0x01f9, 0x016f },
  { 0x01fb, 0x0171 },
  { 0x01fe, 0x0163 },
  { 0x01ff, 0x02d9 },
  { 0x02a1, 0x0126 },
  { 0x02a6, 0x0124 },
  { 0x02a9, 0x0130 },
  { 0x02ab, 0x011e },
  { 0x02ac, 0x0134 },
  { 0x02b1, 0x0127 },
  { 0x02b6, 0x0125 },
  { 0x02b9, 0x0131 },
  { 0x02bb, 0x011f },
  { 0x02bc, 0x0135 },
  { 0x02c5, 0x010a },
  { 0x02c6, 0x0108 },
  { 0x02d5, 0x0120 },
  { 0x02d8, 0x011c },
  { 0x02dd, 0x016c },
  { 0x02de, 0x015c },
  { 0x02e5, 0x010b },
  { 0x02e6, 0x0109 },
  { 0x02f5, 0x0121 },
  { 0x02f8, 0x011d },
  { 0x02fd, 0x016d },
  { 0x02fe, 0x015d },
  { 0x03a2, 0x0138 },
  { 0x03a3, 0x0156 },
  { 0x03a5, 0x0128 },
  { 0x03a6, 0x013b },
  { 0x03aa, 0x0112 },
  { 0x03ab, 0x0122 },
  { 0x03ac, 0x0166 },
  { 0x03b3, 0x0157 },
  { 0x03b5, 0x0129 },
  { 0x03b6, 0x013c },
  { 0x03ba, 0x0113 },
  { 0x03bb, 0x0123 },
  { 0x03bc, 0x0167 },
  { 0x03bd, 0x014a },
  { 0x03bf, 0x014b },
  { 0x03c0, 0x0100 },
  { 0x03c7, 0x012e },
  { 0x03cc, 0x0116 },
  { 0x03cf, 0x012a },
  { 0x03d1, 0x0145 },
  { 0x03d2, 0x014c },
  { 0x03d3, 0x0136 },
  { 0x03d9, 0x0172 },
  { 0x03dd, 0x0168 },
  { 0x03de, 0x016a },
  { 0x03e0, 0x0101 },
  { 0x03e7, 0x012f },
  { 0x03ec, 0x0117 },
  { 0x03ef, 0x012b },
  { 0x03f1, 0x0146 },
  { 0x03f2, 0x014d },
  { 0x03f3, 0x0137 },
  { 0x03f9, 0x0173 },
  { 0x03fd, 0x0169 },
  { 0x03fe, 0x016b },
  { 0x047e, 0x203e },
  { 0x04a1, 0x3002 },
  { 0x04a2, 0x300c },
  { 0x04a3, 0x300d },
  { 0x04a4, 0x3001 },
  { 0x04a5, 0x30fb },
  { 0x04a6, 0x30f2 },
  { 0x04a7, 0x30a1 },
  { 0x04a8, 0x30a3 },
  { 0x04a9, 0x30a5 },
  { 0x04aa, 0x30a7 },
  { 0x04ab, 0x30a9 },
  { 0x04ac, 0x30e3 },
  { 0x04ad, 0x30e5 },
  { 0x04ae, 0x30e7 },
  { 0x04af, 0x30c3 },
  { 0x04b0, 0x30fc },
  { 0x04b1, 0x30a2 },
  { 0x04b2, 0x30a4 },
  { 0x04b3, 0x30a6 },
  { 0x04b4, 0x30a8 },
  { 0x04b5, 0x30aa },
  { 0x04b6, 0x30ab },
  { 0x04b7, 0x30ad },
  { 0x04b8, 0x30af },
  { 0x04b9, 0x30b1 },
  { 0x04ba, 0x30b3 },
  { 0x04bb, 0x30b5 },
  { 0x04bc, 0x30b7 },
  { 0x04bd, 0x30b9 },
  { 0x04be, 0x30bb },
  { 0x04bf, 0x30bd },
  { 0x04c0, 0x30bf },
  { 0x04c1, 0x30c1 },
  { 0x04c2, 0x30c4 },
  { 0x04c3, 0x30c6 },
  { 0x04c4, 0x30c8 },
  { 0x04c5, 0x30ca },
  { 0x04c6, 0x30cb },
  { 0x04c7, 0x30cc },
  { 0x04c8, 0x30cd },
  { 0x04c9, 0x30ce },
  { 0x04ca, 0x30cf },
  { 0x04cb, 0x30d2 },
  { 0x04cc, 0x30d5 },
  { 0x04cd, 0x30d8 },
  { 0x04ce, 0x30db },
  { 0x04cf, 0x30de },
  { 0x04d0, 0x30df },
  { 0x04d1, 0x30e0 },
  { 0x04d2, 0x30e1 },
  { 0x04d3, 0x30e2 },
  { 0x04d4, 0x30e4 },
  { 0x04d5, 0x30e6 },
  { 0x04d6, 0x30e8 },
  { 0x04d7, 0x30e9 },
  { 0x04d8, 0x30ea },
  { 0x04d9, 0x30eb },
  { 0x04da, 0x30ec },
  { 0x04db, 0x30ed },
  { 0x04dc, 0x30ef },
  { 0x04dd, 0x30f3 },
  { 0x04de, 0x309b },
  { 0x04df, 0x309c },
  { 0x05ac, 0x060c },
  { 0x05bb, 0x061b },
  { 0x05bf, 0x061f },
  { 0x05c1, 0x0621 },
  { 0x05c2, 0x0622 },
  { 0x05c3, 0x0623 },
  { 0x05c4, 0x0624 },
  { 0x05c5, 0x0625 },
  { 0x05c6, 0x0626 },
  { 0x05c7, 0x0627 },
  { 0x05c8, 0x0628 },
  { 0x05c9, 0x0629 },
  { 0x05ca, 0x062a },
  { 0x05cb, 0x062b },
  { 0x05cc, 0x062c },
  { 0x05cd, 0x062d },
  { 0x05ce, 0x062e },
  { 0x05cf, 0x062f },
  { 0x05d0, 0x0630 },
  { 0x05d1, 0x0631 },
  { 0x05d2, 0x0632 },
  { 0x05d3, 0x0633 },
  { 0x05d4, 0x0634 },
  { 0x05d5, 0x0635 },
  { 0x05d6, 0x0636 },
  { 0x05d7, 0x0637 },
  { 0x05d8, 0x0638 },
  { 0x05d9, 0x0639 },
  { 0x05da, 0x063a },
  { 0x05e0, 0x0640 },
  { 0x05e1, 0x0641 },
  { 0x05e2, 0x0642 },
  { 0x05e3, 0x0643 },
  { 0x05e4, 0x0644 },
  { 0x05e5, 0x0645 },
  { 0x05e6, 0x0646 },
  { 0x05e7, 0x0647 },
  { 0x05e8, 0x0648 },
  { 0x05e9, 0x0649 },
  { 0x05ea, 0x064a },
  { 0x05eb, 0x064b },
  { 0x05ec, 0x064c },
  { 0x05ed, 0x064d },
  { 0x05ee, 0x064e },
  { 0x05ef, 0x064f },
  { 0x05f0, 0x0650 },
  { 0x05f1, 0x0651 },
  { 0x05f2, 0x0652 },
  { 0x06a1, 0x0452 },
  { 0x06a2, 0x0453 },
  { 0x06a3, 0x0451 },
  { 0x06a4, 0x0454 },
  { 0x06a5, 0x0455 },
  { 0x06a6, 0x0456 },
  { 0x06a7, 0x0457 },
  { 0x06a8, 0x0458 },
  { 0x06a9, 0x0459 },
  { 0x06aa, 0x045a },
  { 0x06ab, 0x045b },
  { 0x06ac, 0x045c },
  { 0x06ae, 0x045e },
  { 0x06af, 0x045f },
  { 0x06b0, 0x2116 },
  { 0x06b1, 0x0402 },
  { 0x06b2, 0x0403 },
  { 0x06b3, 0x0401 },
  { 0x06b4, 0x0404 },
  { 0x06b5, 0x0405 },
  { 0x06b6, 0x0406 },
  { 0x06b7, 0x0407 },
  { 0x06b8, 0x0408 },
  { 0x06b9, 0x0409 },
  { 0x06ba, 0x040a },
  { 0x06bb, 0x040b },
  { 0x06bc, 0x040c },
  { 0x06be, 0x040e },
  { 0x06bf, 0x040f },
  { 0x06c0, 0x044e },
  { 0x06c1, 0x0430 },
  { 0x06c2, 0x0431 },
  { 0x06c3, 0x0446 },
  { 0x06c4, 0x0434 },
  { 0x06c5, 0x0435 },
  { 0x06c6, 0x0444 },
  { 0x06c7, 0x0433 },
  { 0x06c8, 0x0445 },
  { 0x06c9, 0x0438 },
  { 0x06ca, 0x0439 },
  { 0x06cb, 0x043a },
  { 0x06cc, 0x043b },
  { 0x06cd, 0x043c },
  { 0x06ce, 0x043d },
  { 0x06cf, 0x043e },
  { 0x06d0, 0x043f },
  { 0x06d1, 0x044f },
  { 0x06d2, 0x0440 },
  { 0x06d3, 0x0441 },
  { 0x06d4, 0x0442 },
  { 0x06d5, 0x0443 },
  { 0x06d6, 0x0436 },
  { 0x06d7, 0x0432 },
  { 0x06d8, 0x044c },
  { 0x06d9, 0x044b },
  { 0x06da, 0x0437 },
  { 0x06db, 0x0448 },
  { 0x06dc, 0x044d },
  { 0x06dd, 0x0449 },
  { 0x06de, 0x0447 },
  { 0x06df, 0x044a },
  { 0x06e0, 0x042e },
  { 0x06e1, 0x0410 },
  { 0x06e2, 0x0411 },
  { 0x06e3, 0x0426 },
  { 0x06e4, 0x0414 },
  { 0x06e5, 0x0415 },
  { 0x06e6, 0x0424 },
  { 0x06e7, 0x0413 },
  { 0x06e8, 0x0425 },
  { 0x06e9, 0x0418 },
  { 0x06ea, 0x0419 },
  { 0x06eb, 0x041a },
  { 0x06ec, 0x041b },
  { 0x06ed, 0x041c },
  { 0x06ee, 0x041d },
  { 0x06ef, 0x041e },
  { 0x06f0, 0x041f },
  { 0x06f1, 0x042f },
  { 0x06f2, 0x0420 },
  { 0x06f3, 0x0421 },
  { 0x06f4, 0x0422 },
  { 0x06f5, 0x0423 },
  { 0x06f6, 0x0416 },
  { 0x06f7, 0x0412 },
  { 0x06f8, 0x042c },
  { 0x06f9, 0x042b },
  { 0x06fa, 0x0417 },
  { 0x06fb, 0x0428 },
  { 0x06fc, 0x042d },
  { 0x06fd, 0x0429 },
  { 0x06fe, 0x0427 },
  { 0x06ff, 0x042a },
  { 0x07a1, 0x0386 },
  { 0x07a2, 0x0388 },
  { 0x07a3, 0x0389 },
  { 0x07a4, 0x038a },
  { 0x07a5, 0x03aa },
  { 0x07a7, 0x038c },
  { 0x07a8, 0x038e },
  { 0x07a9, 0x03ab },
  { 0x07ab, 0x038f },
  { 0x07ae, 0x0385 },
  { 0x07af, 0x2015 },
  { 0x07b1, 0x03ac },
  { 0x07b2, 0x03ad },
  { 0x07b3, 0x03ae },
  { 0x07b4, 0x03af },
  { 0x07b5, 0x03ca },
  { 0x07b6, 0x0390 },
  { 0x07b7, 0x03cc },
  { 0x07b8, 0x03cd },
  { 0x07b9, 0x03cb },
  { 0x07ba, 0x03b0 },
  { 0x07bb, 0x03ce },
  { 0x07c1, 0x0391 },
  { 0x07c2, 0x0392 },
  { 0x07c3, 0x0393 },
  { 0x07c4, 0x0394 },
  { 0x07c5, 0x0395 },
  { 0x07c6, 0x0396 },
  { 0x07c7, 0x0397 },
  { 0x07c8, 0x0398 },
  { 0x07c9, 0x0399 },
  { 0x07ca, 0x039a },
  { 0x07cb, 0x039b },
  { 0x07cc, 0x039c },
  { 0x07cd, 0x039d },
  { 0x07ce, 0x039e },
  { 0x07cf, 0x039f },
  { 0x07d0, 0x03a0 },
  { 0x07d1, 0x03a1 },
  { 0x07d2, 0x03a3 },
  { 0x07d4, 0x03a4 },
  { 0x07d5, 0x03a5 },
  { 0x07d6, 0x03a6 },
  { 0x07d7, 0x03a7 },
  { 0x07d8, 0x03a8 },
  { 0x07d9, 0x03a9 },
  { 0x07e1, 0x03b1 },
  { 0x07e2, 0x03b2 },
  { 0x07e3, 0x03b3 },
  { 0x07e4, 0x03b4 },
  { 0x07e5, 0x03b5 },
  { 0x07e6, 0x03b6 },
  { 0x07e7, 0x03b7 },
  { 0x07e8, 0x03b8 },
  { 0x07e9, 0x03b9 },
  { 0x07ea, 0x03ba },
  { 0x07eb, 0x03bb },
  { 0x07ec, 0x03bc },
  { 0x07ed, 0x03bd },
  { 0x07ee, 0x03be },
  { 0x07ef, 0x03bf },
  { 0x07f0, 0x03c0 },
  { 0x07f1, 0x03c1 },
  { 0x07f2, 0x03c3 },
  { 0x07f3, 0x03c2 },
  { 0x07f4, 0x03c4 },
  { 0x07f5, 0x03c5 },
  { 0x07f6, 0x03c6 },
  { 0x07f7, 0x03c7 },
  { 0x07f8, 0x03c8 },
  { 0x07f9, 0x03c9 },
  { 0x08a1, 0x23b7 },
  { 0x08a2, 0x250c },
  { 0x08a3, 0x2500 },
  { 0x08a4, 0x2320 },
  { 0x08a5, 0x2321 },
  { 0x08a6, 0x2502 },
  { 0x08a7, 0x23a1 },
  { 0x08a8, 0x23a3 },
  { 0x08a9, 0x23a4 },
  { 0x08aa, 0x23a6 },
  { 0x08ab, 0x239b },
  { 0x08ac, 0x239d },
  { 0x08ad, 0x239e },
  { 0x08ae, 0x23a0 },
  { 0x08af, 0x23a8 },
  { 0x08b0, 0x23ac },
  { 0x08bc, 0x2264 },
  { 0x08bd, 0x2260 },
  { 0x08be, 0x2265 },
  { 0x08bf, 0x222b },
  { 0x08c0, 0x2234 },
  { 0x08c1, 0x221d },
  { 0x08c2, 0x221e },
  { 0x08c5, 0x2207 },
  { 0x08c8, 0x223c },
  { 0x08c9, 0x2243 },
  { 0x08cd, 0x21d4 },
  { 0x08ce, 0x21d2 },
  { 0x08cf, 0x2261 },
  { 0x08d6, 0x221a },
  { 0x08da, 0x2282 },
  { 0x08db, 0x2283 },
  { 0x08dc, 0x2229 },
  { 0x08dd, 0x222a },
  { 0x08de, 0x2227 },
  { 0x08df, 0x2228 },
  { 0x08ef, 0x2202 },
  { 0x08f6, 0x0192 },
  { 0x08fb, 0x2190 },
  { 0x08fc, 0x2191 },
  { 0x08fd, 0x2192 },
  { 0x08fe, 0x2193 },
  { 0x09e0, 0x25c6 },
  { 0x09e1, 0x2592 },
  { 0x09e2, 0x2409 },
  { 0x09e3, 0x240c },
  { 0x09e4, 0x240d },
  { 0x09e5, 0x240a },
  { 0x09e8, 0x2424 },
  { 0x09e9, 0x240b },
  { 0x09ea, 0x2518 },
  { 0x09eb, 0x2510 },
  { 0x09ec, 0x250c },
  { 0x09ed, 0x2514 },
  { 0x09ee, 0x253c },
  { 0x09ef, 0x23ba },
  { 0x09f0, 0x23bb },
  { 0x09f1, 0x2500 },
  { 0x09f2, 0x23bc },
  { 0x09f3, 0x23bd },
  { 0x09f4, 0x251c },
  { 0x09f5, 0x2524 },
  { 0x09f6, 0x2534 },
  { 0x09f7, 0x252c },
  { 0x09f8, 0x2502 },
  { 0x0aa1, 0x2003 },
  { 0x0aa2, 0x2002 },
  { 0x0aa3, 0x2004 },
  { 0x0aa4, 0x2005 },
  { 0x0aa5, 0x2007 },
  { 0x0aa6, 0x2008 },
  { 0x0aa7, 0x2009 },
  { 0x0aa8, 0x200a },
  { 0x0aa9, 0x2014 },
  { 0x0aaa, 0x2013 },
  { 0x0aae, 0x2026 },
  { 0x0aaf, 0x2025 },
  { 0x0ab0, 0x2153 },
  { 0x0ab1, 0x2154 },
  { 0x0ab2, 0x2155 },
  { 0x0ab3, 0x2156 },
  { 0x0ab4, 0x2157 },
  { 0x0ab5, 0x2158 },
  { 0x0ab6, 0x2159 },
  { 0x0ab7, 0x215a },
  { 0x0ab8, 0x2105 },
  { 0x0abb, 0x2012 },
  { 0x0abc, 0x2329 },
  { 0x0abe, 0x232a },
  { 0x0ac3, 0x215b },
  { 0x0ac4, 0x215c },
  { 0x0ac5, 0x215d },
  { 0x0ac6, 0x215e },
  { 0x0ac9, 0x2122 },
  { 0x0aca, 0x2613 },
  { 0x0acc, 0x25c1 },
  { 0x0acd, 0x25b7 },
  { 0x0ace, 0x25cb },
  { 0x0acf, 0x25af },
  { 0x0ad0, 0x2018 },
  { 0x0ad1, 0x2019 },
  { 0x0ad2, 0x201c },
  { 0x0ad3, 0x201d },
  { 0x0ad4, 0x211e },
  { 0x0ad6, 0x2032 },
  { 0x0ad7, 0x2033 },
  { 0x0ad9, 0x271d },
  { 0x0adb, 0x25ac },
  { 0x0adc, 0x25c0 },
  { 0x0add, 0x25b6 },
  { 0x0ade, 0x25cf },
  { 0x0adf, 0x25ae },
  { 0x0ae0, 0x25e6 },
  { 0x0ae1, 0x25ab },
  { 0x0ae2, 0x25ad },
  { 0x0ae3, 0x25b3 },
  { 0x0ae4, 0x25bd },
  { 0x0ae5, 0x2606 },
  { 0x0ae6, 0x2022 },
  { 0x0ae7, 0x25aa },
  { 0x0ae8, 0x25b2 },
  { 0x0ae9, 0x25bc },
  { 0x0aea, 0x261c },
  { 0x0aeb, 0x261e },
  { 0x0aec, 0x2663 },
  { 0x0aed, 0x2666 },
  { 0x0aee, 0x2665 },
  { 0x0af0, 0x2720 },
  { 0x0af1, 0x2020 },
  { 0x0af2, 0x2021 },
  { 0x0af3, 0x2713 },
  { 0x0af4, 0x2717 },
  { 0x0af5, 0x266f },
  { 0x0af6, 0x266d },
  { 0x0af7, 0x2642 },
  { 0x0af8, 0x2640 },
  { 0x0af9, 0x260e },
  { 0x0afa, 0x2315 },
  { 0x0afb, 0x2117 },
  { 0x0afc, 0x2038 },
  { 0x0afd, 0x201a },
  { 0x0afe, 0x201e },
  { 0x0ba3, 0x003c },
  { 0x0ba6, 0x003e },
  { 0x0ba8, 0x2228 },
  { 0x0ba9, 0x2227 },
  { 0x0bc0, 0x00af },
  { 0x0bc2, 0x22a5 },
  { 0x0bc3, 0x2229 },
  { 0x0bc4, 0x230a },
  { 0x0bc6, 0x005f },
  { 0x0bca, 0x2218 },
  { 0x0bcc, 0x2395 },
  { 0x0bce, 0x22a4 },
  { 0x0bcf, 0x25cb },
  { 0x0bd3, 0x2308 },
  { 0x0bd6, 0x222a },
  { 0x0bd8, 0x2283 },
  { 0x0bda, 0x2282 },
  { 0x0bdc, 0x22a2 },
  { 0x0bfc, 0x22a3 },
  { 0x0cdf, 0x2017 },
  { 0x0ce0, 0x05d0 },
  { 0x0ce1, 0x05d1 },
  { 0x0ce2, 0x05d2 },
  { 0x0ce3, 0x05d3 },
  { 0x0ce4, 0x05d4 },
  { 0x0ce5, 0x05d5 },
  { 0x0ce6, 0x05d6 },
  { 0x0ce7, 0x05d7 },
  { 0x0ce8, 0x05d8 },
  { 0x0ce9, 0x05d9 },
  { 0x0cea, 0x05da },
  { 0x0ceb, 0x05db },
  { 0x0cec, 0x05dc },
  { 0x0ced, 0x05dd },
  { 0x0cee, 0x05de },
  { 0x0cef, 0x05df },
  { 0x0cf0, 0x05e0 },
  { 0x0cf1, 0x05e1 },
  { 0x0cf2, 0x05e2 },
  { 0x0cf3, 0x05e3 },
  { 0x0cf4, 0x05e4 },
  { 0x0cf5, 0x05e5 },
  { 0x0cf6, 0x05e6 },
  { 0x0cf7, 0x05e7 },
  { 0x0cf8, 0x05e8 },
  { 0x0cf9, 0x05e9 },
  { 0x0cfa, 0x05ea },
  { 0x0da1, 0x0e01 },
  { 0x0da2, 0x0e02 },
  { 0x0da3, 0x0e03 },
  { 0x0da4, 0x0e04 },
  { 0x0da5, 0x0e05 },
  { 0x0da6, 0x0e06 },
  { 0x0da7, 0x0e07 },
  { 0x0da8, 0x0e08 },
  { 0x0da9, 0x0e09 },
  { 0x0daa, 0x0e0a },
  { 0x0dab, 0x0e0b },
  { 0x0dac, 0x0e0c },
  { 0x0dad, 0x0e0d },
  { 0x0dae, 0x0e0e },
  { 0x0daf, 0x0e0f },
  { 0x0db0, 0x0e10 },
  { 0x0db1, 0x0e11 },
  { 0x0db2, 0x0e12 },
  { 0x0db3, 0x0e13 },
  { 0x0db4, 0x0e14 },
  { 0x0db5, 0x0e15 },
  { 0x0db6, 0x0e16 },
  { 0x0db7, 0x0e17 },
  { 0x0db8, 0x0e18 },
  { 0x0db9, 0x0e19 },
  { 0x0dba, 0x0e1a },
  { 0x0dbb, 0x0e1b },
  { 0x0dbc, 0x0e1c },
  { 0x0dbd, 0x0e1d },
  { 0x0dbe, 0x0e1e },
  { 0x0dbf, 0x0e1f },
  { 0x0dc0, 0x0e20 },
  { 0x0dc1, 0x0e21 },
  { 0x0dc2, 0x0e22 },
  { 0x0dc3, 0x0e23 },
  { 0x0dc4, 0x0e24 },
  { 0x0dc5, 0x0e25 },
  { 0x0dc6, 0x0e26 },
  { 0x0dc7, 0x0e27 },
  { 0x0dc8, 0x0e28 },
  { 0x0dc9, 0x0e29 },
  { 0x0dca, 0x0e2a },
  { 0x0dcb, 0x0e2b },
  { 0x0dcc, 0x0e2c },
  { 0x0dcd, 0x0e2d },
  { 0x0dce, 0x0e2e },
  { 0x0dcf, 0x0e2f },
  { 0x0dd0, 0x0e30 },
  { 0x0dd1, 0x0e31 },
  { 0x0dd2, 0x0e32 },
  { 0x0dd3, 0x0e33 },
  { 0x0dd4, 0x0e34 },
  { 0x0dd5, 0x0e35 },
  { 0x0dd6, 0x0e36 },
  { 0x0dd7, 0x0e37 },
  { 0x0dd8, 0x0e38 },
  { 0x0dd9, 0x0e39 },
  { 0x0dda, 0x0e3a },
  { 0x0ddf, 0x0e3f },
  { 0x0de0, 0x0e40 },
  { 0x0de1, 0x0e41 },
  { 0x0de2, 0x0e42 },
  { 0x0de3, 0x0e43 },
  { 0x0de4, 0x0e44 },
  { 0x0de5, 0x0e45 },
  { 0x0de6, 0x0e46 },
  { 0x0de7, 0x0e47 },
  { 0x0de8, 0x0e48 },
  { 0x0de9, 0x0e49 },
  { 0x0dea, 0x0e4a },
  { 0x0deb, 0x0e4b },
  { 0x0dec, 0x0e4c },
  { 0x0ded, 0x0e4d },
  { 0x0df0, 0x0e50 },
  { 0x0df1, 0x0e51 },
  { 0x0df2, 0x0e52 },
  { 0x0df3, 0x0e53 },
  { 0x0df4, 0x0e54 },
  { 0x0df5, 0x0e55 },
  { 0x0df6, 0x0e56 },
  { 0x0df7, 0x0e57 },
  { 0x0df8, 0x0e58 },
  { 0x0df9, 0x0e59 },
  { 0x0ea1, 0x3131 },
  { 0x0ea2, 0x3132 },
  { 0x0ea3, 0x3133 },
  { 0x0ea4, 0x3134 },
  { 0x0ea5, 0x3135 },
  { 0x0ea6, 0x3136 },
  { 0x0ea7, 0x3137 },
  { 0x0ea8, 0x3138 },
  { 0x0ea9, 0x3139 },
  { 0x0eaa, 0x313a },
  { 0x0eab, 0x313b },
  { 0x0eac, 0x313c },
  { 0x0ead, 0x313d },
  { 0x0eae, 0x313e },
  { 0x0eaf, 0x313f },
  { 0x0eb0, 0x3140 },
  { 0x0eb1, 0x3141 },
  { 0x0eb2, 0x3142 },
  { 0x0eb3, 0x3143 },
  { 0x0eb4, 0x3144 },
  { 0x0eb5, 0x3145 },
  { 0x0eb6, 0x3146 },
  { 0x0eb7, 0x3147 },
  { 0x0eb8, 0x3148 },
  { 0x0eb9, 0x3149 },
  { 0x0eba, 0x314a },
  { 0x0ebb, 0x314b },
  { 0x0ebc, 0x314c },
  { 0x0ebd, 0x314d },
  { 0x0ebe, 0x314e },
  { 0x0ebf, 0x314f },
  { 0x0ec0, 0x3150 },
  { 0x0ec1, 0x3151 },
  { 0x0ec2, 0x3152 },
  { 0x0ec3, 0x3153 },
  { 0x0ec4, 0x3154 },
  { 0x0ec5, 0x3155 },
  { 0x0ec6, 0x3156 },
  { 0x0ec7, 0x3157 },
  { 0x0ec8, 0x3158 },
  { 0x0ec9, 0x3159 },
  { 0x0eca, 0x315a },
  { 0x0ecb, 0x315b },
  { 0x0ecc, 0x315c },
  { 0x0ecd, 0x315d },
  { 0x0ece, 0x315e },
  { 0x0ecf, 0x315f },
  { 0x0ed0, 0x3160 },
  { 0x0ed1, 0x3161 },
  { 0x0ed2, 0x3162 },
  { 0x0ed3, 0x3163 },
  { 0x0ed4, 0x11a8 },
  { 0x0ed5, 0x11a9 },
  { 0x0ed6, 0x11aa },
  { 0x0ed7, 0x11ab },
  { 0x0ed8, 0x11ac },
  { 0x0ed9, 0x11ad },
  { 0x0eda, 0x11ae },
  { 0x0edb, 0x11af },
  { 0x0edc, 0x11b0 },
  { 0x0edd, 0x11b1 },
  { 0x0ede, 0x11b2 },
  { 0x0edf, 0x11b3 },
  { 0x0ee0, 0x11b4 },
  { 0x0ee1, 0x11b5 },
  { 0x0ee2, 0x11b6 },
  { 0x0ee3, 0x11b7 },
  { 0x0ee4, 0x11b8 },
  { 0x0ee5, 0x11b9 },
  { 0x0ee6, 0x11ba },
  { 0x0ee7, 0x11bb },
  { 0x0ee8, 0x11bc },
  { 0x0ee9, 0x11bd },
  { 0x0eea, 0x11be },
  { 0x0eeb, 0x11bf },
  { 0x0eec, 0x11c0 },
  { 0x0eed, 0x11c1 },
  { 0x0eee, 0x11c2 },
  { 0x0eef, 0x316d },
  { 0x0ef0, 0x3171 },
  { 0x0ef1, 0x3178 },
  { 0x0ef2, 0x317f },
  { 0x0ef3, 0x3181 },
  { 0x0ef4, 0x3184 },
  { 0x0ef5, 0x3186 },
  { 0x0ef6, 0x318d },
  { 0x0ef7, 0x318e },
  { 0x0ef8, 0x11eb },
  { 0x0ef9, 0x11f0 },
  { 0x0efa, 0x11f9 },
  { 0x0eff, 0x20a9 },
  { 0x13a4, 0x20ac },
  { 0x13bc, 0x0152 },
  { 0x13bd, 0x0153 },
  { 0x13be, 0x0178 },
  { 0x20ac, 0x20ac },
  { 0xfe50,    '`' },
  { 0xfe51, 0x00b4 },
  { 0xfe52,    '^' },
  { 0xfe53,    '~' },
  { 0xfe54, 0x00af },
  { 0xfe55, 0x02d8 },
  { 0xfe56, 0x02d9 },
  { 0xfe57, 0x00a8 },
  { 0xfe58, 0x02da },
  { 0xfe59, 0x02dd },
  { 0xfe5a, 0x02c7 },
  { 0xfe5b, 0x00b8 },
  { 0xfe5c, 0x02db },
  { 0xfe5d, 0x037a },
  { 0xfe5e, 0x309b },
  { 0xfe5f, 0x309c },
  { 0xfe63,    '/' },
  { 0xfe64, 0x02bc },
  { 0xfe65, 0x02bd },
  { 0xfe66, 0x02f5 },
  { 0xfe67, 0x02f3 },
  { 0xfe68, 0x02cd },
  { 0xfe69, 0xa788 },
  { 0xfe6a, 0x02f7 },
  { 0xfe6e,    ',' },
  { 0xfe6f, 0x00a4 },
  { 0xfe80,    'a' }, /* XK_dead_a */
  { 0xfe81,    'A' }, /* XK_dead_A */
  { 0xfe82,    'e' }, /* XK_dead_e */
  { 0xfe83,    'E' }, /* XK_dead_E */
  { 0xfe84,    'i' }, /* XK_dead_i */
  { 0xfe85,    'I' }, /* XK_dead_I */
  { 0xfe86,    'o' }, /* XK_dead_o */
  { 0xfe87,    'O' }, /* XK_dead_O */
  { 0xfe88,    'u' }, /* XK_dead_u */
  { 0xfe89,    'U' }, /* XK_dead_U */
  { 0xfe8a, 0x0259 },
  { 0xfe8b, 0x018f },
  { 0xfe8c, 0x00b5 },
  { 0xfe90,    '_' },
  { 0xfe91, 0x02c8 },
  { 0xfe92, 0x02cc },
  { 0xff80 /*XKB_KEY_KP_Space*/,     ' ' },
  { 0xff95 /*XKB_KEY_KP_7*/, 0x0037 },
  { 0xff96 /*XKB_KEY_KP_4*/, 0x0034 },
  { 0xff97 /*XKB_KEY_KP_8*/, 0x0038 },
  { 0xff98 /*XKB_KEY_KP_6*/, 0x0036 },
  { 0xff99 /*XKB_KEY_KP_2*/, 0x0032 },
  { 0xff9a /*XKB_KEY_KP_9*/, 0x0039 },
  { 0xff9b /*XKB_KEY_KP_3*/, 0x0033 },
  { 0xff9c /*XKB_KEY_KP_1*/, 0x0031 },
  { 0xff9d /*XKB_KEY_KP_5*/, 0x0035 },
  { 0xff9e /*XKB_KEY_KP_0*/, 0x0030 },
  { 0xffaa /*XKB_KEY_KP_Multiply*/,  '*' },
  { 0xffab /*XKB_KEY_KP_Add*/,       '+' },
  { 0xffac /*XKB_KEY_KP_Separator*/, ',' },
  { 0xffad /*XKB_KEY_KP_Subtract*/,  '-' },
  { 0xffae /*XKB_KEY_KP_Decimal*/,   '.' },
  { 0xffaf /*XKB_KEY_KP_Divide*/,    '/' },
  { 0xffb0 /*XKB_KEY_KP_0*/, 0x0030 },
  { 0xffb1 /*XKB_KEY_KP_1*/, 0x0031 },
  { 0xffb2 /*XKB_KEY_KP_2*/, 0x0032 },
  { 0xffb3 /*XKB_KEY_KP_3*/, 0x0033 },
  { 0xffb4 /*XKB_KEY_KP_4*/, 0x0034 },
  { 0xffb5 /*XKB_KEY_KP_5*/, 0x0035 },
  { 0xffb6 /*XKB_KEY_KP_6*/, 0x0036 },
  { 0xffb7 /*XKB_KEY_KP_7*/, 0x0037 },
  { 0xffb8 /*XKB_KEY_KP_8*/, 0x0038 },
  { 0xffb9 /*XKB_KEY_KP_9*/, 0x0039 },
  { 0xffbd /*XKB_KEY_KP_Equal*/,     '=' }
};

_SOKOL_PRIVATE int _sapp_x11_error_handler(Display* display, XErrorEvent* event) {
    _SOKOL_UNUSED(display);
    _sapp.x11.error_code = event->error_code;
    return 0;
}

_SOKOL_PRIVATE void _sapp_x11_grab_error_handler(void) {
    _sapp.x11.error_code = Success;
    XSetErrorHandler(_sapp_x11_error_handler);
}

_SOKOL_PRIVATE void _sapp_x11_release_error_handler(void) {
    XSync(_sapp.x11.display, False);
    XSetErrorHandler(NULL);
}

_SOKOL_PRIVATE void _sapp_x11_init_extensions(void) {
    _sapp.x11.UTF8_STRING             = XInternAtom(_sapp.x11.display, "UTF8_STRING", False);
    _sapp.x11.WM_PROTOCOLS            = XInternAtom(_sapp.x11.display, "WM_PROTOCOLS", False);
    _sapp.x11.WM_DELETE_WINDOW        = XInternAtom(_sapp.x11.display, "WM_DELETE_WINDOW", False);
    _sapp.x11.WM_STATE                = XInternAtom(_sapp.x11.display, "WM_STATE", False);
    _sapp.x11.NET_WM_NAME             = XInternAtom(_sapp.x11.display, "_NET_WM_NAME", False);
    _sapp.x11.NET_WM_ICON_NAME        = XInternAtom(_sapp.x11.display, "_NET_WM_ICON_NAME", False);
    _sapp.x11.NET_WM_ICON             = XInternAtom(_sapp.x11.display, "_NET_WM_ICON", False);
    _sapp.x11.NET_WM_STATE            = XInternAtom(_sapp.x11.display, "_NET_WM_STATE", False);
    _sapp.x11.NET_WM_STATE_FULLSCREEN = XInternAtom(_sapp.x11.display, "_NET_WM_STATE_FULLSCREEN", False);
    if (_sapp.drop.enabled) {
        _sapp.x11.xdnd.XdndAware        = XInternAtom(_sapp.x11.display, "XdndAware", False);
        _sapp.x11.xdnd.XdndEnter        = XInternAtom(_sapp.x11.display, "XdndEnter", False);
        _sapp.x11.xdnd.XdndPosition     = XInternAtom(_sapp.x11.display, "XdndPosition", False);
        _sapp.x11.xdnd.XdndStatus       = XInternAtom(_sapp.x11.display, "XdndStatus", False);
        _sapp.x11.xdnd.XdndActionCopy   = XInternAtom(_sapp.x11.display, "XdndActionCopy", False);
        _sapp.x11.xdnd.XdndDrop         = XInternAtom(_sapp.x11.display, "XdndDrop", False);
        _sapp.x11.xdnd.XdndFinished     = XInternAtom(_sapp.x11.display, "XdndFinished", False);
        _sapp.x11.xdnd.XdndSelection    = XInternAtom(_sapp.x11.display, "XdndSelection", False);
        _sapp.x11.xdnd.XdndTypeList     = XInternAtom(_sapp.x11.display, "XdndTypeList", False);
        _sapp.x11.xdnd.text_uri_list    = XInternAtom(_sapp.x11.display, "text/uri-list", False);
    }

    /* check Xi extension for raw mouse input */
    if (XQueryExtension(_sapp.x11.display, "XInputExtension", &_sapp.x11.xi.major_opcode, &_sapp.x11.xi.event_base, &_sapp.x11.xi.error_base)) {
        _sapp.x11.xi.major = 2;
        _sapp.x11.xi.minor = 0;
        if (XIQueryVersion(_sapp.x11.display, &_sapp.x11.xi.major, &_sapp.x11.xi.minor) == Success) {
            _sapp.x11.xi.available = true;
        }
    }
}

// translate the X11 KeySyms for a key to sokol-app key code
// NOTE: this is only used as a fallback, in case the XBK method fails
//       it is layout-dependent and will fail partially on most non-US layouts.
//
_SOKOL_PRIVATE sapp_keycode _sapp_x11_translate_keysyms(const KeySym* keysyms, int width) {
    if (width > 1) {
        switch (keysyms[1]) {
            case XK_KP_0:           return SAPP_KEYCODE_KP_0;
            case XK_KP_1:           return SAPP_KEYCODE_KP_1;
            case XK_KP_2:           return SAPP_KEYCODE_KP_2;
            case XK_KP_3:           return SAPP_KEYCODE_KP_3;
            case XK_KP_4:           return SAPP_KEYCODE_KP_4;
            case XK_KP_5:           return SAPP_KEYCODE_KP_5;
            case XK_KP_6:           return SAPP_KEYCODE_KP_6;
            case XK_KP_7:           return SAPP_KEYCODE_KP_7;
            case XK_KP_8:           return SAPP_KEYCODE_KP_8;
            case XK_KP_9:           return SAPP_KEYCODE_KP_9;
            case XK_KP_Separator:
            case XK_KP_Decimal:     return SAPP_KEYCODE_KP_DECIMAL;
            case XK_KP_Equal:       return SAPP_KEYCODE_KP_EQUAL;
            case XK_KP_Enter:       return SAPP_KEYCODE_KP_ENTER;
            default:                break;
        }
    }

    switch (keysyms[0]) {
        case XK_Escape:         return SAPP_KEYCODE_ESCAPE;
        case XK_Tab:            return SAPP_KEYCODE_TAB;
        case XK_Shift_L:        return SAPP_KEYCODE_LEFT_SHIFT;
        case XK_Shift_R:        return SAPP_KEYCODE_RIGHT_SHIFT;
        case XK_Control_L:      return SAPP_KEYCODE_LEFT_CONTROL;
        case XK_Control_R:      return SAPP_KEYCODE_RIGHT_CONTROL;
        case XK_Meta_L:
        case XK_Alt_L:          return SAPP_KEYCODE_LEFT_ALT;
        case XK_Mode_switch: // Mapped to Alt_R on many keyboards
        case XK_ISO_Level3_Shift: // AltGr on at least some machines
        case XK_Meta_R:
        case XK_Alt_R:          return SAPP_KEYCODE_RIGHT_ALT;
        case XK_Super_L:        return SAPP_KEYCODE_LEFT_SUPER;
        case XK_Super_R:        return SAPP_KEYCODE_RIGHT_SUPER;
        case XK_Menu:           return SAPP_KEYCODE_MENU;
        case XK_Num_Lock:       return SAPP_KEYCODE_NUM_LOCK;
        case XK_Caps_Lock:      return SAPP_KEYCODE_CAPS_LOCK;
        case XK_Print:          return SAPP_KEYCODE_PRINT_SCREEN;
        case XK_Scroll_Lock:    return SAPP_KEYCODE_SCROLL_LOCK;
        case XK_Pause:          return SAPP_KEYCODE_PAUSE;
        case XK_Delete:         return SAPP_KEYCODE_DELETE;
        case XK_BackSpace:      return SAPP_KEYCODE_BACKSPACE;
        case XK_Return:         return SAPP_KEYCODE_ENTER;
        case XK_Home:           return SAPP_KEYCODE_HOME;
        case XK_End:            return SAPP_KEYCODE_END;
        case XK_Page_Up:        return SAPP_KEYCODE_PAGE_UP;
        case XK_Page_Down:      return SAPP_KEYCODE_PAGE_DOWN;
        case XK_Insert:         return SAPP_KEYCODE_INSERT;
        case XK_Left:           return SAPP_KEYCODE_LEFT;
        case XK_Right:          return SAPP_KEYCODE_RIGHT;
        case XK_Down:           return SAPP_KEYCODE_DOWN;
        case XK_Up:             return SAPP_KEYCODE_UP;
        case XK_F1:             return SAPP_KEYCODE_F1;
        case XK_F2:             return SAPP_KEYCODE_F2;
        case XK_F3:             return SAPP_KEYCODE_F3;
        case XK_F4:             return SAPP_KEYCODE_F4;
        case XK_F5:             return SAPP_KEYCODE_F5;
        case XK_F6:             return SAPP_KEYCODE_F6;
        case XK_F7:             return SAPP_KEYCODE_F7;
        case XK_F8:             return SAPP_KEYCODE_F8;
        case XK_F9:             return SAPP_KEYCODE_F9;
        case XK_F10:            return SAPP_KEYCODE_F10;
        case XK_F11:            return SAPP_KEYCODE_F11;
        case XK_F12:            return SAPP_KEYCODE_F12;
        case XK_F13:            return SAPP_KEYCODE_F13;
        case XK_F14:            return SAPP_KEYCODE_F14;
        case XK_F15:            return SAPP_KEYCODE_F15;
        case XK_F16:            return SAPP_KEYCODE_F16;
        case XK_F17:            return SAPP_KEYCODE_F17;
        case XK_F18:            return SAPP_KEYCODE_F18;
        case XK_F19:            return SAPP_KEYCODE_F19;
        case XK_F20:            return SAPP_KEYCODE_F20;
        case XK_F21:            return SAPP_KEYCODE_F21;
        case XK_F22:            return SAPP_KEYCODE_F22;
        case XK_F23:            return SAPP_KEYCODE_F23;
        case XK_F24:            return SAPP_KEYCODE_F24;
        case XK_F25:            return SAPP_KEYCODE_F25;

        // numeric keypad
        case XK_KP_Divide:      return SAPP_KEYCODE_KP_DIVIDE;
        case XK_KP_Multiply:    return SAPP_KEYCODE_KP_MULTIPLY;
        case XK_KP_Subtract:    return SAPP_KEYCODE_KP_SUBTRACT;
        case XK_KP_Add:         return SAPP_KEYCODE_KP_ADD;

        // these should have been detected in secondary keysym test above!
        case XK_KP_Insert:      return SAPP_KEYCODE_KP_0;
        case XK_KP_End:         return SAPP_KEYCODE_KP_1;
        case XK_KP_Down:        return SAPP_KEYCODE_KP_2;
        case XK_KP_Page_Down:   return SAPP_KEYCODE_KP_3;
        case XK_KP_Left:        return SAPP_KEYCODE_KP_4;
        case XK_KP_Right:       return SAPP_KEYCODE_KP_6;
        case XK_KP_Home:        return SAPP_KEYCODE_KP_7;
        case XK_KP_Up:          return SAPP_KEYCODE_KP_8;
        case XK_KP_Page_Up:     return SAPP_KEYCODE_KP_9;
        case XK_KP_Delete:      return SAPP_KEYCODE_KP_DECIMAL;
        case XK_KP_Equal:       return SAPP_KEYCODE_KP_EQUAL;
        case XK_KP_Enter:       return SAPP_KEYCODE_KP_ENTER;

        // last resort: Check for printable keys (should not happen if the XKB
        // extension is available). This will give a layout dependent mapping
        // (which is wrong, and we may miss some keys, especially on non-US
        // keyboards), but it's better than nothing...
        case XK_a:              return SAPP_KEYCODE_A;
        case XK_b:              return SAPP_KEYCODE_B;
        case XK_c:              return SAPP_KEYCODE_C;
        case XK_d:              return SAPP_KEYCODE_D;
        case XK_e:              return SAPP_KEYCODE_E;
        case XK_f:              return SAPP_KEYCODE_F;
        case XK_g:              return SAPP_KEYCODE_G;
        case XK_h:              return SAPP_KEYCODE_H;
        case XK_i:              return SAPP_KEYCODE_I;
        case XK_j:              return SAPP_KEYCODE_J;
        case XK_k:              return SAPP_KEYCODE_K;
        case XK_l:              return SAPP_KEYCODE_L;
        case XK_m:              return SAPP_KEYCODE_M;
        case XK_n:              return SAPP_KEYCODE_N;
        case XK_o:              return SAPP_KEYCODE_O;
        case XK_p:              return SAPP_KEYCODE_P;
        case XK_q:              return SAPP_KEYCODE_Q;
        case XK_r:              return SAPP_KEYCODE_R;
        case XK_s:              return SAPP_KEYCODE_S;
        case XK_t:              return SAPP_KEYCODE_T;
        case XK_u:              return SAPP_KEYCODE_U;
        case XK_v:              return SAPP_KEYCODE_V;
        case XK_w:              return SAPP_KEYCODE_W;
        case XK_x:              return SAPP_KEYCODE_X;
        case XK_y:              return SAPP_KEYCODE_Y;
        case XK_z:              return SAPP_KEYCODE_Z;
        case XK_1:              return SAPP_KEYCODE_1;
        case XK_2:              return SAPP_KEYCODE_2;
        case XK_3:              return SAPP_KEYCODE_3;
        case XK_4:              return SAPP_KEYCODE_4;
        case XK_5:              return SAPP_KEYCODE_5;
        case XK_6:              return SAPP_KEYCODE_6;
        case XK_7:              return SAPP_KEYCODE_7;
        case XK_8:              return SAPP_KEYCODE_8;
        case XK_9:              return SAPP_KEYCODE_9;
        case XK_0:              return SAPP_KEYCODE_0;
        case XK_space:          return SAPP_KEYCODE_SPACE;
        case XK_minus:          return SAPP_KEYCODE_MINUS;
        case XK_equal:          return SAPP_KEYCODE_EQUAL;
        case XK_bracketleft:    return SAPP_KEYCODE_LEFT_BRACKET;
        case XK_bracketright:   return SAPP_KEYCODE_RIGHT_BRACKET;
        case XK_backslash:      return SAPP_KEYCODE_BACKSLASH;
        case XK_semicolon:      return SAPP_KEYCODE_SEMICOLON;
        case XK_apostrophe:     return SAPP_KEYCODE_APOSTROPHE;
        case XK_grave:          return SAPP_KEYCODE_GRAVE_ACCENT;
        case XK_comma:          return SAPP_KEYCODE_COMMA;
        case XK_period:         return SAPP_KEYCODE_PERIOD;
        case XK_slash:          return SAPP_KEYCODE_SLASH;
        case XK_less:           return SAPP_KEYCODE_WORLD_1; // At least in some layouts...
        default:                break;
    }

    // no matching translation was found
    return SAPP_KEYCODE_INVALID;
}


// setup dynamic keycode/scancode mapping tables, this is required
// for getting layout-independent keycodes on X11.
//
// see GLFW x11_init.c/createKeyTables()
_SOKOL_PRIVATE void _sapp_x11_init_keytable(void) {
    for (int i = 0; i < SAPP_MAX_KEYCODES; i++) {
        _sapp.keycodes[i] = SAPP_KEYCODE_INVALID;
    }
    // use XKB to determine physical key locations independently of the current keyboard layout
    XkbDescPtr desc = XkbGetMap(_sapp.x11.display, 0, XkbUseCoreKbd);
    SOKOL_ASSERT(desc);
    XkbGetNames(_sapp.x11.display, XkbKeyNamesMask | XkbKeyAliasesMask, desc);

    const int scancode_min = desc->min_key_code;
    const int scancode_max = desc->max_key_code;

    const struct { sapp_keycode key; const char* name; } keymap[] = {
        { SAPP_KEYCODE_GRAVE_ACCENT, "TLDE" },
        { SAPP_KEYCODE_1, "AE01" },
        { SAPP_KEYCODE_2, "AE02" },
        { SAPP_KEYCODE_3, "AE03" },
        { SAPP_KEYCODE_4, "AE04" },
        { SAPP_KEYCODE_5, "AE05" },
        { SAPP_KEYCODE_6, "AE06" },
        { SAPP_KEYCODE_7, "AE07" },
        { SAPP_KEYCODE_8, "AE08" },
        { SAPP_KEYCODE_9, "AE09" },
        { SAPP_KEYCODE_0, "AE10" },
        { SAPP_KEYCODE_MINUS, "AE11" },
        { SAPP_KEYCODE_EQUAL, "AE12" },
        { SAPP_KEYCODE_Q, "AD01" },
        { SAPP_KEYCODE_W, "AD02" },
        { SAPP_KEYCODE_E, "AD03" },
        { SAPP_KEYCODE_R, "AD04" },
        { SAPP_KEYCODE_T, "AD05" },
        { SAPP_KEYCODE_Y, "AD06" },
        { SAPP_KEYCODE_U, "AD07" },
        { SAPP_KEYCODE_I, "AD08" },
        { SAPP_KEYCODE_O, "AD09" },
        { SAPP_KEYCODE_P, "AD10" },
        { SAPP_KEYCODE_LEFT_BRACKET, "AD11" },
        { SAPP_KEYCODE_RIGHT_BRACKET, "AD12" },
        { SAPP_KEYCODE_A, "AC01" },
        { SAPP_KEYCODE_S, "AC02" },
        { SAPP_KEYCODE_D, "AC03" },
        { SAPP_KEYCODE_F, "AC04" },
        { SAPP_KEYCODE_G, "AC05" },
        { SAPP_KEYCODE_H, "AC06" },
        { SAPP_KEYCODE_J, "AC07" },
        { SAPP_KEYCODE_K, "AC08" },
        { SAPP_KEYCODE_L, "AC09" },
        { SAPP_KEYCODE_SEMICOLON, "AC10" },
        { SAPP_KEYCODE_APOSTROPHE, "AC11" },
        { SAPP_KEYCODE_Z, "AB01" },
        { SAPP_KEYCODE_X, "AB02" },
        { SAPP_KEYCODE_C, "AB03" },
        { SAPP_KEYCODE_V, "AB04" },
        { SAPP_KEYCODE_B, "AB05" },
        { SAPP_KEYCODE_N, "AB06" },
        { SAPP_KEYCODE_M, "AB07" },
        { SAPP_KEYCODE_COMMA, "AB08" },
        { SAPP_KEYCODE_PERIOD, "AB09" },
        { SAPP_KEYCODE_SLASH, "AB10" },
        { SAPP_KEYCODE_BACKSLASH, "BKSL" },
        { SAPP_KEYCODE_WORLD_1, "LSGT" },
        { SAPP_KEYCODE_SPACE, "SPCE" },
        { SAPP_KEYCODE_ESCAPE, "ESC" },
        { SAPP_KEYCODE_ENTER, "RTRN" },
        { SAPP_KEYCODE_TAB, "TAB" },
        { SAPP_KEYCODE_BACKSPACE, "BKSP" },
        { SAPP_KEYCODE_INSERT, "INS" },
        { SAPP_KEYCODE_DELETE, "DELE" },
        { SAPP_KEYCODE_RIGHT, "RGHT" },
        { SAPP_KEYCODE_LEFT, "LEFT" },
        { SAPP_KEYCODE_DOWN, "DOWN" },
        { SAPP_KEYCODE_UP, "UP" },
        { SAPP_KEYCODE_PAGE_UP, "PGUP" },
        { SAPP_KEYCODE_PAGE_DOWN, "PGDN" },
        { SAPP_KEYCODE_HOME, "HOME" },
        { SAPP_KEYCODE_END, "END" },
        { SAPP_KEYCODE_CAPS_LOCK, "CAPS" },
        { SAPP_KEYCODE_SCROLL_LOCK, "SCLK" },
        { SAPP_KEYCODE_NUM_LOCK, "NMLK" },
        { SAPP_KEYCODE_PRINT_SCREEN, "PRSC" },
        { SAPP_KEYCODE_PAUSE, "PAUS" },
        { SAPP_KEYCODE_F1, "FK01" },
        { SAPP_KEYCODE_F2, "FK02" },
        { SAPP_KEYCODE_F3, "FK03" },
        { SAPP_KEYCODE_F4, "FK04" },
        { SAPP_KEYCODE_F5, "FK05" },
        { SAPP_KEYCODE_F6, "FK06" },
        { SAPP_KEYCODE_F7, "FK07" },
        { SAPP_KEYCODE_F8, "FK08" },
        { SAPP_KEYCODE_F9, "FK09" },
        { SAPP_KEYCODE_F10, "FK10" },
        { SAPP_KEYCODE_F11, "FK11" },
        { SAPP_KEYCODE_F12, "FK12" },
        { SAPP_KEYCODE_F13, "FK13" },
        { SAPP_KEYCODE_F14, "FK14" },
        { SAPP_KEYCODE_F15, "FK15" },
        { SAPP_KEYCODE_F16, "FK16" },
        { SAPP_KEYCODE_F17, "FK17" },
        { SAPP_KEYCODE_F18, "FK18" },
        { SAPP_KEYCODE_F19, "FK19" },
        { SAPP_KEYCODE_F20, "FK20" },
        { SAPP_KEYCODE_F21, "FK21" },
        { SAPP_KEYCODE_F22, "FK22" },
        { SAPP_KEYCODE_F23, "FK23" },
        { SAPP_KEYCODE_F24, "FK24" },
        { SAPP_KEYCODE_F25, "FK25" },
        { SAPP_KEYCODE_KP_0, "KP0" },
        { SAPP_KEYCODE_KP_1, "KP1" },
        { SAPP_KEYCODE_KP_2, "KP2" },
        { SAPP_KEYCODE_KP_3, "KP3" },
        { SAPP_KEYCODE_KP_4, "KP4" },
        { SAPP_KEYCODE_KP_5, "KP5" },
        { SAPP_KEYCODE_KP_6, "KP6" },
        { SAPP_KEYCODE_KP_7, "KP7" },
        { SAPP_KEYCODE_KP_8, "KP8" },
        { SAPP_KEYCODE_KP_9, "KP9" },
        { SAPP_KEYCODE_KP_DECIMAL, "KPDL" },
        { SAPP_KEYCODE_KP_DIVIDE, "KPDV" },
        { SAPP_KEYCODE_KP_MULTIPLY, "KPMU" },
        { SAPP_KEYCODE_KP_SUBTRACT, "KPSU" },
        { SAPP_KEYCODE_KP_ADD, "KPAD" },
        { SAPP_KEYCODE_KP_ENTER, "KPEN" },
        { SAPP_KEYCODE_KP_EQUAL, "KPEQ" },
        { SAPP_KEYCODE_LEFT_SHIFT, "LFSH" },
        { SAPP_KEYCODE_LEFT_CONTROL, "LCTL" },
        { SAPP_KEYCODE_LEFT_ALT, "LALT" },
        { SAPP_KEYCODE_LEFT_SUPER, "LWIN" },
        { SAPP_KEYCODE_RIGHT_SHIFT, "RTSH" },
        { SAPP_KEYCODE_RIGHT_CONTROL, "RCTL" },
        { SAPP_KEYCODE_RIGHT_ALT, "RALT" },
        { SAPP_KEYCODE_RIGHT_ALT, "LVL3" },
        { SAPP_KEYCODE_RIGHT_ALT, "MDSW" },
        { SAPP_KEYCODE_RIGHT_SUPER, "RWIN" },
        { SAPP_KEYCODE_MENU, "MENU" }
    };
    const int num_keymap_items = (int)(sizeof(keymap) / sizeof(keymap[0]));

    // find X11 keycode to sokol-app key code mapping
    for (int scancode = scancode_min; scancode <= scancode_max; scancode++) {
        sapp_keycode key = SAPP_KEYCODE_INVALID;
        for (int i = 0; i < num_keymap_items; i++) {
            if (strncmp(desc->names->keys[scancode].name, keymap[i].name, XkbKeyNameLength) == 0) {
                key = keymap[i].key;
                break;
            }
        }

        // fall back to key aliases in case the key name did not match
        for (int i = 0; i < desc->names->num_key_aliases; i++) {
            if (key != SAPP_KEYCODE_INVALID) {
                break;
            }
            if (strncmp(desc->names->key_aliases[i].real, desc->names->keys[scancode].name, XkbKeyNameLength) != 0) {
                continue;
            }
            for (int j = 0; j < num_keymap_items; j++) {
                if (strncmp(desc->names->key_aliases[i].alias, keymap[i].name, XkbKeyNameLength) == 0) {
                    key = keymap[i].key;
                    break;
                }
            }
        }
        _sapp.keycodes[scancode] = key;
    }
    XkbFreeNames(desc, XkbKeyNamesMask, True);
    XkbFreeKeyboard(desc, 0, True);

    int width = 0;
    KeySym* keysyms = XGetKeyboardMapping(_sapp.x11.display, scancode_min, scancode_max - scancode_min + 1, &width);
    for (int scancode = scancode_min; scancode <= scancode_max; scancode++) {
        // translate untranslated key codes using the traditional X11 KeySym lookups
        if (_sapp.keycodes[scancode] == SAPP_KEYCODE_INVALID) {
            const size_t base = (size_t)((scancode - scancode_min) * width);
            _sapp.keycodes[scancode] = _sapp_x11_translate_keysyms(&keysyms[base], width);
        }
    }
    XFree(keysyms);
}

_SOKOL_PRIVATE void _sapp_x11_query_system_dpi(void) {
    /* from GLFW:

       NOTE: Default to the display-wide DPI as we don't currently have a policy
             for which monitor a window is considered to be on

        _sapp.x11.dpi = DisplayWidth(_sapp.x11.display, _sapp.x11.screen) *
                        25.4f / DisplayWidthMM(_sapp.x11.display, _sapp.x11.screen);

       NOTE: Basing the scale on Xft.dpi where available should provide the most
             consistent user experience (matches Qt, Gtk, etc), although not
             always the most accurate one
    */
    bool dpi_ok = false;
    char* rms = XResourceManagerString(_sapp.x11.display);
    if (rms) {
        XrmDatabase db = XrmGetStringDatabase(rms);
        if (db) {
            XrmValue value;
            char* type = NULL;
            if (XrmGetResource(db, "Xft.dpi", "Xft.Dpi", &type, &value)) {
                if (type && strcmp(type, "String") == 0) {
                    _sapp.x11.dpi = atof(value.addr);
                    dpi_ok = true;
                }
            }
            XrmDestroyDatabase(db);
        }
    }
    // fallback if querying DPI had failed: assume the standard DPI 96.0f
    if (!dpi_ok) {
        _sapp.x11.dpi = 96.0f;
        _SAPP_WARN(LINUX_X11_QUERY_SYSTEM_DPI_FAILED);
    }
}

#if defined(_SAPP_GLX)

_SOKOL_PRIVATE bool _sapp_glx_has_ext(const char* ext, const char* extensions) {
    SOKOL_ASSERT(ext);
    const char* start = extensions;
    while (true) {
        const char* where = strstr(start, ext);
        if (!where) {
            return false;
        }
        const char* terminator = where + strlen(ext);
        if ((where == start) || (*(where - 1) == ' ')) {
            if (*terminator == ' ' || *terminator == '\0') {
                break;
            }
        }
        start = terminator;
    }
    return true;
}

_SOKOL_PRIVATE bool _sapp_glx_extsupported(const char* ext, const char* extensions) {
    if (extensions) {
        return _sapp_glx_has_ext(ext, extensions);
    }
    else {
        return false;
    }
}

_SOKOL_PRIVATE void* _sapp_glx_getprocaddr(const char* procname)
{
    if (_sapp.glx.GetProcAddress) {
        return (void*) _sapp.glx.GetProcAddress(procname);
    }
    else if (_sapp.glx.GetProcAddressARB) {
        return (void*) _sapp.glx.GetProcAddressARB(procname);
    }
    else {
        return dlsym(_sapp.glx.libgl, procname);
    }
}

_SOKOL_PRIVATE void _sapp_glx_init(void) {
    const char* sonames[] = { "libGL.so.1", "libGL.so", 0 };
    for (int i = 0; sonames[i]; i++) {
        _sapp.glx.libgl = dlopen(sonames[i], RTLD_LAZY|RTLD_GLOBAL);
        if (_sapp.glx.libgl) {
            break;
        }
    }
    if (!_sapp.glx.libgl) {
        _SAPP_PANIC(LINUX_GLX_LOAD_LIBGL_FAILED);
    }
    _sapp.glx.GetFBConfigs          = (PFNGLXGETFBCONFIGSPROC)          dlsym(_sapp.glx.libgl, "glXGetFBConfigs");
    _sapp.glx.GetFBConfigAttrib     = (PFNGLXGETFBCONFIGATTRIBPROC)     dlsym(_sapp.glx.libgl, "glXGetFBConfigAttrib");
    _sapp.glx.GetClientString       = (PFNGLXGETCLIENTSTRINGPROC)       dlsym(_sapp.glx.libgl, "glXGetClientString");
    _sapp.glx.QueryExtension        = (PFNGLXQUERYEXTENSIONPROC)        dlsym(_sapp.glx.libgl, "glXQueryExtension");
    _sapp.glx.QueryVersion          = (PFNGLXQUERYVERSIONPROC)          dlsym(_sapp.glx.libgl, "glXQueryVersion");
    _sapp.glx.DestroyContext        = (PFNGLXDESTROYCONTEXTPROC)        dlsym(_sapp.glx.libgl, "glXDestroyContext");
    _sapp.glx.MakeCurrent           = (PFNGLXMAKECURRENTPROC)           dlsym(_sapp.glx.libgl, "glXMakeCurrent");
    _sapp.glx.SwapBuffers           = (PFNGLXSWAPBUFFERSPROC)           dlsym(_sapp.glx.libgl, "glXSwapBuffers");
    _sapp.glx.QueryExtensionsString = (PFNGLXQUERYEXTENSIONSSTRINGPROC) dlsym(_sapp.glx.libgl, "glXQueryExtensionsString");
    _sapp.glx.CreateWindow          = (PFNGLXCREATEWINDOWPROC)          dlsym(_sapp.glx.libgl, "glXCreateWindow");
    _sapp.glx.DestroyWindow         = (PFNGLXDESTROYWINDOWPROC)         dlsym(_sapp.glx.libgl, "glXDestroyWindow");
    _sapp.glx.GetProcAddress        = (PFNGLXGETPROCADDRESSPROC)        dlsym(_sapp.glx.libgl, "glXGetProcAddress");
    _sapp.glx.GetProcAddressARB     = (PFNGLXGETPROCADDRESSPROC)        dlsym(_sapp.glx.libgl, "glXGetProcAddressARB");
    _sapp.glx.GetVisualFromFBConfig = (PFNGLXGETVISUALFROMFBCONFIGPROC) dlsym(_sapp.glx.libgl, "glXGetVisualFromFBConfig");
    if (!_sapp.glx.GetFBConfigs ||
        !_sapp.glx.GetFBConfigAttrib ||
        !_sapp.glx.GetClientString ||
        !_sapp.glx.QueryExtension ||
        !_sapp.glx.QueryVersion ||
        !_sapp.glx.DestroyContext ||
        !_sapp.glx.MakeCurrent ||
        !_sapp.glx.SwapBuffers ||
        !_sapp.glx.QueryExtensionsString ||
        !_sapp.glx.CreateWindow ||
        !_sapp.glx.DestroyWindow ||
        !_sapp.glx.GetProcAddress ||
        !_sapp.glx.GetProcAddressARB ||
        !_sapp.glx.GetVisualFromFBConfig)
    {
        _SAPP_PANIC(LINUX_GLX_LOAD_ENTRY_POINTS_FAILED);
    }

    if (!_sapp.glx.QueryExtension(_sapp.x11.display, &_sapp.glx.error_base, &_sapp.glx.event_base)) {
        _SAPP_PANIC(LINUX_GLX_EXTENSION_NOT_FOUND);
    }
    if (!_sapp.glx.QueryVersion(_sapp.x11.display, &_sapp.glx.major, &_sapp.glx.minor)) {
        _SAPP_PANIC(LINUX_GLX_QUERY_VERSION_FAILED);
    }
    if (_sapp.glx.major == 1 && _sapp.glx.minor < 3) {
        _SAPP_PANIC(LINUX_GLX_VERSION_TOO_LOW);
    }
    const char* exts = _sapp.glx.QueryExtensionsString(_sapp.x11.display, _sapp.x11.screen);
    if (_sapp_glx_extsupported("GLX_EXT_swap_control", exts)) {
        _sapp.glx.SwapIntervalEXT = (PFNGLXSWAPINTERVALEXTPROC) _sapp_glx_getprocaddr("glXSwapIntervalEXT");
        _sapp.glx.EXT_swap_control = 0 != _sapp.glx.SwapIntervalEXT;
    }
    if (_sapp_glx_extsupported("GLX_MESA_swap_control", exts)) {
        _sapp.glx.SwapIntervalMESA = (PFNGLXSWAPINTERVALMESAPROC) _sapp_glx_getprocaddr("glXSwapIntervalMESA");
        _sapp.glx.MESA_swap_control = 0 != _sapp.glx.SwapIntervalMESA;
    }
    _sapp.glx.ARB_multisample = _sapp_glx_extsupported("GLX_ARB_multisample", exts);
    if (_sapp_glx_extsupported("GLX_ARB_create_context", exts)) {
        _sapp.glx.CreateContextAttribsARB = (PFNGLXCREATECONTEXTATTRIBSARBPROC) _sapp_glx_getprocaddr("glXCreateContextAttribsARB");
        _sapp.glx.ARB_create_context = 0 != _sapp.glx.CreateContextAttribsARB;
    }
    _sapp.glx.ARB_create_context_profile = _sapp_glx_extsupported("GLX_ARB_create_context_profile", exts);
}

_SOKOL_PRIVATE int _sapp_glx_attrib(GLXFBConfig fbconfig, int attrib) {
    int value;
    _sapp.glx.GetFBConfigAttrib(_sapp.x11.display, fbconfig, attrib, &value);
    return value;
}

_SOKOL_PRIVATE GLXFBConfig _sapp_glx_choosefbconfig(void) {
    GLXFBConfig* native_configs;
    _sapp_gl_fbconfig* usable_configs;
    const _sapp_gl_fbconfig* closest;
    int i, native_count, usable_count;
    const char* vendor;
    bool trust_window_bit = true;

    /* HACK: This is a (hopefully temporary) workaround for Chromium
           (VirtualBox GL) not setting the window bit on any GLXFBConfigs
    */
    vendor = _sapp.glx.GetClientString(_sapp.x11.display, GLX_VENDOR);
    if (vendor && strcmp(vendor, "Chromium") == 0) {
        trust_window_bit = false;
    }

    native_configs = _sapp.glx.GetFBConfigs(_sapp.x11.display, _sapp.x11.screen, &native_count);
    if (!native_configs || !native_count) {
        _SAPP_PANIC(LINUX_GLX_NO_GLXFBCONFIGS);
    }

    usable_configs = (_sapp_gl_fbconfig*) _sapp_malloc_clear((size_t)native_count * sizeof(_sapp_gl_fbconfig));
    usable_count = 0;
    for (i = 0;  i < native_count;  i++) {
        const GLXFBConfig n = native_configs[i];
        _sapp_gl_fbconfig* u = usable_configs + usable_count;
        _sapp_gl_init_fbconfig(u);

        /* Only consider RGBA GLXFBConfigs */
        if (0 == (_sapp_glx_attrib(n, GLX_RENDER_TYPE) & GLX_RGBA_BIT)) {
            continue;
        }
        /* Only consider window GLXFBConfigs */
        if (0 == (_sapp_glx_attrib(n, GLX_DRAWABLE_TYPE) & GLX_WINDOW_BIT)) {
            if (trust_window_bit) {
                continue;
            }
        }
        u->red_bits = _sapp_glx_attrib(n, GLX_RED_SIZE);
        u->green_bits = _sapp_glx_attrib(n, GLX_GREEN_SIZE);
        u->blue_bits = _sapp_glx_attrib(n, GLX_BLUE_SIZE);
        u->alpha_bits = _sapp_glx_attrib(n, GLX_ALPHA_SIZE);
        u->depth_bits = _sapp_glx_attrib(n, GLX_DEPTH_SIZE);
        u->stencil_bits = _sapp_glx_attrib(n, GLX_STENCIL_SIZE);
        if (_sapp_glx_attrib(n, GLX_DOUBLEBUFFER)) {
            u->doublebuffer = true;
        }
        if (_sapp.glx.ARB_multisample) {
            u->samples = _sapp_glx_attrib(n, GLX_SAMPLES);
        }
        u->handle = (uintptr_t) n;
        usable_count++;
    }
    _sapp_gl_fbconfig desired;
    _sapp_gl_init_fbconfig(&desired);
    desired.red_bits = 8;
    desired.green_bits = 8;
    desired.blue_bits = 8;
    desired.alpha_bits = 8;
    desired.depth_bits = 24;
    desired.stencil_bits = 8;
    desired.doublebuffer = true;
    desired.samples = _sapp.sample_count > 1 ? _sapp.sample_count : 0;
    closest = _sapp_gl_choose_fbconfig(&desired, usable_configs, usable_count);
    GLXFBConfig result = 0;
    if (closest) {
        result = (GLXFBConfig) closest->handle;
    }
    XFree(native_configs);
    _sapp_free(usable_configs);
    return result;
}

_SOKOL_PRIVATE void _sapp_glx_choose_visual(Visual** visual, int* depth) {
    GLXFBConfig native = _sapp_glx_choosefbconfig();
    if (0 == native) {
        _SAPP_PANIC(LINUX_GLX_NO_SUITABLE_GLXFBCONFIG);
    }
    XVisualInfo* result = _sapp.glx.GetVisualFromFBConfig(_sapp.x11.display, native);
    if (!result) {
        _SAPP_PANIC(LINUX_GLX_GET_VISUAL_FROM_FBCONFIG_FAILED);
    }
    *visual = result->visual;
    *depth = result->depth;
    XFree(result);
}

_SOKOL_PRIVATE void _sapp_glx_make_current(void) {
    _sapp.glx.MakeCurrent(_sapp.x11.display, _sapp.glx.window, _sapp.glx.ctx);
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, (GLint*)&_sapp.gl.framebuffer);
}

_SOKOL_PRIVATE void _sapp_glx_create_context(void) {
    GLXFBConfig native = _sapp_glx_choosefbconfig();
    if (0 == native){
        _SAPP_PANIC(LINUX_GLX_NO_SUITABLE_GLXFBCONFIG);
    }
    if (!(_sapp.glx.ARB_create_context && _sapp.glx.ARB_create_context_profile)) {
        _SAPP_PANIC(LINUX_GLX_REQUIRED_EXTENSIONS_MISSING);
    }
    _sapp_x11_grab_error_handler();
    const int attribs[] = {
        GLX_CONTEXT_MAJOR_VERSION_ARB, _sapp.desc.gl_major_version,
        GLX_CONTEXT_MINOR_VERSION_ARB, _sapp.desc.gl_minor_version,
        GLX_CONTEXT_PROFILE_MASK_ARB, GLX_CONTEXT_CORE_PROFILE_BIT_ARB,
        GLX_CONTEXT_FLAGS_ARB, GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB,
        0, 0
    };
    _sapp.glx.ctx = _sapp.glx.CreateContextAttribsARB(_sapp.x11.display, native, NULL, True, attribs);
    if (!_sapp.glx.ctx) {
        _SAPP_PANIC(LINUX_GLX_CREATE_CONTEXT_FAILED);
    }
    _sapp_x11_release_error_handler();
    _sapp.glx.window = _sapp.glx.CreateWindow(_sapp.x11.display, native, _sapp.x11.window, NULL);
    if (!_sapp.glx.window) {
        _SAPP_PANIC(LINUX_GLX_CREATE_WINDOW_FAILED);
    }
    _sapp_glx_make_current();
}

_SOKOL_PRIVATE void _sapp_glx_destroy_context(void) {
    if (_sapp.glx.window) {
        _sapp.glx.DestroyWindow(_sapp.x11.display, _sapp.glx.window);
        _sapp.glx.window = 0;
    }
    if (_sapp.glx.ctx) {
        _sapp.glx.DestroyContext(_sapp.x11.display, _sapp.glx.ctx);
        _sapp.glx.ctx = 0;
    }
}

_SOKOL_PRIVATE void _sapp_glx_swap_buffers(void) {
    _sapp.glx.SwapBuffers(_sapp.x11.display, _sapp.glx.window);
}

_SOKOL_PRIVATE void _sapp_glx_swapinterval(int interval) {
    if (_sapp.glx.EXT_swap_control) {
        _sapp.glx.SwapIntervalEXT(_sapp.x11.display, _sapp.glx.window, interval);
    }
    else if (_sapp.glx.MESA_swap_control) {
        _sapp.glx.SwapIntervalMESA(interval);
    }
}

#endif /* _SAPP_GLX */

_SOKOL_PRIVATE void _sapp_x11_send_event(Atom type, int a, int b, int c, int d, int e) {
    XEvent event;
    _sapp_clear(&event, sizeof(event));

    event.type = ClientMessage;
    event.xclient.window = _sapp.x11.window;
    event.xclient.format = 32;
    event.xclient.message_type = type;
    event.xclient.data.l[0] = a;
    event.xclient.data.l[1] = b;
    event.xclient.data.l[2] = c;
    event.xclient.data.l[3] = d;
    event.xclient.data.l[4] = e;

    XSendEvent(_sapp.x11.display, _sapp.x11.root,
               False,
               SubstructureNotifyMask | SubstructureRedirectMask,
               &event);
}

_SOKOL_PRIVATE void _sapp_x11_query_window_size(void) {
    XWindowAttributes attribs;
    XGetWindowAttributes(_sapp.x11.display, _sapp.x11.window, &attribs);
    _sapp.window_width = attribs.width;
    _sapp.window_height = attribs.height;
    _sapp.framebuffer_width = _sapp.window_width;
    _sapp.framebuffer_height = _sapp.window_height;
}

_SOKOL_PRIVATE void _sapp_x11_set_fullscreen(bool enable) {
    /* NOTE: this function must be called after XMapWindow (which happens in _sapp_x11_show_window()) */
    if (_sapp.x11.NET_WM_STATE && _sapp.x11.NET_WM_STATE_FULLSCREEN) {
        if (enable) {
            const int _NET_WM_STATE_ADD = 1;
            _sapp_x11_send_event(_sapp.x11.NET_WM_STATE,
                                _NET_WM_STATE_ADD,
                                _sapp.x11.NET_WM_STATE_FULLSCREEN,
                                0, 1, 0);
        }
        else {
            const int _NET_WM_STATE_REMOVE = 0;
            _sapp_x11_send_event(_sapp.x11.NET_WM_STATE,
                                _NET_WM_STATE_REMOVE,
                                _sapp.x11.NET_WM_STATE_FULLSCREEN,
                                0, 1, 0);
        }
    }
    XFlush(_sapp.x11.display);
}

_SOKOL_PRIVATE void _sapp_x11_create_hidden_cursor(void) {
    SOKOL_ASSERT(0 == _sapp.x11.hidden_cursor);
    const int w = 16;
    const int h = 16;
    XcursorImage* img = XcursorImageCreate(w, h);
    SOKOL_ASSERT(img && (img->width == 16) && (img->height == 16) && img->pixels);
    img->xhot = 0;
    img->yhot = 0;
    const size_t num_bytes = (size_t)(w * h) * sizeof(XcursorPixel);
    _sapp_clear(img->pixels, num_bytes);
    _sapp.x11.hidden_cursor = XcursorImageLoadCursor(_sapp.x11.display, img);
    XcursorImageDestroy(img);
}

 _SOKOL_PRIVATE void _sapp_x11_create_standard_cursor(sapp_mouse_cursor cursor, const char* name, const char* theme, int size, uint32_t fallback_native) {
    SOKOL_ASSERT((cursor >= 0) && (cursor < _SAPP_MOUSECURSOR_NUM));
    SOKOL_ASSERT(_sapp.x11.display);
    if (theme) {
        XcursorImage* img = XcursorLibraryLoadImage(name, theme, size);
        if (img) {
            _sapp.x11.cursors[cursor] = XcursorImageLoadCursor(_sapp.x11.display, img);
            XcursorImageDestroy(img);
        }
    }
    if (0 == _sapp.x11.cursors[cursor]) {
        _sapp.x11.cursors[cursor] = XCreateFontCursor(_sapp.x11.display, fallback_native);
    }
}

_SOKOL_PRIVATE void _sapp_x11_create_cursors(void) {
    SOKOL_ASSERT(_sapp.x11.display);
    const char* cursor_theme = XcursorGetTheme(_sapp.x11.display);
    const int size = XcursorGetDefaultSize(_sapp.x11.display);
    _sapp_x11_create_standard_cursor(SAPP_MOUSECURSOR_ARROW, "default", cursor_theme, size, XC_left_ptr);
    _sapp_x11_create_standard_cursor(SAPP_MOUSECURSOR_IBEAM, "text", cursor_theme, size, XC_xterm);
    _sapp_x11_create_standard_cursor(SAPP_MOUSECURSOR_CROSSHAIR, "crosshair", cursor_theme, size, XC_crosshair);
    _sapp_x11_create_standard_cursor(SAPP_MOUSECURSOR_POINTING_HAND, "pointer", cursor_theme, size, XC_hand2);
    _sapp_x11_create_standard_cursor(SAPP_MOUSECURSOR_RESIZE_EW, "ew-resize", cursor_theme, size, XC_sb_h_double_arrow);
    _sapp_x11_create_standard_cursor(SAPP_MOUSECURSOR_RESIZE_NS, "ns-resize", cursor_theme, size, XC_sb_v_double_arrow);
    _sapp_x11_create_standard_cursor(SAPP_MOUSECURSOR_RESIZE_NWSE, "nwse-resize", cursor_theme, size, 0);
    _sapp_x11_create_standard_cursor(SAPP_MOUSECURSOR_RESIZE_NESW, "nesw-resize", cursor_theme, size, 0);
    _sapp_x11_create_standard_cursor(SAPP_MOUSECURSOR_RESIZE_ALL, "all-scroll", cursor_theme, size, XC_fleur);
    _sapp_x11_create_standard_cursor(SAPP_MOUSECURSOR_NOT_ALLOWED, "no-allowed", cursor_theme, size, 0);
    _sapp_x11_create_hidden_cursor();
}

_SOKOL_PRIVATE void _sapp_x11_destroy_cursors(void) {
    SOKOL_ASSERT(_sapp.x11.display);
    if (_sapp.x11.hidden_cursor) {
        XFreeCursor(_sapp.x11.display, _sapp.x11.hidden_cursor);
        _sapp.x11.hidden_cursor = 0;
    }
    for (int i = 0; i < _SAPP_MOUSECURSOR_NUM; i++) {
        if (_sapp.x11.cursors[i]) {
            XFreeCursor(_sapp.x11.display, _sapp.x11.cursors[i]);
            _sapp.x11.cursors[i] = 0;
        }
    }
}

_SOKOL_PRIVATE void _sapp_x11_toggle_fullscreen(void) {
    _sapp.fullscreen = !_sapp.fullscreen;
    _sapp_x11_set_fullscreen(_sapp.fullscreen);
    _sapp_x11_query_window_size();
}

_SOKOL_PRIVATE void _sapp_x11_update_cursor(sapp_mouse_cursor cursor, bool shown) {
    SOKOL_ASSERT((cursor >= 0) && (cursor < _SAPP_MOUSECURSOR_NUM));
    if (shown) {
        if (_sapp.x11.cursors[cursor]) {
            XDefineCursor(_sapp.x11.display, _sapp.x11.window, _sapp.x11.cursors[cursor]);
        }
        else {
            XUndefineCursor(_sapp.x11.display, _sapp.x11.window);
        }
    }
    else {
        XDefineCursor(_sapp.x11.display, _sapp.x11.window, _sapp.x11.hidden_cursor);
    }
    XFlush(_sapp.x11.display);
}

_SOKOL_PRIVATE void _sapp_x11_lock_mouse(bool lock) {
    if (lock == _sapp.mouse.locked) {
        return;
    }
    _sapp.mouse.dx = 0.0f;
    _sapp.mouse.dy = 0.0f;
    _sapp.mouse.locked = lock;
    if (_sapp.mouse.locked) {
        if (_sapp.x11.xi.available) {
            XIEventMask em;
            unsigned char mask[XIMaskLen(XI_RawMotion)] = { 0 }; // XIMaskLen is a macro
            em.deviceid = XIAllMasterDevices;
            em.mask_len = sizeof(mask);
            em.mask = mask;
            XISetMask(mask, XI_RawMotion);
            XISelectEvents(_sapp.x11.display, _sapp.x11.root, &em, 1);
        }
        XGrabPointer(_sapp.x11.display, // display
            _sapp.x11.window,           // grab_window
            True,                       // owner_events
            ButtonPressMask | ButtonReleaseMask | PointerMotionMask,    // event_mask
            GrabModeAsync,              // pointer_mode
            GrabModeAsync,              // keyboard_mode
            _sapp.x11.window,           // confine_to
            _sapp.x11.hidden_cursor,    // cursor
            CurrentTime);               // time
    }
    else {
        if (_sapp.x11.xi.available) {
            XIEventMask em;
            unsigned char mask[] = { 0 };
            em.deviceid = XIAllMasterDevices;
            em.mask_len = sizeof(mask);
            em.mask = mask;
            XISelectEvents(_sapp.x11.display, _sapp.x11.root, &em, 1);
        }
        XWarpPointer(_sapp.x11.display, None, _sapp.x11.window, 0, 0, 0, 0, (int) _sapp.mouse.x, _sapp.mouse.y);
        XUngrabPointer(_sapp.x11.display, CurrentTime);
    }
    XFlush(_sapp.x11.display);
}

_SOKOL_PRIVATE void _sapp_x11_update_window_title(void) {
    Xutf8SetWMProperties(_sapp.x11.display,
        _sapp.x11.window,
        _sapp.window_title, _sapp.window_title,
        NULL, 0, NULL, NULL, NULL);
    XChangeProperty(_sapp.x11.display, _sapp.x11.window,
        _sapp.x11.NET_WM_NAME, _sapp.x11.UTF8_STRING, 8,
        PropModeReplace,
        (unsigned char*)_sapp.window_title,
        strlen(_sapp.window_title));
    XChangeProperty(_sapp.x11.display, _sapp.x11.window,
        _sapp.x11.NET_WM_ICON_NAME, _sapp.x11.UTF8_STRING, 8,
        PropModeReplace,
        (unsigned char*)_sapp.window_title,
        strlen(_sapp.window_title));
    XFlush(_sapp.x11.display);
}

_SOKOL_PRIVATE void _sapp_x11_set_icon(const sapp_icon_desc* icon_desc, int num_images) {
    SOKOL_ASSERT((num_images > 0) && (num_images <= SAPP_MAX_ICONIMAGES));
    int long_count = 0;
    for (int i = 0; i < num_images; i++) {
        const sapp_image_desc* img_desc = &icon_desc->images[i];
        long_count += 2 + (img_desc->width * img_desc->height);
    }
    long* icon_data = (long*) _sapp_malloc_clear((size_t)long_count * sizeof(long));
    SOKOL_ASSERT(icon_data);
    long* dst = icon_data;
    for (int img_index = 0; img_index < num_images; img_index++) {
        const sapp_image_desc* img_desc = &icon_desc->images[img_index];
        const uint8_t* src = (const uint8_t*) img_desc->pixels.ptr;
        *dst++ = img_desc->width;
        *dst++ = img_desc->height;
        const int num_pixels = img_desc->width * img_desc->height;
        for (int pixel_index = 0; pixel_index < num_pixels; pixel_index++) {
            *dst++ = ((long)(src[pixel_index * 4 + 0]) << 16) |
                     ((long)(src[pixel_index * 4 + 1]) << 8) |
                     ((long)(src[pixel_index * 4 + 2]) << 0) |
                     ((long)(src[pixel_index * 4 + 3]) << 24);
        }
    }
    XChangeProperty(_sapp.x11.display, _sapp.x11.window,
        _sapp.x11.NET_WM_ICON,
        XA_CARDINAL, 32,
        PropModeReplace,
        (unsigned char*)icon_data,
        long_count);
    _sapp_free(icon_data);
    XFlush(_sapp.x11.display);
}

_SOKOL_PRIVATE void _sapp_x11_create_window(Visual* visual, int depth) {
    _sapp.x11.colormap = XCreateColormap(_sapp.x11.display, _sapp.x11.root, visual, AllocNone);
    XSetWindowAttributes wa;
    _sapp_clear(&wa, sizeof(wa));
    const uint32_t wamask = CWBorderPixel | CWColormap | CWEventMask;
    wa.colormap = _sapp.x11.colormap;
    wa.border_pixel = 0;
    wa.event_mask = StructureNotifyMask | KeyPressMask | KeyReleaseMask |
                    PointerMotionMask | ButtonPressMask | ButtonReleaseMask |
                    ExposureMask | FocusChangeMask | VisibilityChangeMask |
                    EnterWindowMask | LeaveWindowMask | PropertyChangeMask;

    int display_width = DisplayWidth(_sapp.x11.display, _sapp.x11.screen);
    int display_height = DisplayHeight(_sapp.x11.display, _sapp.x11.screen);
    int window_width = _sapp.window_width;
    int window_height = _sapp.window_height;
    if (0 == window_width) {
        window_width = (display_width * 4) / 5;
    }
    if (0 == window_height) {
        window_height = (display_height * 4) / 5;
    }
    int window_xpos = (display_width - window_width) / 2;
    int window_ypos = (display_height - window_height) / 2;
    if (window_xpos < 0) {
        window_xpos = 0;
    }
    if (window_ypos < 0) {
        window_ypos = 0;
    }
    _sapp_x11_grab_error_handler();
    _sapp.x11.window = XCreateWindow(_sapp.x11.display,
                                     _sapp.x11.root,
                                     window_xpos,
                                     window_ypos,
                                     (uint32_t)window_width,
                                     (uint32_t)window_height,
                                     0,     /* border width */
                                     depth, /* color depth */
                                     InputOutput,
                                     visual,
                                     wamask,
                                     &wa);
    _sapp_x11_release_error_handler();
    if (!_sapp.x11.window) {
        _SAPP_PANIC(LINUX_X11_CREATE_WINDOW_FAILED);
    }
    Atom protocols[] = {
        _sapp.x11.WM_DELETE_WINDOW
    };
    XSetWMProtocols(_sapp.x11.display, _sapp.x11.window, protocols, 1);

    XSizeHints* hints = XAllocSizeHints();
    hints->flags = (PWinGravity | PPosition | PSize);
    hints->win_gravity = StaticGravity;
    hints->x = window_xpos;
    hints->y = window_ypos;
    hints->width = window_width;
    hints->height = window_height;
    XSetWMNormalHints(_sapp.x11.display, _sapp.x11.window, hints);
    XFree(hints);

    /* announce support for drag'n'drop */
    if (_sapp.drop.enabled) {
        const Atom version = _SAPP_X11_XDND_VERSION;
        XChangeProperty(_sapp.x11.display, _sapp.x11.window, _sapp.x11.xdnd.XdndAware, XA_ATOM, 32, PropModeReplace, (unsigned char*) &version, 1);
    }
    _sapp_x11_update_window_title();
    _sapp_x11_query_window_size();
}

_SOKOL_PRIVATE void _sapp_x11_destroy_window(void) {
    if (_sapp.x11.window) {
        XUnmapWindow(_sapp.x11.display, _sapp.x11.window);
        XDestroyWindow(_sapp.x11.display, _sapp.x11.window);
        _sapp.x11.window = 0;
    }
    if (_sapp.x11.colormap) {
        XFreeColormap(_sapp.x11.display, _sapp.x11.colormap);
        _sapp.x11.colormap = 0;
    }
    XFlush(_sapp.x11.display);
}

_SOKOL_PRIVATE bool _sapp_x11_window_visible(void) {
    XWindowAttributes wa;
    XGetWindowAttributes(_sapp.x11.display, _sapp.x11.window, &wa);
    return wa.map_state == IsViewable;
}

_SOKOL_PRIVATE void _sapp_x11_show_window(void) {
    if (!_sapp_x11_window_visible()) {
        XMapWindow(_sapp.x11.display, _sapp.x11.window);
        XRaiseWindow(_sapp.x11.display, _sapp.x11.window);
        XFlush(_sapp.x11.display);
    }
}

_SOKOL_PRIVATE void _sapp_x11_hide_window(void) {
    XUnmapWindow(_sapp.x11.display, _sapp.x11.window);
    XFlush(_sapp.x11.display);
}

_SOKOL_PRIVATE unsigned long _sapp_x11_get_window_property(Window window, Atom property, Atom type, unsigned char** value) {
    Atom actualType;
    int actualFormat;
    unsigned long itemCount, bytesAfter;
    XGetWindowProperty(_sapp.x11.display,
                       window,
                       property,
                       0,
                       LONG_MAX,
                       False,
                       type,
                       &actualType,
                       &actualFormat,
                       &itemCount,
                       &bytesAfter,
                       value);
    return itemCount;
}

_SOKOL_PRIVATE int _sapp_x11_get_window_state(void) {
    int result = WithdrawnState;
    struct {
        CARD32 state;
        Window icon;
    } *state = NULL;

    if (_sapp_x11_get_window_property(_sapp.x11.window, _sapp.x11.WM_STATE, _sapp.x11.WM_STATE, (unsigned char**)&state) >= 2) {
        result = (int)state->state;
    }
    if (state) {
        XFree(state);
    }
    return result;
}

_SOKOL_PRIVATE uint32_t _sapp_x11_key_modifier_bit(sapp_keycode key) {
    switch (key) {
        case SAPP_KEYCODE_LEFT_SHIFT:
        case SAPP_KEYCODE_RIGHT_SHIFT:
            return SAPP_MODIFIER_SHIFT;
        case SAPP_KEYCODE_LEFT_CONTROL:
        case SAPP_KEYCODE_RIGHT_CONTROL:
            return SAPP_MODIFIER_CTRL;
        case SAPP_KEYCODE_LEFT_ALT:
        case SAPP_KEYCODE_RIGHT_ALT:
            return SAPP_MODIFIER_ALT;
        case SAPP_KEYCODE_LEFT_SUPER:
        case SAPP_KEYCODE_RIGHT_SUPER:
            return SAPP_MODIFIER_SUPER;
        default:
            return 0;
    }
}

_SOKOL_PRIVATE uint32_t _sapp_x11_button_modifier_bit(sapp_mousebutton btn) {
    switch (btn) {
        case SAPP_MOUSEBUTTON_LEFT:     return SAPP_MODIFIER_LMB;
        case SAPP_MOUSEBUTTON_RIGHT:    return SAPP_MODIFIER_RMB;
        case SAPP_MOUSEBUTTON_MIDDLE:   return SAPP_MODIFIER_MMB;
        default: return 0;
    }
}

_SOKOL_PRIVATE uint32_t _sapp_x11_mods(uint32_t x11_mods) {
    uint32_t mods = 0;
    if (x11_mods & ShiftMask) {
        mods |= SAPP_MODIFIER_SHIFT;
    }
    if (x11_mods & ControlMask) {
        mods |= SAPP_MODIFIER_CTRL;
    }
    if (x11_mods & Mod1Mask) {
        mods |= SAPP_MODIFIER_ALT;
    }
    if (x11_mods & Mod4Mask) {
        mods |= SAPP_MODIFIER_SUPER;
    }
    if (x11_mods & Button1Mask) {
        mods |= SAPP_MODIFIER_LMB;
    }
    if (x11_mods & Button2Mask) {
        mods |= SAPP_MODIFIER_MMB;
    }
    if (x11_mods & Button3Mask) {
        mods |= SAPP_MODIFIER_RMB;
    }
    return mods;
}

_SOKOL_PRIVATE void _sapp_x11_app_event(sapp_event_type type) {
    if (_sapp_events_enabled()) {
        _sapp_init_event(type);
        _sapp_call_event(&_sapp.event);
    }
}

_SOKOL_PRIVATE sapp_mousebutton _sapp_x11_translate_button(const XEvent* event) {
    switch (event->xbutton.button) {
        case Button1: return SAPP_MOUSEBUTTON_LEFT;
        case Button2: return SAPP_MOUSEBUTTON_MIDDLE;
        case Button3: return SAPP_MOUSEBUTTON_RIGHT;
        default:      return SAPP_MOUSEBUTTON_INVALID;
    }
}

_SOKOL_PRIVATE void _sapp_x11_mouse_update(int x, int y, bool clear_dxdy) {
    if (!_sapp.mouse.locked) {
        const float new_x = (float) x;
        const float new_y = (float) y;
        if (clear_dxdy) {
            _sapp.mouse.dx = 0.0f;
            _sapp.mouse.dy = 0.0f;
        } else if (_sapp.mouse.pos_valid) {
            _sapp.mouse.dx = new_x - _sapp.mouse.x;
            _sapp.mouse.dy = new_y - _sapp.mouse.y;
        }
        _sapp.mouse.x = new_x;
        _sapp.mouse.y = new_y;
        _sapp.mouse.pos_valid = true;
    }
}

_SOKOL_PRIVATE void _sapp_x11_mouse_event(sapp_event_type type, sapp_mousebutton btn, uint32_t mods) {
    if (_sapp_events_enabled()) {
        _sapp_init_event(type);
        _sapp.event.mouse_button = btn;
        _sapp.event.modifiers = mods;
        _sapp_call_event(&_sapp.event);
    }
}

_SOKOL_PRIVATE void _sapp_x11_scroll_event(float x, float y, uint32_t mods) {
    if (_sapp_events_enabled()) {
        _sapp_init_event(SAPP_EVENTTYPE_MOUSE_SCROLL);
        _sapp.event.modifiers = mods;
        _sapp.event.scroll_x = x;
        _sapp.event.scroll_y = y;
        _sapp_call_event(&_sapp.event);
    }
}

_SOKOL_PRIVATE void _sapp_x11_key_event(sapp_event_type type, sapp_keycode key, bool repeat, uint32_t mods) {
    if (_sapp_events_enabled()) {
        _sapp_init_event(type);
        _sapp.event.key_code = key;
        _sapp.event.key_repeat = repeat;
        _sapp.event.modifiers = mods;
        _sapp_call_event(&_sapp.event);
        /* check if a CLIPBOARD_PASTED event must be sent too */
        if (_sapp.clipboard.enabled &&
            (type == SAPP_EVENTTYPE_KEY_DOWN) &&
            (_sapp.event.modifiers == SAPP_MODIFIER_CTRL) &&
            (_sapp.event.key_code == SAPP_KEYCODE_V))
        {
            _sapp_init_event(SAPP_EVENTTYPE_CLIPBOARD_PASTED);
            _sapp_call_event(&_sapp.event);
        }
    }
}

_SOKOL_PRIVATE void _sapp_x11_char_event(uint32_t chr, bool repeat, uint32_t mods) {
    if (_sapp_events_enabled()) {
        _sapp_init_event(SAPP_EVENTTYPE_CHAR);
        _sapp.event.char_code = chr;
        _sapp.event.key_repeat = repeat;
        _sapp.event.modifiers = mods;
        _sapp_call_event(&_sapp.event);
    }
}

_SOKOL_PRIVATE sapp_keycode _sapp_x11_translate_key(int scancode) {
    if ((scancode >= 0) && (scancode < _SAPP_X11_MAX_X11_KEYCODES)) {
        return _sapp.keycodes[scancode];
    } else {
        return SAPP_KEYCODE_INVALID;
    }
}

_SOKOL_PRIVATE int32_t _sapp_x11_keysym_to_unicode(KeySym keysym) {
    int min = 0;
    int max = sizeof(_sapp_x11_keysymtab) / sizeof(struct _sapp_x11_codepair) - 1;
    int mid;

    /* First check for Latin-1 characters (1:1 mapping) */
    if ((keysym >= 0x0020 && keysym <= 0x007e) ||
        (keysym >= 0x00a0 && keysym <= 0x00ff))
    {
        return keysym;
    }

    /* Also check for directly encoded 24-bit UCS characters */
    if ((keysym & 0xff000000) == 0x01000000) {
        return keysym & 0x00ffffff;
    }

    /* Binary search in table */
    while (max >= min) {
        mid = (min + max) / 2;
        if (_sapp_x11_keysymtab[mid].keysym < keysym) {
            min = mid + 1;
        }
        else if (_sapp_x11_keysymtab[mid].keysym > keysym) {
            max = mid - 1;
        }
        else {
            return _sapp_x11_keysymtab[mid].ucs;
        }
    }

    /* No matching Unicode value found */
    return -1;
}

_SOKOL_PRIVATE bool _sapp_x11_keypress_repeat(int keycode) {
    bool repeat = false;
    if ((keycode >= 0) && (keycode < _SAPP_X11_MAX_X11_KEYCODES)) {
        repeat = _sapp.x11.key_repeat[keycode];
        _sapp.x11.key_repeat[keycode] = true;
    }
    return repeat;
}

_SOKOL_PRIVATE void _sapp_x11_keyrelease_repeat(int keycode) {
    if ((keycode >= 0) && (keycode < _SAPP_X11_MAX_X11_KEYCODES)) {
        _sapp.x11.key_repeat[keycode] = false;
    }
}

_SOKOL_PRIVATE bool _sapp_x11_parse_dropped_files_list(const char* src) {
    SOKOL_ASSERT(src);
    SOKOL_ASSERT(_sapp.drop.buffer);

    _sapp_clear_drop_buffer();
    _sapp.drop.num_files = 0;

    /*
        src is (potentially percent-encoded) string made of one or multiple paths
        separated by \r\n, each path starting with 'file://'
    */
    bool err = false;
    int src_count = 0;
    char src_chr = 0;
    char* dst_ptr = _sapp.drop.buffer;
    const char* dst_end_ptr = dst_ptr + (_sapp.drop.max_path_length - 1); // room for terminating 0
    while (0 != (src_chr = *src++)) {
        src_count++;
        char dst_chr = 0;
        /* check leading 'file://' */
        if (src_count <= 7) {
            if (((src_count == 1) && (src_chr != 'f')) ||
                ((src_count == 2) && (src_chr != 'i')) ||
                ((src_count == 3) && (src_chr != 'l')) ||
                ((src_count == 4) && (src_chr != 'e')) ||
                ((src_count == 5) && (src_chr != ':')) ||
                ((src_count == 6) && (src_chr != '/')) ||
                ((src_count == 7) && (src_chr != '/')))
            {
                _SAPP_ERROR(LINUX_X11_DROPPED_FILE_URI_WRONG_SCHEME);
                err = true;
                break;
            }
        }
        else if (src_chr == '\r') {
            // skip
        }
        else if (src_chr == '\n') {
            src_count = 0;
            _sapp.drop.num_files++;
            // too many files is not an error
            if (_sapp.drop.num_files >= _sapp.drop.max_files) {
                break;
            }
            dst_ptr = _sapp.drop.buffer + _sapp.drop.num_files * _sapp.drop.max_path_length;
            dst_end_ptr = dst_ptr + (_sapp.drop.max_path_length - 1);
        }
        else if ((src_chr == '%') && src[0] && src[1]) {
            // a percent-encoded byte (most likely UTF-8 multibyte sequence)
            const char digits[3] = { src[0], src[1], 0 };
            src += 2;
            dst_chr = (char) strtol(digits, 0, 16);
        }
        else {
            dst_chr = src_chr;
        }
        if (dst_chr) {
            // dst_end_ptr already has adjustment for terminating zero
            if (dst_ptr < dst_end_ptr) {
                *dst_ptr++ = dst_chr;
            }
            else {
                _SAPP_ERROR(DROPPED_FILE_PATH_TOO_LONG);
                err = true;
                break;
            }
        }
    }
    if (err) {
        _sapp_clear_drop_buffer();
        _sapp.drop.num_files = 0;
        return false;
    }
    else {
        return true;
    }
}

_SOKOL_PRIVATE void _sapp_x11_on_genericevent(XEvent* event) {
    if (_sapp.mouse.locked && _sapp.x11.xi.available) {
        if (event->xcookie.extension == _sapp.x11.xi.major_opcode) {
            if (XGetEventData(_sapp.x11.display, &event->xcookie)) {
                if (event->xcookie.evtype == XI_RawMotion) {
                    XIRawEvent* re = (XIRawEvent*) event->xcookie.data;
                    if (re->valuators.mask_len) {
                        const double* values = re->raw_values;
                        if (XIMaskIsSet(re->valuators.mask, 0)) {
                            _sapp.mouse.dx = (float) *values;
                            values++;
                        }
                        if (XIMaskIsSet(re->valuators.mask, 1)) {
                            _sapp.mouse.dy = (float) *values;
                        }
                        _sapp_x11_mouse_event(SAPP_EVENTTYPE_MOUSE_MOVE, SAPP_MOUSEBUTTON_INVALID, _sapp_x11_mods(event->xmotion.state));
                    }
                }
                XFreeEventData(_sapp.x11.display, &event->xcookie);
            }
        }
    }
}

_SOKOL_PRIVATE void _sapp_x11_on_focusin(XEvent* event) {
    // NOTE: ignoring NotifyGrab and NotifyUngrab is same behaviour as GLFW
    if ((event->xfocus.mode != NotifyGrab) && (event->xfocus.mode != NotifyUngrab)) {
        _sapp_x11_app_event(SAPP_EVENTTYPE_FOCUSED);
    }
}

_SOKOL_PRIVATE void _sapp_x11_on_focusout(XEvent* event) {
    // if focus is lost for any reason, and we're in mouse locked mode, disable mouse lock
    if (_sapp.mouse.locked) {
        _sapp_x11_lock_mouse(false);
    }
    // NOTE: ignoring NotifyGrab and NotifyUngrab is same behaviour as GLFW
    if ((event->xfocus.mode != NotifyGrab) && (event->xfocus.mode != NotifyUngrab)) {
        _sapp_x11_app_event(SAPP_EVENTTYPE_UNFOCUSED);
    }
}

_SOKOL_PRIVATE void _sapp_x11_on_keypress(XEvent* event) {
    int keycode = (int)event->xkey.keycode;

    const sapp_keycode key = _sapp_x11_translate_key(keycode);
    const bool repeat = _sapp_x11_keypress_repeat(keycode);
    uint32_t mods = _sapp_x11_mods(event->xkey.state);
    // X11 doesn't set modifier bit on key down, so emulate that
    mods |= _sapp_x11_key_modifier_bit(key);
    if (key != SAPP_KEYCODE_INVALID) {
        _sapp_x11_key_event(SAPP_EVENTTYPE_KEY_DOWN, key, repeat, mods);
    }
    KeySym keysym;
    XLookupString(&event->xkey, NULL, 0, &keysym, NULL);
    int32_t chr = _sapp_x11_keysym_to_unicode(keysym);
    if (chr > 0) {
        _sapp_x11_char_event((uint32_t)chr, repeat, mods);
    }
}

_SOKOL_PRIVATE void _sapp_x11_on_keyrelease(XEvent* event) {
    int keycode = (int)event->xkey.keycode;
    const sapp_keycode key = _sapp_x11_translate_key(keycode);
    _sapp_x11_keyrelease_repeat(keycode);
    if (key != SAPP_KEYCODE_INVALID) {
        uint32_t mods = _sapp_x11_mods(event->xkey.state);
        // X11 doesn't clear modifier bit on key up, so emulate that
        mods &= ~_sapp_x11_key_modifier_bit(key);
        _sapp_x11_key_event(SAPP_EVENTTYPE_KEY_UP, key, false, mods);
    }
}

_SOKOL_PRIVATE void _sapp_x11_on_buttonpress(XEvent* event) {
    _sapp_x11_mouse_update(event->xbutton.x, event->xbutton.y, false);
    const sapp_mousebutton btn = _sapp_x11_translate_button(event);
    uint32_t mods = _sapp_x11_mods(event->xbutton.state);
    // X11 doesn't set modifier bit on button down, so emulate that
    mods |= _sapp_x11_button_modifier_bit(btn);
    if (btn != SAPP_MOUSEBUTTON_INVALID) {
        _sapp_x11_mouse_event(SAPP_EVENTTYPE_MOUSE_DOWN, btn, mods);
        _sapp.x11.mouse_buttons |= (1 << btn);
    }
    else {
        // might be a scroll event
        switch (event->xbutton.button) {
            case 4: _sapp_x11_scroll_event(0.0f, 1.0f, mods); break;
            case 5: _sapp_x11_scroll_event(0.0f, -1.0f, mods); break;
            case 6: _sapp_x11_scroll_event(1.0f, 0.0f, mods); break;
            case 7: _sapp_x11_scroll_event(-1.0f, 0.0f, mods); break;
        }
    }
}

_SOKOL_PRIVATE void _sapp_x11_on_buttonrelease(XEvent* event) {
    _sapp_x11_mouse_update(event->xbutton.x, event->xbutton.y, false);
    const sapp_mousebutton btn = _sapp_x11_translate_button(event);
    if (btn != SAPP_MOUSEBUTTON_INVALID) {
        uint32_t mods = _sapp_x11_mods(event->xbutton.state);
        // X11 doesn't clear modifier bit on button up, so emulate that
        mods &= ~_sapp_x11_button_modifier_bit(btn);
        _sapp_x11_mouse_event(SAPP_EVENTTYPE_MOUSE_UP, btn, mods);
        _sapp.x11.mouse_buttons &= ~(1 << btn);
    }
}

_SOKOL_PRIVATE void _sapp_x11_on_enternotify(XEvent* event) {
    // don't send enter/leave events while mouse button held down
    if (0 == _sapp.x11.mouse_buttons) {
        _sapp_x11_mouse_update(event->xcrossing.x, event->xcrossing.y, true);
        _sapp_x11_mouse_event(SAPP_EVENTTYPE_MOUSE_ENTER, SAPP_MOUSEBUTTON_INVALID, _sapp_x11_mods(event->xcrossing.state));
    }
}

_SOKOL_PRIVATE void _sapp_x11_on_leavenotify(XEvent* event) {
    if (0 == _sapp.x11.mouse_buttons) {
        _sapp_x11_mouse_update(event->xcrossing.x, event->xcrossing.y, true);
        _sapp_x11_mouse_event(SAPP_EVENTTYPE_MOUSE_LEAVE, SAPP_MOUSEBUTTON_INVALID, _sapp_x11_mods(event->xcrossing.state));
    }
}

_SOKOL_PRIVATE void _sapp_x11_on_motionnotify(XEvent* event) {
    if (!_sapp.mouse.locked) {
        _sapp_x11_mouse_update(event->xmotion.x, event->xmotion.y, false);
        _sapp_x11_mouse_event(SAPP_EVENTTYPE_MOUSE_MOVE, SAPP_MOUSEBUTTON_INVALID, _sapp_x11_mods(event->xmotion.state));
    }
}

_SOKOL_PRIVATE void _sapp_x11_on_configurenotify(XEvent* event) {
    if ((event->xconfigure.width != _sapp.window_width) || (event->xconfigure.height != _sapp.window_height)) {
        _sapp.window_width = event->xconfigure.width;
        _sapp.window_height = event->xconfigure.height;
        _sapp.framebuffer_width = _sapp.window_width;
        _sapp.framebuffer_height = _sapp.window_height;
        _sapp_x11_app_event(SAPP_EVENTTYPE_RESIZED);
    }
}

_SOKOL_PRIVATE void _sapp_x11_on_propertynotify(XEvent* event) {
    if (event->xproperty.state == PropertyNewValue) {
        if (event->xproperty.atom == _sapp.x11.WM_STATE) {
            const int state = _sapp_x11_get_window_state();
            if (state != _sapp.x11.window_state) {
                _sapp.x11.window_state = state;
                if (state == IconicState) {
                    _sapp_x11_app_event(SAPP_EVENTTYPE_ICONIFIED);
                }
                else if (state == NormalState) {
                    _sapp_x11_app_event(SAPP_EVENTTYPE_RESTORED);
                }
            }
        }
    }
}

_SOKOL_PRIVATE void _sapp_x11_on_selectionnotify(XEvent* event) {
    if (event->xselection.property == _sapp.x11.xdnd.XdndSelection) {
        char* data = 0;
        uint32_t result = _sapp_x11_get_window_property(event->xselection.requestor,
                                                        event->xselection.property,
                                                        event->xselection.target,
                                                        (unsigned char**) &data);
        if (_sapp.drop.enabled && result) {
            if (_sapp_x11_parse_dropped_files_list(data)) {
                _sapp.mouse.dx = 0.0f;
                _sapp.mouse.dy = 0.0f;
                if (_sapp_events_enabled()) {
                    // FIXME: Figure out how to get modifier key state here.
                    // The XSelection event has no 'state' item, and
                    // XQueryKeymap() always returns a zeroed array.
                    _sapp_init_event(SAPP_EVENTTYPE_FILES_DROPPED);
                    _sapp_call_event(&_sapp.event);
                }
            }
        }
        if (_sapp.x11.xdnd.version >= 2) {
            XEvent reply;
            _sapp_clear(&reply, sizeof(reply));
            reply.type = ClientMessage;
            reply.xclient.window = _sapp.x11.xdnd.source;
            reply.xclient.message_type = _sapp.x11.xdnd.XdndFinished;
            reply.xclient.format = 32;
            reply.xclient.data.l[0] = (long)_sapp.x11.window;
            reply.xclient.data.l[1] = result;
            reply.xclient.data.l[2] = (long)_sapp.x11.xdnd.XdndActionCopy;
            XSendEvent(_sapp.x11.display, _sapp.x11.xdnd.source, False, NoEventMask, &reply);
            XFlush(_sapp.x11.display);
        }
    }
}

_SOKOL_PRIVATE void _sapp_x11_on_clientmessage(XEvent* event) {
    if (XFilterEvent(event, None)) {
        return;
    }
    if (event->xclient.message_type == _sapp.x11.WM_PROTOCOLS) {
        const Atom protocol = (Atom)event->xclient.data.l[0];
        if (protocol == _sapp.x11.WM_DELETE_WINDOW) {
            _sapp.quit_requested = true;
        }
    } else if (event->xclient.message_type == _sapp.x11.xdnd.XdndEnter) {
        const bool is_list = 0 != (event->xclient.data.l[1] & 1);
        _sapp.x11.xdnd.source  = (Window)event->xclient.data.l[0];
        _sapp.x11.xdnd.version = event->xclient.data.l[1] >> 24;
        _sapp.x11.xdnd.format  = None;
        if (_sapp.x11.xdnd.version > _SAPP_X11_XDND_VERSION) {
            return;
        }
        uint32_t count = 0;
        Atom* formats = 0;
        if (is_list) {
            count = _sapp_x11_get_window_property(_sapp.x11.xdnd.source, _sapp.x11.xdnd.XdndTypeList, XA_ATOM, (unsigned char**)&formats);
        } else {
            count = 3;
            formats = (Atom*) event->xclient.data.l + 2;
        }
        for (uint32_t i = 0; i < count; i++) {
            if (formats[i] == _sapp.x11.xdnd.text_uri_list) {
                _sapp.x11.xdnd.format = _sapp.x11.xdnd.text_uri_list;
                break;
            }
        }
        if (is_list && formats) {
            XFree(formats);
        }
    } else if (event->xclient.message_type == _sapp.x11.xdnd.XdndDrop) {
        if (_sapp.x11.xdnd.version > _SAPP_X11_XDND_VERSION) {
            return;
        }
        Time time = CurrentTime;
        if (_sapp.x11.xdnd.format) {
            if (_sapp.x11.xdnd.version >= 1) {
                time = (Time)event->xclient.data.l[2];
            }
            XConvertSelection(_sapp.x11.display,
                                _sapp.x11.xdnd.XdndSelection,
                                _sapp.x11.xdnd.format,
                                _sapp.x11.xdnd.XdndSelection,
                                _sapp.x11.window,
                                time);
        } else if (_sapp.x11.xdnd.version >= 2) {
            XEvent reply;
            _sapp_clear(&reply, sizeof(reply));
            reply.type = ClientMessage;
            reply.xclient.window = _sapp.x11.xdnd.source;
            reply.xclient.message_type = _sapp.x11.xdnd.XdndFinished;
            reply.xclient.format = 32;
            reply.xclient.data.l[0] = (long)_sapp.x11.window;
            reply.xclient.data.l[1] = 0;    // drag was rejected
            reply.xclient.data.l[2] = None;
            XSendEvent(_sapp.x11.display, _sapp.x11.xdnd.source, False, NoEventMask, &reply);
            XFlush(_sapp.x11.display);
        }
    } else if (event->xclient.message_type == _sapp.x11.xdnd.XdndPosition) {
        // drag operation has moved over the window
        //  FIXME: we could track the mouse position here, but
        //  this isn't implemented on other platforms either so far
        if (_sapp.x11.xdnd.version > _SAPP_X11_XDND_VERSION) {
            return;
        }
        XEvent reply;
        _sapp_clear(&reply, sizeof(reply));
        reply.type = ClientMessage;
        reply.xclient.window = _sapp.x11.xdnd.source;
        reply.xclient.message_type = _sapp.x11.xdnd.XdndStatus;
        reply.xclient.format = 32;
        reply.xclient.data.l[0] = (long)_sapp.x11.window;
        if (_sapp.x11.xdnd.format) {
            /* reply that we are ready to copy the dragged data */
            reply.xclient.data.l[1] = 1;    // accept with no rectangle
            if (_sapp.x11.xdnd.version >= 2) {
                reply.xclient.data.l[4] = (long)_sapp.x11.xdnd.XdndActionCopy;
            }
        }
        XSendEvent(_sapp.x11.display, _sapp.x11.xdnd.source, False, NoEventMask, &reply);
        XFlush(_sapp.x11.display);
    }
}

_SOKOL_PRIVATE void _sapp_x11_process_event(XEvent* event) {
    switch (event->type) {
        case GenericEvent:
            _sapp_x11_on_genericevent(event);
            break;
        case FocusIn:
            _sapp_x11_on_focusin(event);
            break;
        case FocusOut:
            _sapp_x11_on_focusout(event);
            break;
        case KeyPress:
            _sapp_x11_on_keypress(event);
            break;
        case KeyRelease:
            _sapp_x11_on_keyrelease(event);
            break;
        case ButtonPress:
            _sapp_x11_on_buttonpress(event);
            break;
        case ButtonRelease:
            _sapp_x11_on_buttonrelease(event);
            break;
        case EnterNotify:
            _sapp_x11_on_enternotify(event);
            break;
        case LeaveNotify:
            _sapp_x11_on_leavenotify(event);
            break;
        case MotionNotify:
            _sapp_x11_on_motionnotify(event);
            break;
        case ConfigureNotify:
            _sapp_x11_on_configurenotify(event);
            break;
        case PropertyNotify:
            _sapp_x11_on_propertynotify(event);
            break;
        case SelectionNotify:
            _sapp_x11_on_selectionnotify(event);
            break;
        case DestroyNotify:
            // not a bug
            break;
        case ClientMessage:
            _sapp_x11_on_clientmessage(event);
            break;
    }
}

#if !defined(_SAPP_GLX)

_SOKOL_PRIVATE void _sapp_egl_init(void) {
#if defined(SOKOL_GLCORE)
    if (!eglBindAPI(EGL_OPENGL_API)) {
        _SAPP_PANIC(LINUX_EGL_BIND_OPENGL_API_FAILED);
    }
#else
    if (!eglBindAPI(EGL_OPENGL_ES_API)) {
        _SAPP_PANIC(LINUX_EGL_BIND_OPENGL_ES_API_FAILED);
    }
#endif

    _sapp.egl.display = eglGetDisplay((EGLNativeDisplayType)_sapp.x11.display);
    if (EGL_NO_DISPLAY == _sapp.egl.display) {
        _SAPP_PANIC(LINUX_EGL_GET_DISPLAY_FAILED);
    }

    EGLint major, minor;
    if (!eglInitialize(_sapp.egl.display, &major, &minor)) {
        _SAPP_PANIC(LINUX_EGL_INITIALIZE_FAILED);
    }

    EGLint sample_count = _sapp.desc.sample_count > 1 ? _sapp.desc.sample_count : 0;
    EGLint alpha_size = _sapp.desc.alpha ? 8 : 0;
    const EGLint config_attrs[] = {
        EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
        #if defined(SOKOL_GLCORE)
            EGL_RENDERABLE_TYPE, EGL_OPENGL_BIT,
        #elif defined(SOKOL_GLES3)
            EGL_RENDERABLE_TYPE, EGL_OPENGL_ES3_BIT,
        #endif
        EGL_RED_SIZE, 8,
        EGL_GREEN_SIZE, 8,
        EGL_BLUE_SIZE, 8,
        EGL_ALPHA_SIZE, alpha_size,
        EGL_DEPTH_SIZE, 24,
        EGL_STENCIL_SIZE, 8,
        EGL_SAMPLE_BUFFERS, _sapp.desc.sample_count > 1 ? 1 : 0,
        EGL_SAMPLES, sample_count,
        EGL_NONE,
    };

    EGLConfig egl_configs[32];
    EGLint config_count;
    if (!eglChooseConfig(_sapp.egl.display, config_attrs, egl_configs, 32, &config_count) || config_count == 0) {
        _SAPP_PANIC(LINUX_EGL_NO_CONFIGS);
    }

    EGLConfig config = egl_configs[0];
    for (int i = 0; i < config_count; ++i) {
        EGLConfig c = egl_configs[i];
        EGLint r, g, b, a, d, s, n;
        if (eglGetConfigAttrib(_sapp.egl.display, c, EGL_RED_SIZE, &r) &&
            eglGetConfigAttrib(_sapp.egl.display, c, EGL_GREEN_SIZE, &g) &&
            eglGetConfigAttrib(_sapp.egl.display, c, EGL_BLUE_SIZE, &b) &&
            eglGetConfigAttrib(_sapp.egl.display, c, EGL_ALPHA_SIZE, &a) &&
            eglGetConfigAttrib(_sapp.egl.display, c, EGL_DEPTH_SIZE, &d) &&
            eglGetConfigAttrib(_sapp.egl.display, c, EGL_STENCIL_SIZE, &s) &&
            eglGetConfigAttrib(_sapp.egl.display, c, EGL_SAMPLES, &n) &&
            (r == 8) && (g == 8) && (b == 8) && (a == alpha_size) && (d == 24) && (s == 8) && (n == sample_count)) {
            config = c;
            break;
        }
    }

    EGLint visual_id;
    if (!eglGetConfigAttrib(_sapp.egl.display, config, EGL_NATIVE_VISUAL_ID, &visual_id)) {
        _SAPP_PANIC(LINUX_EGL_NO_NATIVE_VISUAL);
    }

    XVisualInfo visual_info_template;
    _sapp_clear(&visual_info_template, sizeof(visual_info_template));
    visual_info_template.visualid = (VisualID)visual_id;

    int num_visuals;
    XVisualInfo* visual_info = XGetVisualInfo(_sapp.x11.display, VisualIDMask, &visual_info_template, &num_visuals);
    if (!visual_info) {
        _SAPP_PANIC(LINUX_EGL_GET_VISUAL_INFO_FAILED);
    }

    _sapp_x11_create_window(visual_info->visual, visual_info->depth);
    XFree(visual_info);

    _sapp.egl.surface = eglCreateWindowSurface(_sapp.egl.display, config, (EGLNativeWindowType)_sapp.x11.window, NULL);
    if (EGL_NO_SURFACE == _sapp.egl.surface) {
        _SAPP_PANIC(LINUX_EGL_CREATE_WINDOW_SURFACE_FAILED);
    }

    EGLint ctx_attrs[] = {
        #if defined(SOKOL_GLCORE)
            EGL_CONTEXT_MAJOR_VERSION, _sapp.desc.gl_major_version,
            EGL_CONTEXT_MINOR_VERSION, _sapp.desc.gl_minor_version,
            EGL_CONTEXT_OPENGL_PROFILE_MASK, EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT,
        #elif defined(SOKOL_GLES3)
            EGL_CONTEXT_CLIENT_VERSION, 3,
        #endif
        EGL_NONE,
    };

    _sapp.egl.context = eglCreateContext(_sapp.egl.display, config, EGL_NO_CONTEXT, ctx_attrs);
    if (EGL_NO_CONTEXT == _sapp.egl.context) {
        _SAPP_PANIC(LINUX_EGL_CREATE_CONTEXT_FAILED);
    }

    if (!eglMakeCurrent(_sapp.egl.display, _sapp.egl.surface, _sapp.egl.surface, _sapp.egl.context)) {
        _SAPP_PANIC(LINUX_EGL_MAKE_CURRENT_FAILED);
    }
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, (GLint*)&_sapp.gl.framebuffer);

    eglSwapInterval(_sapp.egl.display, _sapp.swap_interval);
}

_SOKOL_PRIVATE void _sapp_egl_destroy(void) {
    if (_sapp.egl.display != EGL_NO_DISPLAY) {
        eglMakeCurrent(_sapp.egl.display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);

        if (_sapp.egl.context != EGL_NO_CONTEXT) {
            eglDestroyContext(_sapp.egl.display, _sapp.egl.context);
            _sapp.egl.context = EGL_NO_CONTEXT;
        }

        if (_sapp.egl.surface != EGL_NO_SURFACE) {
            eglDestroySurface(_sapp.egl.display, _sapp.egl.surface);
            _sapp.egl.surface = EGL_NO_SURFACE;
        }

        eglTerminate(_sapp.egl.display);
        _sapp.egl.display = EGL_NO_DISPLAY;
    }
}

#endif /* _SAPP_GLX */

_SOKOL_PRIVATE void _sapp_linux_run(const sapp_desc* desc) {
    /* The following lines are here to trigger a linker error instead of an
        obscure runtime error if the user has forgotten to add -pthread to
        the compiler or linker options. They have no other purpose.
    */
    pthread_attr_t pthread_attr;
    pthread_attr_init(&pthread_attr);
    pthread_attr_destroy(&pthread_attr);

    _sapp_init_state(desc);
    _sapp.x11.window_state = NormalState;

    XInitThreads();
    XrmInitialize();
    _sapp.x11.display = XOpenDisplay(NULL);
    if (!_sapp.x11.display) {
        _SAPP_PANIC(LINUX_X11_OPEN_DISPLAY_FAILED);
    }
    _sapp.x11.screen = DefaultScreen(_sapp.x11.display);
    _sapp.x11.root = DefaultRootWindow(_sapp.x11.display);
    _sapp_x11_query_system_dpi();
    _sapp.dpi_scale = _sapp.x11.dpi / 96.0f;
    _sapp_x11_init_extensions();
    _sapp_x11_create_cursors();
    XkbSetDetectableAutoRepeat(_sapp.x11.display, true, NULL);
    _sapp_x11_init_keytable();
#if defined(_SAPP_GLX)
    _sapp_glx_init();
    Visual* visual = 0;
    int depth = 0;
    _sapp_glx_choose_visual(&visual, &depth);
    _sapp_x11_create_window(visual, depth);
    _sapp_glx_create_context();
    _sapp_glx_swapinterval(_sapp.swap_interval);
#else
    _sapp_egl_init();
#endif
    sapp_set_icon(&desc->icon);
    _sapp.valid = true;
    _sapp_x11_show_window();
    if (_sapp.fullscreen) {
        _sapp_x11_set_fullscreen(true);
    }

    XFlush(_sapp.x11.display);
    while (!_sapp.quit_ordered) {
        _sapp_timing_measure(&_sapp.timing);
        int count = XPending(_sapp.x11.display);
        while (count--) {
            XEvent event;
            XNextEvent(_sapp.x11.display, &event);
            _sapp_x11_process_event(&event);
        }
        _sapp_frame();
#if defined(_SAPP_GLX)
        _sapp_glx_swap_buffers();
#else
        eglSwapBuffers(_sapp.egl.display, _sapp.egl.surface);
#endif
        XFlush(_sapp.x11.display);
        /* handle quit-requested, either from window or from sapp_request_quit() */
        if (_sapp.quit_requested && !_sapp.quit_ordered) {
            /* give user code a chance to intervene */
            _sapp_x11_app_event(SAPP_EVENTTYPE_QUIT_REQUESTED);
            /* if user code hasn't intervened, quit the app */
            if (_sapp.quit_requested) {
                _sapp.quit_ordered = true;
            }
        }
    }
    _sapp_call_cleanup();
#if defined(_SAPP_GLX)
    _sapp_glx_destroy_context();
#else
    _sapp_egl_destroy();
#endif
    _sapp_x11_destroy_window();
    _sapp_x11_destroy_cursors();
    XCloseDisplay(_sapp.x11.display);
    _sapp_discard_state();
}

#if !defined(SOKOL_NO_ENTRY)
int main(int argc, char* argv[]) {
    sapp_desc desc = sokol_main(argc, argv);
    _sapp_linux_run(&desc);
    return 0;
}
#endif /* SOKOL_NO_ENTRY */
#endif /* _SAPP_LINUX */

// ██████  ██    ██ ██████  ██      ██  ██████
// ██   ██ ██    ██ ██   ██ ██      ██ ██
// ██████  ██    ██ ██████  ██      ██ ██
// ██      ██    ██ ██   ██ ██      ██ ██
// ██       ██████  ██████  ███████ ██  ██████
//
// >>public
#if defined(SOKOL_NO_ENTRY)
SOKOL_API_IMPL void sapp_run(const sapp_desc* desc) {
    SOKOL_ASSERT(desc);
    #if defined(_SAPP_MACOS)
        _sapp_macos_run(desc);
    #elif defined(_SAPP_IOS)
        _sapp_ios_run(desc);
    #elif defined(_SAPP_EMSCRIPTEN)
        _sapp_emsc_run(desc);
    #elif defined(_SAPP_WIN32)
        _sapp_win32_run(desc);
    #elif defined(_SAPP_LINUX)
        _sapp_linux_run(desc);
    #else
    #error "sapp_run() not supported on this platform"
    #endif
}

/* this is just a stub so the linker doesn't complain */
sapp_desc sokol_main(int argc, char* argv[]) {
    _SOKOL_UNUSED(argc);
    _SOKOL_UNUSED(argv);
    sapp_desc desc;
    _sapp_clear(&desc, sizeof(desc));
    return desc;
}
#else
/* likewise, in normal mode, sapp_run() is just an empty stub */
SOKOL_API_IMPL void sapp_run(const sapp_desc* desc) {
    _SOKOL_UNUSED(desc);
}
#endif

SOKOL_API_IMPL bool sapp_isvalid(void) {
    return _sapp.valid;
}

SOKOL_API_IMPL void* sapp_userdata(void) {
    return _sapp.desc.user_data;
}

SOKOL_API_IMPL sapp_desc sapp_query_desc(void) {
    return _sapp.desc;
}

SOKOL_API_IMPL uint64_t sapp_frame_count(void) {
    return _sapp.frame_count;
}

SOKOL_API_IMPL double sapp_frame_duration(void) {
    return _sapp_timing_get_avg(&_sapp.timing);
}

SOKOL_API_IMPL int sapp_width(void) {
    return (_sapp.framebuffer_width > 0) ? _sapp.framebuffer_width : 1;
}

SOKOL_API_IMPL float sapp_widthf(void) {
    return (float)sapp_width();
}

SOKOL_API_IMPL int sapp_height(void) {
    return (_sapp.framebuffer_height > 0) ? _sapp.framebuffer_height : 1;
}

SOKOL_API_IMPL float sapp_heightf(void) {
    return (float)sapp_height();
}

SOKOL_API_IMPL int sapp_color_format(void) {
    #if defined(_SAPP_EMSCRIPTEN) && defined(SOKOL_WGPU)
        switch (_sapp.wgpu.render_format) {
            case WGPUTextureFormat_RGBA8Unorm:
                return _SAPP_PIXELFORMAT_RGBA8;
            case WGPUTextureFormat_BGRA8Unorm:
                return _SAPP_PIXELFORMAT_BGRA8;
            default:
                SOKOL_UNREACHABLE;
                return 0;
        }
    #elif defined(SOKOL_METAL) || defined(SOKOL_D3D11)
        return _SAPP_PIXELFORMAT_BGRA8;
    #else
        return _SAPP_PIXELFORMAT_RGBA8;
    #endif
}

SOKOL_API_IMPL int sapp_depth_format(void) {
    return _SAPP_PIXELFORMAT_DEPTH_STENCIL;
}

SOKOL_API_IMPL int sapp_sample_count(void) {
    return _sapp.sample_count;
}

SOKOL_API_IMPL bool sapp_high_dpi(void) {
    return _sapp.desc.high_dpi && (_sapp.dpi_scale >= 1.5f);
}

SOKOL_API_IMPL float sapp_dpi_scale(void) {
    return _sapp.dpi_scale;
}

SOKOL_API_IMPL const void* sapp_egl_get_display(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(_SAPP_ANDROID)
        return _sapp.android.display;
    #elif defined(_SAPP_LINUX) && !defined(_SAPP_GLX)
        return _sapp.egl.display;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_egl_get_context(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(_SAPP_ANDROID)
        return _sapp.android.context;
    #elif defined(_SAPP_LINUX) && !defined(_SAPP_GLX)
        return _sapp.egl.context;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL void sapp_show_keyboard(bool show) {
    #if defined(_SAPP_IOS)
    _sapp_ios_show_keyboard(show);
    #elif defined(_SAPP_ANDROID)
    _sapp_android_show_keyboard(show);
    #else
    _SOKOL_UNUSED(show);
    #endif
}

SOKOL_API_IMPL bool sapp_keyboard_shown(void) {
    return _sapp.onscreen_keyboard_shown;
}

SOKOL_API_IMPL bool sapp_is_fullscreen(void) {
    return _sapp.fullscreen;
}

SOKOL_API_IMPL void sapp_toggle_fullscreen(void) {
    #if defined(_SAPP_MACOS)
    _sapp_macos_toggle_fullscreen();
    #elif defined(_SAPP_WIN32)
    _sapp_win32_toggle_fullscreen();
    #elif defined(_SAPP_LINUX)
    _sapp_x11_toggle_fullscreen();
    #endif
}

/* NOTE that sapp_show_mouse() does not "stack" like the Win32 or macOS API functions! */
SOKOL_API_IMPL void sapp_show_mouse(bool show) {
    if (_sapp.mouse.shown != show) {
        #if defined(_SAPP_MACOS)
        _sapp_macos_update_cursor(_sapp.mouse.current_cursor, show);
        #elif defined(_SAPP_WIN32)
        _sapp_win32_update_cursor(_sapp.mouse.current_cursor, show, false);
        #elif defined(_SAPP_LINUX)
        _sapp_x11_update_cursor(_sapp.mouse.current_cursor, show);
        #elif defined(_SAPP_EMSCRIPTEN)
        _sapp_emsc_update_cursor(_sapp.mouse.current_cursor, show);
        #endif
        _sapp.mouse.shown = show;
    }
}

SOKOL_API_IMPL bool sapp_mouse_shown(void) {
    return _sapp.mouse.shown;
}

SOKOL_API_IMPL void sapp_lock_mouse(bool lock) {
    #if defined(_SAPP_MACOS)
    _sapp_macos_lock_mouse(lock);
    #elif defined(_SAPP_EMSCRIPTEN)
    _sapp_emsc_lock_mouse(lock);
    #elif defined(_SAPP_WIN32)
    _sapp_win32_lock_mouse(lock);
    #elif defined(_SAPP_LINUX)
    _sapp_x11_lock_mouse(lock);
    #else
    _sapp.mouse.locked = lock;
    #endif
}

SOKOL_API_IMPL bool sapp_mouse_locked(void) {
    return _sapp.mouse.locked;
}

SOKOL_API_IMPL void sapp_set_mouse_cursor(sapp_mouse_cursor cursor) {
    SOKOL_ASSERT((cursor >= 0) && (cursor < _SAPP_MOUSECURSOR_NUM));
    if (_sapp.mouse.current_cursor != cursor) {
        #if defined(_SAPP_MACOS)
        _sapp_macos_update_cursor(cursor, _sapp.mouse.shown);
        #elif defined(_SAPP_WIN32)
        _sapp_win32_update_cursor(cursor, _sapp.mouse.shown, false);
        #elif defined(_SAPP_LINUX)
        _sapp_x11_update_cursor(cursor, _sapp.mouse.shown);
        #elif defined(_SAPP_EMSCRIPTEN)
        _sapp_emsc_update_cursor(cursor, _sapp.mouse.shown);
        #endif
        _sapp.mouse.current_cursor = cursor;
    }
}

SOKOL_API_IMPL sapp_mouse_cursor sapp_get_mouse_cursor(void) {
    return _sapp.mouse.current_cursor;
}

SOKOL_API_IMPL void sapp_request_quit(void) {
    _sapp.quit_requested = true;
}

SOKOL_API_IMPL void sapp_cancel_quit(void) {
    _sapp.quit_requested = false;
}

SOKOL_API_IMPL void sapp_quit(void) {
    _sapp.quit_ordered = true;
}

SOKOL_API_IMPL void sapp_consume_event(void) {
    _sapp.event_consumed = true;
}

/* NOTE: on HTML5, sapp_set_clipboard_string() must be called from within event handler! */
SOKOL_API_IMPL void sapp_set_clipboard_string(const char* str) {
    if (!_sapp.clipboard.enabled) {
        return;
    }
    SOKOL_ASSERT(str);
    #if defined(_SAPP_MACOS)
        _sapp_macos_set_clipboard_string(str);
    #elif defined(_SAPP_EMSCRIPTEN)
        _sapp_emsc_set_clipboard_string(str);
    #elif defined(_SAPP_WIN32)
        _sapp_win32_set_clipboard_string(str);
    #else
        /* not implemented */
    #endif
    _sapp_strcpy(str, _sapp.clipboard.buffer, _sapp.clipboard.buf_size);
}

SOKOL_API_IMPL const char* sapp_get_clipboard_string(void) {
    if (!_sapp.clipboard.enabled) {
        return "";
    }
    #if defined(_SAPP_MACOS)
        return _sapp_macos_get_clipboard_string();
    #elif defined(_SAPP_EMSCRIPTEN)
        return _sapp.clipboard.buffer;
    #elif defined(_SAPP_WIN32)
        return _sapp_win32_get_clipboard_string();
    #else
        /* not implemented */
        return _sapp.clipboard.buffer;
    #endif
}

SOKOL_API_IMPL void sapp_set_window_title(const char* title) {
    SOKOL_ASSERT(title);
    _sapp_strcpy(title, _sapp.window_title, sizeof(_sapp.window_title));
    #if defined(_SAPP_MACOS)
        _sapp_macos_update_window_title();
    #elif defined(_SAPP_WIN32)
        _sapp_win32_update_window_title();
    #elif defined(_SAPP_LINUX)
        _sapp_x11_update_window_title();
    #endif
}

SOKOL_API_IMPL void sapp_set_icon(const sapp_icon_desc* desc) {
    SOKOL_ASSERT(desc);
    if (desc->sokol_default) {
        if (0 == _sapp.default_icon_pixels) {
            _sapp_setup_default_icon();
        }
        SOKOL_ASSERT(0 != _sapp.default_icon_pixels);
        desc = &_sapp.default_icon_desc;
    }
    const int num_images = _sapp_icon_num_images(desc);
    if (num_images == 0) {
        return;
    }
    SOKOL_ASSERT((num_images > 0) && (num_images <= SAPP_MAX_ICONIMAGES));
    if (!_sapp_validate_icon_desc(desc, num_images)) {
        return;
    }
    #if defined(_SAPP_MACOS)
        _sapp_macos_set_icon(desc, num_images);
    #elif defined(_SAPP_WIN32)
        _sapp_win32_set_icon(desc, num_images);
    #elif defined(_SAPP_LINUX)
        _sapp_x11_set_icon(desc, num_images);
    #elif defined(_SAPP_EMSCRIPTEN)
        _sapp_emsc_set_icon(desc, num_images);
    #endif
}

SOKOL_API_IMPL int sapp_get_num_dropped_files(void) {
    SOKOL_ASSERT(_sapp.drop.enabled);
    return _sapp.drop.num_files;
}

SOKOL_API_IMPL const char* sapp_get_dropped_file_path(int index) {
    SOKOL_ASSERT(_sapp.drop.enabled);
    SOKOL_ASSERT((index >= 0) && (index < _sapp.drop.num_files));
    SOKOL_ASSERT(_sapp.drop.buffer);
    if (!_sapp.drop.enabled) {
        return "";
    }
    if ((index < 0) || (index >= _sapp.drop.max_files)) {
        return "";
    }
    return (const char*) _sapp_dropped_file_path_ptr(index);
}

SOKOL_API_IMPL uint32_t sapp_html5_get_dropped_file_size(int index) {
    SOKOL_ASSERT(_sapp.drop.enabled);
    SOKOL_ASSERT((index >= 0) && (index < _sapp.drop.num_files));
    #if defined(_SAPP_EMSCRIPTEN)
        if (!_sapp.drop.enabled) {
            return 0;
        }
        return sapp_js_dropped_file_size(index);
    #else
        (void)index;
        return 0;
    #endif
}

SOKOL_API_IMPL void sapp_html5_fetch_dropped_file(const sapp_html5_fetch_request* request) {
    SOKOL_ASSERT(_sapp.drop.enabled);
    SOKOL_ASSERT(request);
    SOKOL_ASSERT(request->callback);
    SOKOL_ASSERT(request->buffer.ptr);
    SOKOL_ASSERT(request->buffer.size > 0);
    #if defined(_SAPP_EMSCRIPTEN)
        const int index = request->dropped_file_index;
        sapp_html5_fetch_error error_code = SAPP_HTML5_FETCH_ERROR_NO_ERROR;
        if ((index < 0) || (index >= _sapp.drop.num_files)) {
            error_code = SAPP_HTML5_FETCH_ERROR_OTHER;
        }
        if (sapp_html5_get_dropped_file_size(index) > request->buffer.size) {
            error_code = SAPP_HTML5_FETCH_ERROR_BUFFER_TOO_SMALL;
        }
        if (SAPP_HTML5_FETCH_ERROR_NO_ERROR != error_code) {
            _sapp_emsc_invoke_fetch_cb(index,
                false, // success
                (int)error_code,
                request->callback,
                0, // fetched_size
                (void*)request->buffer.ptr,
                request->buffer.size,
                request->user_data);
        }
        else {
            sapp_js_fetch_dropped_file(index,
                request->callback,
                (void*)request->buffer.ptr,
                request->buffer.size,
                request->user_data);
        }
    #else
        (void)request;
    #endif
}

SOKOL_API_IMPL const void* sapp_metal_get_device(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(SOKOL_METAL)
        #if defined(_SAPP_MACOS)
            const void* obj = (__bridge const void*) _sapp.macos.mtl_device;
        #else
            const void* obj = (__bridge const void*) _sapp.ios.mtl_device;
        #endif
        SOKOL_ASSERT(obj);
        return obj;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_metal_get_current_drawable(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(SOKOL_METAL)
        #if defined(_SAPP_MACOS)
            const void* obj = (__bridge const void*) [_sapp.macos.view currentDrawable];
        #else
            const void* obj = (__bridge const void*) [_sapp.ios.view currentDrawable];
        #endif
        SOKOL_ASSERT(obj);
        return obj;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_metal_get_depth_stencil_texture(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(SOKOL_METAL)
        #if defined(_SAPP_MACOS)
            const void* obj = (__bridge const void*) [_sapp.macos.view depthStencilTexture];
        #else
            const void* obj = (__bridge const void*) [_sapp.ios.view depthStencilTexture];
        #endif
        return obj;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_metal_get_msaa_color_texture(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(SOKOL_METAL)
        #if defined(_SAPP_MACOS)
            const void* obj = (__bridge const void*) [_sapp.macos.view multisampleColorTexture];
        #else
            const void* obj = (__bridge const void*) [_sapp.ios.view multisampleColorTexture];
        #endif
        return obj;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_macos_get_window(void) {
    #if defined(_SAPP_MACOS)
        const void* obj = (__bridge const void*) _sapp.macos.window;
        SOKOL_ASSERT(obj);
        return obj;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_ios_get_window(void) {
    #if defined(_SAPP_IOS)
        const void* obj = (__bridge const void*) _sapp.ios.window;
        SOKOL_ASSERT(obj);
        return obj;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_d3d11_get_device(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(SOKOL_D3D11)
        return _sapp.d3d11.device;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_d3d11_get_device_context(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(SOKOL_D3D11)
        return _sapp.d3d11.device_context;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_d3d11_get_swap_chain(void) {
    SOKOL_ASSERT(_sapp.valid);
#if defined(SOKOL_D3D11)
    return _sapp.d3d11.swap_chain;
#else
    return 0;
#endif
}

SOKOL_API_IMPL const void* sapp_d3d11_get_render_view(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(SOKOL_D3D11)
        if (_sapp.sample_count > 1) {
            SOKOL_ASSERT(_sapp.d3d11.msaa_rtv);
            return _sapp.d3d11.msaa_rtv;
        } else {
            SOKOL_ASSERT(_sapp.d3d11.rtv);
            return _sapp.d3d11.rtv;
        }
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_d3d11_get_resolve_view(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(SOKOL_D3D11)
        if (_sapp.sample_count > 1) {
            SOKOL_ASSERT(_sapp.d3d11.rtv);
            return _sapp.d3d11.rtv;
        } else {
            return 0;
        }
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_d3d11_get_depth_stencil_view(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(SOKOL_D3D11)
        return _sapp.d3d11.dsv;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_win32_get_hwnd(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(_SAPP_WIN32)
        return _sapp.win32.hwnd;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_wgpu_get_device(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(_SAPP_EMSCRIPTEN) && defined(SOKOL_WGPU)
        return (const void*) _sapp.wgpu.device;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_wgpu_get_render_view(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(_SAPP_EMSCRIPTEN) && defined(SOKOL_WGPU)
        if (_sapp.sample_count > 1) {
            SOKOL_ASSERT(_sapp.wgpu.msaa_view);
            return (const void*) _sapp.wgpu.msaa_view;
        } else {
            SOKOL_ASSERT(_sapp.wgpu.swapchain_view);
            return (const void*) _sapp.wgpu.swapchain_view;
        }
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_wgpu_get_resolve_view(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(_SAPP_EMSCRIPTEN) && defined(SOKOL_WGPU)
        if (_sapp.sample_count > 1) {
            SOKOL_ASSERT(_sapp.wgpu.swapchain_view);
            return (const void*) _sapp.wgpu.swapchain_view;
        } else {
            return 0;
        }
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_wgpu_get_depth_stencil_view(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(_SAPP_EMSCRIPTEN) && defined(SOKOL_WGPU)
        return (const void*) _sapp.wgpu.depth_stencil_view;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL uint32_t sapp_gl_get_framebuffer(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(_SAPP_ANY_GL)
        return _sapp.gl.framebuffer;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL int sapp_gl_get_major_version(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(SOKOL_GLCORE)
        return _sapp.desc.gl_major_version;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL int sapp_gl_get_minor_version(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(SOKOL_GLCORE)
        return _sapp.desc.gl_minor_version;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_android_get_native_activity(void) {
    // NOTE: _sapp.valid is not asserted here because sapp_android_get_native_activity()
    // needs to be callable from within sokol_main() (see: https://github.com/floooh/sokol/issues/708)
    #if defined(_SAPP_ANDROID)
        return (void*)_sapp.android.activity;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL void sapp_html5_ask_leave_site(bool ask) {
    _sapp.html5_ask_leave_site = ask;
}

#endif /* SOKOL_APP_IMPL */
