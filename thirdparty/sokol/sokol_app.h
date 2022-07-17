#if defined(SOKOL_IMPL) && !defined(SOKOL_APP_IMPL)
#define SOKOL_APP_IMPL
#endif
#ifndef SOKOL_APP_INCLUDED
/*
    V language IMPORTANT NOTE:
    all the V patch in this code are marked with:
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

        #define SOKOL_GLCORE33
        #define SOKOL_GLES2
        #define SOKOL_GLES3
        #define SOKOL_D3D11
        #define SOKOL_METAL
        #define SOKOL_WGPU

    Optionally provide the following defines with your own implementations:

        SOKOL_ASSERT(c)     - your own assert macro (default: assert(c))
        SOKOL_LOG(msg)      - your own logging function (default: puts(msg))
        SOKOL_UNREACHABLE() - a guard macro for unreachable code (default: assert(false))
        SOKOL_ABORT()       - called after an unrecoverable error (default: abort())
        SOKOL_WIN32_FORCE_MAIN  - define this on Win32 to use a main() entry point instead of WinMain
        SOKOL_NO_ENTRY      - define this if sokol_app.h shouldn't "hijack" the main() function
        SOKOL_APP_API_DECL  - public function declaration prefix (default: extern)
        SOKOL_API_DECL      - same as SOKOL_APP_API_DECL
        SOKOL_API_IMPL      - public function implementation prefix (default: -)
        SOKOL_CALLOC        - your own calloc function (default: calloc(n, s))
        SOKOL_FREE          - your own free function (default: free(p))

    Optionally define the following to force debug checks and validations
    even in release mode:

        SOKOL_DEBUG         - by default this is defined if _DEBUG is defined

    If sokol_app.h is compiled as a DLL, define the following before
    including the declaration or implementation:

        SOKOL_DLL

    On Windows, SOKOL_DLL will define SOKOL_APP_API_DECL as __declspec(dllexport)
    or __declspec(dllimport) as needed.

    For example code, see https://github.com/floooh/sokol-samples/tree/master/sapp

    Portions of the Windows and Linux GL initialization, event-, icon- etc... code
    have been taken from GLFW (http://www.glfw.org/)

    iOS onscreen keyboard support 'inspired' by libgdx.

    Link with the following system libraries:

    - on macOS with Metal: Cocoa, QuartzCore, Metal, MetalKit
    - on macOS with GL: Cocoa, QuartzCore, OpenGL
    - on iOS with Metal: Foundation, UIKit, Metal, MetalKit
    - on iOS with GL: Foundation, UIKit, OpenGLES, GLKit
    - on Linux: X11, Xi, Xcursor, GL, dl, pthread, m(?)
    - on Android: GLESv3, EGL, log, android
    - on Windows with the MSVC or Clang toolchains: no action needed, libs are defined in-source via pragma-comment-lib
    - on Windows with MINGW/MSYS2 gcc: compile with '-mwin32' so that _WIN32 is defined
        - link with the following libs: -lkernel32 -luser32 -lshell32
        - additionally with the GL backend: -lgdi32
        - additionally with the D3D11 backend: -ld3d11 -ldxgi

    On Linux, you also need to use the -pthread compiler and linker option, otherwise weird
    things will happen, see here for details: https://github.com/floooh/sokol/issues/376

    Building for UWP requires a recent Visual Studio toolchain and Windows SDK
    (at least VS2019 and Windows SDK 10.0.19041.0). When the UWP backend is
    selected, the sokol_app.h implementation must be compiled as C++17.

    On macOS and iOS, the implementation must be compiled as Objective-C.

    FEATURE OVERVIEW
    ================
    sokol_app.h provides a minimalistic cross-platform API which
    implements the 'application-wrapper' parts of a 3D application:

    - a common application entry function
    - creates a window and 3D-API context/device with a 'default framebuffer'
    - makes the rendered frame visible
    - provides keyboard-, mouse- and low-level touch-events
    - platforms: MacOS, iOS, HTML5, Win32, Linux, Android (TODO: RaspberryPi)
    - 3D-APIs: Metal, D3D11, GL3.2, GLES2, GLES3, WebGL, WebGL2

    FEATURE/PLATFORM MATRIX
    =======================
                        | Windows | macOS | Linux |  iOS  | Android | UWP  | Raspi | HTML5
    --------------------+---------+-------+-------+-------+---------+------+-------+-------
    gl 3.x              | YES     | YES   | YES   | ---   | ---     | ---  | ---   | ---
    gles2/webgl         | ---     | ---   | ---   | YES   | YES     | ---  | TODO  | YES
    gles3/webgl2        | ---     | ---   | ---   | YES   | YES     | ---  | ---   | YES
    metal               | ---     | YES   | ---   | YES   | ---     | ---  | ---   | ---
    d3d11               | YES     | ---   | ---   | ---   | ---     | YES  | ---   | ---
    KEY_DOWN            | YES     | YES   | YES   | SOME  | TODO    | YES  | TODO  | YES
    KEY_UP              | YES     | YES   | YES   | SOME  | TODO    | YES  | TODO  | YES
    CHAR                | YES     | YES   | YES   | YES   | TODO    | YES  | TODO  | YES
    MOUSE_DOWN          | YES     | YES   | YES   | ---   | ---     | YES  | TODO  | YES
    MOUSE_UP            | YES     | YES   | YES   | ---   | ---     | YES  | TODO  | YES
    MOUSE_SCROLL        | YES     | YES   | YES   | ---   | ---     | YES  | TODO  | YES
    MOUSE_MOVE          | YES     | YES   | YES   | ---   | ---     | YES  | TODO  | YES
    MOUSE_ENTER         | YES     | YES   | YES   | ---   | ---     | YES  | TODO  | YES
    MOUSE_LEAVE         | YES     | YES   | YES   | ---   | ---     | YES  | TODO  | YES
    TOUCHES_BEGAN       | ---     | ---   | ---   | YES   | YES     | TODO | ---   | YES
    TOUCHES_MOVED       | ---     | ---   | ---   | YES   | YES     | TODO | ---   | YES
    TOUCHES_ENDED       | ---     | ---   | ---   | YES   | YES     | TODO | ---   | YES
    TOUCHES_CANCELLED   | ---     | ---   | ---   | YES   | YES     | TODO | ---   | YES
    RESIZED             | YES     | YES   | YES   | YES   | YES     | YES  | ---   | YES
    ICONIFIED           | YES     | YES   | YES   | ---   | ---     | YES  | ---   | ---
    RESTORED            | YES     | YES   | YES   | ---   | ---     | YES  | ---   | ---
    FOCUSED             | YES     | YES   | YES   | ---   | ---     | ---  | ---   | YES
    UNFOCUSED           | YES     | YES   | YES   | ---   | ---     | ---  | ---   | YES
    SUSPENDED           | ---     | ---   | ---   | YES   | YES     | YES  | ---   | TODO
    RESUMED             | ---     | ---   | ---   | YES   | YES     | YES  | ---   | TODO
    QUIT_REQUESTED      | YES     | YES   | YES   | ---   | ---     | ---  | TODO  | YES
    IME                 | TODO    | TODO? | TODO  | ???   | TODO    | ---  | ???   | ???
    key repeat flag     | YES     | YES   | YES   | ---   | ---     | YES  | TODO  | YES
    windowed            | YES     | YES   | YES   | ---   | ---     | YES  | TODO  | YES
    fullscreen          | YES     | YES   | YES   | YES   | YES     | YES  | TODO  | ---
    mouse hide          | YES     | YES   | YES   | ---   | ---     | YES  | TODO  | YES
    mouse lock          | YES     | YES   | YES   | ---   | ---     | TODO | TODO  | YES
    set cursor type     | YES     | YES   | YES   | ---   | ---     | YES  | TODO  | YES
    screen keyboard     | ---     | ---   | ---   | YES   | TODO    | TODO | ---   | YES
    swap interval       | YES     | YES   | YES   | YES   | TODO    | ---  | TODO  | YES
    high-dpi            | YES     | YES   | TODO  | YES   | YES     | YES  | TODO  | YES
    clipboard           | YES     | YES   | TODO  | ---   | ---     | TODO | ---   | YES
    MSAA                | YES     | YES   | YES   | YES   | YES     | TODO | TODO  | YES
    drag'n'drop         | YES     | YES   | YES   | ---   | ---     | TODO | TODO  | YES
    window icon         | YES     | YES(1)| YES   | ---   | ---     | TODO | TODO  | YES

    (1) macOS has no regular window icons, instead the dock icon is changed

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

        There are many more setup parameters, but these are the most important.
        For a complete list search for the sapp_desc structure declaration
        below.

        DO NOT call any sokol-app function from inside sokol_main(), since
        sokol-app will not be initialized at this point.

        The .width and .height parameters are the preferred size of the 3D
        rendering canvas. The actual size may differ from this depending on
        platform and other circumstances. Also the canvas size may change at
        any time (for instance when the user resizes the application window,
        or rotates the mobile device).

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
        .fail_cb (void (*)(const char* msg))
            The fail callback is called when a fatal error is encountered
            during start which doesn't allow the program to continue.
            Providing a callback here gives you a chance to show an error message
            to the user. The default behaviour is SOKOL_LOG(msg)

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
        .fail_userdata_cb (void(*)(const char* msg, void* user_data))
            These are the user-data versions of the callback functions. You
            can mix those with the standard callbacks that don't have the
            user_data argument.

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
                27 == SG_PIXELFORMAT_BGRA8
                41 == SG_PIXELFORMAT_DEPTH
                42 == SG_PIXELFORMAT_DEPTH_STENCIL

        int sapp_sample_count(void)
            Return the MSAA sample count of the default framebuffer.

        bool sapp_gles2(void)
            Returns true if a GLES2 or WebGL context has been created. This
            is useful when a GLES3/WebGL2 context was requested but is not
            available so that sokol_app.h had to fallback to GLES2/WebGL.

        const void* sapp_metal_get_device(void)
        const void* sapp_metal_get_renderpass_descriptor(void)
        const void* sapp_metal_get_drawable(void)
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

        const void* sapp_win32_get_hwnd(void)
            On Windows, get the window's HWND, otherwise a null pointer. The
            HWND has been cast to a void pointer in order to be tunneled
            through code which doesn't include Windows.h.

        const void* sapp_d3d11_get_device(void)
        const void* sapp_d3d11_get_device_context(void)
        const void* sapp_d3d11_get_render_target_view(void)
        const void* sapp_d3d11_get_depth_stencil_view(void)
            Similar to the sapp_metal_* functions, the sapp_d3d11_* functions
            return pointers to D3D11 API objects required for rendering,
            only if the D3D11 backend has been selected. Otherwise they
            return a null pointer. Note that the returned pointers to the
            render-target-view and depth-stencil-view may change from one
            frame to the next!

        const void* sapp_wgpu_get_device(void)
        const void* sapp_wgpu_get_render_view(void)
        const void* sapp_wgpu_get_resolve_view(void)
        const void* sapp_wgpu_get_depth_stencil_view(void)
            These are the WebGPU-specific functions to get the WebGPU
            objects and values required for rendering. If sokol_app.h
            is not compiled with SOKOL_WGPU, these functions return null.

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
        be added as needed, please open a github ticket and/or provide
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

        sapp_desc sokol_main() {
            return (sapp_desc) {
                .enable_dragndrop = true,   // default is false
                ...
            };
        }

    You can also adjust the maximum number of files that are accepted
    in a drop operation, and the maximum path length in bytes if needed:

        sapp_desc sokol_main() {
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
            .buffer_ptr = buf,
            .buffer_size = buf_size,
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
                const uint32_t num_bytes = response->fetched_size;
                // and the pointer to the data (same as 'buf' in the fetch-call):
                const void* ptr = response->buffer_ptr;
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
    ratio. You can also get a DPI scaling factor with the function
    sapp_dpi_scale().

    Here's an example on a Mac with Retina display:

    sapp_desc sokol_main() {
        return (sapp_desc) {
            .width = 640,
            .height = 480,
            .high_dpi = true,
            ...
        };
    }

    The functions sapp_width(), sapp_height() and sapp_dpi_scale() will
    return the following values:

    sapp_width      -> 1280
    sapp_height     -> 960
    sapp_dpi_scale  -> 2.0

    If the high_dpi flag is false, or you're not running on a Retina display,
    the values would be:

    sapp_width      -> 640
    sapp_height     -> 480
    sapp_dpi_scale  -> 1.0

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
    this means that JS event handlers are unregisters), and then
    the request-animation-loop will be exited. However that's all. The
    web page itself will continue to exist (e.g. it's not possible to
    programmatically close the browser tab).

    On the web it's also not possible to run custom code when the user
    closes a brower tab, so it's not possible to prevent this with a
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

    For an example and test of the window icon feature, check out the the
    'icon-sapp' sample on the sokol-samples git repository.

    ONSCREEN KEYBOARD
    =================
    On some platforms which don't provide a physical keyboard, sokol-app
    can display the platform's integrated onscreen keyboard for text
    input. To request that the onscreen keyboard is shown, call

        sapp_show_keyboard(true);

    Likewise, to hide the keyboard call:

        sapp_show_keyboard(false);

    Note that on the web platform, the keyboard can only be shown from
    inside an input handler. On such platforms, sapp_show_keyboard()
    will only work as expected when it is called from inside the
    sokol-app event callback function. When called from other places,
    an internal flag will be set, and the onscreen keyboard will be
    called at the next 'legal' opportunity (when the next input event
    is handled).

    OPTIONAL: DON'T HIJACK main() (#define SOKOL_NO_ENTRY)
    ======================================================
    In its default configuration, sokol_app.h "hijacks" the platform's
    standard main() function. This was done because different platforms
    have different main functions which are not compatible with
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
    - ```sapp_run()``` takes over control and calls the provided init-, frame-,
      shutdown- and event-callbacks just like in the default model, it
      will only return when the application quits (or not at all on some
      platforms, like emscripten)

    NOTE: SOKOL_NO_ENTRY is currently not supported on Android.

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

    TEMP NOTE DUMP
    ==============
    - onscreen keyboard support on Android requires Java :(, should we even bother?
    - sapp_desc needs a bool whether to initialize depth-stencil surface
    - GL context initialization needs more control (at least what GL version to initialize)
    - application icon
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
    - cursor images are ??? (FIXME)
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


typedef struct sapp_desc {
    void (*init_cb)(void);                  // these are the user-provided callbacks without user data
    void (*frame_cb)(void);
    void (*cleanup_cb)(void);
    void (*event_cb)(const sapp_event*);
    void (*fail_cb)(const char*);

    void* user_data;                        // these are the user-provided callbacks with user data
    void (*init_userdata_cb)(void*);
    void (*frame_userdata_cb)(void*);
    void (*cleanup_userdata_cb)(void*);
    void (*event_userdata_cb)(const sapp_event*, void*);
    void (*fail_userdata_cb)(const char*, void*);

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

    /* backend-specific options */
    bool gl_force_gles2;                // if true, setup GLES2/WebGL even if GLES3/WebGL2 is available
    bool win32_console_utf8;            // if true, set the output console codepage to UTF-8
    bool win32_console_create;          // if true, attach stdout/stderr to a new console window
    bool win32_console_attach;          // if true, attach stdout/stderr to parent process
    const char* html5_canvas_name;      // the name (id) of the HTML5 canvas element, default is "canvas"
    bool html5_canvas_resize;           // if true, the HTML5 canvas size is set to sapp_desc.width/height, otherwise canvas size is tracked
    bool html5_preserve_drawing_buffer; // HTML5 only: whether to preserve default framebuffer content between frames
    bool html5_premultiplied_alpha;     // HTML5 only: whether the rendered pixels use premultiplied alpha convention
    bool html5_ask_leave_site;          // initial state of the internal html5_ask_leave_site flag (see sapp_html5_ask_leave_site())
    bool ios_keyboard_resizes_canvas;   // if true, showing the iOS keyboard shrinks the canvas
    // __v_ start
    bool __v_native_render;             // V patch to allow for native rendering
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
    bool succeeded;         /* true if the loading operation has succeeded */
    sapp_html5_fetch_error error_code;
    int file_index;         /* index of the dropped file (0..sapp_get_num_dropped_filed()-1) */
    uint32_t fetched_size;  /* size in bytes of loaded data */
    void* buffer_ptr;       /* pointer to user-provided buffer which contains the loaded data */
    uint32_t buffer_size;   /* size of user-provided buffer (buffer_size >= fetched_size) */
    void* user_data;        /* user-provided user data pointer */
} sapp_html5_fetch_response;

typedef struct sapp_html5_fetch_request {
    int dropped_file_index;                 /* 0..sapp_get_num_dropped_files()-1 */
    void (*callback)(const sapp_html5_fetch_response*);     /* response callback function pointer (required) */
    void* buffer_ptr;                       /* pointer to buffer to load data into */
    uint32_t buffer_size;                   /* size in bytes of buffer */
    void* user_data;                        /* optional userdata pointer */
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
/* initiate a "hard quit" (quit application without sending SAPP_EVENTTYPE_QUIT_REQUSTED) */
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

/* GL: return true when GLES2 fallback is active (to detect fallback from GLES3) */
SOKOL_APP_API_DECL bool sapp_gles2(void);

/* HTML5: enable or disable the hardwired "Leave Site?" dialog box */
SOKOL_APP_API_DECL void sapp_html5_ask_leave_site(bool ask);
/* HTML5: get byte size of a dropped file */
SOKOL_APP_API_DECL uint32_t sapp_html5_get_dropped_file_size(int index);
/* HTML5: asynchronously load the content of a dropped file */
SOKOL_APP_API_DECL void sapp_html5_fetch_dropped_file(const sapp_html5_fetch_request* request);

/* Metal: get bridged pointer to Metal device object */
SOKOL_APP_API_DECL const void* sapp_metal_get_device(void);
/* Metal: get bridged pointer to this frame's renderpass descriptor */
SOKOL_APP_API_DECL const void* sapp_metal_get_renderpass_descriptor(void);
/* Metal: get bridged pointer to current drawable */
SOKOL_APP_API_DECL const void* sapp_metal_get_drawable(void);
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
/* D3D11: get pointer to ID3D11RenderTargetView object */
SOKOL_APP_API_DECL const void* sapp_d3d11_get_render_target_view(void);
/* D3D11: get pointer to ID3D11DepthStencilView */
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

/*-- IMPLEMENTATION ----------------------------------------------------------*/
#ifdef SOKOL_APP_IMPL
#define SOKOL_APP_IMPL_INCLUDED (1)

#include <string.h> // memset
#include <stddef.h> // size_t

/* check if the config defines are alright */
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
        #if !defined(SOKOL_METAL) && !defined(SOKOL_GLCORE33)
        #error("sokol_app.h: unknown 3D API selected for MacOS, must be SOKOL_METAL or SOKOL_GLCORE33")
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
    #if !defined(SOKOL_GLES3) && !defined(SOKOL_GLES2) && !defined(SOKOL_WGPU)
    #error("sokol_app.h: unknown 3D API selected for emscripten, must be SOKOL_GLES3, SOKOL_GLES2 or SOKOL_WGPU")
    #endif
#elif defined(_WIN32)
    /* Windows (D3D11 or GL) */
    #include <winapifamily.h>
    #if (defined(WINAPI_FAMILY_PARTITION) && !WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP))
        #define _SAPP_UWP (1)
        #if !defined(SOKOL_D3D11)
        #error("sokol_app.h: unknown 3D API selected for UWP, must be SOKOL_D3D11")
        #endif
        #if !defined(__cplusplus)
        #error("sokol_app.h: UWP bindings require C++/17")
        #endif
    #else
        #define _SAPP_WIN32 (1)
        #if !defined(SOKOL_D3D11) && !defined(SOKOL_GLCORE33)
        #error("sokol_app.h: unknown 3D API selected for Win32, must be SOKOL_D3D11 or SOKOL_GLCORE33")
        #endif
    #endif
#elif defined(__ANDROID__)
    /* Android */
    #define _SAPP_ANDROID (1)
    #if !defined(SOKOL_GLES3) && !defined(SOKOL_GLES2)
    #error("sokol_app.h: unknown 3D API selected for Android, must be SOKOL_GLES3 or SOKOL_GLES2")
    #endif
    #if defined(SOKOL_NO_ENTRY)
    #error("sokol_app.h: SOKOL_NO_ENTRY is not supported on Android")
    #endif
#elif defined(__linux__) || defined(__unix__)
    /* Linux */
    #define _SAPP_LINUX (1)
    #if !defined(SOKOL_GLCORE33)
    #error("sokol_app.h: unknown 3D API selected for Linux, must be SOKOL_GLCORE33")
    #endif
#else
#error "sokol_app.h: Unknown platform"
#endif

#ifndef SOKOL_API_IMPL
    #define SOKOL_API_IMPL
#endif
#ifndef SOKOL_DEBUG
    #ifndef NDEBUG
        #define SOKOL_DEBUG (1)
    #endif
#endif
#ifndef SOKOL_ASSERT
    #include <assert.h>
    #define SOKOL_ASSERT(c) assert(c)
#endif
#ifndef SOKOL_UNREACHABLE
    #define SOKOL_UNREACHABLE SOKOL_ASSERT(false)
#endif
#if !defined(SOKOL_CALLOC) || !defined(SOKOL_FREE)
    #include <stdlib.h>
#endif
#if !defined(SOKOL_CALLOC)
    #define SOKOL_CALLOC(n,s) calloc(n,s)
#endif
#if !defined(SOKOL_FREE)
    #define SOKOL_FREE(p) free(p)
#endif
#ifndef SOKOL_LOG
    #ifdef SOKOL_DEBUG
        #if defined(__ANDROID__)
            #include <android/log.h>
            #define SOKOL_LOG(s) { SOKOL_ASSERT(s); __android_log_write(ANDROID_LOG_INFO, "SOKOL_APP", s); }
        #else
            #include <stdio.h>
            #define SOKOL_LOG(s) { SOKOL_ASSERT(s); puts(s); }
        #endif
    #else
        #define SOKOL_LOG(s)
    #endif
#endif
#ifndef SOKOL_ABORT
    #include <stdlib.h>
    #define SOKOL_ABORT() abort()
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

/*== PLATFORM SPECIFIC INCLUDES AND DEFINES ==================================*/
#if defined(_SAPP_APPLE)
    #if defined(SOKOL_METAL)
        #import <Metal/Metal.h>
        #import <MetalKit/MetalKit.h>
    #endif
    #if defined(_SAPP_MACOS)
        #if !defined(SOKOL_METAL)
            #ifndef GL_SILENCE_DEPRECATION
            #define GL_SILENCE_DEPRECATION
            #endif
            #include <Cocoa/Cocoa.h>
        #endif
    #elif defined(_SAPP_IOS)
        #import <UIKit/UIKit.h>
        #if !defined(SOKOL_METAL)
            #import <GLKit/GLKit.h>
        #endif
    #endif
    #include <AvailabilityMacros.h>
    #include <mach/mach_time.h>
#elif defined(_SAPP_EMSCRIPTEN)
    #if defined(SOKOL_WGPU)
        #include <webgpu/webgpu.h>
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
#elif defined(_SAPP_UWP)
    #ifndef NOMINMAX
        #define NOMINMAX
    #endif
    #ifdef _MSC_VER
        #pragma warning(push)
        #pragma warning(disable:4201)   /* nonstandard extension used: nameless struct/union */
        #pragma warning(disable:4054)   /* 'type cast': from function pointer */
        #pragma warning(disable:4055)   /* 'type cast': from data pointer */
        #pragma warning(disable:4505)   /* unreferenced local function has been removed */
        #pragma warning(disable:4115)   /* /W4: 'ID3D11ModuleInstance': named type definition in parentheses (in d3d11.h) */
    #endif
    #include <windows.h>
    #include <winrt/Windows.ApplicationModel.Core.h>
    #include <winrt/Windows.Foundation.h>
    #include <winrt/Windows.Foundation.Collections.h>
    #include <winrt/Windows.Graphics.Display.h>
    #include <winrt/Windows.UI.Core.h>
    #include <winrt/Windows.UI.Composition.h>
    #include <winrt/Windows.UI.Input.h>
    #include <winrt/Windows.UI.ViewManagement.h>
    #include <winrt/Windows.System.h>
    #include <ppltasks.h>

    #include <dxgi1_4.h>
    #include <d3d11_3.h>
    #include <DirectXMath.h>

    #pragma comment (lib, "WindowsApp")
    #pragma comment (lib, "dxguid")
#elif defined(_SAPP_ANDROID)
    #include <pthread.h>
    #include <unistd.h>
    #include <time.h>
    #include <android/native_activity.h>
    #include <android/looper.h>
    #include <EGL/egl.h>
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
    #include <dlfcn.h> /* dlopen, dlsym, dlclose */
    #include <limits.h> /* LONG_MAX */
    #include <pthread.h>    /* only used a linker-guard, search for _sapp_linux_run() and see first comment */
    #include <time.h>
#endif

/*== frame timing helpers ===================================================*/
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
    #elif defined(_SAPP_WIN32) || defined(_SAPP_UWP)
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
    #elif defined(_SAPP_WIN32) || defined(_SAPP_UWP)
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
    #elif defined(_SAPP_WIN32) || defined(_SAPP_UWP)
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
    int num;
    _sapp_timestamp_t timestamp;
    _sapp_ring_t ring;
} _sapp_timing_t;

_SOKOL_PRIVATE void _sapp_timing_init(_sapp_timing_t* t) {
    t->last = 0.0;
    t->accum = 0.0;
    // dummy value until first actual value is available
    t->avg = 1.0 / 60.0;
    t->num = 0;
    _sapp_timestamp_init(&t->timestamp);
    _sapp_ring_init(&t->ring);
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
}

_SOKOL_PRIVATE void _sapp_timing_measure(_sapp_timing_t* t) {
    const double now = _sapp_timestamp_now(&t->timestamp);
    if (t->last > 0.0) {
        double dur = now - t->last;
        _sapp_timing_put(t, dur);
    }
    t->last = now;
}

// call this if the external timing had been disrupted somehow
_SOKOL_PRIVATE void _sapp_timing_external_reset(_sapp_timing_t* t) {
    t->last = 0.0;
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

/*== MACOS DECLARATIONS ======================================================*/
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
#elif defined(SOKOL_GLCORE33)
    @interface _sapp_macos_view : NSOpenGLView
    - (void)timerFired:(id)sender;
    @end
#endif // SOKOL_GLCORE33

typedef struct {
    uint32_t flags_changed_store;
    uint8_t mouse_buttons;
    // __v_ start
    //NSWindow* window; // __v_ removed
    _sapp_macos_window* window; // __v_ added
    // __v_ end
    NSTrackingArea* tracking_area;
    _sapp_macos_app_delegate* app_dlg;
    _sapp_macos_window_delegate* win_dlg;
    _sapp_macos_view* view;
    NSCursor* cursors[_SAPP_MOUSECURSOR_NUM];
    #if defined(SOKOL_METAL)
        id<MTLDevice> mtl_device;
    #endif
} _sapp_macos_t;

#endif // _SAPP_MACOS

/*== IOS DECLARATIONS ========================================================*/
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

/*== EMSCRIPTEN DECLARATIONS =================================================*/
#if defined(_SAPP_EMSCRIPTEN)

#if defined(SOKOL_WGPU)
typedef struct {
    int state;
    WGPUDevice device;
    WGPUSwapChain swapchain;
    WGPUTextureFormat render_format;
    WGPUTexture msaa_tex;
    WGPUTexture depth_stencil_tex;
    WGPUTextureView swapchain_view;
    WGPUTextureView msaa_view;
    WGPUTextureView depth_stencil_view;
} _sapp_wgpu_t;
#endif

typedef struct {
    bool textfield_created;
    bool wants_show_keyboard;
    bool wants_hide_keyboard;
    bool mouse_lock_requested;
    uint16_t mouse_buttons;
    #if defined(SOKOL_WGPU)
    _sapp_wgpu_t wgpu;
    #endif
} _sapp_emsc_t;
#endif // _SAPP_EMSCRIPTEN

/*== WIN32 DECLARATIONS ======================================================*/
#if defined(SOKOL_D3D11) && (defined(_SAPP_WIN32) || defined(_SAPP_UWP))
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
    UINT sync_refresh_count;
} _sapp_d3d11_t;
#endif

/*== WIN32 DECLARATIONS ======================================================*/
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
#endif /*DPI_ENUMS_DECLARED*/

typedef struct {
    bool aware;
    float content_scale;
    float window_scale;
    float mouse_scale;
} _sapp_win32_dpi_t;

typedef struct {
    HWND hwnd;
    HDC dc;
    HICON big_icon;
    HICON small_icon;
    HCURSOR cursors[_SAPP_MOUSECURSOR_NUM];
    UINT orig_codepage;
    LONG mouse_locked_x, mouse_locked_y;
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

#if defined(SOKOL_GLCORE33)
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
    bool ext_swap_control;
    bool arb_multisample;
    bool arb_pixel_format;
    bool arb_create_context;
    bool arb_create_context_profile;
    HWND msg_hwnd;
    HDC msg_dc;
} _sapp_wgl_t;
#endif // SOKOL_GLCORE33

#endif // _SAPP_WIN32

/*== UWP DECLARATIONS ======================================================*/
#if defined(_SAPP_UWP)

typedef struct {
    float content_scale;
    float window_scale;
    float mouse_scale;
} _sapp_uwp_dpi_t;

typedef struct {
    bool mouse_tracked;
    uint8_t mouse_buttons;
    _sapp_uwp_dpi_t dpi;
} _sapp_uwp_t;

#endif // _SAPP_UWP

/*== ANDROID DECLARATIONS ====================================================*/

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

/*== LINUX DECLARATIONS ======================================================*/
#if defined(_SAPP_LINUX)

#define _SAPP_X11_XDND_VERSION (5)

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
} _sapp_x11_t;

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

    // extension availability
    bool EXT_swap_control;
    bool MESA_swap_control;
    bool ARB_multisample;
    bool ARB_create_context;
    bool ARB_create_context_profile;
} _sapp_glx_t;

#endif // _SAPP_LINUX

/*== COMMON DECLARATIONS =====================================================*/

/* helper macros */
#define _sapp_def(val, def) (((val) == 0) ? (def) : (val))
#define _sapp_absf(a) (((a)<0.0f)?-(a):(a))

#define _SAPP_MAX_TITLE_LENGTH (128)
/* NOTE: the pixel format values *must* be compatible with sg_pixel_format */
#define _SAPP_PIXELFORMAT_RGBA8 (23)
#define _SAPP_PIXELFORMAT_BGRA8 (27)
#define _SAPP_PIXELFORMAT_DEPTH (41)
#define _SAPP_PIXELFORMAT_DEPTH_STENCIL (42)

#if defined(_SAPP_MACOS) || defined(_SAPP_IOS)
    // this is ARC compatible
    #if defined(__cplusplus)
        #define _SAPP_CLEAR(type, item) { item = (type) { }; }
    #else
        #define _SAPP_CLEAR(type, item) { item = (type) { 0 }; }
    #endif
#else
    #define _SAPP_CLEAR(type, item) { memset(&item, 0, sizeof(item)); }
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
    bool gles2_fallback;
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
    #elif defined(_SAPP_WIN32)
        _sapp_win32_t win32;
        #if defined(SOKOL_D3D11)
            _sapp_d3d11_t d3d11;
        #elif defined(SOKOL_GLCORE33)
            _sapp_wgl_t wgl;
        #endif
    #elif defined(_SAPP_UWP)
            _sapp_uwp_t uwp;
        #if defined(SOKOL_D3D11)
            _sapp_d3d11_t d3d11;
        #endif
    #elif defined(_SAPP_ANDROID)
        _sapp_android_t android;
    #elif defined(_SAPP_LINUX)
        _sapp_x11_t x11;
        _sapp_glx_t glx;
    #endif
    char html5_canvas_selector[_SAPP_MAX_TITLE_LENGTH];
    char window_title[_SAPP_MAX_TITLE_LENGTH];      /* UTF-8 */
    wchar_t window_title_wide[_SAPP_MAX_TITLE_LENGTH];   /* UTF-32 or UCS-2 */
    sapp_keycode keycodes[SAPP_MAX_KEYCODES];
    // __v_ start
    bool __v_native_render;             /* V patch to allow for native rendering */
    // __v_ end
} _sapp_t;
static _sapp_t _sapp;

/*=== PRIVATE HELPER FUNCTIONS ===============================================*/
_SOKOL_PRIVATE void _sapp_fail(const char* msg) {
    if (_sapp.desc.fail_cb) {
        _sapp.desc.fail_cb(msg);
    }
    else if (_sapp.desc.fail_userdata_cb) {
        _sapp.desc.fail_userdata_cb(msg, _sapp.desc.user_data);
    }
    else {
        SOKOL_LOG(msg);
    }
    SOKOL_ABORT();
}

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
    // __v_ end
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

_SOKOL_PRIVATE sapp_desc _sapp_desc_defaults(const sapp_desc* in_desc) {
    sapp_desc desc = *in_desc;
    desc.width = _sapp_def(desc.width, 640);
    desc.height = _sapp_def(desc.height, 480);
    desc.sample_count = _sapp_def(desc.sample_count, 1);
    desc.swap_interval = _sapp_def(desc.swap_interval, 1);
    desc.html5_canvas_name = _sapp_def(desc.html5_canvas_name, "canvas");
    desc.clipboard_size = _sapp_def(desc.clipboard_size, 8192);
    desc.max_dropped_files = _sapp_def(desc.max_dropped_files, 1);
    desc.max_dropped_file_path_length = _sapp_def(desc.max_dropped_file_path_length, 2048);
    desc.window_title = _sapp_def(desc.window_title, "sokol_app");
    return desc;
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
    _SAPP_CLEAR(_sapp_t, _sapp);
    _sapp.desc = _sapp_desc_defaults(desc);
    _sapp.first_frame = true;
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
        _sapp.clipboard.buffer = (char*) SOKOL_CALLOC(1, (size_t)_sapp.clipboard.buf_size);
    }
    _sapp.drop.enabled = _sapp.desc.enable_dragndrop;
    if (_sapp.drop.enabled) {
        _sapp.drop.max_files = _sapp.desc.max_dropped_files;
        _sapp.drop.max_path_length = _sapp.desc.max_dropped_file_path_length;
        _sapp.drop.buf_size = _sapp.drop.max_files * _sapp.drop.max_path_length;
        _sapp.drop.buffer = (char*) SOKOL_CALLOC(1, (size_t)_sapp.drop.buf_size);
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
        SOKOL_FREE((void*)_sapp.clipboard.buffer);
    }
    if (_sapp.drop.enabled) {
        SOKOL_ASSERT(_sapp.drop.buffer);
        SOKOL_FREE((void*)_sapp.drop.buffer);
    }
    if (_sapp.default_icon_pixels) {
        SOKOL_FREE((void*)_sapp.default_icon_pixels);
    }
    _SAPP_CLEAR(_sapp_t, _sapp);
}

_SOKOL_PRIVATE void _sapp_init_event(sapp_event_type type) {
    memset(&_sapp.event, 0, sizeof(_sapp.event));
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
        memset(_sapp.drop.buffer, 0, (size_t)_sapp.drop.buf_size);
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
        SOKOL_LOG("Image data size mismatch (must be width*height*4 bytes)\n");
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
    _sapp.default_icon_pixels = (uint32_t*) SOKOL_CALLOC((size_t)all_num_pixels, sizeof(uint32_t));

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

/*== MacOS/iOS ===============================================================*/
#if defined(_SAPP_APPLE)

#if __has_feature(objc_arc)
#define _SAPP_OBJC_RELEASE(obj) { obj = nil; }
#else
#define _SAPP_OBJC_RELEASE(obj) { [obj release]; obj = nil; }
#endif

/*== MacOS ===================================================================*/
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
    const NSEventModifierFlags f = ev.modifierFlags;
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
    #if defined(SOKOL_METAL)
        const NSRect fb_rect = [_sapp.macos.view bounds];
        _sapp.framebuffer_width = fb_rect.size.width * _sapp.dpi_scale;
        _sapp.framebuffer_height = fb_rect.size.height * _sapp.dpi_scale;
    #elif defined(SOKOL_GLCORE33)
        const NSRect fb_rect = [_sapp.macos.view convertRectToBacking:[_sapp.macos.view frame]];
        _sapp.framebuffer_width = fb_rect.size.width;
        _sapp.framebuffer_height = fb_rect.size.height;
    #endif
    const NSRect bounds = [_sapp.macos.view bounds];
    _sapp.window_width = bounds.size.width;
    _sapp.window_height = bounds.size.height;
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
    _sapp.dpi_scale = (float)_sapp.framebuffer_width / (float)_sapp.window_width;

    /* NOTE: _sapp_macos_update_dimensions() isn't called each frame, but only
        when the window size actually changes, so resizing the MTKView's
        in each call is fine even when MTKView doesn't ignore setting an
        identical drawableSize.
    */
    #if defined(SOKOL_METAL)
    CGSize drawable_size = { (CGFloat) _sapp.framebuffer_width, (CGFloat) _sapp.framebuffer_height };
    _sapp.macos.view.drawableSize = drawable_size;
    #endif
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

_SOKOL_PRIVATE void _sapp_macos_update_mouse(NSEvent* event) {
    if (!_sapp.mouse.locked) {
        const NSPoint mouse_pos = event.locationInWindow;
        float new_x = mouse_pos.x * _sapp.dpi_scale;
        float new_y = _sapp.framebuffer_height - (mouse_pos.y * _sapp.dpi_scale) - 1;
        /* don't update dx/dy in the very first update */
        if (_sapp.mouse.pos_valid) {
            _sapp.mouse.dx = new_x - _sapp.mouse.x;
            _sapp.mouse.dy = new_y - _sapp.mouse.y;
        }
        _sapp.mouse.x = new_x;
        _sapp.mouse.y = new_y;
        _sapp.mouse.pos_valid = true;
    }
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
    if (_sapp.fullscreen) {
        NSRect screen_rect = NSScreen.mainScreen.frame;
        _sapp.window_width = screen_rect.size.width;
        _sapp.window_height = screen_rect.size.height;
    }
    if (_sapp.desc.high_dpi) {
        _sapp.framebuffer_width = 2 * _sapp.window_width;
        _sapp.framebuffer_height = 2 * _sapp.window_height;
    }
    else {
        _sapp.framebuffer_width = _sapp.window_width;
        _sapp.framebuffer_height = _sapp.window_height;
    }
    _sapp.dpi_scale = (float)_sapp.framebuffer_width / (float) _sapp.window_width;
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

    _sapp.macos.win_dlg = [[_sapp_macos_window_delegate alloc] init];
    _sapp.macos.window.delegate = _sapp.macos.win_dlg;
    #if defined(SOKOL_METAL)
        NSInteger max_fps = 60;
        #if (__MAC_OS_X_VERSION_MAX_ALLOWED >= 120000)
        if (@available(macOS 12.0, *)) {
            max_fps = NSScreen.mainScreen.maximumFramesPerSecond;
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
    #elif defined(SOKOL_GLCORE33)
        NSOpenGLPixelFormatAttribute attrs[32];
        int i = 0;
        attrs[i++] = NSOpenGLPFAAccelerated;
        attrs[i++] = NSOpenGLPFADoubleBuffer;
        attrs[i++] = NSOpenGLPFAOpenGLProfile; attrs[i++] = NSOpenGLProfileVersion3_2Core;
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
    _sapp.valid = true;
    // __v_ start
    if (!_sapp.__v_native_render) { // __v_
    if (_sapp.fullscreen) {
        /* on GL, this already toggles a rendered frame, so set the valid flag before */
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
        overlayWindow.contentView = g_view;

    [   contentView addSubview:g_view];
//[    _sapp.macos.window addChildWindow:overlayWindow ordered:NSWindowAbove];
        [_sapp.macos.window center];
    }
    //////////////////////////////////
    // __v_ end
    [_sapp.macos.window makeKeyAndOrderFront:nil];
    _sapp_macos_update_dimensions();
// __v_ start
//    [NSEvent setMouseCoalescingEnabled:NO];
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

- (void)windowDidResize:(NSNotification*)notification {
    _SOKOL_UNUSED(notification);
    _sapp_macos_update_dimensions();
    if (!_sapp.first_frame) {
        _sapp_macos_app_event(SAPP_EVENTTYPE_RESIZED);
    }
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
        _sapp.drop.num_files = ((int)pboard.pasteboardItems.count > _sapp.drop.max_files) ? _sapp.drop.max_files : pboard.pasteboardItems.count;
        bool drop_failed = false;
        for (int i = 0; i < _sapp.drop.num_files; i++) {
            NSURL *fileUrl = [NSURL fileURLWithPath:[pboard.pasteboardItems[(NSUInteger)i] stringForType:NSPasteboardTypeFileURL]];
            if (!_sapp_strcpy(fileUrl.standardizedURL.path.UTF8String, _sapp_dropped_file_path_ptr(i), _sapp.drop.max_path_length)) {
                SOKOL_LOG("sokol_app.h: dropped file path too long (sapp_desc.max_dropped_file_path_length)\n");
                drop_failed = true;
                break;
            }
        }
        if (!drop_failed) {
            if (_sapp_events_enabled()) {
                _sapp_init_event(SAPP_EVENTTYPE_FILES_DROPPED);
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
#if defined(SOKOL_GLCORE33)
/* NOTE: this is a hack/fix when the initial window size has been clipped by
    macOS because it didn't fit on the screen, in that case the
    frame size of the window is reported wrong if low-dpi rendering
    was requested (instead the high-dpi dimensions are returned)
    until the window is resized for the first time.

    Hooking into reshape and getting the frame dimensions seems to report
    the correct dimensions.
*/
- (void)reshape {
    _sapp_macos_update_dimensions();
    [super reshape];
}
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

_SOKOL_PRIVATE void _sapp_macos_poll_input_events() {
    /*

    NOTE: late event polling temporarily out-commented to check if this
    causes infrequent and almost impossible to reproduce probelms with the
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
    _sapp_timing_measure(&_sapp.timing);
    /* Catch any last-moment input events */
    _sapp_macos_poll_input_events();
    @autoreleasepool {
        _sapp_macos_frame();
    }
    #if !defined(SOKOL_METAL)
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
- (void)mouseEntered:(NSEvent*)event {
    _sapp_macos_update_mouse(event);
    /* don't send mouse enter/leave while dragging (so that it behaves the same as
       on Windows while SetCapture is active
    */
    if (0 == _sapp.macos.mouse_buttons) {
        _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_ENTER, SAPP_MOUSEBUTTON_INVALID, _sapp_macos_mods(event));
    }
}
- (void)mouseExited:(NSEvent*)event {
    _sapp_macos_update_mouse(event);
    if (0 == _sapp.macos.mouse_buttons) {
        _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_LEAVE, SAPP_MOUSEBUTTON_INVALID, _sapp_macos_mods(event));
    }
}
- (void)mouseDown:(NSEvent*)event {
    _sapp_macos_update_mouse(event);
    _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_DOWN, SAPP_MOUSEBUTTON_LEFT, _sapp_macos_mods(event));
    _sapp.macos.mouse_buttons |= (1<<SAPP_MOUSEBUTTON_LEFT);
}
- (void)mouseUp:(NSEvent*)event {
    _sapp_macos_update_mouse(event);
    _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_UP, SAPP_MOUSEBUTTON_LEFT, _sapp_macos_mods(event));
    _sapp.macos.mouse_buttons &= ~(1<<SAPP_MOUSEBUTTON_LEFT);
}
- (void)rightMouseDown:(NSEvent*)event {
    _sapp_macos_update_mouse(event);
    _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_DOWN, SAPP_MOUSEBUTTON_RIGHT, _sapp_macos_mods(event));
    _sapp.macos.mouse_buttons |= (1<<SAPP_MOUSEBUTTON_RIGHT);
}
- (void)rightMouseUp:(NSEvent*)event {
    _sapp_macos_update_mouse(event);
    _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_UP, SAPP_MOUSEBUTTON_RIGHT, _sapp_macos_mods(event));
    _sapp.macos.mouse_buttons &= ~(1<<SAPP_MOUSEBUTTON_RIGHT);
}
- (void)otherMouseDown:(NSEvent*)event {
    _sapp_macos_update_mouse(event);
    if (2 == event.buttonNumber) {
        _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_DOWN, SAPP_MOUSEBUTTON_MIDDLE, _sapp_macos_mods(event));
        _sapp.macos.mouse_buttons |= (1<<SAPP_MOUSEBUTTON_MIDDLE);
    }
}
- (void)otherMouseUp:(NSEvent*)event {
    _sapp_macos_update_mouse(event);
    if (2 == event.buttonNumber) {
        _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_UP, SAPP_MOUSEBUTTON_MIDDLE, _sapp_macos_mods(event));
        _sapp.macos.mouse_buttons &= (1<<SAPP_MOUSEBUTTON_MIDDLE);
    }
}
- (void)otherMouseDragged:(NSEvent*)event {
    _sapp_macos_update_mouse(event);
    if (2 == event.buttonNumber) {
        if (_sapp.mouse.locked) {
            _sapp.mouse.dx = [event deltaX];
            _sapp.mouse.dy = [event deltaY];
        }
        _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_MOVE, SAPP_MOUSEBUTTON_INVALID, _sapp_macos_mods(event));
    }
}
- (void)mouseMoved:(NSEvent*)event {
    _sapp_macos_update_mouse(event);
    if (_sapp.mouse.locked) {
        _sapp.mouse.dx = [event deltaX];
        _sapp.mouse.dy = [event deltaY];
    }
    _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_MOVE, SAPP_MOUSEBUTTON_INVALID , _sapp_macos_mods(event));
}
- (void)mouseDragged:(NSEvent*)event {
    _sapp_macos_update_mouse(event);
    if (_sapp.mouse.locked) {
        _sapp.mouse.dx = [event deltaX];
        _sapp.mouse.dy = [event deltaY];
    }
    _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_MOVE, SAPP_MOUSEBUTTON_INVALID , _sapp_macos_mods(event));
}
- (void)rightMouseDragged:(NSEvent*)event {
    _sapp_macos_update_mouse(event);
    if (_sapp.mouse.locked) {
        _sapp.mouse.dx = [event deltaX];
        _sapp.mouse.dy = [event deltaY];
    }
    _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_MOVE, SAPP_MOUSEBUTTON_INVALID, _sapp_macos_mods(event));
}
- (void)scrollWheel:(NSEvent*)event {
    _sapp_macos_update_mouse(event);
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
        const uint32_t mods = _sapp_macos_mods(event);
        /* NOTE: macOS doesn't send keyUp events while the Cmd key is pressed,
            as a workaround, to prevent key presses from sticking we'll send
            a keyup event following right after the keydown if SUPER is also pressed
        */
        const sapp_keycode key_code = _sapp_translate_key(event.keyCode);
        _sapp_macos_key_event(SAPP_EVENTTYPE_KEY_DOWN, key_code, event.isARepeat, mods);
        if (0 != (mods & SAPP_MODIFIER_SUPER)) {
            _sapp_macos_key_event(SAPP_EVENTTYPE_KEY_UP, key_code, event.isARepeat, mods);
        }
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
    _sapp_macos_key_event(SAPP_EVENTTYPE_KEY_UP,
        _sapp_translate_key(event.keyCode),
        event.isARepeat,
        _sapp_macos_mods(event));
}
- (void)flagsChanged:(NSEvent*)event {
    const uint32_t old_f = _sapp.macos.flags_changed_store;
    const uint32_t new_f = event.modifierFlags;
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

#endif /* MacOS */

/*== iOS =====================================================================*/
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
    _sapp.framebuffer_width = (int)(screen_rect.size.width * _sapp.dpi_scale);
    _sapp.framebuffer_height = (int)(screen_rect.size.height * _sapp.dpi_scale);
    _sapp.window_width = (int)screen_rect.size.width;
    _sapp.window_height = (int)screen_rect.size.height;
    int cur_fb_width, cur_fb_height;
    #if defined(SOKOL_METAL)
        const CGSize fb_size = _sapp.ios.view.drawableSize;
        cur_fb_width = (int) fb_size.width;
        cur_fb_height = (int) fb_size.height;
    #else
        cur_fb_width = (int) _sapp.ios.view.drawableWidth;
        cur_fb_height = (int) _sapp.ios.view.drawableHeight;
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
    _sapp.window_width = screen_rect.size.width;
    _sapp.window_height = screen_rect.size.height;
    if (_sapp.desc.high_dpi) {
        _sapp.dpi_scale = (float) UIScreen.mainScreen.nativeScale;
    }
    else {
        _sapp.dpi_scale = 1.0f;
    }
    _sapp.framebuffer_width = _sapp.window_width * _sapp.dpi_scale;
    _sapp.framebuffer_height = _sapp.window_height * _sapp.dpi_scale;
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
        if (_sapp.desc.gl_force_gles2) {
            _sapp.ios.eagl_ctx = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES2];
            _sapp.gles2_fallback = true;
        }
        else {
            _sapp.ios.eagl_ctx = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES3];
            if (_sapp.ios.eagl_ctx == nil) {
                _sapp.ios.eagl_ctx = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES2];
                _sapp.gles2_fallback = true;
            }
        }
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

/*== EMSCRIPTEN ==============================================================*/
#if defined(_SAPP_EMSCRIPTEN)

#ifdef __cplusplus
extern "C" {
#endif

typedef void (*_sapp_html5_fetch_callback) (const sapp_html5_fetch_response*);

/* this function is called from a JS event handler when the user hides
    the onscreen keyboard pressing the 'dismiss keyboard key'
*/
EMSCRIPTEN_KEEPALIVE void _sapp_emsc_notify_keyboard_hidden(void) {
    _sapp.onscreen_keyboard_shown = false;
}

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
        SOKOL_LOG("sokol_app.h: dropped file path too long!\n");
        _sapp.drop.num_files = 0;
    }
}

EMSCRIPTEN_KEEPALIVE void _sapp_emsc_end_drop(int x, int y) {
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
        _sapp_call_event(&_sapp.event);
    }
}

EMSCRIPTEN_KEEPALIVE void _sapp_emsc_invoke_fetch_cb(int index, int success, int error_code, _sapp_html5_fetch_callback callback, uint32_t fetched_size, void* buf_ptr, uint32_t buf_size, void* user_data) {
    sapp_html5_fetch_response response;
    memset(&response, 0, sizeof(response));
    response.succeeded = (0 != success);
    response.error_code = (sapp_html5_fetch_error) error_code;
    response.file_index = index;
    response.fetched_size = fetched_size;
    response.buffer_ptr = buf_ptr;
    response.buffer_size = buf_size;
    response.user_data = user_data;
    callback(&response);
}

#ifdef __cplusplus
} /* extern "C" */
#endif

/* Javascript helper functions for mobile virtual keyboard input */
EM_JS(void, sapp_js_create_textfield, (void), {
    var _sapp_inp = document.createElement("input");
    _sapp_inp.type = "text";
    _sapp_inp.id = "_sokol_app_input_element";
    _sapp_inp.autocapitalize = "none";
    _sapp_inp.addEventListener("focusout", function(_sapp_event) {
        __sapp_emsc_notify_keyboard_hidden()

    });
    document.body.append(_sapp_inp);
});

EM_JS(void, sapp_js_focus_textfield, (void), {
    document.getElementById("_sokol_app_input_element").focus();
});

EM_JS(void, sapp_js_unfocus_textfield, (void), {
    document.getElementById("_sokol_app_input_element").blur();
});

EM_JS(void, sapp_js_add_beforeunload_listener, (void), {
    Module.sokol_beforeunload = function(event) {
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
    Module.sokol_paste = function(event) {
        var pasted_str = event.clipboardData.getData('text');
        ccall('_sapp_emsc_onpaste', 'void', ['string'], [pasted_str]);
    };
    window.addEventListener('paste', Module.sokol_paste);
});

EM_JS(void, sapp_js_remove_clipboard_listener, (void), {
    window.removeEventListener('paste', Module.sokol_paste);
});

EM_JS(void, sapp_js_write_clipboard, (const char* c_str), {
    var str = UTF8ToString(c_str);
    var ta = document.createElement('textarea');
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
    var canvas_name = UTF8ToString(canvas_name_cstr);
    var canvas = document.getElementById(canvas_name);
    Module.sokol_dragenter = function(event) {
        event.stopPropagation();
        event.preventDefault();
    };
    Module.sokol_dragleave = function(event) {
        event.stopPropagation();
        event.preventDefault();
    };
    Module.sokol_dragover = function(event) {
        event.stopPropagation();
        event.preventDefault();
    };
    Module.sokol_drop = function(event) {
        event.stopPropagation();
        event.preventDefault();
        var files = event.dataTransfer.files;
        Module.sokol_dropped_files = files;
        __sapp_emsc_begin_drop(files.length);
        var i;
        for (i = 0; i < files.length; i++) {
            ccall('_sapp_emsc_drop', 'void', ['number', 'string'], [i, files[i].name]);
        }
        // FIXME? see computation of targetX/targetY in emscripten via getClientBoundingRect
        __sapp_emsc_end_drop(event.clientX, event.clientY);
    };
    canvas.addEventListener('dragenter', Module.sokol_dragenter, false);
    canvas.addEventListener('dragleave', Module.sokol_dragleave, false);
    canvas.addEventListener('dragover',  Module.sokol_dragover, false);
    canvas.addEventListener('drop',      Module.sokol_drop, false);
});

EM_JS(uint32_t, sapp_js_dropped_file_size, (int index), {
    if ((index < 0) || (index >= Module.sokol_dropped_files.length)) {
        return 0;
    }
    else {
        return Module.sokol_dropped_files[index].size;
    }
});

EM_JS(void, sapp_js_fetch_dropped_file, (int index, _sapp_html5_fetch_callback callback, void* buf_ptr, uint32_t buf_size, void* user_data), {
    var reader = new FileReader();
    reader.onload = function(loadEvent) {
        var content = loadEvent.target.result;
        if (content.byteLength > buf_size) {
            // SAPP_HTML5_FETCH_ERROR_BUFFER_TOO_SMALL
            __sapp_emsc_invoke_fetch_cb(index, 0, 1, callback, 0, buf_ptr, buf_size, user_data);
        }
        else {
            HEAPU8.set(new Uint8Array(content), buf_ptr);
            __sapp_emsc_invoke_fetch_cb(index, 1, 0, callback, content.byteLength, buf_ptr, buf_size, user_data);
        }
    };
    reader.onerror = function() {
        // SAPP_HTML5_FETCH_ERROR_OTHER
        __sapp_emsc_invoke_fetch_cb(index, 0, 2, callback, 0, buf_ptr, buf_size, user_data);
    };
    reader.readAsArrayBuffer(Module.sokol_dropped_files[index]);
});

EM_JS(void, sapp_js_remove_dragndrop_listeners, (const char* canvas_name_cstr), {
    var canvas_name = UTF8ToString(canvas_name_cstr);
    var canvas = document.getElementById(canvas_name);
    canvas.removeEventListener('dragenter', Module.sokol_dragenter);
    canvas.removeEventListener('dragleave', Module.sokol_dragleave);
    canvas.removeEventListener('dragover',  Module.sokol_dragover);
    canvas.removeEventListener('drop',      Module.sokol_drop);
});

/* called from the emscripten event handler to update the keyboard visibility
    state, this must happen from an JS input event handler, otherwise
    the request will be ignored by the browser
*/
_SOKOL_PRIVATE void _sapp_emsc_update_keyboard_state(void) {
    if (_sapp.emsc.wants_show_keyboard) {
        /* create input text field on demand */
        if (!_sapp.emsc.textfield_created) {
            _sapp.emsc.textfield_created = true;
            sapp_js_create_textfield();
        }
        /* focus the text input field, this will bring up the keyboard */
        _sapp.onscreen_keyboard_shown = true;
        _sapp.emsc.wants_show_keyboard = false;
        sapp_js_focus_textfield();
    }
    if (_sapp.emsc.wants_hide_keyboard) {
        /* unfocus the text input field */
        if (_sapp.emsc.textfield_created) {
            _sapp.onscreen_keyboard_shown = false;
            _sapp.emsc.wants_hide_keyboard = false;
            sapp_js_unfocus_textfield();
        }
    }
}

/* actually showing the onscreen keyboard must be initiated from a JS
    input event handler, so we'll just keep track of the desired
    state, and the actual state change will happen with the next input event
*/
_SOKOL_PRIVATE void _sapp_emsc_show_keyboard(bool show) {
    if (show) {
        _sapp.emsc.wants_show_keyboard = true;
    }
    else {
        _sapp.emsc.wants_hide_keyboard = true;
    }
}

EM_JS(void, sapp_js_pointer_init, (const char* c_str_target), {
    // lookup and store canvas object by name
    var target_str = UTF8ToString(c_str_target);
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
        var cursor;
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
    var link = document.getElementById('sokol-app-favicon');
    if (link) {
        document.head.removeChild(link);
    }
});

EM_JS(void, sapp_js_set_favicon, (int w, int h, const uint8_t* pixels), {
    var canvas = document.createElement('canvas');
    canvas.width = w;
    canvas.height = h;
    var ctx = canvas.getContext('2d');
    var img_data = ctx.createImageData(w, h);
    img_data.data.set(HEAPU8.subarray(pixels, pixels + w*h*4));
    ctx.putImageData(img_data, 0, 0);
    var new_link = document.createElement('link');
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

#if defined(SOKOL_WGPU)
_SOKOL_PRIVATE void _sapp_emsc_wgpu_surfaces_create(void);
_SOKOL_PRIVATE void _sapp_emsc_wgpu_surfaces_discard(void);
#endif

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
        _sapp.window_width = (int) w;
    }
    if (h < 1.0) {
        h = ui_event->windowInnerHeight;
    }
    else {
        _sapp.window_height = (int) h;
    }
    if (_sapp.desc.high_dpi) {
        _sapp.dpi_scale = emscripten_get_device_pixel_ratio();
    }
    _sapp.framebuffer_width = (int) (w * _sapp.dpi_scale);
    _sapp.framebuffer_height = (int) (h * _sapp.dpi_scale);
    SOKOL_ASSERT((_sapp.framebuffer_width > 0) && (_sapp.framebuffer_height > 0));
    emscripten_set_canvas_element_size(_sapp.html5_canvas_selector, _sapp.framebuffer_width, _sapp.framebuffer_height);
    #if defined(SOKOL_WGPU)
        /* on WebGPU: recreate size-dependent rendering surfaces */
        _sapp_emsc_wgpu_surfaces_discard();
        _sapp_emsc_wgpu_surfaces_create();
    #endif
    if (_sapp_events_enabled()) {
        _sapp_init_event(SAPP_EVENTTYPE_RESIZED);
        _sapp_call_event(&_sapp.event);
    }
    return true;
}

_SOKOL_PRIVATE EM_BOOL _sapp_emsc_mouse_cb(int emsc_type, const EmscriptenMouseEvent* emsc_event, void* user_data) {
    _SOKOL_UNUSED(user_data);
    _sapp.emsc.mouse_buttons = emsc_event->buttons;
    if (_sapp.mouse.locked) {
        _sapp.mouse.dx = (float) emsc_event->movementX;
        _sapp.mouse.dy = (float) emsc_event->movementY;
    }
    else {
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
                break;
            case EMSCRIPTEN_EVENT_MOUSELEAVE:
                type = SAPP_EVENTTYPE_MOUSE_LEAVE;
                break;
            default:
                type = SAPP_EVENTTYPE_INVALID;
                break;
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
            }
            else {
                _sapp.event.mouse_button = SAPP_MOUSEBUTTON_INVALID;
            }
            _sapp_call_event(&_sapp.event);
        }
        /* mouse lock can only be activated in mouse button events (not in move, enter or leave) */
        if (is_button_event) {
            _sapp_emsc_update_mouse_lock_state();
        }
    }
    _sapp_emsc_update_keyboard_state();
    return true;
}

_SOKOL_PRIVATE EM_BOOL _sapp_emsc_wheel_cb(int emsc_type, const EmscriptenWheelEvent* emsc_event, void* user_data) {
    _SOKOL_UNUSED(emsc_type);
    _SOKOL_UNUSED(user_data);
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
        _sapp_call_event(&_sapp.event);
    }
    _sapp_emsc_update_keyboard_state();
    _sapp_emsc_update_mouse_lock_state();
    return true;
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

_SOKOL_PRIVATE EM_BOOL _sapp_emsc_key_cb(int emsc_type, const EmscriptenKeyboardEvent* emsc_event, void* user_data) {
    _SOKOL_UNUSED(user_data);
    bool retval = true;
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
                _sapp.event.char_code = emsc_event->charCode;
                /* workaround to make Cmd+V work on Safari */
                if ((emsc_event->metaKey) && (emsc_event->charCode == 118)) {
                    retval = false;
                }
            }
            else {
                _sapp.event.key_code = _sapp_emsc_translate_key(emsc_event->code);
                /* Special hack for macOS: if the Super key is pressed, macOS doesn't
                    send keyUp events. As a workaround, to prevent keys from
                    "sticking", we'll send a keyup event following a keydown
                    when the SUPER key is pressed
                */
                if ((type == SAPP_EVENTTYPE_KEY_DOWN) &&
                    (_sapp.event.key_code != SAPP_KEYCODE_LEFT_SUPER) &&
                    (_sapp.event.key_code != SAPP_KEYCODE_RIGHT_SUPER) &&
                    (_sapp.event.modifiers & SAPP_MODIFIER_SUPER))
                {
                    send_keyup_followup = true;
                }
                /* only forward a certain key ranges to the browser */
                switch (_sapp.event.key_code) {
                    case SAPP_KEYCODE_WORLD_1:
                    case SAPP_KEYCODE_WORLD_2:
                    case SAPP_KEYCODE_ESCAPE:
                    case SAPP_KEYCODE_ENTER:
                    case SAPP_KEYCODE_TAB:
                    case SAPP_KEYCODE_BACKSPACE:
                    case SAPP_KEYCODE_INSERT:
                    case SAPP_KEYCODE_DELETE:
                    case SAPP_KEYCODE_RIGHT:
                    case SAPP_KEYCODE_LEFT:
                    case SAPP_KEYCODE_DOWN:
                    case SAPP_KEYCODE_UP:
                    case SAPP_KEYCODE_PAGE_UP:
                    case SAPP_KEYCODE_PAGE_DOWN:
                    case SAPP_KEYCODE_HOME:
                    case SAPP_KEYCODE_END:
                    case SAPP_KEYCODE_CAPS_LOCK:
                    case SAPP_KEYCODE_SCROLL_LOCK:
                    case SAPP_KEYCODE_NUM_LOCK:
                    case SAPP_KEYCODE_PRINT_SCREEN:
                    case SAPP_KEYCODE_PAUSE:
                    case SAPP_KEYCODE_F1:
                    case SAPP_KEYCODE_F2:
                    case SAPP_KEYCODE_F3:
                    case SAPP_KEYCODE_F4:
                    case SAPP_KEYCODE_F5:
                    case SAPP_KEYCODE_F6:
                    case SAPP_KEYCODE_F7:
                    case SAPP_KEYCODE_F8:
                    case SAPP_KEYCODE_F9:
                    case SAPP_KEYCODE_F10:
                    case SAPP_KEYCODE_F11:
                    case SAPP_KEYCODE_F12:
                    case SAPP_KEYCODE_F13:
                    case SAPP_KEYCODE_F14:
                    case SAPP_KEYCODE_F15:
                    case SAPP_KEYCODE_F16:
                    case SAPP_KEYCODE_F17:
                    case SAPP_KEYCODE_F18:
                    case SAPP_KEYCODE_F19:
                    case SAPP_KEYCODE_F20:
                    case SAPP_KEYCODE_F21:
                    case SAPP_KEYCODE_F22:
                    case SAPP_KEYCODE_F23:
                    case SAPP_KEYCODE_F24:
                    case SAPP_KEYCODE_F25:
                    case SAPP_KEYCODE_LEFT_SHIFT:
                    case SAPP_KEYCODE_LEFT_CONTROL:
                    case SAPP_KEYCODE_LEFT_ALT:
                    case SAPP_KEYCODE_LEFT_SUPER:
                    case SAPP_KEYCODE_RIGHT_SHIFT:
                    case SAPP_KEYCODE_RIGHT_CONTROL:
                    case SAPP_KEYCODE_RIGHT_ALT:
                    case SAPP_KEYCODE_RIGHT_SUPER:
                    case SAPP_KEYCODE_MENU:
                        /* consume the event */
                        break;
                    default:
                        /* forward key to browser */
                        retval = false;
                        break;
                }
            }
            if (_sapp_call_event(&_sapp.event)) {
                /* consume event via sapp_consume_event() */
                retval = true;
            }
            if (send_keyup_followup) {
                _sapp.event.type = SAPP_EVENTTYPE_KEY_UP;
                if (_sapp_call_event(&_sapp.event)) {
                    retval = true;
                }
            }
        }
    }
    _sapp_emsc_update_keyboard_state();
    _sapp_emsc_update_mouse_lock_state();
    return retval;
}

_SOKOL_PRIVATE EM_BOOL _sapp_emsc_touch_cb(int emsc_type, const EmscriptenTouchEvent* emsc_event, void* user_data) {
    _SOKOL_UNUSED(user_data);
    bool retval = true;
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
                retval = false;
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
            _sapp_call_event(&_sapp.event);
        }
    }
    _sapp_emsc_update_keyboard_state();
    return retval;
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

#if defined(SOKOL_GLES2) || defined(SOKOL_GLES3)
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
    #if defined(SOKOL_GLES3)
        if (_sapp.desc.gl_force_gles2) {
            attrs.majorVersion = 1;
            _sapp.gles2_fallback = true;
        }
        else {
            attrs.majorVersion = 2;
        }
    #endif
    EMSCRIPTEN_WEBGL_CONTEXT_HANDLE ctx = emscripten_webgl_create_context(_sapp.html5_canvas_selector, &attrs);
    if (!ctx) {
        attrs.majorVersion = 1;
        ctx = emscripten_webgl_create_context(_sapp.html5_canvas_selector, &attrs);
        _sapp.gles2_fallback = true;
    }
    emscripten_webgl_make_context_current(ctx);

    /* some WebGL extension are not enabled automatically by emscripten */
    emscripten_webgl_enable_extension(ctx, "WEBKIT_WEBGL_compressed_texture_pvrtc");
}
#endif

#if defined(SOKOL_WGPU)
#define _SAPP_EMSC_WGPU_STATE_INITIAL (0)
#define _SAPP_EMSC_WGPU_STATE_READY (1)
#define _SAPP_EMSC_WGPU_STATE_RUNNING (2)

#if defined(__cplusplus)
extern "C" {
#endif
/* called when the asynchronous WebGPU device + swapchain init code in JS has finished */
EMSCRIPTEN_KEEPALIVE void _sapp_emsc_wgpu_ready(int device_id, int swapchain_id, int swapchain_fmt) {
    SOKOL_ASSERT(0 == _sapp.emsc.wgpu.device);
    _sapp.emsc.wgpu.device = (WGPUDevice) device_id;
    _sapp.emsc.wgpu.swapchain = (WGPUSwapChain) swapchain_id;
    _sapp.emsc.wgpu.render_format = (WGPUTextureFormat) swapchain_fmt;
    _sapp.emsc.wgpu.state = _SAPP_EMSC_WGPU_STATE_READY;
}
#if defined(__cplusplus)
} // extern "C"
#endif

/* embedded JS function to handle all the asynchronous WebGPU setup */
EM_JS(void, sapp_js_wgpu_init, (), {
    WebGPU.initManagers();
    // FIXME: the extension activation must be more clever here
    navigator.gpu.requestAdapter().then(function(adapter) {
        console.log("wgpu adapter extensions: " + adapter.extensions);
        adapter.requestDevice({ extensions: ["textureCompressionBC"]}).then(function(device) {
            var gpuContext = document.getElementById("canvas").getContext("gpupresent");
            console.log("wgpu device extensions: " + adapter.extensions);
            gpuContext.getSwapChainPreferredFormat(device).then(function(fmt) {
                var swapChainDescriptor = { device: device, format: fmt };
                var swapChain = gpuContext.configureSwapChain(swapChainDescriptor);
                var deviceId = WebGPU.mgrDevice.create(device);
                var swapChainId = WebGPU.mgrSwapChain.create(swapChain);
                var fmtId = WebGPU.TextureFormat.findIndex(function(elm) { return elm==fmt; });
                console.log("wgpu device: " + device);
                console.log("wgpu swap chain: " + swapChain);
                console.log("wgpu preferred format: " + fmt + " (" + fmtId + ")");
                __sapp_emsc_wgpu_ready(deviceId, swapChainId, fmtId);
            });
        });
    });
});

_SOKOL_PRIVATE void _sapp_emsc_wgpu_surfaces_create(void) {
    SOKOL_ASSERT(_sapp.emsc.wgpu.device);
    SOKOL_ASSERT(_sapp.emsc.wgpu.swapchain);
    SOKOL_ASSERT(0 == _sapp.emsc.wgpu.depth_stencil_tex);
    SOKOL_ASSERT(0 == _sapp.emsc.wgpu.depth_stencil_view);
    SOKOL_ASSERT(0 == _sapp.emsc.wgpu.msaa_tex);
    SOKOL_ASSERT(0 == _sapp.emsc.wgpu.msaa_view);

    WGPUTextureDescriptor ds_desc;
    memset(&ds_desc, 0, sizeof(ds_desc));
    ds_desc.usage = WGPUTextureUsage_OutputAttachment;
    ds_desc.dimension = WGPUTextureDimension_2D;
    ds_desc.size.width = (uint32_t) _sapp.framebuffer_width;
    ds_desc.size.height = (uint32_t) _sapp.framebuffer_height;
    ds_desc.size.depth = 1;
    ds_desc.arrayLayerCount = 1;
    ds_desc.format = WGPUTextureFormat_Depth24PlusStencil8;
    ds_desc.mipLevelCount = 1;
    ds_desc.sampleCount = _sapp.sample_count;
    _sapp.emsc.wgpu.depth_stencil_tex = wgpuDeviceCreateTexture(_sapp.emsc.wgpu.device, &ds_desc);
    _sapp.emsc.wgpu.depth_stencil_view = wgpuTextureCreateView(_sapp.emsc.wgpu.depth_stencil_tex, 0);

    if (_sapp.sample_count > 1) {
        WGPUTextureDescriptor msaa_desc;
        memset(&msaa_desc, 0, sizeof(msaa_desc));
        msaa_desc.usage = WGPUTextureUsage_OutputAttachment;
        msaa_desc.dimension = WGPUTextureDimension_2D;
        msaa_desc.size.width = (uint32_t) _sapp.framebuffer_width;
        msaa_desc.size.height = (uint32_t) _sapp.framebuffer_height;
        msaa_desc.size.depth = 1;
        msaa_desc.arrayLayerCount = 1;
        msaa_desc.format = _sapp.emsc.wgpu.render_format;
        msaa_desc.mipLevelCount = 1;
        msaa_desc.sampleCount = _sapp.sample_count;
        _sapp.emsc.wgpu.msaa_tex = wgpuDeviceCreateTexture(_sapp.emsc.wgpu.device, &msaa_desc);
        _sapp.emsc.wgpu.msaa_view = wgpuTextureCreateView(_sapp.emsc.wgpu.msaa_tex, 0);
    }
}

_SOKOL_PRIVATE void _sapp_emsc_wgpu_surfaces_discard(void) {
    if (_sapp.emsc.wgpu.msaa_tex) {
        wgpuTextureRelease(_sapp.emsc.wgpu.msaa_tex);
        _sapp.emsc.wgpu.msaa_tex = 0;
    }
    if (_sapp.emsc.wgpu.msaa_view) {
        wgpuTextureViewRelease(_sapp.emsc.wgpu.msaa_view);
        _sapp.emsc.wgpu.msaa_view = 0;
    }
    if (_sapp.emsc.wgpu.depth_stencil_tex) {
        wgpuTextureRelease(_sapp.emsc.wgpu.depth_stencil_tex);
        _sapp.emsc.wgpu.depth_stencil_tex = 0;
    }
    if (_sapp.emsc.wgpu.depth_stencil_view) {
        wgpuTextureViewRelease(_sapp.emsc.wgpu.depth_stencil_view);
        _sapp.emsc.wgpu.depth_stencil_view = 0;
    }
}

_SOKOL_PRIVATE void _sapp_emsc_wgpu_next_frame(void) {
    if (_sapp.emsc.wgpu.swapchain_view) {
        wgpuTextureViewRelease(_sapp.emsc.wgpu.swapchain_view);
    }
    _sapp.emsc.wgpu.swapchain_view = wgpuSwapChainGetCurrentTextureView(_sapp.emsc.wgpu.swapchain);
}
#endif

_SOKOL_PRIVATE void _sapp_emsc_register_eventhandlers(void) {
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
    #if defined(SOKOL_GLES2) || defined(SOKOL_GLES3)
        emscripten_set_webglcontextlost_callback(_sapp.html5_canvas_selector, 0, true, _sapp_emsc_webgl_context_cb);
        emscripten_set_webglcontextrestored_callback(_sapp.html5_canvas_selector, 0, true, _sapp_emsc_webgl_context_cb);
    #endif
}

_SOKOL_PRIVATE void _sapp_emsc_unregister_eventhandlers() {
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
    sapp_js_remove_beforeunload_listener();
    if (_sapp.clipboard.enabled) {
        sapp_js_remove_clipboard_listener();
    }
    if (_sapp.drop.enabled) {
        sapp_js_remove_dragndrop_listeners(&_sapp.html5_canvas_selector[1]);
    }
    #if defined(SOKOL_GLES2) || defined(SOKOL_GLES3)
        emscripten_set_webglcontextlost_callback(_sapp.html5_canvas_selector, 0, true, 0);
        emscripten_set_webglcontextrestored_callback(_sapp.html5_canvas_selector, 0, true, 0);
    #endif
}

_SOKOL_PRIVATE EM_BOOL _sapp_emsc_frame(double time, void* userData) {
    _SOKOL_UNUSED(userData);
    _sapp_timing_external(&_sapp.timing, time / 1000.0);

    #if defined(SOKOL_WGPU)
        /*
            on WebGPU, the emscripten frame callback will already be called while
            the asynchronous WebGPU device and swapchain initialization is still
            in progress
        */
        switch (_sapp.emsc.wgpu.state) {
            case _SAPP_EMSC_WGPU_STATE_INITIAL:
                /* async JS init hasn't finished yet */
                break;
            case _SAPP_EMSC_WGPU_STATE_READY:
                /* perform post-async init stuff */
                _sapp_emsc_wgpu_surfaces_create();
                _sapp.emsc.wgpu.state = _SAPP_EMSC_WGPU_STATE_RUNNING;
                break;
            case _SAPP_EMSC_WGPU_STATE_RUNNING:
                /* a regular frame */
                _sapp_emsc_wgpu_next_frame();
                _sapp_frame();
                break;
        }
    #else
        /* WebGL code path */
        _sapp_frame();
    #endif

    /* quit-handling */
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

_SOKOL_PRIVATE void _sapp_emsc_run(const sapp_desc* desc) {
    _sapp_init_state(desc);
    sapp_js_pointer_init(&_sapp.html5_canvas_selector[1]);
    double w, h;
    if (_sapp.desc.html5_canvas_resize) {
        w = (double) _sapp.desc.width;
        h = (double) _sapp.desc.height;
    }
    else {
        emscripten_get_element_css_size(_sapp.html5_canvas_selector, &w, &h);
        emscripten_set_resize_callback(EMSCRIPTEN_EVENT_TARGET_WINDOW, 0, false, _sapp_emsc_size_changed);
    }
    if (_sapp.desc.high_dpi) {
        _sapp.dpi_scale = emscripten_get_device_pixel_ratio();
    }
    _sapp.window_width = (int) w;
    _sapp.window_height = (int) h;
    _sapp.framebuffer_width = (int) (w * _sapp.dpi_scale);
    _sapp.framebuffer_height = (int) (h * _sapp.dpi_scale);
    emscripten_set_canvas_element_size(_sapp.html5_canvas_selector, _sapp.framebuffer_width, _sapp.framebuffer_height);
    #if defined(SOKOL_GLES2) || defined(SOKOL_GLES3)
        _sapp_emsc_webgl_init();
    #elif defined(SOKOL_WGPU)
        sapp_js_wgpu_init();
    #endif
    _sapp.valid = true;
    _sapp_emsc_register_eventhandlers();
    sapp_set_icon(&desc->icon);

    /* start the frame loop */
    emscripten_request_animation_frame_loop(_sapp_emsc_frame, 0);

    /* NOT A BUG: do not call _sapp_discard_state() here, instead this is
       called in _sapp_emsc_frame() when the application is ordered to quit
     */
}

#if !defined(SOKOL_NO_ENTRY)
int main(int argc, char* argv[]) {
    sapp_desc desc = sokol_main(argc, argv);
    _sapp_emsc_run(&desc);
    return 0;
}
#endif /* SOKOL_NO_ENTRY */
#endif /* _SAPP_EMSCRIPTEN */

/*== MISC GL SUPPORT FUNCTIONS ================================================*/
#if defined(SOKOL_GLCORE33)
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
    memset(fbconfig, 0, sizeof(_sapp_gl_fbconfig));
    /* -1 means "don't care" */
    fbconfig->red_bits = -1;
    fbconfig->green_bits = -1;
    fbconfig->blue_bits = -1;
    fbconfig->alpha_bits = -1;
    fbconfig->depth_bits = -1;
    fbconfig->stencil_bits = -1;
    fbconfig->samples = -1;
}

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

/*== WINDOWS DESKTOP and UWP====================================================*/
#if defined(_SAPP_WIN32) || defined(_SAPP_UWP)
_SOKOL_PRIVATE bool _sapp_win32_uwp_utf8_to_wide(const char* src, wchar_t* dst, int dst_num_bytes) {
    SOKOL_ASSERT(src && dst && (dst_num_bytes > 1));
    memset(dst, 0, (size_t)dst_num_bytes);
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

_SOKOL_PRIVATE void _sapp_win32_uwp_app_event(sapp_event_type type) {
    if (_sapp_events_enabled()) {
        _sapp_init_event(type);
        _sapp_call_event(&_sapp.event);
    }
}

_SOKOL_PRIVATE void _sapp_win32_uwp_init_keytable(void) {
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
#endif // _SAPP_WIN32 || _SAPP_UWP

/*== WINDOWS DESKTOP===========================================================*/
#if defined(_SAPP_WIN32)

#if defined(SOKOL_D3D11)

#if defined(__cplusplus)
#define _sapp_d3d11_Release(self) (self)->Release()
#else
#define _sapp_d3d11_Release(self) (self)->lpVtbl->Release(self)
#endif

#define _SAPP_SAFE_RELEASE(obj) if (obj) { _sapp_d3d11_Release(obj); obj=0; }

static const IID _sapp_IID_ID3D11Texture2D = { 0x6f15aaf2,0xd208,0x4e89,0x9a,0xb4,0x48,0x95,0x35,0xd3,0x4f,0x9c };

static inline HRESULT _sapp_dxgi_GetBuffer(IDXGISwapChain* self, UINT Buffer, REFIID riid, void** ppSurface) {
    #if defined(__cplusplus)
        return self->GetBuffer(Buffer, riid, ppSurface);
    #else
        return self->lpVtbl->GetBuffer(self, Buffer, riid, ppSurface);
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

static inline void _sapp_d3d11_ResolveSubresource(ID3D11DeviceContext* self, ID3D11Resource* pDstResource, UINT DstSubresource, ID3D11Resource* pSrcResource, UINT SrcSubresource, DXGI_FORMAT Format) {
    #if defined(__cplusplus)
        self->ResolveSubresource(pDstResource, DstSubresource, pSrcResource, SrcSubresource, Format);
    #else
        self->lpVtbl->ResolveSubresource(self, pDstResource, DstSubresource, pSrcResource, SrcSubresource, Format);
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
    }
    else {
        sc_desc->BufferCount = 1;
        sc_desc->SwapEffect = DXGI_SWAP_EFFECT_DISCARD;
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
    SOKOL_ASSERT(SUCCEEDED(hr) && _sapp.d3d11.swap_chain && _sapp.d3d11.device && _sapp.d3d11.device_context);
}

_SOKOL_PRIVATE void _sapp_d3d11_destroy_device_and_swapchain(void) {
    _SAPP_SAFE_RELEASE(_sapp.d3d11.swap_chain);
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
    #ifdef __cplusplus
    hr = _sapp_dxgi_GetBuffer(_sapp.d3d11.swap_chain, 0, _sapp_IID_ID3D11Texture2D, (void**)&_sapp.d3d11.rt);
    #else
    hr = _sapp_dxgi_GetBuffer(_sapp.d3d11.swap_chain, 0, &_sapp_IID_ID3D11Texture2D, (void**)&_sapp.d3d11.rt);
    #endif
    SOKOL_ASSERT(SUCCEEDED(hr) && _sapp.d3d11.rt);
    hr = _sapp_d3d11_CreateRenderTargetView(_sapp.d3d11.device, (ID3D11Resource*)_sapp.d3d11.rt, NULL, &_sapp.d3d11.rtv);
    SOKOL_ASSERT(SUCCEEDED(hr) && _sapp.d3d11.rtv);

    /* common desc for MSAA and depth-stencil texture */
    D3D11_TEXTURE2D_DESC tex_desc;
    memset(&tex_desc, 0, sizeof(tex_desc));
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

_SOKOL_PRIVATE void _sapp_d3d11_present(void) {
    /* do MSAA resolve if needed */
    if (_sapp.sample_count > 1) {
        SOKOL_ASSERT(_sapp.d3d11.rt);
        SOKOL_ASSERT(_sapp.d3d11.msaa_rt);
        _sapp_d3d11_ResolveSubresource(_sapp.d3d11.device_context, (ID3D11Resource*)_sapp.d3d11.rt, 0, (ID3D11Resource*)_sapp.d3d11.msaa_rt, 0, DXGI_FORMAT_B8G8R8A8_UNORM);
    }
    _sapp_dxgi_Present(_sapp.d3d11.swap_chain, (UINT)_sapp.swap_interval, 0);
}

#endif /* SOKOL_D3D11 */

#if defined(SOKOL_GLCORE33)
_SOKOL_PRIVATE void _sapp_wgl_init(void) {
    _sapp.wgl.opengl32 = LoadLibraryA("opengl32.dll");
    if (!_sapp.wgl.opengl32) {
        _sapp_fail("Failed to load opengl32.dll\n");
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

    _sapp.wgl.msg_hwnd = CreateWindowExW(WS_EX_OVERLAPPEDWINDOW,
        L"SOKOLAPP",
        L"sokol-app message window",
        WS_CLIPSIBLINGS|WS_CLIPCHILDREN,
        0, 0, 1, 1,
        NULL, NULL,
        GetModuleHandleW(NULL),
        NULL);
    if (!_sapp.wgl.msg_hwnd) {
        _sapp_fail("Win32: failed to create helper window!\n");
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
        _sapp_fail("Win32: failed to obtain helper window DC!\n");
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
    memset(&pfd, 0, sizeof(pfd));
    pfd.nSize = sizeof(pfd);
    pfd.nVersion = 1;
    pfd.dwFlags = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER;
    pfd.iPixelType = PFD_TYPE_RGBA;
    pfd.cColorBits = 24;
    if (!SetPixelFormat(_sapp.wgl.msg_dc, ChoosePixelFormat(_sapp.wgl.msg_dc, &pfd), &pfd)) {
        _sapp_fail("WGL: failed to set pixel format for dummy context\n");
    }
    HGLRC rc = _sapp.wgl.CreateContext(_sapp.wgl.msg_dc);
    if (!rc) {
        _sapp_fail("WGL: Failed to create dummy context\n");
    }
    if (!_sapp.wgl.MakeCurrent(_sapp.wgl.msg_dc, rc)) {
        _sapp_fail("WGL: Failed to make context current\n");
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
        _sapp_fail("WGL: Failed to retrieve pixel format attribute\n");
    }
    return value;
}

_SOKOL_PRIVATE int _sapp_wgl_find_pixel_format(void) {
    SOKOL_ASSERT(_sapp.win32.dc);
    SOKOL_ASSERT(_sapp.wgl.arb_pixel_format);
    const _sapp_gl_fbconfig* closest;

    int native_count = _sapp_wgl_attrib(1, WGL_NUMBER_PIXEL_FORMATS_ARB);
    _sapp_gl_fbconfig* usable_configs = (_sapp_gl_fbconfig*) SOKOL_CALLOC((size_t)native_count, sizeof(_sapp_gl_fbconfig));
    SOKOL_ASSERT(usable_configs);
    int usable_count = 0;
    for (int i = 0; i < native_count; i++) {
        const int n = i + 1;
        _sapp_gl_fbconfig* u = usable_configs + usable_count;
        _sapp_gl_init_fbconfig(u);
        if (!_sapp_wgl_attrib(n, WGL_SUPPORT_OPENGL_ARB) || !_sapp_wgl_attrib(n, WGL_DRAW_TO_WINDOW_ARB)) {
            continue;
        }
        if (_sapp_wgl_attrib(n, WGL_PIXEL_TYPE_ARB) != WGL_TYPE_RGBA_ARB) {
            continue;
        }
        if (_sapp_wgl_attrib(n, WGL_ACCELERATION_ARB) == WGL_NO_ACCELERATION_ARB) {
            continue;
        }
        u->red_bits     = _sapp_wgl_attrib(n, WGL_RED_BITS_ARB);
        u->green_bits   = _sapp_wgl_attrib(n, WGL_GREEN_BITS_ARB);
        u->blue_bits    = _sapp_wgl_attrib(n, WGL_BLUE_BITS_ARB);
        u->alpha_bits   = _sapp_wgl_attrib(n, WGL_ALPHA_BITS_ARB);
        u->depth_bits   = _sapp_wgl_attrib(n, WGL_DEPTH_BITS_ARB);
        u->stencil_bits = _sapp_wgl_attrib(n, WGL_STENCIL_BITS_ARB);
        if (_sapp_wgl_attrib(n, WGL_DOUBLE_BUFFER_ARB)) {
            u->doublebuffer = true;
        }
        if (_sapp.wgl.arb_multisample) {
            u->samples = _sapp_wgl_attrib(n, WGL_SAMPLES_ARB);
        }
        u->handle = (uintptr_t)n;
        usable_count++;
    }
    SOKOL_ASSERT(usable_count > 0);
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
    int pixel_format = 0;
    if (closest) {
        pixel_format = (int) closest->handle;
    }
    SOKOL_FREE(usable_configs);
    return pixel_format;
}

_SOKOL_PRIVATE void _sapp_wgl_create_context(void) {
    int pixel_format = _sapp_wgl_find_pixel_format();
    if (0 == pixel_format) {
        _sapp_fail("WGL: Didn't find matching pixel format.\n");
    }
    PIXELFORMATDESCRIPTOR pfd;
    if (!DescribePixelFormat(_sapp.win32.dc, pixel_format, sizeof(pfd), &pfd)) {
        _sapp_fail("WGL: Failed to retrieve PFD for selected pixel format!\n");
    }
    if (!SetPixelFormat(_sapp.win32.dc, pixel_format, &pfd)) {
        _sapp_fail("WGL: Failed to set selected pixel format!\n");
    }
    if (!_sapp.wgl.arb_create_context) {
        _sapp_fail("WGL: ARB_create_context required!\n");
    }
    if (!_sapp.wgl.arb_create_context_profile) {
        _sapp_fail("WGL: ARB_create_context_profile required!\n");
    }
    const int attrs[] = {
        WGL_CONTEXT_MAJOR_VERSION_ARB, 3,
        WGL_CONTEXT_MINOR_VERSION_ARB, 3,
        WGL_CONTEXT_FLAGS_ARB, WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB,
        WGL_CONTEXT_PROFILE_MASK_ARB, WGL_CONTEXT_CORE_PROFILE_BIT_ARB,
        0, 0
    };
    _sapp.wgl.gl_ctx = _sapp.wgl.CreateContextAttribsARB(_sapp.win32.dc, 0, attrs);
    if (!_sapp.wgl.gl_ctx) {
        const DWORD err = GetLastError();
        if (err == (0xc0070000 | ERROR_INVALID_VERSION_ARB)) {
            _sapp_fail("WGL: Driver does not support OpenGL version 3.3\n");
        }
        else if (err == (0xc0070000 | ERROR_INVALID_PROFILE_ARB)) {
            _sapp_fail("WGL: Driver does not support the requested OpenGL profile");
        }
        else if (err == (0xc0070000 | ERROR_INCOMPATIBLE_DEVICE_CONTEXTS_ARB)) {
            _sapp_fail("WGL: The share context is not compatible with the requested context");
        }
        else {
            _sapp_fail("WGL: Failed to create OpenGL context");
        }
    }
    _sapp.wgl.MakeCurrent(_sapp.win32.dc, _sapp.wgl.gl_ctx);
    if (_sapp.wgl.ext_swap_control) {
        /* FIXME: DwmIsCompositionEnabled() (see GLFW) */
        _sapp.wgl.SwapIntervalEXT(_sapp.swap_interval);
    }
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
#endif /* SOKOL_GLCORE33 */

_SOKOL_PRIVATE bool _sapp_win32_wide_to_utf8(const wchar_t* src, char* dst, int dst_num_bytes) {
    SOKOL_ASSERT(src && dst && (dst_num_bytes > 1));
    memset(dst, 0, (size_t)dst_num_bytes);
    const int bytes_needed = WideCharToMultiByte(CP_UTF8, 0, src, -1, NULL, 0, NULL, NULL);
    if (bytes_needed <= dst_num_bytes) {
        WideCharToMultiByte(CP_UTF8, 0, src, -1, dst, dst_num_bytes, NULL, NULL);
        return true;
    }
    else {
        return false;
    }
}

_SOKOL_PRIVATE void _sapp_win32_toggle_fullscreen(void) {
    HMONITOR monitor = MonitorFromWindow(_sapp.win32.hwnd, MONITOR_DEFAULTTONEAREST);
    MONITORINFO minfo;
    memset(&minfo, 0, sizeof(minfo));
    minfo.cbSize = sizeof(MONITORINFO);
    GetMonitorInfo(monitor, &minfo);
    const RECT mr = minfo.rcMonitor;
    const int monitor_w = mr.right - mr.left;
    const int monitor_h = mr.bottom - mr.top;

    const DWORD win_ex_style = WS_EX_APPWINDOW | WS_EX_WINDOWEDGE;
    DWORD win_style;
    RECT rect = { 0, 0, 0, 0 };

    _sapp.fullscreen = !_sapp.fullscreen;
    if (!_sapp.fullscreen) {
        win_style = WS_CLIPSIBLINGS | WS_CLIPCHILDREN | WS_CAPTION | WS_SYSMENU | WS_MINIMIZEBOX | WS_MAXIMIZEBOX | WS_SIZEBOX;
        rect.right = (int) ((float)_sapp.desc.width * _sapp.win32.dpi.window_scale);
        rect.bottom = (int) ((float)_sapp.desc.height * _sapp.win32.dpi.window_scale);
    }
    else {
        win_style = WS_POPUP | WS_SYSMENU | WS_VISIBLE;
        rect.right = monitor_w;
        rect.bottom = monitor_h;
    }
    AdjustWindowRectEx(&rect, win_style, FALSE, win_ex_style);
    int win_width = rect.right - rect.left;
    int win_height = rect.bottom - rect.top;
    if (!_sapp.fullscreen) {
        rect.left = (monitor_w - win_width) / 2;
        rect.top = (monitor_h - win_height) / 2;
    }

    SetWindowLongPtr(_sapp.win32.hwnd, GWL_STYLE, win_style);
    SetWindowPos(_sapp.win32.hwnd, HWND_TOP, mr.left + rect.left, mr.top + rect.top, win_width, win_height, SWP_SHOWWINDOW | SWP_FRAMECHANGED);
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
    return PtInRect(&area, pos);
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
           (so that we dont miss any mouse up events)
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
            SOKOL_LOG("RegisterRawInputDevices() failed (on mouse lock).\n");
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
            SOKOL_LOG("RegisterRawInputDevices() failed (on mouse unlock).\n");
        }

        /* let the mouse roam freely again */
        ClipCursor(NULL);
        ShowCursor(TRUE);

        /* restore the 'pre-locked' mouse position */
        BOOL res = SetCursorPos(_sapp.win32.mouse_locked_x, _sapp.win32.mouse_locked_y);
        SOKOL_ASSERT(res); _SOKOL_UNUSED(res);
    }
}

/* updates current window and framebuffer size from the window's client rect, returns true if size has changed */
_SOKOL_PRIVATE bool _sapp_win32_update_dimensions(void) {
    RECT rect;
    if (GetClientRect(_sapp.win32.hwnd, &rect)) {
        _sapp.window_width = (int)((float)(rect.right - rect.left) / _sapp.win32.dpi.window_scale);
        _sapp.window_height = (int)((float)(rect.bottom - rect.top) / _sapp.win32.dpi.window_scale);
        int fb_width = (int)((float)_sapp.window_width * _sapp.win32.dpi.content_scale);
        int fb_height = (int)((float)_sapp.window_height * _sapp.win32.dpi.content_scale);
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
        WCHAR* buffer = (WCHAR*) SOKOL_CALLOC(num_chars, sizeof(WCHAR));
        DragQueryFileW(hdrop, i, buffer, num_chars);
        if (!_sapp_win32_wide_to_utf8(buffer, _sapp_dropped_file_path_ptr((int)i), _sapp.drop.max_path_length)) {
            SOKOL_LOG("sokol_app.h: dropped file path too long (sapp_desc.max_dropped_file_path_length)\n");
            drop_failed = true;
        }
        SOKOL_FREE(buffer);
    }
    DragFinish(hdrop);
    if (!drop_failed) {
        if (_sapp_events_enabled()) {
            _sapp_init_event(SAPP_EVENTTYPE_FILES_DROPPED);
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
        DXGI_FRAME_STATISTICS dxgi_stats;
        _SAPP_CLEAR(DXGI_FRAME_STATISTICS, dxgi_stats);
        HRESULT hr = _sapp_dxgi_GetFrameStatistics(_sapp.d3d11.swap_chain, &dxgi_stats);
        if (SUCCEEDED(hr)) {
            if (dxgi_stats.SyncRefreshCount != _sapp.d3d11.sync_refresh_count) {
                if ((_sapp.d3d11.sync_refresh_count + 1) != dxgi_stats.SyncRefreshCount) {
                    _sapp_timing_external_reset(&_sapp.timing);
                }
                _sapp.d3d11.sync_refresh_count = dxgi_stats.SyncRefreshCount;
                LARGE_INTEGER qpc = dxgi_stats.SyncQPCTime;
                const uint64_t now = (uint64_t)_sapp_int64_muldiv(qpc.QuadPart - _sapp.timing.timestamp.win.start.QuadPart, 1000000000, _sapp.timing.timestamp.win.freq.QuadPart);
                _sapp_timing_external(&_sapp.timing, (double)now / 1000000000.0);
            }
        }
        else {
            // fallback if GetFrameStats doesn't work for some reason
            _sapp_timing_measure(&_sapp.timing);
        }
    #endif
    #if defined(SOKOL_GLCORE33)
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
                    _sapp_win32_uwp_app_event(SAPP_EVENTTYPE_QUIT_REQUESTED);
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
                            _sapp_win32_uwp_app_event(SAPP_EVENTTYPE_ICONIFIED);
                        }
                        else {
                            _sapp_win32_uwp_app_event(SAPP_EVENTTYPE_RESTORED);
                        }
                    }
                }
                break;
            case WM_SETFOCUS:
                _sapp_win32_uwp_app_event(SAPP_EVENTTYPE_FOCUSED);
                break;
            case WM_KILLFOCUS:
                /* if focus is lost for any reason, and we're in mouse locked mode, disable mouse lock */
                if (_sapp.mouse.locked) {
                    _sapp_win32_lock_mouse(false);
                }
                _sapp_win32_uwp_app_event(SAPP_EVENTTYPE_UNFOCUSED);
                break;
            case WM_SETCURSOR:
                if (LOWORD(lParam) == HTCLIENT) {
                    _sapp_win32_update_cursor(_sapp.mouse.current_cursor, _sapp.mouse.shown, true);
                    return TRUE;
                }
                break;
            case WM_LBUTTONDOWN:
                _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_DOWN, SAPP_MOUSEBUTTON_LEFT);
                _sapp_win32_capture_mouse(1<<SAPP_MOUSEBUTTON_LEFT);
                break;
            case WM_RBUTTONDOWN:
                _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_DOWN, SAPP_MOUSEBUTTON_RIGHT);
                _sapp_win32_capture_mouse(1<<SAPP_MOUSEBUTTON_RIGHT);
                break;
            case WM_MBUTTONDOWN:
                _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_DOWN, SAPP_MOUSEBUTTON_MIDDLE);
                _sapp_win32_capture_mouse(1<<SAPP_MOUSEBUTTON_MIDDLE);
                break;
            case WM_LBUTTONUP:
                _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_UP, SAPP_MOUSEBUTTON_LEFT);
                _sapp_win32_release_mouse(1<<SAPP_MOUSEBUTTON_LEFT);
                break;
            case WM_RBUTTONUP:
                _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_UP, SAPP_MOUSEBUTTON_RIGHT);
                _sapp_win32_release_mouse(1<<SAPP_MOUSEBUTTON_RIGHT);
                break;
            case WM_MBUTTONUP:
                _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_UP, SAPP_MOUSEBUTTON_MIDDLE);
                _sapp_win32_release_mouse(1<<SAPP_MOUSEBUTTON_MIDDLE);
                break;
            case WM_MOUSEMOVE:
                if (!_sapp.mouse.locked) {
                    const float new_x  = (float)GET_X_LPARAM(lParam) * _sapp.win32.dpi.mouse_scale;
                    const float new_y = (float)GET_Y_LPARAM(lParam) * _sapp.win32.dpi.mouse_scale;
                    /* don't update dx/dy in the very first event */
                    if (_sapp.mouse.pos_valid) {
                        _sapp.mouse.dx = new_x - _sapp.mouse.x;
                        _sapp.mouse.dy = new_y - _sapp.mouse.y;
                    }
                    _sapp.mouse.x = new_x;
                    _sapp.mouse.y = new_y;
                    _sapp.mouse.pos_valid = true;
                    if (!_sapp.win32.mouse_tracked) {
                        _sapp.win32.mouse_tracked = true;
                        TRACKMOUSEEVENT tme;
                        memset(&tme, 0, sizeof(tme));
                        tme.cbSize = sizeof(tme);
                        tme.dwFlags = TME_LEAVE;
                        tme.hwndTrack = _sapp.win32.hwnd;
                        TrackMouseEvent(&tme);
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
                        SOKOL_LOG("GetRawInputData() failed\n");
                        break;
                    }
                    const RAWINPUT* raw_mouse_data = (const RAWINPUT*) &_sapp.win32.raw_input_data;
                    if (raw_mouse_data->data.mouse.usFlags & MOUSE_MOVE_ABSOLUTE) {
                        /* mouse only reports absolute position
                           NOTE: THIS IS UNTESTED, it's unclear from reading the
                           Win32 RawInput docs under which circumstances absolute
                           positions are sent.
                        */
                        if (_sapp.win32.raw_input_mousepos_valid) {
                            LONG new_x = raw_mouse_data->data.mouse.lLastX;
                            LONG new_y = raw_mouse_data->data.mouse.lLastY;
                            _sapp.mouse.dx = (float) (new_x - _sapp.win32.raw_input_mousepos_x);
                            _sapp.mouse.dy = (float) (new_y - _sapp.win32.raw_input_mousepos_y);
                            _sapp.win32.raw_input_mousepos_x = new_x;
                            _sapp.win32.raw_input_mousepos_y = new_y;
                            _sapp.win32.raw_input_mousepos_valid = true;
                        }
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
                    _sapp_d3d11_present();
                #endif
                #if defined(SOKOL_GLCORE33)
                    _sapp_wgl_swap_buffers();
                #endif
                /* NOTE: resizing the swap-chain during resize leads to a substantial
                   memory spike (hundreds of megabytes for a few seconds).

                if (_sapp_win32_update_dimensions()) {
                    #if defined(SOKOL_D3D11)
                    _sapp_d3d11_resize_default_render_target();
                    #endif
                    _sapp_win32_uwp_app_event(SAPP_EVENTTYPE_RESIZED);
                }
                */
                break;
            case WM_DROPFILES:
                _sapp_win32_files_dropped((HDROP)wParam);
                break;
            default:
                break;
        }
    }
    return DefWindowProcW(hWnd, uMsg, wParam, lParam);
}

_SOKOL_PRIVATE void _sapp_win32_create_window(void) {
    WNDCLASSW wndclassw;
    memset(&wndclassw, 0, sizeof(wndclassw));
    wndclassw.style = CS_HREDRAW | CS_VREDRAW | CS_OWNDC;
    wndclassw.lpfnWndProc = (WNDPROC) _sapp_win32_wndproc;
    wndclassw.hInstance = GetModuleHandleW(NULL);
    wndclassw.hCursor = LoadCursor(NULL, IDC_ARROW);
    wndclassw.hIcon = LoadIcon(NULL, IDI_WINLOGO);
    wndclassw.lpszClassName = L"SOKOLAPP";
    RegisterClassW(&wndclassw);

    DWORD win_style;
    const DWORD win_ex_style = WS_EX_APPWINDOW | WS_EX_WINDOWEDGE;
    RECT rect = { 0, 0, 0, 0 };
    if (_sapp.fullscreen) {
        win_style = WS_POPUP | WS_SYSMENU | WS_VISIBLE;
        rect.right = GetSystemMetrics(SM_CXSCREEN);
        rect.bottom = GetSystemMetrics(SM_CYSCREEN);
    }
    else {
        win_style = WS_CLIPSIBLINGS | WS_CLIPCHILDREN | WS_CAPTION | WS_SYSMENU | WS_MINIMIZEBOX | WS_MAXIMIZEBOX | WS_SIZEBOX;
        rect.right = (int) ((float)_sapp.window_width * _sapp.win32.dpi.window_scale);
        rect.bottom = (int) ((float)_sapp.window_height * _sapp.win32.dpi.window_scale);
    }
    AdjustWindowRectEx(&rect, win_style, FALSE, win_ex_style);
    const int win_width = rect.right - rect.left;
    const int win_height = rect.bottom - rect.top;
    _sapp.win32.in_create_window = true;
    _sapp.win32.hwnd = CreateWindowExW(
        win_ex_style,               /* dwExStyle */
        L"SOKOLAPP",                /* lpClassName */
        _sapp.window_title_wide,    /* lpWindowName */
        win_style,                  /* dwStyle */
        CW_USEDEFAULT,              /* X */
        CW_USEDEFAULT,              /* Y */
        win_width,                  /* nWidth */
        win_height,                 /* nHeight */
        NULL,                       /* hWndParent */
        NULL,                       /* hMenu */
        GetModuleHandle(NULL),      /* hInstance */
        NULL);                      /* lParam */
    ShowWindow(_sapp.win32.hwnd, SW_SHOW);
    _sapp.win32.in_create_window = false;
    _sapp.win32.dc = GetDC(_sapp.win32.hwnd);
    SOKOL_ASSERT(_sapp.win32.dc);
    _sapp_win32_update_dimensions();

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

    typedef BOOL(WINAPI * SETPROCESSDPIAWARE_T)(void);
    typedef HRESULT(WINAPI * SETPROCESSDPIAWARENESS_T)(PROCESS_DPI_AWARENESS);
    typedef HRESULT(WINAPI * GETDPIFORMONITOR_T)(HMONITOR, MONITOR_DPI_TYPE, UINT*, UINT*);

    SETPROCESSDPIAWARE_T fn_setprocessdpiaware = 0;
    SETPROCESSDPIAWARENESS_T fn_setprocessdpiawareness = 0;
    GETDPIFORMONITOR_T fn_getdpiformonitor = 0;
    HINSTANCE user32 = LoadLibraryA("user32.dll");
    if (user32) {
        fn_setprocessdpiaware = (SETPROCESSDPIAWARE_T)(void*) GetProcAddress(user32, "SetProcessDPIAware");
    }
    HINSTANCE shcore = LoadLibraryA("shcore.dll");
    if (shcore) {
        fn_setprocessdpiawareness = (SETPROCESSDPIAWARENESS_T)(void*) GetProcAddress(shcore, "SetProcessDpiAwareness");
        fn_getdpiformonitor = (GETDPIFORMONITOR_T)(void*) GetProcAddress(shcore, "GetDpiForMonitor");
    }
    if (fn_setprocessdpiawareness) {
        /* if the app didn't request HighDPI rendering, let Windows do the upscaling */
        PROCESS_DPI_AWARENESS process_dpi_awareness = PROCESS_SYSTEM_DPI_AWARE;
        _sapp.win32.dpi.aware = true;
        if (!_sapp.desc.high_dpi) {
            process_dpi_awareness = PROCESS_DPI_UNAWARE;
            _sapp.win32.dpi.aware = false;
        }
        fn_setprocessdpiawareness(process_dpi_awareness);
    }
    else if (fn_setprocessdpiaware) {
        fn_setprocessdpiaware();
        _sapp.win32.dpi.aware = true;
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

    wchar_t* wchar_buf = 0;
    const SIZE_T wchar_buf_size = (SIZE_T)_sapp.clipboard.buf_size * sizeof(wchar_t);
    HANDLE object = GlobalAlloc(GMEM_MOVEABLE, wchar_buf_size);
    if (!object) {
        goto error;
    }
    wchar_buf = (wchar_t*) GlobalLock(object);
    if (!wchar_buf) {
        goto error;
    }
    if (!_sapp_win32_uwp_utf8_to_wide(str, wchar_buf, (int)wchar_buf_size)) {
        goto error;
    }
    GlobalUnlock(wchar_buf);
    wchar_buf = 0;
    if (!OpenClipboard(_sapp.win32.hwnd)) {
        goto error;
    }
    EmptyClipboard();
    SetClipboardData(CF_UNICODETEXT, object);
    CloseClipboard();
    return true;

error:
    if (wchar_buf) {
        GlobalUnlock(object);
    }
    if (object) {
        GlobalFree(object);
    }
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
        SOKOL_LOG("sokol_app.h: clipboard string didn't fit into clipboard buffer\n");
    }
    GlobalUnlock(object);
    CloseClipboard();
    return _sapp.clipboard.buffer;
}

_SOKOL_PRIVATE void _sapp_win32_update_window_title(void) {
    _sapp_win32_uwp_utf8_to_wide(_sapp.window_title, _sapp.window_title_wide, sizeof(_sapp.window_title_wide));
    SetWindowTextW(_sapp.win32.hwnd, _sapp.window_title_wide);
}

_SOKOL_PRIVATE HICON _sapp_win32_create_icon_from_image(const sapp_image_desc* desc) {
    BITMAPV5HEADER bi;
    memset(&bi, 0, sizeof(bi));
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
    memset(&icon_info, 0, sizeof(icon_info));
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
    _sapp_win32_uwp_init_keytable();
    _sapp_win32_uwp_utf8_to_wide(_sapp.window_title, _sapp.window_title_wide, sizeof(_sapp.window_title_wide));
    _sapp_win32_init_dpi();
    _sapp_win32_init_cursors();
    _sapp_win32_create_window();
    sapp_set_icon(&desc->icon);
    #if defined(SOKOL_D3D11)
        _sapp_d3d11_create_device_and_swapchain();
        _sapp_d3d11_create_default_render_target();
    #endif
    #if defined(SOKOL_GLCORE33)
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
                DispatchMessage(&msg);
            }
        }
        _sapp_frame();
        #if defined(SOKOL_D3D11)
            _sapp_d3d11_present();
            if (IsIconic(_sapp.win32.hwnd)) {
                Sleep((DWORD)(16 * _sapp.swap_interval));
            }
        #endif
        #if defined(SOKOL_GLCORE33)
            _sapp_wgl_swap_buffers();
        #endif
        /* check for window resized, this cannot happen in WM_SIZE as it explodes memory usage */
        if (_sapp_win32_update_dimensions()) {
            #if defined(SOKOL_D3D11)
            _sapp_d3d11_resize_default_render_target();
            #endif
            _sapp_win32_uwp_app_event(SAPP_EVENTTYPE_RESIZED);
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
        _sapp_fail("Win32: failed to parse command line");
    } else {
        size_t size = wcslen(w_command_line) * 4;
        argv = (char**) SOKOL_CALLOC(1, ((size_t)argc + 1) * sizeof(char*) + size);
        SOKOL_ASSERT(argv);
        args = (char*) &argv[argc + 1];
        int n;
        for (int i = 0; i < argc; ++i) {
            n = WideCharToMultiByte(CP_UTF8, 0, w_argv[i], -1, args, (int)size, NULL, NULL);
            if (n == 0) {
                _sapp_fail("Win32: failed to convert all arguments to utf8");
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
    SOKOL_FREE(argv_utf8);
    return 0;
}
#endif /* SOKOL_WIN32_FORCE_MAIN */
#endif /* SOKOL_NO_ENTRY */

#ifdef _MSC_VER
    #pragma warning(pop)
#endif

#endif /* _SAPP_WIN32 */

/*== UWP ================================================================*/
#if defined(_SAPP_UWP)

// Helper functions
_SOKOL_PRIVATE void _sapp_uwp_configure_dpi(float monitor_dpi) {
    _sapp.uwp.dpi.window_scale = monitor_dpi / 96.0f;
    if (_sapp.desc.high_dpi) {
        _sapp.uwp.dpi.content_scale = _sapp.uwp.dpi.window_scale;
        _sapp.uwp.dpi.mouse_scale = 1.0f * _sapp.uwp.dpi.window_scale;
    }
    else {
        _sapp.uwp.dpi.content_scale = 1.0f;
        _sapp.uwp.dpi.mouse_scale = 1.0f;
    }
    _sapp.dpi_scale = _sapp.uwp.dpi.content_scale;
}

_SOKOL_PRIVATE void _sapp_uwp_update_cursor(sapp_mouse_cursor cursor, bool shown) {
    using namespace winrt::Windows::UI::Core;

    CoreCursor uwp_cursor(nullptr);
    if (shown) {
        switch (cursor) {
            case SAPP_MOUSECURSOR_ARROW: uwp_cursor = CoreCursor(CoreCursorType::Arrow, 0); break;
            case SAPP_MOUSECURSOR_IBEAM: uwp_cursor = CoreCursor(CoreCursorType::IBeam, 0); break;
            case SAPP_MOUSECURSOR_CROSSHAIR: uwp_cursor = CoreCursor(CoreCursorType::Cross, 0); break;
            case SAPP_MOUSECURSOR_POINTING_HAND: uwp_cursor = CoreCursor(CoreCursorType::Hand, 0); break;
            case SAPP_MOUSECURSOR_RESIZE_EW: uwp_cursor = CoreCursor(CoreCursorType::SizeWestEast, 0); break;
            case SAPP_MOUSECURSOR_RESIZE_NS: uwp_cursor = CoreCursor(CoreCursorType::SizeNorthSouth, 0); break;
            case SAPP_MOUSECURSOR_RESIZE_NWSE: uwp_cursor = CoreCursor(CoreCursorType::SizeNorthwestSoutheast, 0); break;
            case SAPP_MOUSECURSOR_RESIZE_NESW: uwp_cursor = CoreCursor(CoreCursorType::SizeNortheastSouthwest, 0); break;
            case SAPP_MOUSECURSOR_RESIZE_ALL: uwp_cursor = CoreCursor(CoreCursorType::SizeAll, 0); break;
            case SAPP_MOUSECURSOR_NOT_ALLOWED: uwp_cursor = CoreCursor(CoreCursorType::UniversalNo, 0); break;
            default: uwp_cursor = CoreCursor(CoreCursorType::Arrow, 0); break;
        }
    }
    CoreWindow::GetForCurrentThread().PointerCursor(uwp_cursor);
}

_SOKOL_PRIVATE uint32_t _sapp_uwp_mods(winrt::Windows::UI::Core::CoreWindow const& sender_window) {
    using namespace winrt::Windows::System;
    using namespace winrt::Windows::UI::Core;

    uint32_t mods = 0;
    if ((sender_window.GetKeyState(VirtualKey::Shift) & CoreVirtualKeyStates::Down) == CoreVirtualKeyStates::Down) {
        mods |= SAPP_MODIFIER_SHIFT;
    }
    if ((sender_window.GetKeyState(VirtualKey::Control) & CoreVirtualKeyStates::Down) == CoreVirtualKeyStates::Down) {
        mods |= SAPP_MODIFIER_CTRL;
    }
    if ((sender_window.GetKeyState(VirtualKey::Menu) & CoreVirtualKeyStates::Down) == CoreVirtualKeyStates::Down) {
        mods |= SAPP_MODIFIER_ALT;
    }
    if (((sender_window.GetKeyState(VirtualKey::LeftWindows) & CoreVirtualKeyStates::Down) == CoreVirtualKeyStates::Down) ||
        ((sender_window.GetKeyState(VirtualKey::RightWindows) & CoreVirtualKeyStates::Down) == CoreVirtualKeyStates::Down))
    {
        mods |= SAPP_MODIFIER_SUPER;
    }
    if (0 != (_sapp.uwp.mouse_buttons & (1<<SAPP_MOUSEBUTTON_LEFT))) {
        mods |= SAPP_MODIFIER_LMB;
    }
    if (0 != (_sapp.uwp.mouse_buttons & (1<<SAPP_MOUSEBUTTON_MIDDLE))) {
        mods |= SAPP_MODIFIER_MMB;
    }
    if (0 != (_sapp.uwp.mouse_buttons & (1<<SAPP_MOUSEBUTTON_RIGHT))) {
        mods |= SAPP_MODIFIER_RMB;
    }
    return mods;
}

_SOKOL_PRIVATE void _sapp_uwp_mouse_event(sapp_event_type type, sapp_mousebutton btn, winrt::Windows::UI::Core::CoreWindow const& sender_window) {
    if (_sapp_events_enabled()) {
        _sapp_init_event(type);
        _sapp.event.modifiers = _sapp_uwp_mods(sender_window);
        _sapp.event.mouse_button = btn;
        _sapp_call_event(&_sapp.event);
    }
}

_SOKOL_PRIVATE void _sapp_uwp_scroll_event(float delta, bool horizontal, winrt::Windows::UI::Core::CoreWindow const& sender_window) {
    if (_sapp_events_enabled()) {
        _sapp_init_event(SAPP_EVENTTYPE_MOUSE_SCROLL);
        _sapp.event.modifiers = _sapp_uwp_mods(sender_window);
        _sapp.event.scroll_x = horizontal ? (-delta / 30.0f) : 0.0f;
        _sapp.event.scroll_y = horizontal ? 0.0f : (delta / 30.0f);
        _sapp_call_event(&_sapp.event);
    }
}

_SOKOL_PRIVATE void _sapp_uwp_extract_mouse_button_events(winrt::Windows::UI::Core::CoreWindow const& sender, winrt::Windows::UI::Core::PointerEventArgs const& args) {

    // we need to figure out ourselves what mouse buttons have been pressed and released,
    // because UWP doesn't properly send down/up mouse button events when multiple buttons
    // are pressed down, so we also need to check the mouse button state in other mouse events
    // to track what buttons have been pressed down and released
    //
    auto properties = args.CurrentPoint().Properties();
    const uint8_t lmb_bit = (1 << SAPP_MOUSEBUTTON_LEFT);
    const uint8_t rmb_bit = (1 << SAPP_MOUSEBUTTON_RIGHT);
    const uint8_t mmb_bit = (1 << SAPP_MOUSEBUTTON_MIDDLE);
    uint8_t new_btns = 0;
    if (properties.IsLeftButtonPressed()) {
        new_btns |= lmb_bit;
    }
    if (properties.IsRightButtonPressed()) {
        new_btns |= rmb_bit;
    }
    if (properties.IsMiddleButtonPressed()) {
        new_btns |= mmb_bit;
    }
    const uint8_t old_btns = _sapp.uwp.mouse_buttons;
    const uint8_t chg_btns = new_btns ^ old_btns;

    _sapp.uwp.mouse_buttons = new_btns;

    sapp_event_type type = SAPP_EVENTTYPE_INVALID;
    sapp_mousebutton btn = SAPP_MOUSEBUTTON_INVALID;
    if (chg_btns & lmb_bit) {
        btn = SAPP_MOUSEBUTTON_LEFT;
        type = (new_btns & lmb_bit) ? SAPP_EVENTTYPE_MOUSE_DOWN : SAPP_EVENTTYPE_MOUSE_UP;
    }
    if (chg_btns & rmb_bit) {
        btn = SAPP_MOUSEBUTTON_RIGHT;
        type = (new_btns & rmb_bit) ? SAPP_EVENTTYPE_MOUSE_DOWN : SAPP_EVENTTYPE_MOUSE_UP;
    }
    if (chg_btns & mmb_bit) {
        btn = SAPP_MOUSEBUTTON_MIDDLE;
        type = (new_btns & mmb_bit) ? SAPP_EVENTTYPE_MOUSE_DOWN : SAPP_EVENTTYPE_MOUSE_UP;
    }
    if (type != SAPP_EVENTTYPE_INVALID) {
        _sapp_uwp_mouse_event(type, btn, sender);
    }
}

_SOKOL_PRIVATE void _sapp_uwp_key_event(sapp_event_type type, winrt::Windows::UI::Core::CoreWindow const& sender_window, winrt::Windows::UI::Core::KeyEventArgs const& args) {
    auto key_status = args.KeyStatus();
    uint32_t ext_scan_code = key_status.ScanCode | (key_status.IsExtendedKey ? 0x100 : 0);
    if (_sapp_events_enabled() && (ext_scan_code < SAPP_MAX_KEYCODES)) {
        _sapp_init_event(type);
        _sapp.event.modifiers = _sapp_uwp_mods(sender_window);
        _sapp.event.key_code = _sapp.keycodes[ext_scan_code];
        _sapp.event.key_repeat = type == SAPP_EVENTTYPE_KEY_UP ? false : key_status.WasKeyDown;
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

_SOKOL_PRIVATE void _sapp_uwp_char_event(uint32_t c, bool repeat, winrt::Windows::UI::Core::CoreWindow const& sender_window) {
    if (_sapp_events_enabled() && (c >= 32)) {
        _sapp_init_event(SAPP_EVENTTYPE_CHAR);
        _sapp.event.modifiers = _sapp_uwp_mods(sender_window);
        _sapp.event.char_code = c;
        _sapp.event.key_repeat = repeat;
        _sapp_call_event(&_sapp.event);
    }
}

_SOKOL_PRIVATE void _sapp_uwp_toggle_fullscreen(void) {
    auto appView = winrt::Windows::UI::ViewManagement::ApplicationView::GetForCurrentView();
    _sapp.fullscreen = appView.IsFullScreenMode();
    if (!_sapp.fullscreen) {
        appView.TryEnterFullScreenMode();
    }
    else {
        appView.ExitFullScreenMode();
    }
    _sapp.fullscreen = appView.IsFullScreenMode();
}

namespace {/* Empty namespace to ensure internal linkage (same as _SOKOL_PRIVATE) */

// Controls all the DirectX device resources.
class DeviceResources {
public:
    // Provides an interface for an application that owns DeviceResources to be notified of the device being lost or created.
    interface IDeviceNotify {
        virtual void OnDeviceLost() = 0;
        virtual void OnDeviceRestored() = 0;
    };

    DeviceResources();
    ~DeviceResources();
    void SetWindow(winrt::Windows::UI::Core::CoreWindow const& window);
    void SetLogicalSize(winrt::Windows::Foundation::Size logicalSize);
    void SetCurrentOrientation(winrt::Windows::Graphics::Display::DisplayOrientations currentOrientation);
    void SetDpi(float dpi);
    void ValidateDevice();
    void HandleDeviceLost();
    void RegisterDeviceNotify(IDeviceNotify* deviceNotify);
    void Trim();
    void Present();

private:

    // Swapchain Rotation Matrices (Z-rotation)
    static inline const DirectX::XMFLOAT4X4 DeviceResources::m_rotation0 = {
        1.0f, 0.0f, 0.0f, 0.0f,
        0.0f, 1.0f, 0.0f, 0.0f,
        0.0f, 0.0f, 1.0f, 0.0f,
        0.0f, 0.0f, 0.0f, 1.0f
    };
    static inline const DirectX::XMFLOAT4X4 DeviceResources::m_rotation90 = {
        0.0f, 1.0f, 0.0f, 0.0f,
        -1.0f, 0.0f, 0.0f, 0.0f,
        0.0f, 0.0f, 1.0f, 0.0f,
        0.0f, 0.0f, 0.0f, 1.0f
    };
    static inline const DirectX::XMFLOAT4X4 DeviceResources::m_rotation180 = {
        -1.0f, 0.0f, 0.0f, 0.0f,
        0.0f, -1.0f, 0.0f, 0.0f,
        0.0f, 0.0f, 1.0f, 0.0f,
        0.0f, 0.0f, 0.0f, 1.0f
    };
    static inline const DirectX::XMFLOAT4X4 DeviceResources::m_rotation270 = {
        0.0f, -1.0f, 0.0f, 0.0f,
        1.0f, 0.0f, 0.0f, 0.0f,
        0.0f, 0.0f, 1.0f, 0.0f,
        0.0f, 0.0f, 0.0f, 1.0f
    };

    void CreateDeviceResources();
    void CreateWindowSizeDependentResources();
    void UpdateRenderTargetSize();
    DXGI_MODE_ROTATION ComputeDisplayRotation();
    bool SdkLayersAvailable();

    // Direct3D objects.
    winrt::com_ptr<ID3D11Device3> m_d3dDevice;
    winrt::com_ptr<ID3D11DeviceContext3> m_d3dContext;
    winrt::com_ptr<IDXGISwapChain3> m_swapChain;

    // Direct3D rendering objects. Required for 3D.
    winrt::com_ptr<ID3D11Texture2D1> m_d3dRenderTarget;
    winrt::com_ptr<ID3D11RenderTargetView1> m_d3dRenderTargetView;
    winrt::com_ptr<ID3D11Texture2D1> m_d3dMSAARenderTarget;
    winrt::com_ptr<ID3D11RenderTargetView1> m_d3dMSAARenderTargetView;
    winrt::com_ptr<ID3D11Texture2D1> m_d3dDepthStencil;
    winrt::com_ptr<ID3D11DepthStencilView> m_d3dDepthStencilView;
    D3D11_VIEWPORT m_screenViewport = { };

    // Cached reference to the Window.
    winrt::agile_ref< winrt::Windows::UI::Core::CoreWindow> m_window;

    // Cached device properties.
    D3D_FEATURE_LEVEL m_d3dFeatureLevel = D3D_FEATURE_LEVEL_9_1;
    winrt::Windows::Foundation::Size m_d3dRenderTargetSize = { };
    winrt::Windows::Foundation::Size m_outputSize = { };
    winrt::Windows::Foundation::Size m_logicalSize = { };
    winrt::Windows::Graphics::Display::DisplayOrientations m_nativeOrientation = winrt::Windows::Graphics::Display::DisplayOrientations::None;
    winrt::Windows::Graphics::Display::DisplayOrientations m_currentOrientation = winrt::Windows::Graphics::Display::DisplayOrientations::None;
    float m_dpi = -1.0f;

    // Transforms used for display orientation.
    DirectX::XMFLOAT4X4 m_orientationTransform3D;

    // The IDeviceNotify can be held directly as it owns the DeviceResources.
    IDeviceNotify* m_deviceNotify = nullptr;
};

// Main entry point for our app. Connects the app with the Windows shell and handles application lifecycle events.
struct App : winrt::implements<App, winrt::Windows::ApplicationModel::Core::IFrameworkViewSource, winrt::Windows::ApplicationModel::Core::IFrameworkView> {
public:
    // IFrameworkViewSource Methods
    winrt::Windows::ApplicationModel::Core::IFrameworkView CreateView() { return *this; }

    // IFrameworkView Methods.
    virtual void Initialize(winrt::Windows::ApplicationModel::Core::CoreApplicationView const& applicationView);
    virtual void SetWindow(winrt::Windows::UI::Core::CoreWindow const& window);
    virtual void Load(winrt::hstring const& entryPoint);
    virtual void Run();
    virtual void Uninitialize();

protected:
    // Application lifecycle event handlers
    void OnActivated(winrt::Windows::ApplicationModel::Core::CoreApplicationView const& applicationView, winrt::Windows::ApplicationModel::Activation::IActivatedEventArgs const& args);
    void OnSuspending(winrt::Windows::Foundation::IInspectable const& sender, winrt::Windows::ApplicationModel::SuspendingEventArgs const& args);
    void OnResuming(winrt::Windows::Foundation::IInspectable const& sender, winrt::Windows::Foundation::IInspectable const& args);

    // Window event handlers
    void OnWindowSizeChanged(winrt::Windows::UI::Core::CoreWindow const& sender, winrt::Windows::UI::Core::WindowSizeChangedEventArgs const& args);
    void OnVisibilityChanged(winrt::Windows::UI::Core::CoreWindow const& sender, winrt::Windows::UI::Core::VisibilityChangedEventArgs const& args);

    // Navigation event handlers
    void OnBackRequested(winrt::Windows::Foundation::IInspectable const& sender, winrt::Windows::UI::Core::BackRequestedEventArgs const& args);

    // Input event handlers
    void OnKeyDown(winrt::Windows::UI::Core::CoreWindow const& sender, winrt::Windows::UI::Core::KeyEventArgs const& args);
    void OnKeyUp(winrt::Windows::UI::Core::CoreWindow const& sender, winrt::Windows::UI::Core::KeyEventArgs const& args);
    void OnCharacterReceived(winrt::Windows::UI::Core::CoreWindow const& sender, winrt::Windows::UI::Core::CharacterReceivedEventArgs const& args);

    // Pointer event handlers
    void OnPointerEntered(winrt::Windows::UI::Core::CoreWindow const& sender, winrt::Windows::UI::Core::PointerEventArgs const& args);
    void OnPointerExited(winrt::Windows::UI::Core::CoreWindow const& sender, winrt::Windows::UI::Core::PointerEventArgs const& args);
    void OnPointerPressed(winrt::Windows::UI::Core::CoreWindow const& sender, winrt::Windows::UI::Core::PointerEventArgs const& args);
    void OnPointerReleased(winrt::Windows::UI::Core::CoreWindow const& sender, winrt::Windows::UI::Core::PointerEventArgs const& args);
    void OnPointerMoved(winrt::Windows::UI::Core::CoreWindow const& sender, winrt::Windows::UI::Core::PointerEventArgs const& args);
    void OnPointerWheelChanged(winrt::Windows::UI::Core::CoreWindow const& sender, winrt::Windows::UI::Core::PointerEventArgs const& args);

    // DisplayInformation event handlers.
    void OnDpiChanged(winrt::Windows::Graphics::Display::DisplayInformation const& sender, winrt::Windows::Foundation::IInspectable const& args);
    void OnOrientationChanged(winrt::Windows::Graphics::Display::DisplayInformation const& sender, winrt::Windows::Foundation::IInspectable const& args);
    void OnDisplayContentsInvalidated(winrt::Windows::Graphics::Display::DisplayInformation const& sender, winrt::Windows::Foundation::IInspectable const& args);

private:
    std::unique_ptr<DeviceResources> m_deviceResources;
    bool m_windowVisible = true;
};

DeviceResources::DeviceResources() {
    CreateDeviceResources();
}

DeviceResources::~DeviceResources() {
    // Cleanup Sokol Context
    _sapp.d3d11.device = nullptr;
    _sapp.d3d11.device_context = nullptr;
}

void DeviceResources::CreateDeviceResources() {
    // This flag adds support for surfaces with a different color channel ordering
    // than the API default. It is required for compatibility with Direct2D.
    UINT creationFlags = D3D11_CREATE_DEVICE_BGRA_SUPPORT;

    #if defined(_DEBUG)
    if (SdkLayersAvailable()) {
        // If the project is in a debug build, enable debugging via SDK Layers with this flag.
        creationFlags |= D3D11_CREATE_DEVICE_DEBUG;
    }
    #endif

    // This array defines the set of DirectX hardware feature levels this app will support.
    // Note the ordering should be preserved.
    // Don't forget to declare your application's minimum required feature level in its
    // description.  All applications are assumed to support 9.1 unless otherwise stated.
    D3D_FEATURE_LEVEL featureLevels[] = {
        D3D_FEATURE_LEVEL_12_1,
        D3D_FEATURE_LEVEL_12_0,
        D3D_FEATURE_LEVEL_11_1,
        D3D_FEATURE_LEVEL_11_0,
        D3D_FEATURE_LEVEL_10_1,
        D3D_FEATURE_LEVEL_10_0,
        D3D_FEATURE_LEVEL_9_3,
        D3D_FEATURE_LEVEL_9_2,
        D3D_FEATURE_LEVEL_9_1
    };

    // Create the Direct3D 11 API device object and a corresponding context.
    winrt::com_ptr<ID3D11Device> device;
    winrt::com_ptr<ID3D11DeviceContext> context;

    HRESULT hr = D3D11CreateDevice(
        nullptr,                    // Specify nullptr to use the default adapter.
        D3D_DRIVER_TYPE_HARDWARE,   // Create a device using the hardware graphics driver.
        0,                          // Should be 0 unless the driver is D3D_DRIVER_TYPE_SOFTWARE.
        creationFlags,              // Set debug and Direct2D compatibility flags.
        featureLevels,              // List of feature levels this app can support.
        ARRAYSIZE(featureLevels),   // Size of the list above.
        D3D11_SDK_VERSION,          // Always set this to D3D11_SDK_VERSION for Microsoft Store apps.
        device.put(),               // Returns the Direct3D device created.
        &m_d3dFeatureLevel,         // Returns feature level of device created.
        context.put()               // Returns the device immediate context.
    );

    if (FAILED(hr)) {
        // If the initialization fails, fall back to the WARP device.
        // For more information on WARP, see:
        // https://go.microsoft.com/fwlink/?LinkId=286690
        winrt::check_hresult(
            D3D11CreateDevice(
                nullptr,
                D3D_DRIVER_TYPE_WARP, // Create a WARP device instead of a hardware device.
                0,
                creationFlags,
                featureLevels,
                ARRAYSIZE(featureLevels),
                D3D11_SDK_VERSION,
                device.put(),
                &m_d3dFeatureLevel,
                context.put()
            )
        );
    }

    // Store pointers to the Direct3D 11.3 API device and immediate context.
    m_d3dDevice = device.as<ID3D11Device3>();
    m_d3dContext = context.as<ID3D11DeviceContext3>();

    // Setup Sokol Context
    _sapp.d3d11.device = m_d3dDevice.get();
    _sapp.d3d11.device_context = m_d3dContext.get();
}

void DeviceResources::CreateWindowSizeDependentResources() {
    // Cleanup Sokol Context (these are non-owning raw pointers)
    _sapp.d3d11.rt = nullptr;
    _sapp.d3d11.rtv = nullptr;
    _sapp.d3d11.msaa_rt = nullptr;
    _sapp.d3d11.msaa_rtv = nullptr;
    _sapp.d3d11.ds = nullptr;
    _sapp.d3d11.dsv = nullptr;

    // Clear the previous window size specific context.
    ID3D11RenderTargetView* nullViews[] = { nullptr };
    m_d3dContext->OMSetRenderTargets(ARRAYSIZE(nullViews), nullViews, nullptr);
    // these are smart pointers, setting to nullptr will delete the objects
    m_d3dRenderTarget = nullptr;
    m_d3dRenderTargetView = nullptr;
    m_d3dMSAARenderTarget = nullptr;
    m_d3dMSAARenderTargetView = nullptr;
    m_d3dDepthStencilView = nullptr;
    m_d3dDepthStencil = nullptr;
    m_d3dContext->Flush1(D3D11_CONTEXT_TYPE_ALL, nullptr);

    UpdateRenderTargetSize();

    // The width and height of the swap chain must be based on the window's
    // natively-oriented width and height. If the window is not in the native
    // orientation, the dimensions must be reversed.
    DXGI_MODE_ROTATION displayRotation = ComputeDisplayRotation();

    bool swapDimensions = displayRotation == DXGI_MODE_ROTATION_ROTATE90 || displayRotation == DXGI_MODE_ROTATION_ROTATE270;
    m_d3dRenderTargetSize.Width = swapDimensions ? m_outputSize.Height : m_outputSize.Width;
    m_d3dRenderTargetSize.Height = swapDimensions ? m_outputSize.Width : m_outputSize.Height;

    if (m_swapChain != nullptr) {
        // If the swap chain already exists, resize it.
        HRESULT hr = m_swapChain->ResizeBuffers(
            2, // Double-buffered swap chain.
            lround(m_d3dRenderTargetSize.Width),
            lround(m_d3dRenderTargetSize.Height),
            DXGI_FORMAT_B8G8R8A8_UNORM,
            0
        );

        if (hr == DXGI_ERROR_DEVICE_REMOVED || hr == DXGI_ERROR_DEVICE_RESET) {
            // If the device was removed for any reason, a new device and swap chain will need to be created.
            HandleDeviceLost();

            // Everything is set up now. Do not continue execution of this method. HandleDeviceLost will reenter this method
            // and correctly set up the new device.
            return;
        }
        else {
            winrt::check_hresult(hr);
        }
    }
    else {
        // Otherwise, create a new one using the same adapter as the existing Direct3D device.
        DXGI_SCALING scaling = (_sapp.uwp.dpi.content_scale == _sapp.uwp.dpi.window_scale) ? DXGI_SCALING_NONE : DXGI_SCALING_STRETCH;
        DXGI_SWAP_CHAIN_DESC1 swapChainDesc = { 0 };

        swapChainDesc.Width = lround(m_d3dRenderTargetSize.Width);      // Match the size of the window.
        swapChainDesc.Height = lround(m_d3dRenderTargetSize.Height);
        swapChainDesc.Format = DXGI_FORMAT_B8G8R8A8_UNORM;              // This is the most common swap chain format.
        swapChainDesc.Stereo = false;
        swapChainDesc.SampleDesc.Count = 1;                             // Don't use multi-sampling.
        swapChainDesc.SampleDesc.Quality = 0;
        swapChainDesc.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
        swapChainDesc.BufferCount = 2;                                  // Use double-buffering to minimize latency.
        swapChainDesc.SwapEffect = DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL;    // All Microsoft Store apps must use this SwapEffect.
        swapChainDesc.Flags = 0;
        swapChainDesc.Scaling = scaling;
        swapChainDesc.AlphaMode = DXGI_ALPHA_MODE_IGNORE;

        // This sequence obtains the DXGI factory that was used to create the Direct3D device above.
        winrt::com_ptr<IDXGIDevice3> dxgiDevice = m_d3dDevice.as<IDXGIDevice3>();
        winrt::com_ptr<IDXGIAdapter> dxgiAdapter;
        winrt::check_hresult(dxgiDevice->GetAdapter(dxgiAdapter.put()));
        winrt::com_ptr<IDXGIFactory4> dxgiFactory;
        winrt::check_hresult(dxgiAdapter->GetParent(__uuidof(IDXGIFactory4), dxgiFactory.put_void()));
        winrt::com_ptr<IDXGISwapChain1> swapChain;
        winrt::check_hresult(dxgiFactory->CreateSwapChainForCoreWindow(m_d3dDevice.get(), m_window.get().as<::IUnknown>().get(), &swapChainDesc, nullptr, swapChain.put()));
        m_swapChain = swapChain.as<IDXGISwapChain3>();

        // Ensure that DXGI does not queue more than one frame at a time. This both reduces latency and
        // ensures that the application will only render after each VSync, minimizing power consumption.
        winrt::check_hresult(dxgiDevice->SetMaximumFrameLatency(1));

        // Setup Sokol Context
        winrt::check_hresult(swapChain->GetDesc(&_sapp.d3d11.swap_chain_desc));
        _sapp.d3d11.swap_chain = m_swapChain.as<IDXGISwapChain3>().detach();
    }

    // Set the proper orientation for the swap chain, and generate 2D and
    // 3D matrix transformations for rendering to the rotated swap chain.
    // Note the rotation angle for the 2D and 3D transforms are different.
    // This is due to the difference in coordinate spaces.  Additionally,
    // the 3D matrix is specified explicitly to avoid rounding errors.
    switch (displayRotation) {
        case DXGI_MODE_ROTATION_IDENTITY:
            m_orientationTransform3D = m_rotation0;
            break;

        case DXGI_MODE_ROTATION_ROTATE90:
            m_orientationTransform3D = m_rotation270;
            break;

        case DXGI_MODE_ROTATION_ROTATE180:
            m_orientationTransform3D = m_rotation180;
            break;

        case DXGI_MODE_ROTATION_ROTATE270:
            m_orientationTransform3D = m_rotation90;
            break;
    }
    winrt::check_hresult(m_swapChain->SetRotation(displayRotation));

    // Create a render target view of the swap chain back buffer.
    winrt::check_hresult(m_swapChain->GetBuffer(0, IID_PPV_ARGS(&m_d3dRenderTarget)));
    winrt::check_hresult(m_d3dDevice->CreateRenderTargetView1(m_d3dRenderTarget.get(), nullptr, m_d3dRenderTargetView.put()));

    // Create MSAA texture and view if needed
    if (_sapp.sample_count > 1) {
        CD3D11_TEXTURE2D_DESC1 msaaTexDesc(
            DXGI_FORMAT_B8G8R8A8_UNORM,
            lround(m_d3dRenderTargetSize.Width),
            lround(m_d3dRenderTargetSize.Height),
            1,  // arraySize
            1,  // mipLevels
            D3D11_BIND_RENDER_TARGET,
            D3D11_USAGE_DEFAULT,
            0,  // cpuAccessFlags
            _sapp.sample_count,
            _sapp.sample_count > 1 ? D3D11_STANDARD_MULTISAMPLE_PATTERN : 0
        );
        winrt::check_hresult(m_d3dDevice->CreateTexture2D1(&msaaTexDesc, nullptr, m_d3dMSAARenderTarget.put()));
        winrt::check_hresult(m_d3dDevice->CreateRenderTargetView1(m_d3dMSAARenderTarget.get(), nullptr, m_d3dMSAARenderTargetView.put()));
    }

    // Create a depth stencil view for use with 3D rendering if needed.
    CD3D11_TEXTURE2D_DESC1 depthStencilDesc(
        DXGI_FORMAT_D24_UNORM_S8_UINT,
        lround(m_d3dRenderTargetSize.Width),
        lround(m_d3dRenderTargetSize.Height),
        1, // This depth stencil view has only one texture.
        1, // Use a single mipmap level.
        D3D11_BIND_DEPTH_STENCIL,
        D3D11_USAGE_DEFAULT,
        0,  // cpuAccessFlag
        _sapp.sample_count,
        _sapp.sample_count > 1 ? D3D11_STANDARD_MULTISAMPLE_PATTERN : 0
    );
    winrt::check_hresult(m_d3dDevice->CreateTexture2D1(&depthStencilDesc, nullptr, m_d3dDepthStencil.put()));

    CD3D11_DEPTH_STENCIL_VIEW_DESC depthStencilViewDesc(D3D11_DSV_DIMENSION_TEXTURE2D);
    winrt::check_hresult(m_d3dDevice->CreateDepthStencilView(m_d3dDepthStencil.get(), nullptr, m_d3dDepthStencilView.put()));

    // Set sokol window and framebuffer sizes
    _sapp.window_width = (int) m_logicalSize.Width;
    _sapp.window_height = (int) m_logicalSize.Height;
    _sapp.framebuffer_width = lround(m_d3dRenderTargetSize.Width);
    _sapp.framebuffer_height = lround(m_d3dRenderTargetSize.Height);

    // Setup Sokol Context
    _sapp.d3d11.rt = m_d3dRenderTarget.as<ID3D11Texture2D>().get();
    _sapp.d3d11.rtv = m_d3dRenderTargetView.as<ID3D11RenderTargetView>().get();
    _sapp.d3d11.ds = m_d3dDepthStencil.as<ID3D11Texture2D>().get();
    _sapp.d3d11.dsv = m_d3dDepthStencilView.get();
    if (_sapp.sample_count > 1) {
        _sapp.d3d11.msaa_rt = m_d3dMSAARenderTarget.as<ID3D11Texture2D>().get();
        _sapp.d3d11.msaa_rtv = m_d3dMSAARenderTargetView.as<ID3D11RenderTargetView>().get();
    }

    // Sokol app is now valid
    _sapp.valid = true;
}

// Determine the dimensions of the render target and whether it will be scaled down.
void DeviceResources::UpdateRenderTargetSize() {
    // Calculate the necessary render target size in pixels.
    m_outputSize.Width = m_logicalSize.Width * _sapp.uwp.dpi.content_scale;
    m_outputSize.Height = m_logicalSize.Height * _sapp.uwp.dpi.content_scale;

    // Prevent zero size DirectX content from being created.
    m_outputSize.Width = std::max(m_outputSize.Width, 1.0f);
    m_outputSize.Height = std::max(m_outputSize.Height, 1.0f);
}

// This method is called when the CoreWindow is created (or re-created).
void DeviceResources::SetWindow(winrt::Windows::UI::Core::CoreWindow const& window) {
    auto currentDisplayInformation = winrt::Windows::Graphics::Display::DisplayInformation::GetForCurrentView();
    m_window = window;
    m_logicalSize = winrt::Windows::Foundation::Size(window.Bounds().Width, window.Bounds().Height);
    m_nativeOrientation = currentDisplayInformation.NativeOrientation();
    m_currentOrientation = currentDisplayInformation.CurrentOrientation();
    m_dpi = currentDisplayInformation.LogicalDpi();
    _sapp_uwp_configure_dpi(m_dpi);
    CreateWindowSizeDependentResources();
}

// This method is called in the event handler for the SizeChanged event.
void DeviceResources::SetLogicalSize(winrt::Windows::Foundation::Size logicalSize) {
    if (m_logicalSize != logicalSize) {
        m_logicalSize = logicalSize;
        CreateWindowSizeDependentResources();
    }
}

// This method is called in the event handler for the DpiChanged event.
void DeviceResources::SetDpi(float dpi) {
    if (dpi != m_dpi) {
        m_dpi = dpi;
        _sapp_uwp_configure_dpi(m_dpi);
        // When the display DPI changes, the logical size of the window (measured in Dips) also changes and needs to be updated.
        auto window = m_window.get();
        m_logicalSize = winrt::Windows::Foundation::Size(window.Bounds().Width, window.Bounds().Height);
        CreateWindowSizeDependentResources();
    }
}

// This method is called in the event handler for the OrientationChanged event.
void DeviceResources::SetCurrentOrientation(winrt::Windows::Graphics::Display::DisplayOrientations currentOrientation) {
    if (m_currentOrientation != currentOrientation) {
        m_currentOrientation = currentOrientation;
        CreateWindowSizeDependentResources();
    }
}

// This method is called in the event handler for the DisplayContentsInvalidated event.
void DeviceResources::ValidateDevice() {
    // The D3D Device is no longer valid if the default adapter changed since the device
    // was created or if the device has been removed.

    // First, get the information for the default adapter from when the device was created.
    winrt::com_ptr<IDXGIDevice3> dxgiDevice = m_d3dDevice.as< IDXGIDevice3>();
    winrt::com_ptr<IDXGIAdapter> deviceAdapter;
    winrt::check_hresult(dxgiDevice->GetAdapter(deviceAdapter.put()));
    winrt::com_ptr<IDXGIFactory4> deviceFactory;
    winrt::check_hresult(deviceAdapter->GetParent(IID_PPV_ARGS(&deviceFactory)));
    winrt::com_ptr<IDXGIAdapter1> previousDefaultAdapter;
    winrt::check_hresult(deviceFactory->EnumAdapters1(0, previousDefaultAdapter.put()));
    DXGI_ADAPTER_DESC1 previousDesc;
    winrt::check_hresult(previousDefaultAdapter->GetDesc1(&previousDesc));

    // Next, get the information for the current default adapter.
    winrt::com_ptr<IDXGIFactory4> currentFactory;
    winrt::check_hresult(CreateDXGIFactory1(IID_PPV_ARGS(&currentFactory)));
    winrt::com_ptr<IDXGIAdapter1> currentDefaultAdapter;
    winrt::check_hresult(currentFactory->EnumAdapters1(0, currentDefaultAdapter.put()));
    DXGI_ADAPTER_DESC1 currentDesc;
    winrt::check_hresult(currentDefaultAdapter->GetDesc1(&currentDesc));

    // If the adapter LUIDs don't match, or if the device reports that it has been removed,
    // a new D3D device must be created.
    if (previousDesc.AdapterLuid.LowPart != currentDesc.AdapterLuid.LowPart ||
        previousDesc.AdapterLuid.HighPart != currentDesc.AdapterLuid.HighPart ||
        FAILED(m_d3dDevice->GetDeviceRemovedReason()))
    {
        // Release references to resources related to the old device.
        dxgiDevice = nullptr;
        deviceAdapter = nullptr;
        deviceFactory = nullptr;
        previousDefaultAdapter = nullptr;

        // Create a new device and swap chain.
        HandleDeviceLost();
    }
}

// Recreate all device resources and set them back to the current state.
void DeviceResources::HandleDeviceLost() {
    m_swapChain = nullptr;
    if (m_deviceNotify != nullptr) {
        m_deviceNotify->OnDeviceLost();
    }
    CreateDeviceResources();
    CreateWindowSizeDependentResources();
    if (m_deviceNotify != nullptr) {
        m_deviceNotify->OnDeviceRestored();
    }
}

// Register our DeviceNotify to be informed on device lost and creation.
void DeviceResources::RegisterDeviceNotify(IDeviceNotify* deviceNotify) {
    m_deviceNotify = deviceNotify;
}

// Call this method when the app suspends. It provides a hint to the driver that the app
// is entering an idle state and that temporary buffers can be reclaimed for use by other apps.
void DeviceResources::Trim() {
    m_d3dDevice.as<IDXGIDevice3>()->Trim();
}

// Present the contents of the swap chain to the screen.
void DeviceResources::Present() {

    // MSAA resolve if needed
    if (_sapp.sample_count > 1) {
        m_d3dContext->ResolveSubresource(m_d3dRenderTarget.get(), 0, m_d3dMSAARenderTarget.get(), 0, DXGI_FORMAT_B8G8R8A8_UNORM);
        m_d3dContext->DiscardView1(m_d3dMSAARenderTargetView.get(), nullptr, 0);
    }

    // The first argument instructs DXGI to block until VSync, putting the application
    // to sleep until the next VSync. This ensures we don't waste any cycles rendering
    // frames that will never be displayed to the screen.
    DXGI_PRESENT_PARAMETERS parameters = { 0 };
    HRESULT hr = m_swapChain->Present1(1, 0, &parameters);

    // Discard the contents of the render target.
    // This is a valid operation only when the existing contents will be entirely
    // overwritten. If dirty or scroll rects are used, this call should be removed.
    m_d3dContext->DiscardView1(m_d3dRenderTargetView.get(), nullptr, 0);

    // Discard the contents of the depth stencil.
    m_d3dContext->DiscardView1(m_d3dDepthStencilView.get(), nullptr, 0);

    // If the device was removed either by a disconnection or a driver upgrade, we
    // must recreate all device resources.
    if (hr == DXGI_ERROR_DEVICE_REMOVED || hr == DXGI_ERROR_DEVICE_RESET) {
        HandleDeviceLost();
    }
    else {
        winrt::check_hresult(hr);
    }
}

// This method determines the rotation between the display device's native orientation and the
// current display orientation.
DXGI_MODE_ROTATION DeviceResources::ComputeDisplayRotation() {
    DXGI_MODE_ROTATION rotation = DXGI_MODE_ROTATION_UNSPECIFIED;

    // Note: NativeOrientation can only be Landscape or Portrait even though
    // the DisplayOrientations enum has other values.
    switch (m_nativeOrientation) {
        case winrt::Windows::Graphics::Display::DisplayOrientations::Landscape:
            switch (m_currentOrientation) {
                case winrt::Windows::Graphics::Display::DisplayOrientations::Landscape:
                    rotation = DXGI_MODE_ROTATION_IDENTITY;
                    break;

                case winrt::Windows::Graphics::Display::DisplayOrientations::Portrait:
                    rotation = DXGI_MODE_ROTATION_ROTATE270;
                    break;

                case winrt::Windows::Graphics::Display::DisplayOrientations::LandscapeFlipped:
                    rotation = DXGI_MODE_ROTATION_ROTATE180;
                    break;

                case winrt::Windows::Graphics::Display::DisplayOrientations::PortraitFlipped:
                    rotation = DXGI_MODE_ROTATION_ROTATE90;
                    break;
            }
            break;

        case winrt::Windows::Graphics::Display::DisplayOrientations::Portrait:
            switch (m_currentOrientation) {
                case winrt::Windows::Graphics::Display::DisplayOrientations::Landscape:
                    rotation = DXGI_MODE_ROTATION_ROTATE90;
                    break;

                case winrt::Windows::Graphics::Display::DisplayOrientations::Portrait:
                    rotation = DXGI_MODE_ROTATION_IDENTITY;
                    break;

                case winrt::Windows::Graphics::Display::DisplayOrientations::LandscapeFlipped:
                    rotation = DXGI_MODE_ROTATION_ROTATE270;
                    break;

                case winrt::Windows::Graphics::Display::DisplayOrientations::PortraitFlipped:
                    rotation = DXGI_MODE_ROTATION_ROTATE180;
                    break;
            }
            break;
    }
    return rotation;
}

// Check for SDK Layer support.
bool DeviceResources::SdkLayersAvailable() {
    #if defined(_DEBUG)
        HRESULT hr = D3D11CreateDevice(
            nullptr,
            D3D_DRIVER_TYPE_NULL,       // There is no need to create a real hardware device.
            0,
            D3D11_CREATE_DEVICE_DEBUG,  // Check for the SDK layers.
            nullptr,                    // Any feature level will do.
            0,
            D3D11_SDK_VERSION,          // Always set this to D3D11_SDK_VERSION for Microsoft Store apps.
            nullptr,                    // No need to keep the D3D device reference.
            nullptr,                    // No need to know the feature level.
            nullptr                     // No need to keep the D3D device context reference.
        );
        return SUCCEEDED(hr);
    #else
        return false;
    #endif
}

// The first method called when the IFrameworkView is being created.
void App::Initialize(winrt::Windows::ApplicationModel::Core::CoreApplicationView const& applicationView) {
    // Register event handlers for app lifecycle. This example includes Activated, so that we
    // can make the CoreWindow active and start rendering on the window.
    applicationView.Activated({ this, &App::OnActivated });

    winrt::Windows::ApplicationModel::Core::CoreApplication::Suspending({ this, &App::OnSuspending });
    winrt::Windows::ApplicationModel::Core::CoreApplication::Resuming({ this, &App::OnResuming });

    // At this point we have access to the device.
    // We can create the device-dependent resources.
    m_deviceResources = std::make_unique<DeviceResources>();
}

// Called when the CoreWindow object is created (or re-created).
void App::SetWindow(winrt::Windows::UI::Core::CoreWindow const& window) {
    window.SizeChanged({ this, &App::OnWindowSizeChanged });
    window.VisibilityChanged({ this, &App::OnVisibilityChanged });

    window.KeyDown({ this, &App::OnKeyDown });
    window.KeyUp({ this, &App::OnKeyUp });
    window.CharacterReceived({ this, &App::OnCharacterReceived });

    window.PointerEntered({ this, &App::OnPointerEntered });
    window.PointerExited({ this, &App::OnPointerExited });
    window.PointerPressed({ this, &App::OnPointerPressed });
    window.PointerReleased({ this, &App::OnPointerReleased });
    window.PointerMoved({ this, &App::OnPointerMoved });
    window.PointerWheelChanged({ this, &App::OnPointerWheelChanged });

    auto currentDisplayInformation = winrt::Windows::Graphics::Display::DisplayInformation::GetForCurrentView();

    currentDisplayInformation.DpiChanged({ this, &App::OnDpiChanged });
    currentDisplayInformation.OrientationChanged({ this, &App::OnOrientationChanged });
    winrt::Windows::Graphics::Display::DisplayInformation::DisplayContentsInvalidated({ this, &App::OnDisplayContentsInvalidated });

    winrt::Windows::UI::Core::SystemNavigationManager::GetForCurrentView().BackRequested({ this, &App::OnBackRequested });

    m_deviceResources->SetWindow(window);
}

// Initializes scene resources, or loads a previously saved app state.
void App::Load(winrt::hstring const& entryPoint) {
    _SOKOL_UNUSED(entryPoint);
}

// This method is called after the window becomes active.
void App::Run() {
    // NOTE: UWP will simply terminate an application, it's not possible to detect when an application is being closed
    while (true) {
        if (m_windowVisible) {
            _sapp_timing_measure(&_sapp.timing);
            winrt::Windows::UI::Core::CoreWindow::GetForCurrentThread().Dispatcher().ProcessEvents(winrt::Windows::UI::Core::CoreProcessEventsOption::ProcessAllIfPresent);
            _sapp_frame();
            m_deviceResources->Present();
        }
        else {
            winrt::Windows::UI::Core::CoreWindow::GetForCurrentThread().Dispatcher().ProcessEvents(winrt::Windows::UI::Core::CoreProcessEventsOption::ProcessOneAndAllPending);
        }
    }
}

// Required for IFrameworkView.
// Terminate events do not cause Uninitialize to be called. It will be called if your IFrameworkView
// class is torn down while the app is in the foreground.
void App::Uninitialize() {
    // empty
}

// Application lifecycle event handlers.
void App::OnActivated(winrt::Windows::ApplicationModel::Core::CoreApplicationView const& applicationView, winrt::Windows::ApplicationModel::Activation::IActivatedEventArgs const& args) {
    _SOKOL_UNUSED(args);
    _SOKOL_UNUSED(applicationView);
    auto appView = winrt::Windows::UI::ViewManagement::ApplicationView::GetForCurrentView();
    auto targetSize = winrt::Windows::Foundation::Size((float)_sapp.desc.width, (float)_sapp.desc.height);
    appView.SetPreferredMinSize(targetSize);
    appView.TryResizeView(targetSize);

    // Disabling this since it can only append the title to the app name (Title - Appname).
    // There's no way of just setting a string to be the window title.
    //appView.Title(_sapp.window_title_wide);

    // Run() won't start until the CoreWindow is activated.
    winrt::Windows::UI::Core::CoreWindow::GetForCurrentThread().Activate();
    if (_sapp.desc.fullscreen) {
        appView.TryEnterFullScreenMode();
    }
    _sapp.fullscreen = appView.IsFullScreenMode();
}

void App::OnSuspending(winrt::Windows::Foundation::IInspectable const& sender, winrt::Windows::ApplicationModel::SuspendingEventArgs const& args) {
    _SOKOL_UNUSED(sender);
    _SOKOL_UNUSED(args);
    _sapp_win32_uwp_app_event(SAPP_EVENTTYPE_SUSPENDED);
}

void App::OnResuming(winrt::Windows::Foundation::IInspectable const& sender, winrt::Windows::Foundation::IInspectable const& args) {
    _SOKOL_UNUSED(args);
    _SOKOL_UNUSED(sender);
    _sapp_win32_uwp_app_event(SAPP_EVENTTYPE_RESUMED);
}

void App::OnWindowSizeChanged(winrt::Windows::UI::Core::CoreWindow const& sender, winrt::Windows::UI::Core::WindowSizeChangedEventArgs const& args) {
    _SOKOL_UNUSED(args);
    m_deviceResources->SetLogicalSize(winrt::Windows::Foundation::Size(sender.Bounds().Width, sender.Bounds().Height));
    _sapp_win32_uwp_app_event(SAPP_EVENTTYPE_RESIZED);
}

void App::OnVisibilityChanged(winrt::Windows::UI::Core::CoreWindow const& sender, winrt::Windows::UI::Core::VisibilityChangedEventArgs const& args) {
    _SOKOL_UNUSED(sender);
    m_windowVisible = args.Visible();
    _sapp_win32_uwp_app_event(m_windowVisible ? SAPP_EVENTTYPE_RESTORED : SAPP_EVENTTYPE_ICONIFIED);
}

void App::OnBackRequested(winrt::Windows::Foundation::IInspectable const& sender, winrt::Windows::UI::Core::BackRequestedEventArgs const& args) {
    _SOKOL_UNUSED(sender);
    args.Handled(true);
}

void App::OnKeyDown(winrt::Windows::UI::Core::CoreWindow const& sender, winrt::Windows::UI::Core::KeyEventArgs const& args) {
    auto status = args.KeyStatus();
    _sapp_uwp_key_event(SAPP_EVENTTYPE_KEY_DOWN, sender, args);
}

void App::OnKeyUp(winrt::Windows::UI::Core::CoreWindow const& sender, winrt::Windows::UI::Core::KeyEventArgs const& args) {
    auto status = args.KeyStatus();
    _sapp_uwp_key_event(SAPP_EVENTTYPE_KEY_UP, sender, args);
}

void App::OnCharacterReceived(winrt::Windows::UI::Core::CoreWindow const& sender, winrt::Windows::UI::Core::CharacterReceivedEventArgs const& args) {
    _sapp_uwp_char_event(args.KeyCode(), args.KeyStatus().WasKeyDown, sender);
}

void App::OnPointerEntered(winrt::Windows::UI::Core::CoreWindow const& sender, winrt::Windows::UI::Core::PointerEventArgs const& args) {
    _SOKOL_UNUSED(args);
    _sapp.uwp.mouse_tracked = true;
    _sapp_uwp_mouse_event(SAPP_EVENTTYPE_MOUSE_ENTER, SAPP_MOUSEBUTTON_INVALID, sender);
}

void App::OnPointerExited(winrt::Windows::UI::Core::CoreWindow const& sender, winrt::Windows::UI::Core::PointerEventArgs const& args) {
    _SOKOL_UNUSED(args);
    _sapp.uwp.mouse_tracked = false;
    _sapp_uwp_mouse_event(SAPP_EVENTTYPE_MOUSE_LEAVE, SAPP_MOUSEBUTTON_INVALID, sender);
}

void App::OnPointerPressed(winrt::Windows::UI::Core::CoreWindow const& sender, winrt::Windows::UI::Core::PointerEventArgs const& args) {
    _sapp_uwp_extract_mouse_button_events(sender, args);
}

// NOTE: for some reason this event handler is never called??
void App::OnPointerReleased(winrt::Windows::UI::Core::CoreWindow const& sender, winrt::Windows::UI::Core::PointerEventArgs const& args) {
    _sapp_uwp_extract_mouse_button_events(sender, args);
}

void App::OnPointerMoved(winrt::Windows::UI::Core::CoreWindow const& sender, winrt::Windows::UI::Core::PointerEventArgs const& args) {
    auto position = args.CurrentPoint().Position();
    const float new_x = (float)(int)(position.X * _sapp.uwp.dpi.mouse_scale + 0.5f);
    const float new_y = (float)(int)(position.Y * _sapp.uwp.dpi.mouse_scale + 0.5f);
    // don't update dx/dy in the very first event
    if (_sapp.mouse.pos_valid) {
        _sapp.mouse.dx = new_x - _sapp.mouse.x;
        _sapp.mouse.dy = new_y - _sapp.mouse.y;
    }
    _sapp.mouse.x = new_x;
    _sapp.mouse.y = new_y;
    _sapp.mouse.pos_valid = true;
    if (!_sapp.uwp.mouse_tracked) {
        _sapp.uwp.mouse_tracked = true;
        _sapp_uwp_mouse_event(SAPP_EVENTTYPE_MOUSE_ENTER, SAPP_MOUSEBUTTON_INVALID, sender);
    }
    _sapp_uwp_mouse_event(SAPP_EVENTTYPE_MOUSE_MOVE, SAPP_MOUSEBUTTON_INVALID, sender);

    // HACK for detecting multiple mouse button presses
    _sapp_uwp_extract_mouse_button_events(sender, args);
}

void App::OnPointerWheelChanged(winrt::Windows::UI::Core::CoreWindow const& sender, winrt::Windows::UI::Core::PointerEventArgs const& args) {
    auto properties = args.CurrentPoint().Properties();
    _sapp_uwp_scroll_event((float)properties.MouseWheelDelta(), properties.IsHorizontalMouseWheel(), sender);
}

void App::OnDpiChanged(winrt::Windows::Graphics::Display::DisplayInformation const& sender, winrt::Windows::Foundation::IInspectable const& args) {
    // NOTE: UNTESTED
    _SOKOL_UNUSED(args);
    m_deviceResources->SetDpi(sender.LogicalDpi());
    _sapp_win32_uwp_app_event(SAPP_EVENTTYPE_RESIZED);
}

void App::OnOrientationChanged(winrt::Windows::Graphics::Display::DisplayInformation const& sender, winrt::Windows::Foundation::IInspectable const& args) {
    // NOTE: UNTESTED
    _SOKOL_UNUSED(args);
    m_deviceResources->SetCurrentOrientation(sender.CurrentOrientation());
    _sapp_win32_uwp_app_event(SAPP_EVENTTYPE_RESIZED);
}

void App::OnDisplayContentsInvalidated(winrt::Windows::Graphics::Display::DisplayInformation const& sender, winrt::Windows::Foundation::IInspectable const& args) {
    // NOTE: UNTESTED
    _SOKOL_UNUSED(args);
    _SOKOL_UNUSED(sender);
    m_deviceResources->ValidateDevice();
}

} /* End empty namespace */

_SOKOL_PRIVATE void _sapp_uwp_run(const sapp_desc* desc) {
    _sapp_init_state(desc);
    _sapp_win32_uwp_init_keytable();
    _sapp_win32_uwp_utf8_to_wide(_sapp.window_title, _sapp.window_title_wide, sizeof(_sapp.window_title_wide));
    winrt::Windows::ApplicationModel::Core::CoreApplication::Run(winrt::make<App>());
}

#if !defined(SOKOL_NO_ENTRY)
#if defined(UNICODE)
int WINAPI wWinMain(_In_ HINSTANCE hInstance, _In_opt_ HINSTANCE hPrevInstance, _In_ LPWSTR lpCmdLine, _In_ int nCmdShow) {
#else
int WINAPI WinMain(_In_ HINSTANCE hInstance, _In_opt_ HINSTANCE hPrevInstance, _In_ LPSTR lpCmdLine, _In_ int nCmdShow) {
#endif
    _SOKOL_UNUSED(hInstance);
    _SOKOL_UNUSED(hPrevInstance);
    _SOKOL_UNUSED(lpCmdLine);
    _SOKOL_UNUSED(nCmdShow);
    sapp_desc desc = sokol_main(0, nullptr);
    _sapp_uwp_run(&desc);
    return 0;
}
#endif /* SOKOL_NO_ENTRY */
#endif /* _SAPP_UWP */

/*== Android ================================================================*/
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
        #if defined(SOKOL_GLES3)
            EGL_CONTEXT_CLIENT_VERSION, _sapp.desc.gl_force_gles2 ? 2 : 3,
        #else
            EGL_CONTEXT_CLIENT_VERSION, 2,
        #endif
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
            SOKOL_LOG("Destroying egl surface");
            eglDestroySurface(_sapp.android.display, _sapp.android.surface);
            _sapp.android.surface = EGL_NO_SURFACE;
        }
        if (_sapp.android.context != EGL_NO_CONTEXT) {
            SOKOL_LOG("Destroying egl context");
            eglDestroyContext(_sapp.android.display, _sapp.android.context);
            _sapp.android.context = EGL_NO_CONTEXT;
        }
        SOKOL_LOG("Terminating egl display");
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
        SOKOL_LOG("event_cb()");
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
            SOKOL_ASSERT(egl_result == EGL_TRUE);
            /* NOTE: calling ANativeWindow_setBuffersGeometry() with the same dimensions
                as the ANativeWindow size results in weird display artefacts, that's
                why it's only called when the buffer geometry is different from
                the window size
            */
            int32_t result = ANativeWindow_setBuffersGeometry(window, buf_w, buf_h, format);
            SOKOL_ASSERT(result == 0);
        }
    }

    /* query surface size */
    EGLint fb_w, fb_h;
    EGLBoolean egl_result_w = eglQuerySurface(_sapp.android.display, _sapp.android.surface, EGL_WIDTH, &fb_w);
    EGLBoolean egl_result_h = eglQuerySurface(_sapp.android.display, _sapp.android.surface, EGL_HEIGHT, &fb_h);
    SOKOL_ASSERT(egl_result_w == EGL_TRUE);
    SOKOL_ASSERT(egl_result_h == EGL_TRUE);
    const bool fb_changed = (fb_w != _sapp.framebuffer_width) || (fb_h != _sapp.framebuffer_height);
    _sapp.framebuffer_width = fb_w;
    _sapp.framebuffer_height = fb_h;
    _sapp.dpi_scale = (float)_sapp.framebuffer_width / (float)_sapp.window_width;
    if (win_changed || fb_changed || force_update) {
        if (!_sapp.first_frame) {
            SOKOL_LOG("SAPP_EVENTTYPE_RESIZED");
            _sapp_android_app_event(SAPP_EVENTTYPE_RESIZED);
        }
    }
}

_SOKOL_PRIVATE void _sapp_android_cleanup(void) {
    SOKOL_LOG("Cleaning up");
    if (_sapp.android.surface != EGL_NO_SURFACE) {
        /* egl context is bound, cleanup gracefully */
        if (_sapp.init_called && !_sapp.cleanup_called) {
            SOKOL_LOG("cleanup_cb()");
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
            SOKOL_LOG("Touch: down");
        case AMOTION_EVENT_ACTION_POINTER_DOWN:
            SOKOL_LOG("Touch: ptr down");
            type = SAPP_EVENTTYPE_TOUCHES_BEGAN;
            break;
        case AMOTION_EVENT_ACTION_MOVE:
            type = SAPP_EVENTTYPE_TOUCHES_MOVED;
            break;
        case AMOTION_EVENT_ACTION_UP:
            SOKOL_LOG("Touch: up");
        case AMOTION_EVENT_ACTION_POINTER_UP:
            SOKOL_LOG("Touch: ptr up");
            type = SAPP_EVENTTYPE_TOUCHES_ENDED;
            break;
        case AMOTION_EVENT_ACTION_CANCEL:
            SOKOL_LOG("Touch: cancel");
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
        dst->pos_x = (AMotionEvent_getRawX(e, (size_t)i) / _sapp.window_width) * _sapp.framebuffer_width;
        dst->pos_y = (AMotionEvent_getRawY(e, (size_t)i) / _sapp.window_height) * _sapp.framebuffer_height;

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
    if ((events & ALOOPER_EVENT_INPUT) == 0) {
        SOKOL_LOG("_sapp_android_input_cb() encountered unsupported event");
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
    if ((events & ALOOPER_EVENT_INPUT) == 0) {
        SOKOL_LOG("_sapp_android_main_cb() encountered unsupported event");
        return 1;
    }

    _sapp_android_msg_t msg;
    if (read(fd, &msg, sizeof(msg)) != sizeof(msg)) {
        SOKOL_LOG("Could not write to read_from_main_fd");
        return 1;
    }

    pthread_mutex_lock(&_sapp.android.pt.mutex);
    switch (msg) {
        case _SOKOL_ANDROID_MSG_CREATE:
            {
                SOKOL_LOG("MSG_CREATE");
                SOKOL_ASSERT(!_sapp.valid);
                bool result = _sapp_android_init_egl();
                SOKOL_ASSERT(result);
                _sapp.valid = true;
                _sapp.android.has_created = true;
            }
            break;
        case _SOKOL_ANDROID_MSG_RESUME:
            SOKOL_LOG("MSG_RESUME");
            _sapp.android.has_resumed = true;
            _sapp_android_app_event(SAPP_EVENTTYPE_RESUMED);
            break;
        case _SOKOL_ANDROID_MSG_PAUSE:
            SOKOL_LOG("MSG_PAUSE");
            _sapp.android.has_resumed = false;
            _sapp_android_app_event(SAPP_EVENTTYPE_SUSPENDED);
            break;
        case _SOKOL_ANDROID_MSG_FOCUS:
            SOKOL_LOG("MSG_FOCUS");
            _sapp.android.has_focus = true;
            break;
        case _SOKOL_ANDROID_MSG_NO_FOCUS:
            SOKOL_LOG("MSG_NO_FOCUS");
            _sapp.android.has_focus = false;
            break;
        case _SOKOL_ANDROID_MSG_SET_NATIVE_WINDOW:
            SOKOL_LOG("MSG_SET_NATIVE_WINDOW");
            if (_sapp.android.current.window != _sapp.android.pending.window) {
                if (_sapp.android.current.window != NULL) {
                    _sapp_android_cleanup_egl_surface();
                }
                if (_sapp.android.pending.window != NULL) {
                    SOKOL_LOG("Creating egl surface ...");
                    if (_sapp_android_init_egl_surface(_sapp.android.pending.window)) {
                        SOKOL_LOG("... ok!");
                        _sapp_android_update_dimensions(_sapp.android.pending.window, true);
                    } else {
                        SOKOL_LOG("... failed!");
                        _sapp_android_shutdown();
                    }
                }
            }
            _sapp.android.current.window = _sapp.android.pending.window;
            break;
        case _SOKOL_ANDROID_MSG_SET_INPUT_QUEUE:
            SOKOL_LOG("MSG_SET_INPUT_QUEUE");
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
            SOKOL_LOG("MSG_DESTROY");
            _sapp_android_cleanup();
            _sapp.valid = false;
            _sapp.android.is_thread_stopping = true;
            break;
        default:
            SOKOL_LOG("Unknown msg type received");
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
        SOKOL_LOG("Showing keyboard");
        ANativeActivity_showSoftInput(_sapp.android.activity, ANATIVEACTIVITY_SHOW_SOFT_INPUT_FORCED);
    } else {
        SOKOL_LOG("Hiding keyboard");
        ANativeActivity_hideSoftInput(_sapp.android.activity, ANATIVEACTIVITY_HIDE_SOFT_INPUT_NOT_ALWAYS);
    }
}

_SOKOL_PRIVATE void* _sapp_android_loop(void* arg) {
    _SOKOL_UNUSED(arg);
    SOKOL_LOG("Loop thread started");

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
    SOKOL_LOG("Loop thread done");
    return NULL;
}

/* android main/ui thread */
_SOKOL_PRIVATE void _sapp_android_msg(_sapp_android_msg_t msg) {
    if (write(_sapp.android.pt.write_from_main_fd, &msg, sizeof(msg)) != sizeof(msg)) {
        SOKOL_LOG("Could not write to write_from_main_fd");
    }
}

_SOKOL_PRIVATE void _sapp_android_on_start(ANativeActivity* activity) {
    SOKOL_LOG("NativeActivity onStart()");
}

_SOKOL_PRIVATE void _sapp_android_on_resume(ANativeActivity* activity) {
    SOKOL_LOG("NativeActivity onResume()");
    _sapp_android_msg(_SOKOL_ANDROID_MSG_RESUME);
}

_SOKOL_PRIVATE void* _sapp_android_on_save_instance_state(ANativeActivity* activity, size_t* out_size) {
    SOKOL_LOG("NativeActivity onSaveInstanceState()");
    *out_size = 0;
    return NULL;
}

_SOKOL_PRIVATE void _sapp_android_on_window_focus_changed(ANativeActivity* activity, int has_focus) {
    SOKOL_LOG("NativeActivity onWindowFocusChanged()");
    if (has_focus) {
        _sapp_android_msg(_SOKOL_ANDROID_MSG_FOCUS);
    } else {
        _sapp_android_msg(_SOKOL_ANDROID_MSG_NO_FOCUS);
    }
}

_SOKOL_PRIVATE void _sapp_android_on_pause(ANativeActivity* activity) {
    SOKOL_LOG("NativeActivity onPause()");
    _sapp_android_msg(_SOKOL_ANDROID_MSG_PAUSE);
}

_SOKOL_PRIVATE void _sapp_android_on_stop(ANativeActivity* activity) {
    SOKOL_LOG("NativeActivity onStop()");
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
    SOKOL_LOG("NativeActivity onNativeWindowCreated()");
    _sapp_android_msg_set_native_window(window);
}

_SOKOL_PRIVATE void _sapp_android_on_native_window_destroyed(ANativeActivity* activity, ANativeWindow* window) {
    SOKOL_LOG("NativeActivity onNativeWindowDestroyed()");
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
    SOKOL_LOG("NativeActivity onInputQueueCreated()");
    _sapp_android_msg_set_input_queue(queue);
}

_SOKOL_PRIVATE void _sapp_android_on_input_queue_destroyed(ANativeActivity* activity, AInputQueue* queue) {
    SOKOL_LOG("NativeActivity onInputQueueDestroyed()");
    _sapp_android_msg_set_input_queue(NULL);
}

_SOKOL_PRIVATE void _sapp_android_on_config_changed(ANativeActivity* activity) {
    SOKOL_LOG("NativeActivity onConfigurationChanged()");
    /* see android:configChanges in manifest */
}

_SOKOL_PRIVATE void _sapp_android_on_low_memory(ANativeActivity* activity) {
    SOKOL_LOG("NativeActivity onLowMemory()");
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
    SOKOL_LOG("NativeActivity onDestroy()");

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

    SOKOL_LOG("NativeActivity done");

    /* this is a bit naughty, but causes a clean restart of the app (static globals are reset) */
    exit(0);
}

JNIEXPORT
void ANativeActivity_onCreate(ANativeActivity* activity, void* saved_state, size_t saved_state_size) {
    SOKOL_LOG("NativeActivity onCreate()");

    sapp_desc desc = sokol_main(0, NULL);
    _sapp_init_state(&desc);

    /* start loop thread */
    _sapp.android.activity = activity;

    int pipe_fd[2];
    if (pipe(pipe_fd) != 0) {
        SOKOL_LOG("Could not create thread pipe");
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

    SOKOL_LOG("NativeActivity successfully created");

    /* NOT A BUG: do NOT call sapp_discard_state() */
}

#endif /* _SAPP_ANDROID */

/*== LINUX ==================================================================*/
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
    char* rms = XResourceManagerString(_sapp.x11.display);
    if (rms) {
        XrmDatabase db = XrmGetStringDatabase(rms);
        if (db) {
            XrmValue value;
            char* type = NULL;
            if (XrmGetResource(db, "Xft.dpi", "Xft.Dpi", &type, &value)) {
                if (type && strcmp(type, "String") == 0) {
                    _sapp.x11.dpi = atof(value.addr);
                }
            }
            XrmDestroyDatabase(db);
        }
    }
}

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

_SOKOL_PRIVATE void _sapp_glx_init() {
    const char* sonames[] = { "libGL.so.1", "libGL.so", 0 };
    for (int i = 0; sonames[i]; i++) {
        _sapp.glx.libgl = dlopen(sonames[i], RTLD_LAZY|RTLD_GLOBAL);
        if (_sapp.glx.libgl) {
            break;
        }
    }
    if (!_sapp.glx.libgl) {
        _sapp_fail("GLX: failed to load libGL");
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
        _sapp_fail("GLX: failed to load required entry points");
    }

    if (!_sapp.glx.QueryExtension(_sapp.x11.display, &_sapp.glx.error_base, &_sapp.glx.event_base)) {
        _sapp_fail("GLX: GLX extension not found");
    }
    if (!_sapp.glx.QueryVersion(_sapp.x11.display, &_sapp.glx.major, &_sapp.glx.minor)) {
        _sapp_fail("GLX: Failed to query GLX version");
    }
    if (_sapp.glx.major == 1 && _sapp.glx.minor < 3) {
        _sapp_fail("GLX: GLX version 1.3 is required");
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

_SOKOL_PRIVATE GLXFBConfig _sapp_glx_choosefbconfig() {
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
        _sapp_fail("GLX: No GLXFBConfigs returned");
    }

    usable_configs = (_sapp_gl_fbconfig*) SOKOL_CALLOC((size_t)native_count, sizeof(_sapp_gl_fbconfig));
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
    SOKOL_FREE(usable_configs);
    return result;
}

_SOKOL_PRIVATE void _sapp_glx_choose_visual(Visual** visual, int* depth) {
    GLXFBConfig native = _sapp_glx_choosefbconfig();
    if (0 == native) {
        _sapp_fail("GLX: Failed to find a suitable GLXFBConfig");
    }
    XVisualInfo* result = _sapp.glx.GetVisualFromFBConfig(_sapp.x11.display, native);
    if (!result) {
        _sapp_fail("GLX: Failed to retrieve Visual for GLXFBConfig");
    }
    *visual = result->visual;
    *depth = result->depth;
    XFree(result);
}

_SOKOL_PRIVATE void _sapp_glx_create_context(void) {
    GLXFBConfig native = _sapp_glx_choosefbconfig();
    if (0 == native){
        _sapp_fail("GLX: Failed to find a suitable GLXFBConfig (2)");
    }
    if (!(_sapp.glx.ARB_create_context && _sapp.glx.ARB_create_context_profile)) {
        _sapp_fail("GLX: ARB_create_context and ARB_create_context_profile required");
    }
    _sapp_x11_grab_error_handler();
    const int attribs[] = {
        GLX_CONTEXT_MAJOR_VERSION_ARB, 3,
        GLX_CONTEXT_MINOR_VERSION_ARB, 3,
        GLX_CONTEXT_PROFILE_MASK_ARB, GLX_CONTEXT_CORE_PROFILE_BIT_ARB,
        GLX_CONTEXT_FLAGS_ARB, GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB,
        0, 0
    };
    _sapp.glx.ctx = _sapp.glx.CreateContextAttribsARB(_sapp.x11.display, native, NULL, True, attribs);
    if (!_sapp.glx.ctx) {
        _sapp_fail("GLX: failed to create GL context");
    }
    _sapp_x11_release_error_handler();
    _sapp.glx.window = _sapp.glx.CreateWindow(_sapp.x11.display, native, _sapp.x11.window, NULL);
    if (!_sapp.glx.window) {
        _sapp_fail("GLX: failed to create window");
    }
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

_SOKOL_PRIVATE void _sapp_glx_make_current(void) {
    _sapp.glx.MakeCurrent(_sapp.x11.display, _sapp.glx.window, _sapp.glx.ctx);
}

_SOKOL_PRIVATE void _sapp_glx_swap_buffers(void) {
    _sapp.glx.SwapBuffers(_sapp.x11.display, _sapp.glx.window);
}

_SOKOL_PRIVATE void _sapp_glx_swapinterval(int interval) {
    _sapp_glx_make_current();
    if (_sapp.glx.EXT_swap_control) {
        _sapp.glx.SwapIntervalEXT(_sapp.x11.display, _sapp.glx.window, interval);
    }
    else if (_sapp.glx.MESA_swap_control) {
        _sapp.glx.SwapIntervalMESA(interval);
    }
}

_SOKOL_PRIVATE void _sapp_x11_send_event(Atom type, int a, int b, int c, int d, int e) {
    XEvent event;
    memset(&event, 0, sizeof(event));

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
    memset(img->pixels, 0, num_bytes);
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
    long* icon_data = (long*) SOKOL_CALLOC((size_t)long_count, sizeof(long));
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
    SOKOL_FREE(icon_data);
    XFlush(_sapp.x11.display);
}

_SOKOL_PRIVATE void _sapp_x11_create_window(Visual* visual, int depth) {
    _sapp.x11.colormap = XCreateColormap(_sapp.x11.display, _sapp.x11.root, visual, AllocNone);
    XSetWindowAttributes wa;
    memset(&wa, 0, sizeof(wa));
    const uint32_t wamask = CWBorderPixel | CWColormap | CWEventMask;
    wa.colormap = _sapp.x11.colormap;
    wa.border_pixel = 0;
    wa.event_mask = StructureNotifyMask | KeyPressMask | KeyReleaseMask |
                    PointerMotionMask | ButtonPressMask | ButtonReleaseMask |
                    ExposureMask | FocusChangeMask | VisibilityChangeMask |
                    EnterWindowMask | LeaveWindowMask | PropertyChangeMask;
    _sapp_x11_grab_error_handler();
    _sapp.x11.window = XCreateWindow(_sapp.x11.display,
                                     _sapp.x11.root,
                                     0, 0,
                                     (uint32_t)_sapp.window_width,
                                     (uint32_t)_sapp.window_height,
                                     0,     /* border width */
                                     depth, /* color depth */
                                     InputOutput,
                                     visual,
                                     wamask,
                                     &wa);
    _sapp_x11_release_error_handler();
    if (!_sapp.x11.window) {
        _sapp_fail("X11: Failed to create window");
    }
    Atom protocols[] = {
        _sapp.x11.WM_DELETE_WINDOW
    };
    XSetWMProtocols(_sapp.x11.display, _sapp.x11.window, protocols, 1);

    XSizeHints* hints = XAllocSizeHints();
    hints->flags |= PWinGravity;
    hints->win_gravity = StaticGravity;
    XSetWMNormalHints(_sapp.x11.display, _sapp.x11.window, hints);
    XFree(hints);

    /* announce support for drag'n'drop */
    if (_sapp.drop.enabled) {
        const Atom version = _SAPP_X11_XDND_VERSION;
        XChangeProperty(_sapp.x11.display, _sapp.x11.window, _sapp.x11.xdnd.XdndAware, XA_ATOM, 32, PropModeReplace, (unsigned char*) &version, 1);
    }

    _sapp_x11_update_window_title();
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
    int dummy;
    KeySym* keysyms = XGetKeyboardMapping(_sapp.x11.display, scancode, 1, &dummy);
    SOKOL_ASSERT(keysyms);
    KeySym keysym = keysyms[0];
    XFree(keysyms);
    switch (keysym) {
        case XK_Escape:         return SAPP_KEYCODE_ESCAPE;
        case XK_Tab:            return SAPP_KEYCODE_TAB;
        case XK_Shift_L:        return SAPP_KEYCODE_LEFT_SHIFT;
        case XK_Shift_R:        return SAPP_KEYCODE_RIGHT_SHIFT;
        case XK_Control_L:      return SAPP_KEYCODE_LEFT_CONTROL;
        case XK_Control_R:      return SAPP_KEYCODE_RIGHT_CONTROL;
        case XK_Meta_L:
        case XK_Alt_L:          return SAPP_KEYCODE_LEFT_ALT;
        case XK_Mode_switch:    /* Mapped to Alt_R on many keyboards */
        case XK_ISO_Level3_Shift: /* AltGr on at least some machines */
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

        case XK_KP_Divide:      return SAPP_KEYCODE_KP_DIVIDE;
        case XK_KP_Multiply:    return SAPP_KEYCODE_KP_MULTIPLY;
        case XK_KP_Subtract:    return SAPP_KEYCODE_KP_SUBTRACT;
        case XK_KP_Add:         return SAPP_KEYCODE_KP_ADD;

        case XK_KP_Insert:      return SAPP_KEYCODE_KP_0;
        case XK_KP_End:         return SAPP_KEYCODE_KP_1;
        case XK_KP_Down:        return SAPP_KEYCODE_KP_2;
        case XK_KP_Page_Down:   return SAPP_KEYCODE_KP_3;
        case XK_KP_Left:        return SAPP_KEYCODE_KP_4;
        case XK_KP_Begin:       return SAPP_KEYCODE_KP_5;
        case XK_KP_Right:       return SAPP_KEYCODE_KP_6;
        case XK_KP_Home:        return SAPP_KEYCODE_KP_7;
        case XK_KP_Up:          return SAPP_KEYCODE_KP_8;
        case XK_KP_Page_Up:     return SAPP_KEYCODE_KP_9;
        case XK_KP_Delete:      return SAPP_KEYCODE_KP_DECIMAL;
        case XK_KP_Equal:       return SAPP_KEYCODE_KP_EQUAL;
        case XK_KP_Enter:       return SAPP_KEYCODE_KP_ENTER;

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
        case XK_less:           return SAPP_KEYCODE_WORLD_1; /* At least in some layouts... */
        default:                return SAPP_KEYCODE_INVALID;
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
                SOKOL_LOG("sokol_app.h: dropped file URI doesn't start with file://");
                err = true;
                break;
            }
        }
        else if (src_chr == '\r') {
            // skip
        }
        else if (src_chr == '\n') {
            src_chr = 0;
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
            // a percent-encoded byte (most like UTF-8 multibyte sequence)
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
                SOKOL_LOG("sokol_app.h: dropped file path too long (sapp_desc.max_dropped_file_path_length)");
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

// XLib manual says keycodes are in the range [8, 255] inclusive.
// https://tronche.com/gui/x/xlib/input/keyboard-encoding.html
static bool _sapp_x11_keycodes[256];

_SOKOL_PRIVATE void _sapp_x11_process_event(XEvent* event) {
    Bool filtered = XFilterEvent(event, None);
    switch (event->type) {
        case GenericEvent:
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
            break;
        case FocusIn:
            // NOTE: ingnoring NotifyGrab and NotifyUngrab is same behaviour as GLFW
            if ((event->xfocus.mode != NotifyGrab) && (event->xfocus.mode != NotifyUngrab)) {
                _sapp_x11_app_event(SAPP_EVENTTYPE_FOCUSED);
            }
            break;
        case FocusOut:
            /* if focus is lost for any reason, and we're in mouse locked mode, disable mouse lock */
            if (_sapp.mouse.locked) {
                _sapp_x11_lock_mouse(false);
            }
            // NOTE: ingnoring NotifyGrab and NotifyUngrab is same behaviour as GLFW
            if ((event->xfocus.mode != NotifyGrab) && (event->xfocus.mode != NotifyUngrab)) {
                _sapp_x11_app_event(SAPP_EVENTTYPE_UNFOCUSED);
            }
            break;
        case KeyPress:
            {
                int keycode = (int)event->xkey.keycode;
                const sapp_keycode key = _sapp_x11_translate_key(keycode);
                bool repeat = _sapp_x11_keycodes[keycode & 0xFF];
                _sapp_x11_keycodes[keycode & 0xFF] = true;
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
            break;
        case KeyRelease:
            {
                int keycode = (int)event->xkey.keycode;
                const sapp_keycode key = _sapp_x11_translate_key(keycode);
                _sapp_x11_keycodes[keycode & 0xFF] = false;
                if (key != SAPP_KEYCODE_INVALID) {
                    uint32_t mods = _sapp_x11_mods(event->xkey.state);
                    // X11 doesn't clear modifier bit on key up, so emulate that
                    mods &= ~_sapp_x11_key_modifier_bit(key);
                    _sapp_x11_key_event(SAPP_EVENTTYPE_KEY_UP, key, false, mods);
                }
            }
            break;
        case ButtonPress:
            {
                const sapp_mousebutton btn = _sapp_x11_translate_button(event);
                uint32_t mods = _sapp_x11_mods(event->xbutton.state);
                // X11 doesn't set modifier bit on button down, so emulate that
                mods |= _sapp_x11_button_modifier_bit(btn);
                if (btn != SAPP_MOUSEBUTTON_INVALID) {
                    _sapp_x11_mouse_event(SAPP_EVENTTYPE_MOUSE_DOWN, btn, mods);
                    _sapp.x11.mouse_buttons |= (1 << btn);
                }
                else {
                    /* might be a scroll event */
                    switch (event->xbutton.button) {
                        case 4: _sapp_x11_scroll_event(0.0f, 1.0f, mods); break;
                        case 5: _sapp_x11_scroll_event(0.0f, -1.0f, mods); break;
                        case 6: _sapp_x11_scroll_event(1.0f, 0.0f, mods); break;
                        case 7: _sapp_x11_scroll_event(-1.0f, 0.0f, mods); break;
                    }
                }
            }
            break;
        case ButtonRelease:
            {
                const sapp_mousebutton btn = _sapp_x11_translate_button(event);
                if (btn != SAPP_MOUSEBUTTON_INVALID) {
                    uint32_t mods = _sapp_x11_mods(event->xbutton.state);
                    // X11 doesn't clear modifier bit on button up, so emulate that
                    mods &= ~_sapp_x11_button_modifier_bit(btn);
                    _sapp_x11_mouse_event(SAPP_EVENTTYPE_MOUSE_UP, btn, mods);
                    _sapp.x11.mouse_buttons &= ~(1 << btn);
                }
            }
            break;
        case EnterNotify:
            /* don't send enter/leave events while mouse button held down */
            if (0 == _sapp.x11.mouse_buttons) {
                _sapp_x11_mouse_event(SAPP_EVENTTYPE_MOUSE_ENTER, SAPP_MOUSEBUTTON_INVALID, _sapp_x11_mods(event->xcrossing.state));
            }
            break;
        case LeaveNotify:
            if (0 == _sapp.x11.mouse_buttons) {
                _sapp_x11_mouse_event(SAPP_EVENTTYPE_MOUSE_LEAVE, SAPP_MOUSEBUTTON_INVALID, _sapp_x11_mods(event->xcrossing.state));
            }
            break;
        case MotionNotify:
            if (!_sapp.mouse.locked) {
                const float new_x = (float) event->xmotion.x;
                const float new_y = (float) event->xmotion.y;
                if (_sapp.mouse.pos_valid) {
                    _sapp.mouse.dx = new_x - _sapp.mouse.x;
                    _sapp.mouse.dy = new_y - _sapp.mouse.y;
                }
                _sapp.mouse.x = new_x;
                _sapp.mouse.y = new_y;
                _sapp.mouse.pos_valid = true;
                _sapp_x11_mouse_event(SAPP_EVENTTYPE_MOUSE_MOVE, SAPP_MOUSEBUTTON_INVALID, _sapp_x11_mods(event->xmotion.state));
            }
            break;
        case ConfigureNotify:
            if ((event->xconfigure.width != _sapp.window_width) || (event->xconfigure.height != _sapp.window_height)) {
                _sapp.window_width = event->xconfigure.width;
                _sapp.window_height = event->xconfigure.height;
                _sapp.framebuffer_width = _sapp.window_width;
                _sapp.framebuffer_height = _sapp.window_height;
                _sapp_x11_app_event(SAPP_EVENTTYPE_RESIZED);
            }
            break;
        case PropertyNotify:
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
            break;
        case ClientMessage:
            if (filtered) {
                return;
            }
            if (event->xclient.message_type == _sapp.x11.WM_PROTOCOLS) {
                const Atom protocol = (Atom)event->xclient.data.l[0];
                if (protocol == _sapp.x11.WM_DELETE_WINDOW) {
                    _sapp.quit_requested = true;
                }
            }
            else if (event->xclient.message_type == _sapp.x11.xdnd.XdndEnter) {
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
                }
                else {
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
            }
            else if (event->xclient.message_type == _sapp.x11.xdnd.XdndDrop) {
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
                }
                else if (_sapp.x11.xdnd.version >= 2) {
                    XEvent reply;
                    memset(&reply, 0, sizeof(reply));
                    reply.type = ClientMessage;
                    reply.xclient.window = _sapp.x11.window;
                    reply.xclient.message_type = _sapp.x11.xdnd.XdndFinished;
                    reply.xclient.format = 32;
                    reply.xclient.data.l[0] = (long)_sapp.x11.window;
                    reply.xclient.data.l[1] = 0;    // drag was rejected
                    reply.xclient.data.l[2] = None;
                    XSendEvent(_sapp.x11.display, _sapp.x11.xdnd.source, False, NoEventMask, &reply);
                    XFlush(_sapp.x11.display);
                }
            }
            else if (event->xclient.message_type == _sapp.x11.xdnd.XdndPosition) {
                /* drag operation has moved over the window
                   FIXME: we could track the mouse position here, but
                   this isn't implemented on other platforms either so far
                */
                if (_sapp.x11.xdnd.version > _SAPP_X11_XDND_VERSION) {
                    return;
                }
                XEvent reply;
                memset(&reply, 0, sizeof(reply));
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
            break;
        case SelectionNotify:
            if (event->xselection.property == _sapp.x11.xdnd.XdndSelection) {
                char* data = 0;
                uint32_t result = _sapp_x11_get_window_property(event->xselection.requestor,
                                                                event->xselection.property,
                                                                event->xselection.target,
                                                                (unsigned char**) &data);
                if (_sapp.drop.enabled && result) {
                    if (_sapp_x11_parse_dropped_files_list(data)) {
                        if (_sapp_events_enabled()) {
                            _sapp_init_event(SAPP_EVENTTYPE_FILES_DROPPED);
                            _sapp_call_event(&_sapp.event);
                        }
                    }
                }
                if (_sapp.x11.xdnd.version >= 2) {
                    XEvent reply;
                    memset(&reply, 0, sizeof(reply));
                    reply.type = ClientMessage;
                    reply.xclient.window = _sapp.x11.window;
                    reply.xclient.message_type = _sapp.x11.xdnd.XdndFinished;
                    reply.xclient.format = 32;
                    reply.xclient.data.l[0] = (long)_sapp.x11.window;
                    reply.xclient.data.l[1] = result;
                    reply.xclient.data.l[2] = (long)_sapp.x11.xdnd.XdndActionCopy;
                    XSendEvent(_sapp.x11.display, _sapp.x11.xdnd.source, False, NoEventMask, &reply);
                    XFlush(_sapp.x11.display);
                }
            }
            break;
        case DestroyNotify:
            break;
    }
}

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
        _sapp_fail("XOpenDisplay() failed!\n");
    }
    _sapp.x11.screen = DefaultScreen(_sapp.x11.display);
    _sapp.x11.root = DefaultRootWindow(_sapp.x11.display);
    XkbSetDetectableAutoRepeat(_sapp.x11.display, true, NULL);
    _sapp_x11_query_system_dpi();
    _sapp.dpi_scale = _sapp.x11.dpi / 96.0f;
    _sapp_x11_init_extensions();
    _sapp_x11_create_cursors();
    _sapp_glx_init();
    Visual* visual = 0;
    int depth = 0;
    _sapp_glx_choose_visual(&visual, &depth);
    _sapp_x11_create_window(visual, depth);
    _sapp_glx_create_context();
    sapp_set_icon(&desc->icon);
    _sapp.valid = true;
    _sapp_x11_show_window();
    if (_sapp.fullscreen) {
        _sapp_x11_set_fullscreen(true);
    }
    _sapp_x11_query_window_size();
    _sapp_glx_swapinterval(_sapp.swap_interval);
    XFlush(_sapp.x11.display);
    while (!_sapp.quit_ordered) {
        _sapp_timing_measure(&_sapp.timing);
        _sapp_glx_make_current();
        int count = XPending(_sapp.x11.display);
        while (count--) {
            XEvent event;
            XNextEvent(_sapp.x11.display, &event);
            _sapp_x11_process_event(&event);
        }
        _sapp_frame();
        _sapp_glx_swap_buffers();
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
    _sapp_glx_destroy_context();
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

/*== PUBLIC API FUNCTIONS ====================================================*/
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
    #elif defined(_SAPP_UWP)
        _sapp_uwp_run(desc);
    #elif defined(_SAPP_LINUX)
        _sapp_linux_run(desc);
    #else
        // calling sapp_run() directly is not supported on Android)
        _sapp_fail("sapp_run() not supported on this platform!");
    #endif
}

/* this is just a stub so the linker doesn't complain */
sapp_desc sokol_main(int argc, char* argv[]) {
    _SOKOL_UNUSED(argc);
    _SOKOL_UNUSED(argv);
    sapp_desc desc;
    memset(&desc, 0, sizeof(desc));
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
        switch (_sapp.emsc.wgpu.render_format) {
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

SOKOL_API_IMPL bool sapp_gles2(void) {
    return _sapp.gles2_fallback;
}

SOKOL_API_IMPL void sapp_show_keyboard(bool show) {
    #if defined(_SAPP_IOS)
    _sapp_ios_show_keyboard(show);
    #elif defined(_SAPP_EMSCRIPTEN)
    _sapp_emsc_show_keyboard(show);
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
    #elif defined(_SAPP_UWP)
    _sapp_uwp_toggle_fullscreen();
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
        #elif defined(_SAPP_UWP)
        _sapp_uwp_update_cursor(_sapp.mouse.current_cursor, show);
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
        #elif defined(_SAPP_UWP)
        _sapp_uwp_update_cursor(cursor, _sapp.mouse.shown);
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
    SOKOL_ASSERT(_sapp.clipboard.enabled);
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
    SOKOL_ASSERT(_sapp.clipboard.enabled);
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
    SOKOL_ASSERT(request->buffer_ptr);
    SOKOL_ASSERT(request->buffer_size > 0);
    #if defined(_SAPP_EMSCRIPTEN)
        const int index = request->dropped_file_index;
        sapp_html5_fetch_error error_code = SAPP_HTML5_FETCH_ERROR_NO_ERROR;
        if ((index < 0) || (index >= _sapp.drop.num_files)) {
            error_code = SAPP_HTML5_FETCH_ERROR_OTHER;
        }
        if (sapp_html5_get_dropped_file_size(index) > request->buffer_size) {
            error_code = SAPP_HTML5_FETCH_ERROR_BUFFER_TOO_SMALL;
        }
        if (SAPP_HTML5_FETCH_ERROR_NO_ERROR != error_code) {
            _sapp_emsc_invoke_fetch_cb(index,
                false, // success
                (int)error_code,
                request->callback,
                0, // fetched_size
                request->buffer_ptr,
                request->buffer_size,
                request->user_data);
        }
        else {
            sapp_js_fetch_dropped_file(index,
                request->callback,
                request->buffer_ptr,
                request->buffer_size,
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

SOKOL_API_IMPL const void* sapp_metal_get_renderpass_descriptor(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(SOKOL_METAL)
        #if defined(_SAPP_MACOS)
            const void* obj = (__bridge const void*) [_sapp.macos.view currentRenderPassDescriptor];
        #else
            const void* obj = (__bridge const void*) [_sapp.ios.view currentRenderPassDescriptor];
        #endif
        SOKOL_ASSERT(obj);
        return obj;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_metal_get_drawable(void) {
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

SOKOL_API_IMPL const void* sapp_d3d11_get_render_target_view(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(SOKOL_D3D11)
        if (_sapp.d3d11.msaa_rtv) {
            return _sapp.d3d11.msaa_rtv;
        }
        else {
            return _sapp.d3d11.rtv;
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
        return (const void*) _sapp.emsc.wgpu.device;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_wgpu_get_render_view(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(_SAPP_EMSCRIPTEN) && defined(SOKOL_WGPU)
        if (_sapp.sample_count > 1) {
            return (const void*) _sapp.emsc.wgpu.msaa_view;
        }
        else {
            return (const void*) _sapp.emsc.wgpu.swapchain_view;
        }
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_wgpu_get_resolve_view(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(_SAPP_EMSCRIPTEN) && defined(SOKOL_WGPU)
        if (_sapp.sample_count > 1) {
            return (const void*) _sapp.emsc.wgpu.swapchain_view;
        }
        else {
            return 0;
        }
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_wgpu_get_depth_stencil_view(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(_SAPP_EMSCRIPTEN) && defined(SOKOL_WGPU)
        return (const void*) _sapp.emsc.wgpu.depth_stencil_view;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_android_get_native_activity(void) {
    SOKOL_ASSERT(_sapp.valid);
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
