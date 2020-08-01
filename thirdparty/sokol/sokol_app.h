#ifndef SOKOL_APP_INCLUDED
/*
    sokol_app.h -- cross-platform application wrapper

    Project URL: https://github.com/floooh/sokol

    Do this:
        #define SOKOL_IMPL
    before you include this file in *one* C or C++ file to create the
    implementation.

    Optionally provide the following defines with your own implementations:

    SOKOL_ASSERT(c)     - your own assert macro (default: assert(c))
    SOKOL_LOG(msg)      - your own logging function (default: puts(msg))
    SOKOL_UNREACHABLE() - a guard macro for unreachable code (default: assert(false))
    SOKOL_ABORT()       - called after an unrecoverable error (default: abort())
    SOKOL_WIN32_FORCE_MAIN  - define this on Win32 to use a main() entry point instead of WinMain
    SOKOL_NO_ENTRY      - define this if sokol_app.h shouldn't "hijack" the main() function
    SOKOL_API_DECL      - public function declaration prefix (default: extern)
    SOKOL_API_IMPL      - public function implementation prefix (default: -)
    SOKOL_CALLOC        - your own calloc function (default: calloc(n, s))
    SOKOL_FREE          - your own free function (default: free(p))

    Optionally define the following to force debug checks and validations
    even in release mode:

    SOKOL_DEBUG         - by default this is defined if _DEBUG is defined

    If sokol_app.h is compiled as a DLL, define the following before
    including the declaration or implementation:

    SOKOL_DLL

    On Windows, SOKOL_DLL will define SOKOL_API_DECL as __declspec(dllexport)
    or __declspec(dllimport) as needed.

    Portions of the Windows and Linux GL initialization and event code have been
    taken from GLFW (http://www.glfw.org/)

    iOS onscreen keyboard support 'inspired' by libgdx.

    If you use sokol_app.h together with sokol_gfx.h, include both headers
    in the implementation source file, and include sokol_app.h before
    sokol_gfx.h since sokol_app.h will also include the required 3D-API
    headers.

    On Windows, a minimal 'GL header' and function loader is integrated which
    contains just enough of GL for sokol_gfx.h. If you want to use your own
    GL header-generator/loader instead, define SOKOL_WIN32_NO_GL_LOADER
    before including the implementation part of sokol_app.h.

    For example code, see https://github.com/floooh/sokol-samples/tree/master/sapp

    FEATURE OVERVIEW
    ================
    sokol_app.h provides a minimalistic cross-platform API which
    implements the 'application-wrapper' parts of a 3D application:

    - a common application entry function
    - creates a window and 3D-API context/device with a 'default framebuffer'
    - makes the rendered frame visible
    - provides keyboard-, mouse- and low-level touch-events
    - platforms: MacOS, iOS, HTML5, Win32, Linux, Android (RaspberryPi)
    - 3D-APIs: Metal, D3D11, GL3.2, GLES2, GLES3, WebGL, WebGL2

    FEATURE/PLATFORM MATRIX
    =======================
                        | Windows | macOS | Linux |  iOS  | Android | Raspi | HTML5
    --------------------+---------+-------+-------+-------+---------+-------+-------
    gl 3.x              | YES     | YES   | YES   | ---   | ---     | ---   | ---
    gles2/webgl         | ---     | ---   | ---   | YES   | YES     | TODO  | YES
    gles3/webgl2        | ---     | ---   | ---   | YES   | YES     | ---   | YES
    metal               | ---     | YES   | ---   | YES   | ---     | ---   | ---
    d3d11               | YES     | ---   | ---   | ---   | ---     | ---   | ---
    KEY_DOWN            | YES     | YES   | YES   | SOME  | TODO    | TODO  | YES
    KEY_UP              | YES     | YES   | YES   | SOME  | TODO    | TODO  | YES
    CHAR                | YES     | YES   | YES   | YES   | TODO    | TODO  | YES
    MOUSE_DOWN          | YES     | YES   | YES   | ---   | ---     | TODO  | YES
    MOUSE_UP            | YES     | YES   | YES   | ---   | ---     | TODO  | YES
    MOUSE_SCROLL        | YES     | YES   | YES   | ---   | ---     | TODO  | YES
    MOUSE_MOVE          | YES     | YES   | YES   | ---   | ---     | TODO  | YES
    MOUSE_ENTER         | YES     | YES   | YES   | ---   | ---     | TODO  | YES
    MOUSE_LEAVE         | YES     | YES   | YES   | ---   | ---     | TODO  | YES
    TOUCHES_BEGAN       | ---     | ---   | ---   | YES   | YES     | ---   | YES
    TOUCHES_MOVED       | ---     | ---   | ---   | YES   | YES     | ---   | YES
    TOUCHES_ENDED       | ---     | ---   | ---   | YES   | YES     | ---   | YES
    TOUCHES_CANCELLED   | ---     | ---   | ---   | YES   | YES     | ---   | YES
    RESIZED             | YES     | YES   | YES   | YES   | YES     | ---   | YES
    ICONIFIED           | YES     | YES   | YES   | ---   | ---     | ---   | ---
    RESTORED            | YES     | YES   | YES   | ---   | ---     | ---   | ---
    SUSPENDED           | ---     | ---   | ---   | YES   | YES     | ---   | TODO
    RESUMED             | ---     | ---   | ---   | YES   | YES     | ---   | TODO
    QUIT_REQUESTED      | YES     | YES   | YES   | ---   | ---     | TODO  | ---
    UPDATE_CURSOR       | YES     | YES   | TODO  | ---   | ---     | ---   | TODO
    IME                 | TODO    | TODO? | TODO  | ???   | TODO    | ???   | ???
    key repeat flag     | YES     | YES   | YES   | ---   | ---     | TODO  | YES
    windowed            | YES     | YES   | YES   | ---   | ---     | TODO  | YES
    fullscreen          | YES     | YES   | TODO  | YES   | YES     | TODO  | ---
    pointer lock        | TODO    | TODO  | TODO  | ---   | ---     | TODO  | TODO
    screen keyboard     | ---     | ---   | ---   | YES   | TODO    | ---   | YES
    swap interval       | YES     | YES   | YES   | YES   | TODO    | TODO  | YES
    high-dpi            | YES     | YES   | TODO  | YES   | YES     | TODO  | YES
    clipboard           | YES     | YES   | TODO  | ---   | ---     | ---   | YES

    TODO
    ====
    - Linux clipboard support
    - document sapp_consume_event()
    - sapp_consume_event() on non-web platforms?

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
            The event callback is mainly for input handling, but in the
            future may also be used to communicate other types of events
            to the application. Keep the event_cb struct member zero-initialized
            if your application doesn't require event handling.
        .fail_cb (void (*)(const char* msg))
            The fail callback is called when a fatal error is encountered
            during start which doesn't allow the program to continue.
            Providing a callback here gives you a chance to show an error message
            to the user. The default behaviour is SOKOL_LOG(msg)

        As you can see, those 'standard callbacks' don't have a user_data
        argument, so any data that needs to be preserved between callbacks
        must live in global variables. If you're allergic to global variables
        or cannot use them for other reasons, an alternative set of callbacks
        can be defined in sapp_desc, together with a user_data pointer:

        .user_data (void*)
            The user-data argument for the callbacks below
        .init_userdata_cb (void (*)(void* user_data))
        .frame_userdata_cb (void (*)(void* user_data))
        .cleanup_userdata_cb (void (*)(void* user_data))
        .event_cb (void(*)(const sapp_event* event, void* user_data))
        .fail_cb (void(*)(const char* msg, void* user_data))
            These are the user-data versions of the callback functions. You
            can mix those with the standard callbacks that don't have the
            user_data argument.

        The function sapp_userdata() can be used to query the user_data
        pointer provided in the sapp_desc struct.

        You can call sapp_query_desc() to get a copy of the
        original sapp_desc structure.

        NOTE that there's also an alternative compile mode where sokol_app.h
        doesn't "hijack" the main() function. Search below for SOKOL_NO_ENTRY.

    --- Implement the initialization callback function (init_cb), this is called
        once after the rendering surface, 3D API and swap chain have been
        initialized by sokol_app. All sokol-app functions can be called
        from inside the initialization callback, the most useful functions
        at this point are:

        int sapp_width(void)
            Returns the current width of the default framebuffer, this may change
            from one frame to the next.
        int sapp_height(void)
            Likewise, returns the current height of the default framebuffer.

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
            Objective-C ids converted with an ARC __bridge cast so that
            they ids can be tunnel through C code. Also note that the returned
            pointers to the renderpass-descriptor and drawable may change from one
            frame to the next, only the Metal device object is guaranteed to
            stay the same.

        const void* sapp_macos_get_window(void)
            On macOS, get the NSWindow object pointer, otherwise a null pointer.
            Before being used as Objective-C object, the void* must be converted
            back with an ARC __bridge cast.

        const void* sapp_ios_get_window(void)
            On iOS, get the UIWindow object pointer, otherwise a null pointer.
            Before being used as Objective-C object, the void* must be converted
            back with an ARC __bridge cast.

        const void* sapp_win32_get_hwnd(void)
            On Windows, get the window's HWND, otherwise a null pointer. The
            HWND has been cast to a void pointer in order to be tunneled
            through code which doesn't include Windows.h.

        const void* sapp_d3d11_get_device(void);
        const void* sapp_d3d11_get_device_context(void);
        const void* sapp_d3d11_get_render_target_view(void);
        const void* sapp_d3d11_get_depth_stencil_view(void);
            Similar to the sapp_metal_* functions, the sapp_d3d11_* functions
            return pointers to D3D11 API objects required for rendering,
            only if the D3D11 backend has been selected. Otherwise they
            return a null pointer. Note that the returned pointers to the
            render-target-view and depth-stencil-view may change from one
            frame to the next!

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

    --- Implement the cleanup-callback function, this is called once
        after the user quits the application (see the section
        "APPLICATION QUIT" for detailed information on quitting
        behaviour, and how to intercept a pending quit (for instance to show a
        "Really Quit?" dialog box). Note that the cleanup-callback isn't
        called on the web and mobile platforms.

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
    size if 8 KBytes. Strings that don't fit into the clipboard buffer
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
    to obtain the updated UTF-8 encoded text.

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
    Without special quit handling, a sokol_app.h application will exist
    'gracefully' when the user clicks the window close-button. 'Graceful
    exit' means that the application-provided cleanup callback will be
    called.

    This 'graceful exit' is only supported on native desktop platforms, on
    the web and mobile platforms an application may be terminated at any time
    by the user or browser/OS runtime environment without a chance to run
    custom shutdown code.

    On the web platform, you can call the following function to let the
    browser open a standard popup dialog before the user wants to leave a site:

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

    On native desktop platforms, sokol_app.h provides more control over the
    application-quit-process. It's possible to initiate a 'programmatic quit'
    from the application code, and a quit initiated by the application user
    can be intercepted (for instance to show a custom dialog box).

    This 'programmatic quit protocol' is implemented trough 3 functions
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

    TEMP NOTE DUMP
    ==============
    - onscreen keyboard support on Android requires Java :(, should we even bother?
    - sapp_desc needs a bool whether to initialize depth-stencil surface
    - GL context initialization needs more control (at least what GL version to initialize)
    - application icon
    - mouse pointer visibility(?)
    - the UPDATE_CURSOR event currently behaves differently between Win32 and OSX
      (Win32 sends the event each frame when the mouse moves and is inside the window
      client area, OSX sends it only once when the mouse enters the client area)
    - the Android implementation calls cleanup_cb() and destroys the egl context in onDestroy
      at the latest but should do it earlier, in onStop, as an app is "killable" after onStop
      on Android Honeycomb and later (it can't be done at the moment as the app may be started
      again after onStop and the sokol lifecycle does not yet handle context teardown/bringup)

    FIXME: ERROR HANDLING (this will need an error callback function)

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
#include <stdint.h>
#include <stdbool.h>

#ifndef SOKOL_API_DECL
#if defined(_WIN32) && defined(SOKOL_DLL) && defined(SOKOL_IMPL)
#define SOKOL_API_DECL __declspec(dllexport)
#elif defined(_WIN32) && defined(SOKOL_DLL)
#define SOKOL_API_DECL __declspec(dllimport)
#else
#define SOKOL_API_DECL extern
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

enum {
    SAPP_MAX_TOUCHPOINTS = 8,
    SAPP_MAX_MOUSEBUTTONS = 3,
    SAPP_MAX_KEYCODES = 512,
};

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
    SAPP_EVENTTYPE_SUSPENDED,
    SAPP_EVENTTYPE_RESUMED,
    SAPP_EVENTTYPE_UPDATE_CURSOR,
    SAPP_EVENTTYPE_QUIT_REQUESTED,
    SAPP_EVENTTYPE_CLIPBOARD_PASTED,
    _SAPP_EVENTTYPE_NUM,
    _SAPP_EVENTTYPE_FORCE_U32 = 0x7FFFFFFF
} sapp_event_type;

/* key codes are the same names and values as GLFW */
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

typedef struct sapp_touchpoint {
    uintptr_t identifier;
    float pos_x;
    float pos_y;
    bool changed;
} sapp_touchpoint;

typedef enum sapp_mousebutton {
    SAPP_MOUSEBUTTON_INVALID = -1,
    SAPP_MOUSEBUTTON_LEFT = 0,
    SAPP_MOUSEBUTTON_RIGHT = 1,
    SAPP_MOUSEBUTTON_MIDDLE = 2,
} sapp_mousebutton;

enum {
    SAPP_MODIFIER_SHIFT = (1<<0),
    SAPP_MODIFIER_CTRL = (1<<1),
    SAPP_MODIFIER_ALT = (1<<2),
    SAPP_MODIFIER_SUPER = (1<<3)
};

typedef struct sapp_event {
    uint64_t frame_count;
    sapp_event_type type;
    sapp_keycode key_code;
    uint32_t char_code;
    bool key_repeat;
    uint32_t modifiers;
    sapp_mousebutton mouse_button;
    float mouse_x;
    float mouse_y;
    float scroll_x;
    float scroll_y;
    int num_touches;
    sapp_touchpoint touches[SAPP_MAX_TOUCHPOINTS];
    int window_width;
    int window_height;
    int framebuffer_width;
    int framebuffer_height;
} sapp_event;

typedef struct sapp_desc {
    void (*init_cb)(void);                  /* these are the user-provided callbacks without user data */
    void (*frame_cb)(void);
    void (*cleanup_cb)(void);
    void (*event_cb)(const sapp_event*);
    void (*fail_cb)(const char*);

    void* user_data;                        /* these are the user-provided callbacks with user data */
    void (*init_userdata_cb)(void*);
    void (*frame_userdata_cb)(void*);
    void (*cleanup_userdata_cb)(void*);
    void (*event_userdata_cb)(const sapp_event*, void*);
    void (*fail_userdata_cb)(const char*, void*);

    int width;                          /* the preferred width of the window / canvas */
    int height;                         /* the preferred height of the window / canvas */
    int sample_count;                   /* MSAA sample count */
    int swap_interval;                  /* the preferred swap interval (ignored on some platforms) */
    bool high_dpi;                      /* whether the rendering canvas is full-resolution on HighDPI displays */
    bool fullscreen;                    /* whether the window should be created in fullscreen mode */
    bool alpha;                         /* whether the framebuffer should have an alpha channel (ignored on some platforms) */
    const char* window_title;           /* the window title as UTF-8 encoded string */
    bool user_cursor;                   /* if true, user is expected to manage cursor image in SAPP_EVENTTYPE_UPDATE_CURSOR */
    bool enable_clipboard;              /* enable clipboard access, default is false */
    int clipboard_size;                 /* max size of clipboard content in bytes */

    const char* html5_canvas_name;      /* the name (id) of the HTML5 canvas element, default is "canvas" */
    bool html5_canvas_resize;           /* if true, the HTML5 canvas size is set to sapp_desc.width/height, otherwise canvas size is tracked */
    bool html5_preserve_drawing_buffer; /* HTML5 only: whether to preserve default framebuffer content between frames */
    bool html5_premultiplied_alpha;     /* HTML5 only: whether the rendered pixels use premultiplied alpha convention */
    bool html5_ask_leave_site;          /* initial state of the internal html5_ask_leave_site flag (see sapp_html5_ask_leave_site()) */
    bool ios_keyboard_resizes_canvas;   /* if true, showing the iOS keyboard shrinks the canvas */
    bool gl_force_gles2;                /* if true, setup GLES2/WebGL even if GLES3/WebGL2 is available */
} sapp_desc;

/* user-provided functions */
extern sapp_desc sokol_main(int argc, char* argv[]);

/* returns true after sokol-app has been initialized */
SOKOL_API_DECL bool sapp_isvalid(void);
/* returns the current framebuffer width in pixels */
SOKOL_API_DECL int sapp_width(void);
/* returns the current framebuffer height in pixels */
SOKOL_API_DECL int sapp_height(void);
/* returns true when high_dpi was requested and actually running in a high-dpi scenario */
SOKOL_API_DECL bool sapp_high_dpi(void);
/* returns the dpi scaling factor (window pixels to framebuffer pixels) */
SOKOL_API_DECL float sapp_dpi_scale(void);
/* show or hide the mobile device onscreen keyboard */
SOKOL_API_DECL void sapp_show_keyboard(bool visible);
/* return true if the mobile device onscreen keyboard is currently shown */
SOKOL_API_DECL bool sapp_keyboard_shown(void);
/* show or hide the mouse cursor */
SOKOL_API_DECL void sapp_show_mouse(bool visible);
/* show or hide the mouse cursor */
SOKOL_API_DECL bool sapp_mouse_shown();
/* return the userdata pointer optionally provided in sapp_desc */
SOKOL_API_DECL void* sapp_userdata(void);
/* return a copy of the sapp_desc structure */
SOKOL_API_DECL sapp_desc sapp_query_desc(void);
/* initiate a "soft quit" (sends SAPP_EVENTTYPE_QUIT_REQUESTED) */
SOKOL_API_DECL void sapp_request_quit(void);
/* cancel a pending quit (when SAPP_EVENTTYPE_QUIT_REQUESTED has been received) */
SOKOL_API_DECL void sapp_cancel_quit(void);
/* intiate a "hard quit" (quit application without sending SAPP_EVENTTYPE_QUIT_REQUSTED) */
SOKOL_API_DECL void sapp_quit(void);
/* call from inside event callback to consume the current event (don't forward to platform) */
SOKOL_API_DECL void sapp_consume_event(void);
/* get the current frame counter (for comparison with sapp_event.frame_count) */
SOKOL_API_DECL uint64_t sapp_frame_count(void);
/* write string into clipboard */
SOKOL_API_DECL void sapp_set_clipboard_string(const char* str);
/* read string from clipboard (usually during SAPP_EVENTTYPE_CLIPBOARD_PASTED) */
SOKOL_API_DECL const char* sapp_get_clipboard_string(void);

/* special run-function for SOKOL_NO_ENTRY (in standard mode this is an empty stub) */
SOKOL_API_DECL int sapp_run(const sapp_desc* desc);

/* GL: return true when GLES2 fallback is active (to detect fallback from GLES3) */
SOKOL_API_DECL bool sapp_gles2(void);

/* HTML5: enable or disable the hardwired "Leave Site?" dialog box */
SOKOL_API_DECL void sapp_html5_ask_leave_site(bool ask);

/* Metal: get ARC-bridged pointer to Metal device object */
SOKOL_API_DECL const void* sapp_metal_get_device(void);
/* Metal: get ARC-bridged pointer to this frame's renderpass descriptor */
SOKOL_API_DECL const void* sapp_metal_get_renderpass_descriptor(void);
/* Metal: get ARC-bridged pointer to current drawable */
SOKOL_API_DECL const void* sapp_metal_get_drawable(void);
/* macOS: get ARC-bridged pointer to macOS NSWindow */
SOKOL_API_DECL const void* sapp_macos_get_window(void);
SOKOL_API_DECL void sapp_macos_set_title(const char* title);
/* iOS: get ARC-bridged pointer to iOS UIWindow */
SOKOL_API_DECL const void* sapp_ios_get_window(void);

/* D3D11: get pointer to ID3D11Device object */
SOKOL_API_DECL const void* sapp_d3d11_get_device(void);
/* D3D11: get pointer to ID3D11DeviceContext object */
SOKOL_API_DECL const void* sapp_d3d11_get_device_context(void);
/* D3D11: get pointer to ID3D11RenderTargetView object */
SOKOL_API_DECL const void* sapp_d3d11_get_render_target_view(void);
/* D3D11: get pointer to ID3D11DepthStencilView */
SOKOL_API_DECL const void* sapp_d3d11_get_depth_stencil_view(void);
/* Win32: get the HWND window handle */
SOKOL_API_DECL const void* sapp_win32_get_hwnd(void);

/* Android: get native activity handle */
SOKOL_API_DECL const void* sapp_android_get_native_activity(void);

#ifdef __cplusplus
} /* extern "C" */
#endif
#endif // SOKOL_APP_INCLUDED

/*-- IMPLEMENTATION ----------------------------------------------------------*/
#ifdef SOKOL_IMPL
#define SOKOL_APP_IMPL_INCLUDED (1)

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable:4201)   /* nonstandard extension used: nameless struct/union */
#pragma warning(disable:4115)   /* named type definition in parentheses */
#pragma warning(disable:4054)   /* 'type cast': from function pointer */
#pragma warning(disable:4055)   /* 'type cast': from data pointer */
#pragma warning(disable:4505)   /* unreferenced local function has been removed */
#pragma warning(disable:4115)   /* /W4: 'ID3D11ModuleInstance': named type definition in parentheses (in d3d11.h) */
#endif

#include <string.h> /* memset */

/* check if the config defines are alright */
#if defined(__APPLE__)
    #if !__has_feature(objc_arc)
        #error "sokol_app.h requires ARC (Automatic Reference Counting) on MacOS and iOS"
    #endif
    #include <TargetConditionals.h>
    #if defined(TARGET_OS_IPHONE) && TARGET_OS_IPHONE
        /* iOS */
        #if !defined(SOKOL_METAL) && !defined(SOKOL_GLES3)
        #error("sokol_app.h: unknown 3D API selected for iOS, must be SOKOL_METAL or SOKOL_GLES3")
        #endif
    #else
        /* MacOS */
        #if !defined(SOKOL_METAL) && !defined(SOKOL_GLCORE33)
        #error("sokol_app.h: unknown 3D API selected for MacOS, must be SOKOL_METAL or SOKOL_GLCORE33")
        #endif
    #endif
#elif defined(__EMSCRIPTEN__)
    /* emscripten (asm.js or wasm) */
    #if !defined(SOKOL_GLES3) && !defined(SOKOL_GLES2)
    #error("sokol_app.h: unknown 3D API selected for emscripten, must be SOKOL_GLES3 or SOKOL_GLES2")
    #endif
#elif defined(_WIN32)
    /* Windows (D3D11 or GL) */
    #if !defined(SOKOL_D3D11) && !defined(SOKOL_GLCORE33)
    #error("sokol_app.h: unknown 3D API selected for Win32, must be SOKOL_D3D11 or SOKOL_GLCORE33")
    #endif
#elif defined(__ANDROID__)
    /* Android */
    #if !defined(SOKOL_GLES3) && !defined(SOKOL_GLES2)
    #error("sokol_app.h: unknown 3D API selected for Android, must be SOKOL_GLES3 or SOKOL_GLES2")
    #endif
    #if defined(SOKOL_NO_ENTRY)
    #error("sokol_app.h: SOKOL_NO_ENTRY is not supported on Android")
    #endif
#elif defined(__linux__) || defined(__unix__)
    /* Linux */
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
    #if defined(__GNUC__)
        #define _SOKOL_PRIVATE __attribute__((unused)) static
    #else
        #define _SOKOL_PRIVATE static
    #endif
#endif
#ifndef _SOKOL_UNUSED
    #define _SOKOL_UNUSED(x) (void)(x)
#endif

/* helper macros */
#define _sapp_def(val, def) (((val) == 0) ? (def) : (val))
#define _sapp_absf(a) (((a)<0.0f)?-(a):(a))

enum {
    _SAPP_MAX_TITLE_LENGTH = 128,
};

typedef struct {
    bool valid;
    int window_width;
    int window_height;
    int framebuffer_width;
    int framebuffer_height;
    int sample_count;
    int swap_interval;
    float dpi_scale;
    bool gles2_fallback;
    bool first_frame;
    bool init_called;
    bool cleanup_called;
    bool quit_requested;
    bool quit_ordered;
    bool event_consumed;
    const char* html5_canvas_name;
    bool html5_ask_leave_site;
    char window_title[_SAPP_MAX_TITLE_LENGTH];      /* UTF-8 */
    wchar_t window_title_wide[_SAPP_MAX_TITLE_LENGTH];   /* UTF-32 or UCS-2 */
    uint64_t frame_count;
    float mouse_x;
    float mouse_y;
    bool win32_mouse_tracked;
    bool onscreen_keyboard_shown;
    sapp_event event;
    sapp_desc desc;
    sapp_keycode keycodes[SAPP_MAX_KEYCODES];
    bool clipboard_enabled;
    int clipboard_size;
    char* clipboard;
} _sapp_state;
static _sapp_state _sapp;

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
    if (_sapp.init_called && !_sapp.cleanup_called) {
        if (_sapp.desc.frame_cb) {
            _sapp.desc.frame_cb();
        }
        else if (_sapp.desc.frame_userdata_cb) {
            _sapp.desc.frame_userdata_cb(_sapp.desc.user_data);
        }
    }
}

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

_SOKOL_PRIVATE void _sapp_strcpy(const char* src, char* dst, int max_len) {
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
    }
}

_SOKOL_PRIVATE void _sapp_init_state(const sapp_desc* desc) {
    memset(&_sapp, 0, sizeof(_sapp));
    _sapp.desc = *desc;
    _sapp.first_frame = true;
    _sapp.window_width = _sapp_def(_sapp.desc.width, 640);
    _sapp.window_height = _sapp_def(_sapp.desc.height, 480);
    _sapp.framebuffer_width = _sapp.window_width;
    _sapp.framebuffer_height = _sapp.window_height;
    _sapp.sample_count = _sapp_def(_sapp.desc.sample_count, 1);
    _sapp.swap_interval = _sapp_def(_sapp.desc.swap_interval, 1);
    _sapp.html5_canvas_name = _sapp_def(_sapp.desc.html5_canvas_name, "canvas");
    _sapp.html5_ask_leave_site = _sapp.desc.html5_ask_leave_site;
    _sapp.clipboard_enabled = _sapp.desc.enable_clipboard;
    if (_sapp.clipboard_enabled) {
        _sapp.clipboard_size = _sapp_def(_sapp.desc.clipboard_size, 8192);
        _sapp.clipboard = (char*) SOKOL_CALLOC(1, _sapp.clipboard_size);
    }
    if (_sapp.desc.window_title) {
        _sapp_strcpy(_sapp.desc.window_title, _sapp.window_title, sizeof(_sapp.window_title));
    }
    else {
        _sapp_strcpy("sokol_app", _sapp.window_title, sizeof(_sapp.window_title));
    }
    _sapp.dpi_scale = 1.0f;
}

_SOKOL_PRIVATE void _sapp_discard_state(void) {
    if (_sapp.clipboard_enabled) {
        SOKOL_ASSERT(_sapp.clipboard);
        SOKOL_FREE((void*)_sapp.clipboard);
    }
    memset(&_sapp, 0, sizeof(_sapp));
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

_SOKOL_PRIVATE void _sapp_frame(void) {
    if (_sapp.first_frame) {
        _sapp.first_frame = false;
        _sapp_call_init();
    }
    _sapp_call_frame();
    _sapp.frame_count++;
}

/*== MacOS/iOS ===============================================================*/

#if defined(__APPLE__)

/*== MacOS ===================================================================*/
#if defined(TARGET_OS_IPHONE) && !TARGET_OS_IPHONE

#if defined(SOKOL_METAL)
#import <Metal/Metal.h>
#import <MetalKit/MetalKit.h>
#elif defined(SOKOL_GLCORE33)
#ifndef GL_SILENCE_DEPRECATION
#define GL_SILENCE_DEPRECATION
#endif
#include <Cocoa/Cocoa.h>
#include <OpenGL/gl3.h>
#endif

@interface _sapp_macos_app_delegate : NSObject<NSApplicationDelegate>
@end
@interface _sapp_macos_window_delegate : NSObject<NSWindowDelegate>
@end
#if defined(SOKOL_METAL)
@interface _sapp_macos_mtk_view_dlg : NSObject<MTKViewDelegate>
@end
@interface _sapp_macos_view : MTKView
{
    NSTrackingArea* trackingArea;
}
@end
#elif defined(SOKOL_GLCORE33)
@interface _sapp_macos_view : NSOpenGLView
{
    NSTrackingArea* trackingArea;
}
- (void)timerFired:(id)sender;
- (void)prepareOpenGL;
- (void)drawRect:(NSRect)bounds;
@end
#endif

@interface MyWindow : NSWindow {
}
@end

@implementation MyWindow
- (BOOL)canBecomeKeyWindow {    return YES;}
@end


static MyWindow* _sapp_macos_window_obj;
static _sapp_macos_window_delegate* _sapp_macos_win_dlg_obj;
static _sapp_macos_app_delegate* _sapp_macos_app_dlg_obj;
static _sapp_macos_view* _sapp_view_obj;
#if defined(SOKOL_METAL)
static _sapp_macos_mtk_view_dlg* _sapp_macos_mtk_view_dlg_obj;
static id<MTLDevice> _sapp_mtl_device_obj;
#elif defined(SOKOL_GLCORE33)
static NSOpenGLPixelFormat* _sapp_macos_glpixelformat_obj;
static NSTimer* _sapp_macos_timer_obj;
#endif
static uint32_t _sapp_macos_flags_changed_store;

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

_SOKOL_PRIVATE void _sapp_run(const sapp_desc* desc) {
	//puts("Sokol run()");
    _sapp_init_state(desc);
    _sapp_macos_init_keytable();
    [NSApplication sharedApplication];
    NSApp.activationPolicy = NSApplicationActivationPolicyRegular;
    _sapp_macos_app_dlg_obj = [[_sapp_macos_app_delegate alloc] init];
    NSApp.delegate = _sapp_macos_app_dlg_obj;
    [NSApp activateIgnoringOtherApps:YES];
    [NSApp run];
    _sapp_discard_state();
}

/* MacOS entry function */
#if !defined(SOKOL_NO_ENTRY)
int main(int argc, char* argv[]) {
    sapp_desc desc = sokol_main(argc, argv);
    _sapp_run(&desc);
    return 0;
}
#endif /* SOKOL_NO_ENTRY */

_SOKOL_PRIVATE void _sapp_macos_update_dimensions(void) {
    #if defined(SOKOL_METAL)
        const CGSize fb_size = [_sapp_view_obj drawableSize];
        _sapp.framebuffer_width = fb_size.width;
        _sapp.framebuffer_height = fb_size.height;
    #elif defined(SOKOL_GLCORE33)
        const NSRect fb_rect = [_sapp_view_obj convertRectToBacking:[_sapp_view_obj frame]];
        _sapp.framebuffer_width = fb_rect.size.width;
        _sapp.framebuffer_height = fb_rect.size.height;
    #endif
    const NSRect bounds = [_sapp_view_obj bounds];
    _sapp.window_width = bounds.size.width;
    _sapp.window_height = bounds.size.height;
    SOKOL_ASSERT((_sapp.framebuffer_width > 0) && (_sapp.framebuffer_height > 0));
    _sapp.dpi_scale = (float)_sapp.framebuffer_width / (float)_sapp.window_width;
}

_SOKOL_PRIVATE void _sapp_macos_frame(void) {
    const NSPoint mouse_pos = [_sapp_macos_window_obj mouseLocationOutsideOfEventStream];
    _sapp.mouse_x = mouse_pos.x * _sapp.dpi_scale;
    _sapp.mouse_y = _sapp.framebuffer_height - (mouse_pos.y * _sapp.dpi_scale) - 1;
    _sapp_frame();
    if (_sapp.quit_requested || _sapp.quit_ordered) {
        [_sapp_macos_window_obj performClose:nil];
    }
}

@implementation _sapp_macos_app_delegate
- (void)applicationDidFinishLaunching:(NSNotification*)aNotification {
    if (_sapp.desc.fullscreen) {
        NSRect screen_rect = NSScreen.mainScreen.frame;
        _sapp.window_width = screen_rect.size.width;
        _sapp.window_height = screen_rect.size.height;
        if (_sapp.desc.high_dpi) {
            _sapp.framebuffer_width = 2 * _sapp.window_width;
            _sapp.framebuffer_height = 2 * _sapp.window_height;
        }
        else {
            _sapp.framebuffer_width = _sapp.window_width;
            _sapp.framebuffer_height = _sapp.window_height;
        }
        _sapp.dpi_scale = (float)_sapp.framebuffer_width / (float) _sapp.window_width;
    }
    NSUInteger style =    NSWindowStyleMaskBorderless;
    if (!_sapp.desc.fullscreen) {
	style =
        NSWindowStyleMaskTitled |
        NSWindowStyleMaskClosable |
        NSWindowStyleMaskMiniaturizable |
        NSWindowStyleMaskResizable;
	  }

    NSRect window_rect = NSMakeRect(0, 0, _sapp.window_width, _sapp.window_height);
    _sapp_macos_window_obj = [[MyWindow alloc]
        initWithContentRect:window_rect
        styleMask:style
        backing:NSBackingStoreBuffered
        defer:NO];
    _sapp_macos_window_obj.title = [NSString stringWithUTF8String:_sapp.window_title];
    _sapp_macos_window_obj.acceptsMouseMovedEvents = YES;
    _sapp_macos_window_obj.restorable = YES;
    _sapp_macos_win_dlg_obj = [[_sapp_macos_window_delegate alloc] init];
    _sapp_macos_window_obj.delegate = _sapp_macos_win_dlg_obj;
    #if defined(SOKOL_METAL)
        _sapp_mtl_device_obj = MTLCreateSystemDefaultDevice();
        _sapp_macos_mtk_view_dlg_obj = [[_sapp_macos_mtk_view_dlg alloc] init];
        _sapp_view_obj = [[_sapp_macos_view alloc] init];
        [_sapp_view_obj updateTrackingAreas];
        _sapp_view_obj.preferredFramesPerSecond = 60 / _sapp.swap_interval;
        _sapp_view_obj.delegate = _sapp_macos_mtk_view_dlg_obj;
        _sapp_view_obj.device = _sapp_mtl_device_obj;
        _sapp_view_obj.colorPixelFormat = MTLPixelFormatBGRA8Unorm;
        _sapp_view_obj.depthStencilPixelFormat = MTLPixelFormatDepth32Float_Stencil8;
        _sapp_view_obj.sampleCount = _sapp.sample_count;
        _sapp_macos_window_obj.contentView = _sapp_view_obj;
        [_sapp_macos_window_obj makeFirstResponder:_sapp_view_obj];
        if (!_sapp.desc.high_dpi) {
            CGSize drawable_size = { (CGFloat) _sapp.framebuffer_width, (CGFloat) _sapp.framebuffer_height };
            _sapp_view_obj.drawableSize = drawable_size;
        }
        _sapp_macos_update_dimensions();
        _sapp_view_obj.layer.magnificationFilter = kCAFilterNearest;
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
            attrs[i++] = NSOpenGLPFASamples; attrs[i++] = _sapp.sample_count;
        }
        else {
            attrs[i++] = NSOpenGLPFASampleBuffers; attrs[i++] = 0;
        }
        attrs[i++] = 0;
        _sapp_macos_glpixelformat_obj = [[NSOpenGLPixelFormat alloc] initWithAttributes:attrs];
        SOKOL_ASSERT(_sapp_macos_glpixelformat_obj != nil);

        _sapp_view_obj = [[_sapp_macos_view alloc]
            initWithFrame:window_rect
            pixelFormat:_sapp_macos_glpixelformat_obj];
        [_sapp_view_obj updateTrackingAreas];
        if (_sapp.desc.high_dpi) {
            [_sapp_view_obj setWantsBestResolutionOpenGLSurface:YES];
        }
        else {
            [_sapp_view_obj setWantsBestResolutionOpenGLSurface:NO];
        }

        _sapp_macos_window_obj.contentView = _sapp_view_obj;
        [_sapp_macos_window_obj makeFirstResponder:_sapp_view_obj];

        _sapp_macos_timer_obj = [NSTimer timerWithTimeInterval:0.001
            target:_sapp_view_obj
            selector:@selector(timerFired:)
            userInfo:nil
            repeats:YES];
        [[NSRunLoop currentRunLoop] addTimer:_sapp_macos_timer_obj forMode:NSDefaultRunLoopMode];
    #endif
    _sapp.valid = true;
    if (_sapp.desc.fullscreen && false) {
        /* on GL, this already toggles a rendered frame, so set the valid flag before */
        [_sapp_macos_window_obj toggleFullScreen:self];
    }
    else {
        [_sapp_macos_window_obj center];
    }
    [_sapp_macos_window_obj makeKeyAndOrderFront:nil];
}

- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication*)sender {
    return YES;
}
@end

_SOKOL_PRIVATE uint32_t _sapp_macos_mod(NSEventModifierFlags f) {
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
    return m;
}

_SOKOL_PRIVATE void _sapp_macos_mouse_event(sapp_event_type type, sapp_mousebutton btn, uint32_t mod) {
    if (_sapp_events_enabled()) {
        _sapp_init_event(type);
        _sapp.event.mouse_button = btn;
        _sapp.event.modifiers = mod;
        _sapp.event.mouse_x = _sapp.mouse_x;
        _sapp.event.mouse_y = _sapp.mouse_y;
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

@implementation _sapp_macos_window_delegate
- (BOOL)windowShouldClose:(id)sender {
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
        _sapp_call_cleanup();
        return YES;
    }
    else {
        return NO;
    }
}

- (void)windowDidResize:(NSNotification*)notification {
    _sapp_macos_update_dimensions();
    _sapp_macos_app_event(SAPP_EVENTTYPE_RESIZED);
}

- (void)windowDidMiniaturize:(NSNotification*)notification {
    _sapp_macos_app_event(SAPP_EVENTTYPE_ICONIFIED);
}

- (void)windowDidDeminiaturize:(NSNotification*)notification {
    _sapp_macos_app_event(SAPP_EVENTTYPE_RESTORED);
}
@end

#if defined(SOKOL_METAL)
@implementation _sapp_macos_mtk_view_dlg
- (void)drawInMTKView:(MTKView*)view {
    @autoreleasepool {
        _sapp_macos_frame();
    }
}
- (void)mtkView:(MTKView*)view drawableSizeWillChange:(CGSize)size {
    /* this is required by the protocol, but we can't do anything useful here */
}
@end
#endif

@implementation _sapp_macos_view
#if defined(SOKOL_GLCORE33)
- (void)timerFired:(id)sender {
    [self setNeedsDisplay:YES];
}
- (void)prepareOpenGL {
    [super prepareOpenGL];
    GLint swapInt = 1;
    NSOpenGLContext* ctx = [_sapp_view_obj openGLContext];
    [ctx setValues:&swapInt forParameter:NSOpenGLContextParameterSwapInterval];
    [ctx makeCurrentContext];
}
- (void)drawRect:(NSRect)bound {
    _sapp_macos_frame();
    [[_sapp_view_obj openGLContext] flushBuffer];
}
#endif

- (BOOL)isOpaque {
    return YES;
}
- (BOOL)canBecomeKey {
    return YES;
}
- (BOOL)acceptsFirstResponder {
    return YES;
}

- (void)updateTrackingAreas {
    if (trackingArea != nil) {
        [self removeTrackingArea:trackingArea];
        trackingArea = nil;
    }
    const NSTrackingAreaOptions options = NSTrackingMouseEnteredAndExited |
                                          NSTrackingActiveInKeyWindow |
                                          NSTrackingEnabledDuringMouseDrag |
                                          NSTrackingCursorUpdate |
                                          NSTrackingInVisibleRect |
                                          NSTrackingAssumeInside;
    trackingArea = [[NSTrackingArea alloc] initWithRect:[self bounds] options:options owner:self userInfo:nil];
    [self addTrackingArea:trackingArea];
    [super updateTrackingAreas];
}
- (void)mouseEntered:(NSEvent*)event {
    _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_ENTER, SAPP_MOUSEBUTTON_INVALID, _sapp_macos_mod(event.modifierFlags));
}
- (void)mouseExited:(NSEvent*)event {
    _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_LEAVE, SAPP_MOUSEBUTTON_INVALID, _sapp_macos_mod(event.modifierFlags));
}
- (void)mouseDown:(NSEvent*)event {
    _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_DOWN, SAPP_MOUSEBUTTON_LEFT, _sapp_macos_mod(event.modifierFlags));
}
- (void)mouseUp:(NSEvent*)event {
    _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_UP, SAPP_MOUSEBUTTON_LEFT, _sapp_macos_mod(event.modifierFlags));
}
- (void)rightMouseDown:(NSEvent*)event {
    _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_DOWN, SAPP_MOUSEBUTTON_RIGHT, _sapp_macos_mod(event.modifierFlags));
}
- (void)rightMouseUp:(NSEvent*)event {
    _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_UP, SAPP_MOUSEBUTTON_RIGHT, _sapp_macos_mod(event.modifierFlags));
}
- (void)mouseMoved:(NSEvent*)event {
    _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_MOVE, SAPP_MOUSEBUTTON_INVALID , _sapp_macos_mod(event.modifierFlags));
}
- (void)mouseDragged:(NSEvent*)event {
    _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_MOVE, SAPP_MOUSEBUTTON_INVALID , _sapp_macos_mod(event.modifierFlags));
}
- (void)rightMouseDragged:(NSEvent*)event {
    _sapp_macos_mouse_event(SAPP_EVENTTYPE_MOUSE_MOVE, SAPP_MOUSEBUTTON_INVALID, _sapp_macos_mod(event.modifierFlags));
}
- (void)scrollWheel:(NSEvent*)event {
    if (_sapp_events_enabled()) {
        float dx = (float) event.scrollingDeltaX;
        float dy = (float) event.scrollingDeltaY;
        if (event.hasPreciseScrollingDeltas) {
            dx *= 0.1;
            dy *= 0.1;
        }
        if ((_sapp_absf(dx) > 0.0f) || (_sapp_absf(dy) > 0.0f)) {
            _sapp_init_event(SAPP_EVENTTYPE_MOUSE_SCROLL);
            _sapp.event.modifiers = _sapp_macos_mod(event.modifierFlags);
            _sapp.event.mouse_x = _sapp.mouse_x;
            _sapp.event.mouse_y = _sapp.mouse_y;
            _sapp.event.scroll_x = dx;
            _sapp.event.scroll_y = dy;
            _sapp_call_event(&_sapp.event);
        }
    }
}
- (void)keyDown:(NSEvent*)event {
    if (_sapp_events_enabled()) {
        const uint32_t mods = _sapp_macos_mod(event.modifierFlags);
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
        if (_sapp.clipboard_enabled && (mods == SAPP_MODIFIER_SUPER) && (key_code == SAPP_KEYCODE_V)) {
            _sapp_init_event(SAPP_EVENTTYPE_CLIPBOARD_PASTED);
            _sapp_call_event(&_sapp.event);
        }
    }
}
- (void)keyUp:(NSEvent*)event {
    _sapp_macos_key_event(SAPP_EVENTTYPE_KEY_UP,
        _sapp_translate_key(event.keyCode),
        event.isARepeat,
        _sapp_macos_mod(event.modifierFlags));
}
- (void)flagsChanged:(NSEvent*)event {
    const uint32_t old_f = _sapp_macos_flags_changed_store;
    const uint32_t new_f = event.modifierFlags;
    _sapp_macos_flags_changed_store = new_f;
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
            _sapp_macos_mod(event.modifierFlags));
    }
}
- (void)cursorUpdate:(NSEvent*)event {
    if (_sapp.desc.user_cursor) {
        _sapp_macos_app_event(SAPP_EVENTTYPE_UPDATE_CURSOR);
    }
}
@end

void _sapp_macos_set_clipboard_string(const char* str) {
    @autoreleasepool {
        NSPasteboard* pasteboard = [NSPasteboard generalPasteboard];
        [pasteboard declareTypes:@[NSPasteboardTypeString] owner:nil];
        [pasteboard setString:@(str) forType:NSPasteboardTypeString];
    }
}

const char* _sapp_macos_get_clipboard_string(void) {
    SOKOL_ASSERT(_sapp.clipboard);
    @autoreleasepool {
        _sapp.clipboard[0] = 0;
        NSPasteboard* pasteboard = [NSPasteboard generalPasteboard];
        if (![[pasteboard types] containsObject:NSPasteboardTypeString]) {
            return _sapp.clipboard;
        }
        NSString* str = [pasteboard stringForType:NSPasteboardTypeString];
        if (!str) {
            return _sapp.clipboard;
        }
        _sapp_strcpy([str UTF8String], _sapp.clipboard, _sapp.clipboard_size);
    }
    return _sapp.clipboard;
}

#endif /* MacOS */

/*== iOS =====================================================================*/
#if defined(TARGET_OS_IPHONE) && TARGET_OS_IPHONE
#import <UIKit/UIKit.h>
#if defined(SOKOL_METAL)
#import <Metal/Metal.h>
#import <MetalKit/MetalKit.h>
#else
#import <GLKit/GLKit.h>
#include <OpenGLES/ES3/gl.h>
#include <OpenGLES/ES3/glext.h>
#endif

@interface _sapp_app_delegate : NSObject<UIApplicationDelegate>
@end
@interface _sapp_textfield_dlg : NSObject<UITextFieldDelegate>
- (void)keyboardWasShown:(NSNotification*)notif;
- (void)keyboardWillBeHidden:(NSNotification*)notif;
- (void)keyboardDidChangeFrame:(NSNotification*)notif;
@end
#if defined(SOKOL_METAL)
@interface _sapp_ios_mtk_view_dlg : NSObject<MTKViewDelegate>
@end
@interface _sapp_ios_view : MTKView;
@end
#else
@interface _sapp_ios_glk_view_dlg : NSObject<GLKViewDelegate>
@end
@interface _sapp_ios_view : GLKView
@end
#endif

static bool _sapp_ios_suspended;
static UIWindow* _sapp_ios_window_obj;
static _sapp_ios_view* _sapp_view_obj;
static UITextField* _sapp_ios_textfield_obj;
static _sapp_textfield_dlg* _sapp_ios_textfield_dlg_obj;
#if defined(SOKOL_METAL)
static _sapp_ios_mtk_view_dlg* _sapp_ios_mtk_view_dlg_obj;
static UIViewController<MTKViewDelegate>* _sapp_ios_view_ctrl_obj;
static id<MTLDevice> _sapp_mtl_device_obj;
#else
static EAGLContext* _sapp_ios_eagl_ctx_obj;
static _sapp_ios_glk_view_dlg* _sapp_ios_glk_view_dlg_obj;
static GLKViewController* _sapp_ios_view_ctrl_obj;
#endif

_SOKOL_PRIVATE void _sapp_run(const sapp_desc* desc) {
    _sapp_init_state(desc);
    static int argc = 1;
    static char* argv[] = { (char*)"sokol_app" };
    UIApplicationMain(argc, argv, nil, NSStringFromClass([_sapp_app_delegate class]));
    _sapp_discard_state();
}

/* iOS entry function */
#if !defined(SOKOL_NO_ENTRY)
int main(int argc, char* argv[]) {
    sapp_desc desc = sokol_main(argc, argv);
    _sapp_run(&desc);
    return 0;
}
#endif /* SOKOL_NO_ENTRY */

_SOKOL_PRIVATE void _sapp_ios_app_event(sapp_event_type type) {
    if (_sapp_events_enabled()) {
        _sapp_init_event(type);
        _sapp_call_event(&_sapp.event);
    }
}

_SOKOL_PRIVATE void _sapp_ios_update_dimensions(void) {
    CGRect screen_rect = UIScreen.mainScreen.bounds;
    _sapp.window_width = (int) screen_rect.size.width;
    _sapp.window_height = (int) screen_rect.size.height;
    int cur_fb_width, cur_fb_height;
    #if defined(SOKOL_METAL)
        const CGSize fb_size = _sapp_view_obj.drawableSize;
        cur_fb_width = (int) fb_size.width;
        cur_fb_height = (int) fb_size.height;
    #else
        cur_fb_width = (int) _sapp_view_obj.drawableWidth;
        cur_fb_height = (int) _sapp_view_obj.drawableHeight;
    #endif
    const bool dim_changed =
        (_sapp.framebuffer_width != cur_fb_width) ||
        (_sapp.framebuffer_height != cur_fb_height);
    _sapp.framebuffer_width = cur_fb_width;
    _sapp.framebuffer_height = cur_fb_height;
    SOKOL_ASSERT((_sapp.framebuffer_width > 0) && (_sapp.framebuffer_height > 0));
    _sapp.dpi_scale = (float)_sapp.framebuffer_width / (float) _sapp.window_width;
    if (dim_changed) {
        _sapp_ios_app_event(SAPP_EVENTTYPE_RESIZED);
    }
}

_SOKOL_PRIVATE void _sapp_ios_frame(void) {
    _sapp_ios_update_dimensions();
    _sapp_frame();
}

_SOKOL_PRIVATE void _sapp_ios_show_keyboard(bool shown) {
    /* if not happened yet, create an invisible text field */
    if (nil == _sapp_ios_textfield_obj) {
        _sapp_ios_textfield_dlg_obj = [[_sapp_textfield_dlg alloc] init];
        _sapp_ios_textfield_obj = [[UITextField alloc] initWithFrame:CGRectMake(10, 10, 100, 50)];
        _sapp_ios_textfield_obj.keyboardType = UIKeyboardTypeDefault;
        _sapp_ios_textfield_obj.returnKeyType = UIReturnKeyDefault;
        _sapp_ios_textfield_obj.autocapitalizationType = UITextAutocapitalizationTypeNone;
        _sapp_ios_textfield_obj.autocorrectionType = UITextAutocorrectionTypeNo;
        _sapp_ios_textfield_obj.spellCheckingType = UITextSpellCheckingTypeNo;
        _sapp_ios_textfield_obj.hidden = YES;
        _sapp_ios_textfield_obj.text = @"x";
        _sapp_ios_textfield_obj.delegate = _sapp_ios_textfield_dlg_obj;
        [_sapp_ios_view_ctrl_obj.view addSubview:_sapp_ios_textfield_obj];

        [[NSNotificationCenter defaultCenter] addObserver:_sapp_ios_textfield_dlg_obj
            selector:@selector(keyboardWasShown:)
            name:UIKeyboardDidShowNotification object:nil];
        [[NSNotificationCenter defaultCenter] addObserver:_sapp_ios_textfield_dlg_obj
            selector:@selector(keyboardWillBeHidden:)
            name:UIKeyboardWillHideNotification object:nil];
        [[NSNotificationCenter defaultCenter] addObserver:_sapp_ios_textfield_dlg_obj
            selector:@selector(keyboardDidChangeFrame:)
            name:UIKeyboardDidChangeFrameNotification object:nil];
    }
    if (shown) {
        /* setting the text field as first responder brings up the onscreen keyboard */
        [_sapp_ios_textfield_obj becomeFirstResponder];
    }
    else {
        [_sapp_ios_textfield_obj resignFirstResponder];
    }
}

@implementation _sapp_app_delegate
- (BOOL)application:(UIApplication*)application didFinishLaunchingWithOptions:(NSDictionary*)launchOptions {
    CGRect screen_rect = UIScreen.mainScreen.bounds;
    _sapp_ios_window_obj = [[UIWindow alloc] initWithFrame:screen_rect];
    _sapp.window_width = screen_rect.size.width;
    _sapp.window_height = screen_rect.size.height;
    if (_sapp.desc.high_dpi) {
        _sapp.framebuffer_width = 2 * _sapp.window_width;
        _sapp.framebuffer_height = 2 * _sapp.window_height;
    }
    else {
        _sapp.framebuffer_width = _sapp.window_width;
        _sapp.framebuffer_height = _sapp.window_height;
    }
    _sapp.dpi_scale = (float)_sapp.framebuffer_width / (float) _sapp.window_width;
    #if defined(SOKOL_METAL)
        _sapp_mtl_device_obj = MTLCreateSystemDefaultDevice();
        _sapp_ios_mtk_view_dlg_obj = [[_sapp_ios_mtk_view_dlg alloc] init];
        _sapp_view_obj = [[_sapp_ios_view alloc] init];
        _sapp_view_obj.preferredFramesPerSecond = 60 / _sapp.swap_interval;
        _sapp_view_obj.delegate = _sapp_ios_mtk_view_dlg_obj;
        _sapp_view_obj.device = _sapp_mtl_device_obj;
        _sapp_view_obj.colorPixelFormat = MTLPixelFormatBGRA8Unorm;
        _sapp_view_obj.depthStencilPixelFormat = MTLPixelFormatDepth32Float_Stencil8;
        _sapp_view_obj.sampleCount = _sapp.sample_count;
        if (_sapp.desc.high_dpi) {
            _sapp_view_obj.contentScaleFactor = 2.0;
        }
        else {
            _sapp_view_obj.contentScaleFactor = 1.0;
        }
        _sapp_view_obj.userInteractionEnabled = YES;
        _sapp_view_obj.multipleTouchEnabled = YES;
        [_sapp_ios_window_obj addSubview:_sapp_view_obj];
        _sapp_ios_view_ctrl_obj = [[UIViewController<MTKViewDelegate> alloc] init];
        _sapp_ios_view_ctrl_obj.view = _sapp_view_obj;
        _sapp_ios_window_obj.rootViewController = _sapp_ios_view_ctrl_obj;
    #else
        if (_sapp.desc.gl_force_gles2) {
            _sapp_ios_eagl_ctx_obj = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES2];
            _sapp.gles2_fallback = true;
        }
        else {
            _sapp_ios_eagl_ctx_obj = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES3];
            if (_sapp_ios_eagl_ctx_obj == nil) {
                _sapp_ios_eagl_ctx_obj = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES2];
                _sapp.gles2_fallback = true;
            }
        }
        _sapp_ios_glk_view_dlg_obj = [[_sapp_ios_glk_view_dlg alloc] init];
        _sapp_view_obj = [[_sapp_ios_view alloc] initWithFrame:screen_rect];
        _sapp_view_obj.drawableColorFormat = GLKViewDrawableColorFormatRGBA8888;
        _sapp_view_obj.drawableDepthFormat = GLKViewDrawableDepthFormat24;
        _sapp_view_obj.drawableStencilFormat = GLKViewDrawableStencilFormatNone;
        _sapp_view_obj.drawableMultisample = GLKViewDrawableMultisampleNone; /* FIXME */
        _sapp_view_obj.context = _sapp_ios_eagl_ctx_obj;
        _sapp_view_obj.delegate = _sapp_ios_glk_view_dlg_obj;
        _sapp_view_obj.enableSetNeedsDisplay = NO;
        _sapp_view_obj.userInteractionEnabled = YES;
        _sapp_view_obj.multipleTouchEnabled = YES;
        if (_sapp.desc.high_dpi) {
            _sapp_view_obj.contentScaleFactor = 2.0;
        }
        else {
            _sapp_view_obj.contentScaleFactor = 1.0;
        }
        [_sapp_ios_window_obj addSubview:_sapp_view_obj];
        _sapp_ios_view_ctrl_obj = [[GLKViewController alloc] init];
        _sapp_ios_view_ctrl_obj.view = _sapp_view_obj;
        _sapp_ios_view_ctrl_obj.preferredFramesPerSecond = 60 / _sapp.swap_interval;
        _sapp_ios_window_obj.rootViewController = _sapp_ios_view_ctrl_obj;
    #endif
    [_sapp_ios_window_obj makeKeyAndVisible];

    _sapp.valid = true;
    return YES;
}

- (void)applicationWillResignActive:(UIApplication *)application {
    if (!_sapp_ios_suspended) {
        _sapp_ios_suspended = true;
        _sapp_ios_app_event(SAPP_EVENTTYPE_SUSPENDED);
    }
}

- (void)applicationDidBecomeActive:(UIApplication *)application {
    if (_sapp_ios_suspended) {
        _sapp_ios_suspended = false;
        _sapp_ios_app_event(SAPP_EVENTTYPE_RESUMED);
    }
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
        _sapp_view_obj.frame = view_frame;
    }
}
- (void)keyboardWillBeHidden:(NSNotification*)notif {
    _sapp.onscreen_keyboard_shown = false;
    if (_sapp.desc.ios_keyboard_resizes_canvas) {
        _sapp_view_obj.frame = UIScreen.mainScreen.bounds;
    }
}
- (void)keyboardDidChangeFrame:(NSNotification*)notif {
    /* this is for the case when the screen rotation changes while the keyboard is open */
    if (_sapp.onscreen_keyboard_shown && _sapp.desc.ios_keyboard_resizes_canvas) {
        NSDictionary* info = notif.userInfo;
        CGFloat kbd_h = [[info objectForKey:UIKeyboardFrameEndUserInfoKey] CGRectValue].size.height;
        CGRect view_frame = UIScreen.mainScreen.bounds;
        view_frame.size.height -= kbd_h;
        _sapp_view_obj.frame = view_frame;
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

#if defined(SOKOL_METAL)
@implementation _sapp_ios_mtk_view_dlg
- (void)drawInMTKView:(MTKView*)view {
    @autoreleasepool {
        _sapp_ios_frame();
    }
}

- (void)mtkView:(MTKView*)view drawableSizeWillChange:(CGSize)size {
    /* this is required by the protocol, but we can't do anything useful here */
}
@end
#else
@implementation _sapp_ios_glk_view_dlg
- (void)glkView:(GLKView*)view drawInRect:(CGRect)rect {
    @autoreleasepool {
        _sapp_ios_frame();
    }
}
@end
#endif

_SOKOL_PRIVATE void _sapp_ios_touch_event(sapp_event_type type, NSSet<UITouch *>* touches, UIEvent* event) {
    if (_sapp_events_enabled()) {
        _sapp_init_event(type);
        NSEnumerator* enumerator = event.allTouches.objectEnumerator;
        UITouch* ios_touch;
        while ((ios_touch = [enumerator nextObject])) {
            if ((_sapp.event.num_touches + 1) < SAPP_MAX_TOUCHPOINTS) {
                CGPoint ios_pos = [ios_touch locationInView:_sapp_view_obj];
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

@implementation _sapp_ios_view
- (BOOL) isOpaque {
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

#endif /* __APPLE__ */

/*== EMSCRIPTEN ==============================================================*/
#if defined(__EMSCRIPTEN__)
#if defined(SOKOL_GLES3)
#include <GLES3/gl3.h>
#else
#ifndef GL_EXT_PROTOTYPES
#define GL_GLEXT_PROTOTYPES
#endif
#include <GLES2/gl2.h>
#include <GLES2/gl2ext.h>
#endif
#include <emscripten/emscripten.h>
#include <emscripten/html5.h>

static bool _sapp_emsc_input_created;
static bool _sapp_emsc_wants_show_keyboard;
static bool _sapp_emsc_wants_hide_keyboard;

/* this function is called from a JS event handler when the user hides
    the onscreen keyboard pressing the 'dismiss keyboard key'
*/
#ifdef __cplusplus
extern "C" {
#endif
EMSCRIPTEN_KEEPALIVE void _sapp_emsc_notify_keyboard_hidden(void) {
    _sapp.onscreen_keyboard_shown = false;
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

EMSCRIPTEN_KEEPALIVE void _sapp_emsc_onpaste(const char* str) {
    if (_sapp.clipboard_enabled) {
        _sapp_strcpy(str, _sapp.clipboard, _sapp.clipboard_size);
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

EM_JS(void, sapp_js_hook_beforeunload, (void), {
    window.addEventListener('beforeunload', function(_sapp_event) {
        if (__sapp_html5_get_ask_leave_site() != 0) {
            _sapp_event.preventDefault();
            _sapp_event.returnValue = ' ';
        }
    });
});

EM_JS(void, sapp_js_init_clipboard, (void), {
    window.addEventListener('paste', function(event) {
        var pasted_str = event.clipboardData.getData('text');
        ccall('_sapp_emsc_onpaste', 'void', ['string'], [pasted_str]);
    });
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

/* called from the emscripten event handler to update the keyboard visibility
    state, this must happen from an JS input event handler, otherwise
    the request will be ignored by the browser
*/
_SOKOL_PRIVATE void _sapp_emsc_update_keyboard_state(void) {
    if (_sapp_emsc_wants_show_keyboard) {
        /* create input text field on demand */
        if (!_sapp_emsc_input_created) {
            _sapp_emsc_input_created = true;
            sapp_js_create_textfield();
        }
        /* focus the text input field, this will bring up the keyboard */
        _sapp.onscreen_keyboard_shown = true;
        _sapp_emsc_wants_show_keyboard = false;
        sapp_js_focus_textfield();
    }
    if (_sapp_emsc_wants_hide_keyboard) {
        /* unfocus the text input field */
        if (_sapp_emsc_input_created) {
            _sapp.onscreen_keyboard_shown = false;
            _sapp_emsc_wants_hide_keyboard = false;
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
        _sapp_emsc_wants_show_keyboard = true;
    }
    else {
        _sapp_emsc_wants_hide_keyboard = true;
    }
}

_SOKOL_PRIVATE EM_BOOL _sapp_emsc_size_changed(int event_type, const EmscriptenUiEvent* ui_event, void* user_data) {
    double w, h;
    emscripten_get_element_css_size(_sapp.html5_canvas_name, &w, &h);
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
    emscripten_set_canvas_element_size(_sapp.html5_canvas_name, _sapp.framebuffer_width, _sapp.framebuffer_height);
    if (_sapp_events_enabled()) {
        _sapp_init_event(SAPP_EVENTTYPE_RESIZED);
        _sapp_call_event(&_sapp.event);
    }
    return true;
}

_SOKOL_PRIVATE EM_BOOL _sapp_emsc_frame(double time, void* userData) {
    _SOKOL_UNUSED(time);
    _SOKOL_UNUSED(userData);
    _sapp_frame();
    return EM_TRUE;
}

_SOKOL_PRIVATE EM_BOOL _sapp_emsc_context_cb(int emsc_type, const void* reserved, void* user_data) {
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

_SOKOL_PRIVATE EM_BOOL _sapp_emsc_mouse_cb(int emsc_type, const EmscriptenMouseEvent* emsc_event, void* user_data) {
    _sapp.mouse_x = (emsc_event->targetX * _sapp.dpi_scale);
    _sapp.mouse_y = (emsc_event->targetY * _sapp.dpi_scale);
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
            if (emsc_event->ctrlKey) {
                _sapp.event.modifiers |= SAPP_MODIFIER_CTRL;
            }
            if (emsc_event->shiftKey) {
                _sapp.event.modifiers |= SAPP_MODIFIER_SHIFT;
            }
            if (emsc_event->altKey) {
                _sapp.event.modifiers |= SAPP_MODIFIER_ALT;
            }
            if (emsc_event->metaKey) {
                _sapp.event.modifiers |= SAPP_MODIFIER_SUPER;
            }
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
            _sapp.event.mouse_x = _sapp.mouse_x;
            _sapp.event.mouse_y = _sapp.mouse_y;
            _sapp_call_event(&_sapp.event);
        }
    }
    _sapp_emsc_update_keyboard_state();
    return true;
}

_SOKOL_PRIVATE EM_BOOL _sapp_emsc_wheel_cb(int emsc_type, const EmscriptenWheelEvent* emsc_event, void* user_data) {
    if (_sapp_events_enabled()) {
        _sapp_init_event(SAPP_EVENTTYPE_MOUSE_SCROLL);
        if (emsc_event->mouse.ctrlKey) {
            _sapp.event.modifiers |= SAPP_MODIFIER_CTRL;
        }
        if (emsc_event->mouse.shiftKey) {
            _sapp.event.modifiers |= SAPP_MODIFIER_SHIFT;
        }
        if (emsc_event->mouse.altKey) {
            _sapp.event.modifiers |= SAPP_MODIFIER_ALT;
        }
        if (emsc_event->mouse.metaKey) {
            _sapp.event.modifiers |= SAPP_MODIFIER_SUPER;
        }
        _sapp.event.scroll_x = -0.1 * (float)emsc_event->deltaX;
        _sapp.event.scroll_y = -0.1 * (float)emsc_event->deltaY;
        _sapp_call_event(&_sapp.event);
    }
    _sapp_emsc_update_keyboard_state();
    return true;
}

_SOKOL_PRIVATE EM_BOOL _sapp_emsc_key_cb(int emsc_type, const EmscriptenKeyboardEvent* emsc_event, void* user_data) {
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
            if (emsc_event->ctrlKey) {
                _sapp.event.modifiers |= SAPP_MODIFIER_CTRL;
            }
            if (emsc_event->shiftKey) {
                _sapp.event.modifiers |= SAPP_MODIFIER_SHIFT;
            }
            if (emsc_event->altKey) {
                _sapp.event.modifiers |= SAPP_MODIFIER_ALT;
            }
            if (emsc_event->metaKey) {
                _sapp.event.modifiers |= SAPP_MODIFIER_SUPER;
            }
            if (type == SAPP_EVENTTYPE_CHAR) {
                _sapp.event.char_code = emsc_event->charCode;
                /* workaround to make Cmd+V work on Safari */
                if ((emsc_event->metaKey) && (emsc_event->charCode == 118)) {
                    retval = false;
                }
            }
            else {
                _sapp.event.key_code = _sapp_translate_key(emsc_event->keyCode);
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
    return retval;
}

_SOKOL_PRIVATE EM_BOOL _sapp_emsc_touch_cb(int emsc_type, const EmscriptenTouchEvent* emsc_event, void* user_data) {
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
            if (emsc_event->ctrlKey) {
                _sapp.event.modifiers |= SAPP_MODIFIER_CTRL;
            }
            if (emsc_event->shiftKey) {
                _sapp.event.modifiers |= SAPP_MODIFIER_SHIFT;
            }
            if (emsc_event->altKey) {
                _sapp.event.modifiers |= SAPP_MODIFIER_ALT;
            }
            if (emsc_event->metaKey) {
                _sapp.event.modifiers |= SAPP_MODIFIER_SUPER;
            }
            _sapp.event.num_touches = emsc_event->numTouches;
            if (_sapp.event.num_touches > SAPP_MAX_TOUCHPOINTS) {
                _sapp.event.num_touches = SAPP_MAX_TOUCHPOINTS;
            }
            for (int i = 0; i < _sapp.event.num_touches; i++) {
                const EmscriptenTouchPoint* src = &emsc_event->touches[i];
                sapp_touchpoint* dst = &_sapp.event.touches[i];
                dst->identifier = src->identifier;
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

_SOKOL_PRIVATE void _sapp_emsc_init_keytable(void) {
    _sapp.keycodes[8]   = SAPP_KEYCODE_BACKSPACE;
    _sapp.keycodes[9]   = SAPP_KEYCODE_TAB;
    _sapp.keycodes[13]  = SAPP_KEYCODE_ENTER;
    _sapp.keycodes[16]  = SAPP_KEYCODE_LEFT_SHIFT;
    _sapp.keycodes[17]  = SAPP_KEYCODE_LEFT_CONTROL;
    _sapp.keycodes[18]  = SAPP_KEYCODE_LEFT_ALT;
    _sapp.keycodes[19]  = SAPP_KEYCODE_PAUSE;
    _sapp.keycodes[27]  = SAPP_KEYCODE_ESCAPE;
    _sapp.keycodes[32]  = SAPP_KEYCODE_SPACE;
    _sapp.keycodes[33]  = SAPP_KEYCODE_PAGE_UP;
    _sapp.keycodes[34]  = SAPP_KEYCODE_PAGE_DOWN;
    _sapp.keycodes[35]  = SAPP_KEYCODE_END;
    _sapp.keycodes[36]  = SAPP_KEYCODE_HOME;
    _sapp.keycodes[37]  = SAPP_KEYCODE_LEFT;
    _sapp.keycodes[38]  = SAPP_KEYCODE_UP;
    _sapp.keycodes[39]  = SAPP_KEYCODE_RIGHT;
    _sapp.keycodes[40]  = SAPP_KEYCODE_DOWN;
    _sapp.keycodes[45]  = SAPP_KEYCODE_INSERT;
    _sapp.keycodes[46]  = SAPP_KEYCODE_DELETE;
    _sapp.keycodes[48]  = SAPP_KEYCODE_0;
    _sapp.keycodes[49]  = SAPP_KEYCODE_1;
    _sapp.keycodes[50]  = SAPP_KEYCODE_2;
    _sapp.keycodes[51]  = SAPP_KEYCODE_3;
    _sapp.keycodes[52]  = SAPP_KEYCODE_4;
    _sapp.keycodes[53]  = SAPP_KEYCODE_5;
    _sapp.keycodes[54]  = SAPP_KEYCODE_6;
    _sapp.keycodes[55]  = SAPP_KEYCODE_7;
    _sapp.keycodes[56]  = SAPP_KEYCODE_8;
    _sapp.keycodes[57]  = SAPP_KEYCODE_9;
    _sapp.keycodes[59]  = SAPP_KEYCODE_SEMICOLON;
    _sapp.keycodes[64]  = SAPP_KEYCODE_EQUAL;
    _sapp.keycodes[65]  = SAPP_KEYCODE_A;
    _sapp.keycodes[66]  = SAPP_KEYCODE_B;
    _sapp.keycodes[67]  = SAPP_KEYCODE_C;
    _sapp.keycodes[68]  = SAPP_KEYCODE_D;
    _sapp.keycodes[69]  = SAPP_KEYCODE_E;
    _sapp.keycodes[70]  = SAPP_KEYCODE_F;
    _sapp.keycodes[71]  = SAPP_KEYCODE_G;
    _sapp.keycodes[72]  = SAPP_KEYCODE_H;
    _sapp.keycodes[73]  = SAPP_KEYCODE_I;
    _sapp.keycodes[74]  = SAPP_KEYCODE_J;
    _sapp.keycodes[75]  = SAPP_KEYCODE_K;
    _sapp.keycodes[76]  = SAPP_KEYCODE_L;
    _sapp.keycodes[77]  = SAPP_KEYCODE_M;
    _sapp.keycodes[78]  = SAPP_KEYCODE_N;
    _sapp.keycodes[79]  = SAPP_KEYCODE_O;
    _sapp.keycodes[80]  = SAPP_KEYCODE_P;
    _sapp.keycodes[81]  = SAPP_KEYCODE_Q;
    _sapp.keycodes[82]  = SAPP_KEYCODE_R;
    _sapp.keycodes[83]  = SAPP_KEYCODE_S;
    _sapp.keycodes[84]  = SAPP_KEYCODE_T;
    _sapp.keycodes[85]  = SAPP_KEYCODE_U;
    _sapp.keycodes[86]  = SAPP_KEYCODE_V;
    _sapp.keycodes[87]  = SAPP_KEYCODE_W;
    _sapp.keycodes[88]  = SAPP_KEYCODE_X;
    _sapp.keycodes[89]  = SAPP_KEYCODE_Y;
    _sapp.keycodes[90]  = SAPP_KEYCODE_Z;
    _sapp.keycodes[91]  = SAPP_KEYCODE_LEFT_SUPER;
    _sapp.keycodes[93]  = SAPP_KEYCODE_MENU;
    _sapp.keycodes[96]  = SAPP_KEYCODE_KP_0;
    _sapp.keycodes[97]  = SAPP_KEYCODE_KP_1;
    _sapp.keycodes[98]  = SAPP_KEYCODE_KP_2;
    _sapp.keycodes[99]  = SAPP_KEYCODE_KP_3;
    _sapp.keycodes[100] = SAPP_KEYCODE_KP_4;
    _sapp.keycodes[101] = SAPP_KEYCODE_KP_5;
    _sapp.keycodes[102] = SAPP_KEYCODE_KP_6;
    _sapp.keycodes[103] = SAPP_KEYCODE_KP_7;
    _sapp.keycodes[104] = SAPP_KEYCODE_KP_8;
    _sapp.keycodes[105] = SAPP_KEYCODE_KP_9;
    _sapp.keycodes[106] = SAPP_KEYCODE_KP_MULTIPLY;
    _sapp.keycodes[107] = SAPP_KEYCODE_KP_ADD;
    _sapp.keycodes[109] = SAPP_KEYCODE_KP_SUBTRACT;
    _sapp.keycodes[110] = SAPP_KEYCODE_KP_DECIMAL;
    _sapp.keycodes[111] = SAPP_KEYCODE_KP_DIVIDE;
    _sapp.keycodes[112] = SAPP_KEYCODE_F1;
    _sapp.keycodes[113] = SAPP_KEYCODE_F2;
    _sapp.keycodes[114] = SAPP_KEYCODE_F3;
    _sapp.keycodes[115] = SAPP_KEYCODE_F4;
    _sapp.keycodes[116] = SAPP_KEYCODE_F5;
    _sapp.keycodes[117] = SAPP_KEYCODE_F6;
    _sapp.keycodes[118] = SAPP_KEYCODE_F7;
    _sapp.keycodes[119] = SAPP_KEYCODE_F8;
    _sapp.keycodes[120] = SAPP_KEYCODE_F9;
    _sapp.keycodes[121] = SAPP_KEYCODE_F10;
    _sapp.keycodes[122] = SAPP_KEYCODE_F11;
    _sapp.keycodes[123] = SAPP_KEYCODE_F12;
    _sapp.keycodes[144] = SAPP_KEYCODE_NUM_LOCK;
    _sapp.keycodes[145] = SAPP_KEYCODE_SCROLL_LOCK;
    _sapp.keycodes[173] = SAPP_KEYCODE_MINUS;
    _sapp.keycodes[186] = SAPP_KEYCODE_SEMICOLON;
    _sapp.keycodes[187] = SAPP_KEYCODE_EQUAL;
    _sapp.keycodes[188] = SAPP_KEYCODE_COMMA;
    _sapp.keycodes[189] = SAPP_KEYCODE_MINUS;
    _sapp.keycodes[190] = SAPP_KEYCODE_PERIOD;
    _sapp.keycodes[191] = SAPP_KEYCODE_SLASH;
    _sapp.keycodes[192] = SAPP_KEYCODE_GRAVE_ACCENT;
    _sapp.keycodes[219] = SAPP_KEYCODE_LEFT_BRACKET;
    _sapp.keycodes[220] = SAPP_KEYCODE_BACKSLASH;
    _sapp.keycodes[221] = SAPP_KEYCODE_RIGHT_BRACKET;
    _sapp.keycodes[222] = SAPP_KEYCODE_APOSTROPHE;
    _sapp.keycodes[224] = SAPP_KEYCODE_LEFT_SUPER;
}

_SOKOL_PRIVATE void _sapp_emsc_init_clipboard(void) {
    sapp_js_init_clipboard();
}

_SOKOL_PRIVATE void _sapp_run(const sapp_desc* desc) {
    _sapp_init_state(desc);
    _sapp_emsc_init_keytable();
    if (_sapp.clipboard_enabled) {
        _sapp_emsc_init_clipboard();
    }
    double w, h;
    if (_sapp.desc.html5_canvas_resize) {
        w = (double) _sapp.desc.width;
        h = (double) _sapp.desc.height;
    }
    else {
        emscripten_get_element_css_size(_sapp.html5_canvas_name, &w, &h);
        emscripten_set_resize_callback(EMSCRIPTEN_EVENT_TARGET_WINDOW, 0, false, _sapp_emsc_size_changed);
    }
    if (_sapp.desc.high_dpi) {
        _sapp.dpi_scale = emscripten_get_device_pixel_ratio();
    }
    _sapp.window_width = (int) w;
    _sapp.window_height = (int) h;
    _sapp.framebuffer_width = (int) (w * _sapp.dpi_scale);
    _sapp.framebuffer_height = (int) (h * _sapp.dpi_scale);
    emscripten_set_canvas_element_size(_sapp.html5_canvas_name, _sapp.framebuffer_width, _sapp.framebuffer_height);

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
    EMSCRIPTEN_WEBGL_CONTEXT_HANDLE ctx = emscripten_webgl_create_context(_sapp.html5_canvas_name, &attrs);
    if (!ctx) {
        attrs.majorVersion = 1;
        ctx = emscripten_webgl_create_context(_sapp.html5_canvas_name, &attrs);
        _sapp.gles2_fallback = true;
    }
    emscripten_webgl_make_context_current(ctx);

    /* some WebGL extension are not enabled automatically by emscripten */
    emscripten_webgl_enable_extension(ctx, "WEBKIT_WEBGL_compressed_texture_pvrtc");

    _sapp.valid = true;
    emscripten_set_mousedown_callback(_sapp.html5_canvas_name, 0, true, _sapp_emsc_mouse_cb);
    emscripten_set_mouseup_callback(_sapp.html5_canvas_name, 0, true, _sapp_emsc_mouse_cb);
    emscripten_set_mousemove_callback(_sapp.html5_canvas_name, 0, true, _sapp_emsc_mouse_cb);
    emscripten_set_mouseenter_callback(_sapp.html5_canvas_name, 0, true, _sapp_emsc_mouse_cb);
    emscripten_set_mouseleave_callback(_sapp.html5_canvas_name, 0, true, _sapp_emsc_mouse_cb);
    emscripten_set_wheel_callback(_sapp.html5_canvas_name, 0, true, _sapp_emsc_wheel_cb);
    emscripten_set_keydown_callback(EMSCRIPTEN_EVENT_TARGET_WINDOW, 0, true, _sapp_emsc_key_cb);
    emscripten_set_keyup_callback(EMSCRIPTEN_EVENT_TARGET_WINDOW, 0, true, _sapp_emsc_key_cb);
    emscripten_set_keypress_callback(EMSCRIPTEN_EVENT_TARGET_WINDOW, 0, true, _sapp_emsc_key_cb);
    emscripten_set_touchstart_callback(_sapp.html5_canvas_name, 0, true, _sapp_emsc_touch_cb);
    emscripten_set_touchmove_callback(_sapp.html5_canvas_name, 0, true, _sapp_emsc_touch_cb);
    emscripten_set_touchend_callback(_sapp.html5_canvas_name, 0, true, _sapp_emsc_touch_cb);
    emscripten_set_touchcancel_callback(_sapp.html5_canvas_name, 0, true, _sapp_emsc_touch_cb);
    emscripten_set_webglcontextlost_callback(_sapp.html5_canvas_name, 0, true, _sapp_emsc_context_cb);
    emscripten_set_webglcontextrestored_callback(_sapp.html5_canvas_name, 0, true, _sapp_emsc_context_cb);
    emscripten_request_animation_frame_loop(_sapp_emsc_frame, 0);

    sapp_js_hook_beforeunload();

    // NOT A BUG: do not call _sapp_discard_state()
}

#if !defined(SOKOL_NO_ENTRY)
int main(int argc, char* argv[]) {
    sapp_desc desc = sokol_main(argc, argv);
    _sapp_run(&desc);
    return 0;
}
#endif /* SOKOL_NO_ENTRY */
#endif /* __EMSCRIPTEN__ */

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

_SOKOL_PRIVATE const _sapp_gl_fbconfig* _sapp_gl_choose_fbconfig(const _sapp_gl_fbconfig* desired, const _sapp_gl_fbconfig* alternatives, unsigned int count) {
    unsigned int i;
    unsigned int missing, least_missing = 1000000;
    unsigned int color_diff, least_color_diff = 10000000;
    unsigned int extra_diff, least_extra_diff = 10000000;
    const _sapp_gl_fbconfig* current;
    const _sapp_gl_fbconfig* closest = NULL;
    for (i = 0;  i < count;  i++) {
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

/*== WINDOWS ==================================================================*/
#if defined(_WIN32)
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#include <windowsx.h>
#include <shellapi.h>
#pragma comment (lib, "Shell32")

#if defined(SOKOL_D3D11)
#ifndef D3D11_NO_HELPERS
#define D3D11_NO_HELPERS
#endif
#ifndef CINTERFACE
#define CINTERFACE
#endif
#ifndef COBJMACROS
#define COBJMACROS
#endif
#include <windows.h>
#include <d3d11.h>
#include <dxgi.h>
#if (defined(WINAPI_FAMILY_PARTITION) && !WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP))
#pragma comment (lib, "WindowsApp.lib")
#else
#pragma comment (lib, "user32.lib")
#pragma comment (lib, "dxgi.lib")
#pragma comment (lib, "d3d11.lib")
#pragma comment (lib, "dxguid.lib")
#endif
#endif

/* see https://github.com/floooh/sokol/issues/138 */
#ifndef WM_MOUSEHWHEEL
#define WM_MOUSEHWHEEL (0x020E)
#endif

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

static HWND _sapp_win32_hwnd;
static HDC _sapp_win32_dc;
static bool _sapp_win32_in_create_window;
static bool _sapp_win32_dpi_aware;
static float _sapp_win32_content_scale;
static float _sapp_win32_window_scale;
static float _sapp_win32_mouse_scale;
static bool _sapp_win32_iconified;
typedef BOOL(WINAPI * SETPROCESSDPIAWARE_T)(void);
typedef HRESULT(WINAPI * SETPROCESSDPIAWARENESS_T)(PROCESS_DPI_AWARENESS);
typedef HRESULT(WINAPI * GETDPIFORMONITOR_T)(HMONITOR, MONITOR_DPI_TYPE, UINT*, UINT*);
static SETPROCESSDPIAWARE_T _sapp_win32_setprocessdpiaware;
static SETPROCESSDPIAWARENESS_T _sapp_win32_setprocessdpiawareness;
static GETDPIFORMONITOR_T _sapp_win32_getdpiformonitor;
#if defined(SOKOL_D3D11)
static ID3D11Device* _sapp_d3d11_device;
static ID3D11DeviceContext* _sapp_d3d11_device_context;
static DXGI_SWAP_CHAIN_DESC _sapp_dxgi_swap_chain_desc;
static IDXGISwapChain* _sapp_dxgi_swap_chain;
static ID3D11Texture2D* _sapp_d3d11_rt;
static ID3D11RenderTargetView* _sapp_d3d11_rtv;
static ID3D11Texture2D* _sapp_d3d11_ds;
static ID3D11DepthStencilView* _sapp_d3d11_dsv;
#endif
#define WGL_NUMBER_PIXEL_FORMATS_ARB 0x2000
#define WGL_SUPPORT_OPENGL_ARB 0x2010
#define WGL_DRAW_TO_WINDOW_ARB 0x2001
#define WGL_PIXEL_TYPE_ARB 0x2013
#define WGL_TYPE_RGBA_ARB 0x202b
#define WGL_ACCELERATION_ARB 0x2003
#define WGL_NO_ACCELERATION_ARB 0x2025
#define WGL_RED_BITS_ARB 0x2015
#define WGL_RED_SHIFT_ARB 0x2016
#define WGL_GREEN_BITS_ARB 0x2017
#define WGL_GREEN_SHIFT_ARB 0x2018
#define WGL_BLUE_BITS_ARB 0x2019
#define WGL_BLUE_SHIFT_ARB 0x201a
#define WGL_ALPHA_BITS_ARB 0x201b
#define WGL_ALPHA_SHIFT_ARB 0x201c
#define WGL_ACCUM_BITS_ARB 0x201d
#define WGL_ACCUM_RED_BITS_ARB 0x201e
#define WGL_ACCUM_GREEN_BITS_ARB 0x201f
#define WGL_ACCUM_BLUE_BITS_ARB 0x2020
#define WGL_ACCUM_ALPHA_BITS_ARB 0x2021
#define WGL_DEPTH_BITS_ARB 0x2022
#define WGL_STENCIL_BITS_ARB 0x2023
#define WGL_AUX_BUFFERS_ARB 0x2024
#define WGL_STEREO_ARB 0x2012
#define WGL_DOUBLE_BUFFER_ARB 0x2011
#define WGL_SAMPLES_ARB 0x2042
#define WGL_FRAMEBUFFER_SRGB_CAPABLE_ARB 0x20a9
#define WGL_CONTEXT_DEBUG_BIT_ARB 0x00000001
#define WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB 0x00000002
#define WGL_CONTEXT_PROFILE_MASK_ARB 0x9126
#define WGL_CONTEXT_CORE_PROFILE_BIT_ARB 0x00000001
#define WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB 0x00000002
#define WGL_CONTEXT_MAJOR_VERSION_ARB 0x2091
#define WGL_CONTEXT_MINOR_VERSION_ARB 0x2092
#define WGL_CONTEXT_FLAGS_ARB 0x2094
#define WGL_CONTEXT_ROBUST_ACCESS_BIT_ARB 0x00000004
#define WGL_LOSE_CONTEXT_ON_RESET_ARB 0x8252
#define WGL_CONTEXT_RESET_NOTIFICATION_STRATEGY_ARB 0x8256
#define WGL_NO_RESET_NOTIFICATION_ARB 0x8261
#define WGL_CONTEXT_RELEASE_BEHAVIOR_ARB 0x2097
#define WGL_CONTEXT_RELEASE_BEHAVIOR_NONE_ARB 0
#define WGL_CONTEXT_RELEASE_BEHAVIOR_FLUSH_ARB 0x2098
#define WGL_COLORSPACE_EXT 0x309d
#define WGL_COLORSPACE_SRGB_EXT 0x3089
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
static HINSTANCE _sapp_opengl32;
static HGLRC _sapp_gl_ctx;
static PFN_wglCreateContext _sapp_wglCreateContext;
static PFN_wglDeleteContext _sapp_wglDeleteContext;
static PFN_wglGetProcAddress _sapp_wglGetProcAddress;
static PFN_wglGetCurrentDC _sapp_wglGetCurrentDC;
static PFN_wglMakeCurrent _sapp_wglMakeCurrent;
static PFNWGLSWAPINTERVALEXTPROC _sapp_SwapIntervalEXT;
static PFNWGLGETPIXELFORMATATTRIBIVARBPROC _sapp_GetPixelFormatAttribivARB;
static PFNWGLGETEXTENSIONSSTRINGEXTPROC _sapp_GetExtensionsStringEXT;
static PFNWGLGETEXTENSIONSSTRINGARBPROC _sapp_GetExtensionsStringARB;
static PFNWGLCREATECONTEXTATTRIBSARBPROC _sapp_CreateContextAttribsARB;
static bool _sapp_ext_swap_control;
static bool _sapp_arb_multisample;
static bool _sapp_arb_pixel_format;
static bool _sapp_arb_create_context;
static bool _sapp_arb_create_context_profile;
static HWND _sapp_win32_msg_hwnd;
static HDC _sapp_win32_msg_dc;

/* NOTE: the optional GL loader only contains the GL constants and functions required for sokol_gfx.h, if you need
more, you'll need to use you own gl header-generator/loader
*/
#if !defined(SOKOL_WIN32_NO_GL_LOADER)
#if defined(SOKOL_GLCORE33)
#define __gl_h_ 1
#define __gl32_h_ 1
#define __gl31_h_ 1
#define __GL_H__ 1
#define __glext_h_ 1
#define __GLEXT_H_ 1
#define __gltypes_h_ 1
#define __glcorearb_h_ 1
#define __gl_glcorearb_h_ 1
#define GL_APIENTRY APIENTRY

typedef unsigned int  GLenum;
typedef unsigned int  GLuint;
typedef int  GLsizei;
typedef char  GLchar;
typedef ptrdiff_t  GLintptr;
typedef ptrdiff_t  GLsizeiptr;
typedef double  GLclampd;
typedef unsigned short  GLushort;
typedef unsigned char  GLubyte;
typedef unsigned char  GLboolean;
typedef uint64_t  GLuint64;
typedef double  GLdouble;
typedef unsigned short  GLhalf;
typedef float  GLclampf;
typedef unsigned int  GLbitfield;
typedef signed char  GLbyte;
typedef short  GLshort;
typedef void  GLvoid;
typedef int64_t  GLint64;
typedef float  GLfloat;
typedef struct __GLsync * GLsync;
typedef int  GLint;
#define GL_INT_2_10_10_10_REV 0x8D9F
#define GL_R32F 0x822E
#define GL_PROGRAM_POINT_SIZE 0x8642
#define GL_STENCIL_ATTACHMENT 0x8D20
#define GL_DEPTH_ATTACHMENT 0x8D00
#define GL_COLOR_ATTACHMENT2 0x8CE2
#define GL_COLOR_ATTACHMENT0 0x8CE0
#define GL_R16F 0x822D
#define GL_COLOR_ATTACHMENT22 0x8CF6
#define GL_DRAW_FRAMEBUFFER 0x8CA9
#define GL_FRAMEBUFFER_COMPLETE 0x8CD5
#define GL_NUM_EXTENSIONS 0x821D
#define GL_INFO_LOG_LENGTH 0x8B84
#define GL_VERTEX_SHADER 0x8B31
#define GL_INCR 0x1E02
#define GL_DYNAMIC_DRAW 0x88E8
#define GL_STATIC_DRAW 0x88E4
#define GL_TEXTURE_CUBE_MAP_POSITIVE_Z 0x8519
#define GL_TEXTURE_CUBE_MAP 0x8513
#define GL_FUNC_SUBTRACT 0x800A
#define GL_FUNC_REVERSE_SUBTRACT 0x800B
#define GL_CONSTANT_COLOR 0x8001
#define GL_DECR_WRAP 0x8508
#define GL_R8 0x8229
#define GL_LINEAR_MIPMAP_LINEAR 0x2703
#define GL_ELEMENT_ARRAY_BUFFER 0x8893
#define GL_SHORT 0x1402
#define GL_DEPTH_TEST 0x0B71
#define GL_TEXTURE_CUBE_MAP_NEGATIVE_Y 0x8518
#define GL_LINK_STATUS 0x8B82
#define GL_TEXTURE_CUBE_MAP_POSITIVE_Y 0x8517
#define GL_SAMPLE_ALPHA_TO_COVERAGE 0x809E
#define GL_RGBA16F 0x881A
#define GL_CONSTANT_ALPHA 0x8003
#define GL_READ_FRAMEBUFFER 0x8CA8
#define GL_TEXTURE0 0x84C0
#define GL_TEXTURE_MIN_LOD 0x813A
#define GL_CLAMP_TO_EDGE 0x812F
#define GL_UNSIGNED_SHORT_5_6_5 0x8363
#define GL_TEXTURE_WRAP_R 0x8072
#define GL_UNSIGNED_SHORT_5_5_5_1 0x8034
#define GL_NEAREST_MIPMAP_NEAREST 0x2700
#define GL_UNSIGNED_SHORT_4_4_4_4 0x8033
#define GL_SRC_ALPHA_SATURATE 0x0308
#define GL_STREAM_DRAW 0x88E0
#define GL_ONE 1
#define GL_NEAREST_MIPMAP_LINEAR 0x2702
#define GL_RGB10_A2 0x8059
#define GL_RGBA8 0x8058
#define GL_COLOR_ATTACHMENT1 0x8CE1
#define GL_RGBA4 0x8056
#define GL_RGB8 0x8051
#define GL_ARRAY_BUFFER 0x8892
#define GL_STENCIL 0x1802
#define GL_TEXTURE_2D 0x0DE1
#define GL_DEPTH 0x1801
#define GL_FRONT 0x0404
#define GL_STENCIL_BUFFER_BIT 0x00000400
#define GL_REPEAT 0x2901
#define GL_RGBA 0x1908
#define GL_TEXTURE_CUBE_MAP_POSITIVE_X 0x8515
#define GL_DECR 0x1E03
#define GL_FRAGMENT_SHADER 0x8B30
#define GL_FLOAT 0x1406
#define GL_TEXTURE_MAX_LOD 0x813B
#define GL_DEPTH_COMPONENT 0x1902
#define GL_ONE_MINUS_DST_ALPHA 0x0305
#define GL_COLOR 0x1800
#define GL_TEXTURE_2D_ARRAY 0x8C1A
#define GL_TRIANGLES 0x0004
#define GL_UNSIGNED_BYTE 0x1401
#define GL_TEXTURE_MAG_FILTER 0x2800
#define GL_ONE_MINUS_CONSTANT_ALPHA 0x8004
#define GL_NONE 0
#define GL_SRC_COLOR 0x0300
#define GL_BYTE 0x1400
#define GL_TEXTURE_CUBE_MAP_NEGATIVE_Z 0x851A
#define GL_LINE_STRIP 0x0003
#define GL_TEXTURE_3D 0x806F
#define GL_CW 0x0900
#define GL_LINEAR 0x2601
#define GL_RENDERBUFFER 0x8D41
#define GL_GEQUAL 0x0206
#define GL_COLOR_BUFFER_BIT 0x00004000
#define GL_RGBA32F 0x8814
#define GL_BLEND 0x0BE2
#define GL_ONE_MINUS_SRC_ALPHA 0x0303
#define GL_ONE_MINUS_CONSTANT_COLOR 0x8002
#define GL_TEXTURE_WRAP_T 0x2803
#define GL_TEXTURE_WRAP_S 0x2802
#define GL_TEXTURE_MIN_FILTER 0x2801
#define GL_LINEAR_MIPMAP_NEAREST 0x2701
#define GL_EXTENSIONS 0x1F03
#define GL_NO_ERROR 0
#define GL_REPLACE 0x1E01
#define GL_KEEP 0x1E00
#define GL_CCW 0x0901
#define GL_TEXTURE_CUBE_MAP_NEGATIVE_X 0x8516
#define GL_RGB 0x1907
#define GL_TRIANGLE_STRIP 0x0005
#define GL_FALSE 0
#define GL_ZERO 0
#define GL_CULL_FACE 0x0B44
#define GL_INVERT 0x150A
#define GL_INT 0x1404
#define GL_UNSIGNED_INT 0x1405
#define GL_UNSIGNED_SHORT 0x1403
#define GL_NEAREST 0x2600
#define GL_SCISSOR_TEST 0x0C11
#define GL_LEQUAL 0x0203
#define GL_STENCIL_TEST 0x0B90
#define GL_DITHER 0x0BD0
#define GL_DEPTH_COMPONENT16 0x81A5
#define GL_EQUAL 0x0202
#define GL_FRAMEBUFFER 0x8D40
#define GL_RGB5 0x8050
#define GL_LINES 0x0001
#define GL_DEPTH_BUFFER_BIT 0x00000100
#define GL_SRC_ALPHA 0x0302
#define GL_INCR_WRAP 0x8507
#define GL_LESS 0x0201
#define GL_MULTISAMPLE 0x809D
#define GL_FRAMEBUFFER_BINDING 0x8CA6
#define GL_BACK 0x0405
#define GL_ALWAYS 0x0207
#define GL_FUNC_ADD 0x8006
#define GL_ONE_MINUS_DST_COLOR 0x0307
#define GL_NOTEQUAL 0x0205
#define GL_DST_COLOR 0x0306
#define GL_COMPILE_STATUS 0x8B81
#define GL_RED 0x1903
#define GL_COLOR_ATTACHMENT3 0x8CE3
#define GL_DST_ALPHA 0x0304
#define GL_RGB5_A1 0x8057
#define GL_GREATER 0x0204
#define GL_POLYGON_OFFSET_FILL 0x8037
#define GL_TRUE 1
#define GL_NEVER 0x0200
#define GL_POINTS 0x0000
#define GL_ONE_MINUS_SRC_COLOR 0x0301
#define GL_MIRRORED_REPEAT 0x8370
#define GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS 0x8B4D
#define GL_R11F_G11F_B10F 0x8C3A
#define GL_UNSIGNED_INT_10F_11F_11F_REV 0x8C3B
#define GL_RGBA32UI 0x8D70
#define GL_RGB32UI 0x8D71
#define GL_RGBA16UI 0x8D76
#define GL_RGB16UI 0x8D77
#define GL_RGBA8UI 0x8D7C
#define GL_RGB8UI 0x8D7D
#define GL_RGBA32I 0x8D82
#define GL_RGB32I 0x8D83
#define GL_RGBA16I 0x8D88
#define GL_RGB16I 0x8D89
#define GL_RGBA8I 0x8D8E
#define GL_RGB8I 0x8D8F
#define GL_RED_INTEGER 0x8D94
#define GL_RG 0x8227
#define GL_RG_INTEGER 0x8228
#define GL_R8 0x8229
#define GL_R16 0x822A
#define GL_RG8 0x822B
#define GL_RG16 0x822C
#define GL_R16F 0x822D
#define GL_R32F 0x822E
#define GL_RG16F 0x822F
#define GL_RG32F 0x8230
#define GL_R8I 0x8231
#define GL_R8UI 0x8232
#define GL_R16I 0x8233
#define GL_R16UI 0x8234
#define GL_R32I 0x8235
#define GL_R32UI 0x8236
#define GL_RG8I 0x8237
#define GL_RG8UI 0x8238
#define GL_RG16I 0x8239
#define GL_RG16UI 0x823A
#define GL_RG32I 0x823B
#define GL_RG32UI 0x823C
#define GL_RGBA_INTEGER 0x8D99
#define GL_R8_SNORM 0x8F94
#define GL_RG8_SNORM 0x8F95
#define GL_RGB8_SNORM 0x8F96
#define GL_RGBA8_SNORM 0x8F97
#define GL_R16_SNORM 0x8F98
#define GL_RG16_SNORM 0x8F99
#define GL_RGB16_SNORM 0x8F9A
#define GL_RGBA16_SNORM 0x8F9B
#define GL_RGBA16 0x805B
#define GL_MAX_TEXTURE_SIZE 0x0D33
#define GL_MAX_CUBE_MAP_TEXTURE_SIZE 0x851C
#define GL_MAX_3D_TEXTURE_SIZE 0x8073
#define GL_MAX_ARRAY_TEXTURE_LAYERS 0x88FF
#define GL_MAX_VERTEX_ATTRIBS 0x8869
#define GL_CLAMP_TO_BORDER 0x812D
#define GL_TEXTURE_BORDER_COLOR 0x1004

typedef void  (GL_APIENTRY *PFN_glBindVertexArray)(GLuint array);
static PFN_glBindVertexArray _sapp_glBindVertexArray;
typedef void  (GL_APIENTRY *PFN_glFramebufferTextureLayer)(GLenum target, GLenum attachment, GLuint texture, GLint level, GLint layer);
static PFN_glFramebufferTextureLayer _sapp_glFramebufferTextureLayer;
typedef void  (GL_APIENTRY *PFN_glGenFramebuffers)(GLsizei n, GLuint * framebuffers);
static PFN_glGenFramebuffers _sapp_glGenFramebuffers;
typedef void  (GL_APIENTRY *PFN_glBindFramebuffer)(GLenum target, GLuint framebuffer);
static PFN_glBindFramebuffer _sapp_glBindFramebuffer;
typedef void  (GL_APIENTRY *PFN_glBindRenderbuffer)(GLenum target, GLuint renderbuffer);
static PFN_glBindRenderbuffer _sapp_glBindRenderbuffer;
typedef const GLubyte * (GL_APIENTRY *PFN_glGetStringi)(GLenum name, GLuint index);
static PFN_glGetStringi _sapp_glGetStringi;
typedef void  (GL_APIENTRY *PFN_glClearBufferfi)(GLenum buffer, GLint drawbuffer, GLfloat depth, GLint stencil);
static PFN_glClearBufferfi _sapp_glClearBufferfi;
typedef void  (GL_APIENTRY *PFN_glClearBufferfv)(GLenum buffer, GLint drawbuffer, const GLfloat * value);
static PFN_glClearBufferfv _sapp_glClearBufferfv;
typedef void  (GL_APIENTRY *PFN_glClearBufferuiv)(GLenum buffer, GLint drawbuffer, const GLuint * value);
static PFN_glClearBufferuiv _sapp_glClearBufferuiv;
typedef void  (GL_APIENTRY *PFN_glDeleteRenderbuffers)(GLsizei n, const GLuint * renderbuffers);
static PFN_glDeleteRenderbuffers _sapp_glDeleteRenderbuffers;
typedef void  (GL_APIENTRY *PFN_glUniform4fv)(GLint location, GLsizei count, const GLfloat * value);
static PFN_glUniform4fv _sapp_glUniform4fv;
typedef void  (GL_APIENTRY *PFN_glUniform2fv)(GLint location, GLsizei count, const GLfloat * value);
static PFN_glUniform2fv _sapp_glUniform2fv;
typedef void  (GL_APIENTRY *PFN_glUseProgram)(GLuint program);
static PFN_glUseProgram _sapp_glUseProgram;
typedef void  (GL_APIENTRY *PFN_glShaderSource)(GLuint shader, GLsizei count, const GLchar *const* string, const GLint * length);
static PFN_glShaderSource _sapp_glShaderSource;
typedef void  (GL_APIENTRY *PFN_glLinkProgram)(GLuint program);
static PFN_glLinkProgram _sapp_glLinkProgram;
typedef GLint (GL_APIENTRY *PFN_glGetUniformLocation)(GLuint program, const GLchar * name);
static PFN_glGetUniformLocation _sapp_glGetUniformLocation;
typedef void  (GL_APIENTRY *PFN_glGetShaderiv)(GLuint shader, GLenum pname, GLint * params);
static PFN_glGetShaderiv _sapp_glGetShaderiv;
typedef void  (GL_APIENTRY *PFN_glGetProgramInfoLog)(GLuint program, GLsizei bufSize, GLsizei * length, GLchar * infoLog);
static PFN_glGetProgramInfoLog _sapp_glGetProgramInfoLog;
typedef GLint (GL_APIENTRY *PFN_glGetAttribLocation)(GLuint program, const GLchar * name);
static PFN_glGetAttribLocation _sapp_glGetAttribLocation;
typedef void  (GL_APIENTRY *PFN_glDisableVertexAttribArray)(GLuint index);
static PFN_glDisableVertexAttribArray _sapp_glDisableVertexAttribArray;
typedef void  (GL_APIENTRY *PFN_glDeleteShader)(GLuint shader);
static PFN_glDeleteShader _sapp_glDeleteShader;
typedef void  (GL_APIENTRY *PFN_glDeleteProgram)(GLuint program);
static PFN_glDeleteProgram _sapp_glDeleteProgram;
typedef void  (GL_APIENTRY *PFN_glCompileShader)(GLuint shader);
static PFN_glCompileShader _sapp_glCompileShader;
typedef void  (GL_APIENTRY *PFN_glStencilFuncSeparate)(GLenum face, GLenum func, GLint ref, GLuint mask);
static PFN_glStencilFuncSeparate _sapp_glStencilFuncSeparate;
typedef void  (GL_APIENTRY *PFN_glStencilOpSeparate)(GLenum face, GLenum sfail, GLenum dpfail, GLenum dppass);
static PFN_glStencilOpSeparate _sapp_glStencilOpSeparate;
typedef void  (GL_APIENTRY *PFN_glRenderbufferStorageMultisample)(GLenum target, GLsizei samples, GLenum internalformat, GLsizei width, GLsizei height);
static PFN_glRenderbufferStorageMultisample _sapp_glRenderbufferStorageMultisample;
typedef void  (GL_APIENTRY *PFN_glDrawBuffers)(GLsizei n, const GLenum * bufs);
static PFN_glDrawBuffers _sapp_glDrawBuffers;
typedef void  (GL_APIENTRY *PFN_glVertexAttribDivisor)(GLuint index, GLuint divisor);
static PFN_glVertexAttribDivisor _sapp_glVertexAttribDivisor;
typedef void  (GL_APIENTRY *PFN_glBufferSubData)(GLenum target, GLintptr offset, GLsizeiptr size, const void * data);
static PFN_glBufferSubData _sapp_glBufferSubData;
typedef void  (GL_APIENTRY *PFN_glGenBuffers)(GLsizei n, GLuint * buffers);
static PFN_glGenBuffers _sapp_glGenBuffers;
typedef GLenum (GL_APIENTRY *PFN_glCheckFramebufferStatus)(GLenum target);
static PFN_glCheckFramebufferStatus _sapp_glCheckFramebufferStatus;
typedef void  (GL_APIENTRY *PFN_glFramebufferRenderbuffer)(GLenum target, GLenum attachment, GLenum renderbuffertarget, GLuint renderbuffer);
static PFN_glFramebufferRenderbuffer _sapp_glFramebufferRenderbuffer;
typedef void  (GL_APIENTRY *PFN_glCompressedTexImage2D)(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLsizei imageSize, const void * data);
static PFN_glCompressedTexImage2D _sapp_glCompressedTexImage2D;
typedef void  (GL_APIENTRY *PFN_glCompressedTexImage3D)(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLsizei imageSize, const void * data);
static PFN_glCompressedTexImage3D _sapp_glCompressedTexImage3D;
typedef void  (GL_APIENTRY *PFN_glActiveTexture)(GLenum texture);
static PFN_glActiveTexture _sapp_glActiveTexture;
typedef void  (GL_APIENTRY *PFN_glTexSubImage3D)(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, const void * pixels);
static PFN_glTexSubImage3D _sapp_glTexSubImage3D;
typedef void  (GL_APIENTRY *PFN_glUniformMatrix4fv)(GLint location, GLsizei count, GLboolean transpose, const GLfloat * value);
static PFN_glUniformMatrix4fv _sapp_glUniformMatrix4fv;
typedef void  (GL_APIENTRY *PFN_glRenderbufferStorage)(GLenum target, GLenum internalformat, GLsizei width, GLsizei height);
static PFN_glRenderbufferStorage _sapp_glRenderbufferStorage;
typedef void  (GL_APIENTRY *PFN_glGenTextures)(GLsizei n, GLuint * textures);
static PFN_glGenTextures _sapp_glGenTextures;
typedef void  (GL_APIENTRY *PFN_glPolygonOffset)(GLfloat factor, GLfloat units);
static PFN_glPolygonOffset _sapp_glPolygonOffset;
typedef void  (GL_APIENTRY *PFN_glDrawElements)(GLenum mode, GLsizei count, GLenum type, const void * indices);
static PFN_glDrawElements _sapp_glDrawElements;
typedef void  (GL_APIENTRY *PFN_glDeleteFramebuffers)(GLsizei n, const GLuint * framebuffers);
static PFN_glDeleteFramebuffers _sapp_glDeleteFramebuffers;
typedef void  (GL_APIENTRY *PFN_glBlendEquationSeparate)(GLenum modeRGB, GLenum modeAlpha);
static PFN_glBlendEquationSeparate _sapp_glBlendEquationSeparate;
typedef void  (GL_APIENTRY *PFN_glDeleteTextures)(GLsizei n, const GLuint * textures);
static PFN_glDeleteTextures _sapp_glDeleteTextures;
typedef void  (GL_APIENTRY *PFN_glGetProgramiv)(GLuint program, GLenum pname, GLint * params);
static PFN_glGetProgramiv _sapp_glGetProgramiv;
typedef void  (GL_APIENTRY *PFN_glBindTexture)(GLenum target, GLuint texture);
static PFN_glBindTexture _sapp_glBindTexture;
typedef void  (GL_APIENTRY *PFN_glTexImage3D)(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLenum format, GLenum type, const void * pixels);
static PFN_glTexImage3D _sapp_glTexImage3D;
typedef GLuint (GL_APIENTRY *PFN_glCreateShader)(GLenum type);
static PFN_glCreateShader _sapp_glCreateShader;
typedef void  (GL_APIENTRY *PFN_glTexSubImage2D)(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const void * pixels);
static PFN_glTexSubImage2D _sapp_glTexSubImage2D;
typedef void  (GL_APIENTRY *PFN_glClearDepth)(GLdouble depth);
static PFN_glClearDepth _sapp_glClearDepth;
typedef void  (GL_APIENTRY *PFN_glFramebufferTexture2D)(GLenum target, GLenum attachment, GLenum textarget, GLuint texture, GLint level);
static PFN_glFramebufferTexture2D _sapp_glFramebufferTexture2D;
typedef GLuint (GL_APIENTRY *PFN_glCreateProgram)();
static PFN_glCreateProgram _sapp_glCreateProgram;
typedef void  (GL_APIENTRY *PFN_glViewport)(GLint x, GLint y, GLsizei width, GLsizei height);
static PFN_glViewport _sapp_glViewport;
typedef void  (GL_APIENTRY *PFN_glDeleteBuffers)(GLsizei n, const GLuint * buffers);
static PFN_glDeleteBuffers _sapp_glDeleteBuffers;
typedef void  (GL_APIENTRY *PFN_glDrawArrays)(GLenum mode, GLint first, GLsizei count);
static PFN_glDrawArrays _sapp_glDrawArrays;
typedef void  (GL_APIENTRY *PFN_glDrawElementsInstanced)(GLenum mode, GLsizei count, GLenum type, const void * indices, GLsizei instancecount);
static PFN_glDrawElementsInstanced _sapp_glDrawElementsInstanced;
typedef void  (GL_APIENTRY *PFN_glVertexAttribPointer)(GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride, const void * pointer);
static PFN_glVertexAttribPointer _sapp_glVertexAttribPointer;
typedef void  (GL_APIENTRY *PFN_glUniform1i)(GLint location, GLint v0);
static PFN_glUniform1i _sapp_glUniform1i;
typedef void  (GL_APIENTRY *PFN_glDisable)(GLenum cap);
static PFN_glDisable _sapp_glDisable;
typedef void  (GL_APIENTRY *PFN_glColorMask)(GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha);
static PFN_glColorMask _sapp_glColorMask;
typedef void  (GL_APIENTRY *PFN_glBindBuffer)(GLenum target, GLuint buffer);
static PFN_glBindBuffer _sapp_glBindBuffer;
typedef void  (GL_APIENTRY *PFN_glDeleteVertexArrays)(GLsizei n, const GLuint * arrays);
static PFN_glDeleteVertexArrays _sapp_glDeleteVertexArrays;
typedef void  (GL_APIENTRY *PFN_glDepthMask)(GLboolean flag);
static PFN_glDepthMask _sapp_glDepthMask;
typedef void  (GL_APIENTRY *PFN_glDrawArraysInstanced)(GLenum mode, GLint first, GLsizei count, GLsizei instancecount);
static PFN_glDrawArraysInstanced _sapp_glDrawArraysInstanced;
typedef void  (GL_APIENTRY *PFN_glClearStencil)(GLint s);
static PFN_glClearStencil _sapp_glClearStencil;
typedef void  (GL_APIENTRY *PFN_glScissor)(GLint x, GLint y, GLsizei width, GLsizei height);
static PFN_glScissor _sapp_glScissor;
typedef void  (GL_APIENTRY *PFN_glUniform3fv)(GLint location, GLsizei count, const GLfloat * value);
static PFN_glUniform3fv _sapp_glUniform3fv;
typedef void  (GL_APIENTRY *PFN_glGenRenderbuffers)(GLsizei n, GLuint * renderbuffers);
static PFN_glGenRenderbuffers _sapp_glGenRenderbuffers;
typedef void  (GL_APIENTRY *PFN_glBufferData)(GLenum target, GLsizeiptr size, const void * data, GLenum usage);
static PFN_glBufferData _sapp_glBufferData;
typedef void  (GL_APIENTRY *PFN_glBlendFuncSeparate)(GLenum sfactorRGB, GLenum dfactorRGB, GLenum sfactorAlpha, GLenum dfactorAlpha);
static PFN_glBlendFuncSeparate _sapp_glBlendFuncSeparate;
typedef void  (GL_APIENTRY *PFN_glTexParameteri)(GLenum target, GLenum pname, GLint param);
static PFN_glTexParameteri _sapp_glTexParameteri;
typedef void  (GL_APIENTRY *PFN_glGetIntegerv)(GLenum pname, GLint * data);
static PFN_glGetIntegerv _sapp_glGetIntegerv;
typedef void  (GL_APIENTRY *PFN_glEnable)(GLenum cap);
static PFN_glEnable _sapp_glEnable;
typedef void  (GL_APIENTRY *PFN_glBlitFramebuffer)(GLint srcX0, GLint srcY0, GLint srcX1, GLint srcY1, GLint dstX0, GLint dstY0, GLint dstX1, GLint dstY1, GLbitfield mask, GLenum filter);
static PFN_glBlitFramebuffer _sapp_glBlitFramebuffer;
typedef void  (GL_APIENTRY *PFN_glStencilMask)(GLuint mask);
static PFN_glStencilMask _sapp_glStencilMask;
typedef void  (GL_APIENTRY *PFN_glAttachShader)(GLuint program, GLuint shader);
static PFN_glAttachShader _sapp_glAttachShader;
typedef GLenum (GL_APIENTRY *PFN_glGetError)();
static PFN_glGetError _sapp_glGetError;
typedef void  (GL_APIENTRY *PFN_glClearColor)(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);
static PFN_glClearColor _sapp_glClearColor;
typedef void  (GL_APIENTRY *PFN_glBlendColor)(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);
static PFN_glBlendColor _sapp_glBlendColor;
typedef void  (GL_APIENTRY *PFN_glTexParameterf)(GLenum target, GLenum pname, GLfloat param);
static PFN_glTexParameterf _sapp_glTexParameterf;
typedef void  (GL_APIENTRY *PFN_glTexParameterfv)(GLenum target, GLenum pname, GLfloat* params);
static PFN_glTexParameterfv _sapp_glTexParameterfv;
typedef void  (GL_APIENTRY *PFN_glGetShaderInfoLog)(GLuint shader, GLsizei bufSize, GLsizei * length, GLchar * infoLog);
static PFN_glGetShaderInfoLog _sapp_glGetShaderInfoLog;
typedef void  (GL_APIENTRY *PFN_glDepthFunc)(GLenum func);
static PFN_glDepthFunc _sapp_glDepthFunc;
typedef void  (GL_APIENTRY *PFN_glStencilOp)(GLenum fail, GLenum zfail, GLenum zpass);
static PFN_glStencilOp _sapp_glStencilOp;
typedef void  (GL_APIENTRY *PFN_glStencilFunc)(GLenum func, GLint ref, GLuint mask);
static PFN_glStencilFunc _sapp_glStencilFunc;
typedef void  (GL_APIENTRY *PFN_glEnableVertexAttribArray)(GLuint index);
static PFN_glEnableVertexAttribArray _sapp_glEnableVertexAttribArray;
typedef void  (GL_APIENTRY *PFN_glBlendFunc)(GLenum sfactor, GLenum dfactor);
static PFN_glBlendFunc _sapp_glBlendFunc;
typedef void  (GL_APIENTRY *PFN_glUniform1fv)(GLint location, GLsizei count, const GLfloat * value);
static PFN_glUniform1fv _sapp_glUniform1fv;
typedef void  (GL_APIENTRY *PFN_glReadBuffer)(GLenum src);
static PFN_glReadBuffer _sapp_glReadBuffer;
typedef void  (GL_APIENTRY *PFN_glClear)(GLbitfield mask);
static PFN_glClear _sapp_glClear;
typedef void  (GL_APIENTRY *PFN_glTexImage2D)(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const void * pixels);
static PFN_glTexImage2D _sapp_glTexImage2D;
typedef void  (GL_APIENTRY *PFN_glGenVertexArrays)(GLsizei n, GLuint * arrays);
static PFN_glGenVertexArrays _sapp_glGenVertexArrays;
typedef void  (GL_APIENTRY *PFN_glFrontFace)(GLenum mode);
static PFN_glFrontFace _sapp_glFrontFace;
typedef void  (GL_APIENTRY *PFN_glCullFace)(GLenum mode);
static PFN_glCullFace _sapp_glCullFace;

_SOKOL_PRIVATE void* _sapp_win32_glgetprocaddr(const char* name) {
    void* proc_addr = (void*) _sapp_wglGetProcAddress(name);
    if (0 == proc_addr) {
        proc_addr = (void*) GetProcAddress(_sapp_opengl32, name);
    }
    SOKOL_ASSERT(proc_addr);
    return proc_addr;
}

#define _SAPP_GLPROC(name) _sapp_ ## name = (PFN_ ## name) _sapp_win32_glgetprocaddr(#name)

_SOKOL_PRIVATE  void _sapp_win32_gl_loadfuncs(void) {
    SOKOL_ASSERT(_sapp_wglGetProcAddress);
    SOKOL_ASSERT(_sapp_opengl32);
    _SAPP_GLPROC(glBindVertexArray);
    _SAPP_GLPROC(glFramebufferTextureLayer);
    _SAPP_GLPROC(glGenFramebuffers);
    _SAPP_GLPROC(glBindFramebuffer);
    _SAPP_GLPROC(glBindRenderbuffer);
    _SAPP_GLPROC(glGetStringi);
    _SAPP_GLPROC(glClearBufferfi);
    _SAPP_GLPROC(glClearBufferfv);
    _SAPP_GLPROC(glClearBufferuiv);
    _SAPP_GLPROC(glDeleteRenderbuffers);
    _SAPP_GLPROC(glUniform4fv);
    _SAPP_GLPROC(glUniform2fv);
    _SAPP_GLPROC(glUseProgram);
    _SAPP_GLPROC(glShaderSource);
    _SAPP_GLPROC(glLinkProgram);
    _SAPP_GLPROC(glGetUniformLocation);
    _SAPP_GLPROC(glGetShaderiv);
    _SAPP_GLPROC(glGetProgramInfoLog);
    _SAPP_GLPROC(glGetAttribLocation);
    _SAPP_GLPROC(glDisableVertexAttribArray);
    _SAPP_GLPROC(glDeleteShader);
    _SAPP_GLPROC(glDeleteProgram);
    _SAPP_GLPROC(glCompileShader);
    _SAPP_GLPROC(glStencilFuncSeparate);
    _SAPP_GLPROC(glStencilOpSeparate);
    _SAPP_GLPROC(glRenderbufferStorageMultisample);
    _SAPP_GLPROC(glDrawBuffers);
    _SAPP_GLPROC(glVertexAttribDivisor);
    _SAPP_GLPROC(glBufferSubData);
    _SAPP_GLPROC(glGenBuffers);
    _SAPP_GLPROC(glCheckFramebufferStatus);
    _SAPP_GLPROC(glFramebufferRenderbuffer);
    _SAPP_GLPROC(glCompressedTexImage2D);
    _SAPP_GLPROC(glCompressedTexImage3D);
    _SAPP_GLPROC(glActiveTexture);
    _SAPP_GLPROC(glTexSubImage3D);
    _SAPP_GLPROC(glUniformMatrix4fv);
    _SAPP_GLPROC(glRenderbufferStorage);
    _SAPP_GLPROC(glGenTextures);
    _SAPP_GLPROC(glPolygonOffset);
    _SAPP_GLPROC(glDrawElements);
    _SAPP_GLPROC(glDeleteFramebuffers);
    _SAPP_GLPROC(glBlendEquationSeparate);
    _SAPP_GLPROC(glDeleteTextures);
    _SAPP_GLPROC(glGetProgramiv);
    _SAPP_GLPROC(glBindTexture);
    _SAPP_GLPROC(glTexImage3D);
    _SAPP_GLPROC(glCreateShader);
    _SAPP_GLPROC(glTexSubImage2D);
    _SAPP_GLPROC(glClearDepth);
    _SAPP_GLPROC(glFramebufferTexture2D);
    _SAPP_GLPROC(glCreateProgram);
    _SAPP_GLPROC(glViewport);
    _SAPP_GLPROC(glDeleteBuffers);
    _SAPP_GLPROC(glDrawArrays);
    _SAPP_GLPROC(glDrawElementsInstanced);
    _SAPP_GLPROC(glVertexAttribPointer);
    _SAPP_GLPROC(glUniform1i);
    _SAPP_GLPROC(glDisable);
    _SAPP_GLPROC(glColorMask);
    _SAPP_GLPROC(glBindBuffer);
    _SAPP_GLPROC(glDeleteVertexArrays);
    _SAPP_GLPROC(glDepthMask);
    _SAPP_GLPROC(glDrawArraysInstanced);
    _SAPP_GLPROC(glClearStencil);
    _SAPP_GLPROC(glScissor);
    _SAPP_GLPROC(glUniform3fv);
    _SAPP_GLPROC(glGenRenderbuffers);
    _SAPP_GLPROC(glBufferData);
    _SAPP_GLPROC(glBlendFuncSeparate);
    _SAPP_GLPROC(glTexParameteri);
    _SAPP_GLPROC(glGetIntegerv);
    _SAPP_GLPROC(glEnable);
    _SAPP_GLPROC(glBlitFramebuffer);
    _SAPP_GLPROC(glStencilMask);
    _SAPP_GLPROC(glAttachShader);
    _SAPP_GLPROC(glGetError);
    _SAPP_GLPROC(glClearColor);
    _SAPP_GLPROC(glBlendColor);
    _SAPP_GLPROC(glTexParameterf);
    _SAPP_GLPROC(glTexParameterfv);
    _SAPP_GLPROC(glGetShaderInfoLog);
    _SAPP_GLPROC(glDepthFunc);
    _SAPP_GLPROC(glStencilOp);
    _SAPP_GLPROC(glStencilFunc);
    _SAPP_GLPROC(glEnableVertexAttribArray);
    _SAPP_GLPROC(glBlendFunc);
    _SAPP_GLPROC(glUniform1fv);
    _SAPP_GLPROC(glReadBuffer);
    _SAPP_GLPROC(glClear);
    _SAPP_GLPROC(glTexImage2D);
    _SAPP_GLPROC(glGenVertexArrays);
    _SAPP_GLPROC(glFrontFace);
    _SAPP_GLPROC(glCullFace);
}
#define glBindVertexArray _sapp_glBindVertexArray
#define glFramebufferTextureLayer _sapp_glFramebufferTextureLayer
#define glGenFramebuffers _sapp_glGenFramebuffers
#define glBindFramebuffer _sapp_glBindFramebuffer
#define glBindRenderbuffer _sapp_glBindRenderbuffer
#define glGetStringi _sapp_glGetStringi
#define glClearBufferfi _sapp_glClearBufferfi
#define glClearBufferfv _sapp_glClearBufferfv
#define glClearBufferuiv _sapp_glClearBufferuiv
#define glDeleteRenderbuffers _sapp_glDeleteRenderbuffers
#define glUniform4fv _sapp_glUniform4fv
#define glUniform2fv _sapp_glUniform2fv
#define glUseProgram _sapp_glUseProgram
#define glShaderSource _sapp_glShaderSource
#define glLinkProgram _sapp_glLinkProgram
#define glGetUniformLocation _sapp_glGetUniformLocation
#define glGetShaderiv _sapp_glGetShaderiv
#define glGetProgramInfoLog _sapp_glGetProgramInfoLog
#define glGetAttribLocation _sapp_glGetAttribLocation
#define glDisableVertexAttribArray _sapp_glDisableVertexAttribArray
#define glDeleteShader _sapp_glDeleteShader
#define glDeleteProgram _sapp_glDeleteProgram
#define glCompileShader _sapp_glCompileShader
#define glStencilFuncSeparate _sapp_glStencilFuncSeparate
#define glStencilOpSeparate _sapp_glStencilOpSeparate
#define glRenderbufferStorageMultisample _sapp_glRenderbufferStorageMultisample
#define glDrawBuffers _sapp_glDrawBuffers
#define glVertexAttribDivisor _sapp_glVertexAttribDivisor
#define glBufferSubData _sapp_glBufferSubData
#define glGenBuffers _sapp_glGenBuffers
#define glCheckFramebufferStatus _sapp_glCheckFramebufferStatus
#define glFramebufferRenderbuffer _sapp_glFramebufferRenderbuffer
#define glCompressedTexImage2D _sapp_glCompressedTexImage2D
#define glCompressedTexImage3D _sapp_glCompressedTexImage3D
#define glActiveTexture _sapp_glActiveTexture
#define glTexSubImage3D _sapp_glTexSubImage3D
#define glUniformMatrix4fv _sapp_glUniformMatrix4fv
#define glRenderbufferStorage _sapp_glRenderbufferStorage
#define glGenTextures _sapp_glGenTextures
#define glPolygonOffset _sapp_glPolygonOffset
#define glDrawElements _sapp_glDrawElements
#define glDeleteFramebuffers _sapp_glDeleteFramebuffers
#define glBlendEquationSeparate _sapp_glBlendEquationSeparate
#define glDeleteTextures _sapp_glDeleteTextures
#define glGetProgramiv _sapp_glGetProgramiv
#define glBindTexture _sapp_glBindTexture
#define glTexImage3D _sapp_glTexImage3D
#define glCreateShader _sapp_glCreateShader
#define glTexSubImage2D _sapp_glTexSubImage2D
#define glClearDepth _sapp_glClearDepth
#define glFramebufferTexture2D _sapp_glFramebufferTexture2D
#define glCreateProgram _sapp_glCreateProgram
#define glViewport _sapp_glViewport
#define glDeleteBuffers _sapp_glDeleteBuffers
#define glDrawArrays _sapp_glDrawArrays
#define glDrawElementsInstanced _sapp_glDrawElementsInstanced
#define glVertexAttribPointer _sapp_glVertexAttribPointer
#define glUniform1i _sapp_glUniform1i
#define glDisable _sapp_glDisable
#define glColorMask _sapp_glColorMask
#define glBindBuffer _sapp_glBindBuffer
#define glDeleteVertexArrays _sapp_glDeleteVertexArrays
#define glDepthMask _sapp_glDepthMask
#define glDrawArraysInstanced _sapp_glDrawArraysInstanced
#define glClearStencil _sapp_glClearStencil
#define glScissor _sapp_glScissor
#define glUniform3fv _sapp_glUniform3fv
#define glGenRenderbuffers _sapp_glGenRenderbuffers
#define glBufferData _sapp_glBufferData
#define glBlendFuncSeparate _sapp_glBlendFuncSeparate
#define glTexParameteri _sapp_glTexParameteri
#define glGetIntegerv _sapp_glGetIntegerv
#define glEnable _sapp_glEnable
#define glBlitFramebuffer _sapp_glBlitFramebuffer
#define glStencilMask _sapp_glStencilMask
#define glAttachShader _sapp_glAttachShader
#define glGetError _sapp_glGetError
#define glClearColor _sapp_glClearColor
#define glBlendColor _sapp_glBlendColor
#define glTexParameterf _sapp_glTexParameterf
#define glTexParameterfv _sapp_glTexParameterfv
#define glGetShaderInfoLog _sapp_glGetShaderInfoLog
#define glDepthFunc _sapp_glDepthFunc
#define glStencilOp _sapp_glStencilOp
#define glStencilFunc _sapp_glStencilFunc
#define glEnableVertexAttribArray _sapp_glEnableVertexAttribArray
#define glBlendFunc _sapp_glBlendFunc
#define glUniform1fv _sapp_glUniform1fv
#define glReadBuffer _sapp_glReadBuffer
#define glClear _sapp_glClear
#define glTexImage2D _sapp_glTexImage2D
#define glGenVertexArrays _sapp_glGenVertexArrays
#define glFrontFace _sapp_glFrontFace
#define glCullFace _sapp_glCullFace

#endif /* SOKOL_WIN32_NO_GL_LOADER */

#endif /* SOKOL_GLCORE33 */

#if defined(SOKOL_D3D11)
#define _SAPP_SAFE_RELEASE(class, obj) if (obj) { class##_Release(obj); obj=0; }
_SOKOL_PRIVATE void _sapp_d3d11_create_device_and_swapchain(void) {
    DXGI_SWAP_CHAIN_DESC* sc_desc = &_sapp_dxgi_swap_chain_desc;
    sc_desc->BufferDesc.Width = _sapp.framebuffer_width;
    sc_desc->BufferDesc.Height = _sapp.framebuffer_height;
    sc_desc->BufferDesc.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
    sc_desc->BufferDesc.RefreshRate.Numerator = 60;
    sc_desc->BufferDesc.RefreshRate.Denominator = 1;
    sc_desc->OutputWindow = _sapp_win32_hwnd;
    sc_desc->Windowed = true;
    sc_desc->SwapEffect = DXGI_SWAP_EFFECT_DISCARD;
    sc_desc->BufferCount = 1;
    sc_desc->SampleDesc.Count = _sapp.sample_count;
    sc_desc->SampleDesc.Quality = _sapp.sample_count > 1 ? D3D11_STANDARD_MULTISAMPLE_PATTERN : 0;
    sc_desc->BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
    int create_flags = D3D11_CREATE_DEVICE_SINGLETHREADED | D3D11_CREATE_DEVICE_BGRA_SUPPORT;
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
        &_sapp_dxgi_swap_chain,         /* ppSwapChain */
        &_sapp_d3d11_device,            /* ppDevice */
        &feature_level,                 /* pFeatureLevel */
        &_sapp_d3d11_device_context);   /* ppImmediateContext */
    _SOKOL_UNUSED(hr);
    SOKOL_ASSERT(SUCCEEDED(hr) && _sapp_dxgi_swap_chain && _sapp_d3d11_device && _sapp_d3d11_device_context);
}

_SOKOL_PRIVATE void _sapp_d3d11_destroy_device_and_swapchain(void) {
    _SAPP_SAFE_RELEASE(IDXGISwapChain, _sapp_dxgi_swap_chain);
    _SAPP_SAFE_RELEASE(ID3D11DeviceContext, _sapp_d3d11_device_context);
    _SAPP_SAFE_RELEASE(ID3D11Device, _sapp_d3d11_device);
}

_SOKOL_PRIVATE void _sapp_d3d11_create_default_render_target(void) {
    HRESULT hr;
    #ifdef __cplusplus
    hr = IDXGISwapChain_GetBuffer(_sapp_dxgi_swap_chain, 0, IID_ID3D11Texture2D, (void**)&_sapp_d3d11_rt);
    #else
    hr = IDXGISwapChain_GetBuffer(_sapp_dxgi_swap_chain, 0, &IID_ID3D11Texture2D, (void**)&_sapp_d3d11_rt);
    #endif
    SOKOL_ASSERT(SUCCEEDED(hr) && _sapp_d3d11_rt);
    hr = ID3D11Device_CreateRenderTargetView(_sapp_d3d11_device, (ID3D11Resource*)_sapp_d3d11_rt, NULL, &_sapp_d3d11_rtv);
    SOKOL_ASSERT(SUCCEEDED(hr) && _sapp_d3d11_rtv);
    D3D11_TEXTURE2D_DESC ds_desc;
    memset(&ds_desc, 0, sizeof(ds_desc));
    ds_desc.Width = _sapp.framebuffer_width;
    ds_desc.Height = _sapp.framebuffer_height;
    ds_desc.MipLevels = 1;
    ds_desc.ArraySize = 1;
    ds_desc.Format = DXGI_FORMAT_D24_UNORM_S8_UINT;
    ds_desc.SampleDesc = _sapp_dxgi_swap_chain_desc.SampleDesc;
    ds_desc.Usage = D3D11_USAGE_DEFAULT;
    ds_desc.BindFlags = D3D11_BIND_DEPTH_STENCIL;
    hr = ID3D11Device_CreateTexture2D(_sapp_d3d11_device, &ds_desc, NULL, &_sapp_d3d11_ds);
    SOKOL_ASSERT(SUCCEEDED(hr) && _sapp_d3d11_ds);
    D3D11_DEPTH_STENCIL_VIEW_DESC dsv_desc;
    memset(&dsv_desc, 0, sizeof(dsv_desc));
    dsv_desc.Format = ds_desc.Format;
    dsv_desc.ViewDimension = _sapp.sample_count > 1 ? D3D11_DSV_DIMENSION_TEXTURE2DMS : D3D11_DSV_DIMENSION_TEXTURE2D;
    hr = ID3D11Device_CreateDepthStencilView(_sapp_d3d11_device, (ID3D11Resource*)_sapp_d3d11_ds, &dsv_desc, &_sapp_d3d11_dsv);
    SOKOL_ASSERT(SUCCEEDED(hr) && _sapp_d3d11_dsv);
}

_SOKOL_PRIVATE void _sapp_d3d11_destroy_default_render_target(void) {
    _SAPP_SAFE_RELEASE(ID3D11Texture2D, _sapp_d3d11_rt);
    _SAPP_SAFE_RELEASE(ID3D11RenderTargetView, _sapp_d3d11_rtv);
    _SAPP_SAFE_RELEASE(ID3D11Texture2D, _sapp_d3d11_ds);
    _SAPP_SAFE_RELEASE(ID3D11DepthStencilView, _sapp_d3d11_dsv);
}

_SOKOL_PRIVATE void _sapp_d3d11_resize_default_render_target(void) {
    if (_sapp_dxgi_swap_chain) {
        _sapp_d3d11_destroy_default_render_target();
        IDXGISwapChain_ResizeBuffers(_sapp_dxgi_swap_chain, 1, _sapp.framebuffer_width, _sapp.framebuffer_height, DXGI_FORMAT_B8G8R8A8_UNORM, 0);
        _sapp_d3d11_create_default_render_target();
    }
}
#endif

#if defined(SOKOL_GLCORE33)
_SOKOL_PRIVATE void _sapp_wgl_init(void) {
    _sapp_opengl32 = LoadLibraryA("opengl32.dll");
    if (!_sapp_opengl32) {
        _sapp_fail("Failed to load opengl32.dll\n");
    }
    SOKOL_ASSERT(_sapp_opengl32);
    _sapp_wglCreateContext = (PFN_wglCreateContext) GetProcAddress(_sapp_opengl32, "wglCreateContext");
    SOKOL_ASSERT(_sapp_wglCreateContext);
    _sapp_wglDeleteContext = (PFN_wglDeleteContext) GetProcAddress(_sapp_opengl32, "wglDeleteContext");
    SOKOL_ASSERT(_sapp_wglDeleteContext);
    _sapp_wglGetProcAddress = (PFN_wglGetProcAddress) GetProcAddress(_sapp_opengl32, "wglGetProcAddress");
    SOKOL_ASSERT(_sapp_wglGetProcAddress);
    _sapp_wglGetCurrentDC = (PFN_wglGetCurrentDC) GetProcAddress(_sapp_opengl32, "wglGetCurrentDC");
    SOKOL_ASSERT(_sapp_wglGetCurrentDC);
    _sapp_wglMakeCurrent = (PFN_wglMakeCurrent) GetProcAddress(_sapp_opengl32, "wglMakeCurrent");
    SOKOL_ASSERT(_sapp_wglMakeCurrent);

    _sapp_win32_msg_hwnd = CreateWindowExW(WS_EX_OVERLAPPEDWINDOW,
        L"SOKOLAPP",
        L"sokol-app message window",
        WS_CLIPSIBLINGS|WS_CLIPCHILDREN,
        0, 0, 1, 1,
        NULL, NULL,
        GetModuleHandleW(NULL),
        NULL);
    if (!_sapp_win32_msg_hwnd) {
        _sapp_fail("Win32: failed to create helper window!\n");
    }
    ShowWindow(_sapp_win32_msg_hwnd, SW_HIDE);
    MSG msg;
    while (PeekMessageW(&msg, _sapp_win32_msg_hwnd, 0, 0, PM_REMOVE)) {
        TranslateMessage(&msg);
        DispatchMessageW(&msg);
    }
    _sapp_win32_msg_dc = GetDC(_sapp_win32_msg_hwnd);
    if (!_sapp_win32_msg_dc) {
        _sapp_fail("Win32: failed to obtain helper window DC!\n");
    }
}

_SOKOL_PRIVATE void _sapp_wgl_shutdown(void) {
    SOKOL_ASSERT(_sapp_opengl32 && _sapp_win32_msg_hwnd);
    DestroyWindow(_sapp_win32_msg_hwnd); _sapp_win32_msg_hwnd = 0;
    FreeLibrary(_sapp_opengl32); _sapp_opengl32 = 0;
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
    if (_sapp_GetExtensionsStringEXT) {
        const char* extensions = _sapp_GetExtensionsStringEXT();
        if (extensions) {
            if (_sapp_wgl_has_ext(ext, extensions)) {
                return true;
            }
        }
    }
    if (_sapp_GetExtensionsStringARB) {
        const char* extensions = _sapp_GetExtensionsStringARB(_sapp_wglGetCurrentDC());
        if (extensions) {
            if (_sapp_wgl_has_ext(ext, extensions)) {
                return true;
            }
        }
    }
    return false;
}

_SOKOL_PRIVATE void _sapp_wgl_load_extensions(void) {
    SOKOL_ASSERT(_sapp_win32_msg_dc);
    PIXELFORMATDESCRIPTOR pfd;
    memset(&pfd, 0, sizeof(pfd));
    pfd.nSize = sizeof(pfd);
    pfd.nVersion = 1;
    pfd.dwFlags = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER;
    pfd.iPixelType = PFD_TYPE_RGBA;
    pfd.cColorBits = 24;
    if (!SetPixelFormat(_sapp_win32_msg_dc, ChoosePixelFormat(_sapp_win32_msg_dc, &pfd), &pfd)) {
        _sapp_fail("WGL: failed to set pixel format for dummy context\n");
    }
    HGLRC rc = _sapp_wglCreateContext(_sapp_win32_msg_dc);
    if (!rc) {
        _sapp_fail("WGL: Failed to create dummy context\n");
    }
    if (!_sapp_wglMakeCurrent(_sapp_win32_msg_dc, rc)) {
        _sapp_fail("WGL: Failed to make context current\n");
    }
    _sapp_GetExtensionsStringEXT = (PFNWGLGETEXTENSIONSSTRINGEXTPROC) _sapp_wglGetProcAddress("wglGetExtensionsStringEXT");
    _sapp_GetExtensionsStringARB = (PFNWGLGETEXTENSIONSSTRINGARBPROC) _sapp_wglGetProcAddress("wglGetExtensionsStringARB");
    _sapp_CreateContextAttribsARB = (PFNWGLCREATECONTEXTATTRIBSARBPROC) _sapp_wglGetProcAddress("wglCreateContextAttribsARB");
    _sapp_SwapIntervalEXT = (PFNWGLSWAPINTERVALEXTPROC) _sapp_wglGetProcAddress("wglSwapIntervalEXT");
    _sapp_GetPixelFormatAttribivARB = (PFNWGLGETPIXELFORMATATTRIBIVARBPROC) _sapp_wglGetProcAddress("wglGetPixelFormatAttribivARB");
    _sapp_arb_multisample = _sapp_wgl_ext_supported("WGL_ARB_multisample");
    _sapp_arb_create_context = _sapp_wgl_ext_supported("WGL_ARB_create_context");
    _sapp_arb_create_context_profile = _sapp_wgl_ext_supported("WGL_ARB_create_context_profile");
    _sapp_ext_swap_control = _sapp_wgl_ext_supported("WGL_EXT_swap_control");
    _sapp_arb_pixel_format = _sapp_wgl_ext_supported("WGL_ARB_pixel_format");
    _sapp_wglMakeCurrent(_sapp_win32_msg_dc, 0);
    _sapp_wglDeleteContext(rc);
}

_SOKOL_PRIVATE int _sapp_wgl_attrib(int pixel_format, int attrib) {
    SOKOL_ASSERT(_sapp_arb_pixel_format);
    int value = 0;
    if (!_sapp_GetPixelFormatAttribivARB(_sapp_win32_dc, pixel_format, 0, 1, &attrib, &value)) {
        _sapp_fail("WGL: Failed to retrieve pixel format attribute\n");
    }
    return value;
}

_SOKOL_PRIVATE int _sapp_wgl_find_pixel_format(void) {
    SOKOL_ASSERT(_sapp_win32_dc);
    SOKOL_ASSERT(_sapp_arb_pixel_format);
    const _sapp_gl_fbconfig* closest;

    int native_count = _sapp_wgl_attrib(1, WGL_NUMBER_PIXEL_FORMATS_ARB);
    _sapp_gl_fbconfig* usable_configs = (_sapp_gl_fbconfig*) SOKOL_CALLOC(native_count, sizeof(_sapp_gl_fbconfig));
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
        if (_sapp_arb_multisample) {
            u->samples = _sapp_wgl_attrib(n, WGL_SAMPLES_ARB);
        }
        u->handle = n;
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
    if (!DescribePixelFormat(_sapp_win32_dc, pixel_format, sizeof(pfd), &pfd)) {
        _sapp_fail("WGL: Failed to retrieve PFD for selected pixel format!\n");
    }
    if (!SetPixelFormat(_sapp_win32_dc, pixel_format, &pfd)) {
        _sapp_fail("WGL: Failed to set selected pixel format!\n");
    }
    if (!_sapp_arb_create_context) {
        _sapp_fail("WGL: ARB_create_context required!\n");
    }
    if (!_sapp_arb_create_context_profile) {
        _sapp_fail("WGL: ARB_create_context_profile required!\n");
    }
    const int attrs[] = {
        WGL_CONTEXT_MAJOR_VERSION_ARB, 3,
        WGL_CONTEXT_MINOR_VERSION_ARB, 3,
        WGL_CONTEXT_FLAGS_ARB, WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB,
        WGL_CONTEXT_PROFILE_MASK_ARB, WGL_CONTEXT_CORE_PROFILE_BIT_ARB,
        0, 0
    };
    _sapp_gl_ctx = _sapp_CreateContextAttribsARB(_sapp_win32_dc, 0, attrs);
    if (!_sapp_gl_ctx) {
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
    _sapp_wglMakeCurrent(_sapp_win32_dc, _sapp_gl_ctx);
    if (_sapp_ext_swap_control) {
        /* FIXME: DwmIsCompositionEnabled() (see GLFW) */
        _sapp_SwapIntervalEXT(_sapp.swap_interval);
    }
}

_SOKOL_PRIVATE void _sapp_wgl_destroy_context(void) {
    SOKOL_ASSERT(_sapp_gl_ctx);
    _sapp_wglDeleteContext(_sapp_gl_ctx);
    _sapp_gl_ctx = 0;
}

_SOKOL_PRIVATE void _sapp_wgl_swap_buffers(void) {
    SOKOL_ASSERT(_sapp_win32_dc);
    /* FIXME: DwmIsCompositionEnabled? (see GLFW) */
    SwapBuffers(_sapp_win32_dc);
}
#endif

_SOKOL_PRIVATE bool _sapp_win32_utf8_to_wide(const char* src, wchar_t* dst, int dst_num_bytes) {
    SOKOL_ASSERT(src && dst && (dst_num_bytes > 1));
    memset(dst, 0, dst_num_bytes);
    const int dst_chars = dst_num_bytes / sizeof(wchar_t);
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

_SOKOL_PRIVATE bool _sapp_win32_wide_to_utf8(const wchar_t* src, char* dst, int dst_num_bytes) {
    SOKOL_ASSERT(src && dst && (dst_num_bytes > 1));
    memset(dst, 0, dst_num_bytes);
    return 0 != WideCharToMultiByte(CP_UTF8, 0, src, -1, dst, dst_num_bytes, NULL, NULL);
}

_SOKOL_PRIVATE void _sapp_win32_show_mouse(bool shown) {
    ShowCursor((BOOL)shown);
}

_SOKOL_PRIVATE bool _sapp_win32_mouse_shown(void) {
    CURSORINFO cursor_info;
    memset(&cursor_info, 0, sizeof(CURSORINFO));
    cursor_info.cbSize = sizeof(CURSORINFO);
    GetCursorInfo(&cursor_info);
    return (cursor_info.flags & CURSOR_SHOWING) != 0;
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

/* updates current window and framebuffer size from the window's client rect, returns true if size has changed */
_SOKOL_PRIVATE bool _sapp_win32_update_dimensions(void) {
    RECT rect;
    if (GetClientRect(_sapp_win32_hwnd, &rect)) {
        _sapp.window_width = (int)((float)(rect.right - rect.left) / _sapp_win32_window_scale);
        _sapp.window_height = (int)((float)(rect.bottom - rect.top) / _sapp_win32_window_scale);
        const int fb_width = (int)((float)_sapp.window_width * _sapp_win32_content_scale);
        const int fb_height = (int)((float)_sapp.window_height * _sapp_win32_content_scale);
        if ((fb_width != _sapp.framebuffer_width) || (fb_height != _sapp.framebuffer_height)) {
            _sapp.framebuffer_width = (int)((float)_sapp.window_width * _sapp_win32_content_scale);
            _sapp.framebuffer_height = (int)((float)_sapp.window_height * _sapp_win32_content_scale);
            /* prevent a framebuffer size of 0 when window is minimized */
            if (_sapp.framebuffer_width == 0) {
                _sapp.framebuffer_width = 1;
            }
            if (_sapp.framebuffer_height == 0) {
                _sapp.framebuffer_height = 1;
            }
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
    if (GetKeyState(VK_SHIFT) & (1<<31)) {
        mods |= SAPP_MODIFIER_SHIFT;
    }
    if (GetKeyState(VK_CONTROL) & (1<<31)) {
        mods |= SAPP_MODIFIER_CTRL;
    }
    if (GetKeyState(VK_MENU) & (1<<31)) {
        mods |= SAPP_MODIFIER_ALT;
    }
    if ((GetKeyState(VK_LWIN) | GetKeyState(VK_RWIN)) & (1<<31)) {
        mods |= SAPP_MODIFIER_SUPER;
    }
    return mods;
}

_SOKOL_PRIVATE void _sapp_win32_mouse_event(sapp_event_type type, sapp_mousebutton btn) {
    if (_sapp_events_enabled()) {
        _sapp_init_event(type);
        _sapp.event.modifiers = _sapp_win32_mods();
        _sapp.event.mouse_button = btn;
        _sapp.event.mouse_x = _sapp.mouse_x;
        _sapp.event.mouse_y = _sapp.mouse_y;
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
        if (_sapp.clipboard_enabled &&
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

_SOKOL_PRIVATE void _sapp_win32_app_event(sapp_event_type type) {
    if (_sapp_events_enabled()) {
        _sapp_init_event(type);
        _sapp_call_event(&_sapp.event);
    }
}

_SOKOL_PRIVATE LRESULT CALLBACK _sapp_win32_wndproc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam) {
    /* FIXME: refresh rendering during resize with a WM_TIMER event */
    if (!_sapp_win32_in_create_window) {
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
                        if (_sapp.desc.fullscreen) {
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
                    if (iconified != _sapp_win32_iconified) {
                        _sapp_win32_iconified = iconified;
                        if (iconified) {
                            _sapp_win32_app_event(SAPP_EVENTTYPE_ICONIFIED);
                        }
                        else {
                            _sapp_win32_app_event(SAPP_EVENTTYPE_RESTORED);
                        }
                    }
                }
                break;
            case WM_SETCURSOR:
                if (_sapp.desc.user_cursor) {
                    if (LOWORD(lParam) == HTCLIENT) {
                        _sapp_win32_app_event(SAPP_EVENTTYPE_UPDATE_CURSOR);
                        return 1;
                    }
                }
                break;
            case WM_LBUTTONDOWN:
                _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_DOWN, SAPP_MOUSEBUTTON_LEFT);
                break;
            case WM_RBUTTONDOWN:
                _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_DOWN, SAPP_MOUSEBUTTON_RIGHT);
                break;
            case WM_MBUTTONDOWN:
                _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_DOWN, SAPP_MOUSEBUTTON_MIDDLE);
                break;
            case WM_LBUTTONUP:
                _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_UP, SAPP_MOUSEBUTTON_LEFT);
                break;
            case WM_RBUTTONUP:
                _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_UP, SAPP_MOUSEBUTTON_RIGHT);
                break;
            case WM_MBUTTONUP:
                _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_UP, SAPP_MOUSEBUTTON_MIDDLE);
                break;
            case WM_MOUSEMOVE:
                _sapp.mouse_x = (float)GET_X_LPARAM(lParam) * _sapp_win32_mouse_scale;
                _sapp.mouse_y = (float)GET_Y_LPARAM(lParam) * _sapp_win32_mouse_scale;
                if (!_sapp.win32_mouse_tracked) {
                    _sapp.win32_mouse_tracked = true;
                    TRACKMOUSEEVENT tme;
                    memset(&tme, 0, sizeof(tme));
                    tme.cbSize = sizeof(tme);
                    tme.dwFlags = TME_LEAVE;
                    tme.hwndTrack = _sapp_win32_hwnd;
                    TrackMouseEvent(&tme);
                    _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_ENTER, SAPP_MOUSEBUTTON_INVALID);
                }
                _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_MOVE,  SAPP_MOUSEBUTTON_INVALID);
                break;
            case WM_MOUSELEAVE:
                _sapp.win32_mouse_tracked = false;
                _sapp_win32_mouse_event(SAPP_EVENTTYPE_MOUSE_LEAVE, SAPP_MOUSEBUTTON_INVALID);
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
    if (_sapp.desc.fullscreen) {
        win_style = WS_POPUP | WS_SYSMENU | WS_VISIBLE;
        rect.right = GetSystemMetrics(SM_CXSCREEN);
        rect.bottom = GetSystemMetrics(SM_CYSCREEN);
    }
    else {
        win_style = WS_CLIPSIBLINGS | WS_CLIPCHILDREN | WS_CAPTION | WS_SYSMENU | WS_MINIMIZEBOX | WS_MAXIMIZEBOX | WS_SIZEBOX;
        rect.right = (int) ((float)_sapp.window_width * _sapp_win32_window_scale);
        rect.bottom = (int) ((float)_sapp.window_height * _sapp_win32_window_scale);
    }
    AdjustWindowRectEx(&rect, win_style, FALSE, win_ex_style);
    const int win_width = rect.right - rect.left;
    const int win_height = rect.bottom - rect.top;
    _sapp_win32_in_create_window = true;
    _sapp_win32_hwnd = CreateWindowExW(
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
    ShowWindow(_sapp_win32_hwnd, SW_SHOW);
    _sapp_win32_in_create_window = false;
    _sapp_win32_dc = GetDC(_sapp_win32_hwnd);
    SOKOL_ASSERT(_sapp_win32_dc);
    _sapp_win32_update_dimensions();
}

_SOKOL_PRIVATE void _sapp_win32_destroy_window(void) {
    DestroyWindow(_sapp_win32_hwnd); _sapp_win32_hwnd = 0;
    UnregisterClassW(L"SOKOLAPP", GetModuleHandleW(NULL));
}

_SOKOL_PRIVATE void _sapp_win32_init_dpi(void) {
    SOKOL_ASSERT(0 == _sapp_win32_setprocessdpiaware);
    SOKOL_ASSERT(0 == _sapp_win32_setprocessdpiawareness);
    SOKOL_ASSERT(0 == _sapp_win32_getdpiformonitor);
    HINSTANCE user32 = LoadLibraryA("user32.dll");
    if (user32) {
        _sapp_win32_setprocessdpiaware = (SETPROCESSDPIAWARE_T) GetProcAddress(user32, "SetProcessDPIAware");
    }
    HINSTANCE shcore = LoadLibraryA("shcore.dll");
    if (shcore) {
        _sapp_win32_setprocessdpiawareness = (SETPROCESSDPIAWARENESS_T) GetProcAddress(shcore, "SetProcessDpiAwareness");
        _sapp_win32_getdpiformonitor = (GETDPIFORMONITOR_T) GetProcAddress(shcore, "GetDpiForMonitor");
    }
    if (_sapp_win32_setprocessdpiawareness) {
        /* if the app didn't request HighDPI rendering, let Windows do the upscaling */
        PROCESS_DPI_AWARENESS process_dpi_awareness = PROCESS_SYSTEM_DPI_AWARE;
        _sapp_win32_dpi_aware = true;
        if (!_sapp.desc.high_dpi) {
            process_dpi_awareness = PROCESS_DPI_UNAWARE;
            _sapp_win32_dpi_aware = false;
        }
        _sapp_win32_setprocessdpiawareness(process_dpi_awareness);
    }
    else if (_sapp_win32_setprocessdpiaware) {
        _sapp_win32_setprocessdpiaware();
        _sapp_win32_dpi_aware = true;
    }
    /* get dpi scale factor for main monitor */
    if (_sapp_win32_getdpiformonitor && _sapp_win32_dpi_aware) {
        POINT pt = { 1, 1 };
        HMONITOR hm = MonitorFromPoint(pt, MONITOR_DEFAULTTONEAREST);
        UINT dpix, dpiy;
        HRESULT hr = _sapp_win32_getdpiformonitor(hm, MDT_EFFECTIVE_DPI, &dpix, &dpiy);
        _SOKOL_UNUSED(hr);
        SOKOL_ASSERT(SUCCEEDED(hr));
        /* clamp window scale to an integer factor */
        _sapp_win32_window_scale = (float)dpix / 96.0f;
    }
    else {
        _sapp_win32_window_scale = 1.0f;
    }
    if (_sapp.desc.high_dpi) {
        _sapp_win32_content_scale = _sapp_win32_window_scale;
        _sapp_win32_mouse_scale = 1.0f;
    }
    else {
        _sapp_win32_content_scale = 1.0f;
        _sapp_win32_mouse_scale = 1.0f / _sapp_win32_window_scale;
    }
    _sapp.dpi_scale = _sapp_win32_content_scale;
    if (user32) {
        FreeLibrary(user32);
    }
    if (shcore) {
        FreeLibrary(shcore);
    }
}

_SOKOL_PRIVATE bool _sapp_win32_set_clipboard_string(const char* str) {
    SOKOL_ASSERT(str);
    SOKOL_ASSERT(_sapp_win32_hwnd);
    SOKOL_ASSERT(_sapp.clipboard_enabled && (_sapp.clipboard_size > 0));

    wchar_t* wchar_buf = 0;
    const int wchar_buf_size = _sapp.clipboard_size * sizeof(wchar_t);
    HANDLE object = GlobalAlloc(GMEM_MOVEABLE, wchar_buf_size);
    if (!object) {
        goto error;
    }
    wchar_buf = (wchar_t*) GlobalLock(object);
    if (!wchar_buf) {
        goto error;
    }
    if (!_sapp_win32_utf8_to_wide(str, wchar_buf, wchar_buf_size)) {
        goto error;
    }
    GlobalUnlock(wchar_buf);
    wchar_buf = 0;
    if (!OpenClipboard(_sapp_win32_hwnd)) {
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
    SOKOL_ASSERT(_sapp.clipboard_enabled && _sapp.clipboard);
    SOKOL_ASSERT(_sapp_win32_hwnd);
    if (!OpenClipboard(_sapp_win32_hwnd)) {
        /* silently ignore any errors and just return the current
           content of the local clipboard buffer
        */
        return _sapp.clipboard;
    }
    HANDLE object = GetClipboardData(CF_UNICODETEXT);
    if (!object) {
        CloseClipboard();
        return _sapp.clipboard;
    }
    const wchar_t* wchar_buf = (const wchar_t*) GlobalLock(object);
    if (!wchar_buf) {
        CloseClipboard();
        return _sapp.clipboard;
    }
    _sapp_win32_wide_to_utf8(wchar_buf, _sapp.clipboard, _sapp.clipboard_size);
    GlobalUnlock(object);
    CloseClipboard();
    return _sapp.clipboard;
}

_SOKOL_PRIVATE void _sapp_run(const sapp_desc* desc) {
    _sapp_init_state(desc);
    _sapp_win32_init_keytable();
    _sapp_win32_utf8_to_wide(_sapp.window_title, _sapp.window_title_wide, sizeof(_sapp.window_title_wide));
    _sapp_win32_init_dpi();
    _sapp_win32_create_window();
    #if defined(SOKOL_D3D11)
        _sapp_d3d11_create_device_and_swapchain();
        _sapp_d3d11_create_default_render_target();
    #endif
    #if defined(SOKOL_GLCORE33)
        _sapp_wgl_init();
        _sapp_wgl_load_extensions();
        _sapp_wgl_create_context();
        #if !defined(SOKOL_WIN32_NO_GL_LOADER)
            _sapp_win32_gl_loadfuncs();
        #endif
    #endif
    _sapp.valid = true;

    bool done = false;
    while (!(done || _sapp.quit_ordered)) {
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
            IDXGISwapChain_Present(_sapp_dxgi_swap_chain, _sapp.swap_interval, 0);
            if (IsIconic(_sapp_win32_hwnd)) {
                Sleep(16 * _sapp.swap_interval);
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
            _sapp_win32_app_event(SAPP_EVENTTYPE_RESIZED);
        }
        if (_sapp.quit_requested) {
            PostMessage(_sapp_win32_hwnd, WM_CLOSE, 0, 0);
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
    _sapp_discard_state();
}

static char** _sapp_win32_command_line_to_utf8_argv(LPWSTR w_command_line, int* o_argc) {
    int argc = 0;
    char** argv = 0;
    char* args;

    LPWSTR* w_argv = CommandLineToArgvW(w_command_line, &argc);
    if (w_argv == NULL) {
        _sapp_fail("Win32: failed to parse command line");
    } else {
        size_t size = wcslen(w_command_line) * 4;
        argv = (char**) SOKOL_CALLOC(1, (argc + 1) * sizeof(char*) + size);
        args = (char*)&argv[argc + 1];
        int n;
        for (int i = 0; i < argc; ++i) {
            n = WideCharToMultiByte(CP_UTF8, 0, w_argv[i], -1, args, (int)size, NULL, NULL);
            if (n == 0) {
                _sapp_fail("Win32: failed to convert all arguments to utf8");
                break;
            }
            argv[i] = args;
            size -= n;
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
    _sapp_run(&desc);
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
    _sapp_run(&desc);
    SOKOL_FREE(argv_utf8);
    return 0;
}
#endif /* SOKOL_WIN32_FORCE_MAIN */
#endif /* SOKOL_NO_ENTRY */
#undef _SAPP_SAFE_RELEASE
#endif /* WINDOWS */

/*== Android ================================================================*/
#if defined(__ANDROID__)
#include <pthread.h>
#include <unistd.h>
#include <android/native_activity.h>
#include <android/looper.h>

#include <EGL/egl.h>
#if defined(SOKOL_GLES3)
    #include <GLES3/gl3.h>
#else
    #ifndef GL_EXT_PROTOTYPES
        #define GL_GLEXT_PROTOTYPES
    #endif
    #include <GLES2/gl2.h>
    #include <GLES2/gl2ext.h>
#endif

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
} _sapp_android_state_t;

static _sapp_android_state_t _sapp_android_state;

/* android loop thread */
_SOKOL_PRIVATE bool _sapp_android_init_egl(void) {
    _sapp_android_state_t* state = &_sapp_android_state;
    SOKOL_ASSERT(state->display == EGL_NO_DISPLAY);
    SOKOL_ASSERT(state->context == EGL_NO_CONTEXT);

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

    state->config = config;
    state->display = display;
    state->context = context;
    return true;
}

_SOKOL_PRIVATE void _sapp_android_cleanup_egl(void) {
    _sapp_android_state_t* state = &_sapp_android_state;
    if (state->display != EGL_NO_DISPLAY) {
        eglMakeCurrent(state->display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
        if (state->surface != EGL_NO_SURFACE) {
            SOKOL_LOG("Destroying egl surface");
            eglDestroySurface(state->display, state->surface);
            state->surface = EGL_NO_SURFACE;
        }
        if (state->context != EGL_NO_CONTEXT) {
            SOKOL_LOG("Destroying egl context");
            eglDestroyContext(state->display, state->context);
            state->context = EGL_NO_CONTEXT;
        }
        SOKOL_LOG("Terminating egl display");
        eglTerminate(state->display);
        state->display = EGL_NO_DISPLAY;
    }
}

_SOKOL_PRIVATE bool _sapp_android_init_egl_surface(ANativeWindow* window) {
    _sapp_android_state_t* state = &_sapp_android_state;
    SOKOL_ASSERT(state->display != EGL_NO_DISPLAY);
    SOKOL_ASSERT(state->context != EGL_NO_CONTEXT);
    SOKOL_ASSERT(state->surface == EGL_NO_SURFACE);
    SOKOL_ASSERT(window);

    /* TODO: set window flags */
    /* ANativeActivity_setWindowFlags(activity, AWINDOW_FLAG_KEEP_SCREEN_ON, 0); */

    /* create egl surface and make it current */
    EGLSurface surface = eglCreateWindowSurface(state->display, state->config, window, NULL);
    if (surface == EGL_NO_SURFACE) {
        return false;
    }
    if (eglMakeCurrent(state->display, surface, surface, state->context) == EGL_FALSE) {
        return false;
    }
    state->surface = surface;
    return true;
}

_SOKOL_PRIVATE void _sapp_android_cleanup_egl_surface(void) {
    _sapp_android_state_t* state = &_sapp_android_state;
    if (state->display == EGL_NO_DISPLAY) {
        return;
    }
    eglMakeCurrent(state->display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
    if (state->surface != EGL_NO_SURFACE) {
        eglDestroySurface(state->display, state->surface);
        state->surface = EGL_NO_SURFACE;
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
    _sapp_android_state_t* state = &_sapp_android_state;
    SOKOL_ASSERT(state->display != EGL_NO_DISPLAY);
    SOKOL_ASSERT(state->context != EGL_NO_CONTEXT);
    SOKOL_ASSERT(state->surface != EGL_NO_SURFACE);
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
            EGLBoolean egl_result = eglGetConfigAttrib(state->display, state->config, EGL_NATIVE_VISUAL_ID, &format);
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
    EGLBoolean egl_result_w = eglQuerySurface(state->display, state->surface, EGL_WIDTH, &fb_w);
    EGLBoolean egl_result_h = eglQuerySurface(state->display, state->surface, EGL_HEIGHT, &fb_h);
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
    _sapp_android_state_t* state = &_sapp_android_state;
    SOKOL_LOG("Cleaning up");
    if (state->surface != EGL_NO_SURFACE) {
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
    ANativeActivity_finish(_sapp_android_state.activity);
}

_SOKOL_PRIVATE void _sapp_android_frame(void) {
    _sapp_android_state_t* state = &_sapp_android_state;
    SOKOL_ASSERT(state->display != EGL_NO_DISPLAY);
    SOKOL_ASSERT(state->context != EGL_NO_CONTEXT);
    SOKOL_ASSERT(state->surface != EGL_NO_SURFACE);
    _sapp_android_update_dimensions(state->current.window, false);
    _sapp_frame();
    eglSwapBuffers(state->display, _sapp_android_state.surface);
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
    _sapp.event.num_touches = AMotionEvent_getPointerCount(e);
    if (_sapp.event.num_touches > SAPP_MAX_TOUCHPOINTS) {
        _sapp.event.num_touches = SAPP_MAX_TOUCHPOINTS;
    }
    for (int32_t i = 0; i < _sapp.event.num_touches; i++) {
        sapp_touchpoint* dst = &_sapp.event.touches[i];
        dst->identifier = AMotionEvent_getPointerId(e, i);
        dst->pos_x = (AMotionEvent_getRawX(e, i) / _sapp.window_width) * _sapp.framebuffer_width;
        dst->pos_y = (AMotionEvent_getRawY(e, i) / _sapp.window_height) * _sapp.framebuffer_height;

        if (action == AMOTION_EVENT_ACTION_POINTER_DOWN ||
            action == AMOTION_EVENT_ACTION_POINTER_UP) {
            dst->changed = i == idx;
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
    _sapp_android_state_t* state = &_sapp_android_state;;
    SOKOL_ASSERT(state->current.input);
    AInputEvent* event = NULL;
    while (AInputQueue_getEvent(state->current.input, &event) >= 0) {
        if (AInputQueue_preDispatchEvent(state->current.input, event) != 0) {
            continue;
        }
        int32_t handled = 0;
        if (_sapp_android_touch_event(event) || _sapp_android_key_event(event)) {
            handled = 1;
        }
        AInputQueue_finishEvent(state->current.input, event, handled);
    }
    return 1;
}

_SOKOL_PRIVATE int _sapp_android_main_cb(int fd, int events, void* data) {
    if ((events & ALOOPER_EVENT_INPUT) == 0) {
        SOKOL_LOG("_sapp_android_main_cb() encountered unsupported event");
        return 1;
    }
    _sapp_android_state_t* state = &_sapp_android_state;

    _sapp_android_msg_t msg;
    if (read(fd, &msg, sizeof(msg)) != sizeof(msg)) {
        SOKOL_LOG("Could not write to read_from_main_fd");
        return 1;
    }

    pthread_mutex_lock(&state->pt.mutex);
    switch (msg) {
        case _SOKOL_ANDROID_MSG_CREATE:
            {
                SOKOL_LOG("MSG_CREATE");
                SOKOL_ASSERT(!_sapp.valid);
                bool result = _sapp_android_init_egl();
                SOKOL_ASSERT(result);
                _sapp.valid = true;
                state->has_created = true;
            }
            break;
        case _SOKOL_ANDROID_MSG_RESUME:
            SOKOL_LOG("MSG_RESUME");
            state->has_resumed = true;
            _sapp_android_app_event(SAPP_EVENTTYPE_RESUMED);
            break;
        case _SOKOL_ANDROID_MSG_PAUSE:
            SOKOL_LOG("MSG_PAUSE");
            state->has_resumed = false;
            _sapp_android_app_event(SAPP_EVENTTYPE_SUSPENDED);
            break;
        case _SOKOL_ANDROID_MSG_FOCUS:
            SOKOL_LOG("MSG_FOCUS");
            state->has_focus = true;
            break;
        case _SOKOL_ANDROID_MSG_NO_FOCUS:
            SOKOL_LOG("MSG_NO_FOCUS");
            state->has_focus = false;
            break;
        case _SOKOL_ANDROID_MSG_SET_NATIVE_WINDOW:
            SOKOL_LOG("MSG_SET_NATIVE_WINDOW");
            if (state->current.window != state->pending.window) {
                if (state->current.window != NULL) {
                    _sapp_android_cleanup_egl_surface();
                }
                if (state->pending.window != NULL) {
                    SOKOL_LOG("Creating egl surface ...");
                    if (_sapp_android_init_egl_surface(state->pending.window)) {
                        SOKOL_LOG("... ok!");
                        _sapp_android_update_dimensions(state->pending.window, true);
                    } else {
                        SOKOL_LOG("... failed!");
                        _sapp_android_shutdown();
                    }
                }
            }
            state->current.window = state->pending.window;
            break;
        case _SOKOL_ANDROID_MSG_SET_INPUT_QUEUE:
            SOKOL_LOG("MSG_SET_INPUT_QUEUE");
            if (state->current.input != state->pending.input) {
                if (state->current.input != NULL) {
                    AInputQueue_detachLooper(state->current.input);
                }
                if (state->pending.input != NULL) {
                    AInputQueue_attachLooper(
                        state->pending.input,
                        state->looper,
                        ALOOPER_POLL_CALLBACK,
                        _sapp_android_input_cb,
                        NULL); /* data */
                }
            }
            state->current.input = state->pending.input;
            break;
        case _SOKOL_ANDROID_MSG_DESTROY:
            SOKOL_LOG("MSG_DESTROY");
            _sapp_android_cleanup();
            _sapp.valid = false;
            state->is_thread_stopping = true;
            break;
        default:
            SOKOL_LOG("Unknown msg type received");
            break;
    }
    pthread_cond_broadcast(&state->pt.cond); /* signal "received" */
    pthread_mutex_unlock(&state->pt.mutex);
    return 1;
}

_SOKOL_PRIVATE bool _sapp_android_should_update(void) {
    bool is_in_front = _sapp_android_state.has_resumed && _sapp_android_state.has_focus;
    bool has_surface = _sapp_android_state.surface != EGL_NO_SURFACE;
    return is_in_front && has_surface;
}

_SOKOL_PRIVATE void _sapp_android_show_keyboard(bool shown) {
    SOKOL_ASSERT(_sapp.valid);
    /* This seems to be broken in the NDK, but there is (a very cumbersome) workaround... */
    if (shown) {
        SOKOL_LOG("Showing keyboard");
        ANativeActivity_showSoftInput(_sapp_android_state.activity, ANATIVEACTIVITY_SHOW_SOFT_INPUT_FORCED);
    } else {
        SOKOL_LOG("Hiding keyboard");
        ANativeActivity_hideSoftInput(_sapp_android_state.activity, ANATIVEACTIVITY_HIDE_SOFT_INPUT_NOT_ALWAYS);
    }
}

_SOKOL_PRIVATE void* _sapp_android_loop(void* obj) {
    SOKOL_LOG("Loop thread started");
    _sapp_android_state_t* state = (_sapp_android_state_t*)obj;

    state->looper = ALooper_prepare(0 /* or ALOOPER_PREPARE_ALLOW_NON_CALLBACKS*/);
    ALooper_addFd(state->looper,
        state->pt.read_from_main_fd,
        ALOOPER_POLL_CALLBACK,
        ALOOPER_EVENT_INPUT,
        _sapp_android_main_cb,
        NULL); /* data */

    /* signal start to main thread */
    pthread_mutex_lock(&state->pt.mutex);
    state->is_thread_started = true;
    pthread_cond_broadcast(&state->pt.cond);
    pthread_mutex_unlock(&state->pt.mutex);

    /* main loop */
    while (!state->is_thread_stopping) {
        /* sokol frame */
        if (_sapp_android_should_update()) {
            _sapp_android_frame();
        }

        /* process all events (or stop early if app is requested to quit) */
        bool process_events = true;
        while (process_events && !state->is_thread_stopping) {
            bool block_until_event = !state->is_thread_stopping && !_sapp_android_should_update();
            process_events = ALooper_pollOnce(block_until_event ? -1 : 0, NULL, NULL, NULL) == ALOOPER_POLL_CALLBACK;
        }
    }

    /* cleanup thread */
    if (state->current.input != NULL) {
        AInputQueue_detachLooper(state->current.input);
    }

    /* the following causes heap corruption on exit, why??
    ALooper_removeFd(state->looper, state->pt.read_from_main_fd);
    ALooper_release(state->looper);*/

    /* signal "destroyed" */
    pthread_mutex_lock(&state->pt.mutex);
    state->is_thread_stopped = true;
    pthread_cond_broadcast(&state->pt.cond);
    pthread_mutex_unlock(&state->pt.mutex);
    SOKOL_LOG("Loop thread done");
    return NULL;
}

/* android main/ui thread */
_SOKOL_PRIVATE void _sapp_android_msg(_sapp_android_state_t* state, _sapp_android_msg_t msg) {
    if (write(state->pt.write_from_main_fd, &msg, sizeof(msg)) != sizeof(msg)) {
        SOKOL_LOG("Could not write to write_from_main_fd");
    }
}

_SOKOL_PRIVATE void _sapp_android_on_start(ANativeActivity* activity) {
    SOKOL_LOG("NativeActivity onStart()");
}

_SOKOL_PRIVATE void _sapp_android_on_resume(ANativeActivity* activity) {
    SOKOL_LOG("NativeActivity onResume()");
    _sapp_android_msg(&_sapp_android_state, _SOKOL_ANDROID_MSG_RESUME);
}

_SOKOL_PRIVATE void* _sapp_android_on_save_instance_state(ANativeActivity* activity, size_t* out_size) {
    SOKOL_LOG("NativeActivity onSaveInstanceState()");
    *out_size = 0;
    return NULL;
}

_SOKOL_PRIVATE void _sapp_android_on_window_focus_changed(ANativeActivity* activity, int has_focus) {
    SOKOL_LOG("NativeActivity onWindowFocusChanged()");
    if (has_focus) {
        _sapp_android_msg(&_sapp_android_state, _SOKOL_ANDROID_MSG_FOCUS);
    } else {
        _sapp_android_msg(&_sapp_android_state, _SOKOL_ANDROID_MSG_NO_FOCUS);
    }
}

_SOKOL_PRIVATE void _sapp_android_on_pause(ANativeActivity* activity) {
    SOKOL_LOG("NativeActivity onPause()");
    _sapp_android_msg(&_sapp_android_state, _SOKOL_ANDROID_MSG_PAUSE);
}

_SOKOL_PRIVATE void _sapp_android_on_stop(ANativeActivity* activity) {
    SOKOL_LOG("NativeActivity onStop()");
}

_SOKOL_PRIVATE void _sapp_android_msg_set_native_window(_sapp_android_state_t* state, ANativeWindow* window) {
    pthread_mutex_lock(&state->pt.mutex);
    state->pending.window = window;
    _sapp_android_msg(state, _SOKOL_ANDROID_MSG_SET_NATIVE_WINDOW);
    while (state->current.window != window) {
        pthread_cond_wait(&state->pt.cond, &state->pt.mutex);
    }
    pthread_mutex_unlock(&state->pt.mutex);
}

_SOKOL_PRIVATE void _sapp_android_on_native_window_created(ANativeActivity* activity, ANativeWindow* window) {
    SOKOL_LOG("NativeActivity onNativeWindowCreated()");
    _sapp_android_msg_set_native_window(&_sapp_android_state, window);
}

_SOKOL_PRIVATE void _sapp_android_on_native_window_destroyed(ANativeActivity* activity, ANativeWindow* window) {
    SOKOL_LOG("NativeActivity onNativeWindowDestroyed()");
    _sapp_android_msg_set_native_window(&_sapp_android_state, NULL);
}

_SOKOL_PRIVATE void _sapp_android_msg_set_input_queue(_sapp_android_state_t* state, AInputQueue* input) {
    pthread_mutex_lock(&state->pt.mutex);
    state->pending.input = input;
    _sapp_android_msg(state, _SOKOL_ANDROID_MSG_SET_INPUT_QUEUE);
    while (state->current.input != input) {
        pthread_cond_wait(&state->pt.cond, &state->pt.mutex);
    }
    pthread_mutex_unlock(&state->pt.mutex);
}

_SOKOL_PRIVATE void _sapp_android_on_input_queue_created(ANativeActivity* activity, AInputQueue* queue) {
    SOKOL_LOG("NativeActivity onInputQueueCreated()");
    _sapp_android_msg_set_input_queue(&_sapp_android_state, queue);
}

_SOKOL_PRIVATE void _sapp_android_on_input_queue_destroyed(ANativeActivity* activity, AInputQueue* queue) {
    SOKOL_LOG("NativeActivity onInputQueueDestroyed()");
    _sapp_android_msg_set_input_queue(&_sapp_android_state, NULL);
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
    _sapp_android_state_t* state = &_sapp_android_state;

    /* send destroy msg */
    pthread_mutex_lock(&state->pt.mutex);
    _sapp_android_msg(state, _SOKOL_ANDROID_MSG_DESTROY);
    while (!_sapp_android_state.is_thread_stopped) {
        pthread_cond_wait(&state->pt.cond, &state->pt.mutex);
    }
    pthread_mutex_unlock(&state->pt.mutex);

    /* clean up main thread */
    pthread_cond_destroy(&state->pt.cond);
    pthread_mutex_destroy(&state->pt.mutex);

    close(state->pt.read_from_main_fd);
    close(state->pt.write_from_main_fd);

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
    _sapp_android_state = (_sapp_android_state_t){0};
    _sapp_android_state_t* state = &_sapp_android_state;

    state->activity = activity;

    int pipe_fd[2];
    if (pipe(pipe_fd) != 0) {
        SOKOL_LOG("Could not create thread pipe");
        return;
    }
    state->pt.read_from_main_fd = pipe_fd[0];
    state->pt.write_from_main_fd = pipe_fd[1];

    pthread_mutex_init(&state->pt.mutex, NULL);
    pthread_cond_init(&state->pt.cond, NULL);

    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    pthread_create(&state->pt.thread, &attr, _sapp_android_loop, state);
    pthread_attr_destroy(&attr);

    /* wait until main loop has started */
    pthread_mutex_lock(&state->pt.mutex);
    while (!state->is_thread_started) {
        pthread_cond_wait(&state->pt.cond, &state->pt.mutex);
    }
    pthread_mutex_unlock(&state->pt.mutex);

    /* send create msg */
    pthread_mutex_lock(&state->pt.mutex);
    _sapp_android_msg(state, _SOKOL_ANDROID_MSG_CREATE);
    while (!state->has_created) {
        pthread_cond_wait(&state->pt.cond, &state->pt.mutex);
    }
    pthread_mutex_unlock(&state->pt.mutex);

    /* register for callbacks */
    activity->instance = state;
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

#endif /* Android */

/*== LINUX ==================================================================*/
#if (defined(__linux__) || defined(__unix__)) && !defined(__EMSCRIPTEN__) && !defined(__ANDROID__)
#define GL_GLEXT_PROTOTYPES
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/XKBlib.h>
#include <X11/Xresource.h>
#include <X11/extensions/Xrandr.h>
#include <X11/Xmd.h> /* CARD32 */
#include <GL/gl.h>
#include <dlfcn.h> /* dlopen, dlsym, dlclose */
#include <limits.h> /* LONG_MAX */

#define GLX_VENDOR 1
#define GLX_RGBA_BIT 0x00000001
#define GLX_WINDOW_BIT 0x00000001
#define GLX_DRAWABLE_TYPE 0x8010
#define GLX_RENDER_TYPE	0x8011
#define GLX_RGBA_TYPE 0x8014
#define GLX_DOUBLEBUFFER 5
#define GLX_STEREO 6
#define GLX_AUX_BUFFERS	7
#define GLX_RED_SIZE 8
#define GLX_GREEN_SIZE 9
#define GLX_BLUE_SIZE 10
#define GLX_ALPHA_SIZE 11
#define GLX_DEPTH_SIZE 12
#define GLX_STENCIL_SIZE 13
#define GLX_ACCUM_RED_SIZE 14
#define GLX_ACCUM_GREEN_SIZE 15
#define GLX_ACCUM_BLUE_SIZE	16
#define GLX_ACCUM_ALPHA_SIZE 17
#define GLX_SAMPLES 0x186a1
#define GLX_VISUAL_ID 0x800b

#define GLX_FRAMEBUFFER_SRGB_CAPABLE_ARB 0x20b2
#define GLX_CONTEXT_DEBUG_BIT_ARB 0x00000001
#define GLX_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB 0x00000002
#define GLX_CONTEXT_CORE_PROFILE_BIT_ARB 0x00000001
#define GLX_CONTEXT_PROFILE_MASK_ARB 0x9126
#define GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB 0x00000002
#define GLX_CONTEXT_MAJOR_VERSION_ARB 0x2091
#define GLX_CONTEXT_MINOR_VERSION_ARB 0x2092
#define GLX_CONTEXT_FLAGS_ARB 0x2094
#define GLX_CONTEXT_ROBUST_ACCESS_BIT_ARB 0x00000004
#define GLX_LOSE_CONTEXT_ON_RESET_ARB 0x8252
#define GLX_CONTEXT_RESET_NOTIFICATION_STRATEGY_ARB 0x8256
#define GLX_NO_RESET_NOTIFICATION_ARB 0x8261
#define GLX_CONTEXT_RELEASE_BEHAVIOR_ARB 0x2097
#define GLX_CONTEXT_RELEASE_BEHAVIOR_NONE_ARB 0
#define GLX_CONTEXT_RELEASE_BEHAVIOR_FLUSH_ARB 0x2098

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
typedef GLXContext (*PFNGLXCREATENEWCONTEXTPROC)(Display*,GLXFBConfig,int,GLXContext,Bool);
typedef __GLXextproc (* PFNGLXGETPROCADDRESSPROC)(const GLubyte *procName);
typedef void (*PFNGLXSWAPINTERVALEXTPROC)(Display*,GLXDrawable,int);
typedef XVisualInfo* (*PFNGLXGETVISUALFROMFBCONFIGPROC)(Display*,GLXFBConfig);
typedef GLXWindow (*PFNGLXCREATEWINDOWPROC)(Display*,GLXFBConfig,Window,const int*);
typedef void (*PFNGLXDESTROYWINDOWPROC)(Display*,GLXWindow);

typedef int (*PFNGLXSWAPINTERVALMESAPROC)(int);
typedef GLXContext (*PFNGLXCREATECONTEXTATTRIBSARBPROC)(Display*,GLXFBConfig,GLXContext,Bool,const int*);

static Display* _sapp_x11_display;
static int _sapp_x11_screen;
static Window _sapp_x11_root;
static Colormap _sapp_x11_colormap;
static Window _sapp_x11_window;
static float _sapp_x11_dpi;
static int _sapp_x11_window_state;
static unsigned char _sapp_x11_error_code;
static void* _sapp_glx_libgl;
static int _sapp_glx_major;
static int _sapp_glx_minor;
static int _sapp_glx_eventbase;
static int _sapp_glx_errorbase;
static GLXContext _sapp_glx_ctx;
static GLXWindow _sapp_glx_window;
static Atom _sapp_x11_UTF8_STRING;
static Atom _sapp_x11_WM_PROTOCOLS;
static Atom _sapp_x11_WM_DELETE_WINDOW;
static Atom _sapp_x11_WM_STATE;
static Atom _sapp_x11_NET_WM_NAME;
static Atom _sapp_x11_NET_WM_ICON_NAME;
// GLX 1.3 functions
static PFNGLXGETFBCONFIGSPROC              _sapp_glx_GetFBConfigs;
static PFNGLXGETFBCONFIGATTRIBPROC         _sapp_glx_GetFBConfigAttrib;
static PFNGLXGETCLIENTSTRINGPROC           _sapp_glx_GetClientString;
static PFNGLXQUERYEXTENSIONPROC            _sapp_glx_QueryExtension;
static PFNGLXQUERYVERSIONPROC              _sapp_glx_QueryVersion;
static PFNGLXDESTROYCONTEXTPROC            _sapp_glx_DestroyContext;
static PFNGLXMAKECURRENTPROC               _sapp_glx_MakeCurrent;
static PFNGLXSWAPBUFFERSPROC               _sapp_glx_SwapBuffers;
static PFNGLXQUERYEXTENSIONSSTRINGPROC     _sapp_glx_QueryExtensionsString;
static PFNGLXCREATENEWCONTEXTPROC          _sapp_glx_CreateNewContext;
static PFNGLXGETVISUALFROMFBCONFIGPROC     _sapp_glx_GetVisualFromFBConfig;
static PFNGLXCREATEWINDOWPROC              _sapp_glx_CreateWindow;
static PFNGLXDESTROYWINDOWPROC             _sapp_glx_DestroyWindow;

// GLX 1.4 and extension functions
static PFNGLXGETPROCADDRESSPROC            _sapp_glx_GetProcAddress;
static PFNGLXGETPROCADDRESSPROC            _sapp_glx_GetProcAddressARB;
static PFNGLXSWAPINTERVALEXTPROC           _sapp_glx_SwapIntervalEXT;
static PFNGLXSWAPINTERVALMESAPROC          _sapp_glx_SwapIntervalMESA;
static PFNGLXCREATECONTEXTATTRIBSARBPROC   _sapp_glx_CreateContextAttribsARB;
static bool _sapp_glx_EXT_swap_control;
static bool _sapp_glx_MESA_swap_control;
static bool _sapp_glx_ARB_multisample;
static bool _sapp_glx_ARB_framebuffer_sRGB;
static bool _sapp_glx_EXT_framebuffer_sRGB;
static bool _sapp_glx_ARB_create_context;
static bool _sapp_glx_ARB_create_context_profile;

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
    _sapp_x11_error_code = event->error_code;
    return 0;
}

_SOKOL_PRIVATE void _sapp_x11_grab_error_handler(void) {
    _sapp_x11_error_code = Success;
    XSetErrorHandler(_sapp_x11_error_handler);
}

_SOKOL_PRIVATE void _sapp_x11_release_error_handler(void) {
    XSync(_sapp_x11_display, False);
    XSetErrorHandler(NULL);
}

_SOKOL_PRIVATE void _sapp_x11_init_extensions(void) {
    _sapp_x11_UTF8_STRING       = XInternAtom(_sapp_x11_display, "UTF8_STRING", False);
    _sapp_x11_WM_PROTOCOLS      = XInternAtom(_sapp_x11_display, "WM_PROTOCOLS", False);
    _sapp_x11_WM_DELETE_WINDOW  = XInternAtom(_sapp_x11_display, "WM_DELETE_WINDOW", False);
    _sapp_x11_WM_STATE          = XInternAtom(_sapp_x11_display, "WM_STATE", False);
    _sapp_x11_NET_WM_NAME    = XInternAtom(_sapp_x11_display, "_NET_WM_NAME", False);
    _sapp_x11_NET_WM_ICON_NAME = XInternAtom(_sapp_x11_display, "_NET_WM_ICON_NAME", False);
}

_SOKOL_PRIVATE void _sapp_x11_query_system_dpi(void) {
    /* from GLFW:

       NOTE: Default to the display-wide DPI as we don't currently have a policy
             for which monitor a window is considered to be on
    _sapp_x11_dpi = DisplayWidth(_sapp_x11_display, _sapp_x11_screen) *
        25.4f / DisplayWidthMM(_sapp_x11_display, _sapp_x11_screen);

       NOTE: Basing the scale on Xft.dpi where available should provide the most
             consistent user experience (matches Qt, Gtk, etc), although not
             always the most accurate one
    */
    char* rms = XResourceManagerString(_sapp_x11_display);
    if (rms) {
        XrmDatabase db = XrmGetStringDatabase(rms);
        if (db) {
            XrmValue value;
            char* type = NULL;
            if (XrmGetResource(db, "Xft.dpi", "Xft.Dpi", &type, &value)) {
                if (type && strcmp(type, "String") == 0) {
                    _sapp_x11_dpi = atof(value.addr);
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
    if (_sapp_glx_GetProcAddress) {
        return (void*) _sapp_glx_GetProcAddress((const GLubyte*) procname);
    }
    else if (_sapp_glx_GetProcAddressARB) {
        return (void*) _sapp_glx_GetProcAddressARB((const GLubyte*) procname);
    }
    else {
        return dlsym(_sapp_glx_libgl, procname);
    }
}

_SOKOL_PRIVATE void _sapp_glx_init() {
    const char* sonames[] = { "libGL.so.1", "libGL.so", 0 };
    for (int i = 0; sonames[i]; i++) {
        _sapp_glx_libgl = dlopen(sonames[i], RTLD_LAZY|RTLD_GLOBAL);
        if (_sapp_glx_libgl) {
            break;
        }
    }
    if (!_sapp_glx_libgl) {
        _sapp_fail("GLX: failed to load libGL");
    }
    _sapp_glx_GetFBConfigs          = (PFNGLXGETFBCONFIGSPROC)          dlsym(_sapp_glx_libgl, "glXGetFBConfigs");
    _sapp_glx_GetFBConfigAttrib     = (PFNGLXGETFBCONFIGATTRIBPROC)     dlsym(_sapp_glx_libgl, "glXGetFBConfigAttrib");
    _sapp_glx_GetClientString       = (PFNGLXGETCLIENTSTRINGPROC)       dlsym(_sapp_glx_libgl, "glXGetClientString");
    _sapp_glx_QueryExtension        = (PFNGLXQUERYEXTENSIONPROC)        dlsym(_sapp_glx_libgl, "glXQueryExtension");
    _sapp_glx_QueryVersion          = (PFNGLXQUERYVERSIONPROC)          dlsym(_sapp_glx_libgl, "glXQueryVersion");
    _sapp_glx_DestroyContext        = (PFNGLXDESTROYCONTEXTPROC)        dlsym(_sapp_glx_libgl, "glXDestroyContext");
    _sapp_glx_MakeCurrent           = (PFNGLXMAKECURRENTPROC)           dlsym(_sapp_glx_libgl, "glXMakeCurrent");
    _sapp_glx_SwapBuffers           = (PFNGLXSWAPBUFFERSPROC)           dlsym(_sapp_glx_libgl, "glXSwapBuffers");
    _sapp_glx_QueryExtensionsString = (PFNGLXQUERYEXTENSIONSSTRINGPROC) dlsym(_sapp_glx_libgl, "glXQueryExtensionsString");
    _sapp_glx_CreateNewContext      = (PFNGLXCREATENEWCONTEXTPROC)      dlsym(_sapp_glx_libgl, "glXCreateNewContext");
    _sapp_glx_CreateWindow          = (PFNGLXCREATEWINDOWPROC)          dlsym(_sapp_glx_libgl, "glXCreateWindow");
    _sapp_glx_DestroyWindow         = (PFNGLXDESTROYWINDOWPROC)         dlsym(_sapp_glx_libgl, "glXDestroyWindow");
    _sapp_glx_GetProcAddress        = (PFNGLXGETPROCADDRESSPROC)        dlsym(_sapp_glx_libgl, "glXGetProcAddress");
    _sapp_glx_GetProcAddressARB     = (PFNGLXGETPROCADDRESSPROC)        dlsym(_sapp_glx_libgl, "glXGetProcAddressARB");
    _sapp_glx_GetVisualFromFBConfig = (PFNGLXGETVISUALFROMFBCONFIGPROC) dlsym(_sapp_glx_libgl, "glXGetVisualFromFBConfig");
    if (!_sapp_glx_GetFBConfigs ||
        !_sapp_glx_GetFBConfigAttrib ||
        !_sapp_glx_GetClientString ||
        !_sapp_glx_QueryExtension ||
        !_sapp_glx_QueryVersion ||
        !_sapp_glx_DestroyContext ||
        !_sapp_glx_MakeCurrent ||
        !_sapp_glx_SwapBuffers ||
        !_sapp_glx_QueryExtensionsString ||
        !_sapp_glx_CreateNewContext ||
        !_sapp_glx_CreateWindow ||
        !_sapp_glx_DestroyWindow ||
        !_sapp_glx_GetProcAddress ||
        !_sapp_glx_GetProcAddressARB ||
        !_sapp_glx_GetVisualFromFBConfig)
    {
        _sapp_fail("GLX: failed to load required entry points");
    }

    if (!_sapp_glx_QueryExtension(_sapp_x11_display,
                           &_sapp_glx_errorbase,
                           &_sapp_glx_eventbase))
    {
        _sapp_fail("GLX: GLX extension not found");
    }
    if (!_sapp_glx_QueryVersion(_sapp_x11_display, &_sapp_glx_major, &_sapp_glx_minor)) {
        _sapp_fail("GLX: Failed to query GLX version");
    }
    if (_sapp_glx_major == 1 && _sapp_glx_minor < 3) {
        _sapp_fail("GLX: GLX version 1.3 is required");
    }
    const char* exts = _sapp_glx_QueryExtensionsString(_sapp_x11_display, _sapp_x11_screen);
    if (_sapp_glx_extsupported("GLX_EXT_swap_control", exts)) {
        _sapp_glx_SwapIntervalEXT = (PFNGLXSWAPINTERVALEXTPROC) _sapp_glx_getprocaddr("glXSwapIntervalEXT");
        _sapp_glx_EXT_swap_control = 0 != _sapp_glx_SwapIntervalEXT;
    }
    if (_sapp_glx_extsupported("GLX_MESA_swap_control", exts)) {
        _sapp_glx_SwapIntervalMESA = (PFNGLXSWAPINTERVALMESAPROC) _sapp_glx_getprocaddr("glXSwapIntervalMESA");
        _sapp_glx_MESA_swap_control = 0 != _sapp_glx_SwapIntervalMESA;
    }
    _sapp_glx_ARB_multisample = _sapp_glx_extsupported("GLX_ARB_multisample", exts);
    _sapp_glx_ARB_framebuffer_sRGB = _sapp_glx_extsupported("GLX_ARB_framebuffer_sRGB", exts);
    _sapp_glx_EXT_framebuffer_sRGB = _sapp_glx_extsupported("GLX_EXT_framebuffer_sRGB", exts);
    if (_sapp_glx_extsupported("GLX_ARB_create_context", exts)) {
        _sapp_glx_CreateContextAttribsARB = (PFNGLXCREATECONTEXTATTRIBSARBPROC) _sapp_glx_getprocaddr("glXCreateContextAttribsARB");
        _sapp_glx_ARB_create_context = 0 != _sapp_glx_CreateContextAttribsARB;
    }
    _sapp_glx_ARB_create_context_profile = _sapp_glx_extsupported("GLX_ARB_create_context_profile", exts);
}

_SOKOL_PRIVATE int _sapp_glx_attrib(GLXFBConfig fbconfig, int attrib) {
    int value;
    _sapp_glx_GetFBConfigAttrib(_sapp_x11_display, fbconfig, attrib, &value);
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
    vendor = _sapp_glx_GetClientString(_sapp_x11_display, GLX_VENDOR);
    if (vendor && strcmp(vendor, "Chromium") == 0) {
        trust_window_bit = false;
    }

    native_configs = _sapp_glx_GetFBConfigs(_sapp_x11_display, _sapp_x11_screen, &native_count);
    if (!native_configs || !native_count) {
        _sapp_fail("GLX: No GLXFBConfigs returned");
    }

    usable_configs = (_sapp_gl_fbconfig*) SOKOL_CALLOC(native_count, sizeof(_sapp_gl_fbconfig));
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
        if (_sapp_glx_ARB_multisample) {
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
    XVisualInfo* result = _sapp_glx_GetVisualFromFBConfig(_sapp_x11_display, native);
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
    if (!(_sapp_glx_ARB_create_context && _sapp_glx_ARB_create_context_profile)) {
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
    _sapp_glx_ctx = _sapp_glx_CreateContextAttribsARB(_sapp_x11_display, native, NULL, True, attribs);
    if (!_sapp_glx_ctx) {
        _sapp_fail("GLX: failed to create GL context");
    }
    _sapp_x11_release_error_handler();
    _sapp_glx_window = _sapp_glx_CreateWindow(_sapp_x11_display, native, _sapp_x11_window, NULL);
    if (!_sapp_glx_window) {
        _sapp_fail("GLX: failed to create window");
    }
}

_SOKOL_PRIVATE void _sapp_glx_destroy_context(void) {
    if (_sapp_glx_window) {
        _sapp_glx_DestroyWindow(_sapp_x11_display, _sapp_glx_window);
        _sapp_glx_window = 0;
    }
    if (_sapp_glx_ctx) {
        _sapp_glx_DestroyContext(_sapp_x11_display, _sapp_glx_ctx);
        _sapp_glx_ctx = 0;
    }
}

_SOKOL_PRIVATE void _sapp_glx_make_current(void) {
    _sapp_glx_MakeCurrent(_sapp_x11_display, _sapp_glx_window, _sapp_glx_ctx);
}

_SOKOL_PRIVATE void _sapp_glx_swap_buffers(void) {
    _sapp_glx_SwapBuffers(_sapp_x11_display, _sapp_glx_window);
}

_SOKOL_PRIVATE void _sapp_glx_swapinterval(int interval) {
    _sapp_glx_make_current();
    if (_sapp_glx_EXT_swap_control) {
        _sapp_glx_SwapIntervalEXT(_sapp_x11_display, _sapp_glx_window, interval);
    }
    else if (_sapp_glx_MESA_swap_control) {
        _sapp_glx_SwapIntervalMESA(interval);
    }
}

_SOKOL_PRIVATE void _sapp_x11_update_window_title(void) {
    Xutf8SetWMProperties(_sapp_x11_display,
        _sapp_x11_window,
        _sapp.window_title, _sapp.window_title,
        NULL, 0, NULL, NULL, NULL);
    XChangeProperty(_sapp_x11_display, _sapp_x11_window,
        _sapp_x11_NET_WM_NAME, _sapp_x11_UTF8_STRING, 8,
        PropModeReplace,
        (unsigned char*)_sapp.window_title,
        strlen(_sapp.window_title));
    XChangeProperty(_sapp_x11_display, _sapp_x11_window,
        _sapp_x11_NET_WM_ICON_NAME, _sapp_x11_UTF8_STRING, 8,
        PropModeReplace,
        (unsigned char*)_sapp.window_title,
        strlen(_sapp.window_title));
    XFlush(_sapp_x11_display);
}

_SOKOL_PRIVATE void _sapp_x11_query_window_size(void) {
    XWindowAttributes attribs;
    XGetWindowAttributes(_sapp_x11_display, _sapp_x11_window, &attribs);
    _sapp.window_width = attribs.width;
    _sapp.window_height = attribs.height;
    _sapp.framebuffer_width = _sapp.window_width;
    _sapp.framebuffer_height = _sapp.framebuffer_height;
}

_SOKOL_PRIVATE void _sapp_x11_create_window(Visual* visual, int depth) {
    _sapp_x11_colormap = XCreateColormap(_sapp_x11_display, _sapp_x11_root, visual, AllocNone);




    XSetWindowAttributes wa;
    memset(&wa, 0, sizeof(wa));
   //wa.override_redirect = 1;
    const uint32_t wamask = CWBorderPixel | CWColormap | CWEventMask;
    wa.colormap = _sapp_x11_colormap;
    wa.border_pixel = 0;
    wa.event_mask = StructureNotifyMask | KeyPressMask | KeyReleaseMask |
                    PointerMotionMask | ButtonPressMask | ButtonReleaseMask |
                    ExposureMask | FocusChangeMask | VisibilityChangeMask |
                    EnterWindowMask | LeaveWindowMask | PropertyChangeMask;
    _sapp_x11_grab_error_handler();
    _sapp_x11_window = XCreateWindow(_sapp_x11_display,
                                     _sapp_x11_root,
                                     0, 0,
                                     _sapp.window_width,
                                     _sapp.window_height,
                                     0,     /* border width */
                                     depth, /* color depth */
                                     InputOutput,
                                     visual,
                                     wamask,
                                     &wa);
    _sapp_x11_release_error_handler();
    if (!_sapp_x11_window) {
        _sapp_fail("X11: Failed to create window");
    }

    if (_sapp.desc.fullscreen) {
	    Atom wm_state = XInternAtom(_sapp_x11_display, "_NET_WM_STATE", False);
	    Atom fullscreen = XInternAtom(_sapp_x11_display, "_NET_WM_STATE_FULLSCREEN", False);

	    XEvent xev;
	    memset(&xev, 0, sizeof(xev));
	    xev.type = ClientMessage;
	    xev.xclient.window = _sapp_x11_window;
	    xev.xclient.message_type = wm_state;
	    xev.xclient.format = 32;
	    xev.xclient.data.l[0] = 1;
	    xev.xclient.data.l[1] = fullscreen;
	    xev.xclient.data.l[2] = 0;

	    XMapWindow(_sapp_x11_display, _sapp_x11_window);

	    XSendEvent (_sapp_x11_display, DefaultRootWindow(_sapp_x11_display), False,
		    SubstructureRedirectMask | SubstructureNotifyMask, &xev);
    }

    Atom protocols[] = {
        _sapp_x11_WM_DELETE_WINDOW
    };
    XSetWMProtocols(_sapp_x11_display, _sapp_x11_window, protocols, 1);

    XSizeHints* hints = XAllocSizeHints();
    hints->flags |= PWinGravity;
    hints->win_gravity = StaticGravity;
    XSetWMNormalHints(_sapp_x11_display, _sapp_x11_window, hints);
    XFree(hints);

    _sapp_x11_update_window_title();
    _sapp_x11_query_window_size();
}

_SOKOL_PRIVATE void _sapp_x11_destroy_window(void) {
    if (_sapp_x11_window) {
        XUnmapWindow(_sapp_x11_display, _sapp_x11_window);
        XDestroyWindow(_sapp_x11_display, _sapp_x11_window);
        _sapp_x11_window = 0;
    }
    if (_sapp_x11_colormap) {
        XFreeColormap(_sapp_x11_display, _sapp_x11_colormap);
        _sapp_x11_colormap = 0;
    }
    XFlush(_sapp_x11_display);
}

_SOKOL_PRIVATE bool _sapp_x11_window_visible(void) {
    XWindowAttributes wa;
    XGetWindowAttributes(_sapp_x11_display, _sapp_x11_window, &wa);
    return wa.map_state == IsViewable;
}

_SOKOL_PRIVATE void _sapp_x11_show_window(void) {
    if (!_sapp_x11_window_visible()) {
        XMapWindow(_sapp_x11_display, _sapp_x11_window);
        XRaiseWindow(_sapp_x11_display, _sapp_x11_window);
        XFlush(_sapp_x11_display);
    }
}

_SOKOL_PRIVATE void _sapp_x11_hide_window(void) {
    XUnmapWindow(_sapp_x11_display, _sapp_x11_window);
    XFlush(_sapp_x11_display);
}

_SOKOL_PRIVATE unsigned long _sapp_x11_get_window_property(Atom property, Atom type, unsigned char** value) {
    Atom actualType;
    int actualFormat;
    unsigned long itemCount, bytesAfter;
    XGetWindowProperty(_sapp_x11_display,
                       _sapp_x11_window,
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

    if (_sapp_x11_get_window_property(_sapp_x11_WM_STATE, _sapp_x11_WM_STATE, (unsigned char**)&state) >= 2) {
        result = state->state;
    }
    if (state) {
        XFree(state);
    }
    return result;
}

_SOKOL_PRIVATE uint32_t _sapp_x11_mod(int x11_mods) {
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
        _sapp.event.mouse_x = _sapp.mouse_x;
        _sapp.event.mouse_y = _sapp.mouse_y;
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
        if (_sapp.clipboard_enabled &&
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
    KeySym* keysyms = XGetKeyboardMapping(_sapp_x11_display, scancode, 1, &dummy);
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

// XLib manual says keycodes are in the range [8, 255] inclusive.
// https://tronche.com/gui/x/xlib/input/keyboard-encoding.html
static bool _sapp_x11_keycodes[256];

_SOKOL_PRIVATE void _sapp_x11_process_event(XEvent* event) {
    switch (event->type) {
        case KeyPress:
            {
                int keycode = event->xkey.keycode;
                const sapp_keycode key = _sapp_x11_translate_key(keycode);
                bool repeat = _sapp_x11_keycodes[keycode & 0xFF];
                _sapp_x11_keycodes[keycode & 0xFF] = true;
                const uint32_t mods = _sapp_x11_mod(event->xkey.state);
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
                int keycode = event->xkey.keycode;
                const sapp_keycode key = _sapp_x11_translate_key(keycode);
                _sapp_x11_keycodes[keycode & 0xFF] = false;
                if (key != SAPP_KEYCODE_INVALID) {
                    const uint32_t mods = _sapp_x11_mod(event->xkey.state);
                    _sapp_x11_key_event(SAPP_EVENTTYPE_KEY_UP, key, false, mods);
                }
            }
            break;
        case ButtonPress:
            {
                const sapp_mousebutton btn = _sapp_x11_translate_button(event);
                const uint32_t mods = _sapp_x11_mod(event->xbutton.state);
                if (btn != SAPP_MOUSEBUTTON_INVALID) {
                    _sapp_x11_mouse_event(SAPP_EVENTTYPE_MOUSE_DOWN, btn, mods);
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
                    _sapp_x11_mouse_event(SAPP_EVENTTYPE_MOUSE_UP, btn, _sapp_x11_mod(event->xbutton.state));
                }
            }
            break;
        case EnterNotify:
            _sapp_x11_mouse_event(SAPP_EVENTTYPE_MOUSE_ENTER, SAPP_MOUSEBUTTON_INVALID, _sapp_x11_mod(event->xcrossing.state));
            break;
        case LeaveNotify:
            _sapp_x11_mouse_event(SAPP_EVENTTYPE_MOUSE_LEAVE, SAPP_MOUSEBUTTON_INVALID, _sapp_x11_mod(event->xcrossing.state));
            break;
        case MotionNotify:
            _sapp.mouse_x = event->xmotion.x;
            _sapp.mouse_y = event->xmotion.y;
            _sapp_x11_mouse_event(SAPP_EVENTTYPE_MOUSE_MOVE, SAPP_MOUSEBUTTON_INVALID, _sapp_x11_mod(event->xmotion.state));
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
                if (event->xproperty.atom == _sapp_x11_WM_STATE) {
                    const int state = _sapp_x11_get_window_state();
                    if (state != _sapp_x11_window_state) {
                        _sapp_x11_window_state = state;
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
            if (event->xclient.message_type == _sapp_x11_WM_PROTOCOLS) {
                const Atom protocol = event->xclient.data.l[0];
                if (protocol == _sapp_x11_WM_DELETE_WINDOW) {
                    _sapp.quit_requested = true;
                }
            }
            break;
        case DestroyNotify:
            break;
    }
}

_SOKOL_PRIVATE void _sapp_run(const sapp_desc* desc) {
    _sapp_init_state(desc);
    _sapp_x11_window_state = NormalState;

    XInitThreads();
    XrmInitialize();
    _sapp_x11_display = XOpenDisplay(NULL);
    if (!_sapp_x11_display) {
        _sapp_fail("XOpenDisplay() failed!\n");
    }
    _sapp_x11_screen = DefaultScreen(_sapp_x11_display);
    _sapp_x11_root = DefaultRootWindow(_sapp_x11_display);
    XkbSetDetectableAutoRepeat(_sapp_x11_display, true, NULL);
    _sapp_x11_query_system_dpi();
    _sapp.dpi_scale = _sapp_x11_dpi / 96.0f;
    _sapp_x11_init_extensions();
    _sapp_glx_init();
    Visual* visual = 0;
    int depth = 0;
    _sapp_glx_choose_visual(&visual, &depth);
    _sapp_x11_create_window(visual, depth);
    _sapp_glx_create_context();
    _sapp.valid = true;
    _sapp_x11_show_window();
    _sapp_glx_swapinterval(_sapp.swap_interval);
    XFlush(_sapp_x11_display);
    while (!_sapp.quit_ordered) {
        _sapp_glx_make_current();
        int count = XPending(_sapp_x11_display);
        while (count--) {
            XEvent event;
            XNextEvent(_sapp_x11_display, &event);
            _sapp_x11_process_event(&event);
        }
        _sapp_frame();
        _sapp_glx_swap_buffers();
        XFlush(_sapp_x11_display);
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
    XCloseDisplay(_sapp_x11_display);
    _sapp_discard_state();
}

#if !defined(SOKOL_NO_ENTRY)
int main(int argc, char* argv[]) {
    sapp_desc desc = sokol_main(argc, argv);
    _sapp_run(&desc);
    return 0;
}
#endif /* SOKOL_NO_ENTRY */
#endif /* LINUX */

/*== PUBLIC API FUNCTIONS ====================================================*/
#if defined(SOKOL_NO_ENTRY)
SOKOL_API_IMPL int sapp_run(const sapp_desc* desc) {
    SOKOL_ASSERT(desc);
    _sapp_run(desc);
    return 0;
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
SOKOL_API_IMPL int sapp_run(const sapp_desc* desc) {
    _SOKOL_UNUSED(desc);
    return 0;
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

SOKOL_API_IMPL int sapp_width(void) {
    return (_sapp.framebuffer_width > 0) ? _sapp.framebuffer_width : 1;
}

SOKOL_API_IMPL int sapp_height(void) {
    return (_sapp.framebuffer_height > 0) ? _sapp.framebuffer_height : 1;
}

SOKOL_API_IMPL bool sapp_high_dpi(void) {
    return _sapp.desc.high_dpi && (_sapp.dpi_scale > 1.5f);
}

SOKOL_API_IMPL float sapp_dpi_scale(void) {
    return _sapp.dpi_scale;
}

SOKOL_API_IMPL bool sapp_gles2(void) {
    return _sapp.gles2_fallback;
}

SOKOL_API_IMPL void sapp_show_keyboard(bool shown) {
    #if defined(TARGET_OS_IPHONE) && TARGET_OS_IPHONE
    _sapp_ios_show_keyboard(shown);
    #elif defined(__EMSCRIPTEN__)
    _sapp_emsc_show_keyboard(shown);
    #elif defined(__ANDROID__)
    _sapp_android_show_keyboard(shown);
    #else
    _SOKOL_UNUSED(shown);
    #endif
}

SOKOL_API_IMPL bool sapp_keyboard_shown(void) {
    return _sapp.onscreen_keyboard_shown;
}

SOKOL_API_IMPL void sapp_show_mouse(bool shown) {
    #if defined(_WIN32)
    _sapp_win32_show_mouse(shown);
    #else
    _SOKOL_UNUSED(shown);
    #endif
}

SOKOL_API_IMPL bool sapp_mouse_shown(void) {
    #if defined(_WIN32)
    return _sapp_win32_mouse_shown();
    #else
    return false;
    #endif
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
    if (!_sapp.clipboard_enabled) {
        return;
    }
    SOKOL_ASSERT(str);
    #if defined(__APPLE__) && defined(TARGET_OS_IPHONE) && !TARGET_OS_IPHONE
        _sapp_macos_set_clipboard_string(str);
    #elif defined(__EMSCRIPTEN__)
        _sapp_emsc_set_clipboard_string(str);
    #elif defined(_WIN32)
        _sapp_win32_set_clipboard_string(str);
    #else
        /* not implemented */
    #endif
    _sapp_strcpy(str, _sapp.clipboard, _sapp.clipboard_size);
}

SOKOL_API_IMPL const char* sapp_get_clipboard_string(void) {
    if (!_sapp.clipboard_enabled) {
        return "";
    }
    #if defined(__APPLE__) && defined(TARGET_OS_IPHONE) && !TARGET_OS_IPHONE
        return _sapp_macos_get_clipboard_string();
    #elif defined(__EMSCRIPTEN__)
        return _sapp.clipboard;
    #elif defined(_WIN32)
        return _sapp_win32_get_clipboard_string();
    #else
        /* not implemented */
        return _sapp.clipboard;
    #endif
}

SOKOL_API_IMPL const void* sapp_metal_get_device(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(SOKOL_METAL)
        const void* obj = (__bridge const void*) _sapp_mtl_device_obj;
        SOKOL_ASSERT(obj);
        return obj;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_metal_get_renderpass_descriptor(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(SOKOL_METAL)
        const void* obj =  (__bridge const void*) [_sapp_view_obj currentRenderPassDescriptor];
        SOKOL_ASSERT(obj);
        return obj;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_metal_get_drawable(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(SOKOL_METAL)
        const void* obj = (__bridge const void*) [_sapp_view_obj currentDrawable];
        SOKOL_ASSERT(obj);
        return obj;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_macos_get_window(void) {
    #if defined(__APPLE__) && !TARGET_OS_IPHONE
        const void* obj = (__bridge const void*) _sapp_macos_window_obj;
        SOKOL_ASSERT(obj);
        return obj;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL void sapp_macos_set_title(const char* title) {
    #if defined(__APPLE__) && !TARGET_OS_IPHONE
        [_sapp_macos_window_obj setTitle: [NSString stringWithUTF8String:title]];
    #endif
}

SOKOL_API_IMPL const void* sapp_ios_get_window(void) {
    #if defined(__APPLE__) && TARGET_OS_IPHONE
        const void* obj = (__bridge const void*) _sapp_ios_window_obj;
        SOKOL_ASSERT(obj);
        return obj;
    #else
        return 0;
    #endif

}

SOKOL_API_IMPL const void* sapp_d3d11_get_device(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(SOKOL_D3D11)
        return _sapp_d3d11_device;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_d3d11_get_device_context(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(SOKOL_D3D11)
        return _sapp_d3d11_device_context;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_d3d11_get_render_target_view(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(SOKOL_D3D11)
        return _sapp_d3d11_rtv;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_d3d11_get_depth_stencil_view(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(SOKOL_D3D11)
        return _sapp_d3d11_dsv;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_win32_get_hwnd(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(_WIN32)
        return _sapp_win32_hwnd;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL const void* sapp_android_get_native_activity(void) {
    SOKOL_ASSERT(_sapp.valid);
    #if defined(__ANDROID__)
        return (void*)_sapp_android_state.activity;
    #else
        return 0;
    #endif
}

SOKOL_API_IMPL void sapp_html5_ask_leave_site(bool ask) {
    _sapp.html5_ask_leave_site = ask;
}

#undef _sapp_def

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#endif /* SOKOL_IMPL */
