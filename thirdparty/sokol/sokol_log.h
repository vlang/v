#if defined(SOKOL_IMPL) && !defined(SOKOL_LOG_IMPL)
#define SOKOL_LOG_IMPL
#endif
#ifndef SOKOL_LOG_INCLUDED
/*
    sokol_log.h -- common logging callback for sokol headers

    Project URL: https://github.com/floooh/sokol

    Example code: https://github.com/floooh/sokol-samples

    Do this:
        #define SOKOL_IMPL or
        #define SOKOL_LOG_IMPL
    before you include this file in *one* C or C++ file to create the
    implementation.

    Optionally provide the following defines when building the implementation:

    SOKOL_ASSERT(c)             - your own assert macro (default: assert(c))
    SOKOL_UNREACHABLE()         - a guard macro for unreachable code (default: assert(false))
    SOKOL_LOG_API_DECL          - public function declaration prefix (default: extern)
    SOKOL_API_DECL              - same as SOKOL_GFX_API_DECL
    SOKOL_API_IMPL              - public function implementation prefix (default: -)

    Optionally define the following for verbose output:

    SOKOL_DEBUG         - by default this is defined if NDEBUG is not defined


    OVERVIEW
    ========
    sokol_log.h provides a default logging callback for other sokol headers.

    To use the default log callback, just include sokol_log.h and provide
    a function pointer to the 'slog_func' function when setting up the
    sokol library:

    For instance with sokol_audio.h:

        #include "sokol_log.h"
        ...
        saudio_setup(&(saudio_desc){ .logger.func = slog_func });

    Logging output goes to stderr and/or a platform specific logging subsystem
    (which means that in some scenarios you might see logging messages duplicated):

        - Windows: stderr + OutputDebugStringA()
        - macOS/iOS/Linux: stderr + syslog()
        - Emscripten: console.info()/warn()/error()
        - Android: __android_log_write()

    On Windows with sokol_app.h also note the runtime config items to make
    stdout/stderr output visible on the console for WinMain() applications
    via sapp_desc.win32.console_attach or sapp_desc.win32.console_create,
    however when running in a debugger on Windows, the logging output should
    show up on the debug output UI panel.

    In debug mode, a log message might look like this:

        [sspine][error][id:12] /Users/floh/projects/sokol/util/sokol_spine.h:3472:0:
            SKELETON_DESC_NO_ATLAS: no atlas object provided in sspine_skeleton_desc.atlas

    The source path and line number is formatted like compiler errors, in some IDEs (like VSCode)
    such error messages are clickable.

    In release mode, logging is less verbose as to not bloat the executable with string data, but you still get
    enough information to identify the type and location of an error:

        [sspine][error][id:12][line:3472]

    RULES FOR WRITING YOUR OWN LOGGING FUNCTION
    ===========================================
    - must be re-entrant because it might be called from different threads
    - must treat **all** provided string pointers as optional (can be null)
    - don't store the string pointers, copy the string data instead
    - must not return for log level panic

    LICENSE
    =======
    zlib/libpng license

    Copyright (c) 2023 Andre Weissflog

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
#define SOKOL_LOG_INCLUDED (1)
#include <stdint.h>

#if defined(SOKOL_API_DECL) && !defined(SOKOL_LOG_API_DECL)
#define SOKOL_LOG_API_DECL SOKOL_API_DECL
#endif
#ifndef SOKOL_LOG_API_DECL
#if defined(_WIN32) && defined(SOKOL_DLL) && defined(SOKOL_LOG_IMPL)
#define SOKOL_LOG_API_DECL __declspec(dllexport)
#elif defined(_WIN32) && defined(SOKOL_DLL)
#define SOKOL_LOG_API_DECL __declspec(dllimport)
#else
#define SOKOL_LOG_API_DECL extern
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*
    Plug this function into the 'logger.func' struct item when initializing any of the sokol
    headers. For instance for sokol_audio.h it would look like this:

    saudio_setup(&(saudio_desc){
        .logger = {
            .func = slog_func
        }
    });
*/
SOKOL_LOG_API_DECL void slog_func(const char* tag, uint32_t log_level, uint32_t log_item, const char* message, uint32_t line_nr, const char* filename, void* user_data);

#ifdef __cplusplus
} // extern "C"
#endif
#endif // SOKOL_LOG_INCLUDED

// ██ ███    ███ ██████  ██      ███████ ███    ███ ███████ ███    ██ ████████  █████  ████████ ██  ██████  ███    ██
// ██ ████  ████ ██   ██ ██      ██      ████  ████ ██      ████   ██    ██    ██   ██    ██    ██ ██    ██ ████   ██
// ██ ██ ████ ██ ██████  ██      █████   ██ ████ ██ █████   ██ ██  ██    ██    ███████    ██    ██ ██    ██ ██ ██  ██
// ██ ██  ██  ██ ██      ██      ██      ██  ██  ██ ██      ██  ██ ██    ██    ██   ██    ██    ██ ██    ██ ██  ██ ██
// ██ ██      ██ ██      ███████ ███████ ██      ██ ███████ ██   ████    ██    ██   ██    ██    ██  ██████  ██   ████
//
// >>implementation
#ifdef SOKOL_LOG_IMPL
#define SOKOL_LOG_IMPL_INCLUDED (1)

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

// platform detection
#if defined(__APPLE__)
    #define _SLOG_APPLE (1)
#elif defined(__EMSCRIPTEN__)
    #define _SLOG_EMSCRIPTEN (1)
#elif defined(_WIN32)
    #define _SLOG_WINDOWS (1)
#elif defined(__ANDROID__)
    #define _SLOG_ANDROID (1)
#elif defined(__linux__) || defined(__unix__)
    #define _SLOG_LINUX (1)
#else
#error "sokol_log.h: unknown platform"
#endif

#include <stdlib.h> // abort
#include <stdio.h>  // fputs
#include <stddef.h> // size_t

#if defined(_SLOG_EMSCRIPTEN)
#include <emscripten/emscripten.h>
#elif defined(_SLOG_WINDOWS)
#ifndef WIN32_LEAN_AND_MEAN
    #define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
    #define NOMINMAX
#endif
#include <windows.h>
#elif defined(_SLOG_ANDROID)
#include <android/log.h>
#elif defined(_SLOG_LINUX) || defined(_SLOG_APPLE)
#include <syslog.h>
#endif

// size of line buffer (on stack!) in bytes including terminating zero
#define _SLOG_LINE_LENGTH (512)

_SOKOL_PRIVATE char* _slog_append(const char* str, char* dst, char* end) {
    if (str) {
        char c;
        while (((c = *str++) != 0) && (dst < (end - 1))) {
            *dst++ = c;
        }
    }
    *dst = 0;
    return dst;
}

_SOKOL_PRIVATE char* _slog_itoa(uint32_t x, char* buf, size_t buf_size) {
    const size_t max_digits_and_null = 11;
    if (buf_size < max_digits_and_null) {
        return 0;
    }
    char* p = buf + max_digits_and_null;
    *--p = 0;
    do {
        *--p = '0' + (x % 10);
        x /= 10;
    } while (x != 0);
    return p;
}

#if defined(_SLOG_EMSCRIPTEN)
EM_JS(void, slog_js_log, (uint32_t level, const char* c_str), {
    const str = UTF8ToString(c_str);
    switch (level) {
        case 0: console.error(str); break;
        case 1: console.error(str); break;
        case 2: console.warn(str); break;
        default: console.info(str); break;
    }
})
#endif

SOKOL_API_IMPL void slog_func(const char* tag, uint32_t log_level, uint32_t log_item, const char* message, uint32_t line_nr, const char* filename, void* user_data) {
    _SOKOL_UNUSED(user_data);

    const char* log_level_str;
    switch (log_level) {
        case 0: log_level_str = "panic"; break;
        case 1: log_level_str = "error"; break;
        case 2: log_level_str = "warning"; break;
        default: log_level_str = "info"; break;
    }

    // build log output line
    char line_buf[_SLOG_LINE_LENGTH];
    char* str = line_buf;
    char* end = line_buf + sizeof(line_buf);
    char num_buf[32];
    if (tag) {
        str = _slog_append("[", str, end);
        str = _slog_append(tag, str, end);
        str = _slog_append("]", str, end);
    }
    str = _slog_append("[", str, end);
    str = _slog_append(log_level_str, str, end);
    str = _slog_append("]", str, end);
    str = _slog_append("[id:", str, end);
    str = _slog_append(_slog_itoa(log_item, num_buf, sizeof(num_buf)), str, end);
    str = _slog_append("]", str, end);
    // if a filename is provided, build a clickable log message that's compatible with compiler error messages
    if (filename) {
        str = _slog_append(" ", str, end);
        #if defined(_MSC_VER)
            // MSVC compiler error format
            str = _slog_append(filename, str, end);
            str = _slog_append("(", str, end);
            str = _slog_append(_slog_itoa(line_nr, num_buf, sizeof(num_buf)), str, end);
            str = _slog_append("): ", str, end);
        #else
            // gcc/clang compiler error format
            str = _slog_append(filename, str, end);
            str = _slog_append(":", str, end);
            str = _slog_append(_slog_itoa(line_nr, num_buf, sizeof(num_buf)), str, end);
            str = _slog_append(":0: ", str, end);
        #endif
    }
    else {
        str = _slog_append("[line:", str, end);
        str = _slog_append(_slog_itoa(line_nr, num_buf, sizeof(num_buf)), str, end);
        str = _slog_append("] ", str, end);
    }
    if (message) {
        str = _slog_append("\n\t", str, end);
        str = _slog_append(message, str, end);
    }
    str = _slog_append("\n\n", str, end);
    if (0 == log_level) {
        str = _slog_append("ABORTING because of [panic]\n", str, end);
        (void)str;
    }

    // print to stderr?
    #if defined(_SLOG_LINUX) || defined(_SLOG_WINDOWS) || defined(_SLOG_APPLE)
        fputs(line_buf, stderr);
    #endif

    // platform specific logging calls
    #if defined(_SLOG_WINDOWS)
        OutputDebugStringA(line_buf);
    #elif defined(_SLOG_ANDROID)
        int prio;
        switch (log_level) {
            case 0: prio = ANDROID_LOG_FATAL; break;
            case 1: prio = ANDROID_LOG_ERROR; break;
            case 2: prio = ANDROID_LOG_WARN; break;
            default: prio = ANDROID_LOG_INFO; break;
        }
        __android_log_write(prio, "SOKOL", line_buf);
    #elif defined(_SLOG_EMSCRIPTEN)
        slog_js_log(log_level, line_buf);
    #endif
    if (0 == log_level) {
        abort();
    }
}
#endif // SOKOL_LOG_IMPL
