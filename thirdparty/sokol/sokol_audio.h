#if defined(SOKOL_IMPL) && !defined(SOKOL_AUDIO_IMPL)
#define SOKOL_AUDIO_IMPL
#endif
#ifndef SOKOL_AUDIO_INCLUDED
/*
    sokol_audio.h -- cross-platform audio-streaming API

    Project URL: https://github.com/floooh/sokol

    Do this:
        #define SOKOL_IMPL or
        #define SOKOL_AUDIO_IMPL
    before you include this file in *one* C or C++ file to create the
    implementation.

    Optionally provide the following defines with your own implementations:

    SOKOL_DUMMY_BACKEND - use a dummy backend
    SOKOL_ASSERT(c)     - your own assert macro (default: assert(c))
    SOKOL_AUDIO_API_DECL- public function declaration prefix (default: extern)
    SOKOL_API_DECL      - same as SOKOL_AUDIO_API_DECL
    SOKOL_API_IMPL      - public function implementation prefix (default: -)

    SAUDIO_RING_MAX_SLOTS           - max number of slots in the push-audio ring buffer (default 1024)
    SAUDIO_OSX_USE_SYSTEM_HEADERS   - define this to force inclusion of system headers on
                                      macOS instead of using embedded CoreAudio declarations
    SAUDIO_ANDROID_AAUDIO           - on Android, select the AAudio backend (default)
    SAUDIO_ANDROID_SLES             - on Android, select the OpenSLES backend

    If sokol_audio.h is compiled as a DLL, define the following before
    including the declaration or implementation:

    SOKOL_DLL

    On Windows, SOKOL_DLL will define SOKOL_AUDIO_API_DECL as __declspec(dllexport)
    or __declspec(dllimport) as needed.

    Link with the following libraries:

    - on macOS: AudioToolbox
    - on iOS: AudioToolbox, AVFoundation
    - on Linux: asound
    - on Android: link with OpenSLES or aaudio
    - on Windows with MSVC or Clang toolchain: no action needed, libs are defined in-source via pragma-comment-lib
    - on Windows with MINGW/MSYS2 gcc: compile with '-mwin32' and link with -lole32

    FEATURE OVERVIEW
    ================
    You provide a mono- or stereo-stream of 32-bit float samples, which
    Sokol Audio feeds into platform-specific audio backends:

    - Windows: WASAPI
    - Linux: ALSA
    - macOS: CoreAudio
    - iOS: CoreAudio+AVAudioSession
    - emscripten: WebAudio with ScriptProcessorNode
    - Android: AAudio (default) or OpenSLES, select at build time

    Sokol Audio will not do any buffer mixing or volume control, if you have
    multiple independent input streams of sample data you need to perform the
    mixing yourself before forwarding the data to Sokol Audio.

    There are two mutually exclusive ways to provide the sample data:

    1. Callback model: You provide a callback function, which will be called
       when Sokol Audio needs new samples. On all platforms except emscripten,
       this function is called from a separate thread.
    2. Push model: Your code pushes small blocks of sample data from your
       main loop or a thread you created. The pushed data is stored in
       a ring buffer where it is pulled by the backend code when
       needed.

    The callback model is preferred because it is the most direct way to
    feed sample data into the audio backends and also has less moving parts
    (there is no ring buffer between your code and the audio backend).

    Sometimes it is not possible to generate the audio stream directly in a
    callback function running in a separate thread, for such cases Sokol Audio
    provides the push-model as a convenience.

    SOKOL AUDIO, SOLOUD AND MINIAUDIO
    =================================
    The WASAPI, ALSA, OpenSLES and CoreAudio backend code has been taken from the
    SoLoud library (with some modifications, so any bugs in there are most
    likely my fault). If you need a more fully-featured audio solution, check
    out SoLoud, it's excellent:

        https://github.com/jarikomppa/soloud

    Another alternative which feature-wise is somewhere inbetween SoLoud and
    sokol-audio might be MiniAudio:

        https://github.com/mackron/miniaudio

    GLOSSARY
    ========
    - stream buffer:
        The internal audio data buffer, usually provided by the backend API. The
        size of the stream buffer defines the base latency, smaller buffers have
        lower latency but may cause audio glitches. Bigger buffers reduce or
        eliminate glitches, but have a higher base latency.

    - stream callback:
        Optional callback function which is called by Sokol Audio when it
        needs new samples. On Windows, macOS/iOS and Linux, this is called in
        a separate thread, on WebAudio, this is called per-frame in the
        browser thread.

    - channel:
        A discrete track of audio data, currently 1-channel (mono) and
        2-channel (stereo) is supported and tested.

    - sample:
        The magnitude of an audio signal on one channel at a given time. In
        Sokol Audio, samples are 32-bit float numbers in the range -1.0 to
        +1.0.

    - frame:
        The tightly packed set of samples for all channels at a given time.
        For mono 1 frame is 1 sample. For stereo, 1 frame is 2 samples.

    - packet:
        In Sokol Audio, a small chunk of audio data that is moved from the
        main thread to the audio streaming thread in order to decouple the
        rate at which the main thread provides new audio data, and the
        streaming thread consuming audio data.

    WORKING WITH SOKOL AUDIO
    ========================
    First call saudio_setup() with your preferred audio playback options.
    In most cases you can stick with the default values, these provide
    a good balance between low-latency and glitch-free playback
    on all audio backends.

    You should always provide a logging callback to be aware of any
    warnings and errors. The easiest way is to use sokol_log.h for this:

        #include "sokol_log.h"
        // ...
        saudio_setup(&(saudio_desc){
            .logger = {
                .func = slog_func,
            }
        });

    If you want to use the callback-model, you need to provide a stream
    callback function either in saudio_desc.stream_cb or saudio_desc.stream_userdata_cb,
    otherwise keep both function pointers zero-initialized.

    Use push model and default playback parameters:

        saudio_setup(&(saudio_desc){ .logger.func = slog_func });

    Use stream callback model and default playback parameters:

        saudio_setup(&(saudio_desc){
            .stream_cb = my_stream_callback
            .logger.func = slog_func,
        });

    The standard stream callback doesn't have a user data argument, if you want
    that, use the alternative stream_userdata_cb and also set the user_data pointer:

        saudio_setup(&(saudio_desc){
            .stream_userdata_cb = my_stream_callback,
            .user_data = &my_data
            .logger.func = slog_func,
        });

    The following playback parameters can be provided through the
    saudio_desc struct:

    General parameters (both for stream-callback and push-model):

        int sample_rate     -- the sample rate in Hz, default: 44100
        int num_channels    -- number of channels, default: 1 (mono)
        int buffer_frames   -- number of frames in streaming buffer, default: 2048

    The stream callback prototype (either with or without userdata):

        void (*stream_cb)(float* buffer, int num_frames, int num_channels)
        void (*stream_userdata_cb)(float* buffer, int num_frames, int num_channels, void* user_data)
            Function pointer to the user-provide stream callback.

    Push-model parameters:

        int packet_frames   -- number of frames in a packet, default: 128
        int num_packets     -- number of packets in ring buffer, default: 64

    The sample_rate and num_channels parameters are only hints for the audio
    backend, it isn't guaranteed that those are the values used for actual
    playback.

    To get the actual parameters, call the following functions after
    saudio_setup():

        int saudio_sample_rate(void)
        int saudio_channels(void);

    It's unlikely that the number of channels will be different than requested,
    but a different sample rate isn't uncommon.

    (NOTE: there's an yet unsolved issue when an audio backend might switch
    to a different sample rate when switching output devices, for instance
    plugging in a bluetooth headset, this case is currently not handled in
    Sokol Audio).

    You can check if audio initialization was successful with
    saudio_isvalid(). If backend initialization failed for some reason
    (for instance when there's no audio device in the machine), this
    will return false. Not checking for success won't do any harm, all
    Sokol Audio function will silently fail when called after initialization
    has failed, so apart from missing audio output, nothing bad will happen.

    Before your application exits, you should call

        saudio_shutdown();

    This stops the audio thread (on Linux, Windows and macOS/iOS) and
    properly shuts down the audio backend.

    THE STREAM CALLBACK MODEL
    =========================
    To use Sokol Audio in stream-callback-mode, provide a callback function
    like this in the saudio_desc struct when calling saudio_setup():

    void stream_cb(float* buffer, int num_frames, int num_channels) {
        ...
    }

    Or the alternative version with a user-data argument:

    void stream_userdata_cb(float* buffer, int num_frames, int num_channels, void* user_data) {
        my_data_t* my_data = (my_data_t*) user_data;
        ...
    }

    The job of the callback function is to fill the *buffer* with 32-bit
    float sample values.

    To output silence, fill the buffer with zeros:

        void stream_cb(float* buffer, int num_frames, int num_channels) {
            const int num_samples = num_frames * num_channels;
            for (int i = 0; i < num_samples; i++) {
                buffer[i] = 0.0f;
            }
        }

    For stereo output (num_channels == 2), the samples for the left
    and right channel are interleaved:

        void stream_cb(float* buffer, int num_frames, int num_channels) {
            assert(2 == num_channels);
            for (int i = 0; i < num_frames; i++) {
                buffer[2*i + 0] = ...;  // left channel
                buffer[2*i + 1] = ...;  // right channel
            }
        }

    Please keep in mind that the stream callback function is running in a
    separate thread, if you need to share data with the main thread you need
    to take care yourself to make the access to the shared data thread-safe!

    THE PUSH MODEL
    ==============
    To use the push-model for providing audio data, simply don't set (keep
    zero-initialized) the stream_cb field in the saudio_desc struct when
    calling saudio_setup().

    To provide sample data with the push model, call the saudio_push()
    function at regular intervals (for instance once per frame). You can
    call the saudio_expect() function to ask Sokol Audio how much room is
    in the ring buffer, but if you provide a continuous stream of data
    at the right sample rate, saudio_expect() isn't required (it's a simple
    way to sync/throttle your sample generation code with the playback
    rate though).

    With saudio_push() you may need to maintain your own intermediate sample
    buffer, since pushing individual sample values isn't very efficient.
    The following example is from the MOD player sample in
    sokol-samples (https://github.com/floooh/sokol-samples):

        const int num_frames = saudio_expect();
        if (num_frames > 0) {
            const int num_samples = num_frames * saudio_channels();
            read_samples(flt_buf, num_samples);
            saudio_push(flt_buf, num_frames);
        }

    Another option is to ignore saudio_expect(), and just push samples as they
    are generated in small batches. In this case you *need* to generate the
    samples at the right sample rate:

    The following example is taken from the Tiny Emulators project
    (https://github.com/floooh/chips-test), this is for mono playback,
    so (num_samples == num_frames):

        // tick the sound generator
        if (ay38910_tick(&sys->psg)) {
            // new sample is ready
            sys->sample_buffer[sys->sample_pos++] = sys->psg.sample;
            if (sys->sample_pos == sys->num_samples) {
                // new sample packet is ready
                saudio_push(sys->sample_buffer, sys->num_samples);
                sys->sample_pos = 0;
            }
        }

    THE WEBAUDIO BACKEND
    ====================
    The WebAudio backend is currently using a ScriptProcessorNode callback to
    feed the sample data into WebAudio. ScriptProcessorNode has been
    deprecated for a while because it is running from the main thread, with
    the default initialization parameters it works 'pretty well' though.
    Ultimately Sokol Audio will use Audio Worklets, but this requires a few
    more things to fall into place (Audio Worklets implemented everywhere,
    SharedArrayBuffers enabled again, and I need to figure out a 'low-cost'
    solution in terms of implementation effort, since Audio Worklets are
    a lot more complex than ScriptProcessorNode if the audio data needs to come
    from the main thread).

    The WebAudio backend is automatically selected when compiling for
    emscripten (__EMSCRIPTEN__ define exists).

    https://developers.google.com/web/updates/2017/12/audio-worklet
    https://developers.google.com/web/updates/2018/06/audio-worklet-design-pattern

    "Blob URLs": https://www.html5rocks.com/en/tutorials/workers/basics/

    Also see: https://blog.paul.cx/post/a-wait-free-spsc-ringbuffer-for-the-web/

    THE COREAUDIO BACKEND
    =====================
    The CoreAudio backend is selected on macOS and iOS (__APPLE__ is defined).
    Since the CoreAudio API is implemented in C (not Objective-C) on macOS the
    implementation part of Sokol Audio can be included into a C source file.

    However on iOS, Sokol Audio must be compiled as Objective-C due to it's
    reliance on the AVAudioSession object. The iOS code path support both
    being compiled with or without ARC (Automatic Reference Counting).

    For thread synchronisation, the CoreAudio backend will use the
    pthread_mutex_* functions.

    The incoming floating point samples will be directly forwarded to
    CoreAudio without further conversion.

    macOS and iOS applications that use Sokol Audio need to link with
    the AudioToolbox framework.

    THE WASAPI BACKEND
    ==================
    The WASAPI backend is automatically selected when compiling on Windows
    (_WIN32 is defined).

    For thread synchronisation a Win32 critical section is used.

    WASAPI may use a different size for its own streaming buffer then requested,
    so the base latency may be slightly bigger. The current backend implementation
    converts the incoming floating point sample values to signed 16-bit
    integers.

    The required Windows system DLLs are linked with #pragma comment(lib, ...),
    so you shouldn't need to add additional linker libs in the build process
    (otherwise this is a bug which should be fixed in sokol_audio.h).

    THE ALSA BACKEND
    ================
    The ALSA backend is automatically selected when compiling on Linux
    ('linux' is defined).

    For thread synchronisation, the pthread_mutex_* functions are used.

    Samples are directly forwarded to ALSA in 32-bit float format, no
    further conversion is taking place.

    You need to link with the 'asound' library, and the <alsa/asoundlib.h>
    header must be present (usually both are installed with some sort
    of ALSA development package).


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

        ...
            saudio_setup(&(saudio_desc){
                // ...
                .allocator = {
                    .alloc_fn = my_alloc,
                    .free_fn = my_free,
                    .user_data = ...,
                }
            });
        ...

    If no overrides are provided, malloc and free will be used.

    This only affects memory allocation calls done by sokol_audio.h
    itself though, not any allocations in OS libraries.

    Memory allocation will only happen on the same thread where saudio_setup()
    was called, so you don't need to worry about thread-safety.


    ERROR REPORTING AND LOGGING
    ===========================
    To get any logging information at all you need to provide a logging callback in the setup call
    the easiest way is to use sokol_log.h:

        #include "sokol_log.h"

        saudio_setup(&(saudio_desc){ .logger.func = slog_func });

    To override logging with your own callback, first write a logging function like this:

        void my_log(const char* tag,                // e.g. 'saudio'
                    uint32_t log_level,             // 0=panic, 1=error, 2=warn, 3=info
                    uint32_t log_item_id,           // SAUDIO_LOGITEM_*
                    const char* message_or_null,    // a message string, may be nullptr in release mode
                    uint32_t line_nr,               // line number in sokol_audio.h
                    const char* filename_or_null,   // source filename, may be nullptr in release mode
                    void* user_data)
        {
            ...
        }

    ...and then setup sokol-audio like this:

        saudio_setup(&(saudio_desc){
            .logger = {
                .func = my_log,
                .user_data = my_user_data,
            }
        });

    The provided logging function must be reentrant (e.g. be callable from
    different threads).

    If you don't want to provide your own custom logger it is highly recommended to use
    the standard logger in sokol_log.h instead, otherwise you won't see any warnings or
    errors.

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
#define SOKOL_AUDIO_INCLUDED (1)
#include <stddef.h> // size_t
#include <stdint.h>
#include <stdbool.h>

#if defined(SOKOL_API_DECL) && !defined(SOKOL_AUDIO_API_DECL)
#define SOKOL_AUDIO_API_DECL SOKOL_API_DECL
#endif
#ifndef SOKOL_AUDIO_API_DECL
#if defined(_WIN32) && defined(SOKOL_DLL) && defined(SOKOL_AUDIO_IMPL)
#define SOKOL_AUDIO_API_DECL __declspec(dllexport)
#elif defined(_WIN32) && defined(SOKOL_DLL)
#define SOKOL_AUDIO_API_DECL __declspec(dllimport)
#else
#define SOKOL_AUDIO_API_DECL extern
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*
    saudio_log_item

    Log items are defined via X-Macros, and expanded to an
    enum 'saudio_log_item', and in debug mode only,
    corresponding strings.

    Used as parameter in the logging callback.
*/
#define _SAUDIO_LOG_ITEMS \
    _SAUDIO_LOGITEM_XMACRO(OK, "Ok") \
    _SAUDIO_LOGITEM_XMACRO(MALLOC_FAILED, "memory allocation failed") \
    _SAUDIO_LOGITEM_XMACRO(ALSA_SND_PCM_OPEN_FAILED, "snd_pcm_open() failed") \
    _SAUDIO_LOGITEM_XMACRO(ALSA_FLOAT_SAMPLES_NOT_SUPPORTED, "floating point sample format not supported") \
    _SAUDIO_LOGITEM_XMACRO(ALSA_REQUESTED_BUFFER_SIZE_NOT_SUPPORTED, "requested buffer size not supported") \
    _SAUDIO_LOGITEM_XMACRO(ALSA_REQUESTED_CHANNEL_COUNT_NOT_SUPPORTED, "requested channel count not supported") \
    _SAUDIO_LOGITEM_XMACRO(ALSA_SND_PCM_HW_PARAMS_SET_RATE_NEAR_FAILED, "snd_pcm_hw_params_set_rate_near() failed") \
    _SAUDIO_LOGITEM_XMACRO(ALSA_SND_PCM_HW_PARAMS_FAILED, "snd_pcm_hw_params() failed") \
    _SAUDIO_LOGITEM_XMACRO(ALSA_PTHREAD_CREATE_FAILED, "pthread_create() failed") \
    _SAUDIO_LOGITEM_XMACRO(WASAPI_CREATE_EVENT_FAILED, "CreateEvent() failed") \
    _SAUDIO_LOGITEM_XMACRO(WASAPI_CREATE_DEVICE_ENUMERATOR_FAILED, "CoCreateInstance() for IMMDeviceEnumerator failed") \
    _SAUDIO_LOGITEM_XMACRO(WASAPI_GET_DEFAULT_AUDIO_ENDPOINT_FAILED, "IMMDeviceEnumerator.GetDefaultAudioEndpoint() failed") \
    _SAUDIO_LOGITEM_XMACRO(WASAPI_DEVICE_ACTIVATE_FAILED, "IMMDevice.Activate() failed") \
    _SAUDIO_LOGITEM_XMACRO(WASAPI_AUDIO_CLIENT_INITIALIZE_FAILED, "IAudioClient.Initialize() failed") \
    _SAUDIO_LOGITEM_XMACRO(WASAPI_AUDIO_CLIENT_GET_BUFFER_SIZE_FAILED, "IAudioClient.GetBufferSize() failed") \
    _SAUDIO_LOGITEM_XMACRO(WASAPI_AUDIO_CLIENT_GET_SERVICE_FAILED, "IAudioClient.GetService() failed") \
    _SAUDIO_LOGITEM_XMACRO(WASAPI_AUDIO_CLIENT_SET_EVENT_HANDLE_FAILED, "IAudioClient.SetEventHandle() failed") \
    _SAUDIO_LOGITEM_XMACRO(WASAPI_CREATE_THREAD_FAILED, "CreateThread() failed") \
    _SAUDIO_LOGITEM_XMACRO(AAUDIO_STREAMBUILDER_OPEN_STREAM_FAILED, "AAudioStreamBuilder_openStream() failed") \
    _SAUDIO_LOGITEM_XMACRO(AAUDIO_PTHREAD_CREATE_FAILED, "pthread_create() failed after AAUDIO_ERROR_DISCONNECTED") \
    _SAUDIO_LOGITEM_XMACRO(AAUDIO_RESTARTING_STREAM_AFTER_ERROR, "restarting AAudio stream after error") \
    _SAUDIO_LOGITEM_XMACRO(USING_AAUDIO_BACKEND, "using AAudio backend") \
    _SAUDIO_LOGITEM_XMACRO(AAUDIO_CREATE_STREAMBUILDER_FAILED, "AAudio_createStreamBuilder() failed") \
    _SAUDIO_LOGITEM_XMACRO(USING_SLES_BACKEND, "using OpenSLES backend") \
    _SAUDIO_LOGITEM_XMACRO(SLES_CREATE_ENGINE_FAILED, "slCreateEngine() failed") \
    _SAUDIO_LOGITEM_XMACRO(SLES_ENGINE_GET_ENGINE_INTERFACE_FAILED, "GetInterface() for SL_IID_ENGINE failed") \
    _SAUDIO_LOGITEM_XMACRO(SLES_CREATE_OUTPUT_MIX_FAILED, "CreateOutputMix() failed") \
    _SAUDIO_LOGITEM_XMACRO(SLES_MIXER_GET_VOLUME_INTERFACE_FAILED, "GetInterface() for SL_IID_VOLUME failed") \
    _SAUDIO_LOGITEM_XMACRO(SLES_ENGINE_CREATE_AUDIO_PLAYER_FAILED, "CreateAudioPlayer() failed") \
    _SAUDIO_LOGITEM_XMACRO(SLES_PLAYER_GET_PLAY_INTERFACE_FAILED, "GetInterface() for SL_IID_PLAY failed") \
    _SAUDIO_LOGITEM_XMACRO(SLES_PLAYER_GET_VOLUME_INTERFACE_FAILED, "GetInterface() for SL_IID_VOLUME failed") \
    _SAUDIO_LOGITEM_XMACRO(SLES_PLAYER_GET_BUFFERQUEUE_INTERFACE_FAILED, "GetInterface() for SL_IID_ANDROIDSIMPLEBUFFERQUEUE failed") \
    _SAUDIO_LOGITEM_XMACRO(COREAUDIO_NEW_OUTPUT_FAILED, "AudioQueueNewOutput() failed") \
    _SAUDIO_LOGITEM_XMACRO(COREAUDIO_ALLOCATE_BUFFER_FAILED, "AudioQueueAllocateBuffer() failed") \
    _SAUDIO_LOGITEM_XMACRO(COREAUDIO_START_FAILED, "AudioQueueStart() failed") \
    _SAUDIO_LOGITEM_XMACRO(BACKEND_BUFFER_SIZE_ISNT_MULTIPLE_OF_PACKET_SIZE, "backend buffer size isn't multiple of packet size") \

#define _SAUDIO_LOGITEM_XMACRO(item,msg) SAUDIO_LOGITEM_##item,
typedef enum saudio_log_item {
    _SAUDIO_LOG_ITEMS
} saudio_log_item;
#undef _SAUDIO_LOGITEM_XMACRO

/*
    saudio_logger

    Used in saudio_desc to provide a custom logging and error reporting
    callback to sokol-audio.
*/
typedef struct saudio_logger {
    void (*func)(
        const char* tag,                // always "saudio"
        uint32_t log_level,             // 0=panic, 1=error, 2=warning, 3=info
        uint32_t log_item_id,           // SAUDIO_LOGITEM_*
        const char* message_or_null,    // a message string, may be nullptr in release mode
        uint32_t line_nr,               // line number in sokol_audio.h
        const char* filename_or_null,   // source filename, may be nullptr in release mode
        void* user_data);
    void* user_data;
} saudio_logger;

/*
    saudio_allocator

    Used in saudio_desc to provide custom memory-alloc and -free functions
    to sokol_audio.h. If memory management should be overridden, both the
    alloc_fn and free_fn function must be provided (e.g. it's not valid to
    override one function but not the other).
*/
typedef struct saudio_allocator {
    void* (*alloc_fn)(size_t size, void* user_data);
    void (*free_fn)(void* ptr, void* user_data);
    void* user_data;
} saudio_allocator;

typedef struct saudio_desc {
    int sample_rate;        // requested sample rate
    int num_channels;       // number of channels, default: 1 (mono)
    int buffer_frames;      // number of frames in streaming buffer
    int packet_frames;      // number of frames in a packet
    int num_packets;        // number of packets in packet queue
    void (*stream_cb)(float* buffer, int num_frames, int num_channels);  // optional streaming callback (no user data)
    void (*stream_userdata_cb)(float* buffer, int num_frames, int num_channels, void* user_data); //... and with user data
    void* user_data;        // optional user data argument for stream_userdata_cb
    saudio_allocator allocator;     // optional allocation override functions
    saudio_logger logger;           // optional logging function (default: NO LOGGING!)
} saudio_desc;

/* setup sokol-audio */
SOKOL_AUDIO_API_DECL void saudio_setup(const saudio_desc* desc);
/* shutdown sokol-audio */
SOKOL_AUDIO_API_DECL void saudio_shutdown(void);
/* true after setup if audio backend was successfully initialized */
SOKOL_AUDIO_API_DECL bool saudio_isvalid(void);
/* return the saudio_desc.user_data pointer */
SOKOL_AUDIO_API_DECL void* saudio_userdata(void);
/* return a copy of the original saudio_desc struct */
SOKOL_AUDIO_API_DECL saudio_desc saudio_query_desc(void);
/* actual sample rate */
SOKOL_AUDIO_API_DECL int saudio_sample_rate(void);
/* return actual backend buffer size in number of frames */
SOKOL_AUDIO_API_DECL int saudio_buffer_frames(void);
/* actual number of channels */
SOKOL_AUDIO_API_DECL int saudio_channels(void);
/* return true if audio context is currently suspended (only in WebAudio backend, all other backends return false) */
SOKOL_AUDIO_API_DECL bool saudio_suspended(void);
/* get current number of frames to fill packet queue */
SOKOL_AUDIO_API_DECL int saudio_expect(void);
/* push sample frames from main thread, returns number of frames actually pushed */
SOKOL_AUDIO_API_DECL int saudio_push(const float* frames, int num_frames);

#ifdef __cplusplus
} /* extern "C" */

/* reference-based equivalents for c++ */
inline void saudio_setup(const saudio_desc& desc) { return saudio_setup(&desc); }

#endif
#endif // SOKOL_AUDIO_INCLUDED

// ██ ███    ███ ██████  ██      ███████ ███    ███ ███████ ███    ██ ████████  █████  ████████ ██  ██████  ███    ██
// ██ ████  ████ ██   ██ ██      ██      ████  ████ ██      ████   ██    ██    ██   ██    ██    ██ ██    ██ ████   ██
// ██ ██ ████ ██ ██████  ██      █████   ██ ████ ██ █████   ██ ██  ██    ██    ███████    ██    ██ ██    ██ ██ ██  ██
// ██ ██  ██  ██ ██      ██      ██      ██  ██  ██ ██      ██  ██ ██    ██    ██   ██    ██    ██ ██    ██ ██  ██ ██
// ██ ██      ██ ██      ███████ ███████ ██      ██ ███████ ██   ████    ██    ██   ██    ██    ██  ██████  ██   ████
//
// >>implementation
#ifdef SOKOL_AUDIO_IMPL
#define SOKOL_AUDIO_IMPL_INCLUDED (1)

#if defined(SOKOL_MALLOC) || defined(SOKOL_CALLOC) || defined(SOKOL_FREE)
#error "SOKOL_MALLOC/CALLOC/FREE macros are no longer supported, please use saudio_desc.allocator to override memory allocation functions"
#endif

#include <stdlib.h> // alloc, free
#include <string.h> // memset, memcpy
#include <stddef.h> // size_t

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

// platform detection defines
#if defined(SOKOL_DUMMY_BACKEND)
    // nothing
#elif defined(__APPLE__)
    #define _SAUDIO_APPLE (1)
    #include <TargetConditionals.h>
    #if defined(TARGET_OS_IPHONE) && TARGET_OS_IPHONE
        #define _SAUDIO_IOS (1)
    #else
        #define _SAUDIO_MACOS (1)
    #endif
#elif defined(__EMSCRIPTEN__)
    #define _SAUDIO_EMSCRIPTEN (1)
#elif defined(_WIN32)
    #define _SAUDIO_WINDOWS (1)
    #include <winapifamily.h>
    #if (defined(WINAPI_FAMILY_PARTITION) && !WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP))
        #error "sokol_audio.h no longer supports UWP"
    #endif
#elif defined(__ANDROID__)
    #define _SAUDIO_ANDROID (1)
    #if !defined(SAUDIO_ANDROID_SLES) && !defined(SAUDIO_ANDROID_AAUDIO)
        #define SAUDIO_ANDROID_AAUDIO (1)
    #endif
#elif defined(__linux__) || defined(__unix__)
    #define _SAUDIO_LINUX (1)
#else
#error "sokol_audio.h: Unknown platform"
#endif

// platform-specific headers and definitions
#if defined(SOKOL_DUMMY_BACKEND)
    #define _SAUDIO_NOTHREADS (1)
#elif defined(_SAUDIO_WINDOWS)
    #define _SAUDIO_WINTHREADS (1)
    #ifndef WIN32_LEAN_AND_MEAN
    #define WIN32_LEAN_AND_MEAN
    #endif
    #ifndef NOMINMAX
    #define NOMINMAX
    #endif
    #include <windows.h>
    #include <synchapi.h>
    #pragma comment (lib, "kernel32")
    #pragma comment (lib, "ole32")
    #ifndef CINTERFACE
    #define CINTERFACE
    #endif
    #ifndef COBJMACROS
    #define COBJMACROS
    #endif
    #ifndef CONST_VTABLE
    #define CONST_VTABLE
    #endif
    #include <mmdeviceapi.h>
    #include <audioclient.h>
    static const IID _saudio_IID_IAudioClient                               = { 0x1cb9ad4c, 0xdbfa, 0x4c32, {0xb1, 0x78, 0xc2, 0xf5, 0x68, 0xa7, 0x03, 0xb2} };
    static const IID _saudio_IID_IMMDeviceEnumerator                        = { 0xa95664d2, 0x9614, 0x4f35, {0xa7, 0x46, 0xde, 0x8d, 0xb6, 0x36, 0x17, 0xe6} };
    static const CLSID _saudio_CLSID_IMMDeviceEnumerator                    = { 0xbcde0395, 0xe52f, 0x467c, {0x8e, 0x3d, 0xc4, 0x57, 0x92, 0x91, 0x69, 0x2e} };
    static const IID _saudio_IID_IAudioRenderClient                         = { 0xf294acfc, 0x3146, 0x4483, {0xa7, 0xbf, 0xad, 0xdc, 0xa7, 0xc2, 0x60, 0xe2} };
    static const IID _saudio_IID_Devinterface_Audio_Render                  = { 0xe6327cad, 0xdcec, 0x4949, {0xae, 0x8a, 0x99, 0x1e, 0x97, 0x6a, 0x79, 0xd2} };
    static const IID _saudio_IID_IActivateAudioInterface_Completion_Handler = { 0x94ea2b94, 0xe9cc, 0x49e0, {0xc0, 0xff, 0xee, 0x64, 0xca, 0x8f, 0x5b, 0x90} };
    static const GUID _saudio_KSDATAFORMAT_SUBTYPE_IEEE_FLOAT               = { 0x00000003, 0x0000, 0x0010, {0x80, 0x00, 0x00, 0xaa, 0x00, 0x38, 0x9b, 0x71} };
    #if defined(__cplusplus)
    #define _SOKOL_AUDIO_WIN32COM_ID(x) (x)
    #else
    #define _SOKOL_AUDIO_WIN32COM_ID(x) (&x)
    #endif
    /* fix for Visual Studio 2015 SDKs */
    #ifndef AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM
    #define AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM 0x80000000
    #endif
    #ifndef AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY
    #define AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY 0x08000000
    #endif
    #ifdef _MSC_VER
        #pragma warning(push)
        #pragma warning(disable:4505)   /* unreferenced local function has been removed */
    #endif
#elif defined(_SAUDIO_APPLE)
    #define _SAUDIO_PTHREADS (1)
    #include <pthread.h>
    #if defined(_SAUDIO_IOS)
        // always use system headers on iOS (for now at least)
        #if !defined(SAUDIO_OSX_USE_SYSTEM_HEADERS)
            #define SAUDIO_OSX_USE_SYSTEM_HEADERS (1)
        #endif
        #if !defined(__cplusplus)
            #if __has_feature(objc_arc) && !__has_feature(objc_arc_fields)
                #error "sokol_audio.h on iOS requires __has_feature(objc_arc_field) if ARC is enabled (use a more recent compiler version)"
            #endif
        #endif
        #include <AudioToolbox/AudioToolbox.h>
        #include <AVFoundation/AVFoundation.h>
    #else
        #if defined(SAUDIO_OSX_USE_SYSTEM_HEADERS)
            #include <AudioToolbox/AudioToolbox.h>
        #endif
    #endif
#elif defined(_SAUDIO_ANDROID)
    #define _SAUDIO_PTHREADS (1)
    #include <pthread.h>
    #if defined(SAUDIO_ANDROID_SLES)
        #include "SLES/OpenSLES_Android.h"
    #elif defined(SAUDIO_ANDROID_AAUDIO)
        #include "aaudio/AAudio.h"
    #endif
#elif defined(_SAUDIO_LINUX)
    #include <alloca.h>
    #define _SAUDIO_PTHREADS (1)
    #include <pthread.h>
    #define ALSA_PCM_NEW_HW_PARAMS_API
    #include <alsa/asoundlib.h>
#elif defined(__EMSCRIPTEN__)
    #define _SAUDIO_NOTHREADS (1)
    #include <emscripten/emscripten.h>
#endif

#define _saudio_def(val, def) (((val) == 0) ? (def) : (val))
#define _saudio_def_flt(val, def) (((val) == 0.0f) ? (def) : (val))

#define _SAUDIO_DEFAULT_SAMPLE_RATE (44100)
#define _SAUDIO_DEFAULT_BUFFER_FRAMES (2048)
#define _SAUDIO_DEFAULT_PACKET_FRAMES (128)
#define _SAUDIO_DEFAULT_NUM_PACKETS ((_SAUDIO_DEFAULT_BUFFER_FRAMES/_SAUDIO_DEFAULT_PACKET_FRAMES)*4)

#ifndef SAUDIO_RING_MAX_SLOTS
#define SAUDIO_RING_MAX_SLOTS (1024)
#endif

// ███████ ████████ ██████  ██    ██  ██████ ████████ ███████
// ██         ██    ██   ██ ██    ██ ██         ██    ██
// ███████    ██    ██████  ██    ██ ██         ██    ███████
//      ██    ██    ██   ██ ██    ██ ██         ██         ██
// ███████    ██    ██   ██  ██████   ██████    ██    ███████
//
// >>structs
#if defined(_SAUDIO_PTHREADS)

typedef struct {
    pthread_mutex_t mutex;
} _saudio_mutex_t;

#elif defined(_SAUDIO_WINTHREADS)

typedef struct {
    CRITICAL_SECTION critsec;
} _saudio_mutex_t;

#elif defined(_SAUDIO_NOTHREADS)

typedef struct {
    int dummy_mutex;
} _saudio_mutex_t;

#endif

#if defined(SOKOL_DUMMY_BACKEND)

typedef struct {
    int dummy;
} _saudio_dummy_backend_t;

#elif defined(_SAUDIO_APPLE)

#if defined(SAUDIO_OSX_USE_SYSTEM_HEADERS)

typedef AudioQueueRef _saudio_AudioQueueRef;
typedef AudioQueueBufferRef _saudio_AudioQueueBufferRef;
typedef AudioStreamBasicDescription _saudio_AudioStreamBasicDescription;
typedef OSStatus _saudio_OSStatus;

#define _saudio_kAudioFormatLinearPCM (kAudioFormatLinearPCM)
#define _saudio_kLinearPCMFormatFlagIsFloat (kLinearPCMFormatFlagIsFloat)
#define _saudio_kAudioFormatFlagIsPacked (kAudioFormatFlagIsPacked)

#else
#ifdef __cplusplus
extern "C" {
#endif

// embedded AudioToolbox declarations
typedef uint32_t _saudio_AudioFormatID;
typedef uint32_t _saudio_AudioFormatFlags;
typedef int32_t _saudio_OSStatus;
typedef uint32_t _saudio_SMPTETimeType;
typedef uint32_t _saudio_SMPTETimeFlags;
typedef uint32_t _saudio_AudioTimeStampFlags;
typedef void* _saudio_CFRunLoopRef;
typedef void* _saudio_CFStringRef;
typedef void* _saudio_AudioQueueRef;

#define _saudio_kAudioFormatLinearPCM ('lpcm')
#define _saudio_kLinearPCMFormatFlagIsFloat (1U << 0)
#define _saudio_kAudioFormatFlagIsPacked (1U << 3)

typedef struct _saudio_AudioStreamBasicDescription {
    double mSampleRate;
    _saudio_AudioFormatID mFormatID;
    _saudio_AudioFormatFlags mFormatFlags;
    uint32_t mBytesPerPacket;
    uint32_t mFramesPerPacket;
    uint32_t mBytesPerFrame;
    uint32_t mChannelsPerFrame;
    uint32_t mBitsPerChannel;
    uint32_t mReserved;
} _saudio_AudioStreamBasicDescription;

typedef struct _saudio_AudioStreamPacketDescription {
    int64_t mStartOffset;
    uint32_t mVariableFramesInPacket;
    uint32_t mDataByteSize;
} _saudio_AudioStreamPacketDescription;

typedef struct _saudio_SMPTETime {
    int16_t mSubframes;
    int16_t mSubframeDivisor;
    uint32_t mCounter;
    _saudio_SMPTETimeType mType;
    _saudio_SMPTETimeFlags mFlags;
    int16_t mHours;
    int16_t mMinutes;
    int16_t mSeconds;
    int16_t mFrames;
} _saudio_SMPTETime;

typedef struct _saudio_AudioTimeStamp {
    double mSampleTime;
    uint64_t mHostTime;
    double mRateScalar;
    uint64_t mWordClockTime;
    _saudio_SMPTETime mSMPTETime;
    _saudio_AudioTimeStampFlags mFlags;
    uint32_t mReserved;
} _saudio_AudioTimeStamp;

typedef struct _saudio_AudioQueueBuffer {
    const uint32_t mAudioDataBytesCapacity;
    void* const mAudioData;
    uint32_t mAudioDataByteSize;
    void * mUserData;
    const uint32_t mPacketDescriptionCapacity;
    _saudio_AudioStreamPacketDescription* const mPacketDescriptions;
    uint32_t mPacketDescriptionCount;
} _saudio_AudioQueueBuffer;
typedef _saudio_AudioQueueBuffer* _saudio_AudioQueueBufferRef;

typedef void (*_saudio_AudioQueueOutputCallback)(void* user_data, _saudio_AudioQueueRef inAQ, _saudio_AudioQueueBufferRef inBuffer);

extern _saudio_OSStatus AudioQueueNewOutput(const _saudio_AudioStreamBasicDescription* inFormat, _saudio_AudioQueueOutputCallback inCallbackProc, void* inUserData, _saudio_CFRunLoopRef inCallbackRunLoop, _saudio_CFStringRef inCallbackRunLoopMode, uint32_t inFlags, _saudio_AudioQueueRef* outAQ);
extern _saudio_OSStatus AudioQueueDispose(_saudio_AudioQueueRef inAQ, bool inImmediate);
extern _saudio_OSStatus AudioQueueAllocateBuffer(_saudio_AudioQueueRef inAQ, uint32_t inBufferByteSize, _saudio_AudioQueueBufferRef* outBuffer);
extern _saudio_OSStatus AudioQueueEnqueueBuffer(_saudio_AudioQueueRef inAQ, _saudio_AudioQueueBufferRef inBuffer, uint32_t inNumPacketDescs, const _saudio_AudioStreamPacketDescription* inPacketDescs);
extern _saudio_OSStatus AudioQueueStart(_saudio_AudioQueueRef inAQ, const _saudio_AudioTimeStamp * inStartTime);
extern _saudio_OSStatus AudioQueueStop(_saudio_AudioQueueRef inAQ, bool inImmediate);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // SAUDIO_OSX_USE_SYSTEM_HEADERS

typedef struct {
    _saudio_AudioQueueRef ca_audio_queue;
    #if defined(_SAUDIO_IOS)
    id ca_interruption_handler;
    #endif
} _saudio_apple_backend_t;

#elif defined(_SAUDIO_LINUX)

typedef struct {
    snd_pcm_t* device;
    float* buffer;
    int buffer_byte_size;
    int buffer_frames;
    pthread_t thread;
    bool thread_stop;
} _saudio_alsa_backend_t;

#elif defined(SAUDIO_ANDROID_SLES)

#define SAUDIO_SLES_NUM_BUFFERS (2)

typedef struct {
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    int count;
} _saudio_sles_semaphore_t;

typedef struct {
    SLObjectItf engine_obj;
    SLEngineItf engine;
    SLObjectItf output_mix_obj;
    SLVolumeItf output_mix_vol;
    SLDataLocator_OutputMix out_locator;
    SLDataSink dst_data_sink;
    SLObjectItf player_obj;
    SLPlayItf player;
    SLVolumeItf player_vol;
    SLAndroidSimpleBufferQueueItf player_buffer_queue;

    int16_t* output_buffers[SAUDIO_SLES_NUM_BUFFERS];
    float* src_buffer;
    int active_buffer;
    _saudio_sles_semaphore_t buffer_sem;
    pthread_t thread;
    volatile int thread_stop;
    SLDataLocator_AndroidSimpleBufferQueue in_locator;
} _saudio_sles_backend_t;

#elif defined(SAUDIO_ANDROID_AAUDIO)

typedef struct {
    AAudioStreamBuilder* builder;
    AAudioStream* stream;
    pthread_t thread;
    pthread_mutex_t mutex;
} _saudio_aaudio_backend_t;

#elif defined(_SAUDIO_WINDOWS)

typedef struct {
    HANDLE thread_handle;
    HANDLE buffer_end_event;
    bool stop;
    UINT32 dst_buffer_frames;
    int src_buffer_frames;
    int src_buffer_byte_size;
    int src_buffer_pos;
    float* src_buffer;
} _saudio_wasapi_thread_data_t;

typedef struct {
    IMMDeviceEnumerator* device_enumerator;
    IMMDevice* device;
    IAudioClient* audio_client;
    IAudioRenderClient* render_client;
    _saudio_wasapi_thread_data_t thread;
} _saudio_wasapi_backend_t;

#elif defined(_SAUDIO_EMSCRIPTEN)

typedef struct {
    uint8_t* buffer;
} _saudio_web_backend_t;

#else
#error "unknown platform"
#endif

#if defined(SOKOL_DUMMY_BACKEND)
typedef _saudio_dummy_backend_t _saudio_backend_t;
#elif defined(_SAUDIO_APPLE)
typedef _saudio_apple_backend_t _saudio_backend_t;
#elif defined(_SAUDIO_EMSCRIPTEN)
typedef _saudio_web_backend_t _saudio_backend_t;
#elif defined(_SAUDIO_WINDOWS)
typedef _saudio_wasapi_backend_t _saudio_backend_t;
#elif defined(SAUDIO_ANDROID_SLES)
typedef _saudio_sles_backend_t _saudio_backend_t;
#elif defined(SAUDIO_ANDROID_AAUDIO)
typedef _saudio_aaudio_backend_t _saudio_backend_t;
#elif defined(_SAUDIO_LINUX)
typedef _saudio_alsa_backend_t _saudio_backend_t;
#endif

/* a ringbuffer structure */
typedef struct {
    int head;  // next slot to write to
    int tail;  // next slot to read from
    int num;   // number of slots in queue
    int queue[SAUDIO_RING_MAX_SLOTS];
} _saudio_ring_t;

/* a packet FIFO structure */
typedef struct {
    bool valid;
    int packet_size;            /* size of a single packets in bytes(!) */
    int num_packets;            /* number of packet in fifo */
    uint8_t* base_ptr;          /* packet memory chunk base pointer (dynamically allocated) */
    int cur_packet;             /* current write-packet */
    int cur_offset;             /* current byte-offset into current write packet */
    _saudio_mutex_t mutex;      /* mutex for thread-safe access */
    _saudio_ring_t read_queue;  /* buffers with data, ready to be streamed */
    _saudio_ring_t write_queue; /* empty buffers, ready to be pushed to */
} _saudio_fifo_t;

/* sokol-audio state */
typedef struct {
    bool valid;
    void (*stream_cb)(float* buffer, int num_frames, int num_channels);
    void (*stream_userdata_cb)(float* buffer, int num_frames, int num_channels, void* user_data);
    void* user_data;
    int sample_rate;            /* sample rate */
    int buffer_frames;          /* number of frames in streaming buffer */
    int bytes_per_frame;        /* filled by backend */
    int packet_frames;          /* number of frames in a packet */
    int num_packets;            /* number of packets in packet queue */
    int num_channels;           /* actual number of channels */
    saudio_desc desc;
    _saudio_fifo_t fifo;
    _saudio_backend_t backend;
} _saudio_state_t;

_SOKOL_PRIVATE _saudio_state_t _saudio;

_SOKOL_PRIVATE bool _saudio_has_callback(void) {
    return (_saudio.stream_cb || _saudio.stream_userdata_cb);
}

_SOKOL_PRIVATE void _saudio_stream_callback(float* buffer, int num_frames, int num_channels) {
    if (_saudio.stream_cb) {
        _saudio.stream_cb(buffer, num_frames, num_channels);
    }
    else if (_saudio.stream_userdata_cb) {
        _saudio.stream_userdata_cb(buffer, num_frames, num_channels, _saudio.user_data);
    }
}

// ██       ██████   ██████   ██████  ██ ███    ██  ██████
// ██      ██    ██ ██       ██       ██ ████   ██ ██
// ██      ██    ██ ██   ███ ██   ███ ██ ██ ██  ██ ██   ███
// ██      ██    ██ ██    ██ ██    ██ ██ ██  ██ ██ ██    ██
// ███████  ██████   ██████   ██████  ██ ██   ████  ██████
//
// >>logging
#if defined(SOKOL_DEBUG)
#define _SAUDIO_LOGITEM_XMACRO(item,msg) #item ": " msg,
static const char* _saudio_log_messages[] = {
    _SAUDIO_LOG_ITEMS
};
#undef _SAUDIO_LOGITEM_XMACRO
#endif // SOKOL_DEBUG

#define _SAUDIO_PANIC(code) _saudio_log(SAUDIO_LOGITEM_ ##code, 0, __LINE__)
#define _SAUDIO_ERROR(code) _saudio_log(SAUDIO_LOGITEM_ ##code, 1, __LINE__)
#define _SAUDIO_WARN(code) _saudio_log(SAUDIO_LOGITEM_ ##code, 2, __LINE__)
#define _SAUDIO_INFO(code) _saudio_log(SAUDIO_LOGITEM_ ##code, 3, __LINE__)

static void _saudio_log(saudio_log_item log_item, uint32_t log_level, uint32_t line_nr) {
    if (_saudio.desc.logger.func) {
        #if defined(SOKOL_DEBUG)
            const char* filename = __FILE__;
            const char* message = _saudio_log_messages[log_item];
        #else
            const char* filename = 0;
            const char* message = 0;
        #endif
        _saudio.desc.logger.func("saudio", log_level, log_item, message, line_nr, filename, _saudio.desc.logger.user_data);
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
_SOKOL_PRIVATE void _saudio_clear(void* ptr, size_t size) {
    SOKOL_ASSERT(ptr && (size > 0));
    memset(ptr, 0, size);
}

_SOKOL_PRIVATE void* _saudio_malloc(size_t size) {
    SOKOL_ASSERT(size > 0);
    void* ptr;
    if (_saudio.desc.allocator.alloc_fn) {
        ptr = _saudio.desc.allocator.alloc_fn(size, _saudio.desc.allocator.user_data);
    } else {
        ptr = malloc(size);
    }
    if (0 == ptr) {
        _SAUDIO_PANIC(MALLOC_FAILED);
    }
    return ptr;
}

_SOKOL_PRIVATE void* _saudio_malloc_clear(size_t size) {
    void* ptr = _saudio_malloc(size);
    _saudio_clear(ptr, size);
    return ptr;
}

_SOKOL_PRIVATE void _saudio_free(void* ptr) {
    if (_saudio.desc.allocator.free_fn) {
        _saudio.desc.allocator.free_fn(ptr, _saudio.desc.allocator.user_data);
    } else {
        free(ptr);
    }
}

// ███    ███ ██    ██ ████████ ███████ ██   ██
// ████  ████ ██    ██    ██    ██       ██ ██
// ██ ████ ██ ██    ██    ██    █████     ███
// ██  ██  ██ ██    ██    ██    ██       ██ ██
// ██      ██  ██████     ██    ███████ ██   ██
//
// >>mutex
#if defined(_SAUDIO_NOTHREADS)

_SOKOL_PRIVATE void _saudio_mutex_init(_saudio_mutex_t* m) { (void)m; }
_SOKOL_PRIVATE void _saudio_mutex_destroy(_saudio_mutex_t* m) { (void)m; }
_SOKOL_PRIVATE void _saudio_mutex_lock(_saudio_mutex_t* m) { (void)m; }
_SOKOL_PRIVATE void _saudio_mutex_unlock(_saudio_mutex_t* m) { (void)m; }

#elif defined(_SAUDIO_PTHREADS)

_SOKOL_PRIVATE void _saudio_mutex_init(_saudio_mutex_t* m) {
    pthread_mutexattr_t attr;
    pthread_mutexattr_init(&attr);
    pthread_mutex_init(&m->mutex, &attr);
}

_SOKOL_PRIVATE void _saudio_mutex_destroy(_saudio_mutex_t* m) {
    pthread_mutex_destroy(&m->mutex);
}

_SOKOL_PRIVATE void _saudio_mutex_lock(_saudio_mutex_t* m) {
    pthread_mutex_lock(&m->mutex);
}

_SOKOL_PRIVATE void _saudio_mutex_unlock(_saudio_mutex_t* m) {
    pthread_mutex_unlock(&m->mutex);
}

#elif defined(_SAUDIO_WINTHREADS)

_SOKOL_PRIVATE void _saudio_mutex_init(_saudio_mutex_t* m) {
    InitializeCriticalSection(&m->critsec);
}

_SOKOL_PRIVATE void _saudio_mutex_destroy(_saudio_mutex_t* m) {
    DeleteCriticalSection(&m->critsec);
}

_SOKOL_PRIVATE void _saudio_mutex_lock(_saudio_mutex_t* m) {
    EnterCriticalSection(&m->critsec);
}

_SOKOL_PRIVATE void _saudio_mutex_unlock(_saudio_mutex_t* m) {
    LeaveCriticalSection(&m->critsec);
}
#else
#error "sokol_audio.h: unknown platform!"
#endif

// ██████  ██ ███    ██  ██████  ██████  ██    ██ ███████ ███████ ███████ ██████
// ██   ██ ██ ████   ██ ██       ██   ██ ██    ██ ██      ██      ██      ██   ██
// ██████  ██ ██ ██  ██ ██   ███ ██████  ██    ██ █████   █████   █████   ██████
// ██   ██ ██ ██  ██ ██ ██    ██ ██   ██ ██    ██ ██      ██      ██      ██   ██
// ██   ██ ██ ██   ████  ██████  ██████   ██████  ██      ██      ███████ ██   ██
//
// >>ringbuffer
_SOKOL_PRIVATE int _saudio_ring_idx(_saudio_ring_t* ring, int i) {
    return (i % ring->num);
}

_SOKOL_PRIVATE void _saudio_ring_init(_saudio_ring_t* ring, int num_slots) {
    SOKOL_ASSERT((num_slots + 1) <= SAUDIO_RING_MAX_SLOTS);
    ring->head = 0;
    ring->tail = 0;
    /* one slot reserved to detect 'full' vs 'empty' */
    ring->num = num_slots + 1;
}

_SOKOL_PRIVATE bool _saudio_ring_full(_saudio_ring_t* ring) {
    return _saudio_ring_idx(ring, ring->head + 1) == ring->tail;
}

_SOKOL_PRIVATE bool _saudio_ring_empty(_saudio_ring_t* ring) {
    return ring->head == ring->tail;
}

_SOKOL_PRIVATE int _saudio_ring_count(_saudio_ring_t* ring) {
    int count;
    if (ring->head >= ring->tail) {
        count = ring->head - ring->tail;
    }
    else {
        count = (ring->head + ring->num) - ring->tail;
    }
    SOKOL_ASSERT(count < ring->num);
    return count;
}

_SOKOL_PRIVATE void _saudio_ring_enqueue(_saudio_ring_t* ring, int val) {
    SOKOL_ASSERT(!_saudio_ring_full(ring));
    ring->queue[ring->head] = val;
    ring->head = _saudio_ring_idx(ring, ring->head + 1);
}

_SOKOL_PRIVATE int _saudio_ring_dequeue(_saudio_ring_t* ring) {
    SOKOL_ASSERT(!_saudio_ring_empty(ring));
    int val = ring->queue[ring->tail];
    ring->tail = _saudio_ring_idx(ring, ring->tail + 1);
    return val;
}

// ███████ ██ ███████  ██████
// ██      ██ ██      ██    ██
// █████   ██ █████   ██    ██
// ██      ██ ██      ██    ██
// ██      ██ ██       ██████
//
// >>fifo
_SOKOL_PRIVATE void _saudio_fifo_init_mutex(_saudio_fifo_t* fifo) {
    /* this must be called before initializing both the backend and the fifo itself! */
    _saudio_mutex_init(&fifo->mutex);
}

_SOKOL_PRIVATE void _saudio_fifo_destroy_mutex(_saudio_fifo_t* fifo) {
    _saudio_mutex_destroy(&fifo->mutex);
}

_SOKOL_PRIVATE void _saudio_fifo_init(_saudio_fifo_t* fifo, int packet_size, int num_packets) {
    /* NOTE: there's a chicken-egg situation during the init phase where the
        streaming thread must be started before the fifo is actually initialized,
        thus the fifo init must already be protected from access by the fifo_read() func.
    */
    _saudio_mutex_lock(&fifo->mutex);
    SOKOL_ASSERT((packet_size > 0) && (num_packets > 0));
    fifo->packet_size = packet_size;
    fifo->num_packets = num_packets;
    fifo->base_ptr = (uint8_t*) _saudio_malloc((size_t)(packet_size * num_packets));
    fifo->cur_packet = -1;
    fifo->cur_offset = 0;
    _saudio_ring_init(&fifo->read_queue, num_packets);
    _saudio_ring_init(&fifo->write_queue, num_packets);
    for (int i = 0; i < num_packets; i++) {
        _saudio_ring_enqueue(&fifo->write_queue, i);
    }
    SOKOL_ASSERT(_saudio_ring_full(&fifo->write_queue));
    SOKOL_ASSERT(_saudio_ring_count(&fifo->write_queue) == num_packets);
    SOKOL_ASSERT(_saudio_ring_empty(&fifo->read_queue));
    SOKOL_ASSERT(_saudio_ring_count(&fifo->read_queue) == 0);
    fifo->valid = true;
    _saudio_mutex_unlock(&fifo->mutex);
}

_SOKOL_PRIVATE void _saudio_fifo_shutdown(_saudio_fifo_t* fifo) {
    SOKOL_ASSERT(fifo->base_ptr);
    _saudio_free(fifo->base_ptr);
    fifo->base_ptr = 0;
    fifo->valid = false;
}

_SOKOL_PRIVATE int _saudio_fifo_writable_bytes(_saudio_fifo_t* fifo) {
    _saudio_mutex_lock(&fifo->mutex);
    int num_bytes = (_saudio_ring_count(&fifo->write_queue) * fifo->packet_size);
    if (fifo->cur_packet != -1) {
        num_bytes += fifo->packet_size - fifo->cur_offset;
    }
    _saudio_mutex_unlock(&fifo->mutex);
    SOKOL_ASSERT((num_bytes >= 0) && (num_bytes <= (fifo->num_packets * fifo->packet_size)));
    return num_bytes;
}

/* write new data to the write queue, this is called from main thread */
_SOKOL_PRIVATE int _saudio_fifo_write(_saudio_fifo_t* fifo, const uint8_t* ptr, int num_bytes) {
    /* returns the number of bytes written, this will be smaller then requested
        if the write queue runs full
    */
    int all_to_copy = num_bytes;
    while (all_to_copy > 0) {
        /* need to grab a new packet? */
        if (fifo->cur_packet == -1) {
            _saudio_mutex_lock(&fifo->mutex);
            if (!_saudio_ring_empty(&fifo->write_queue)) {
                fifo->cur_packet = _saudio_ring_dequeue(&fifo->write_queue);
            }
            _saudio_mutex_unlock(&fifo->mutex);
            SOKOL_ASSERT(fifo->cur_offset == 0);
        }
        /* append data to current write packet */
        if (fifo->cur_packet != -1) {
            int to_copy = all_to_copy;
            const int max_copy = fifo->packet_size - fifo->cur_offset;
            if (to_copy > max_copy) {
                to_copy = max_copy;
            }
            uint8_t* dst = fifo->base_ptr + fifo->cur_packet * fifo->packet_size + fifo->cur_offset;
            memcpy(dst, ptr, (size_t)to_copy);
            ptr += to_copy;
            fifo->cur_offset += to_copy;
            all_to_copy -= to_copy;
            SOKOL_ASSERT(fifo->cur_offset <= fifo->packet_size);
            SOKOL_ASSERT(all_to_copy >= 0);
        }
        else {
            /* early out if we're starving */
            int bytes_copied = num_bytes - all_to_copy;
            SOKOL_ASSERT((bytes_copied >= 0) && (bytes_copied < num_bytes));
            return bytes_copied;
        }
        /* if write packet is full, push to read queue */
        if (fifo->cur_offset == fifo->packet_size) {
            _saudio_mutex_lock(&fifo->mutex);
            _saudio_ring_enqueue(&fifo->read_queue, fifo->cur_packet);
            _saudio_mutex_unlock(&fifo->mutex);
            fifo->cur_packet = -1;
            fifo->cur_offset = 0;
        }
    }
    SOKOL_ASSERT(all_to_copy == 0);
    return num_bytes;
}

/* read queued data, this is called form the stream callback (maybe separate thread) */
_SOKOL_PRIVATE int _saudio_fifo_read(_saudio_fifo_t* fifo, uint8_t* ptr, int num_bytes) {
    /* NOTE: fifo_read might be called before the fifo is properly initialized */
    _saudio_mutex_lock(&fifo->mutex);
    int num_bytes_copied = 0;
    if (fifo->valid) {
        SOKOL_ASSERT(0 == (num_bytes % fifo->packet_size));
        SOKOL_ASSERT(num_bytes <= (fifo->packet_size * fifo->num_packets));
        const int num_packets_needed = num_bytes / fifo->packet_size;
        uint8_t* dst = ptr;
        /* either pull a full buffer worth of data, or nothing */
        if (_saudio_ring_count(&fifo->read_queue) >= num_packets_needed) {
            for (int i = 0; i < num_packets_needed; i++) {
                int packet_index = _saudio_ring_dequeue(&fifo->read_queue);
                _saudio_ring_enqueue(&fifo->write_queue, packet_index);
                const uint8_t* src = fifo->base_ptr + packet_index * fifo->packet_size;
                memcpy(dst, src, (size_t)fifo->packet_size);
                dst += fifo->packet_size;
                num_bytes_copied += fifo->packet_size;
            }
            SOKOL_ASSERT(num_bytes == num_bytes_copied);
        }
    }
    _saudio_mutex_unlock(&fifo->mutex);
    return num_bytes_copied;
}

// ██████  ██    ██ ███    ███ ███    ███ ██    ██
// ██   ██ ██    ██ ████  ████ ████  ████  ██  ██
// ██   ██ ██    ██ ██ ████ ██ ██ ████ ██   ████
// ██   ██ ██    ██ ██  ██  ██ ██  ██  ██    ██
// ██████   ██████  ██      ██ ██      ██    ██
//
// >>dummy
#if defined(SOKOL_DUMMY_BACKEND)
_SOKOL_PRIVATE bool _saudio_dummy_backend_init(void) {
    _saudio.bytes_per_frame = _saudio.num_channels * (int)sizeof(float);
    return true;
};
_SOKOL_PRIVATE void _saudio_dummy_backend_shutdown(void) { };

//  █████  ██      ███████  █████
// ██   ██ ██      ██      ██   ██
// ███████ ██      ███████ ███████
// ██   ██ ██           ██ ██   ██
// ██   ██ ███████ ███████ ██   ██
//
// >>alsa
#elif defined(_SAUDIO_LINUX)

/* the streaming callback runs in a separate thread */
_SOKOL_PRIVATE void* _saudio_alsa_cb(void* param) {
    _SOKOL_UNUSED(param);
    while (!_saudio.backend.thread_stop) {
        /* snd_pcm_writei() will be blocking until it needs data */
        int write_res = snd_pcm_writei(_saudio.backend.device, _saudio.backend.buffer, (snd_pcm_uframes_t)_saudio.backend.buffer_frames);
        if (write_res < 0) {
            /* underrun occurred */
            snd_pcm_prepare(_saudio.backend.device);
        }
        else {
            /* fill the streaming buffer with new data */
            if (_saudio_has_callback()) {
                _saudio_stream_callback(_saudio.backend.buffer, _saudio.backend.buffer_frames, _saudio.num_channels);
            }
            else {
                if (0 == _saudio_fifo_read(&_saudio.fifo, (uint8_t*)_saudio.backend.buffer, _saudio.backend.buffer_byte_size)) {
                    /* not enough read data available, fill the entire buffer with silence */
                    _saudio_clear(_saudio.backend.buffer, (size_t)_saudio.backend.buffer_byte_size);
                }
            }
        }
    }
    return 0;
}

_SOKOL_PRIVATE bool _saudio_alsa_backend_init(void) {
    int dir; uint32_t rate;
    int rc = snd_pcm_open(&_saudio.backend.device, "default", SND_PCM_STREAM_PLAYBACK, 0);
    if (rc < 0) {
        _SAUDIO_ERROR(ALSA_SND_PCM_OPEN_FAILED);
        return false;
    }

    /* configuration works by restricting the 'configuration space' step
       by step, we require all parameters except the sample rate to
       match perfectly
    */
    snd_pcm_hw_params_t* params = 0;
    snd_pcm_hw_params_alloca(&params);
    snd_pcm_hw_params_any(_saudio.backend.device, params);
    snd_pcm_hw_params_set_access(_saudio.backend.device, params, SND_PCM_ACCESS_RW_INTERLEAVED);
    if (0 > snd_pcm_hw_params_set_format(_saudio.backend.device, params, SND_PCM_FORMAT_FLOAT_LE)) {
        _SAUDIO_ERROR(ALSA_FLOAT_SAMPLES_NOT_SUPPORTED);
        goto error;
    }
    if (0 > snd_pcm_hw_params_set_buffer_size(_saudio.backend.device, params, (snd_pcm_uframes_t)_saudio.buffer_frames)) {
        _SAUDIO_ERROR(ALSA_REQUESTED_BUFFER_SIZE_NOT_SUPPORTED);
        goto error;
    }
    if (0 > snd_pcm_hw_params_set_channels(_saudio.backend.device, params, (uint32_t)_saudio.num_channels)) {
        _SAUDIO_ERROR(ALSA_REQUESTED_CHANNEL_COUNT_NOT_SUPPORTED);
        goto error;
    }
    /* let ALSA pick a nearby sampling rate */
    rate = (uint32_t) _saudio.sample_rate;
    dir = 0;
    if (0 > snd_pcm_hw_params_set_rate_near(_saudio.backend.device, params, &rate, &dir)) {
        _SAUDIO_ERROR(ALSA_SND_PCM_HW_PARAMS_SET_RATE_NEAR_FAILED);
        goto error;
    }
    if (0 > snd_pcm_hw_params(_saudio.backend.device, params)) {
        _SAUDIO_ERROR(ALSA_SND_PCM_HW_PARAMS_FAILED);
        goto error;
    }

    /* read back actual sample rate and channels */
    _saudio.sample_rate = (int)rate;
    _saudio.bytes_per_frame = _saudio.num_channels * (int)sizeof(float);

    /* allocate the streaming buffer */
    _saudio.backend.buffer_byte_size = _saudio.buffer_frames * _saudio.bytes_per_frame;
    _saudio.backend.buffer_frames = _saudio.buffer_frames;
    _saudio.backend.buffer = (float*) _saudio_malloc_clear((size_t)_saudio.backend.buffer_byte_size);

    /* create the buffer-streaming start thread */
    if (0 != pthread_create(&_saudio.backend.thread, 0, _saudio_alsa_cb, 0)) {
        _SAUDIO_ERROR(ALSA_PTHREAD_CREATE_FAILED);
        goto error;
    }

    return true;
error:
    if (_saudio.backend.device) {
        snd_pcm_close(_saudio.backend.device);
        _saudio.backend.device = 0;
    }
    return false;
};

_SOKOL_PRIVATE void _saudio_alsa_backend_shutdown(void) {
    SOKOL_ASSERT(_saudio.backend.device);
    _saudio.backend.thread_stop = true;
    pthread_join(_saudio.backend.thread, 0);
    snd_pcm_drain(_saudio.backend.device);
    snd_pcm_close(_saudio.backend.device);
    _saudio_free(_saudio.backend.buffer);
};

// ██     ██  █████  ███████  █████  ██████  ██
// ██     ██ ██   ██ ██      ██   ██ ██   ██ ██
// ██  █  ██ ███████ ███████ ███████ ██████  ██
// ██ ███ ██ ██   ██      ██ ██   ██ ██      ██
//  ███ ███  ██   ██ ███████ ██   ██ ██      ██
//
// >>wasapi
#elif defined(_SAUDIO_WINDOWS)

/* fill intermediate buffer with new data and reset buffer_pos */
_SOKOL_PRIVATE void _saudio_wasapi_fill_buffer(void) {
    if (_saudio_has_callback()) {
        _saudio_stream_callback(_saudio.backend.thread.src_buffer, _saudio.backend.thread.src_buffer_frames, _saudio.num_channels);
    }
    else {
        if (0 == _saudio_fifo_read(&_saudio.fifo, (uint8_t*)_saudio.backend.thread.src_buffer, _saudio.backend.thread.src_buffer_byte_size)) {
            /* not enough read data available, fill the entire buffer with silence */
            _saudio_clear(_saudio.backend.thread.src_buffer, (size_t)_saudio.backend.thread.src_buffer_byte_size);
        }
    }
}

_SOKOL_PRIVATE int _saudio_wasapi_min(int a, int b) {
    return (a < b) ? a : b;
}

_SOKOL_PRIVATE void _saudio_wasapi_submit_buffer(int num_frames) {
    BYTE* wasapi_buffer = 0;
    if (FAILED(IAudioRenderClient_GetBuffer(_saudio.backend.render_client, num_frames, &wasapi_buffer))) {
        return;
    }
    SOKOL_ASSERT(wasapi_buffer);

    /* copy samples to WASAPI buffer, refill source buffer if needed */
    int num_remaining_samples = num_frames * _saudio.num_channels;
    int buffer_pos = _saudio.backend.thread.src_buffer_pos;
    const int buffer_size_in_samples = _saudio.backend.thread.src_buffer_byte_size / (int)sizeof(float);
    float* dst = (float*)wasapi_buffer;
    const float* dst_end = dst + num_remaining_samples;
    _SOKOL_UNUSED(dst_end); // suppress unused warning in release mode
    const float* src = _saudio.backend.thread.src_buffer;

    while (num_remaining_samples > 0) {
        if (0 == buffer_pos) {
            _saudio_wasapi_fill_buffer();
        }
        const int samples_to_copy = _saudio_wasapi_min(num_remaining_samples, buffer_size_in_samples - buffer_pos);
        SOKOL_ASSERT((buffer_pos + samples_to_copy) <= buffer_size_in_samples);
        SOKOL_ASSERT((dst + samples_to_copy) <= dst_end);
        memcpy(dst, &src[buffer_pos], (size_t)samples_to_copy * sizeof(float));
        num_remaining_samples -= samples_to_copy;
        SOKOL_ASSERT(num_remaining_samples >= 0);
        buffer_pos += samples_to_copy;
        dst += samples_to_copy;

        SOKOL_ASSERT(buffer_pos <= buffer_size_in_samples);
        if (buffer_pos == buffer_size_in_samples) {
            buffer_pos = 0;
        }
    }
    _saudio.backend.thread.src_buffer_pos = buffer_pos;
    IAudioRenderClient_ReleaseBuffer(_saudio.backend.render_client, num_frames, 0);
}

_SOKOL_PRIVATE DWORD WINAPI _saudio_wasapi_thread_fn(LPVOID param) {
    (void)param;
    _saudio_wasapi_submit_buffer(_saudio.backend.thread.src_buffer_frames);
    IAudioClient_Start(_saudio.backend.audio_client);
    while (!_saudio.backend.thread.stop) {
        WaitForSingleObject(_saudio.backend.thread.buffer_end_event, INFINITE);
        UINT32 padding = 0;
        if (FAILED(IAudioClient_GetCurrentPadding(_saudio.backend.audio_client, &padding))) {
            continue;
        }
        SOKOL_ASSERT(_saudio.backend.thread.dst_buffer_frames >= padding);
        int num_frames = (int)_saudio.backend.thread.dst_buffer_frames - (int)padding;
        if (num_frames > 0) {
            _saudio_wasapi_submit_buffer(num_frames);
        }
    }
    return 0;
}

_SOKOL_PRIVATE void _saudio_wasapi_release(void) {
    if (_saudio.backend.thread.src_buffer) {
        _saudio_free(_saudio.backend.thread.src_buffer);
        _saudio.backend.thread.src_buffer = 0;
    }
    if (_saudio.backend.render_client) {
        IAudioRenderClient_Release(_saudio.backend.render_client);
        _saudio.backend.render_client = 0;
    }
    if (_saudio.backend.audio_client) {
        IAudioClient_Release(_saudio.backend.audio_client);
        _saudio.backend.audio_client = 0;
    }
    if (_saudio.backend.device) {
        IMMDevice_Release(_saudio.backend.device);
        _saudio.backend.device = 0;
    }
    if (_saudio.backend.device_enumerator) {
        IMMDeviceEnumerator_Release(_saudio.backend.device_enumerator);
        _saudio.backend.device_enumerator = 0;
    }
    if (0 != _saudio.backend.thread.buffer_end_event) {
        CloseHandle(_saudio.backend.thread.buffer_end_event);
        _saudio.backend.thread.buffer_end_event = 0;
    }
}

_SOKOL_PRIVATE bool _saudio_wasapi_backend_init(void) {
    REFERENCE_TIME dur;
    /* CoInitializeEx could have been called elsewhere already, in which
        case the function returns with S_FALSE (thus it does not make much
        sense to check the result)
    */
    HRESULT hr = CoInitializeEx(0, COINIT_MULTITHREADED);
    _SOKOL_UNUSED(hr);
    _saudio.backend.thread.buffer_end_event = CreateEvent(0, FALSE, FALSE, 0);
    if (0 == _saudio.backend.thread.buffer_end_event) {
        _SAUDIO_ERROR(WASAPI_CREATE_EVENT_FAILED);
        goto error;
    }
    if (FAILED(CoCreateInstance(_SOKOL_AUDIO_WIN32COM_ID(_saudio_CLSID_IMMDeviceEnumerator),
        0, CLSCTX_ALL,
        _SOKOL_AUDIO_WIN32COM_ID(_saudio_IID_IMMDeviceEnumerator),
        (void**)&_saudio.backend.device_enumerator)))
    {
        _SAUDIO_ERROR(WASAPI_CREATE_DEVICE_ENUMERATOR_FAILED);
        goto error;
    }
    if (FAILED(IMMDeviceEnumerator_GetDefaultAudioEndpoint(_saudio.backend.device_enumerator,
        eRender, eConsole,
        &_saudio.backend.device)))
    {
        _SAUDIO_ERROR(WASAPI_GET_DEFAULT_AUDIO_ENDPOINT_FAILED);
        goto error;
    }
    if (FAILED(IMMDevice_Activate(_saudio.backend.device,
        _SOKOL_AUDIO_WIN32COM_ID(_saudio_IID_IAudioClient),
        CLSCTX_ALL, 0,
        (void**)&_saudio.backend.audio_client)))
    {
        _SAUDIO_ERROR(WASAPI_DEVICE_ACTIVATE_FAILED);
        goto error;
    }

    WAVEFORMATEXTENSIBLE fmtex;
    _saudio_clear(&fmtex, sizeof(fmtex));
    fmtex.Format.nChannels = (WORD)_saudio.num_channels;
    fmtex.Format.nSamplesPerSec = (DWORD)_saudio.sample_rate;
    fmtex.Format.wFormatTag = WAVE_FORMAT_EXTENSIBLE;
    fmtex.Format.wBitsPerSample = 32;
    fmtex.Format.nBlockAlign = (fmtex.Format.nChannels * fmtex.Format.wBitsPerSample) / 8;
    fmtex.Format.nAvgBytesPerSec = fmtex.Format.nSamplesPerSec * fmtex.Format.nBlockAlign;
    fmtex.Format.cbSize = 22;   /* WORD + DWORD + GUID */
    fmtex.Samples.wValidBitsPerSample = 32;
    if (_saudio.num_channels == 1) {
        fmtex.dwChannelMask = SPEAKER_FRONT_CENTER;
    }
    else {
        fmtex.dwChannelMask = SPEAKER_FRONT_LEFT|SPEAKER_FRONT_RIGHT;
    }
    fmtex.SubFormat = _saudio_KSDATAFORMAT_SUBTYPE_IEEE_FLOAT;
    dur = (REFERENCE_TIME)
        (((double)_saudio.buffer_frames) / (((double)_saudio.sample_rate) * (1.0/10000000.0)));
    if (FAILED(IAudioClient_Initialize(_saudio.backend.audio_client,
        AUDCLNT_SHAREMODE_SHARED,
        AUDCLNT_STREAMFLAGS_EVENTCALLBACK|AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM|AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY,
        dur, 0, (WAVEFORMATEX*)&fmtex, 0)))
    {
        _SAUDIO_ERROR(WASAPI_AUDIO_CLIENT_INITIALIZE_FAILED);
        goto error;
    }
    if (FAILED(IAudioClient_GetBufferSize(_saudio.backend.audio_client, &_saudio.backend.thread.dst_buffer_frames))) {
        _SAUDIO_ERROR(WASAPI_AUDIO_CLIENT_GET_BUFFER_SIZE_FAILED);
        goto error;
    }
    if (FAILED(IAudioClient_GetService(_saudio.backend.audio_client,
        _SOKOL_AUDIO_WIN32COM_ID(_saudio_IID_IAudioRenderClient),
        (void**)&_saudio.backend.render_client)))
    {
        _SAUDIO_ERROR(WASAPI_AUDIO_CLIENT_GET_SERVICE_FAILED);
        goto error;
    }
    if (FAILED(IAudioClient_SetEventHandle(_saudio.backend.audio_client, _saudio.backend.thread.buffer_end_event))) {
        _SAUDIO_ERROR(WASAPI_AUDIO_CLIENT_SET_EVENT_HANDLE_FAILED);
        goto error;
    }
    _saudio.bytes_per_frame = _saudio.num_channels * (int)sizeof(float);
    _saudio.backend.thread.src_buffer_frames = _saudio.buffer_frames;
    _saudio.backend.thread.src_buffer_byte_size = _saudio.backend.thread.src_buffer_frames * _saudio.bytes_per_frame;

    /* allocate an intermediate buffer for sample format conversion */
    _saudio.backend.thread.src_buffer = (float*) _saudio_malloc((size_t)_saudio.backend.thread.src_buffer_byte_size);

    /* create streaming thread */
    _saudio.backend.thread.thread_handle = CreateThread(NULL, 0, _saudio_wasapi_thread_fn, 0, 0, 0);
    if (0 == _saudio.backend.thread.thread_handle) {
        _SAUDIO_ERROR(WASAPI_CREATE_THREAD_FAILED);
        goto error;
    }
    return true;
error:
    _saudio_wasapi_release();
    return false;
}

_SOKOL_PRIVATE void _saudio_wasapi_backend_shutdown(void) {
    if (_saudio.backend.thread.thread_handle) {
        _saudio.backend.thread.stop = true;
        SetEvent(_saudio.backend.thread.buffer_end_event);
        WaitForSingleObject(_saudio.backend.thread.thread_handle, INFINITE);
        CloseHandle(_saudio.backend.thread.thread_handle);
        _saudio.backend.thread.thread_handle = 0;
    }
    if (_saudio.backend.audio_client) {
        IAudioClient_Stop(_saudio.backend.audio_client);
    }
    _saudio_wasapi_release();
    CoUninitialize();
}

// ██     ██ ███████ ██████   █████  ██    ██ ██████  ██  ██████
// ██     ██ ██      ██   ██ ██   ██ ██    ██ ██   ██ ██ ██    ██
// ██  █  ██ █████   ██████  ███████ ██    ██ ██   ██ ██ ██    ██
// ██ ███ ██ ██      ██   ██ ██   ██ ██    ██ ██   ██ ██ ██    ██
//  ███ ███  ███████ ██████  ██   ██  ██████  ██████  ██  ██████
//
// >>webaudio
#elif defined(_SAUDIO_EMSCRIPTEN)

#ifdef __cplusplus
extern "C" {
#endif

EMSCRIPTEN_KEEPALIVE int _saudio_emsc_pull(int num_frames) {
    SOKOL_ASSERT(_saudio.backend.buffer);
    if (num_frames == _saudio.buffer_frames) {
        if (_saudio_has_callback()) {
            _saudio_stream_callback((float*)_saudio.backend.buffer, num_frames, _saudio.num_channels);
        }
        else {
            const int num_bytes = num_frames * _saudio.bytes_per_frame;
            if (0 == _saudio_fifo_read(&_saudio.fifo, _saudio.backend.buffer, num_bytes)) {
                /* not enough read data available, fill the entire buffer with silence */
                _saudio_clear(_saudio.backend.buffer, (size_t)num_bytes);
            }
        }
        int res = (int) _saudio.backend.buffer;
        return res;
    }
    else {
        return 0;
    }
}

#ifdef __cplusplus
} /* extern "C" */
#endif

/* setup the WebAudio context and attach a ScriptProcessorNode */
EM_JS(int, saudio_js_init, (int sample_rate, int num_channels, int buffer_size), {
    Module._saudio_context = null;
    Module._saudio_node = null;
    if (typeof AudioContext !== 'undefined') {
        Module._saudio_context = new AudioContext({
            sampleRate: sample_rate,
            latencyHint: 'interactive',
        });
    }
    else {
        Module._saudio_context = null;
        console.log('sokol_audio.h: no WebAudio support');
    }
    if (Module._saudio_context) {
        console.log('sokol_audio.h: sample rate ', Module._saudio_context.sampleRate);
        Module._saudio_node = Module._saudio_context.createScriptProcessor(buffer_size, 0, num_channels);
        Module._saudio_node.onaudioprocess = (event) => {
            const num_frames = event.outputBuffer.length;
            const ptr = __saudio_emsc_pull(num_frames);
            if (ptr) {
                const num_channels = event.outputBuffer.numberOfChannels;
                for (let chn = 0; chn < num_channels; chn++) {
                    const chan = event.outputBuffer.getChannelData(chn);
                    for (let i = 0; i < num_frames; i++) {
                        chan[i] = HEAPF32[(ptr>>2) + ((num_channels*i)+chn)]
                    }
                }
            }
        };
        Module._saudio_node.connect(Module._saudio_context.destination);

        // in some browsers, WebAudio needs to be activated on a user action
        const resume_webaudio = () => {
            if (Module._saudio_context) {
                if (Module._saudio_context.state === 'suspended') {
                    Module._saudio_context.resume();
                }
            }
        };
        document.addEventListener('click', resume_webaudio, {once:true});
        document.addEventListener('touchend', resume_webaudio, {once:true});
        document.addEventListener('keydown', resume_webaudio, {once:true});
        return 1;
    }
    else {
        return 0;
    }
});

/* shutdown the WebAudioContext and ScriptProcessorNode */
EM_JS(void, saudio_js_shutdown, (void), {
    \x2F\x2A\x2A @suppress {missingProperties} \x2A\x2F
    const ctx = Module._saudio_context;
    if (ctx !== null) {
        if (Module._saudio_node) {
            Module._saudio_node.disconnect();
        }
        ctx.close();
        Module._saudio_context = null;
        Module._saudio_node = null;
    }
});

/* get the actual sample rate back from the WebAudio context */
EM_JS(int, saudio_js_sample_rate, (void), {
    if (Module._saudio_context) {
        return Module._saudio_context.sampleRate;
    }
    else {
        return 0;
    }
});

/* get the actual buffer size in number of frames */
EM_JS(int, saudio_js_buffer_frames, (void), {
    if (Module._saudio_node) {
        return Module._saudio_node.bufferSize;
    }
    else {
        return 0;
    }
});

/* return 1 if the WebAudio context is currently suspended, else 0 */
EM_JS(int, saudio_js_suspended, (void), {
    if (Module._saudio_context) {
        if (Module._saudio_context.state === 'suspended') {
            return 1;
        }
        else {
            return 0;
        }
    }
});

_SOKOL_PRIVATE bool _saudio_webaudio_backend_init(void) {
    if (saudio_js_init(_saudio.sample_rate, _saudio.num_channels, _saudio.buffer_frames)) {
        _saudio.bytes_per_frame = (int)sizeof(float) * _saudio.num_channels;
        _saudio.sample_rate = saudio_js_sample_rate();
        _saudio.buffer_frames = saudio_js_buffer_frames();
        const size_t buf_size = (size_t) (_saudio.buffer_frames * _saudio.bytes_per_frame);
        _saudio.backend.buffer = (uint8_t*) _saudio_malloc(buf_size);
        return true;
    }
    else {
        return false;
    }
}

_SOKOL_PRIVATE void _saudio_webaudio_backend_shutdown(void) {
    saudio_js_shutdown();
    if (_saudio.backend.buffer) {
        _saudio_free(_saudio.backend.buffer);
        _saudio.backend.buffer = 0;
    }
}

//  █████   █████  ██    ██ ██████  ██  ██████
// ██   ██ ██   ██ ██    ██ ██   ██ ██ ██    ██
// ███████ ███████ ██    ██ ██   ██ ██ ██    ██
// ██   ██ ██   ██ ██    ██ ██   ██ ██ ██    ██
// ██   ██ ██   ██  ██████  ██████  ██  ██████
//
// >>aaudio
#elif defined(SAUDIO_ANDROID_AAUDIO)

_SOKOL_PRIVATE aaudio_data_callback_result_t _saudio_aaudio_data_callback(AAudioStream* stream, void* user_data, void* audio_data, int32_t num_frames) {
    _SOKOL_UNUSED(user_data);
    _SOKOL_UNUSED(stream);
    if (_saudio_has_callback()) {
        _saudio_stream_callback((float*)audio_data, (int)num_frames, _saudio.num_channels);
    }
    else {
        uint8_t* ptr = (uint8_t*)audio_data;
        int num_bytes = _saudio.bytes_per_frame * num_frames;
        if (0 == _saudio_fifo_read(&_saudio.fifo, ptr, num_bytes)) {
            // not enough read data available, fill the entire buffer with silence
            memset(ptr, 0, (size_t)num_bytes);
        }
    }
    return AAUDIO_CALLBACK_RESULT_CONTINUE;
}

_SOKOL_PRIVATE bool _saudio_aaudio_start_stream(void) {
    if (AAudioStreamBuilder_openStream(_saudio.backend.builder, &_saudio.backend.stream) != AAUDIO_OK) {
        _SAUDIO_ERROR(AAUDIO_STREAMBUILDER_OPEN_STREAM_FAILED);
        return false;
    }
    AAudioStream_requestStart(_saudio.backend.stream);
    return true;
}

_SOKOL_PRIVATE void _saudio_aaudio_stop_stream(void) {
    if (_saudio.backend.stream) {
        AAudioStream_requestStop(_saudio.backend.stream);
        AAudioStream_close(_saudio.backend.stream);
        _saudio.backend.stream = 0;
    }
}

_SOKOL_PRIVATE void* _saudio_aaudio_restart_stream_thread_fn(void* param) {
    _SOKOL_UNUSED(param);
    _SAUDIO_WARN(AAUDIO_RESTARTING_STREAM_AFTER_ERROR);
    pthread_mutex_lock(&_saudio.backend.mutex);
    _saudio_aaudio_stop_stream();
    _saudio_aaudio_start_stream();
    pthread_mutex_unlock(&_saudio.backend.mutex);
    return 0;
}

_SOKOL_PRIVATE void _saudio_aaudio_error_callback(AAudioStream* stream, void* user_data, aaudio_result_t error) {
    _SOKOL_UNUSED(stream);
    _SOKOL_UNUSED(user_data);
    if (error == AAUDIO_ERROR_DISCONNECTED) {
        if (0 != pthread_create(&_saudio.backend.thread, 0, _saudio_aaudio_restart_stream_thread_fn, 0)) {
            _SAUDIO_ERROR(AAUDIO_PTHREAD_CREATE_FAILED);
        }
    }
}

_SOKOL_PRIVATE void _saudio_aaudio_backend_shutdown(void) {
    pthread_mutex_lock(&_saudio.backend.mutex);
    _saudio_aaudio_stop_stream();
    pthread_mutex_unlock(&_saudio.backend.mutex);
    if (_saudio.backend.builder) {
        AAudioStreamBuilder_delete(_saudio.backend.builder);
        _saudio.backend.builder = 0;
    }
    pthread_mutex_destroy(&_saudio.backend.mutex);
}

_SOKOL_PRIVATE bool _saudio_aaudio_backend_init(void) {
    _SAUDIO_INFO(USING_AAUDIO_BACKEND);

    _saudio.bytes_per_frame = _saudio.num_channels * (int)sizeof(float);

    pthread_mutexattr_t attr;
    pthread_mutexattr_init(&attr);
    pthread_mutex_init(&_saudio.backend.mutex, &attr);

    if (AAudio_createStreamBuilder(&_saudio.backend.builder) != AAUDIO_OK) {
        _SAUDIO_ERROR(AAUDIO_CREATE_STREAMBUILDER_FAILED);
        _saudio_aaudio_backend_shutdown();
        return false;
    }

    AAudioStreamBuilder_setFormat(_saudio.backend.builder, AAUDIO_FORMAT_PCM_FLOAT);
    AAudioStreamBuilder_setSampleRate(_saudio.backend.builder, _saudio.sample_rate);
    AAudioStreamBuilder_setChannelCount(_saudio.backend.builder, _saudio.num_channels);
    AAudioStreamBuilder_setBufferCapacityInFrames(_saudio.backend.builder, _saudio.buffer_frames * 2);
    AAudioStreamBuilder_setFramesPerDataCallback(_saudio.backend.builder, _saudio.buffer_frames);
    AAudioStreamBuilder_setDataCallback(_saudio.backend.builder, _saudio_aaudio_data_callback, 0);
    AAudioStreamBuilder_setErrorCallback(_saudio.backend.builder, _saudio_aaudio_error_callback, 0);

    if (!_saudio_aaudio_start_stream()) {
        _saudio_aaudio_backend_shutdown();
        return false;
    }

    return true;
}

//  ██████  ██████  ███████ ███    ██ ███████ ██      ███████ ███████
// ██    ██ ██   ██ ██      ████   ██ ██      ██      ██      ██
// ██    ██ ██████  █████   ██ ██  ██ ███████ ██      █████   ███████
// ██    ██ ██      ██      ██  ██ ██      ██ ██      ██           ██
//  ██████  ██      ███████ ██   ████ ███████ ███████ ███████ ███████
//
//  >>opensles
//  >>sles
#elif defined(SAUDIO_ANDROID_SLES)

_SOKOL_PRIVATE void _saudio_sles_semaphore_init(_saudio_sles_semaphore_t* sem) {
    sem->count = 0;
    int r = pthread_mutex_init(&sem->mutex, NULL);
    SOKOL_ASSERT(r == 0);
    r = pthread_cond_init(&sem->cond, NULL);
    SOKOL_ASSERT(r == 0);
    (void)(r);
}

_SOKOL_PRIVATE void _saudio_sles_semaphore_destroy(_saudio_sles_semaphore_t* sem) {
    pthread_cond_destroy(&sem->cond);
    pthread_mutex_destroy(&sem->mutex);
}

_SOKOL_PRIVATE void _saudio_sles_semaphore_post(_saudio_sles_semaphore_t* sem, int count) {
    int r = pthread_mutex_lock(&sem->mutex);
    SOKOL_ASSERT(r == 0);
    for (int ii = 0; ii < count; ii++) {
        r = pthread_cond_signal(&sem->cond);
        SOKOL_ASSERT(r == 0);
    }
    sem->count += count;
    r = pthread_mutex_unlock(&sem->mutex);
    SOKOL_ASSERT(r == 0);
    (void)(r);
}

_SOKOL_PRIVATE bool _saudio_sles_semaphore_wait(_saudio_sles_semaphore_t* sem) {
    int r = pthread_mutex_lock(&sem->mutex);
    SOKOL_ASSERT(r == 0);
    while (r == 0 && sem->count <= 0) {
        r = pthread_cond_wait(&sem->cond, &sem->mutex);
    }
    bool ok = (r == 0);
    if (ok) {
        --sem->count;
    }
    r = pthread_mutex_unlock(&sem->mutex);
    (void)(r);
    return ok;
}

/* fill intermediate buffer with new data and reset buffer_pos */
_SOKOL_PRIVATE void _saudio_sles_fill_buffer(void) {
    int src_buffer_frames = _saudio.buffer_frames;
    if (_saudio_has_callback()) {
        _saudio_stream_callback(_saudio.backend.src_buffer, src_buffer_frames, _saudio.num_channels);
    }
    else {
        const int src_buffer_byte_size = src_buffer_frames * _saudio.num_channels * (int)sizeof(float);
        if (0 == _saudio_fifo_read(&_saudio.fifo, (uint8_t*)_saudio.backend.src_buffer, src_buffer_byte_size)) {
            /* not enough read data available, fill the entire buffer with silence */
            _saudio_clear(_saudio.backend.src_buffer, (size_t)src_buffer_byte_size);
        }
    }
}

_SOKOL_PRIVATE void SLAPIENTRY _saudio_sles_play_cb(SLPlayItf player, void *context, SLuint32 event) {
    _SOKOL_UNUSED(context);
    _SOKOL_UNUSED(player);
    if (event & SL_PLAYEVENT_HEADATEND) {
        _saudio_sles_semaphore_post(&_saudio.backend.buffer_sem, 1);
    }
}

_SOKOL_PRIVATE void* _saudio_sles_thread_fn(void* param) {
    _SOKOL_UNUSED(param);
    while (!_saudio.backend.thread_stop)  {
        /* get next output buffer, advance, next buffer. */
        int16_t* out_buffer = _saudio.backend.output_buffers[_saudio.backend.active_buffer];
        _saudio.backend.active_buffer = (_saudio.backend.active_buffer + 1) % SAUDIO_SLES_NUM_BUFFERS;
        int16_t* next_buffer = _saudio.backend.output_buffers[_saudio.backend.active_buffer];

        /* queue this buffer */
        const int buffer_size_bytes = _saudio.buffer_frames * _saudio.num_channels * (int)sizeof(short);
        (*_saudio.backend.player_buffer_queue)->Enqueue(_saudio.backend.player_buffer_queue, out_buffer, (SLuint32)buffer_size_bytes);

        /* fill the next buffer */
        _saudio_sles_fill_buffer();
        const int num_samples = _saudio.num_channels * _saudio.buffer_frames;
        for (int i = 0; i < num_samples; ++i) {
            next_buffer[i] = (int16_t) (_saudio.backend.src_buffer[i] * 0x7FFF);
        }

        _saudio_sles_semaphore_wait(&_saudio.backend.buffer_sem);
    }

    return 0;
}

_SOKOL_PRIVATE void _saudio_sles_backend_shutdown(void) {
    _saudio.backend.thread_stop = 1;
    pthread_join(_saudio.backend.thread, 0);

    if (_saudio.backend.player_obj) {
        (*_saudio.backend.player_obj)->Destroy(_saudio.backend.player_obj);
    }

    if (_saudio.backend.output_mix_obj) {
        (*_saudio.backend.output_mix_obj)->Destroy(_saudio.backend.output_mix_obj);
    }

    if (_saudio.backend.engine_obj) {
        (*_saudio.backend.engine_obj)->Destroy(_saudio.backend.engine_obj);
    }

    for (int i = 0; i < SAUDIO_SLES_NUM_BUFFERS; i++) {
        _saudio_free(_saudio.backend.output_buffers[i]);
    }
    _saudio_free(_saudio.backend.src_buffer);
}

_SOKOL_PRIVATE bool _saudio_sles_backend_init(void) {
    _SAUDIO_INFO(USING_SLES_BACKEND);

    _saudio.bytes_per_frame = (int)sizeof(float) * _saudio.num_channels;

    for (int i = 0; i < SAUDIO_SLES_NUM_BUFFERS; ++i) {
        const int buffer_size_bytes = (int)sizeof(int16_t) * _saudio.num_channels * _saudio.buffer_frames;
        _saudio.backend.output_buffers[i] = (int16_t*) _saudio_malloc_clear((size_t)buffer_size_bytes);
    }

    {
        const int buffer_size_bytes = _saudio.bytes_per_frame * _saudio.buffer_frames;
        _saudio.backend.src_buffer = (float*) _saudio_malloc_clear((size_t)buffer_size_bytes);
    }

    /* Create engine */
    const SLEngineOption opts[] = { { SL_ENGINEOPTION_THREADSAFE, SL_BOOLEAN_TRUE } };
    if (slCreateEngine(&_saudio.backend.engine_obj, 1, opts, 0, NULL, NULL ) != SL_RESULT_SUCCESS) {
        _SAUDIO_ERROR(SLES_CREATE_ENGINE_FAILED);
        _saudio_sles_backend_shutdown();
        return false;
    }

    (*_saudio.backend.engine_obj)->Realize(_saudio.backend.engine_obj, SL_BOOLEAN_FALSE);
    if ((*_saudio.backend.engine_obj)->GetInterface(_saudio.backend.engine_obj, SL_IID_ENGINE, &_saudio.backend.engine) != SL_RESULT_SUCCESS) {
        _SAUDIO_ERROR(SLES_ENGINE_GET_ENGINE_INTERFACE_FAILED);
        _saudio_sles_backend_shutdown();
        return false;
    }

    /* Create output mix. */
    {
        const SLInterfaceID ids[] = { SL_IID_VOLUME };
        const SLboolean req[] = { SL_BOOLEAN_FALSE };

        if ((*_saudio.backend.engine)->CreateOutputMix(_saudio.backend.engine, &_saudio.backend.output_mix_obj, 1, ids, req) != SL_RESULT_SUCCESS) {
            _SAUDIO_ERROR(SLES_CREATE_OUTPUT_MIX_FAILED);
            _saudio_sles_backend_shutdown();
            return false;
        }
        (*_saudio.backend.output_mix_obj)->Realize(_saudio.backend.output_mix_obj, SL_BOOLEAN_FALSE);

        if ((*_saudio.backend.output_mix_obj)->GetInterface(_saudio.backend.output_mix_obj, SL_IID_VOLUME, &_saudio.backend.output_mix_vol) != SL_RESULT_SUCCESS) {
            _SAUDIO_WARN(SLES_MIXER_GET_VOLUME_INTERFACE_FAILED);
        }
    }

    /* android buffer queue */
    _saudio.backend.in_locator.locatorType = SL_DATALOCATOR_ANDROIDSIMPLEBUFFERQUEUE;
    _saudio.backend.in_locator.numBuffers = SAUDIO_SLES_NUM_BUFFERS;

    /* data format */
    SLDataFormat_PCM format;
    format.formatType = SL_DATAFORMAT_PCM;
    format.numChannels = (SLuint32)_saudio.num_channels;
    format.samplesPerSec = (SLuint32) (_saudio.sample_rate * 1000);
    format.bitsPerSample = SL_PCMSAMPLEFORMAT_FIXED_16;
    format.containerSize = 16;
    format.endianness = SL_BYTEORDER_LITTLEENDIAN;

    if (_saudio.num_channels == 2) {
        format.channelMask = SL_SPEAKER_FRONT_LEFT | SL_SPEAKER_FRONT_RIGHT;
    } else {
        format.channelMask = SL_SPEAKER_FRONT_CENTER;
    }

    SLDataSource src;
    src.pLocator = &_saudio.backend.in_locator;
    src.pFormat = &format;

    /* Output mix. */
    _saudio.backend.out_locator.locatorType = SL_DATALOCATOR_OUTPUTMIX;
    _saudio.backend.out_locator.outputMix = _saudio.backend.output_mix_obj;

    _saudio.backend.dst_data_sink.pLocator = &_saudio.backend.out_locator;
    _saudio.backend.dst_data_sink.pFormat = NULL;

    /* setup player */
    {
        const SLInterfaceID ids[] = { SL_IID_VOLUME, SL_IID_ANDROIDSIMPLEBUFFERQUEUE };
        const SLboolean req[] = { SL_BOOLEAN_FALSE, SL_BOOLEAN_TRUE };

        if ((*_saudio.backend.engine)->CreateAudioPlayer(_saudio.backend.engine, &_saudio.backend.player_obj, &src, &_saudio.backend.dst_data_sink, sizeof(ids) / sizeof(ids[0]), ids, req) != SL_RESULT_SUCCESS)
        {
            _SAUDIO_ERROR(SLES_ENGINE_CREATE_AUDIO_PLAYER_FAILED);
            _saudio_sles_backend_shutdown();
            return false;
        }
        (*_saudio.backend.player_obj)->Realize(_saudio.backend.player_obj, SL_BOOLEAN_FALSE);

        if ((*_saudio.backend.player_obj)->GetInterface(_saudio.backend.player_obj, SL_IID_PLAY, &_saudio.backend.player) != SL_RESULT_SUCCESS) {
            _SAUDIO_ERROR(SLES_PLAYER_GET_PLAY_INTERFACE_FAILED);
            _saudio_sles_backend_shutdown();
            return false;
        }
        if ((*_saudio.backend.player_obj)->GetInterface(_saudio.backend.player_obj, SL_IID_VOLUME, &_saudio.backend.player_vol) != SL_RESULT_SUCCESS) {
            _SAUDIO_ERROR(SLES_PLAYER_GET_VOLUME_INTERFACE_FAILED);
        }
        if ((*_saudio.backend.player_obj)->GetInterface(_saudio.backend.player_obj, SL_IID_ANDROIDSIMPLEBUFFERQUEUE, &_saudio.backend.player_buffer_queue) != SL_RESULT_SUCCESS) {
            _SAUDIO_ERROR(SLES_PLAYER_GET_BUFFERQUEUE_INTERFACE_FAILED);
            _saudio_sles_backend_shutdown();
            return false;
        }
    }

    /* begin */
    {
        const int buffer_size_bytes = (int)sizeof(int16_t) * _saudio.num_channels * _saudio.buffer_frames;
        (*_saudio.backend.player_buffer_queue)->Enqueue(_saudio.backend.player_buffer_queue, _saudio.backend.output_buffers[0], (SLuint32)buffer_size_bytes);
        _saudio.backend.active_buffer = (_saudio.backend.active_buffer + 1) % SAUDIO_SLES_NUM_BUFFERS;

        (*_saudio.backend.player)->RegisterCallback(_saudio.backend.player, _saudio_sles_play_cb, NULL);
        (*_saudio.backend.player)->SetCallbackEventsMask(_saudio.backend.player, SL_PLAYEVENT_HEADATEND);
        (*_saudio.backend.player)->SetPlayState(_saudio.backend.player, SL_PLAYSTATE_PLAYING);
    }

    /* create the buffer-streaming start thread */
    if (0 != pthread_create(&_saudio.backend.thread, 0, _saudio_sles_thread_fn, 0)) {
        _saudio_sles_backend_shutdown();
        return false;
    }

    return true;
}

//  ██████  ██████  ██████  ███████  █████  ██    ██ ██████  ██  ██████
// ██      ██    ██ ██   ██ ██      ██   ██ ██    ██ ██   ██ ██ ██    ██
// ██      ██    ██ ██████  █████   ███████ ██    ██ ██   ██ ██ ██    ██
// ██      ██    ██ ██   ██ ██      ██   ██ ██    ██ ██   ██ ██ ██    ██
//  ██████  ██████  ██   ██ ███████ ██   ██  ██████  ██████  ██  ██████
//
// >>coreaudio
#elif defined(_SAUDIO_APPLE)

#if defined(_SAUDIO_IOS)
#if __has_feature(objc_arc)
#define _SAUDIO_OBJC_RELEASE(obj) { obj = nil; }
#else
#define _SAUDIO_OBJC_RELEASE(obj) { [obj release]; obj = nil; }
#endif

@interface _saudio_interruption_handler : NSObject { }
@end

@implementation _saudio_interruption_handler
-(id)init {
    self = [super init];
    AVAudioSession* session = [AVAudioSession sharedInstance];
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(handle_interruption:) name:AVAudioSessionInterruptionNotification object:session];
    return self;
}

-(void)dealloc {
    [self remove_handler];
    #if !__has_feature(objc_arc)
    [super dealloc];
    #endif
}

-(void)remove_handler {
    [[NSNotificationCenter defaultCenter] removeObserver:self name:@"AVAudioSessionInterruptionNotification" object:nil];
}

-(void)handle_interruption:(NSNotification*)notification {
    AVAudioSession* session = [AVAudioSession sharedInstance];
    SOKOL_ASSERT(session);
    NSDictionary* dict = notification.userInfo;
    SOKOL_ASSERT(dict);
    NSInteger type = [[dict valueForKey:AVAudioSessionInterruptionTypeKey] integerValue];
    switch (type) {
        case AVAudioSessionInterruptionTypeBegan:
            if (_saudio.backend.ca_audio_queue) {
                AudioQueuePause(_saudio.backend.ca_audio_queue);
            }
            [session setActive:false error:nil];
            break;
        case AVAudioSessionInterruptionTypeEnded:
            [session setActive:true error:nil];
            if (_saudio.backend.ca_audio_queue) {
                AudioQueueStart(_saudio.backend.ca_audio_queue, NULL);
            }
            break;
        default:
            break;
    }
}
@end
#endif // _SAUDIO_IOS

/* NOTE: the buffer data callback is called on a separate thread! */
_SOKOL_PRIVATE void _saudio_coreaudio_callback(void* user_data, _saudio_AudioQueueRef queue, _saudio_AudioQueueBufferRef buffer) {
    _SOKOL_UNUSED(user_data);
    if (_saudio_has_callback()) {
        const int num_frames = (int)buffer->mAudioDataByteSize / _saudio.bytes_per_frame;
        const int num_channels = _saudio.num_channels;
        _saudio_stream_callback((float*)buffer->mAudioData, num_frames, num_channels);
    }
    else {
        uint8_t* ptr = (uint8_t*)buffer->mAudioData;
        int num_bytes = (int) buffer->mAudioDataByteSize;
        if (0 == _saudio_fifo_read(&_saudio.fifo, ptr, num_bytes)) {
            /* not enough read data available, fill the entire buffer with silence */
            _saudio_clear(ptr, (size_t)num_bytes);
        }
    }
    AudioQueueEnqueueBuffer(queue, buffer, 0, NULL);
}

_SOKOL_PRIVATE void _saudio_coreaudio_backend_shutdown(void) {
    if (_saudio.backend.ca_audio_queue) {
        AudioQueueStop(_saudio.backend.ca_audio_queue, true);
        AudioQueueDispose(_saudio.backend.ca_audio_queue, false);
        _saudio.backend.ca_audio_queue = 0;
    }
    #if defined(_SAUDIO_IOS)
        /* remove interruption handler */
        if (_saudio.backend.ca_interruption_handler != nil) {
            [_saudio.backend.ca_interruption_handler remove_handler];
            _SAUDIO_OBJC_RELEASE(_saudio.backend.ca_interruption_handler);
        }
        /* deactivate audio session */
        AVAudioSession* session = [AVAudioSession sharedInstance];
        SOKOL_ASSERT(session);
        [session setActive:false error:nil];;
    #endif // _SAUDIO_IOS
}

_SOKOL_PRIVATE bool _saudio_coreaudio_backend_init(void) {
    SOKOL_ASSERT(0 == _saudio.backend.ca_audio_queue);

    #if defined(_SAUDIO_IOS)
        /* activate audio session */
        AVAudioSession* session = [AVAudioSession sharedInstance];
        SOKOL_ASSERT(session != nil);
        [session setCategory: AVAudioSessionCategoryPlayback error:nil];
        [session setActive:true error:nil];

        /* create interruption handler */
        _saudio.backend.ca_interruption_handler = [[_saudio_interruption_handler alloc] init];
    #endif

    /* create an audio queue with fp32 samples */
    _saudio_AudioStreamBasicDescription fmt;
    _saudio_clear(&fmt, sizeof(fmt));
    fmt.mSampleRate = (double) _saudio.sample_rate;
    fmt.mFormatID = _saudio_kAudioFormatLinearPCM;
    fmt.mFormatFlags = _saudio_kLinearPCMFormatFlagIsFloat | _saudio_kAudioFormatFlagIsPacked;
    fmt.mFramesPerPacket = 1;
    fmt.mChannelsPerFrame = (uint32_t) _saudio.num_channels;
    fmt.mBytesPerFrame = (uint32_t)sizeof(float) * (uint32_t)_saudio.num_channels;
    fmt.mBytesPerPacket = fmt.mBytesPerFrame;
    fmt.mBitsPerChannel = 32;
    _saudio_OSStatus res = AudioQueueNewOutput(&fmt, _saudio_coreaudio_callback, 0, NULL, NULL, 0, &_saudio.backend.ca_audio_queue);
    if (0 != res) {
        _SAUDIO_ERROR(COREAUDIO_NEW_OUTPUT_FAILED);
        return false;
    }
    SOKOL_ASSERT(_saudio.backend.ca_audio_queue);

    /* create 2 audio buffers */
    for (int i = 0; i < 2; i++) {
        _saudio_AudioQueueBufferRef buf = NULL;
        const uint32_t buf_byte_size = (uint32_t)_saudio.buffer_frames * fmt.mBytesPerFrame;
        res = AudioQueueAllocateBuffer(_saudio.backend.ca_audio_queue, buf_byte_size, &buf);
        if (0 != res) {
            _SAUDIO_ERROR(COREAUDIO_ALLOCATE_BUFFER_FAILED);
            _saudio_coreaudio_backend_shutdown();
            return false;
        }
        buf->mAudioDataByteSize = buf_byte_size;
        _saudio_clear(buf->mAudioData, buf->mAudioDataByteSize);
        AudioQueueEnqueueBuffer(_saudio.backend.ca_audio_queue, buf, 0, NULL);
    }

    /* init or modify actual playback parameters */
    _saudio.bytes_per_frame = (int)fmt.mBytesPerFrame;

    /* ...and start playback */
    res = AudioQueueStart(_saudio.backend.ca_audio_queue, NULL);
    if (0 != res) {
        _SAUDIO_ERROR(COREAUDIO_START_FAILED);
        _saudio_coreaudio_backend_shutdown();
        return false;
    }
    return true;
}

#else
#error "unsupported platform"
#endif

bool _saudio_backend_init(void) {
    #if defined(SOKOL_DUMMY_BACKEND)
        return _saudio_dummy_backend_init();
    #elif defined(_SAUDIO_LINUX)
        return _saudio_alsa_backend_init();
    #elif defined(_SAUDIO_WINDOWS)
        return _saudio_wasapi_backend_init();
    #elif defined(_SAUDIO_EMSCRIPTEN)
        return _saudio_webaudio_backend_init();
    #elif defined(SAUDIO_ANDROID_AAUDIO)
        return _saudio_aaudio_backend_init();
    #elif defined(SAUDIO_ANDROID_SLES)
        return _saudio_sles_backend_init();
    #elif defined(_SAUDIO_APPLE)
        return _saudio_coreaudio_backend_init();
    #else
    #error "unknown platform"
    #endif
}

void _saudio_backend_shutdown(void) {
    #if defined(SOKOL_DUMMY_BACKEND)
        _saudio_dummy_backend_shutdown();
    #elif defined(_SAUDIO_LINUX)
        _saudio_alsa_backend_shutdown();
    #elif defined(_SAUDIO_WINDOWS)
        _saudio_wasapi_backend_shutdown();
    #elif defined(_SAUDIO_EMSCRIPTEN)
        _saudio_webaudio_backend_shutdown();
    #elif defined(SAUDIO_ANDROID_AAUDIO)
        _saudio_aaudio_backend_shutdown();
    #elif defined(SAUDIO_ANDROID_SLES)
        _saudio_sles_backend_shutdown();
    #elif defined(_SAUDIO_APPLE)
        return _saudio_coreaudio_backend_shutdown();
    #else
    #error "unknown platform"
    #endif
}

// ██████  ██    ██ ██████  ██      ██  ██████
// ██   ██ ██    ██ ██   ██ ██      ██ ██
// ██████  ██    ██ ██████  ██      ██ ██
// ██      ██    ██ ██   ██ ██      ██ ██
// ██       ██████  ██████  ███████ ██  ██████
//
// >>public
SOKOL_API_IMPL void saudio_setup(const saudio_desc* desc) {
    SOKOL_ASSERT(!_saudio.valid);
    SOKOL_ASSERT(desc);
    SOKOL_ASSERT((desc->allocator.alloc_fn && desc->allocator.free_fn) || (!desc->allocator.alloc_fn && !desc->allocator.free_fn));
    _saudio_clear(&_saudio, sizeof(_saudio));
    _saudio.desc = *desc;
    _saudio.stream_cb = desc->stream_cb;
    _saudio.stream_userdata_cb = desc->stream_userdata_cb;
    _saudio.user_data = desc->user_data;
    _saudio.sample_rate = _saudio_def(_saudio.desc.sample_rate, _SAUDIO_DEFAULT_SAMPLE_RATE);
    _saudio.buffer_frames = _saudio_def(_saudio.desc.buffer_frames, _SAUDIO_DEFAULT_BUFFER_FRAMES);
    _saudio.packet_frames = _saudio_def(_saudio.desc.packet_frames, _SAUDIO_DEFAULT_PACKET_FRAMES);
    _saudio.num_packets = _saudio_def(_saudio.desc.num_packets, _SAUDIO_DEFAULT_NUM_PACKETS);
    _saudio.num_channels = _saudio_def(_saudio.desc.num_channels, 1);
    _saudio_fifo_init_mutex(&_saudio.fifo);
    if (_saudio_backend_init()) {
        /* the backend might not support the requested exact buffer size,
           make sure the actual buffer size is still a multiple of
           the requested packet size
        */
        if (0 != (_saudio.buffer_frames % _saudio.packet_frames)) {
            _SAUDIO_ERROR(BACKEND_BUFFER_SIZE_ISNT_MULTIPLE_OF_PACKET_SIZE);
            _saudio_backend_shutdown();
            return;
        }
        SOKOL_ASSERT(_saudio.bytes_per_frame > 0);
        _saudio_fifo_init(&_saudio.fifo, _saudio.packet_frames * _saudio.bytes_per_frame, _saudio.num_packets);
        _saudio.valid = true;
    }
    else {
        _saudio_fifo_destroy_mutex(&_saudio.fifo);
    }
}

SOKOL_API_IMPL void saudio_shutdown(void) {
    if (_saudio.valid) {
        _saudio_backend_shutdown();
        _saudio_fifo_shutdown(&_saudio.fifo);
        _saudio_fifo_destroy_mutex(&_saudio.fifo);
        _saudio.valid = false;
    }
}

SOKOL_API_IMPL bool saudio_isvalid(void) {
    return _saudio.valid;
}

SOKOL_API_IMPL void* saudio_userdata(void) {
    return _saudio.desc.user_data;
}

SOKOL_API_IMPL saudio_desc saudio_query_desc(void) {
    return _saudio.desc;
}

SOKOL_API_IMPL int saudio_sample_rate(void) {
    return _saudio.sample_rate;
}

SOKOL_API_IMPL int saudio_buffer_frames(void) {
    return _saudio.buffer_frames;
}

SOKOL_API_IMPL int saudio_channels(void) {
    return _saudio.num_channels;
}

SOKOL_API_IMPL bool saudio_suspended(void) {
    #if defined(_SAUDIO_EMSCRIPTEN)
        if (_saudio.valid) {
            return 1 == saudio_js_suspended();
        }
        else {
            return false;
        }
    #else
        return false;
    #endif
}

SOKOL_API_IMPL int saudio_expect(void) {
    if (_saudio.valid) {
        const int num_frames = _saudio_fifo_writable_bytes(&_saudio.fifo) / _saudio.bytes_per_frame;
        return num_frames;
    }
    else {
        return 0;
    }
}

SOKOL_API_IMPL int saudio_push(const float* frames, int num_frames) {
    SOKOL_ASSERT(frames && (num_frames > 0));
    if (_saudio.valid) {
        const int num_bytes = num_frames * _saudio.bytes_per_frame;
        const int num_written = _saudio_fifo_write(&_saudio.fifo, (const uint8_t*)frames, num_bytes);
        return num_written / _saudio.bytes_per_frame;
    }
    else {
        return 0;
    }
}

#undef _saudio_def
#undef _saudio_def_flt

#if defined(_SAUDIO_WINDOWS)
#ifdef _MSC_VER
#pragma warning(pop)
#endif
#endif

#endif /* SOKOL_AUDIO_IMPL */
