#ifndef SOKOL_AUDIO_INCLUDED
/*
    sokol_audio.h -- cross-platform audio-streaming API

    Project URL: https://github.com/floooh/sokol

    Do this:
        #define SOKOL_IMPL
    before you include this file in *one* C or C++ file to create the
    implementation.

    Optionally provide the following defines with your own implementations:

    SOKOL_DUMMY_BACKEND - use a dummy backend
    SOKOL_ASSERT(c)     - your own assert macro (default: assert(c))
    SOKOL_LOG(msg)      - your own logging function (default: puts(msg))
    SOKOL_MALLOC(s)     - your own malloc() implementation (default: malloc(s))
    SOKOL_FREE(p)       - your own free() implementation (default: free(p))
    SOKOL_API_DECL      - public function declaration prefix (default: extern)
    SOKOL_API_IMPL      - public function implementation prefix (default: -)

    SAUDIO_RING_MAX_SLOTS   - max number of slots in the push-audio ring buffer (default 1024)

    If sokol_audio.h is compiled as a DLL, define the following before
    including the declaration or implementation:

    SOKOL_DLL

    On Windows, SOKOL_DLL will define SOKOL_API_DECL as __declspec(dllexport)
    or __declspec(dllimport) as needed.

    FEATURE OVERVIEW
    ================
    You provide a mono- or stereo-stream of 32-bit float samples, which
    Sokol Audio feeds into platform-specific audio backends:

    - Windows: WASAPI
    - Linux: ALSA (link with asound)
    - macOS/iOS: CoreAudio (link with AudioToolbox)
    - emscripten: WebAudio with ScriptProcessorNode
    - Android: OpenSLES (link with OpenSLES)

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

    If you want to use the callback-model, you need to provide a stream
    callback function either in saudio_desc.stream_cb or saudio_desc.stream_userdata_cb,
    otherwise keep both function pointers zero-initialized.

    Use push model and default playback parameters:

        saudio_setup(&(saudio_desc){0});

    Use stream callback model and default playback parameters:

        saudio_setup(&(saudio_desc){
            .stream_cb = my_stream_callback
        });

    The standard stream callback doesn't have a user data argument, if you want
    that, use the alternative stream_userdata_cb and also set the user_data pointer:

        saudio_setup(&(saudio_desc){
            .stream_userdata_cb = my_stream_callback,
            .user_data = &my_data
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

    THE COREAUDIO BACKEND
    =====================
    The CoreAudio backend is selected on macOS and iOS (__APPLE__ is defined).
    Since the CoreAudio API is implemented in C (not Objective-C) the
    implementation part of Sokol Audio can be included into a C source file.

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

typedef struct saudio_desc {
    int sample_rate;        /* requested sample rate */
    int num_channels;       /* number of channels, default: 1 (mono) */
    int buffer_frames;      /* number of frames in streaming buffer */
    int packet_frames;      /* number of frames in a packet */
    int num_packets;        /* number of packets in packet queue */
    void (*stream_cb)(float* buffer, int num_frames, int num_channels);  /* optional streaming callback (no user data) */
    void (*stream_userdata_cb)(float* buffer, int num_frames, int num_channels, void* user_data); /*... and with user data */
    void* user_data;        /* optional user data argument for stream_userdata_cb */
} saudio_desc;

/* setup sokol-audio */
SOKOL_API_DECL void saudio_setup(const saudio_desc* desc);
/* shutdown sokol-audio */
SOKOL_API_DECL void saudio_shutdown(void);
/* true after setup if audio backend was successfully initialized */
SOKOL_API_DECL bool saudio_isvalid(void);
/* return the saudio_desc.user_data pointer */
SOKOL_API_DECL void* saudio_userdata(void);
/* return a copy of the original saudio_desc struct */
SOKOL_API_DECL saudio_desc saudio_query_desc(void);
/* actual sample rate */
SOKOL_API_DECL int saudio_sample_rate(void);
/* return actual backend buffer size in number of frames */
SOKOL_API_DECL int saudio_buffer_frames(void);
/* actual number of channels */
SOKOL_API_DECL int saudio_channels(void);
/* get current number of frames to fill packet queue */
SOKOL_API_DECL int saudio_expect(void);
/* push sample frames from main thread, returns number of frames actually pushed */
SOKOL_API_DECL int saudio_push(const float* frames, int num_frames);

#ifdef __cplusplus
} /* extern "C" */

/* reference-based equivalents for c++ */
inline void saudio_setup(const saudio_desc& desc) { return saudio_setup(&desc); }

#endif
#endif // SOKOL_AUDIO_INCLUDED

/*=== IMPLEMENTATION =========================================================*/
#ifdef SOKOL_IMPL
#define SOKOL_AUDIO_IMPL_INCLUDED (1)
#include <string.h> /* memset, memcpy */

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
#ifndef SOKOL_MALLOC
    #include <stdlib.h>
    #define SOKOL_MALLOC(s) malloc(s)
    #define SOKOL_FREE(p) free(p)
#endif
#ifndef SOKOL_LOG
    #ifdef SOKOL_DEBUG
        #include <stdio.h>
        #define SOKOL_LOG(s) { SOKOL_ASSERT(s); puts(s); }
    #else
        #define SOKOL_LOG(s)
    #endif
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

#if defined(SOKOL_DUMMY_BACKEND)
    // No threads needed for SOKOL_DUMMY_BACKEND
#elif (defined(__APPLE__) || defined(__linux__) || defined(__unix__)) && !defined(__EMSCRIPTEN__)
    #include <pthread.h>
#elif defined(_WIN32)
    #ifndef WIN32_LEAN_AND_MEAN
    #define WIN32_LEAN_AND_MEAN
    #endif
    #ifndef NOMINMAX
    #define NOMINMAX
    #endif
    #include <windows.h>
    #include <synchapi.h>
    #if (defined(WINAPI_FAMILY_PARTITION) && !WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP))
        #define SOKOL_WIN32_NO_MMDEVICE
        #pragma comment (lib, "WindowsApp")
    #else
        #pragma comment (lib, "kernel32")
        #pragma comment (lib, "ole32")
        #if defined(SOKOL_WIN32_NO_MMDEVICE)
            #pragma comment (lib, "mmdevapi")
        #endif
    #endif
#endif

#if defined(SOKOL_DUMMY_BACKEND)
    // No audio API needed for SOKOL_DUMMY_BACKEND
#elif defined(__APPLE__)
    #include <AudioToolbox/AudioToolbox.h>
#elif (defined(__linux__) || defined(__unix__)) && !defined(__EMSCRIPTEN__) && !defined(__ANDROID__)
    #define ALSA_PCM_NEW_HW_PARAMS_API
    #include <alsa/asoundlib.h>
#elif defined(__ANDROID__)
    #include "SLES/OpenSLES_Android.h"
#elif defined(_WIN32)
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
    static const IID _saudio_IID_IAudioClient = { 0x1cb9ad4c, 0xdbfa, 0x4c32, { 0xb1, 0x78, 0xc2, 0xf5, 0x68, 0xa7, 0x03, 0xb2 } };
    static const IID _saudio_IID_IMMDeviceEnumerator = { 0xa95664d2, 0x9614, 0x4f35, { 0xa7, 0x46, 0xde, 0x8d, 0xb6, 0x36, 0x17, 0xe6 } };
    static const CLSID _saudio_CLSID_IMMDeviceEnumerator = { 0xbcde0395, 0xe52f, 0x467c, { 0x8e, 0x3d, 0xc4, 0x57, 0x92, 0x91, 0x69, 0x2e } };
    static const IID _saudio_IID_IAudioRenderClient = { 0xf294acfc, 0x3146, 0x4483,{ 0xa7, 0xbf, 0xad, 0xdc, 0xa7, 0xc2, 0x60, 0xe2 } };
    static const IID _saudio_IID_Devinterface_Audio_Render = { 0xe6327cad, 0xdcec, 0x4949, {0xae, 0x8a, 0x99, 0x1e, 0x97, 0x6a, 0x79, 0xd2 } };
    static const IID _saudio_IID_IActivateAudioInterface_Completion_Handler = { 0x94ea2b94, 0xe9cc, 0x49e0, {0xc0, 0xff, 0xee, 0x64, 0xca, 0x8f, 0x5b, 0x90} };
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
#elif defined(__EMSCRIPTEN__)
    #include <emscripten/emscripten.h>
#endif

#ifdef _MSC_VER
    #pragma warning(push)
    #pragma warning(disable:4505)   /* unreferenced local function has been removed */
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

/*=== MUTEX WRAPPER DECLARATIONS =============================================*/
#if defined(SOKOL_DUMMY_BACKEND)

typedef struct { int dummy_mutex; } _saudio_mutex_t;

#elif (defined(__APPLE__) || defined(__linux__) || defined(__unix__)) && !defined(__EMSCRIPTEN__)

typedef struct {
    pthread_mutex_t mutex;
} _saudio_mutex_t;

#elif defined(_WIN32)

typedef struct {
    CRITICAL_SECTION critsec;
} _saudio_mutex_t;

#else
typedef struct { int dummy_mutex; } _saudio_mutex_t;
#endif

/*=== DUMMY BACKEND DECLARATIONS =============================================*/
#if defined(SOKOL_DUMMY_BACKEND)
typedef struct {
    int dummy_backend;
} _saudio_backend_t;
/*=== COREAUDIO BACKEND DECLARATIONS =========================================*/
#elif defined(__APPLE__)

typedef struct {
    AudioQueueRef ca_audio_queue;
} _saudio_backend_t;

/*=== ALSA BACKEND DECLARATIONS ==============================================*/
#elif (defined(__linux__) || defined(__unix__)) && !defined(__EMSCRIPTEN__) && !defined(__ANDROID__)

typedef struct {
    snd_pcm_t* device;
    float* buffer;
    int buffer_byte_size;
    int buffer_frames;
    pthread_t thread;
    bool thread_stop;
} _saudio_backend_t;

/*=== OpenSLES BACKEND DECLARATIONS ==============================================*/
#elif defined(__ANDROID__)

#define SAUDIO_NUM_BUFFERS 2

typedef struct {
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    int count;
} _saudio_semaphore_t;

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

    int16_t* output_buffers[SAUDIO_NUM_BUFFERS];
    float* src_buffer;
    int active_buffer;
    _saudio_semaphore_t buffer_sem;
    pthread_t thread;
    volatile int thread_stop;
    SLDataLocator_AndroidSimpleBufferQueue in_locator;
} _saudio_backend_t;

/*=== WASAPI BACKEND DECLARATIONS ============================================*/
#elif defined(_WIN32)

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
#if defined(SOKOL_WIN32_NO_MMDEVICE)
    LPOLESTR interface_activation_audio_interface_uid_string;
    IActivateAudioInterfaceAsyncOperation* interface_activation_operation;
    BOOL interface_activation_success;
    HANDLE interface_activation_mutex;
#else
    IMMDeviceEnumerator* device_enumerator;
    IMMDevice* device;
#endif
    IAudioClient* audio_client;
    IAudioRenderClient* render_client;
    int si16_bytes_per_frame;
    _saudio_wasapi_thread_data_t thread;
} _saudio_backend_t;

/*=== WEBAUDIO BACKEND DECLARATIONS ==========================================*/
#elif defined(__EMSCRIPTEN__)

typedef struct {
    uint8_t* buffer;
} _saudio_backend_t;

/*=== DUMMY BACKEND DECLARATIONS =============================================*/
#else
typedef struct { } _saudio_backend_t;
#endif
/*=== GENERAL DECLARATIONS ===================================================*/

/* a ringbuffer structure */
typedef struct {
    uint32_t head;  /* next slot to write to */
    uint32_t tail;  /* next slot to read from */
    uint32_t num;   /* number of slots in queue */
    uint32_t queue[SAUDIO_RING_MAX_SLOTS];
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

static _saudio_state_t _saudio;

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

/*=== MUTEX IMPLEMENTATION ===================================================*/
#if defined(SOKOL_DUMMY_BACKEND)
_SOKOL_PRIVATE void _saudio_mutex_init(_saudio_mutex_t* m) { (void)m; }
_SOKOL_PRIVATE void _saudio_mutex_destroy(_saudio_mutex_t* m) { (void)m; }
_SOKOL_PRIVATE void _saudio_mutex_lock(_saudio_mutex_t* m) { (void)m; }
_SOKOL_PRIVATE void _saudio_mutex_unlock(_saudio_mutex_t* m) { (void)m; }
#elif (defined(__APPLE__) || defined(__linux__) || defined(__unix__)) && !defined(__EMSCRIPTEN__)
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

#elif defined(_WIN32)
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
_SOKOL_PRIVATE void _saudio_mutex_init(_saudio_mutex_t* m) { (void)m; }
_SOKOL_PRIVATE void _saudio_mutex_destroy(_saudio_mutex_t* m) { (void)m; }
_SOKOL_PRIVATE void _saudio_mutex_lock(_saudio_mutex_t* m) { (void)m; }
_SOKOL_PRIVATE void _saudio_mutex_unlock(_saudio_mutex_t* m) { (void)m; }
#endif

/*=== RING-BUFFER QUEUE IMPLEMENTATION =======================================*/
_SOKOL_PRIVATE uint16_t _saudio_ring_idx(_saudio_ring_t* ring, uint32_t i) {
    return (uint16_t) (i % ring->num);
}

_SOKOL_PRIVATE void _saudio_ring_init(_saudio_ring_t* ring, uint32_t num_slots) {
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
    uint32_t count;
    if (ring->head >= ring->tail) {
        count = ring->head - ring->tail;
    }
    else {
        count = (ring->head + ring->num) - ring->tail;
    }
    SOKOL_ASSERT(count < ring->num);
    return count;
}

_SOKOL_PRIVATE void _saudio_ring_enqueue(_saudio_ring_t* ring, uint32_t val) {
    SOKOL_ASSERT(!_saudio_ring_full(ring));
    ring->queue[ring->head] = val;
    ring->head = _saudio_ring_idx(ring, ring->head + 1);
}

_SOKOL_PRIVATE uint32_t _saudio_ring_dequeue(_saudio_ring_t* ring) {
    SOKOL_ASSERT(!_saudio_ring_empty(ring));
    uint32_t val = ring->queue[ring->tail];
    ring->tail = _saudio_ring_idx(ring, ring->tail + 1);
    return val;
}

/*---  a packet fifo for queueing audio data from main thread ----------------*/
_SOKOL_PRIVATE void _saudio_fifo_init_mutex(_saudio_fifo_t* fifo) {
    /* this must be called before initializing both the backend and the fifo itself! */
    _saudio_mutex_init(&fifo->mutex);
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
    fifo->base_ptr = (uint8_t*) SOKOL_MALLOC(packet_size * num_packets);
    SOKOL_ASSERT(fifo->base_ptr);
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
    SOKOL_FREE(fifo->base_ptr);
    fifo->base_ptr = 0;
    fifo->valid = false;
    _saudio_mutex_destroy(&fifo->mutex);
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
            memcpy(dst, ptr, to_copy);
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
                memcpy(dst, src, fifo->packet_size);
                dst += fifo->packet_size;
                num_bytes_copied += fifo->packet_size;
            }
            SOKOL_ASSERT(num_bytes == num_bytes_copied);
        }
    }
    _saudio_mutex_unlock(&fifo->mutex);
    return num_bytes_copied;
}

/*=== DUMMY BACKEND IMPLEMENTATION ===========================================*/
#if defined(SOKOL_DUMMY_BACKEND)
_SOKOL_PRIVATE bool _saudio_backend_init(void) {
    _saudio.bytes_per_frame = _saudio.num_channels * sizeof(float);
    return true;
};
_SOKOL_PRIVATE void _saudio_backend_shutdown(void) { };

/*=== COREAUDIO BACKEND IMPLEMENTATION =======================================*/
#elif defined(__APPLE__)

/* NOTE: the buffer data callback is called on a separate thread! */
_SOKOL_PRIVATE void _sapp_ca_callback(void* user_data, AudioQueueRef queue, AudioQueueBufferRef buffer) {
    _SOKOL_UNUSED(user_data);
    if (_saudio_has_callback()) {
        const int num_frames = buffer->mAudioDataByteSize / _saudio.bytes_per_frame;
        const int num_channels = _saudio.num_channels;
        _saudio_stream_callback((float*)buffer->mAudioData, num_frames, num_channels);
    }
    else {
        uint8_t* ptr = (uint8_t*)buffer->mAudioData;
        int num_bytes = (int) buffer->mAudioDataByteSize;
        if (0 == _saudio_fifo_read(&_saudio.fifo, ptr, num_bytes)) {
            /* not enough read data available, fill the entire buffer with silence */
            memset(ptr, 0, num_bytes);
        }
    }
    AudioQueueEnqueueBuffer(queue, buffer, 0, NULL);
}

_SOKOL_PRIVATE bool _saudio_backend_init(void) {
    SOKOL_ASSERT(0 == _saudio.backend.ca_audio_queue);

    /* create an audio queue with fp32 samples */
    AudioStreamBasicDescription fmt;
    memset(&fmt, 0, sizeof(fmt));
    fmt.mSampleRate = (Float64) _saudio.sample_rate;
    fmt.mFormatID = kAudioFormatLinearPCM;
    fmt.mFormatFlags = kLinearPCMFormatFlagIsFloat | kAudioFormatFlagIsPacked;
    fmt.mFramesPerPacket = 1;
    fmt.mChannelsPerFrame = _saudio.num_channels;
    fmt.mBytesPerFrame = sizeof(float) * _saudio.num_channels;
    fmt.mBytesPerPacket = fmt.mBytesPerFrame;
    fmt.mBitsPerChannel = 32;
    OSStatus res = AudioQueueNewOutput(&fmt, _sapp_ca_callback, 0, NULL, NULL, 0, &_saudio.backend.ca_audio_queue);
    SOKOL_ASSERT((res == 0) && _saudio.backend.ca_audio_queue);

    /* create 2 audio buffers */
    for (int i = 0; i < 2; i++) {
        AudioQueueBufferRef buf = NULL;
        const uint32_t buf_byte_size = _saudio.buffer_frames * fmt.mBytesPerFrame;
        res = AudioQueueAllocateBuffer(_saudio.backend.ca_audio_queue, buf_byte_size, &buf);
        SOKOL_ASSERT((res == 0) && buf);
        buf->mAudioDataByteSize = buf_byte_size;
        memset(buf->mAudioData, 0, buf->mAudioDataByteSize);
        AudioQueueEnqueueBuffer(_saudio.backend.ca_audio_queue, buf, 0, NULL);
    }

    /* init or modify actual playback parameters */
    _saudio.bytes_per_frame = fmt.mBytesPerFrame;

    /* ...and start playback */
    res = AudioQueueStart(_saudio.backend.ca_audio_queue, NULL);
    SOKOL_ASSERT(0 == res);

    return true;
}

_SOKOL_PRIVATE void _saudio_backend_shutdown(void) {
    AudioQueueStop(_saudio.backend.ca_audio_queue, true);
    AudioQueueDispose(_saudio.backend.ca_audio_queue, false);
    _saudio.backend.ca_audio_queue = NULL;
}

/*=== ALSA BACKEND IMPLEMENTATION ============================================*/
#elif (defined(__linux__) || defined(__unix__)) && !defined(__EMSCRIPTEN__) && !defined(__ANDROID__)

/* the streaming callback runs in a separate thread */
_SOKOL_PRIVATE void* _saudio_alsa_cb(void* param) {
    _SOKOL_UNUSED(param);
    while (!_saudio.backend.thread_stop) {
        /* snd_pcm_writei() will be blocking until it needs data */
        int write_res = snd_pcm_writei(_saudio.backend.device, _saudio.backend.buffer, _saudio.backend.buffer_frames);
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
                    memset(_saudio.backend.buffer, 0, _saudio.backend.buffer_byte_size);
                }
            }
        }
    }
    return 0;
}

_SOKOL_PRIVATE bool _saudio_backend_init(void) {
    int dir; unsigned int val;
    int rc = snd_pcm_open(&_saudio.backend.device, "default", SND_PCM_STREAM_PLAYBACK, 0);
    if (rc < 0) {
        return false;
    }
    snd_pcm_hw_params_t* params = 0;
    snd_pcm_hw_params_alloca(&params);
    snd_pcm_hw_params_any(_saudio.backend.device, params);
    snd_pcm_hw_params_set_access(_saudio.backend.device, params, SND_PCM_ACCESS_RW_INTERLEAVED);
    snd_pcm_hw_params_set_channels(_saudio.backend.device, params, _saudio.num_channels);
    snd_pcm_hw_params_set_buffer_size(_saudio.backend.device, params, _saudio.buffer_frames);
    if (0 > snd_pcm_hw_params_test_format(_saudio.backend.device, params, SND_PCM_FORMAT_FLOAT_LE)) {
        goto error;
    }
    else {
        snd_pcm_hw_params_set_format(_saudio.backend.device, params, SND_PCM_FORMAT_FLOAT_LE);
    }
    val = _saudio.sample_rate;
    dir = 0;
    if (0 > snd_pcm_hw_params_set_rate_near(_saudio.backend.device, params, &val, &dir)) {
        goto error;
    }
    if (0 > snd_pcm_hw_params(_saudio.backend.device, params)) {
        goto error;
    }

    /* read back actual sample rate and channels */
    snd_pcm_hw_params_get_rate(params, &val, &dir);
    _saudio.sample_rate = val;
    snd_pcm_hw_params_get_channels(params, &val);
    SOKOL_ASSERT((int)val == _saudio.num_channels);
    _saudio.bytes_per_frame = _saudio.num_channels * sizeof(float);

    /* allocate the streaming buffer */
    _saudio.backend.buffer_byte_size = _saudio.buffer_frames * _saudio.bytes_per_frame;
    _saudio.backend.buffer_frames = _saudio.buffer_frames;
    _saudio.backend.buffer = (float*) SOKOL_MALLOC(_saudio.backend.buffer_byte_size);
    memset(_saudio.backend.buffer, 0, _saudio.backend.buffer_byte_size);

    /* create the buffer-streaming start thread */
    if (0 != pthread_create(&_saudio.backend.thread, 0, _saudio_alsa_cb, 0)) {
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

_SOKOL_PRIVATE void _saudio_backend_shutdown(void) {
    SOKOL_ASSERT(_saudio.backend.device);
    _saudio.backend.thread_stop = true;
    pthread_join(_saudio.backend.thread, 0);
    snd_pcm_drain(_saudio.backend.device);
    snd_pcm_close(_saudio.backend.device);
    SOKOL_FREE(_saudio.backend.buffer);
};

/*=== WASAPI BACKEND IMPLEMENTATION ==========================================*/
#elif defined(_WIN32)

#if defined(SOKOL_WIN32_NO_MMDEVICE)
/* Minimal implementation of an IActivateAudioInterfaceCompletionHandler COM object in plain C.
   Meant to be a static singleton (always one reference when add/remove reference)
   and implements IUnknown and IActivateAudioInterfaceCompletionHandler when queryinterface'd

   Do not know why but IActivateAudioInterfaceCompletionHandler's GUID is not the one system queries for,
   so I'm advertising the one actually requested.
*/
_SOKOL_PRIVATE HRESULT STDMETHODCALLTYPE _saudio_interface_completion_handler_queryinterface(IActivateAudioInterfaceCompletionHandler* instance, REFIID riid, void** ppvObject) {
    if (!ppvObject) {
        return E_POINTER;
    }

    if (IsEqualIID(riid, _SOKOL_AUDIO_WIN32COM_ID(_saudio_IID_IActivateAudioInterface_Completion_Handler)) || IsEqualIID(riid, _SOKOL_AUDIO_WIN32COM_ID(IID_IUnknown)))
    {
        *ppvObject = (void*)instance;
        return S_OK;
    }

    *ppvObject = NULL;
    return E_NOINTERFACE;
}

_SOKOL_PRIVATE ULONG STDMETHODCALLTYPE _saudio_interface_completion_handler_addref_release(IActivateAudioInterfaceCompletionHandler* instance) {
    _SOKOL_UNUSED(instance);
    return 1;
}

_SOKOL_PRIVATE HRESULT STDMETHODCALLTYPE _saudio_backend_activate_audio_interface_cb(IActivateAudioInterfaceCompletionHandler* instance, IActivateAudioInterfaceAsyncOperation* activateOperation) {
    _SOKOL_UNUSED(instance);
    WaitForSingleObject(_saudio.backend.interface_activation_mutex, INFINITE);
    _saudio.backend.interface_activation_success = TRUE;
    HRESULT activation_result;
    if (FAILED(activateOperation->lpVtbl->GetActivateResult(activateOperation, &activation_result, (IUnknown**)(&_saudio.backend.audio_client))) || FAILED(activation_result)) {
        _saudio.backend.interface_activation_success = FALSE;
    }

    ReleaseMutex(_saudio.backend.interface_activation_mutex);
    return S_OK;
}
#endif

/* fill intermediate buffer with new data and reset buffer_pos */
_SOKOL_PRIVATE void _saudio_wasapi_fill_buffer(void) {
    if (_saudio_has_callback()) {
        _saudio_stream_callback(_saudio.backend.thread.src_buffer, _saudio.backend.thread.src_buffer_frames, _saudio.num_channels);
    }
    else {
        if (0 == _saudio_fifo_read(&_saudio.fifo, (uint8_t*)_saudio.backend.thread.src_buffer, _saudio.backend.thread.src_buffer_byte_size)) {
            /* not enough read data available, fill the entire buffer with silence */
            memset(_saudio.backend.thread.src_buffer, 0, _saudio.backend.thread.src_buffer_byte_size);
        }
    }
}

_SOKOL_PRIVATE void _saudio_wasapi_submit_buffer(UINT32 num_frames) {
    BYTE* wasapi_buffer = 0;
    if (FAILED(IAudioRenderClient_GetBuffer(_saudio.backend.render_client, num_frames, &wasapi_buffer))) {
        return;
    }
    SOKOL_ASSERT(wasapi_buffer);

    /* convert float samples to int16_t, refill float buffer if needed */
    const int num_samples = num_frames * _saudio.num_channels;
    int16_t* dst = (int16_t*) wasapi_buffer;
    uint32_t buffer_pos = _saudio.backend.thread.src_buffer_pos;
    const uint32_t buffer_float_size = _saudio.backend.thread.src_buffer_byte_size / sizeof(float);
    float* src = _saudio.backend.thread.src_buffer;
    for (int i = 0; i < num_samples; i++) {
        if (0 == buffer_pos) {
            _saudio_wasapi_fill_buffer();
        }
        dst[i] = (int16_t) (src[buffer_pos] * 0x7FFF);
        buffer_pos += 1;
        if (buffer_pos == buffer_float_size) {
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
        UINT32 num_frames = _saudio.backend.thread.dst_buffer_frames - padding;
        if (num_frames > 0) {
            _saudio_wasapi_submit_buffer(num_frames);
        }
    }
    return 0;
}

_SOKOL_PRIVATE void _saudio_wasapi_release(void) {
    if (_saudio.backend.thread.src_buffer) {
        SOKOL_FREE(_saudio.backend.thread.src_buffer);
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
#if defined(SOKOL_WIN32_NO_MMDEVICE)
    if (_saudio.backend.interface_activation_audio_interface_uid_string) {
        CoTaskMemFree(_saudio.backend.interface_activation_audio_interface_uid_string);
        _saudio.backend.interface_activation_audio_interface_uid_string = 0;
    }
    if (_saudio.backend.interface_activation_operation) {
        IActivateAudioInterfaceAsyncOperation_Release(_saudio.backend.interface_activation_operation);
        _saudio.backend.interface_activation_operation = 0;
    }
#else
    if (_saudio.backend.device) {
        IMMDevice_Release(_saudio.backend.device);
        _saudio.backend.device = 0;
    }
    if (_saudio.backend.device_enumerator) {
        IMMDeviceEnumerator_Release(_saudio.backend.device_enumerator);
        _saudio.backend.device_enumerator = 0;
    }
#endif
    if (0 != _saudio.backend.thread.buffer_end_event) {
        CloseHandle(_saudio.backend.thread.buffer_end_event);
        _saudio.backend.thread.buffer_end_event = 0;
    }
}

_SOKOL_PRIVATE bool _saudio_backend_init(void) {
    REFERENCE_TIME dur;
    /* UWP Threads are CoInitialized by default with a different threading model, and this call fails
    See https://github.com/Microsoft/cppwinrt/issues/6#issuecomment-253930637 */
#if (defined(WINAPI_FAMILY_PARTITION) && WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP))
    /* CoInitializeEx could have been called elsewhere already, in which
        case the function returns with S_FALSE (thus it doesn't make much
        sense to check the result)
    */
    HRESULT hr = CoInitializeEx(0, COINIT_MULTITHREADED);
    _SOKOL_UNUSED(hr);
#endif
    _saudio.backend.thread.buffer_end_event = CreateEvent(0, FALSE, FALSE, 0);
    if (0 == _saudio.backend.thread.buffer_end_event) {
        SOKOL_LOG("sokol_audio wasapi: failed to create buffer_end_event");
        goto error;
    }
#if defined(SOKOL_WIN32_NO_MMDEVICE)
    _saudio.backend.interface_activation_mutex = CreateMutexA(NULL, FALSE, "interface_activation_mutex");
    if (_saudio.backend.interface_activation_mutex == NULL) {
        SOKOL_LOG("sokol_audio wasapi: failed to create interface activation mutex");
        goto error;
    }
    if (FAILED(StringFromIID(_SOKOL_AUDIO_WIN32COM_ID(_saudio_IID_Devinterface_Audio_Render), &_saudio.backend.interface_activation_audio_interface_uid_string))) {
        SOKOL_LOG("sokol_audio wasapi: failed to get default audio device ID string");
        goto error;
    }

    /* static instance of the fake COM object */
    static IActivateAudioInterfaceCompletionHandlerVtbl completion_handler_interface_vtable = {
        _saudio_interface_completion_handler_queryinterface,
        _saudio_interface_completion_handler_addref_release,
        _saudio_interface_completion_handler_addref_release,
        _saudio_backend_activate_audio_interface_cb
    };
    static IActivateAudioInterfaceCompletionHandler completion_handler_interface = { &completion_handler_interface_vtable };

    if (FAILED(ActivateAudioInterfaceAsync(_saudio.backend.interface_activation_audio_interface_uid_string, _SOKOL_AUDIO_WIN32COM_ID(_saudio_IID_IAudioClient), NULL, &completion_handler_interface, &_saudio.backend.interface_activation_operation))) {
        SOKOL_LOG("sokol_audio wasapi: failed to get default audio device ID string");
        goto error;
    }
    while (!(_saudio.backend.audio_client)) {
        if (WaitForSingleObject(_saudio.backend.interface_activation_mutex, 10) != WAIT_TIMEOUT) {
            ReleaseMutex(_saudio.backend.interface_activation_mutex);
        }
    }

    if (!(_saudio.backend.interface_activation_success)) {
        SOKOL_LOG("sokol_audio wasapi: interface activation failed. Unable to get audio client");
        goto error;
    }

#else
    if (FAILED(CoCreateInstance(_SOKOL_AUDIO_WIN32COM_ID(_saudio_CLSID_IMMDeviceEnumerator),
        0, CLSCTX_ALL,
        _SOKOL_AUDIO_WIN32COM_ID(_saudio_IID_IMMDeviceEnumerator),
        (void**)&_saudio.backend.device_enumerator)))
    {
        SOKOL_LOG("sokol_audio wasapi: failed to create device enumerator");
        goto error;
    }
    if (FAILED(IMMDeviceEnumerator_GetDefaultAudioEndpoint(_saudio.backend.device_enumerator,
        eRender, eConsole,
        &_saudio.backend.device)))
    {
        SOKOL_LOG("sokol_audio wasapi: GetDefaultAudioEndPoint failed");
        goto error;
    }
    if (FAILED(IMMDevice_Activate(_saudio.backend.device,
        _SOKOL_AUDIO_WIN32COM_ID(_saudio_IID_IAudioClient),
        CLSCTX_ALL, 0,
        (void**)&_saudio.backend.audio_client)))
    {
        SOKOL_LOG("sokol_audio wasapi: device activate failed");
        goto error;
    }
#endif
    WAVEFORMATEX fmt;
    memset(&fmt, 0, sizeof(fmt));
    fmt.nChannels = (WORD) _saudio.num_channels;
    fmt.nSamplesPerSec = _saudio.sample_rate;
    fmt.wFormatTag = WAVE_FORMAT_PCM;
    fmt.wBitsPerSample = 16;
    fmt.nBlockAlign = (fmt.nChannels * fmt.wBitsPerSample) / 8;
    fmt.nAvgBytesPerSec = fmt.nSamplesPerSec * fmt.nBlockAlign;
    dur = (REFERENCE_TIME)
        (((double)_saudio.buffer_frames) / (((double)_saudio.sample_rate) * (1.0/10000000.0)));
    if (FAILED(IAudioClient_Initialize(_saudio.backend.audio_client,
        AUDCLNT_SHAREMODE_SHARED,
        AUDCLNT_STREAMFLAGS_EVENTCALLBACK|AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM|AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY,
        dur, 0, &fmt, 0)))
    {
        SOKOL_LOG("sokol_audio wasapi: audio client initialize failed");
        goto error;
    }
    if (FAILED(IAudioClient_GetBufferSize(_saudio.backend.audio_client, &_saudio.backend.thread.dst_buffer_frames))) {
        SOKOL_LOG("sokol_audio wasapi: audio client get buffer size failed");
        goto error;
    }
    if (FAILED(IAudioClient_GetService(_saudio.backend.audio_client,
        _SOKOL_AUDIO_WIN32COM_ID(_saudio_IID_IAudioRenderClient),
        (void**)&_saudio.backend.render_client)))
    {
        SOKOL_LOG("sokol_audio wasapi: audio client GetService failed");
        goto error;
    }
    if (FAILED(IAudioClient_SetEventHandle(_saudio.backend.audio_client, _saudio.backend.thread.buffer_end_event))) {
        SOKOL_LOG("sokol_audio wasapi: audio client SetEventHandle failed");
        goto error;
    }
    _saudio.backend.si16_bytes_per_frame = _saudio.num_channels * sizeof(int16_t);
    _saudio.bytes_per_frame = _saudio.num_channels * sizeof(float);
    _saudio.backend.thread.src_buffer_frames = _saudio.buffer_frames;
    _saudio.backend.thread.src_buffer_byte_size = _saudio.backend.thread.src_buffer_frames * _saudio.bytes_per_frame;

    /* allocate an intermediate buffer for sample format conversion */
    _saudio.backend.thread.src_buffer = (float*) SOKOL_MALLOC(_saudio.backend.thread.src_buffer_byte_size);
    SOKOL_ASSERT(_saudio.backend.thread.src_buffer);

    /* create streaming thread */
    _saudio.backend.thread.thread_handle = CreateThread(NULL, 0, _saudio_wasapi_thread_fn, 0, 0, 0);
    if (0 == _saudio.backend.thread.thread_handle) {
        SOKOL_LOG("sokol_audio wasapi: CreateThread failed");
        goto error;
    }
    return true;
error:
    _saudio_wasapi_release();
    return false;
}

_SOKOL_PRIVATE void _saudio_backend_shutdown(void) {
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

#if (defined(WINAPI_FAMILY_PARTITION) && WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP))
    CoUninitialize();
#endif
}

/*=== EMSCRIPTEN BACKEND IMPLEMENTATION ======================================*/
#elif defined(__EMSCRIPTEN__)

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
                memset(_saudio.backend.buffer, 0, num_bytes);
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
    else if (typeof webkitAudioContext !== 'undefined') {
        Module._saudio_context = new webkitAudioContext({
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
        Module._saudio_node.onaudioprocess = function pump_audio(event) {
            var num_frames = event.outputBuffer.length;
            var ptr = __saudio_emsc_pull(num_frames);
            if (ptr) {
                var num_channels = event.outputBuffer.numberOfChannels;
                for (var chn = 0; chn < num_channels; chn++) {
                    var chan = event.outputBuffer.getChannelData(chn);
                    for (var i = 0; i < num_frames; i++) {
                        chan[i] = HEAPF32[(ptr>>2) + ((num_channels*i)+chn)]
                    }
                }
            }
        };
        Module._saudio_node.connect(Module._saudio_context.destination);

        // in some browsers, WebAudio needs to be activated on a user action
        var resume_webaudio = function() {
            if (Module._saudio_context) {
                if (Module._saudio_context.state === 'suspended') {
                    Module._saudio_context.resume();
                }
            }
        };
        document.addEventListener('click', resume_webaudio, {once:true});
        document.addEventListener('touchstart', resume_webaudio, {once:true});
        document.addEventListener('keydown', resume_webaudio, {once:true});
        return 1;
    }
    else {
        return 0;
    }
});

/* shutdown the WebAudioContext and ScriptProcessorNode */
EM_JS(void, saudio_js_shutdown, (void), {
    if (Module._saudio_context !== null) {
        if (Module._saudio_node) {
            Module._saudio_node.disconnect();
        }
        Module._saudio_context.close();
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

_SOKOL_PRIVATE bool _saudio_backend_init(void) {
    if (saudio_js_init(_saudio.sample_rate, _saudio.num_channels, _saudio.buffer_frames)) {
        _saudio.bytes_per_frame = sizeof(float) * _saudio.num_channels;
        _saudio.sample_rate = saudio_js_sample_rate();
        _saudio.buffer_frames = saudio_js_buffer_frames();
        const int buf_size = _saudio.buffer_frames * _saudio.bytes_per_frame;
        _saudio.backend.buffer = (uint8_t*) SOKOL_MALLOC(buf_size);
        return true;
    }
    else {
        return false;
    }
}

_SOKOL_PRIVATE void _saudio_backend_shutdown(void) {
    saudio_js_shutdown();
    if (_saudio.backend.buffer) {
        SOKOL_FREE(_saudio.backend.buffer);
        _saudio.backend.buffer = 0;
    }
}

/*=== ANDROID BACKEND IMPLEMENTATION ======================================*/
#elif defined(__ANDROID__)

#ifdef __cplusplus
extern "C" {
#endif

_SOKOL_PRIVATE void _saudio_semaphore_init(_saudio_semaphore_t* sem) {
    sem->count = 0;
    int r = pthread_mutex_init(&sem->mutex, NULL);
    SOKOL_ASSERT(r == 0);

    r = pthread_cond_init(&sem->cond, NULL);
    SOKOL_ASSERT(r == 0);

    (void)(r);
}

_SOKOL_PRIVATE void _saudio_semaphore_destroy(_saudio_semaphore_t* sem)
{
    pthread_cond_destroy(&sem->cond);
    pthread_mutex_destroy(&sem->mutex);
}

_SOKOL_PRIVATE void _saudio_semaphore_post(_saudio_semaphore_t* sem, int count)
{
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

_SOKOL_PRIVATE bool _saudio_semaphore_wait(_saudio_semaphore_t* sem)
{
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
_SOKOL_PRIVATE void _saudio_opensles_fill_buffer(void) {
    int src_buffer_frames = _saudio.buffer_frames;
    if (_saudio_has_callback()) {
        _saudio_stream_callback(_saudio.backend.src_buffer, src_buffer_frames, _saudio.num_channels);
    }
    else {
        const int src_buffer_byte_size = src_buffer_frames * _saudio.num_channels * sizeof(float);
        if (0 == _saudio_fifo_read(&_saudio.fifo, (uint8_t*)_saudio.backend.src_buffer, src_buffer_byte_size)) {
            /* not enough read data available, fill the entire buffer with silence */
            memset(_saudio.backend.src_buffer, 0x0, src_buffer_byte_size);
        }
    }
}

_SOKOL_PRIVATE void SLAPIENTRY _saudio_opensles_play_cb(SLPlayItf player, void *context, SLuint32 event) {
    (void)(context);
    (void)(player);

    if (event & SL_PLAYEVENT_HEADATEND) {
        _saudio_semaphore_post(&_saudio.backend.buffer_sem, 1);
    }
}

_SOKOL_PRIVATE void* _saudio_opensles_thread_fn(void* param) {
    while (!_saudio.backend.thread_stop)  {
        /* get next output buffer, advance, next buffer. */
        int16_t* out_buffer = _saudio.backend.output_buffers[_saudio.backend.active_buffer];
        _saudio.backend.active_buffer = (_saudio.backend.active_buffer + 1) % SAUDIO_NUM_BUFFERS;
        int16_t* next_buffer = _saudio.backend.output_buffers[_saudio.backend.active_buffer];

        /* queue this buffer */
        const int buffer_size_bytes = _saudio.buffer_frames * _saudio.num_channels * sizeof(short);
        (*_saudio.backend.player_buffer_queue)->Enqueue(_saudio.backend.player_buffer_queue, out_buffer, buffer_size_bytes);

        /* fill the next buffer */
        _saudio_opensles_fill_buffer();
        const int num_samples = _saudio.num_channels * _saudio.buffer_frames;
        for (int i = 0; i < num_samples; ++i) {
            next_buffer[i] = (int16_t) (_saudio.backend.src_buffer[i] * 0x7FFF);
        }

        _saudio_semaphore_wait(&_saudio.backend.buffer_sem);
    }

    return 0;
}

_SOKOL_PRIVATE void _saudio_backend_shutdown(void) {
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

    for (int i = 0; i < SAUDIO_NUM_BUFFERS; i++) {
        SOKOL_FREE(_saudio.backend.output_buffers[i]);
    }
    SOKOL_FREE(_saudio.backend.src_buffer);
}

_SOKOL_PRIVATE bool _saudio_backend_init(void) {
    _saudio.bytes_per_frame = sizeof(float) * _saudio.num_channels;

    for (int i = 0; i < SAUDIO_NUM_BUFFERS; ++i) {
        const int buffer_size_bytes = sizeof(int16_t) * _saudio.num_channels * _saudio.buffer_frames;
        _saudio.backend.output_buffers[i] = (int16_t*) SOKOL_MALLOC(buffer_size_bytes);
        SOKOL_ASSERT(_saudio.backend.output_buffers[i]);
        memset(_saudio.backend.output_buffers[i], 0x0, buffer_size_bytes);
    }

    {
        const int buffer_size_bytes = _saudio.bytes_per_frame * _saudio.buffer_frames;
        _saudio.backend.src_buffer = (float*) SOKOL_MALLOC(buffer_size_bytes);
        SOKOL_ASSERT(_saudio.backend.src_buffer);
        memset(_saudio.backend.src_buffer, 0x0, buffer_size_bytes);
    }


    /* Create engine */
    const SLEngineOption opts[] = { SL_ENGINEOPTION_THREADSAFE, SL_BOOLEAN_TRUE };
    if (slCreateEngine(&_saudio.backend.engine_obj, 1, opts, 0, NULL, NULL ) != SL_RESULT_SUCCESS) {
        SOKOL_LOG("sokol_audio opensles: slCreateEngine failed");
        _saudio_backend_shutdown();
        return false;
    }

    (*_saudio.backend.engine_obj)->Realize(_saudio.backend.engine_obj, SL_BOOLEAN_FALSE);
    if ((*_saudio.backend.engine_obj)->GetInterface(_saudio.backend.engine_obj, SL_IID_ENGINE, &_saudio.backend.engine) != SL_RESULT_SUCCESS) {
        SOKOL_LOG("sokol_audio opensles: GetInterface->Engine failed");
        _saudio_backend_shutdown();
        return false;
    }

    /* Create output mix. */
    {
        const SLInterfaceID ids[] = { SL_IID_VOLUME };
        const SLboolean req[] = { SL_BOOLEAN_FALSE };

        if( (*_saudio.backend.engine)->CreateOutputMix(_saudio.backend.engine, &_saudio.backend.output_mix_obj, 1, ids, req) != SL_RESULT_SUCCESS)
        {
            SOKOL_LOG("sokol_audio opensles: CreateOutputMix failed");
            _saudio_backend_shutdown();
            return false;
        }
        (*_saudio.backend.output_mix_obj)->Realize(_saudio.backend.output_mix_obj, SL_BOOLEAN_FALSE);

        if((*_saudio.backend.output_mix_obj)->GetInterface(_saudio.backend.output_mix_obj, SL_IID_VOLUME, &_saudio.backend.output_mix_vol) != SL_RESULT_SUCCESS) {
            SOKOL_LOG("sokol_audio opensles: GetInterface->OutputMixVol failed");
        }
    }

    /* android buffer queue */
    _saudio.backend.in_locator.locatorType = SL_DATALOCATOR_ANDROIDSIMPLEBUFFERQUEUE;
    _saudio.backend.in_locator.numBuffers = SAUDIO_NUM_BUFFERS;

    /* data format */
    SLDataFormat_PCM format;
    format.formatType = SL_DATAFORMAT_PCM;
    format.numChannels = _saudio.num_channels;
    format.samplesPerSec = _saudio.sample_rate * 1000;
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

        (*_saudio.backend.engine)->CreateAudioPlayer(_saudio.backend.engine, &_saudio.backend.player_obj, &src, &_saudio.backend.dst_data_sink, sizeof(ids) / sizeof(ids[0]), ids, req);

        (*_saudio.backend.player_obj)->Realize(_saudio.backend.player_obj, SL_BOOLEAN_FALSE);

        (*_saudio.backend.player_obj)->GetInterface(_saudio.backend.player_obj, SL_IID_PLAY, &_saudio.backend.player);
        (*_saudio.backend.player_obj)->GetInterface(_saudio.backend.player_obj, SL_IID_VOLUME, &_saudio.backend.player_vol);

        (*_saudio.backend.player_obj)->GetInterface(_saudio.backend.player_obj, SL_IID_ANDROIDSIMPLEBUFFERQUEUE, &_saudio.backend.player_buffer_queue);
    }

    /* begin */
    {
        const int buffer_size_bytes = sizeof(int16_t) * _saudio.num_channels * _saudio.buffer_frames;
        (*_saudio.backend.player_buffer_queue)->Enqueue(_saudio.backend.player_buffer_queue, _saudio.backend.output_buffers[0], buffer_size_bytes);
        _saudio.backend.active_buffer = (_saudio.backend.active_buffer + 1) % SAUDIO_NUM_BUFFERS;

        (*_saudio.backend.player)->RegisterCallback(_saudio.backend.player, _saudio_opensles_play_cb, NULL);
        (*_saudio.backend.player)->SetCallbackEventsMask(_saudio.backend.player, SL_PLAYEVENT_HEADATEND);
        (*_saudio.backend.player)->SetPlayState(_saudio.backend.player, SL_PLAYSTATE_PLAYING);
    }

    /* create the buffer-streaming start thread */
    if (0 != pthread_create(&_saudio.backend.thread, 0, _saudio_opensles_thread_fn, 0)) {
        _saudio_backend_shutdown();
        return false;
    }

    return true;
}

#ifdef __cplusplus
} /* extern "C" */
#endif

#else /* dummy backend */
_SOKOL_PRIVATE bool _saudio_backend_init(void) { return false; };
_SOKOL_PRIVATE void _saudio_backend_shutdown(void) { };
#endif

/*=== PUBLIC API FUNCTIONS ===================================================*/
SOKOL_API_IMPL void saudio_setup(const saudio_desc* desc) {
    SOKOL_ASSERT(!_saudio.valid);
    SOKOL_ASSERT(desc);
    memset(&_saudio, 0, sizeof(_saudio));
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
        SOKOL_ASSERT(0 == (_saudio.buffer_frames % _saudio.packet_frames));
        SOKOL_ASSERT(_saudio.bytes_per_frame > 0);
        _saudio_fifo_init(&_saudio.fifo, _saudio.packet_frames * _saudio.bytes_per_frame, _saudio.num_packets);
        _saudio.valid = true;
    }
}

SOKOL_API_IMPL void saudio_shutdown(void) {
    if (_saudio.valid) {
        _saudio_backend_shutdown();
        _saudio_fifo_shutdown(&_saudio.fifo);
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

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#endif /* SOKOL_IMPL */
