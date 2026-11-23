#ifndef V_SOKOL_AUDIO_TEST_BACKEND_H
#define V_SOKOL_AUDIO_TEST_BACKEND_H

/* System ALSA is uninstrumented; keep exercising setup/shutdown under MSan. */
#if defined(__has_feature)
#if __has_feature(memory_sanitizer)
#define SOKOL_DUMMY_BACKEND
#endif
#endif

#endif
