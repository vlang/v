#ifndef V_MULTIWINDOW_EVENT_SEQUENCE_EXHAUSTION_TEST_HELPERS_H
#define V_MULTIWINDOW_EVENT_SEQUENCE_EXHAUSTION_TEST_HELPERS_H

#include <stdint.h>

#if defined(_WIN32)

static uint64_t v_multiwindow_win32_event_sequence;
static int v_multiwindow_win32_event_sequence_exhausted_flag;
static inline uint64_t v_multiwindow_win32_next_event_sequence(void);

static inline uint64_t v_multiwindow_test_event_sequence_current(void) {
	return v_multiwindow_win32_event_sequence;
}

static inline int v_multiwindow_test_event_sequence_exhausted(void) {
	return v_multiwindow_win32_event_sequence_exhausted_flag;
}

static inline void v_multiwindow_test_set_event_sequence(uint64_t sequence, int exhausted) {
	v_multiwindow_win32_event_sequence = sequence;
	v_multiwindow_win32_event_sequence_exhausted_flag = exhausted;
}

static inline uint64_t v_multiwindow_test_next_event_sequence(void) {
	return v_multiwindow_win32_next_event_sequence();
}

#elif defined(__APPLE__)

#import <objc/objc.h>

static uint64_t v_multiwindow_appkit_event_sequence;
static int v_multiwindow_appkit_event_sequence_exhausted_flag;
static uint64_t v_multiwindow_appkit_next_event_sequence(void);

@protocol VMultiwindowEventSequenceTestState
- (void)clearQueuedEvents;
- (void)queueLifecycleEvent:(int)kind;
@end

static inline uint64_t v_multiwindow_test_event_sequence_current(void) {
	return v_multiwindow_appkit_event_sequence;
}

static inline int v_multiwindow_test_event_sequence_exhausted(void) {
	return v_multiwindow_appkit_event_sequence_exhausted_flag;
}

static inline void v_multiwindow_test_set_event_sequence(uint64_t sequence, int exhausted) {
	v_multiwindow_appkit_event_sequence = sequence;
	v_multiwindow_appkit_event_sequence_exhausted_flag = exhausted;
}

static inline uint64_t v_multiwindow_test_next_event_sequence(void) {
	return v_multiwindow_appkit_next_event_sequence();
}

static inline void v_multiwindow_test_appkit_clear_events(void *state_ptr) {
	id<VMultiwindowEventSequenceTestState> state = (__bridge id<VMultiwindowEventSequenceTestState>)state_ptr;
	[state clearQueuedEvents];
}

static inline void v_multiwindow_test_appkit_queue_lifecycle(void *state_ptr, int kind) {
	id<VMultiwindowEventSequenceTestState> state = (__bridge id<VMultiwindowEventSequenceTestState>)state_ptr;
	[state queueLifecycleEvent:kind];
}

#else

static uint64_t v_multiwindow_wayland_event_sequence;
static int v_multiwindow_wayland_event_sequence_exhausted_flag;
static inline uint64_t v_multiwindow_wayland_next_event_sequence(void);

static inline uint64_t v_multiwindow_test_event_sequence_current(void) {
	return v_multiwindow_wayland_event_sequence;
}

static inline int v_multiwindow_test_event_sequence_exhausted(void) {
	return v_multiwindow_wayland_event_sequence_exhausted_flag;
}

static inline void v_multiwindow_test_set_event_sequence(uint64_t sequence, int exhausted) {
	v_multiwindow_wayland_event_sequence = sequence;
	v_multiwindow_wayland_event_sequence_exhausted_flag = exhausted;
}

static inline uint64_t v_multiwindow_test_next_event_sequence(void) {
	return v_multiwindow_wayland_next_event_sequence();
}

#endif

#endif
