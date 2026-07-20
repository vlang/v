#ifndef V_MULTIWINDOW_APPKIT_BACKEND_HELPERS_H
#define V_MULTIWINDOW_APPKIT_BACKEND_HELPERS_H

#include <stdint.h>
#include "native_render_result.h"

#ifdef __cplusplus
extern "C" {
#endif

#define V_MULTIWINDOW_APPKIT_EVENT_LIFECYCLE 1
#define V_MULTIWINDOW_APPKIT_EVENT_INPUT 2

#define V_MULTIWINDOW_APPKIT_LIFECYCLE_CLOSE_REQUESTED 1
#define V_MULTIWINDOW_APPKIT_LIFECYCLE_DESTROYED 2
#define V_MULTIWINDOW_APPKIT_LIFECYCLE_RESIZED 3

#define V_MULTIWINDOW_APPKIT_INPUT_KEY_DOWN 1
#define V_MULTIWINDOW_APPKIT_INPUT_KEY_UP 2
#define V_MULTIWINDOW_APPKIT_INPUT_CHAR 3
#define V_MULTIWINDOW_APPKIT_INPUT_MOUSE_DOWN 4
#define V_MULTIWINDOW_APPKIT_INPUT_MOUSE_UP 5
#define V_MULTIWINDOW_APPKIT_INPUT_MOUSE_SCROLL 6
#define V_MULTIWINDOW_APPKIT_INPUT_MOUSE_MOVE 7
#define V_MULTIWINDOW_APPKIT_INPUT_MOUSE_ENTER 8
#define V_MULTIWINDOW_APPKIT_INPUT_MOUSE_LEAVE 9
#define V_MULTIWINDOW_APPKIT_INPUT_FOCUSED 10
#define V_MULTIWINDOW_APPKIT_INPUT_UNFOCUSED 11
#define V_MULTIWINDOW_APPKIT_INPUT_RESIZED 12
#define V_MULTIWINDOW_APPKIT_INPUT_ICONIFIED 13
#define V_MULTIWINDOW_APPKIT_INPUT_RESTORED 14
#define V_MULTIWINDOW_APPKIT_INPUT_CLIPBOARD_PASTED 15
#define V_MULTIWINDOW_APPKIT_INPUT_FILES_DROPPED 16
#define V_MULTIWINDOW_APPKIT_INPUT_TOUCHES_BEGAN 17
#define V_MULTIWINDOW_APPKIT_INPUT_TOUCHES_MOVED 18
#define V_MULTIWINDOW_APPKIT_INPUT_TOUCHES_ENDED 19
#define V_MULTIWINDOW_APPKIT_INPUT_TOUCHES_CANCELLED 20

#define V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_LEFT 0
#define V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_RIGHT 1
#define V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_MIDDLE 2
#define V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_INVALID 256
#define V_MULTIWINDOW_APPKIT_MAX_TOUCH_POINTS 8

#define V_MULTIWINDOW_APPKIT_MODIFIER_LMB 0x100
#define V_MULTIWINDOW_APPKIT_MODIFIER_RMB 0x200
#define V_MULTIWINDOW_APPKIT_MODIFIER_MMB 0x400

typedef struct VMultiwindowAppKitQueuedEvent {
	uint64_t sequence;
	int event_kind;
	int lifecycle_kind;
	int input_kind;
	int key_code;
	uint32_t char_code;
	int key_repeat;
	uint32_t modifiers;
	int mouse_button;
	float mouse_x;
	float mouse_y;
	float mouse_dx;
	float mouse_dy;
	float scroll_x;
	float scroll_y;
	int window_width;
	int window_height;
	int framebuffer_width;
	int framebuffer_height;
	int dropped_file_count;
	char **dropped_files;
	int touch_count;
	uint64_t touch_ids[V_MULTIWINDOW_APPKIT_MAX_TOUCH_POINTS];
	float touch_x[V_MULTIWINDOW_APPKIT_MAX_TOUCH_POINTS];
	float touch_y[V_MULTIWINDOW_APPKIT_MAX_TOUCH_POINTS];
	int touch_changed[V_MULTIWINDOW_APPKIT_MAX_TOUCH_POINTS];
} VMultiwindowAppKitQueuedEvent;

int v_multiwindow_appkit_is_main_thread(void);
int v_multiwindow_appkit_prepare_application(void);
VMultiwindowNativePrimitive v_multiwindow_appkit_create_metal_device(void);
VMultiwindowNativePrimitive v_multiwindow_appkit_release_metal_device(void *device);
VMultiwindowNativePrimitive v_multiwindow_appkit_create_window(void *device_ptr, const char *title, int width, int height, int min_width, int min_height, int resizable, int visible, int high_dpi, int borderless, int fullscreen, int *out_width, int *out_height, int *out_framebuffer_width, int *out_framebuffer_height);
VMultiwindowNativePrimitive v_multiwindow_appkit_configure_window_device(void *state_ptr, void *device_ptr);
VMultiwindowNativePrimitive v_multiwindow_appkit_destroy_window(void *state_ptr);
VMultiwindowNativePrimitive v_multiwindow_appkit_release_window(void *state_ptr);
int v_multiwindow_appkit_set_window_title(void *state_ptr, const char *title);
int v_multiwindow_appkit_set_cursor_shape(void *state_ptr, int shape);
int v_multiwindow_appkit_resize_window(void *state_ptr, int width, int height, int *out_width, int *out_height, int *out_framebuffer_width, int *out_framebuffer_height);
void v_multiwindow_appkit_poll_events(void);
int v_multiwindow_appkit_event_sequence_exhausted(void);
int v_multiwindow_appkit_take_queued_event(void *state_ptr, VMultiwindowAppKitQueuedEvent *out_event);
void v_multiwindow_appkit_release_queued_event_resources(VMultiwindowAppKitQueuedEvent *event);
VMultiwindowNativePrimitive v_multiwindow_appkit_create_renderer_anchor(void *device_ptr);
VMultiwindowNativePrimitive v_multiwindow_appkit_destroy_renderer_anchor(void *state_ptr);
VMultiwindowNativePrimitive v_multiwindow_appkit_begin_frame(void *state_ptr, void *device_ptr);
VMultiwindowNativePrimitive v_multiwindow_appkit_end_frame(void *state_ptr, void *drawable_ptr);
VMultiwindowNativePrimitive v_multiwindow_appkit_abort_frame(void *state_ptr, void *drawable_ptr);
VMultiwindowNativePrimitive v_multiwindow_appkit_release_drawable(void *state_ptr, void *drawable_ptr);
VMultiwindowNativePrimitive v_multiwindow_appkit_begin_render_batch(void);
VMultiwindowNativePrimitive v_multiwindow_appkit_end_render_batch(void *pool);

#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
#define V_MULTIWINDOW_APPKIT_SIDE_EFFECT_CAPACITY 256

enum VMultiwindowAppKitSideEffectKind {
	V_MULTIWINDOW_APPKIT_SIDE_EFFECT_BRIDGE_RETAIN = 1,
	V_MULTIWINDOW_APPKIT_SIDE_EFFECT_BRIDGE_RELEASE = 2,
	V_MULTIWINDOW_APPKIT_SIDE_EFFECT_RELEASE_PROBE_DEALLOC = 3,
	V_MULTIWINDOW_APPKIT_SIDE_EFFECT_POOL_PUSH = 4,
	V_MULTIWINDOW_APPKIT_SIDE_EFFECT_POOL_PROBE_CREATE = 5,
	V_MULTIWINDOW_APPKIT_SIDE_EFFECT_POOL_PROBE_DEALLOC = 6,
	V_MULTIWINDOW_APPKIT_SIDE_EFFECT_POOL_POP = 7,
	V_MULTIWINDOW_APPKIT_SIDE_EFFECT_NEXT_DRAWABLE = 8,
	V_MULTIWINDOW_APPKIT_SIDE_EFFECT_CURRENT_DRAWABLE_CLEAR = 9,
	V_MULTIWINDOW_APPKIT_SIDE_EFFECT_LAYER_DEVICE_SET_READ = 10
};

enum VMultiwindowAppKitSideEffectSubject {
	V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_NONE = 0,
	V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_DEVICE_ROOT = 1,
	V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_WINDOW_ROOT = 2,
	V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_ANCHOR_ROOT = 3,
	V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_POOL = 4,
	V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_DRAWABLE = 5,
	V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_LAYER = 6
};

typedef struct VMultiwindowAppKitSideEffectRecord {
	uint64_t generation;
	uint64_t sequence;
	uint64_t kind;
	uint64_t subject;
	uint64_t identity;
	uint64_t parent_identity;
	uint64_t before_identity;
	uint64_t after_identity;
	uint64_t auxiliary_identity;
	uint64_t thread_identity;
	uint64_t main_thread;
} VMultiwindowAppKitSideEffectRecord;

uint64_t v_multiwindow_appkit_side_effect_reset(void);
uint64_t v_multiwindow_appkit_side_effect_generation(void);
uint64_t v_multiwindow_appkit_side_effect_count(void);
int v_multiwindow_appkit_side_effect_overflow(void);
int v_multiwindow_appkit_side_effect_record(uint64_t index,
	VMultiwindowAppKitSideEffectRecord *out_record);
void *v_multiwindow_appkit_side_effect_create_release_probe(uint64_t subject);
uint64_t v_multiwindow_appkit_side_effect_probe_generation(void *probe_ptr);
uint64_t v_multiwindow_appkit_side_effect_probe_subject(void *probe_ptr);

void *v_multiwindow_appkit_native_proof_install_physical_nil_drawable(
	void *state_ptr,
	void *expected_layer_ptr,
	void *expected_device_ptr,
	uint64_t expected_owner_thread);
int v_multiwindow_appkit_native_proof_restore_physical_nil_drawable(
	void *lease_ptr,
	uint64_t expected_owner_thread);
#endif

#ifdef __cplusplus
}
#endif

#endif
