#ifndef V_MULTIWINDOW_APPKIT_BACKEND_HELPERS_H
#define V_MULTIWINDOW_APPKIT_BACKEND_HELPERS_H

#include <stddef.h>
#include <stdint.h>
#include "native_render_result.h"

#ifdef __cplusplus
extern "C" {
#endif

#define V_MULTIWINDOW_APPKIT_EVENT_LIFECYCLE 1
#define V_MULTIWINDOW_APPKIT_EVENT_INPUT 2
#define V_MULTIWINDOW_APPKIT_EVENT_SERVICE 3

#define V_MULTIWINDOW_APPKIT_SERVICE_EVENT_STATE 1
#define V_MULTIWINDOW_APPKIT_SERVICE_EVENT_METRICS 2

#define V_MULTIWINDOW_APPKIT_SERVICE_RESULT_OK 1
#define V_MULTIWINDOW_APPKIT_SERVICE_RESULT_UNAVAILABLE 0
#define V_MULTIWINDOW_APPKIT_SERVICE_RESULT_FAILED -1
#define V_MULTIWINDOW_APPKIT_SERVICE_RESULT_CAPACITY -2
#define V_MULTIWINDOW_APPKIT_SERVICE_RESULT_INDETERMINATE -3

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
	int service_snapshot_valid;
	int service_kind;
	int service_operation;
	int service_mapping;
	int service_visibility;
	int service_active;
	int service_focused;
	int service_minimized;
	int service_maximized;
	int service_fullscreen;
	int service_mouse_locked;
	int service_position_known;
	int service_x;
	int service_y;
	uint64_t service_monitor_native_id;
	float service_scale;
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
int v_multiwindow_appkit_render_snapshot(void *state_ptr, int *out_visible,
	int *out_miniaturized, int *out_occluded, int *out_width, int *out_height,
	int *out_framebuffer_width, int *out_framebuffer_height, float *out_scale);
int v_multiwindow_appkit_logical_to_pixel_rect(void *state_ptr, float x, float y,
	float width, float height, int *out_x, int *out_y, int *out_width,
	int *out_height);
int v_multiwindow_appkit_pixel_to_logical_rect(void *state_ptr, int x, int y,
	int width, int height, float *out_x, float *out_y, float *out_width,
	float *out_height);

uint32_t v_multiwindow_appkit_service_abi_version(void);
int v_multiwindow_appkit_service_capability(void *state_ptr, int operation,
	int renderer_ready);
int v_multiwindow_appkit_service_window_state(void *state_ptr, int *out_mapping,
	int *out_visibility, int *out_active, int *out_focused, int *out_minimized,
	int *out_maximized, int *out_fullscreen, int *out_mouse_locked,
	int *out_position_known, int *out_x, int *out_y,
	uint64_t *out_monitor_native_id, float *out_scale);
int v_multiwindow_appkit_service_show_window(void *state_ptr);
int v_multiwindow_appkit_service_hide_window(void *state_ptr);
int v_multiwindow_appkit_service_focus_window(void *state_ptr);
int v_multiwindow_appkit_service_raise_window(void *state_ptr);
int v_multiwindow_appkit_service_set_window_position(void *state_ptr, int x, int y);
int v_multiwindow_appkit_service_minimize_window(void *state_ptr);
int v_multiwindow_appkit_service_maximize_window(void *state_ptr);
int v_multiwindow_appkit_service_restore_window(void *state_ptr);
int v_multiwindow_appkit_service_set_fullscreen(void *state_ptr, int enabled);
uint64_t v_multiwindow_appkit_service_monitor_revision(void);
int v_multiwindow_appkit_service_monitor_count(void);
int v_multiwindow_appkit_service_monitor_info(int index, uint64_t *out_native_id,
	int *out_x, int *out_y, int *out_width, int *out_height, int *out_work_x,
	int *out_work_y, int *out_work_width, int *out_work_height, float *out_scale,
	int *out_primary, size_t *out_name_length);
int v_multiwindow_appkit_service_copy_monitor_name(int index, char *out_name,
	size_t capacity);
int v_multiwindow_appkit_service_set_clipboard_text(void *state_ptr,
	const char *text, size_t text_length);
int v_multiwindow_appkit_service_clipboard_text_length(void *state_ptr,
	size_t *out_length);
int v_multiwindow_appkit_service_copy_clipboard_text(void *state_ptr,
	char *out_text, size_t capacity);
int v_multiwindow_appkit_service_set_owner(void *state_ptr, void *owner_state_ptr,
	int modal);
int v_multiwindow_appkit_service_clear_owner(void *state_ptr);
void *v_multiwindow_appkit_service_native_window(void *state_ptr);
int v_multiwindow_appkit_service_set_mouse_lock(void *state_ptr, int enabled);
int v_multiwindow_appkit_service_set_titlebar_appearance(void *state_ptr,
	int appearance);
int v_multiwindow_appkit_service_detach_accessibility(void *state_ptr);
int v_multiwindow_appkit_service_arm_offscreen_readback_pass(void *state_ptr,
	void *texture_ptr, uint64_t pass_serial, uint64_t producing_frame);
int v_multiwindow_appkit_service_stage_window_readback(void *state_ptr,
	uint64_t request, int x, int y, int width, int height, uint64_t producing_frame);
int v_multiwindow_appkit_service_stage_image_readback(void *state_ptr,
	void *texture_ptr, uint64_t request, int x, int y, int width, int height,
	uint64_t producing_frame);
int v_multiwindow_appkit_service_resolve_readbacks_after_submit(void *state_ptr,
	uint64_t submitted_frame, int submission_succeeded);
int v_multiwindow_appkit_service_take_readback_result(void *state_ptr,
	uint64_t *out_request, int *out_status, int *out_width, int *out_height,
	int *out_stride, uint64_t *out_submitted_frame, size_t *out_byte_length);
int v_multiwindow_appkit_service_copy_readback_pixels(void *state_ptr,
	uint64_t request, uint8_t *out_pixels, size_t capacity);
int v_multiwindow_appkit_service_release_readback_result(void *state_ptr,
	uint64_t request);
int v_multiwindow_appkit_service_cancel_readback(void *state_ptr, uint64_t request);
int v_multiwindow_appkit_service_cancel_all_readbacks(void *state_ptr);
int v_multiwindow_appkit_service_release_window_services(void *state_ptr);

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
