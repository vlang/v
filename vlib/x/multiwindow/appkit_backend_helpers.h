#ifndef V_MULTIWINDOW_APPKIT_BACKEND_HELPERS_H
#define V_MULTIWINDOW_APPKIT_BACKEND_HELPERS_H

#include <stdint.h>

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
int v_multiwindow_appkit_create_metal_device(void **out_device);
void v_multiwindow_appkit_release_metal_device(void *device);
int v_multiwindow_appkit_create_window(void *device_ptr, const char *title, int width, int height, int min_width, int min_height, int resizable, int visible, int high_dpi, int borderless, int fullscreen, void **out_state, int *out_width, int *out_height, int *out_framebuffer_width, int *out_framebuffer_height);
void v_multiwindow_appkit_destroy_window(void *state_ptr);
void v_multiwindow_appkit_release_window(void *state_ptr);
int v_multiwindow_appkit_set_window_title(void *state_ptr, const char *title);
int v_multiwindow_appkit_set_cursor_shape(void *state_ptr, int shape);
int v_multiwindow_appkit_resize_window(void *state_ptr, int width, int height, int *out_width, int *out_height, int *out_framebuffer_width, int *out_framebuffer_height);
void v_multiwindow_appkit_poll_events(void);
int v_multiwindow_appkit_take_queued_event(void *state_ptr, VMultiwindowAppKitQueuedEvent *out_event);
void v_multiwindow_appkit_release_queued_event_resources(VMultiwindowAppKitQueuedEvent *event);
int v_multiwindow_appkit_begin_frame(void *state_ptr, void *device_ptr, void **out_drawable, void **out_depth_texture, int *out_framebuffer_width, int *out_framebuffer_height);
void v_multiwindow_appkit_end_frame(void *state_ptr);
void v_multiwindow_appkit_abort_frame(void *state_ptr);

#ifdef __cplusplus
}
#endif

#endif
