#ifndef V_MULTIWINDOW_APPKIT_BACKEND_HELPERS_H
#define V_MULTIWINDOW_APPKIT_BACKEND_HELPERS_H

#ifdef __cplusplus
extern "C" {
#endif

int v_multiwindow_appkit_is_main_thread(void);
int v_multiwindow_appkit_prepare_application(void);
int v_multiwindow_appkit_create_metal_device(void **out_device);
void v_multiwindow_appkit_release_metal_device(void *device);
int v_multiwindow_appkit_create_window(void *device_ptr, const char *title, int width, int height, int min_width, int min_height, int resizable, int visible, int high_dpi, int borderless, int fullscreen, void **out_state, int *out_width, int *out_height, int *out_framebuffer_width, int *out_framebuffer_height);
void v_multiwindow_appkit_destroy_window(void *state_ptr);
void v_multiwindow_appkit_release_window(void *state_ptr);
int v_multiwindow_appkit_set_window_title(void *state_ptr, const char *title);
int v_multiwindow_appkit_resize_window(void *state_ptr, int width, int height, int *out_width, int *out_height, int *out_framebuffer_width, int *out_framebuffer_height);
void v_multiwindow_appkit_poll_events(void);
int v_multiwindow_appkit_take_close_requested(void *state_ptr);
int v_multiwindow_appkit_take_resized(void *state_ptr, int *out_width, int *out_height, int *out_framebuffer_width, int *out_framebuffer_height);
int v_multiwindow_appkit_take_destroyed(void *state_ptr);
int v_multiwindow_appkit_begin_frame(void *state_ptr, void *device_ptr, void **out_drawable, void **out_depth_texture, int *out_framebuffer_width, int *out_framebuffer_height);
void v_multiwindow_appkit_end_frame(void *state_ptr);
void v_multiwindow_appkit_abort_frame(void *state_ptr);

#ifdef __cplusplus
}
#endif

#endif
