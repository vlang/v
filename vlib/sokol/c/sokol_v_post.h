#ifndef V_SOKOL_POST_DECLARATIONS_H
#define V_SOKOL_POST_DECLARATIONS_H

#if defined(SOKOL_APP_INCLUDED)
SOKOL_APP_API_DECL void v_sapp_get_environment(sapp_environment* out_env);
SOKOL_APP_API_DECL void v_sapp_get_swapchain(sapp_swapchain* out_swapchain);
#endif
int v_sapp_read_rgba_pixels(int x, int y, int width, int height, unsigned char* pixels);

#endif
