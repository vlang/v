#ifndef V_GG_DARWIN_ABI_H
#define V_GG_DARWIN_ABI_H
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
typedef struct v_gg_color { uint8_t r, g, b, a; } v_gg_color;
typedef struct v_gg_string { const char* str; size_t len; } v_gg_string;
typedef struct v_gg_text_cfg { v_gg_color color; int32_t size; bool bold, mono; } v_gg_text_cfg;
typedef struct v_gg_size { int32_t width, height; } v_gg_size;
typedef struct v_gg_image { int32_t width, height; bool ok; void* data; } v_gg_image;
#endif
