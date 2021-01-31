module gg

#include "@VROOT/vlib/gg/gg_darwin.m"
fn C.gg_get_screen_size() Size

fn C.darwin_draw_string(x int, y int, s string, cfg voidptr)

fn C.darwin_text_width(s string) int

fn C.darwin_window_refresh()

fn C.darwin_draw_rect(f32, f32, f32, f32, voidptr)

fn C.darwin_create_image(path string) Image

fn C.darwin_draw_image(f32, f32, f32, f32, &Image)

fn C.darwin_draw_circle(f32, f32, f32, voidptr)

//, gx.Color c)
