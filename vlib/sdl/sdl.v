// Copyright(C) 2019 Nicolas Sauzede. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module sdl

#flag linux `sdl2-config --cflags --libs`  -lSDL2_ttf -lSDL2_mixer -lSDL2_image
#flag darwin `sdl2-config --cflags --libs`  -lSDL2_ttf -lSDL2_mixer -lSDL2_image

//#flag windows `sdl2-config --cflags`
//#flag windows `sdl2-config --libs`  -lSDL2_ttf -lSDL2_mixer -lSDL2_image
//#flag `sdl2-config --cflags --libs`  -lSDL2_ttf -lSDL2_mixer -lSDL2_image

#flag -DSDL_DISABLE_IMMINTRIN_H

// following kludge until `sdl2-config ...` is supported also on windows
#flag windows -I/msys64/mingw64/include/SDL2
#flag windows -Dmain=SDL_main
#flag windows -L/mingw64/lib -lmingw32 -lSDL2main -lSDL2 -lSDL2_ttf -lSDL2_mixer -lSDL2_image

#include <SDL.h>


pub struct C.SDL_RWops {}
pub struct C.SDL_Window {}
pub struct C.SDL_Renderer {}
pub struct C.SDL_Texture {}

pub struct C.SDL_Color{
pub:
        r byte
        g byte
        b byte
        a byte
}

pub struct C.SDL_Rect {
pub mut:
	x int
	y int
	w int
	h int
}

pub struct C.SDL_Surface {
pub:
	flags u32
	format voidptr
	w int
	h int
	pitch int
	pixels voidptr
	userdata voidptr
	locked int
	lock_data voidptr
	clip_rect SDL_Rect
	map voidptr
	refcount int
}


struct Keysym {
pub:
        scancode int                       /**< hardware specific scancode */
        sym int                            /**< SDL virtual keysym */
        mod u16                            /**< current key modifiers */
        unused u32                         /**< translated character */
}

pub struct C.SDL_AudioSpec {
pub mut:
        freq int                           /**< DSP frequency -- samples per second */
        format u16                         /**< Audio data format */
        channels byte                      /**< Number of channels: 1 mono, 2 stereo */
        silence byte                       /**< Audio buffer silence value (calculated) */
        samples u16                        /**< Audio buffer size in samples (power of 2) */
        size u32                           /**< Necessary for some compile environments */
        callback voidptr
        userdata voidptr
}

// pub struct RwOps {
// pub:
// mut:
//         seek voidptr
//         read voidptr
//         write voidptr
//         close voidptr
//         type_ u32
//         hidden voidptr
// }
//type AudioSpec C.voidptrioSpec

fn C.atexit(func fn ())

///////////////////////////////////////////////////
fn C.SDL_MapRGB(fmt voidptr byte, g byte, b byte) u32
fn C.SDL_CreateRGBSurface(flags u32, width int, height int, depth int, Rmask u32, Gmask u32, Bmask u32, Amask u32) voidptr
fn C.SDL_PollEvent(&SDL_Event) int
fn C.SDL_NumJoysticks() int
fn C.SDL_JoystickNameForIndex(device_index int) voidptr
fn C.SDL_RenderCopy(renderer voidptr, texture voidptr, srcrect voidptr, dstrect voidptr) int
fn C.SDL_CreateWindow(title byteptr, x int, y int, w int, h int, flags u32) voidptr
fn C.SDL_CreateRenderer(window &SDL_Window, index int, flags u32) voidptr
fn C.SDL_CreateWindowAndRenderer(width int, height int, window_flags u32, window &voidptr, renderer &voidptr) int
fn C.SDL_DestroyWindow(window voidptr)
fn C.SDL_DestroyRenderer(renderer voidptr)
fn C.SDL_GetWindowSize(window voidptr, w voidptr, h voidptr)
fn C.SDL_SetHint(name byteptr, value byteptr) C.SDL_bool
//fn C.SDL_RWFromFile(byteptr, byteptr) &RwOps
//fn C.SDL_CreateTextureFromSurface(renderer &C.SDL_Renderer, surface &C.SDL_Surface) &C.SDL_Texture
fn C.SDL_CreateTextureFromSurface(renderer voidptr, surface voidptr) voidptr
fn C.SDL_CreateTexture(renderer voidptr, format u32, access int, w int, h int) voidptr
fn C.SDL_FillRect(dst voidptr, dstrect voidptr, color u32) int
fn C.SDL_SetRenderDrawColor(renderer voidptr, r byte, g byte, b byte, a byte)
fn C.SDL_RenderPresent(renderer voidptr)
fn C.SDL_RenderClear(renderer voidptr) int
fn C.SDL_UpdateTexture(texture voidptr, rect voidptr, pixels voidptr, pitch int) int
fn C.SDL_QueryTexture(texture voidptr, format voidptr, access voidptr, w voidptr, h voidptr) int
fn C.SDL_DestroyTexture(texture voidptr)
fn C.SDL_FreeSurface(surface voidptr)
fn C.SDL_Init(flags u32) int
fn C.SDL_Quit()
fn C.SDL_SetWindowTitle(window voidptr, title byteptr)
// following is wrong : SDL_Zero is a macro accepting an argument
fn C.SDL_zero()
fn C.SDL_LoadWAV(file byteptr, spec voidptr, audio_buf voidptr, audio_len voidptr) voidptr
fn C.SDL_FreeWAV(audio_buf voidptr)
fn C.SDL_OpenAudio(desired voidptr, obtained voidptr) int
fn C.SDL_CloseAudio()
fn C.SDL_PauseAudio(pause_on int)
fn C.SDL_JoystickOpen(device_index int) int
fn C.SDL_JoystickEventState(state int) int

//////////////////////////////////////////////////////////
// SDL_Timer.h
//////////////////////////////////////////////////////////
fn C.SDL_GetTicks() u32
fn C.SDL_TICKS_PASSED(a,b u32) bool
fn C.SDL_GetPerformanceCounter() u64
fn C.SDL_GetPerformanceFrequency() u64
fn C.SDL_Delay(ms u32)

//////////////////////////////////////////////////////////
// GL
//////////////////////////////////////////////////////////
fn C.SDL_GL_SetAttribute(attr int, value int) int
fn C.SDL_GL_CreateContext(window voidptr) voidptr
fn C.SDL_GL_MakeCurrent(window voidptr, context voidptr) int
fn C.SDL_GL_SetSwapInterval(interval int) int
fn C.SDL_GL_SwapWindow(window voidptr)
fn C.SDL_GL_DeleteContext(context voidptr)

pub fn create_texture_from_surface(renderer voidptr, surface &SDL_Surface) voidptr {
	return C.SDL_CreateTextureFromSurface(renderer, voidptr(surface))
}

pub fn create_window_and_renderer(width int, height int, window_flags u32, window voidptr, renderer voidptr) int {
	return C.SDL_CreateWindowAndRenderer(width, height, window_flags, window, renderer)
}

pub fn joystick_name_for_index(device_index int) byteptr {
	return byteptr(C.SDL_JoystickNameForIndex(device_index))
}

pub fn fill_rect(screen &SDL_Surface, rect &SDL_Rect, _col &SDL_Color) {
	col := C.SDL_MapRGB(screen.format, _col.r, _col.g, _col.b)
	_screen := voidptr(screen)
	_rect := voidptr(rect)
	C.SDL_FillRect(_screen, _rect, col)
}

pub fn create_rgb_surface(flags u32, width int, height int, depth int, rmask u32, gmask u32, bmask u32, amask u32) &SDL_Surface {
	res := C.SDL_CreateRGBSurface(flags, width, height, depth, rmask, gmask, bmask, amask)
	return res
}

pub fn render_copy(renderer voidptr, texture voidptr, srcrect &SDL_Rect, dstrect &SDL_Rect) int {
	_srcrect := voidptr(srcrect)
	_dstrect := voidptr(dstrect)
	return C.SDL_RenderCopy(renderer, texture, _srcrect, _dstrect)
}

pub fn poll_event(event &C.SDL_Event) int {
	return C.SDL_PollEvent(event)
}

pub fn destroy_texture(text voidptr) {
        C.SDL_DestroyTexture(text)
}

pub fn free_surface(surf &SDL_Surface) {
	_surf := voidptr(surf)
        C.SDL_FreeSurface(_surf)
}

pub fn get_ticks() u32 {
        return C.SDL_GetTicks()
}

pub fn ticks_passed(a, b u32) bool {
        return C.SDL_TICKS_PASSED(a,b)
}

pub fn get_perf_counter() u64 {
        return C.SDL_GetPerformanceCounter()
}

pub fn get_perf_frequency() u64 {
        return C.SDL_GetPerformanceFrequency()
}

pub fn delay(ms u32) {
        C.SDL_Delay(ms)
}

pub const (
  version = '0.2' // hack to avoid unused module warning in the main program
)
