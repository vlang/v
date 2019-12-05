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
#include <SDL_ttf.h>
#include <SDL_mixer.h>


//struct C.SDL_Color{
pub struct Color{
pub:
        r byte                              /**< Red value 0-255 */
        g byte                              /**< Green value 0-255 */
        b byte                              /**< Blue value 0-255 */
        a byte                              /**< Alpha value 0-255 */
}
//type Color C.SDL_Color

pub struct C.SDL_Color{
pub:
        r byte
        g byte
        b byte
        a byte
}

//struct C.SDL_Rect {
pub struct Rect {
pub:
        x int                               /**< number of pixels from left side of screen */
        y int                               /**< num of pixels from top of screen */
        w int                               /**< width of rectangle */
        h int                               /**< height of rectangle */
}
//type Rect C.SDL_Rect

//pub struct C.SDL_Surface {
pub struct Surface {
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
        clip_rect Rect
        map voidptr
        refcount int
}
//type Surface C.SDL_Surface
//type Surface Surface

/////////////////////////////////////////////////////////

struct QuitEvent {
        _type u32                          /**< SDL_QUIT */
        timestamp u32
}
struct Keysym {
pub:
        scancode int                       /**< hardware specific scancode */
        sym int                            /**< SDL virtual keysym */
        mod u16                            /**< current key modifiers */
        unused u32                         /**< translated character */
}
struct KeyboardEvent {
pub:
        _type u32                          /**< SDL_KEYDOWN or SDL_KEYUP */
        timestamp u32
        windowid u32
        state byte                         /**< SDL_PRESSED or SDL_RELEASED */
        repeat byte
        padding2 byte
        padding3 byte
        keysym Keysym
}
struct JoyButtonEvent {
pub:
        _type u32                          /**< SDL_JOYBUTTONDOWN or SDL_JOYBUTTONUP */
        timestamp u32
        which int                          /**< The joystick device index */
        button byte                        /**< The joystick button index */
        state byte                         /**< SDL_PRESSED or SDL_RELEASED */
}
struct JoyHatEvent {
pub:
        _type u32                          /**< SDL_JOYHATMOTION */
        timestamp u32
        which int                          /**< The joystick device index */
        hat byte                           /**< The joystick hat index */
        value byte                         /**< The hat position value:
			                   *   SDL_HAT_LEFTUP   SDL_HAT_UP       SDL_HAT_RIGHTUP
			                   *   SDL_HAT_LEFT     SDL_HAT_CENTERED SDL_HAT_RIGHT
			                   *   SDL_HAT_LEFTDOWN SDL_HAT_DOWN     SDL_HAT_RIGHTDOWN
			                   *  Note that zero means the POV is centered.
			                   */
}

//pub union EventU {
pub union Event {
pub:
        _type u32
        quit QuitEvent
        key KeyboardEvent
        jbutton JoyButtonEvent
        jhat JoyHatEvent
        _pad56 [56]byte
}
//type Event EventU


//struct C.SDL_AudioSpec {
pub struct AudioSpec {
pub:
mut:
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

type atexit_func_t fn ()
fn C.atexit(atexit_func_t)

///////////////////////////////////////////////////
fn C.SDL_MapRGB(fmt voidptr byte, g byte, b byte) u32
fn C.SDL_CreateRGBSurface(flags u32, width int, height int, depth int, Rmask u32, Gmask u32, Bmask u32, Amask u32) voidptr
fn C.SDL_PollEvent(&Event) int
fn C.SDL_NumJoysticks() int
fn C.SDL_JoystickNameForIndex(device_index int) voidptr
fn C.SDL_RenderCopy(renderer voidptr, texture voidptr, srcrect voidptr, dstrect voidptr) int
fn C.SDL_CreateWindow(title byteptr, x int, y int, w int, h int, flags u32) voidptr
fn C.SDL_CreateWindowAndRenderer(width int, height int, window_flags u32, window &voidptr, renderer &voidptr) int
fn C.SDL_DestroyWindow(window voidptr)
fn C.SDL_GetWindowSize(window voidptr, w voidptr, h voidptr)
fn C.SDL_SetHint(name byteptr, value byteptr) C.SDL_bool
//fn C.SDL_RWFromFile(byteptr, byteptr) &RwOps
//fn C.SDL_CreateTextureFromSurface(renderer &C.SDL_Renderer, surface &C.SDL_Surface) &C.SDL_Texture
fn C.SDL_CreateTextureFromSurface(renderer voidptr, surface voidptr) voidptr
fn C.SDL_CreateTexture(renderer voidptr, format u32, access int, w int, h int) voidptr
fn C.SDL_FillRect(dst voidptr, dstrect voidptr, color u32) int
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
// TTF
//////////////////////////////////////////////////////////
fn C.TTF_Init() int
fn C.TTF_Quit()
fn C.TTF_OpenFont(file byteptr, ptsize int) voidptr
fn C.TTF_CloseFont(font voidptr)
//fn C.TTF_RenderText_Solid(voidptr, voidptr, SdlColor) voidptr
fn C.TTF_RenderText_Solid(voidptr, voidptr, C.SDL_Color) voidptr
//////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////
// MIX
//////////////////////////////////////////////////////////
fn C.Mix_Init(flags int) int
fn C.Mix_OpenAudio(frequency int, format u16, channels int, chunksize int) int
fn C.Mix_LoadMUS(file byteptr) voidptr
fn C.Mix_LoadWAV(file byteptr) voidptr
fn C.Mix_PlayMusic(music voidptr, loops int) int
fn C.Mix_VolumeMusic(volume int) int
fn C.Mix_FreeMusic(music voidptr)
fn C.Mix_CloseAudio()
fn C.Mix_FreeChunk(chunk voidptr)
fn C.Mix_PauseMusic()
fn C.Mix_ResumeMusic()
fn C.Mix_PlayChannel(channel int, chunk voidptr, loops int) int

//////////////////////////////////////////////////////////
// GL
//////////////////////////////////////////////////////////
fn C.SDL_GL_SetAttribute(attr int, value int) int
fn C.SDL_GL_CreateContext(window voidptr) voidptr
fn C.SDL_GL_MakeCurrent(window voidptr, context voidptr) int
fn C.SDL_GL_SetSwapInterval(interval int) int
fn C.SDL_GL_SwapWindow(window voidptr)
fn C.SDL_GL_DeleteContext(context voidptr)

pub fn create_texture_from_surface(renderer voidptr, surface &Surface) voidptr {
	return C.SDL_CreateTextureFromSurface(renderer, voidptr(surface))
}

pub fn create_window_and_renderer(width int, height int, window_flags u32, window voidptr, renderer voidptr) int {
	return C.SDL_CreateWindowAndRenderer(width, height, window_flags, window, renderer)
}

pub fn joystick_name_for_index(device_index int) byteptr {
	return byteptr(C.SDL_JoystickNameForIndex(device_index))
}

pub fn fill_rect(screen &Surface, rect &Rect, _col &Color) {
	col := C.SDL_MapRGB(screen.format, _col.r, _col.g, _col.b)
	_screen := voidptr(screen)
	_rect := voidptr(rect)
	C.SDL_FillRect(_screen, _rect, col)
}

pub fn create_rgb_surface(flags u32, width int, height int, depth int, rmask u32, gmask u32, bmask u32, amask u32) &Surface {
	res := C.SDL_CreateRGBSurface(flags, width, height, depth, rmask, gmask, bmask, amask)
	return res
}

pub fn render_copy(renderer voidptr, texture voidptr, srcrect &Rect, dstrect &Rect) int {
	_srcrect := voidptr(srcrect)
	_dstrect := voidptr(dstrect)
	return C.SDL_RenderCopy(renderer, texture, _srcrect, _dstrect)
}

pub fn poll_event(event &Event) int {
	return C.SDL_PollEvent(voidptr(event))
}

pub fn destroy_texture(text voidptr) {
        C.SDL_DestroyTexture(text)
}

pub fn free_surface(surf &Surface) {
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
