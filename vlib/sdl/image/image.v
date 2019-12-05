module image

#flag linux -lSDL2_image
#include <SDL_image.h>

// following kludge until `sdl2-config ...` is supported also on windows
#flag windows -I/msys64/mingw64/include/SDL2
#flag windows -L/mingw64/lib -lSDL2_image

//////////////////////////////////////////////////////////
// SDL_Image.h
//////////////////////////////////////////////////////////
//fn C.IMG_Load_RW(logo &sdl.RwOps, free_src int) &sdl.Surface
fn C.IMG_Init(flags int) int
fn C.IMG_Quit()
fn C.IMG_Load(file byteptr) voidptr

pub fn img_init(flags int) int {
	return C.IMG_Init(flags)
}

pub fn quit() {
	C.IMG_Quit()
}

pub fn load(file string) &sdl.Surface {
	res := C.IMG_Load(file.str)
	return res
}
