module main

import sdl

fn main() {
	C.SDL_Init(C.SDL_INIT_VIDEO)
	window := C.SDL_CreateWindow('Hello SDL2', 300, 300, 500, 300, 0)
	renderer := C.SDL_CreateRenderer(window, -1, C.SDL_RENDERER_ACCELERATED | C.SDL_RENDERER_PRESENTVSYNC)

	mut should_close := false
	for {
		evt := SDL_Event{}
		for 0 < sdl.poll_event(&evt) {
			match int(evt.@type) {
				C.SDL_QUIT { should_close = true }
				else {}
			}
		}
		if should_close {
			break
		}

		C.SDL_SetRenderDrawColor(renderer, 255, 55, 55, 255)
		C.SDL_RenderClear(renderer)
		C.SDL_RenderPresent(renderer)
	}
	
	C.SDL_DestroyRenderer(renderer)
	C.SDL_DestroyWindow(window)
	C.SDL_Quit()
}
