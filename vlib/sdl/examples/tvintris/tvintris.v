// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// SDL2 port+wrapper, Twintris-like dual-game logic,
// and more, by Nicolas Sauzede 2019.

module main

import filepath
import rand
import time
import os
import math
import sdl
import sdl.image as img
import sdl.mixer as mix
import sdl.ttf as ttf
[inline] fn sdl_fill_rect(s &SDL_Surface,r &SDL_Rect,c &SDL_Color){sdl.fill_rect(s,r,c)}

const (
	Title = 'tVintris'
	BASE = filepath.dir( os.realpath( os.executable() ) )
	FontName = BASE + '/fonts/RobotoMono-Regular.ttf'
	MusicName = BASE + '/sounds/TwintrisThosenine.mod'
	SndBlockName = BASE + '/sounds/block.wav'
	SndLineName = BASE + '/sounds/single.wav'
	SndDoubleName = BASE + '/sounds/triple.wav'
	VLogo = BASE + '/images/v-logo_30_30.png'
	BlockSize = 20 // pixels
	FieldHeight = 20 // # of blocks
	FieldWidth = 10
	TetroSize = 4
	WinWidth = BlockSize * FieldWidth * 3
	WinHeight = BlockSize * FieldHeight
	TimerPeriod = 250 // ms
	TextSize = 16
	AudioBufSize = 1024

	P2FIRE = C.SDLK_l
	P2UP = C.SDLK_UP
	P2DOWN = C.SDLK_DOWN
	P2LEFT = C.SDLK_LEFT
	P2RIGHT = C.SDLK_RIGHT

	P1FIRE = C.SDLK_s
	P1UP = C.SDLK_w
	P1DOWN = C.SDLK_x
	P1LEFT = C.SDLK_a
	P1RIGHT = C.SDLK_d

	NJOYMAX = 2
	// joystick name => enter your own device name
	JOYP1NAME = 'Generic X-Box pad'
	// following are joystick button number
	JBP1FIRE = 1
	// following are joystick hat value
	JHP1UP = 1
	JHP1DOWN = 4
	JHP1LEFT = 8
	JHP1RIGHT = 3

	// joystick name => enter your own device name
	JOYP2NAME = 'RedOctane Guitar Hero X-plorer'
	// following are joystick button number
	JBP2FIRE = 0
	// following are joystick hat value
	JHP2UP = 4
	JHP2DOWN = 1
	JHP2LEFT = 8
	JHP2RIGHT = 2
)

const (
  mix_version = mix.version
  ttf_version = ttf.version
)

const (
	// Tetros' 4 possible states are encoded in binaries
	BTetros = [
		// 0000 0
		// 0000 0
		// 0110 6
		// 0110 6
		[66, 66, 66, 66],
		// 0000 0
		// 0000 0
		// 0010 2
		// 0111 7
		[27, 131, 72, 232],
		// 0000 0
		// 0000 0
		// 0011 3
		// 0110 6
		[36, 231, 36, 231],
		// 0000 0
		// 0000 0
		// 0110 6
		// 0011 3
		[63, 132, 63, 132],
		// 0000 0
		// 0011 3
		// 0001 1
		// 0001 1
		[311, 17, 223, 74],
		// 0000 0
		// 0011 3
		// 0010 2
		// 0010 2
		[322, 71, 113, 47],
		// Special case since 15 can't be used
		// 1111
		[1111, 9, 1111, 9],
	]
	// Each tetro has its unique color
	Colors = [
		SDL_Color{byte(0), byte(0), byte(0), byte(0)},		// unused ?
		SDL_Color{byte(0), byte(0x62), byte(0xc0), byte(0)},	// quad : darkblue 0062c0
		SDL_Color{byte(0xca), byte(0x7d), byte(0x5f), byte(0)},	// tricorn : lightbrown ca7d5f
		SDL_Color{byte(0), byte(0xc1), byte(0xbf), byte(0)},	// short topright : lightblue 00c1bf
		SDL_Color{byte(0), byte(0xc1), byte(0), byte(0)},	// short topleft : lightgreen 00c100
		SDL_Color{byte(0xbf), byte(0xbe), byte(0), byte(0)},	// long topleft : yellowish bfbe00
		SDL_Color{byte(0xd1), byte(0), byte(0xbf), byte(0)},	// long topright : pink d100bf
		SDL_Color{byte(0xd1), byte(0), byte(0), byte(0)},	// longest : lightred d10000
		SDL_Color{byte(0), byte(170), byte(170), byte(0)},	// unused ?
	]
	// Background color
	BackgroundColor = SDL_Color{byte(0), byte(0), byte(0), byte(0)}
	// Foreground color
	ForegroundColor = SDL_Color{byte(0), byte(170), byte(170), byte(0)}
	// Text color
	TextColor = SDL_Color{byte(0xca), byte(0x7d), byte(0x5f), byte(0)}
)

// TODO: type Tetro [TetroSize]struct{ x, y int }
struct Block {
	mut:
	x int
	y int
}

enum GameState {
        paused running gameover
}

struct AudioContext {
mut:
	music voidptr
	volume int
        waves [3]voidptr
}

struct SdlContext {
pub mut:
//	VIDEO
	w		int
	h		int
	window          voidptr
	renderer        voidptr
	screen          &SDL_Surface
	texture         voidptr
//	AUDIO
	actx		AudioContext
//	JOYSTICKS
	jnames		[2]string
	jids		[2]int
//	V logo
	v_logo		&SDL_Surface
	tv_logo		voidptr
}

struct Game {
mut:
	// Score of the current game
	score        int
	// Count consecutive lines for scoring
	lines        int
	// State of the current game
	state    GameState
	// X offset of the game display
	ofs_x           int
	// keys
	k_fire          int
	k_up            int
	k_down          int
	k_left          int
	k_right         int
	// joystick ID
	joy_id           int
	// joystick buttons
	jb_fire          int
	// joystick hat values
	jh_up            int
	jh_down          int
	jh_left          int
	jh_right         int
	// game rand seed
	seed            int
	seed_ini            int
	// Position of the current tetro
	pos_x        int
	pos_y        int
	// field[y][x] contains the color of the block with (x,y) coordinates
	// "-1" border is to avoid bounds checking.
	// -1 -1 -1 -1
	// -1  0  0 -1
	// -1  0  0 -1
	// -1 -1 -1 -1
	field       [][]int
	// TODO: tetro Tetro
	tetro       []Block
	// TODO: tetros_cache []Tetro
	tetros_cache []Block
	// Index of the current tetro. Refers to its color.
	tetro_idx    int
	// Index of the next tetro. Refers to its color.
	tetro_next    int
	// tetro stats : buckets of drawn tetros
	tetro_stats []int
	// total number of drawn tetros
	tetro_total int
	// Index of the rotation (0-3)
	rotation_idx int
	// SDL2 context for drawing
	sdl             SdlContext
	// TTF context for font drawing
	font            voidptr
}

fn (sdlc mut SdlContext) set_sdl_context(w int, h int, title string) {
	C.SDL_Init(C.SDL_INIT_VIDEO | C.SDL_INIT_AUDIO | C.SDL_INIT_JOYSTICK)
	C.atexit(C.SDL_Quit)
	C.TTF_Init()
	C.atexit(C.TTF_Quit)
	bpp := 32
	sdl.create_window_and_renderer(w, h, 0, &sdlc.window, &sdlc.renderer)
//	C.SDL_CreateWindowAndRenderer(w, h, 0, voidptr(&sdlc.window), voidptr(&sdlc.renderer))
	C.SDL_SetWindowTitle(sdlc.window, title.str)
	sdlc.w = w
	sdlc.h = h
	sdlc.screen = sdl.create_rgb_surface(0, w, h, bpp, 0x00FF0000, 0x0000FF00, 0x000000FF, 0xFF000000)
	sdlc.texture = C.SDL_CreateTexture(sdlc.renderer, C.SDL_PIXELFORMAT_ARGB8888, C.SDL_TEXTUREACCESS_STREAMING, w, h)

	C.Mix_Init(C.MIX_INIT_MOD)
	C.atexit(C.Mix_Quit)
	if C.Mix_OpenAudio(48000,C.MIX_DEFAULT_FORMAT,2,AudioBufSize) < 0 {
		println('couldn\'t open audio')
	}
	println('opening music $MusicName')
	sdlc.actx.music = C.Mix_LoadMUS(MusicName.str)
	sdlc.actx.waves[0] = C.Mix_LoadWAV(SndBlockName.str)
	sdlc.actx.waves[1] = C.Mix_LoadWAV(SndLineName.str)
	sdlc.actx.waves[2] = C.Mix_LoadWAV(SndDoubleName.str)
	sdlc.actx.volume = C.SDL_MIX_MAXVOLUME
	if C.Mix_PlayMusic(sdlc.actx.music, 1) != -1 {
		C.Mix_VolumeMusic(sdlc.actx.volume)
	}
	njoy := C.SDL_NumJoysticks()
	for i := 0; i < njoy; i++ {
		C.SDL_JoystickOpen(i)
		jn := tos_clone(sdl.joystick_name_for_index(i))
		println('JOY NAME $jn')
		for j := 0; j < NJOYMAX; j++ {
			if sdlc.jnames[j] == jn {
				println('FOUND JOYSTICK $j $jn ID=$i')
				sdlc.jids[j] = i
			}
		}
	}
	flags := C.IMG_INIT_PNG
	imgres := img.img_init(flags)
	if ((imgres & flags) != flags) {
		println('error initializing image library.')
	}
	println('opening logo $VLogo')
	sdlc.v_logo = img.load(VLogo)
	if !isnil(sdlc.v_logo) {
//		println('got v_logo=$sdlc.v_logo')
		sdlc.tv_logo = sdl.create_texture_from_surface(sdlc.renderer, sdlc.v_logo)
//		println('got tv_logo=$sdlc.tv_logo')
	}
	C.SDL_JoystickEventState(C.SDL_ENABLE)
}

fn main() {
	println('tVintris -- tribute to venerable Twintris')
	mut game := &Game{ font: 0 }
	game.sdl.jnames[0] = JOYP1NAME
	game.sdl.jnames[1] = JOYP2NAME
	game.sdl.jids[0] = -1
	game.sdl.jids[1] = -1
	game.sdl.set_sdl_context(WinWidth, WinHeight, Title)
	game.font = C.TTF_OpenFont(FontName.str, TextSize)
	seed := time.now().uni
	mut game2 := &Game{ font: 0 }
	game2.sdl = game.sdl
	game2.font = game.font

	game.joy_id = game.sdl.jids[0]
//	println('JOY1 id=${game.joy_id}')
	game2.joy_id = game.sdl.jids[1]
//	println('JOY2 id=${game2.joy_id}')

	// delay uses milliseconds so 1000 ms / 30 frames (30fps) roughly = 33.3333 ms/frame
	time_per_frame := 1000.0 / 30.0

	game.k_fire = P1FIRE
	game.k_up = P1UP
	game.k_down = P1DOWN
	game.k_left = P1LEFT
	game.k_right = P1RIGHT
	game.jb_fire = JBP1FIRE
	game.jh_up = JHP1UP
	game.jh_down = JHP1DOWN
	game.jh_left = JHP1LEFT
	game.jh_right = JHP1RIGHT
	game.ofs_x = 0
	game.seed_ini = seed
	game.init_game()
	game.state = .running
	go game.run() // Run the game loop in a new thread

	game2.k_fire = P2FIRE
	game2.k_up = P2UP
	game2.k_down = P2DOWN
	game2.k_left = P2LEFT
	game2.k_right = P2RIGHT
	game2.jb_fire = JBP2FIRE
	game2.jh_up = JHP2UP
	game2.jh_down = JHP2DOWN
	game2.jh_left = JHP2LEFT
	game2.jh_right = JHP2RIGHT
	game2.ofs_x = WinWidth * 2 / 3
	game2.seed_ini = seed
	game2.init_game()
	game2.state = .running
	go game2.run() // Run the game loop in a new thread

	mut g := Game{ font: 0 }
	mut should_close := false
	mut total_frame_ticks := u64(0)
	mut total_frames := u32(0)

	for {
		total_frames++
		start_ticks := sdl.get_perf_counter()

		g1 := game
		g2 := game2
		// here we determine which game contains most recent state
		if g1.tetro_total > g.tetro_total {
			g = *g1
		}
		if g2.tetro_total > g.tetro_total {
			g = *g2
		}
		g.draw_begin()

		g1.draw_tetro()
		g1.draw_field()

		g2.draw_tetro()
		g2.draw_field()

		g.draw_middle()

		g1.draw_score()
		g2.draw_score()

		g.draw_stats()

		g.draw_v_logo()
		g.draw_end()

//		game.handle_events()            // CRASHES if done in function ???
		evt := SDL_Event{}
		for 0 < sdl.poll_event(&evt) {
			match int(evt.@type) {
				C.SDL_QUIT { should_close = true }
				C.SDL_KEYDOWN {
					key := evt.key.keysym.sym
					if key == C.SDLK_ESCAPE {
					        should_close = true
					        break
					}
					game.handle_key(key)
					game2.handle_key(key)
				}
				C.SDL_JOYBUTTONDOWN {
					jb := int(evt.jbutton.button)
					joyid := evt.jbutton.which
//					println('JOY BUTTON $jb $joyid')
					game.handle_jbutton(jb, joyid)
					game2.handle_jbutton(jb, joyid)
				}
				C.SDL_JOYHATMOTION {
					jh := int(evt.jhat.hat)
					jv := int(evt.jhat.value)
					joyid := evt.jhat.which
//					println('JOY HAT $jh $jv $joyid')
					game.handle_jhat(jh, jv, joyid)
					game2.handle_jhat(jh, jv, joyid)
				}
				else {}
			}
		}
		if should_close {
			break
		}
		end_ticks := sdl.get_perf_counter()

		total_frame_ticks += end_ticks-start_ticks
		elapsed_time := f64(end_ticks - start_ticks) / f64(sdl.get_perf_frequency())
		// current_fps := 1.0 / elapsed_time

		// should limit system to (1 / time_per_frame) fps
		sdl.delay(u32(math.floor(time_per_frame - elapsed_time)))
	}
	if game.font != voidptr(0) {
		C.TTF_CloseFont(game.font)
	}
	if game.sdl.actx.music != voidptr(0) {
		C.Mix_FreeMusic(game.sdl.actx.music)
	}
	C.Mix_CloseAudio()
	if game.sdl.actx.waves[0] != voidptr(0) {
		C.Mix_FreeChunk(game.sdl.actx.waves[0])
	}
	if game.sdl.actx.waves[1] != voidptr(0) {
		C.Mix_FreeChunk(game.sdl.actx.waves[1])
	}
	if game.sdl.actx.waves[2] != voidptr(0) {
		C.Mix_FreeChunk(game.sdl.actx.waves[2])
	}
	if !isnil(game.sdl.tv_logo) {
		sdl.destroy_texture(game.sdl.tv_logo)
	}
	if !isnil(game.sdl.v_logo) {
		sdl.free_surface(game.sdl.v_logo)
	}
}

enum Action {
        idle space fire
}
fn (game mut Game) handle_key(key int) {
	// global keys
	mut action := Action.idle
	match key {
		C.SDLK_SPACE { action = .space }
		game.k_fire { action = .fire }
		else {}
	}

	if action == .space {
			match game.state {
				.running {
					C.Mix_PauseMusic()
					game.state = .paused
				}
				.paused {
					C.Mix_ResumeMusic()
					game.state = .running
				}
				else {}
			}
	}

	if action == .fire {
			match game.state {
				.gameover {
					game.init_game()
					game.state = .running
				}
				else {}
			}
	}
	if game.state != .running { return }
	// keys while game is running
	match key {
		game.k_up { game.rotate_tetro() }
		game.k_left { game.move_right(-1) }
		game.k_right { game.move_right(1) }
		game.k_down { game.move_tetro() } // drop faster when the player presses <down>
		else {}
	}
}

fn (game mut Game) handle_jbutton(jb int, joyid int) {
	if joyid != game.joy_id {
		return
	}
	// global buttons
	mut action := Action.idle
	match jb {
		game.jb_fire { action = .fire }
		else {}
	}

	if action == .fire {
			match game.state {
				.gameover {
					game.init_game()
					game.state = .running
				}
				else {}
			}
	}
}

fn (game mut Game) handle_jhat(jh int, jv int, joyid int) {
	if joyid != game.joy_id {
		return
	}
	if game.state != .running { return }
//	println('testing hat values.. joyid=$joyid jh=$jh jv=$jv')
	// hat values while game is running
	match jv {
		game.jh_up { game.rotate_tetro() }
		game.jh_left { game.move_right(-1) }
		game.jh_right { game.move_right(1) }
		game.jh_down { game.move_tetro() } // drop faster when the player presses <down>
		else {}
	}
}

fn (g mut Game) init_game() {
	g.score = 0
	g.tetro_total = 0
	g.tetro_stats = [0, 0, 0, 0, 0, 0, 0]
	g.parse_tetros()
	g.seed = g.seed_ini
	g.generate_tetro()
	g.field = []
	// Generate the field, fill it with 0's, add -1's on each edge
	for i := 0; i < FieldHeight + 2; i++ {
		mut row := [0].repeat(FieldWidth + 2)
		row[0] = - 1
		row[FieldWidth + 1] = - 1
		g.field << row
	}
	mut first_row := g.field[0]
	mut last_row := g.field[FieldHeight + 1]
	for j := 0; j < FieldWidth + 2; j++ {
		first_row[j] = - 1
		last_row[j] = - 1
	}
}

fn (g mut Game) parse_tetros() {
	for b_tetros in BTetros {
		for b_tetro in b_tetros {
			for t in parse_binary_tetro(b_tetro) {
				g.tetros_cache << t
			}
		}
	}
}

fn (g mut Game) run() {
	for {
		if g.state == .running {
			g.move_tetro()
			n := g.delete_completed_lines()
			if n > 0 {
				g.lines += n
			} else {
				if g.lines > 0 {
					if g.lines > 1 {
						C.Mix_PlayChannel(0, g.sdl.actx.waves[2], 0)
					} else if g.lines == 1 {
						C.Mix_PlayChannel(0, g.sdl.actx.waves[1], 0)
					}
					g.score += 10 * g.lines * g.lines
					g.lines = 0
				}
			}
		}
		time.sleep_ms(TimerPeriod)      // medium delay between game step
	}
}

fn (game mut Game) rotate_tetro() {
	// Rotate the tetro
	old_rotation_idx := game.rotation_idx
	game.rotation_idx++
	if game.rotation_idx == TetroSize {
		game.rotation_idx = 0
	}
	game.get_tetro()
	if !game.move_right(0) {
		game.rotation_idx = old_rotation_idx
		game.get_tetro()
	}
	if game.pos_x < 0 {
		game.pos_x = 1
	}
}

fn (g mut Game) move_tetro() {
	// Check each block in current tetro
	for block in g.tetro {
		y := block.y + g.pos_y + 1
		x := block.x + g.pos_x
		// Reached the bottom of the screen or another block?
		// TODO: if g.field[y][x] != 0
		//if g.field[y][x] != 0 {
		row := g.field[y]
		if row[x] != 0 {
			// The new tetro has no space to drop => end of the game
			if g.pos_y < 2 {
				g.state = .gameover
				g.tetro_total = 0
				return
			}
			// Drop it and generate a new one
			g.drop_tetro()
			g.generate_tetro()
			C.Mix_PlayChannel(0, g.sdl.actx.waves[0], 0)
			return
		}
	}
	g.pos_y++
}

fn (g mut Game) move_right(dx int) bool {
	// Reached left/right edge or another tetro?
	for i := 0; i < TetroSize; i++ {
		tetro := g.tetro[i]
		y := tetro.y + g.pos_y
		x := tetro.x + g.pos_x + dx
		row := g.field[y]
		if row[x] != 0 {
			// Do not move
			return false
		}
	}
	g.pos_x += dx
	return true
}

fn (g &Game) delete_completed_lines() int {
	mut n := 0
	for y := FieldHeight; y >= 1; y-- {
		n += g.delete_completed_line(y)
	}
	return n
}

fn (g &Game) delete_completed_line(y int) int {
	for x := 1; x <= FieldWidth; x++ {
		f := g.field[y]
		if f[x] == 0 {
			return 0
		}
	}
	// Move everything down by 1 position
	for yy := y - 1; yy >= 1; yy-- {
		for x := 1; x <= FieldWidth; x++ {
			mut a := g.field[yy + 1]
			b := g.field[yy]
			a[x] = b[x]
		}
	}
	return 1
}

// Draw a rand tetro index
fn (g mut Game) rand_tetro() int {
	cur := g.tetro_next
	g.tetro_next = rand.rand_r(&g.seed)
	g.tetro_next = g.tetro_next % BTetros.len
	return cur
}

// Place a new tetro on top
fn (g mut Game) generate_tetro() {
	g.pos_y = 0
	g.pos_x = FieldWidth / 2 - TetroSize / 2
	g.tetro_idx = g.rand_tetro()
//	println('idx=${g.tetro_idx}')
	g.tetro_stats[g.tetro_idx]+= 2 -1
	g.tetro_total++
	g.rotation_idx = 0
	g.get_tetro()
}

// Get the right tetro from cache
fn (g mut Game) get_tetro() {
	idx := g.tetro_idx * TetroSize * TetroSize + g.rotation_idx * TetroSize
	g.tetro = g.tetros_cache[idx .. idx + TetroSize]
}

fn (g &Game) drop_tetro() {
	for i := 0; i < TetroSize; i++ {
		tetro := g.tetro[i]
		x := tetro.x + g.pos_x
		y := tetro.y + g.pos_y
		// Remember the color of each block
		// TODO: g.field[y][x] = g.tetro_idx + 1
		mut row := g.field[y]
		row[x] = g.tetro_idx + 1
	}
}

fn (g &Game) draw_tetro() {
	for i := 0; i < TetroSize; i++ {
		tetro := g.tetro[i]
		g.draw_block(g.pos_y + tetro.y, g.pos_x + tetro.x, g.tetro_idx + 1)
	}
}

fn (g &Game) draw_block(i, j, color_idx int) {
	rect := SDL_Rect {g.ofs_x + (j - 1) * BlockSize, (i - 1) * BlockSize,
		BlockSize - 1, BlockSize - 1}
	col := Colors[color_idx]
	sdl_fill_rect(g.sdl.screen, &rect, &col)
}

fn (g &Game) draw_field() {
	for i := 1; i < FieldHeight + 1; i++ {
		for j := 1; j < FieldWidth + 1; j++ {
			f := g.field[i]
			if f[j] > 0 {
				g.draw_block(i, j, f[j])
			}
		}
	}
}

fn (g &Game) draw_v_logo() {
	if isnil(g.sdl.tv_logo) {
		return
	}
	texw := 0
	texh := 0
	C.SDL_QueryTexture(g.sdl.tv_logo, 0, 0, &texw, &texh)
	dstrect := SDL_Rect { (WinWidth / 2) - (texw / 2), 20, texw, texh }
	// Currently we can't seem to use sdl.render_copy when we need to pass a nil pointer (eg: srcrect to be NULL)
//	sdl.render_copy(g.sdl.renderer, tv_logo, 0, &dstrect)
	C.SDL_RenderCopy(g.sdl.renderer, g.sdl.tv_logo, voidptr(0), voidptr(&dstrect))
}

fn (g &Game) draw_text(x int, y int, text string, tcol SDL_Color) {
	_tcol := C.SDL_Color{tcol.r, tcol.g, tcol.b, tcol.a}
	tsurf := C.TTF_RenderText_Solid(g.font, text.str, _tcol)
	ttext := C.SDL_CreateTextureFromSurface(g.sdl.renderer, tsurf)
	texw := 0
	texh := 0
	C.SDL_QueryTexture(ttext, 0, 0, &texw, &texh)
	dstrect := SDL_Rect { x, y, texw, texh }
//	sdl.render_copy(g.sdl.renderer, ttext, 0, &dstrect)
	C.SDL_RenderCopy(g.sdl.renderer, ttext, voidptr(0), voidptr(&dstrect))
	C.SDL_DestroyTexture(ttext)
	sdl.free_surface(tsurf)
}

[inline] fn (g &Game) draw_ptext(x int, y int, text string, tcol SDL_Color) {
	g.draw_text(g.ofs_x + x, y, text, tcol)
}

[live]
fn (g &Game) draw_begin() {
//	println('about to clear')
	C.SDL_RenderClear(g.sdl.renderer)
	mut rect := SDL_Rect {0,0,g.sdl.w,g.sdl.h}
	col := SDL_Color{byte(00), byte(00), byte(0), byte(0)}
//	sdl_fill_rect(g.sdl.screen, &rect, BackgroundColor)
	sdl_fill_rect(g.sdl.screen, &rect, col)

	rect = SDL_Rect {BlockSize * FieldWidth + 2,0,2,g.sdl.h}
	sdl_fill_rect(g.sdl.screen, &rect, ForegroundColor)
	rect = SDL_Rect {WinWidth - BlockSize * FieldWidth - 4,0,2,g.sdl.h}
	sdl_fill_rect(g.sdl.screen, &rect, ForegroundColor)

	mut idx := 0
	for st in g.tetro_stats {
		mut s := 10
		if g.tetro_total > 0 {
			s += 90 * st / g.tetro_total
		}
		w := BlockSize
		h := s * 4 * w / 100
		rect = SDL_Rect {(WinWidth - 7 * (w + 1)) / 2 + idx * (w + 1), WinHeight * 3 / 4 - h, w, h}
		sdl_fill_rect(g.sdl.screen, &rect, Colors[idx + 1])
		idx++
	}
}

fn (g &Game) draw_middle() {
	C.SDL_UpdateTexture(g.sdl.texture, 0, g.sdl.screen.pixels, g.sdl.screen.pitch)
//	sdl.render_copy(g.sdl.renderer, g.sdl.texture, voidptr(0), voidptr(0))
	C.SDL_RenderCopy(g.sdl.renderer, g.sdl.texture, voidptr(0), voidptr(0))
}

fn (g &Game) draw_score() {
	if g.font != voidptr(0) {
		g.draw_ptext(1, 2, 'score: ' + g.score.str() + ' nxt=' + g.tetro_next.str(), TextColor)
		if g.state == .gameover {
			g.draw_ptext(1, WinHeight / 2 + 0 * TextSize, 'Game Over', TextColor)
			g.draw_ptext(1, WinHeight / 2 + 2 * TextSize, 'FIRE to restart', TextColor)
		} else if g.state == .paused {
			g.draw_ptext(1, WinHeight / 2 + 0 * TextSize, 'Game Paused', TextColor)
			g.draw_ptext(1, WinHeight / 2 + 2 * TextSize, 'SPACE to resume', TextColor)
		}
	}
}

fn (g &Game) draw_stats() {
	if g.font != voidptr(0) {
		g.draw_text(WinWidth / 3 + 10, WinHeight * 3 / 4 + 0 * TextSize, 'stats: ' + g.tetro_total.str() + ' tetros', TextColor)
		mut stats := ''
		for st in g.tetro_stats {
			mut s := 0
			if g.tetro_total > 0 {
				s = 100 * st / g.tetro_total
			}
			stats += ' '
			stats += s.str()
		}
		g.draw_text(WinWidth / 3 - 8, WinHeight * 3 / 4 + 2 * TextSize, stats, TextColor)
	}
}

fn (g &Game) draw_end() {
	C.SDL_RenderPresent(g.sdl.renderer)
}

fn parse_binary_tetro(t_ int) []Block {
	mut t := t_
	res := [Block{}].repeat(4)
	mut cnt := 0
	horizontal := t == 9// special case for the horizontal line
	for i := 0; i <= 3; i++ {
		// Get ith digit of t
		p := int(math.pow(10, 3 - i))
		mut digit := t / p
		t %= p
		// Convert the digit to binary
		for j := 3; j >= 0; j-- {
			bin := digit % 2
			digit /= 2
			if bin == 1 || (horizontal && i == TetroSize - 1) {
				// TODO: res[cnt].x = j
				// res[cnt].y = i
				mut point := &res[cnt]
				point.x = j
				point.y = i
				cnt++
			}
		}
	}
	return res
}
