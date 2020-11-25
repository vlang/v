// import modules for use in app
import term.ui as termui
import rand

// define some global constants
const (
	block_size = 1
	green      = termui.Color{0, 255, 0}
	grey       = termui.Color{150, 150, 150}
	white      = termui.Color{255, 255, 255}
	blue       = termui.Color{0, 0, 255}
	red        = termui.Color{255, 0, 0}
	black      = termui.Color{0, 0, 0}
)

// what edge of the screen are you facing
enum Orientation {
	top
	right
	bottom
	left
}

// what's the current state of the game
enum GameState {
	pause
	gameover
	game
}

// simple 2d vector representation
struct Vec {
mut:
	x int
	y int
}

// determine orientation from vector (hacky way to set facing from velocity)
fn (v Vec) facing() Orientation {
	result := if v.x >= 0 {
		Orientation.right
	} else if v.x < 0 {
		Orientation.left
	} else if v.y >= 0 {
		Orientation.bottom
	} else {
		Orientation.top
	}
	return result
}

// generate a random vector with x in [min_x, max_x] and y in [min_y, max_y]
fn (mut v Vec) randomize(min_x int, min_y int, max_x int, max_y int) {
	v.x = rand.int_in_range(min_x, max_x)
	v.y = rand.int_in_range(min_y, max_y)
}

// part of snake's body representation
struct BodyPart {
mut:
	pos    Vec = {
	x: block_size
	y: block_size
}
	color  termui.Color = green
	facing Orientation = .top
}

// snake representation
struct Snake {
mut:
	app       &App
	direction Orientation
	body      []BodyPart
	velocity  Vec = Vec{
	x: 0
	y: 0
}
}

// length returns the snake's current length
fn (s Snake) length() int {
	return s.body.len
}

// impulse provides a impulse to change the snake's direction
fn (mut s Snake) impulse(direction Orientation) {
	mut vec := Vec{}
	match direction {
		.top {
			vec.x = 0
			vec.y = -1 * block_size
		}
		.right {
			vec.x = block_size
			vec.y = 0
		}
		.bottom {
			vec.x = 0
			vec.y = block_size
		}
		.left {
			vec.x = -1 * block_size
			vec.y = 0
		}
	}
	s.direction = direction
	s.velocity = vec
}

// move performs the calculations for the snake's movements
fn (mut s Snake) move() {
	mut i := s.body.len - 1
	width := s.app.width
	height := s.app.height
	// move the parts of the snake as appropriate
	for i = s.body.len - 1; i >= 0; i-- {
		mut piece := s.body[i]
		if i > 0 { // just move the body of the snake up one position
			piece.pos = s.body[i - 1].pos
			piece.facing = s.body[i - 1].facing
		} else { // verify that the move is valid and move the head if so
			piece.facing = s.direction
			new_x := piece.pos.x + s.velocity.x
			new_y := piece.pos.y + s.velocity.y
			piece.pos.x += if new_x > block_size && new_x < width - block_size { s.velocity.x } else { 0 }
			piece.pos.y += if new_y > block_size && new_y < height - block_size { s.velocity.y } else { 0 }
		}
		s.body[i] = piece
	}
}

// grow add another part to the snake when it catches the rat
fn (mut s Snake) grow() {
	head := s.get_tail()
	mut pos := Vec{}
	// add the segment on the opposite side of the previous tail
	match head.facing {
		.bottom {
			pos.x = head.pos.x
			pos.y = head.pos.y - block_size
		}
		.left {
			pos.x = head.pos.x + block_size
			pos.y = head.pos.y
		}
		.top {
			pos.x = head.pos.x
			pos.y = head.pos.y + block_size
		}
		.right {
			pos.x = head.pos.x - block_size
			pos.y = head.pos.y
		}
	}
	s.body << BodyPart{
		pos: pos
		facing: head.facing
	}
}

// get_body gets the parts of the snakes body
fn (s Snake) get_body() []BodyPart {
	return s.body
}

// get_head get snake's head
fn (s Snake) get_head() BodyPart {
	return s.body[0]
}

// get_tail get snake's tail
fn (s Snake) get_tail() BodyPart {
	return s.body[s.body.len - 1]
}

// randomize randomizes position and velocity of snake
fn (mut s Snake) randomize() {
	mut pos := s.get_head().pos
	pos.randomize(block_size, block_size, s.app.width, s.app.height)
	s.velocity.randomize(-1 * block_size, -1 * block_size, block_size, block_size)
	s.direction = s.velocity.facing()
	s.body[0].pos = pos
}

// check_overlap determine if the snake's looped onto itself
fn (s Snake) check_overlap() bool {
	h := s.get_head()
	head_pos := h.pos
	for i in 2 .. s.length() {
		piece_pos := s.body[i].pos
		if head_pos.x == piece_pos.x && head_pos.y == piece_pos.y {
			return true
		}
	}
	return false
}

// draw draws the parts of the snake
fn (s Snake) draw() {
	mut a := s.app
	for part in s.get_body() {
		a.termui.set_bg_color(part.color)
		a.termui.draw_rect(part.pos.x, part.pos.y, part.pos.x + block_size, part.pos.y + block_size)
		$if verbose ? {
			text := match part.facing {
				.top { '^' }
				.bottom { 'v' }
				.right { '>' }
				.left { '<' }
			}
			a.termui.set_color(white)
			a.termui.draw_text(part.pos.x, part.pos.y, text)
		}
	}
}

// rat representation
struct Rat {
mut:
	pos      Vec = {
	x: block_size
	y: block_size
}
	captured bool
	color    termui.Color = grey
	app      &App
}

// randomize spawn the rat in a new spot within the playable field
fn (mut r Rat) randomize() {
	r.pos.randomize(2 * block_size, 2 * block_size, r.app.width - block_size, r.app.height - block_size)
}

struct App {
mut:
	termui &termui.Context = 0
	snake  Snake
	rat    Rat
	width  int
	height int
	redraw bool = true
	state  GameState = .game
}

// new_game setups the rat and snake for play
fn (mut a App) new_game() {
	mut snake := Snake{
		body: []BodyPart{len: 1, init: BodyPart{}}
		app: a
	}
	snake.randomize()
	mut rat := Rat{
		app: a
	}
	rat.randomize()
	a.snake = snake
	a.rat = rat
	a.state = .game
	a.redraw = true
}

// initialize the app and record the width and height of the window
fn init(x voidptr) {
	mut app := &App(x)
	w, h := app.termui.window_width, app.termui.window_height
	app.width = w
	app.height = h
	app.new_game()
}

// event handles different events for the app as they occur
fn event(e &termui.Event, x voidptr) {
	mut app := &App(x)
	match e.typ {
		.mouse_down {}
		.mouse_drag {}
		.mouse_up {}
		.key_down {
			match e.code {
				.up, .w { app.move_snake(.top) }
				.down, .s { app.move_snake(.bottom) }
				.left, .a { app.move_snake(.left) }
				.right, .d { app.move_snake(.right) }
				.r { app.new_game() }
				.c {}
				.p { app.state = if app.state == .game { GameState.pause } else { GameState.game } }
				.escape, .q { exit(0) }
				else { exit(0) }
			}
			if e.code == .c {
			} else if e.code == .escape {
				exit(0)
			}
		}
		else {}
	}
	app.redraw = true
}

// frame perform actions on every tick
fn frame(x voidptr) {
	mut app := &App(x)
	app.update()
	app.draw()
}

// update perform any calculations that are needed before drawing
fn (mut a App) update() {
	if a.state == .game {
		a.snake.move()
		if a.snake.check_overlap() {
			a.state = .gameover
			return
		}
		if a.check_capture() {
			a.rat.randomize()
			a.snake.grow()
		}
	}
}

// draw write to the screen
fn (mut a App) draw() {
	// reset screen
	a.termui.clear()
	a.termui.set_bg_color(white)
	a.termui.draw_empty_rect(1, 1, a.width, a.height)
	a.termui.set_bg_color(black)
	a.termui.draw_rect(2, 2, a.width - 1, a.height - 1)
	// determine if a special screen needs to be draw
	match a.state {
		.gameover {
			a.draw_gameover()
			a.redraw = false
		}
		.pause {
			a.draw_pause()
		}
		else {
			a.redraw = true
		}
	}
	a.termui.set_color(blue)
	a.termui.set_bg_color(white)
	a.termui.draw_text(3 * block_size, a.height - (2 * block_size), 'p - (un)pause r - reset q - quit')
	// draw the snake, rat, and score if appropriate
	if a.redraw {
		a.draw_gamescreen()
	}
	// write to the screen
	a.termui.reset_bg_color()
	a.termui.flush()
}

// move_snake move the snake in specified direction
fn (mut a App) move_snake(direction Orientation) {
	a.snake.impulse(direction)
}

// check_capture determine if the snake overlaps with the rat
fn (a App) check_capture() bool {
	snake_pos := a.snake.get_head().pos
	rat_pos := a.rat.pos
	return snake_pos.x < rat_pos.x + block_size &&
		snake_pos.x + block_size > rat_pos.x && snake_pos.y < rat_pos.y + block_size && snake_pos.y +
		block_size > rat_pos.y
}

fn (mut a App) draw_snake() {
	a.snake.draw()
}

fn (mut a App) draw_rat() {
	a.termui.set_bg_color(a.rat.color)
	a.termui.draw_rect(a.rat.pos.x, a.rat.pos.y, a.rat.pos.x + block_size, a.rat.pos.y + block_size)
}

fn (mut a App) draw_gamescreen() {
	$if verbose ? {
		a.draw_debug()
	}
	a.draw_score()
	a.draw_rat()
	a.draw_snake()
}

fn (mut a App) draw_score() {
	a.termui.set_color(blue)
	a.termui.set_bg_color(white)
	score := a.snake.length() - 1
	a.termui.draw_text(a.width - (2 * block_size), block_size, '${score:03d}')
}

fn (mut a App) draw_pause() {
	a.termui.set_color(blue)
	a.termui.draw_text((a.width / 2) - block_size, 3 * block_size, 'Paused!')
}

fn (mut a App) draw_debug() {
	a.termui.set_color(blue)
	a.termui.set_bg_color(white)
	snake := a.snake
	a.termui.draw_text(block_size, 1 * block_size, 'Display_width: ${a.width:04d} Display_height: ${a.height:04d}')
	a.termui.draw_text(block_size, 2 * block_size, 'Vx: ${snake.velocity.x:+02d} Vy: ${snake.velocity.y:+02d}')
	a.termui.draw_text(block_size, 3 * block_size, 'F: $snake.direction')
	snake_head := snake.get_head()
	rat := a.rat
	a.termui.draw_text(block_size, 4 * block_size, 'Sx: ${snake_head.pos.x:+03d} Sy: ${snake_head.pos.y:+03d}')
	a.termui.draw_text(block_size, 5 * block_size, 'Rx: ${rat.pos.x:+03d} Ry: ${rat.pos.y:+03d}')
}

fn (mut a App) draw_gameover() {
	a.termui.set_bg_color(white)
	a.termui.draw_rect(0, 0, a.width, a.height)
	a.termui.set_color(red)
	a.rat.pos = Vec{
		x: -1
		y: -1
	}
	a.termui.draw_text(block_size, 2 * block_size, '   #####                         #######                       ')
	a.termui.draw_text(block_size, 3 * block_size, '  #     #   ##   #    # ######   #     # #    # ###### #####   ')
	a.termui.draw_text(block_size, 4 * block_size, '  #        #  #  ##  ## #        #     # #    # #      #    #  ')
	a.termui.draw_text(block_size, 5 * block_size, '  #  #### #    # # ## # #####    #     # #    # #####  #    #  ')
	a.termui.draw_text(block_size, 6 * block_size, '  #     # ###### #    # #        #     # #    # #      #####   ')
	a.termui.draw_text(block_size, 7 * block_size, '  #     # #    # #    # #        #     #  #  #  #      #   #   ')
	a.termui.draw_text(block_size, 8 * block_size, '   #####  #    # #    # ######   #######   ##   ###### #    #  ')
}

mut app := &App{}
app.termui = termui.init({
	user_data: app
	event_fn: event
	frame_fn: frame
	init_fn: init
	hide_cursor: true
	frame_rate: 15
})
app.termui.run()
