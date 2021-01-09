import sokol
import sokol.sapp
import sokol.gfx
import sokol.sgl
import sokol.sfons
import os
import time


const (
text = '
Once upon a midnight dreary, while I pondered, weak and weary,
Over many a quaint and curious volume of forgotten lore—
    While I nodded, nearly napping, suddenly there came a tapping,
As of some one gently rapping, rapping at my chamber door.
“’Tis some visitor,” I muttered, “tapping at my chamber door—
            Only this and nothing more.”

    Ah, distinctly I remember it was in the bleak December;
And each separate dying ember wrought its ghost upon the floor.
    Eagerly I wished the morrow;—vainly I had sought to borrow
    From my books surcease of sorrow—sorrow for the lost Lenore—
For the rare and radiant maiden whom the angels name Lenore—
            Nameless here for evermore.

    And the silken, sad, uncertain rustling of each purple curtain
Thrilled me—filled me with fantastic terrors never felt before;
    So that now, to still the beating of my heart, I stood repeating
    “’Tis some visitor entreating entrance at my chamber door—
Some late visitor entreating entrance at my chamber door;—
            This it is and nothing more.”

    Presently my soul grew stronger; hesitating then no longer,
“Sir,” said I, “or Madam, truly your forgiveness I implore;
    But the fact is I was napping, and so gently you came rapping,
    And so faintly you came tapping, tapping at my chamber door,
That I scarce was sure I heard you”—here I opened wide the door;—
            Darkness there and nothing more.

Deep into that darkness peering, long I stood there wondering, fearing,
Doubting, dreaming dreams no mortal ever dared to dream before;
    But the silence was unbroken, and the stillness gave no token,
    And the only word there spoken was the whispered word, “Lenore?”
This I whispered, and an echo murmured back the word, “Lenore!”—
            Merely this and nothing more.

    Back into the chamber turning, all my soul within me burning,
Soon again I heard a tapping somewhat louder than before.
    “Surely,” said I, “surely that is something at my window lattice;
      Let me see, then, what thereat is, and this mystery explore—
Let my heart be still a moment and this mystery explore;—
            ’Tis the wind and nothing more!”
'
lines = text.split('\n')
)

struct AppState {
mut:
	pass_action C.sg_pass_action
	fons        &C.FONScontext
	font_normal int
	inited bool
}

fn main() {
	mut color_action := C.sg_color_attachment_action{
		action: gfx.Action(C.SG_ACTION_CLEAR)
	}
	color_action.val[0] = 1
	color_action.val[1] = 1
	color_action.val[2] = 1
	color_action.val[3] = 1.0
	mut pass_action := C.sg_pass_action{}
	pass_action.colors[0] = color_action
	state := &AppState{
		pass_action: pass_action
		fons: &C.FONScontext(0)
	}
	title := 'V Metal/GL Text Rendering'
	desc := C.sapp_desc{
		user_data: state
		init_userdata_cb: init
		frame_userdata_cb: frame
		window_title: title.str
		html5_canvas_name: title.str
		width: 600
		height: 700
		high_dpi: true
	}
	sapp.run(&desc)
}

fn init(user_data voidptr) {
	mut state := &AppState(user_data)
	desc := sapp.create_desc()
	gfx.setup(&desc)
	s := &C.sgl_desc_t{}
	C.sgl_setup(s)
	state.fons = sfons.create(512, 512, 1)
	// or use DroidSerif-Regular.ttf
	if bytes := os.read_bytes(os.resource_abs_path('../assets/fonts/RobotoMono-Regular.ttf')) {
		println('loaded font: $bytes.len')
		state.font_normal = C.fonsAddFontMem(state.fons, 'sans', bytes.data, bytes.len, false)
	}
}

fn frame(user_data voidptr) {
	t := time.ticks()
	mut state := &AppState(user_data)
	state.render_font()
	gfx.begin_default_pass(&state.pass_action, sapp.width(), sapp.height())
	sgl.draw()
	gfx.end_pass()
	gfx.commit()
	println(time.ticks()-t)
}

const (

black = C.sfons_rgba(0, 0, 0, 255)
)

fn (mut state AppState) render_font() {
	lh := 30
	mut dy := lh
	if !state.inited {
		state.fons.clear_state()
		sgl.defaults()
		sgl.matrix_mode_projection()
		sgl.ortho(0.0, f32(C.sapp_width()), f32(C.sapp_height()), 0.0, -1.0, 1.0)
		state.fons.set_font(state.font_normal)
		state.fons.set_size(100.0)
		C.fonsSetColor(state.fons, black)
		C.fonsSetFont(state.fons, state.font_normal)
		C.fonsSetSize(state.fons, 35.0)
		state.inited = true
	}

	for line in lines {
		C.fonsDrawText(state.fons, 40, dy, line.str, C.NULL)
		dy += lh
	}
	C.sfons_flush(state.fons)
}

fn line(sx f32, sy f32, ex f32, ey f32) {
	sgl.begin_lines()
	sgl.c4b(255, 255, 0, 128)
	sgl.v2f(sx, sy)
	sgl.v2f(ex, ey)
	sgl.end()
}
