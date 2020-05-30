module main

import gg
import freetype
import gx
import glfw
import time

const (
	win_width = 600
	win_height = 700
	bg_color = gx.white
)

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


struct Context {
mut:
	gg &gg.GG
	ft &freetype.FreeType
}

fn main() {
	glfw.init_glfw()
	mut ctx := &Context{
		gg: gg.new_context(gg.Cfg {
			width: win_width
			height: win_height
			use_ortho: true // This is needed for 2D drawing
			create_window: true
			window_title: 'Empty window'
			//window_user_ptr: ctx
		})
	}
	ctx.gg.window.set_user_ptr(ctx) // TODO remove this when `window_user_ptr:` works
	gg.clear(bg_color)
	// Try to load font
	ctx.ft = freetype.new_context(gg.Cfg{
		width: win_width
		height: win_height
		use_ortho: true
		font_size: 13
		scale: 2
	})
	for {
		t := time.ticks()
		gg.clear(bg_color)
		ctx.draw()
		ctx.gg.render()
	println(time.ticks()-t)
		if ctx.gg.window.should_close() {
			ctx.gg.window.destroy()
			return
		}
	}
}

fn (ctx Context) draw() {
	mut y := 10
	for line in lines {
		ctx.ft.draw_text_def(10,y, line)
		y += 15
	}
}

