import term.ui as tui
import flag

struct DocTest {
	show_version bool @[short: v; xdoc: 'Show version and exit']
	debug_level  int  @[long: debug; short: d; xdoc: 'Debug level']
	level        f32  @[only: l; xdoc: 'Do not show this']
	example      string
	square       bool
	multi        int   @[only: m; repeats]
	wroom        []int @[short: w]
	the_limit    string
}

struct App {
mut:
	tui       &tui.Context = unsafe { nil }
	frame     int
	square    string = '.____.\n|    |\n|    |\n|____|'
	pad       int    = 1
	direction int    = 1
}

fn event(e &tui.Event, mut app App) {
	match e.typ {
		.mouse_down {}
		.mouse_drag {}
		.mouse_up {}
		.key_down {
			if e.code == .c {
			} else if e.code == .escape {
				exit(0)
			}
		}
		else {}
	}
}

fn frame(mut app App) {
	app.tui.clear()
	app.frame++

	if app.frame % 4 == 0 {
		app.pad += app.direction
		app.square = '${' '.repeat(app.pad)}.____.\n${' '.repeat(app.pad)}|    |\n${' '.repeat(app.pad)}|    |\n${' '.repeat(app.pad)}|____|'
	}
	if app.frame % 100 == 0 {
		if app.direction > 0 {
			app.direction = -1
		} else {
			app.direction = 1
		}
	}

	help_text := flag.to_doc[DocTest](
		version:     '1.0'
		description: 'Hello! This should show an *animated* example application description.
We are at frame ${app.frame}.
Press ESCAPE or Ctrl+C to exit'
		footer:      '
Press ESCAPE or Ctrl+C to exit'
		fields:      {
			'level':                                    'Level of lorem ipsum\nand more\nmany many many more.\nNotice how user newlines/format is kept since\ninput lines are all less or within\nthe default layout.description_padding\nand max width'
			'example':                                  'Looong example text without newlines or anything else and lorem ipsum and more and many many many more. Should be auto fitted'
			'multi':                                    'This flag can be repeated'
			'-e, --extra':                              'Secret flag that does not exist on the struct, but we want documented (in same format as the others)'
			'-q, --quiet-and-quite-long-flag <string>': 'Mega long description and secret flag that does not exist on the struct, but we want documented. Also the flag has custom newlines\nand the flag line itself is super long'
			'square':                                   '${app.square}'
		}
	) or { '' }

	app.tui.draw_text(0, 0, '${help_text}')
	app.tui.reset()

	app.tui.flush()
}

type EventFn = fn (&tui.Event, voidptr)

type FrameFn = fn (voidptr)

fn main() {
	mut app := &App{}
	app.tui = tui.init(
		user_data:   app
		event_fn:    EventFn(event)
		frame_fn:    FrameFn(frame)
		hide_cursor: true
		frame_rate:  60
	)
	app.tui.run()!
}
