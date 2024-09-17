import term.ui as tui
import flag

@[name: 'flag_layout_editor']
@[version: '1.0']
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

const field_docs = {
	'level':                                    'Level of lorem ipsum\nand more\nmany many many more.\nNotice how user newlines/format is kept since\ninput lines are all less or within\nthe default layout.description_padding\nand max width'
	'example':                                  'Looong example text without newlines or anything else and lorem ipsum and more and many many many more. Should be auto fitted'
	'the_limit':                                'Looongbobbytextwithoutnewlinesoranythingelseandlorem ipsumandmoreandmanymanymanymore ffffffffffffffffffffffffffffffff f'
	'multi':                                    'This flag can be repeated'
	'-e, --extra':                              'Secret flag that does not exist on the struct, but we want documented (in same format as the others)'
	'-q, --quiet-and-quite-long-flag <string>': 'Mega long description and secret flag that does not exist on the struct, but we want documented. Also the flag has custom newlines\nand the flag line itself is super long'
}

struct App {
mut:
	tui     &tui.Context = unsafe { nil }
	layout  flag.DocLayout
	options flag.DocOptions
	edit    Edit
}

enum Edit {
	// DocLayout fields
	description_padding
	description_width
	flag_indent
	// DocOptions.compact
	compact
	// DocOptions.show flags
	name
	version
	flags
	flag_type
	flag_hint
	description
	flags_header
	footer
}

pub fn (e Edit) next() Edit {
	return match e {
		.description_padding {
			.description_width
		}
		.description_width {
			.flag_indent
		}
		.flag_indent {
			.compact
		}
		.compact {
			.name
		}
		.name {
			.version
		}
		.version {
			.flags
		}
		.flags {
			.flag_type
		}
		.flag_type {
			.flag_hint
		}
		.flag_hint {
			.description
		}
		.description {
			.flags_header
		}
		.flags_header {
			.footer
		}
		.footer {
			.description_padding
		}
	}
}

fn event(e &tui.Event, mut app App) {
	mut incr_decr := 0
	match e.typ {
		.mouse_down {
			app.edit = app.edit.next()
		}
		.mouse_drag {}
		.mouse_up {}
		.key_down {
			match e.code {
				.left {
					incr_decr = -1
				}
				.right {
					incr_decr = 1
				}
				.space {
					app.edit = app.edit.next()
				}
				.escape {
					exit(0)
				}
				else {}
			}
		}
		else {}
	}

	if incr_decr != 0 {
		match app.edit {
			.flag_indent {
				app.layout.flag_indent += incr_decr
			}
			.description_padding {
				app.layout.description_padding += incr_decr
			}
			.description_width {
				app.layout.description_width += incr_decr
			}
			.compact {
				app.options.compact = !app.options.compact
			}
			.name {
				app.options.show.toggle(.name)
			}
			.version {
				app.options.show.toggle(.version)
			}
			.flags {
				app.options.show.toggle(.flags)
			}
			.flag_type {
				app.options.show.toggle(.flag_type)
			}
			.flag_hint {
				app.options.show.toggle(.flag_hint)
			}
			.description {
				app.options.show.toggle(.description)
			}
			.flags_header {
				app.options.show.toggle(.flags_header)
			}
			.footer {
				app.options.show.toggle(.footer)
			}
		}
	}
}

fn frame(mut app App) {
	app.tui.clear()

	mut value := match app.edit {
		.flag_indent {
			'${app.layout.flag_indent}'
		}
		.description_padding {
			'${app.layout.description_padding}'
		}
		.description_width {
			'${app.layout.description_width}'
		}
		.compact {
			if app.options.compact { 'on' } else { 'off' }
		}
		.name {
			if app.options.show.has(.name) { 'on' } else { 'off' }
		}
		.version {
			if app.options.show.has(.version) { 'on' } else { 'off' }
		}
		.flags {
			if app.options.show.has(.flags) { 'on' } else { 'off' }
		}
		.flag_type {
			if app.options.show.has(.flag_type) { 'on' } else { 'off' }
		}
		.flag_hint {
			if app.options.show.has(.flag_hint) { 'on' } else { 'off' }
		}
		.description {
			if app.options.show.has(.description) { 'on' } else { 'off' }
		}
		.flags_header {
			if app.options.show.has(.flags_header) { 'on' } else { 'off' }
		}
		.footer {
			if app.options.show.has(.footer) { 'on' } else { 'off' }
		}
	}

	app.tui.draw_text(0, 0, 'Click left-mouse button or use space to edit the next property.
Use keyboard arrow keys right and left to adjust the value of the property
Editing property: ${app.edit}, value: ${value}')

	help_text := flag.to_doc[DocTest](
		description: 'Simple DocLayout editor.
Press ESCAPE or Ctrl+C to exit and print layout code'
		footer:      '
Press ESCAPE or Ctrl+C to exit and print layout code'
		fields:      unsafe { field_docs }
		layout:      app.layout
		options:     app.options
	) or { '' }

	app.tui.draw_text(0, 5, '${help_text}')
	app.tui.reset()

	app.tui.flush()
}

type EventFn = fn (&tui.Event, voidptr)

type FrameFn = fn (voidptr)

fn main() {
	mut app := &App{}
	at_exit(fn [app] () {
		println('${app.layout}\n')
		println('${app.options}')
	}) or {}
	app.tui = tui.init(
		user_data:   app
		event_fn:    EventFn(event)
		frame_fn:    FrameFn(frame)
		hide_cursor: true
		frame_rate:  60
	)
	app.tui.run()!
}
