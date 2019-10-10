import (
	ui
	gx
)

const (
	NR_COLS     = 3
	CELL_HEIGHT = 25
	CELL_WIDTH  = 100
	TABLE_WIDTH = CELL_WIDTH * NR_COLS
)

struct User {
	first_name string
	last_name  string
	age        int
}

struct Context {
	mut:
	first_name ui.TextBox
	last_name  ui.TextBox
	age        ui.TextBox
	users      []User
	window     &ui.Window
	txt_pos    int
}

/*
// Current V
struct Window {
	width: 500
	height: 300
	title: 'Users'
	body: Layout {
		body: [
			TextBox {
				placeholder: 'First name'
			},
			TextBox {
				placeholder: 'Last name'
			},
			TextBox {
				placeholder: 'Age'
			},
			Button {
				title: 'Add user'
				onclick: btn_click
			},
		]
	}	
}	

// Improved V
struct Window {
	width: 500
	height: 300
	title: 'Users'
	
	Layout {
	[
		TextBox {
			placeholder: 'First name'
		}
		TextBox {
			placeholder: 'Last name'
		}
		TextBox {
			placeholder: 'Age'
		}
		Button {
			title: 'Add user'
			onclick: btn_click
		}
	]
	}	
}	

/*

Window {
	width: 500
	height: 300
	title: 'Users'
	VLayout {
		TextBox {
			placeholder: 'First name'
		}
		TextBox {
			placeholder: 'Last name'
		}
		TextBox {
			placeholder: 'Age'
		}
		Button {
			title: 'Add user'
			onclick: btn_click
		}
	}	
	
	draw: draw_fn
}	
*/
*/

fn main() {
	mut ctx := &Context {
		txt_pos: 10
	}
	ctx.window = ui.new_window(ui.WinCfg {
		width: 500
		height: 300
		title: 'Users'
		draw_fn: draw
		ptr: ctx
	})
	ctx.first_name = ctx.add_textbox('First name')
	ctx.last_name = ctx.add_textbox('Last name')
	ctx.age = ctx.add_textbox('Age')
	mut btn := ui.new_button('Add user', ctx.window, btn_click)
	btn.widget.set_pos(TABLE_WIDTH + 50, ctx.txt_pos)
	for {
		ui.wait_events()
	}
}

// TODO replace with `fn (ctx mut Context) btn_click() {`
fn btn_click(_ctx &Context) {
	println('users.v: button click')
	mut ctx := _ctx// TODO hack
	ctx.users << User {
		first_name: ctx.first_name.text()
		last_name: ctx.last_name.text()
		age: ctx.age.text().int()
	}
	ctx.window.refresh()
}

// TODO replace with `fn (ctx mut Context) draw() {`
fn draw(ctx &Context) {
	for i, user in ctx.users {
		x := 10
		y := 10 + i * CELL_HEIGHT
		// Outer border
		ui.draw_empty_rect(x, y, TABLE_WIDTH, CELL_HEIGHT, gx.Gray)
		// Vertical separators
		ui.draw_line(x + CELL_WIDTH, y, x + CELL_WIDTH, y + CELL_HEIGHT, gx.Gray)
		ui.draw_line(x + CELL_WIDTH * 2, y, x + CELL_WIDTH * 2, y + CELL_HEIGHT, gx.Gray)
		// Text values
		ui.draw_text_def(x + 5, y + 5, user.first_name)
		ui.draw_text_def(x + 5 + CELL_WIDTH, y + 5, user.last_name)
		ui.draw_text_def(x + 5 + CELL_WIDTH * 2, y + 5, user.age.str())
	}
}

fn (ctx mut Context) add_textbox(placeholder string) ui.TextBox {
	mut txt_box := ui.new_textbox(ctx.window, false)
	txt_box.set_placeholder(placeholder)
	txt_box.widget.set_pos(TABLE_WIDTH + 50, ctx.txt_pos)
	ctx.txt_pos += 30
	return txt_box
}

